###........................................###
###
###    SPA 1B
###    Clappers Numbers per tow
###    Clapper population numbers
###
###   Rehauled July 2020 J.Sameoto
###........................................###

#!NB! Use simple mean for clapper calculations, even in areas with spr design; the numbers are often too low for spr to work

options(stringsAsFactors = FALSE)

#required packages
library(ROracle)
library(PEDstrata) #v.1.0.3
library(tidyverse)
library(PBSmapping)
library(ggplot2)
library(cowplot)
library(plyr)


#Read in functions from Github
funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r" 
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

###
# read in shell height and meat weight data from database
###
#strata.spa1b<-c(35,37,38,49,51:53)

quer1 <- ("SELECT *
          FROM scallsur.scdeadres
          WHERE strata_id IN (35, 37, 38, 49, 51, 52, 53)
          AND (cruise LIKE 'BA%'
OR cruise LIKE 'BF%'
OR cruise LIKE 'BI%'
OR cruise LIKE 'GM%'
OR cruise LIKE 'RF%')")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle; numbers by shell height bin
deadfreq <- dbGetQuery(chan, quer1)

#add YEAR column to data
deadfreq$YEAR <- as.numeric(substr(deadfreq$CRUISE,3,6))
deadfreq$CruiseID <- paste(deadfreq$CRUISE,deadfreq$TOW_NO,sep = '.') #will be used to assign strata_id to cross ref files

#convert lat/lon to DD
deadfreq$lat <- convert.dd.dddd(deadfreq$START_LAT) #format lat and lon
deadfreq$lon <- convert.dd.dddd(deadfreq$START_LONG)
deadfreq$ID <- 1:nrow(deadfreq)

####
# post-stratify MBN for East/West line (do for both deadfreq and deadweight
# for Midbay North, need to assign strata to East/West
SPA1B.MBN.E <- data.frame(PID = 58,POS = 1:4,X = c(-65.710, -65.586, -65.197, -65.264),Y = c(45.280, 45.076, 45.237, 45.459)) #New strata


#assign strata id "58" to Midbay North East   (strata_id 38 to MBN West)
events <- subset(deadfreq,STRATA_ID == 38,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
deadfreq$STRATA_ID[deadfreq$ID %in% findPolys(events,SPA1B.MBN.E)$EID] <- 58


####
###
### ---- CLAPPER RECRUIT AND COMMERCIAL NUMBERS PER TOW AND POPULATION NUMBERS ---- 
###
####

###
# ---- 1. Cape Spencer (simple mean for whole area) ----
###

###### Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 1997:surveyyear
X <- length(years)

SPA1B.CS.RecDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(191023.77,X), Area = rep("CS", X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.CS.RecDead.simple$Year)) {
  temp.data <- deadfreq[deadfreq$YEAR == 1996 + i,]
  #add calculation of variance if required
	SPA1B.CS.RecDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 37 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
	SPA1B.CS.RecDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 37 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.CS.RecDead.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.CS.RecDead.simple$Year, SPA1B.CS.RecDead.simple$Mean.nums, xout=2020) #  0.2258621
SPA1B.CS.RecDead.simple[SPA1B.CS.RecDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(0.2258621, 1.8318719) #assume var from 2019

#calculate CV and Pop. estimate
SPA1B.CS.RecDead.simple$cv <- sqrt(SPA1B.CS.RecDead.simple$var.y)/SPA1B.CS.RecDead.simple$Mean.nums
SPA1B.CS.RecDead.simple$Pop <- SPA1B.CS.RecDead.simple$Mean.nums*SPA1B.CS.RecDead.simple$NH

###### Commercial (80+ mm)

#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 1997:surveyyear
X <- length(years)

SPA1B.CS.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(191023.77, X), Area = rep("CS", X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.CS.CommDead.simple$Year))
  {
  temp.data <- deadfreq[deadfreq$YEAR == 1996 + i,]
	#add calculation of variance if required
	SPA1B.CS.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 37 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
	SPA1B.CS.CommDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 37 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.CS.CommDead.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.CS.CommDead.simple$Year, SPA1B.CS.CommDead.simple$Mean.nums, xout=2020) #  9.734483
SPA1B.CS.CommDead.simple[SPA1B.CS.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(9.734483, 37.194631) #assume var from 2019

#calculate CV and Pop. estimate
SPA1B.CS.CommDead.simple$cv <- sqrt(SPA1B.CS.CommDead.simple$var.y)/SPA1B.CS.CommDead.simple$Mean.nums
SPA1B.CS.CommDead.simple$Pop <- SPA1B.CS.CommDead.simple$Mean.nums*SPA1B.CS.CommDead.simple$NH

#combine Recruit and Commercial dataframes
SPA1B.CS.ClapNumbers <- rbind(SPA1B.CS.RecDead.simple,SPA1B.CS.CommDead.simple)

####
# ---- 2. Middle Bay North (spr) ----
####

#2a. Middle Bay North - EAST

###### Recruit (65-79)

years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNE.RecDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), Prop = rep(0.732, X))
for (i in 1:length(SPA1B.MBNE.RecDead.simple$Year)) {
	temp.data <- deadfreq[deadfreq$YEAR == 1996 + i,]
	#add calculation of variance if required
SPA1B.MBNE.RecDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 58 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
SPA1B.MBNE.RecDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 58 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.MBNE.RecDead.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNE.RecDead.simple$Year, SPA1B.MBNE.RecDead.simple$Mean.nums, xout=2020) #  3.634154
SPA1B.MBNE.RecDead.simple[SPA1B.MBNE.RecDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(3.634154, 13.2985667) #assume var from 2019


###### Commercial (80+ mm)

#simple mean for non-spr years
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNE.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), Prop = rep(0.732, X))
for (i in 1:length(SPA1B.MBNE.CommDead.simple$Year)) {
	temp.data <- deadfreq[deadfreq$YEAR == 1996 + i,]
	#add calculation of variance if required
SPA1B.MBNE.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 58 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
SPA1B.MBNE.CommDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 58 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.MBNE.CommDead.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNE.CommDead.simple$Year, SPA1B.MBNE.CommDead.simple$Mean.nums, xout=2020) #  7.129385
SPA1B.MBNE.CommDead.simple[SPA1B.MBNE.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(7.129385, 25.5946000) #assume var from 2019



#2b. Middle Bay North - WEST

###### Recruit (65-79)

years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.RecDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), Prop = rep(0.268, X))
for (i in 1:length(SPA1B.MBNW.RecDead.simple$Year)) {
	temp.data <- deadfreq[deadfreq$YEAR == 1996 + i,]
	#add calculation of variance if required
SPA1B.MBNW.RecDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
SPA1B.MBNW.RecDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.MBNW.RecDead.simple

#fill in missing years, assume 1997-2002 as MBNE, interpolate 2004 & 2020 
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 1997,c(2,3)] <- c(0,0)
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 1998,c(2,3)] <- c(0,0)
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 1999,c(2,3)] <- c(0,0)
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 2000,c(2,3)] <- c(0,0)
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 2001,c(2,3)] <- c(0,0)
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 2002,c(2,3)] <- c(0,0)
#2004
approx(SPA1B.MBNW.RecDead.simple$Year, SPA1B.MBNW.RecDead.simple$Mean.nums, xout = 2004) # 0.29706
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 2004,"Mean.nums"] <- 0.29706
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year == 2004,"var.y"] <- 1.753088  #assume previous year var 

#2020 
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNW.RecDead.simple$Year, SPA1B.MBNW.RecDead.simple$Mean.nums, xout=2020) #  0.7694444
SPA1B.MBNW.RecDead.simple[SPA1B.MBNW.RecDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(0.7694444, 3.851111) #assume var from 2019


###### Commercial (80+ mm)

#simple mean for non-spr years
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X),Prop = rep(0.268, X))
for (i in 1:length(SPA1B.MBNW.CommDead.simple$Year)) {
temp.data <- deadfreq[deadfreq$YEAR == 1996 + i,]
SPA1B.MBNW.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
SPA1B.MBNW.CommDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.MBNW.CommDead.simple

#fill in missing years, assume 1997-2002 as MBNE, interpolate 2004
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year == 1997,c(2,3)] <- c(2.26667,  3.85333)
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year == 1998,c(2,3)] <- c(0.00000 , 0.00000)
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year == 1999,c(2,3)] <- c(1.02500  ,4.20250)
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year == 2000,c(2,3)] <- c(0.24167,  0.70083)
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year == 2001,c(2,3)] <- c(0.77692 , 2.18359)
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year == 2002,c(2,3)] <- c(0.28182 , 0.8736)
#2004
approx(SPA1B.MBNW.CommDead.simple$Year, SPA1B.MBNW.CommDead.simple$Mean.nums, xout = 2004) # 3.1284
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year == 2004,"Mean.nums"] <- 3.1284
#2020 
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNW.CommDead.simple$Year, SPA1B.MBNW.CommDead.simple$Mean.nums, xout=2020) #  16.99028
SPA1B.MBNW.CommDead.simple[SPA1B.MBNW.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(16.99028, 113.802778) #assume var from 2019


#Calculate survey index for MBN

#Recruit
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.RecDead.simple$prop.mean <- SPA1B.MBNW.RecDead.simple$Prop*SPA1B.MBNW.RecDead.simple$Mean.nums
SPA1B.MBNE.RecDead.simple$prop.mean <- SPA1B.MBNE.RecDead.simple$Prop*SPA1B.MBNE.RecDead.simple$Mean.nums

SPA1B.MBN.RecDead <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(294145.4,X), Area = rep("MBN", X), Age = rep("Recruit", X))

SPA1B.MBN.RecDead$Mean.nums <- SPA1B.MBNW.RecDead.simple$prop.mean + SPA1B.MBNE.RecDead.simple$prop.mean
SPA1B.MBN.RecDead$cv <- sqrt(SPA1B.MBN.RecDead$var.y)/SPA1B.MBN.RecDead$Mean.nums
SPA1B.MBN.RecDead$Pop <- SPA1B.MBN.RecDead$Mean.nums*SPA1B.MBN.RecDead$NH

#Commercial
SPA1B.MBNW.CommDead.simple$prop.mean <- SPA1B.MBNW.CommDead.simple$Prop*SPA1B.MBNW.CommDead.simple$Mean.nums
SPA1B.MBNE.CommDead.simple$prop.mean <- SPA1B.MBNE.CommDead.simple$Prop*SPA1B.MBNE.CommDead.simple$Mean.nums

SPA1B.MBN.CommDead <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(294145.4,X), Area = rep("MBN", X), Age = rep("Commercial", X))

SPA1B.MBN.CommDead$Mean.nums <- SPA1B.MBNW.CommDead.simple$prop.mean + SPA1B.MBNE.CommDead.simple$prop.mean
SPA1B.MBN.CommDead$var.y <- (SPA1B.MBNW.CommDead.simple$Prop*SPA1B.MBNW.CommDead.simple$var.y) + (SPA1B.MBNE.CommDead.simple$Prop*SPA1B.MBNE.CommDead.simple$var.y)
SPA1B.MBN.CommDead$cv <- sqrt(SPA1B.MBN.CommDead$var.y)/SPA1B.MBN.CommDead$Mean.nums
SPA1B.MBN.CommDead$Pop <- SPA1B.MBN.CommDead$Mean.nums*SPA1B.MBN.CommDead$NH

#combine Recruit and Commercial dataframes
SPA1B.MBN.ClapNumbers <- rbind(SPA1B.MBN.RecDead,SPA1B.MBN.CommDead)

####
# ---- 3. Upper Bay 28C ----
####

############Recruit (65-79)

years <- 2001:surveyyear
X <- length(years)

SPA1B.28C.RecDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(88000.2,X),Area = rep("28C", X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.28C.RecDead.simple$Year)) {
  temp.data <- deadfreq[deadfreq$YEAR == 2000 + i,]
	#add calculation of variance if required
SPA1B.28C.RecDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
SPA1B.28C.RecDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.28C.RecDead.simple

#assume 1997-2000 same as MBN
SPA1B.28C.RecDead.simple <- rbind(SPA1B.MBN.RecDead[SPA1B.MBN.RecDead$Year < 2001,c(1:7)], SPA1B.28C.RecDead.simple)
SPA1B.28C.RecDead.simple[c(1:4), "NH"] <- 88000.2
SPA1B.28C.RecDead.simple[c(1:4), "Area"] <- "28C"

#interpolate for other years
approx(SPA1B.28C.RecDead.simple$Year, SPA1B.28C.RecDead.simple$Mean.nums, xout = 2004) #0.24167
SPA1B.28C.RecDead.simple[SPA1B.28C.RecDead.simple$Year == 2004,"Mean.nums"] <- 0.24167
SPA1B.28C.RecDead.simple[SPA1B.28C.RecDead.simple$Year == 2004,"var.y"] <- 1.5835057 # assume var from previous year 
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.28C.RecDead.simple$Year, SPA1B.28C.RecDead.simple$Mean.nums, xout=2020) #  0.2588235
SPA1B.28C.RecDead.simple[SPA1B.28C.RecDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(0.2588235, 2.1352941) #assume var from 2019 but since 0 in 2019 take 2020 var 


SPA1B.28C.RecDead.simple$cv <- sqrt(SPA1B.28C.RecDead.simple$var.y)/SPA1B.28C.RecDead.simple$Mean.nums
SPA1B.28C.RecDead.simple$Pop <- SPA1B.28C.RecDead.simple$Mean.nums*SPA1B.28C.RecDead.simple$NH

###### Commercial (65-79)
years <- 2001:surveyyear
X <- length(years)

SPA1B.28C.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(88000.2,X),Area = rep("28C", X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.28C.CommDead.simple$Year)) {
	temp.data <- deadfreq[deadfreq$YEAR == 2000 + i,]
SPA1B.28C.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1,27:50],1,sum))
SPA1B.28C.CommDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1,27:50],1,sum))
}
SPA1B.28C.CommDead.simple

#assume 1997-2000 same as MBN
SPA1B.28C.CommDead.simple <- rbind(SPA1B.MBN.CommDead[SPA1B.MBN.CommDead$Year < 2001,c(1:7)], SPA1B.28C.CommDead.simple)
SPA1B.28C.CommDead.simple[c(1:4), "NH"] <- 88000.2
SPA1B.28C.CommDead.simple[c(1:4), "Area"] <- "28C"

#interpolate missing years
#2004
approx(SPA1B.28C.CommDead.simple$Year, SPA1B.28C.CommDead.simple$Mean.nums, xout = 2004) #4.874
SPA1B.28C.CommDead.simple[SPA1B.28C.CommDead.simple$Year == 2004,"Mean.nums"] <- 4.874
SPA1B.28C.CommDead.simple[SPA1B.28C.CommDead.simple$Year == 2004,"var.y"] <- 70.442540
#2020
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.28C.CommDead.simple$Year, SPA1B.28C.CommDead.simple$Mean.nums, xout=2020) #  11.90294
SPA1B.28C.CommDead.simple[SPA1B.28C.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(11.90294, 351.777574 ) #assume var from 2019


SPA1B.28C.CommDead.simple$cv <- sqrt(SPA1B.28C.CommDead.simple$var.y)/SPA1B.28C.CommDead.simple$Mean.nums
SPA1B.28C.CommDead.simple$Pop <- SPA1B.28C.CommDead.simple$Mean.nums*SPA1B.28C.CommDead.simple$NH

#combine Recruit and CommDeadercial dataframes
SPA1B.28C.ClapNumbers <- rbind(SPA1B.28C.RecDead.simple,SPA1B.28C.CommDead.simple)


####
# ---- 4. 28D Outer Bay ----
####

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID == 1

years <- 2001:surveyyear
X <- length(years)

SPA1B.Out.DeadRec.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(56009.889
, X),Area = rep("Out", X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.Out.DeadRec.simple$Year)) {
  temp.data <- deadfreq[deadfreq$YEAR == 2000 + i,]
SPA1B.Out.DeadRec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
SPA1B.Out.DeadRec.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.Out.DeadRec.simple

#assume 1997-2000 same as MBN
SPA1B.Out.DeadRec.simple <- rbind(SPA1B.MBN.RecDead[SPA1B.MBN.RecDead$Year < 2001,c(1:7)], SPA1B.Out.DeadRec.simple)
SPA1B.Out.DeadRec.simple[c(1:4), "NH"] <- 56009.889
SPA1B.Out.DeadRec.simple[c(1:4), "Area"] <- "Out"

#interpolate missing years
#2004
approx(SPA1B.Out.DeadRec.simple$Year,SPA1B.Out.DeadRec.simple$Mean.nums, xout = 2004)# 0
SPA1B.Out.DeadRec.simple[SPA1B.Out.DeadRec.simple$Year == 2004,"Mean.nums"] <- 0
SPA1B.Out.DeadRec.simple[SPA1B.Out.DeadRec.simple$Year == 2004,"var.y"] <- 0
#2020
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.Out.DeadRec.simple$Year, SPA1B.Out.DeadRec.simple$Mean.nums, xout=2020) #  0.275
SPA1B.Out.DeadRec.simple[SPA1B.Out.DeadRec.simple$Year==2020,c("Mean.nums","var.y")] <- c(0.275, 2.4200000 ) #assume var from 2019 but since 0 in 2019 take var from 2020 


SPA1B.Out.DeadRec.simple$cv <- sqrt(SPA1B.Out.DeadRec.simple$var.y)/SPA1B.Out.DeadRec.simple$Mean.nums
SPA1B.Out.DeadRec.simple$Pop <- SPA1B.Out.DeadRec.simple$Mean.nums*SPA1B.Out.DeadRec.simple$NH

###### Commercial (80+ mm)

years <- 2001:surveyyear
X <- length(years)

SPA1B.Out.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(56009.889, X),Area = rep("Out", X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.Out.CommDead.simple$Year)) {
	temp.data <- deadfreq[deadfreq$YEAR == 2000 + i,]
SPA1B.Out.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
SPA1B.Out.CommDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.Out.CommDead.simple

#assume 1997-2000 same as MBN
SPA1B.Out.CommDead.simple <- rbind(SPA1B.MBN.CommDead[SPA1B.MBN.CommDead$Year < 2001,c(1:7)], SPA1B.Out.CommDead.simple)
SPA1B.Out.CommDead.simple[c(1:4), "NH"] <- 56009.889
SPA1B.Out.CommDead.simple[c(1:4), "Area"] <- "Out"

#interpolate missing years
#2004
approx(SPA1B.Out.CommDead.simple$Year, SPA1B.Out.CommDead.simple$Mean.nums, xout = 2004)#0
SPA1B.Out.CommDead.simple[SPA1B.Out.CommDead.simple$Year == 2004,"Mean.nums"] <- 0
#2020
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.Out.CommDead.simple$Year, SPA1B.Out.CommDead.simple$Mean.nums, xout=2020) #  0.69375
SPA1B.Out.CommDead.simple[SPA1B.Out.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(0.69375, 0.7200000 ) #assume var from 2019


SPA1B.Out.CommDead.simple$cv <- sqrt(SPA1B.Out.CommDead.simple$var.y)/SPA1B.Out.CommDead.simple$Mean.nums
SPA1B.Out.CommDead.simple$Pop <- SPA1B.Out.CommDead.simple$Mean.nums*SPA1B.Out.CommDead.simple$NH

#combine Recruit and CommDeadercial dataframes
SPA1B.Out.ClapNumbers <- rbind(SPA1B.Out.DeadRec.simple,SPA1B.Out.CommDead.simple)

####
# ---- 5. Advocate Harbour ----
####

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID == 1
years <- 2001:surveyyear
X <- length(years)

SPA1B.AH.RecDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(8626.27, X),Area = rep("AH", X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.AH.RecDead.simple$Year)) {
  temp.data <- deadfreq[deadfreq$YEAR == 2000 + i,]
SPA1B.AH.RecDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
SPA1B.AH.RecDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.AH.RecDead.simple

#assume 1997-2000 same as MBN
SPA1B.AH.RecDead.simple <- rbind(SPA1B.MBN.RecDead[SPA1B.MBN.RecDead$Year < 2001,c(1:7)], SPA1B.AH.RecDead.simple)
SPA1B.AH.RecDead.simple[c(1:4), "NH"] <- 8626.27
SPA1B.AH.RecDead.simple[c(1:4), "Area"] <- "AH"

#interpolate missing years
#2004
approx(SPA1B.AH.RecDead.simple$Year, SPA1B.AH.RecDead.simple$Mean.nums, xout = 2004)#0
SPA1B.AH.RecDead.simple[SPA1B.AH.RecDead.simple$Year == 2004,"Mean.nums"] <- 0
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.AH.RecDead.simple$Year, SPA1B.AH.RecDead.simple$Mean.nums, xout=2020) #  1.1
SPA1B.AH.RecDead.simple[SPA1B.AH.RecDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(1.1, 4.840000 ) #assume var from 2019

#make dataframe for all of AH Recruit Abundance
SPA1B.AH.RecDead.simple$cv <- sqrt(SPA1B.AH.RecDead.simple$var.y)/SPA1B.AH.RecDead.simple$Mean.nums
SPA1B.AH.RecDead.simple$Pop <- SPA1B.AH.RecDead.simple$Mean.nums*SPA1B.AH.RecDead.simple$NH

###### Commercial (80+ mm)
years <- 2001:surveyyear
X <- length(years)

SPA1B.AH.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(8626.27, X),Area = rep("AH", X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.AH.CommDead.simple$Year)) {
  temp.data <- deadfreq[deadfreq$YEAR == 2000 + i,]
SPA1B.AH.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
SPA1B.AH.CommDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.AH.CommDead.simple

#assume 1997-2000 same as MBN
SPA1B.AH.CommDead.simple <- rbind(SPA1B.MBN.CommDead[SPA1B.MBN.CommDead$Year < 2001,c(1:7)], SPA1B.AH.CommDead.simple)
SPA1B.AH.CommDead.simple[c(1:4), "NH"] <- 8626.27
SPA1B.AH.CommDead.simple[c(1:4), "Area"] <- "AH"

#interpolate missing years
#2004
approx(SPA1B.AH.CommDead.simple$Year, SPA1B.AH.CommDead.simple$Mean.nums, xout = 2004)# 8.6125
SPA1B.AH.CommDead.simple[SPA1B.AH.CommDead.simple$Year == 2004,"Mean.nums"] <- 8.6125
SPA1B.AH.CommDead.simple[SPA1B.AH.CommDead.simple$Year == 2004,"var.y"] <- 11.3700000
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.AH.CommDead.simple$Year, SPA1B.AH.CommDead.simple$Mean.nums, xout=2020) #   16.8125
SPA1B.AH.CommDead.simple[SPA1B.AH.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c( 16.8125, 685.9766667 ) #assume var from 2019


#make dataframe for all of AH Commercial Abundance
SPA1B.AH.CommDead.simple$cv <- sqrt(SPA1B.AH.CommDead.simple$var.y)/SPA1B.AH.CommDead.simple$Mean.nums
SPA1B.AH.CommDead.simple$Pop <- SPA1B.AH.CommDead.simple$Mean.nums*SPA1B.AH.CommDead.simple$NH

#combine Recruit and CommDeadercial dataframes
SPA1B.AH.ClapNumbers <- rbind(SPA1B.AH.RecDead.simple,SPA1B.AH.CommDead.simple)

####
# ---- 6. Spencer's Island ----
####

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID == 1
years <- 2005:surveyyear
X <- length(years)

SPA1B.SI.RecDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(20337.96,X), Area = rep("SI", X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.SI.RecDead.simple$Year)) {
  temp.data <- deadfreq[deadfreq$YEAR == 2004 + i,]
	SPA1B.SI.RecDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
	SPA1B.SI.RecDead.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.SI.RecDead.simple

#assume 1997-2000 same as MBN; assume 2000-2004 same as Outer
SPA1B.SI.RecDead.simple <- rbind(SPA1B.Out.DeadRec.simple[c(5:8),c(1:7)], SPA1B.SI.RecDead.simple)
SPA1B.SI.RecDead.simple <- rbind(SPA1B.MBN.RecDead[SPA1B.MBN.RecDead$Year < 2001,c(1:7)], SPA1B.SI.RecDead.simple)
SPA1B.SI.RecDead.simple[SPA1B.SI.RecDead.simple$Year < 2005, "NH"] <- 20337.96
SPA1B.SI.RecDead.simple[SPA1B.SI.RecDead.simple$Year < 2005, "Area"] <- "SI"

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.SI.RecDead.simple$Year, SPA1B.SI.RecDead.simple$Mean.nums, xout=2020) #  1.32
SPA1B.SI.RecDead.simple[SPA1B.SI.RecDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(1.32, 34.848000 ) #assume var from 2019


#calculate CV and Pop. estimate
SPA1B.SI.RecDead.simple$cv <- sqrt(SPA1B.SI.RecDead.simple$var.y)/SPA1B.SI.RecDead.simple$Mean.nums
SPA1B.SI.RecDead.simple$Pop <- SPA1B.SI.RecDead.simple$Mean.nums*SPA1B.SI.RecDead.simple$NH

############ Commercial (80+ mm)

#use simple mean for recruit, only TOW_TYPE_ID == 1
years <- 2005:surveyyear
X <- length(years)

SPA1B.SI.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(20337.96, X), Area = rep("SI", X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.SI.CommDead.simple$Year))  {
  temp.data <- deadfreq[deadfreq$YEAR == 2004 + i,]
	SPA1B.SI.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
	SPA1B.SI.CommDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.SI.CommDead.simple

#assume 1997-2000 same as MBN; assume 2001-2004 same as Outer
SPA1B.SI.CommDead.simple <- rbind(SPA1B.Out.CommDead.simple[c(5:8),c(1:7)], SPA1B.SI.CommDead.simple)
SPA1B.SI.CommDead.simple <- rbind(SPA1B.MBN.CommDead[SPA1B.MBN.CommDead$Year < 2001,c(1:7)], SPA1B.SI.CommDead.simple)
SPA1B.SI.CommDead.simple[SPA1B.SI.CommDead.simple$Year < 2005, "NH"] <- 20337.96
SPA1B.SI.CommDead.simple[SPA1B.SI.CommDead.simple$Year < 2005, "Area"] <- "SI"

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.SI.CommDead.simple$Year, SPA1B.SI.CommDead.simple$Mean.nums, xout=2020) #  2.93
SPA1B.SI.CommDead.simple[SPA1B.SI.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(2.93,  11.85800  ) #assume var from 2019

#calculate CV and Pop.estimate
SPA1B.SI.CommDead.simple$cv <- sqrt(SPA1B.SI.CommDead.simple$var.y)/SPA1B.SI.CommDead.simple$Mean.nums
SPA1B.SI.CommDead.simple$Pop <- SPA1B.SI.CommDead.simple$Mean.nums*SPA1B.SI.CommDead.simple$NH

#combine Recruit and Commercial dataframes
SPA1B.SI.ClapNumbers <- rbind(SPA1B.SI.RecDead.simple,SPA1B.SI.CommDead.simple)

####
# ---- 7. Scots Bay ----
####

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID == 1
years <- 2005:surveyyear
X <- length(years)

SPA1B.SB.RecDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(19415.98,X), Area = rep("SB", X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.SB.RecDead.simple$Year)) {
  temp.data <- deadfreq[deadfreq$YEAR == 2004 + i,]
	SPA1B.SB.RecDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
	SPA1B.SB.RecDead.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.SB.RecDead.simple

#assume 1997-2000 same as MBN; assume 2001-2004 same as Outer
SPA1B.SB.RecDead.simple <- rbind(SPA1B.Out.DeadRec.simple[c(5:8),c(1:7)], SPA1B.SB.RecDead.simple)
SPA1B.SB.RecDead.simple <- rbind(SPA1B.MBN.RecDead[SPA1B.MBN.RecDead$Year < 2001,c(1:7)], SPA1B.SB.RecDead.simple)
SPA1B.SB.RecDead.simple[SPA1B.SB.RecDead.simple$Year < 2005, "NH"] <- 19415.98
SPA1B.SB.RecDead.simple[SPA1B.SB.RecDead.simple$Year < 2005, "Area"] <- "SB"

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.SB.RecDead.simple$Year, SPA1B.SB.RecDead.simple$Mean.nums, xout=2020) #   5.55
SPA1B.SB.RecDead.simple[SPA1B.SB.RecDead.simple$Year==2020,c("Mean.nums","var.y")] <- c( 5.55,  42.902500  ) #assume var from 2019

#calculate CV and Pop. estimate
SPA1B.SB.RecDead.simple$cv <- sqrt(SPA1B.SB.RecDead.simple$var.y)/SPA1B.SB.RecDead.simple$Mean.nums
SPA1B.SB.RecDead.simple$Pop <- SPA1B.SB.RecDead.simple$Mean.nums*SPA1B.SB.RecDead.simple$NH

###### Commercial (80+ mm)

#use simple mean for recruit, only TOW_TYPE_ID == 1
years <- 2005:surveyyear
X <- length(years)

SPA1B.SB.CommDead.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(19415.98, X), Area = rep("SB", X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.SB.CommDead.simple$Year))  {
  temp.data <- deadfreq[deadfreq$YEAR == 2004 + i,]
	SPA1B.SB.CommDead.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
	SPA1B.SB.CommDead.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.SB.CommDead.simple

#assume 1997-2000 same as MBN; assume 2000-2004 same as Outer
SPA1B.SB.CommDead.simple <- rbind(SPA1B.Out.CommDead.simple[c(5:8),c(1:7)], SPA1B.SB.CommDead.simple)
SPA1B.SB.CommDead.simple <- rbind(SPA1B.MBN.CommDead[SPA1B.MBN.CommDead$Year < 2001,c(1:7)], SPA1B.SB.CommDead.simple)
SPA1B.SB.CommDead.simple[SPA1B.SB.CommDead.simple$Year < 2005, "NH"] <- 19415.98
SPA1B.SB.CommDead.simple[SPA1B.SB.CommDead.simple$Year < 2005, "Area"] <- "SB"
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.SB.CommDead.simple$Year, SPA1B.SB.CommDead.simple$Mean.nums, xout=2020) #  8.7625
SPA1B.SB.CommDead.simple[SPA1B.SB.CommDead.simple$Year==2020,c("Mean.nums","var.y")] <- c(8.7625,  21.9400  ) #assume var from 2019

#calculate CV and Pop.estimate
SPA1B.SB.CommDead.simple$cv <- sqrt(SPA1B.SB.CommDead.simple$var.y)/SPA1B.SB.CommDead.simple$Mean.nums
SPA1B.SB.CommDead.simple$Pop <- SPA1B.SB.CommDead.simple$Mean.nums*SPA1B.SB.CommDead.simple$NH

#combine Recruit and Commercial dataframes
SPA1B.SB.ClapNumbers <- rbind(SPA1B.SB.RecDead.simple,SPA1B.SB.CommDead.simple)

####
#  ---- Merge all strata areas to Make Numbers dataframe for SPA1b ----
####

SPA1B.ClapNumbers <- rbind(SPA1B.CS.ClapNumbers,SPA1B.MBN.ClapNumbers, SPA1B.28C.ClapNumbers, SPA1B.Out.ClapNumbers, SPA1B.AH.ClapNumbers, SPA1B.SI.ClapNumbers, SPA1B.SB.ClapNumbers)

write.csv(SPA1B.ClapNumbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1B.Index.Clappers.",surveyyear,".csv"))

####
###
###   ---- Calculate Commercial Clapper (N) Population size ----
###
####
#model only needs 1997+

SPA1B.CS.CommDead.simple.1997on <- SPA1B.CS.CommDead.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.MBN.CommDead.1997on <- SPA1B.MBN.CommDead %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.28C.CommDead.simple.1997on <- SPA1B.28C.CommDead.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.Out.CommDead.simple.1997on <- SPA1B.Out.CommDead.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.AH.CommDead.simple.1997on <- SPA1B.AH.CommDead.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.SI.CommDead.simple.1997on <- SPA1B.SI.CommDead.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.SB.CommDead.simple.1997on <- SPA1B.SB.CommDead.simple %>% filter(Year >= 1997) %>% select(Year, Pop)

#Ensure year ranges are the same: 
SPA1B.CS.CommDead.simple.1997on$Year == SPA1B.MBN.CommDead.1997on$Year 
SPA1B.MBN.CommDead.1997on$Year == SPA1B.28C.CommDead.simple.1997on$Year
SPA1B.28C.CommDead.simple.1997on$Year == SPA1B.Out.CommDead.simple.1997on$Year
SPA1B.Out.CommDead.simple.1997on$Year == SPA1B.AH.CommDead.simple.1997on$Year
SPA1B.AH.CommDead.simple.1997on$Year == SPA1B.SI.CommDead.simple.1997on$Year
SPA1B.SI.CommDead.simple.1997on$Year == SPA1B.SB.CommDead.simple.1997on$Year

#Combine into single dataframe of population clappers 
clappers.N <- data.frame(Year = SPA1B.CS.CommDead.simple.1997on$Year,  
                         N = (SPA1B.CS.CommDead.simple.1997on$Pop + 
                                SPA1B.MBN.CommDead.1997on$Pop + 
                                SPA1B.28C.CommDead.simple.1997on$Pop + 
                                SPA1B.Out.CommDead.simple.1997on$Pop + 
                                SPA1B.AH.CommDead.simple.1997on$Pop + 
                                SPA1B.SI.CommDead.simple.1997on$Pop + 
                                SPA1B.SB.CommDead.simple.1997on$Pop ))
clappers.N$N

write.csv(clappers.N, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1B.Clappers.N.formodel.",surveyyear,".csv"))


####
###
### ----  Plot Clapper No/tow ----
###
####
data <- SPA1B.ClapNumbers
data$Size <- data$Age 
data$Mean.nums[data$Year==2020] <- NA #since don't want 2020 to plot in figures 

SPA1B.ClapNumbers.per.tow.plot <- ggplot(data = data, aes(x=Year, y=Mean.nums, col=Size, pch = Size)) +
  geom_point() + geom_line(aes(linetype = Size)) + facet_wrap(~Area) + 
  theme_bw() + 
  ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c('black','red'))
SPA1B.ClapNumbers.per.tow.plot

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_ClapperNumberPerTow_byStrata",surveyyear,".png"), type="cairo", width=35, height=25, units = "cm", res=300)
SPA1B.ClapNumbers.per.tow.plot
dev.off() 


####
###
### ---- CALCULATE SHF FOR EACH YEAR BY STRATA   (use yrs:1998+ for all areas) ----
###

# ---- 1. Cape Spencer ----
CSdeadfreq <- subset(deadfreq, STRATA_ID == 37 & TOW_TYPE_ID == 1 & YEAR >= 1998)
SPA1B.CS.SHFdead <- sapply(split(CSdeadfreq[c(11:50)], CSdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.CS.SHFdead,2)
SPA1B.CS.SHFdead <- as.data.frame(SPA1B.CS.SHFdead)
SPA1B.CS.SHFdead$'2020' <- NA
SPA1B.CS.SHFdead <- SPA1B.CS.SHFdead %>% relocate('2020', .after = '2019')

#### PLOT ###
names(SPA1B.CS.SHFdead)

names(SPA1B.CS.SHFdead)[1:dim(SPA1B.CS.SHFdead)[2]] <- c(paste0("X",c(seq(1998,surveyyear)))) 
SPA1B.CS.SHFdead$bin.label <- row.names(SPA1B.CS.SHFdead)

head(SPA1B.CS.SHFdead)
SPA1B.CS.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.CS.SHFdead <- pivot_longer(SPA1B.CS.SHFdead, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SH",
                                            values_drop_na = FALSE)
SPA1B.CS.SHFdead$year <- as.numeric(SPA1B.CS.SHFdead$year)

SPA1B.CS.SHFdead.for.plot <- SPA1B.CS.SHFdead %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.CS.SHFdead.for.plot$SH <- round(SPA1B.CS.SHFdead.for.plot$SH,3)


#ylimits <- c(0,10)
ylimits <- c(0,round_any(max(SPA1B.CS.SHFdead.for.plot$SH, na.rm = TRUE), 10, f = ceiling)) 
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.CS.SHFdead <- ggplot() + geom_col(data = SPA1B.CS.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.CS.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B.CS_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1B.CS.SHFdead
dev.off()


# ----  MBN -----
#  2a. MBN - East 
MBNEdeadfreq <- subset(deadfreq, STRATA_ID == 58 & TOW_TYPE_ID == 1 & YEAR >= 1998)
SPA1B.MBNE.SHFdead <- sapply(split(MBNEdeadfreq[c(11:50)],MBNEdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.MBNE.SHFdead)
SPA1B.MBNE.SHFdead <- as.data.frame(SPA1B.MBNE.SHFdead)
SPA1B.MBNE.SHFdead$'2020' <- NA
SPA1B.MBNE.SHFdead <- SPA1B.MBNE.SHFdead %>% relocate('2020', .after = '2019')

names(SPA1B.MBNE.SHFdead)
names(SPA1B.MBNE.SHFdead)[1:dim(SPA1B.MBNE.SHFdead)[2]] <- c(paste0("X",c(seq(1998,surveyyear)))) 

SPA1B.MBNE.SHFdead$bin.label <- row.names(SPA1B.MBNE.SHFdead)

head(SPA1B.MBNE.SHFdead)
SPA1B.MBNE.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.MBNE.SHFdead <- pivot_longer(SPA1B.MBNE.SHFdead, 
                                   cols = starts_with("X"),
                                   names_to = "year",
                                   names_prefix = "X",
                                   values_to = "SH.MBNE",
                                   values_drop_na = FALSE)
SPA1B.MBNE.SHFdead$year <- as.numeric(SPA1B.MBNE.SHFdead$year)
table(SPA1B.MBNE.SHFdead$year)

# 2b. MBN - West 
MBNWdeadfreq <- subset(deadfreq, STRATA_ID == 38 & TOW_TYPE_ID == 1 & YEAR >= 1998)
SPA1B.MBNW.SHFdead <- sapply(split(MBNWdeadfreq[c(11:50)],MBNWdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.MBNW.SHFdead,2)
SPA1B.MBNW.SHFdead <- as.data.frame(SPA1B.MBNW.SHFdead)
SPA1B.MBNW.SHFdead$'1998' <- NA
SPA1B.MBNW.SHFdead$'1999' <- NA
SPA1B.MBNW.SHFdead$'2000' <- NA
SPA1B.MBNW.SHFdead$'2001' <- NA
SPA1B.MBNW.SHFdead$'2004' <- NA
SPA1B.MBNW.SHFdead$'2020' <- NA

SPA1B.MBNW.SHFdead <- SPA1B.MBNW.SHFdead %>% relocate('1998')
SPA1B.MBNW.SHFdead <- SPA1B.MBNW.SHFdead %>% relocate('1999', .after = '1998')
SPA1B.MBNW.SHFdead <- SPA1B.MBNW.SHFdead %>% relocate('2000', .after = '1999')
SPA1B.MBNW.SHFdead <- SPA1B.MBNW.SHFdead %>% relocate('2001', .after = '2000')
SPA1B.MBNW.SHFdead <- SPA1B.MBNW.SHFdead %>% relocate('2004', .after = '2003')
SPA1B.MBNW.SHFdead <- SPA1B.MBNW.SHFdead %>% relocate('2020', .after = '2019')

names(SPA1B.MBNW.SHFdead)

names(SPA1B.MBNW.SHFdead)[1:dim(SPA1B.MBNW.SHFdead)[2]] <- c(paste0("X",c(seq(1998,surveyyear))))

SPA1B.MBNW.SHFdead$bin.label <- row.names(SPA1B.MBNW.SHFdead)

head(SPA1B.MBNW.SHFdead)
SPA1B.MBNW.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.MBNW.SHFdead <- pivot_longer(SPA1B.MBNW.SHFdead, 
                                   cols = starts_with("X"),
                                   names_to = "year",
                                   names_prefix = "X",
                                   values_to = "SH.MBNW",
                                   values_drop_na = FALSE)
SPA1B.MBNW.SHFdead$year <- as.numeric(SPA1B.MBNW.SHFdead$year)
table(SPA1B.MBNW.SHFdead$year)

#merge SPA1B.MBNW.SHFdead & SPA1B.MBNW.SHFdead; For years where MBNW were NA but MBNE were NOT NA assume MBNE applies to all of MBNW; for 2020 have NA 
#check same length 
dim(SPA1B.MBNW.SHFdead)[1] == dim(SPA1B.MBNE.SHFdead)[1]
SPA1B.MBN.SHFdead <- merge(SPA1B.MBNW.SHFdead, SPA1B.MBNE.SHFdead, by = c("year", "bin.label", "bin.mid.pt"))
#For years where MBNW were NA but MBNE were NOT NA assume MBNE applies to all of MBNW
SPA1B.MBN.SHFdead$SH.MBNW[SPA1B.MBN.SHFdead$year %in% c(1998,1999,2000,2001,2004)] <- SPA1B.MBN.SHFdead$SH.MBNE[SPA1B.MBN.SHFdead$year %in% c(1998,1999,2000,2001,2004)]
SPA1B.MBN.SHFdead$SH <- (SPA1B.MBN.SHFdead$SH.MBNE*0.732) + (SPA1B.MBN.SHFdead$SH.MBNW*0.268)

#add rows to SPA1B.MBNW.SHFdead matrix so it can be mutiplied with MBNE
#temp <- cbind(SPA1B.MBNW.SHFdead[, 1:3], matrix(0, nrow = 40), SPA1B.MBNW.SHFdead[,4:20]) # update "SPA1B.MBNW.SHFdead[,4:16]" for added year
#SPA1B.MBNW.SHFdead <- cbind(temp[, 1:6], matrix(0, nrow = 40), temp[,7:21]) # update "temp[,7:17]" for added year
#SPA1B.MBN.SHFdead <- (SPA1B.MBNE.SHFdead[,10:31]*0.732 ) + (SPA1B.MBNW.SHFdead*0.268) # update "SPA1B.MBNE.SHFdead[,10:27]" for added year


#### PLOT ###
names(SPA1B.MBN.SHFdead)

SPA1B.MBN.SHFdead.for.plot <- SPA1B.MBN.SHFdead %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.MBN.SHFdead.for.plot$SH <- round(SPA1B.MBN.SHFdead.for.plot$SH,3)

#ylimits <- c(0,10)
ylimits <- c(0,round_any(max(SPA1B.MBN.SHFdead.for.plot$SH, na.rm = TRUE), 10, f = ceiling)) 
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.MBN.SHFdead <- ggplot() + geom_col(data = SPA1B.MBN.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.MBN.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B.MBN_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1B.MBN.SHFdead
dev.off()


# ---- 3.Upper Bay 28C ----
deadfreq28C <- subset(deadfreq, STRATA_ID == 53 & TOW_TYPE_ID == 1)  #Only data from 2001 on and no data in 2004 
SPA1B.28C.SHFdead <- sapply(split(deadfreq28C[c(11:50)], deadfreq28C$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.28C.SHFdead, 2)
SPA1B.28C.SHFdead <- as.data.frame(SPA1B.28C.SHFdead)
SPA1B.28C.SHFdead$'2004' <- NA
SPA1B.28C.SHFdead$'2020' <- NA
SPA1B.28C.SHFdead <- SPA1B.28C.SHFdead %>% relocate('2004', .after = '2003')
SPA1B.28C.SHFdead <- SPA1B.28C.SHFdead %>% relocate('2020', .after = '2019')

#### PLOT ###
names(SPA1B.28C.SHFdead)

names(SPA1B.28C.SHFdead)[1:dim(SPA1B.28C.SHFdead)[2]] <- c(paste0("X",c(seq(2001,surveyyear)))) 

SPA1B.28C.SHFdead$bin.label <- row.names(SPA1B.28C.SHFdead)

head(SPA1B.28C.SHFdead)
SPA1B.28C.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.28C.SHFdead <- pivot_longer(SPA1B.28C.SHFdead, 
                                 cols = starts_with("X"),
                                 names_to = "year",
                                 names_prefix = "X",
                                 values_to = "SH",
                                 values_drop_na = FALSE)
SPA1B.28C.SHFdead$year <- as.numeric(SPA1B.28C.SHFdead$year)

SPA1B.28C.SHFdead.for.plot <- SPA1B.28C.SHFdead %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.28C.SHFdead.for.plot$SH <- round(SPA1B.28C.SHFdead.for.plot$SH,3)


ylimits <- c(0,15)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.28C.SHFdead <- ggplot() + geom_col(data = SPA1B.28C.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.28C.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B.28C_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1B.28C.SHFdead
dev.off()


# ---- 4. Advocate Harbour ----
AHdeadfreq <- subset(deadfreq, STRATA_ID == 35 & TOW_TYPE_ID == 1) #since 2001 and no 2004 
SPA1B.AH.SHFdead <- sapply(split(AHdeadfreq[c(11:50)], AHdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.AH.SHFdead, 2)
SPA1B.AH.SHFdead <- as.data.frame(SPA1B.AH.SHFdead)
SPA1B.AH.SHFdead$'2004' <- NA
SPA1B.AH.SHFdead$'2020' <- NA
SPA1B.AH.SHFdead <- SPA1B.AH.SHFdead %>% relocate('2004', .after = '2003')
SPA1B.AH.SHFdead <- SPA1B.AH.SHFdead %>% relocate('2020', .after = '2019')


#### PLOT ###
names(SPA1B.AH.SHFdead)

names(SPA1B.AH.SHFdead)[1:dim(SPA1B.AH.SHFdead)[2]] <- c(paste0("X",c(seq(2001,surveyyear)))) 

SPA1B.AH.SHFdead$bin.label <- row.names(SPA1B.AH.SHFdead)

head(SPA1B.AH.SHFdead)
SPA1B.AH.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.AH.SHFdead <- pivot_longer(SPA1B.AH.SHFdead, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA1B.AH.SHFdead$year <- as.numeric(SPA1B.AH.SHFdead$year)

SPA1B.AH.SHFdead.for.plot <- SPA1B.AH.SHFdead %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.AH.SHFdead.for.plot$SH <- round(SPA1B.AH.SHFdead.for.plot$SH,3)


ylimits <- c(0,15)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.AH.SHFdead <- ggplot() + geom_col(data = SPA1B.AH.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.AH.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B.AH_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1B.AH.SHFdead
dev.off()

# ---- 5. 28D Outer Day ----
Outdeadfreq <- subset(deadfreq, STRATA_ID == 49 & TOW_TYPE_ID == 1) #since 2001 and no 2004 
SPA1B.Out.SHFdead <- sapply(split(Outdeadfreq[c(11:50)], Outdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.Out.SHFdead, 2)
SPA1B.Out.SHFdead <- as.data.frame(SPA1B.Out.SHFdead)
SPA1B.Out.SHFdead$'2004' <- NA
SPA1B.Out.SHFdead$'2020' <- NA
SPA1B.Out.SHFdead <- SPA1B.Out.SHFdead %>% relocate('2004', .after = '2003')
SPA1B.Out.SHFdead <- SPA1B.Out.SHFdead %>% relocate('2020', .after = '2019')


#### PLOT ###
names(SPA1B.Out.SHFdead)

names(SPA1B.Out.SHFdead)[1:dim(SPA1B.Out.SHFdead)[2]] <- c(paste0("X",c(seq(2001,surveyyear)))) 

SPA1B.Out.SHFdead$bin.label <- row.names(SPA1B.Out.SHFdead)

head(SPA1B.Out.SHFdead)
SPA1B.Out.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.Out.SHFdead <- pivot_longer(SPA1B.Out.SHFdead, 
                                 cols = starts_with("X"),
                                 names_to = "year",
                                 names_prefix = "X",
                                 values_to = "SH",
                                 values_drop_na = FALSE)
SPA1B.Out.SHFdead$year <- as.numeric(SPA1B.Out.SHFdead$year)

SPA1B.Out.SHFdead.for.plot <- SPA1B.Out.SHFdead %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.Out.SHFdead.for.plot$SH <- round(SPA1B.Out.SHFdead.for.plot$SH,3)


ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.Out.SHFdead <- ggplot() + geom_col(data = SPA1B.Out.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.Out.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B.Out_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1B.Out.SHFdead
dev.off()



# ---- 6. Spencers Island ----
SIdeadfreq <- subset(deadfreq, STRATA_ID == 52 & TOW_TYPE_ID == 1) #since 2005 
SPA1B.SI.SHFdead <- sapply(split(SIdeadfreq[c(11:50)], SIdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.SI.SHFdead, 2)
SPA1B.SI.SHFdead <- as.data.frame(SPA1B.SI.SHFdead)
SPA1B.SI.SHFdead$'2020' <- NA
SPA1B.SI.SHFdead <- SPA1B.SI.SHFdead %>% relocate('2020', .after = '2019')


#### PLOT ###
names(SPA1B.SI.SHFdead)

names(SPA1B.SI.SHFdead)[1:dim(SPA1B.SI.SHFdead)[2]] <- c(paste0("X",c(seq(2005,surveyyear)))) 

SPA1B.SI.SHFdead$bin.label <- row.names(SPA1B.SI.SHFdead)

head(SPA1B.SI.SHFdead)
SPA1B.SI.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.SI.SHFdead <- pivot_longer(SPA1B.SI.SHFdead, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA1B.SI.SHFdead$year <- as.numeric(SPA1B.SI.SHFdead$year)

SPA1B.SI.SHFdead.for.plot <- SPA1B.SI.SHFdead %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.SI.SHFdead.for.plot$SH <- round(SPA1B.SI.SHFdead.for.plot$SH,3)


ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.SI.SHFdead <- ggplot() + geom_col(data = SPA1B.SI.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.SI.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B.SI_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1B.SI.SHFdead
dev.off()



# ---- 7. Scots Bay ----
SBdeadfreq <- subset(deadfreq, STRATA_ID == 51 & TOW_TYPE_ID == 1) #Since 2005 
SPA1B.SB.SHFdead <- sapply(split(SBdeadfreq[c(11:50)], SBdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.SB.SHFdead, 2)
SPA1B.SB.SHFdead <- as.data.frame(SPA1B.SB.SHFdead)
SPA1B.SB.SHFdead$'2020' <- NA
SPA1B.SB.SHFdead <- SPA1B.SB.SHFdead %>% relocate('2020', .after = '2019')


#### PLOT ###
names(SPA1B.SB.SHFdead)

names(SPA1B.SB.SHFdead)[1:dim(SPA1B.SB.SHFdead)[2]] <- c(paste0("X",c(seq(2005,surveyyear)))) 

SPA1B.SB.SHFdead$bin.label <- row.names(SPA1B.SB.SHFdead)

head(SPA1B.SB.SHFdead)
SPA1B.SB.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.SB.SHFdead <- pivot_longer(SPA1B.SB.SHFdead, 
                                 cols = starts_with("X"),
                                 names_to = "year",
                                 names_prefix = "X",
                                 values_to = "SH",
                                 values_drop_na = FALSE)
SPA1B.SB.SHFdead$year <- as.numeric(SPA1B.SB.SHFdead$year)

SPA1B.SB.SHFdead.for.plot <- SPA1B.SB.SHFdead %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.SB.SHFdead.for.plot$SH <- round(SPA1B.SB.SHFdead.for.plot$SH,3)


ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.SB.SHFdead <- ggplot() + geom_col(data = SPA1B.SB.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.SB.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B.SB_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1B.SB.SHFdead
dev.off()



### END OF SCRIPT ###