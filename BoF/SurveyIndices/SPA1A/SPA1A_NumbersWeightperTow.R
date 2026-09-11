###........................................###
###
###    SPA 1A
###    Numbers per tow, Weight per tow
###    Population numbers and biomass
###
###
###   Rehauled July 2020 J.Sameoto
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(PEDstrata) #v.1.0.2
library(cowplot)

# source strata definitions
source("Y:/Inshore/Assessment/BoF/SurveyDesignTables/BoFstratadef.R")
#source("Y:/Inshore/BoF/SurveyDesignTables/BoFstratadef.R")

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
#strata.spa1a<-c(6,7,12:20,39)

#SQL - numbers by shell height bin   #code will exclude cruises not to be used: UB% and JJ%
quer1 <- (                  
"SELECT *
FROM scallsur.scliveres
WHERE strata_id IN (6,7,12,13,14,15,16,17,18,19,20,39)
AND (cruise LIKE 'BA%'
OR cruise LIKE 'BF%'
OR cruise LIKE 'BI%'
OR cruise LIKE 'GM%'
OR cruise LIKE 'RF%')")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle; numbers by shell height bin
livefreq <- dbGetQuery(chan, quer1)

#add YEAR column to data
livefreq$YEAR <- as.numeric(substr(livefreq$CRUISE,3,6))
table(livefreq$YEAR)

###
# read in meat weight data; this is output from the meat weight/shell height modelling
###

#code for reading in multiple csvs at once and combining into one dataframe from D.Keith (2015)
Year <- c(seq(1997,2019),seq(2021,surveyyear))  
num.years <- length(Year)

BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",Year[i],".csv",sep="") ,header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

#add YEAR column to data
BFliveweight$YEAR <- as.numeric(substr(BFliveweight$CRUISE,3,6))

#check data structure
summary(BFliveweight)
str(BFliveweight)
table(BFliveweight$YEAR)

####
###
### ---- RECRUIT AND COMMERCIAL NUMBERS PER TOW AND POPULATION NUMBERS ---- 
###
####

###
# --- 2 to 8 mile ---- 
###
years <- 1984:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.2to8.Rec <- data.frame(Year=years, Mean.nums=rep(NA,X), Pop=rep(NA,X) ,method=rep("PED",X), Area=rep("2to8", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.2to8.Rec$Year)){
  if (years[i] != 2020) { 
temp.data<-livefreq[livefreq$YEAR==1983+i,]
SPA1A.2to8.Rec[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.Rec[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
}}
SPA1A.2to8.Rec

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.Rec$Year, SPA1A.2to8.Rec$Mean.nums, xout=2020) #  4.5221 Mean numbers 
SPA1A.2to8.Rec[SPA1A.2to8.Rec$Year==2020,"Mean.nums"] <- 4.5221
SPA1A.2to8.Rec[SPA1A.2to8.Rec$Year==2020,"Pop"] <- SPA1A.2to8.Rec[SPA1A.2to8.Rec$Year==2020,"Mean.nums"]*51945 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA1A.2to8.new$NH); check by seeing if matchs: 
SPA1A.2to8.Rec$Pop/SPA1A.2to8.Rec$Mean.nums
#Yes is 51945


#Commercial no/tow and population
SPA1A.2to8.Comm <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("2to8", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.2to8.Comm$Year)){
  if (years[i] != 2020) {
temp.data<-livefreq[livefreq$YEAR==1983+i,]
SPA1A.2to8.Comm[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.Comm[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA1A.2to8.Comm

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.Comm$Year, SPA1A.2to8.Comm$Mean.nums, xout=2020) #  105.09 Mean numbers 
SPA1A.2to8.Comm[SPA1A.2to8.Comm$Year==2020,"Mean.nums"] <- 105.09
SPA1A.2to8.Comm[SPA1A.2to8.Comm$Year==2020,"Pop"] <- SPA1A.2to8.Comm[SPA1A.2to8.Comm$Year==2020,"Mean.nums"]*51945 #calculate population number from interpolated no/tow; MUST ; Bumping by sum(strata.SPA1A.2to8.new$NH); check by seeing if matchs: 
SPA1A.2to8.Rec$Pop/SPA1A.2to8.Rec$Mean.nums

#combine Recruit and Commercial dataframes for 2 to 8
SPA1A.2to8.Numbers <- rbind(SPA1A.2to8.Rec, SPA1A.2to8.Comm)


###
# ---- 8 to 16 mile ----
###
years <- 1981:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.8to16.Rec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.8to16.Rec$Year)){
  if (years[i] != 2020) {
temp.data<-livefreq[livefreq$YEAR==1980+i,]
SPA1A.8to16.Rec[i,2]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.Rec[i,3]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA1A.8to16.Rec

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.Rec$Year, SPA1A.8to16.Rec$Mean.nums, xout=2020) #  1.0001 Mean numbers 
SPA1A.8to16.Rec[SPA1A.8to16.Rec$Year==2020,"Mean.nums"] <- 1.0001
SPA1A.8to16.Rec[SPA1A.8to16.Rec$Year==2020,"Pop"] <- SPA1A.8to16.Rec[SPA1A.8to16.Rec$Year==2020,"Mean.nums"]*199441 #calculate population number from interpolated no/tow; MUST ; Bumping by sum(strata.SPA1A.8to16.noctrville.new$NH); check by seeing if matchs: 
SPA1A.8to16.Rec$Pop/SPA1A.8to16.Rec$Mean.nums

#Commercial no/tow and population
SPA1A.8to16.Comm <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.8to16.Comm$Year)){
  if (years[i] != 2020) {
temp.data<-livefreq[livefreq$YEAR==1980+i,]
SPA1A.8to16.Comm[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.Comm[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA1A.8to16.Comm

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.Comm$Year, SPA1A.8to16.Comm$Mean.nums, xout=2020) #  189.38 Mean numbers 
SPA1A.8to16.Comm[SPA1A.8to16.Comm$Year==2020,"Mean.nums"] <- 189.38
SPA1A.8to16.Comm[SPA1A.8to16.Comm$Year==2020,"Pop"] <- SPA1A.8to16.Comm[SPA1A.8to16.Comm$Year==2020,"Mean.nums"]*199441 #calculate population number from interpolated no/tow; MUST ; Bumping by sum(strata.SPA1A.8to16.noctrville.new$NH); check by seeing if matchs: 
SPA1A.8to16.Comm$Pop/SPA1A.8to16.Comm$Mean.nums

#combine Recruit and Commercial dataframes for 8to16
SPA1A.8to16.Numbers <- rbind(SPA1A.8to16.Rec, SPA1A.8to16.Comm)

###
# --- MidBay South ----
###
years <- 1997:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.MBS.Rec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("MBS", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.MBS.Rec$Year)){
  if (years[i] != 2020) {
temp.data<-livefreq[livefreq$YEAR==1996+i,]
SPA1A.MBS.Rec[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1A.MBS.Rec[i,3] <- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))*201138.52
} }
SPA1A.MBS.Rec

#interpolate missing years
approx(SPA1A.MBS.Rec$Year, SPA1A.MBS.Rec$Mean.nums, xout=2003) #5.1885
approx(SPA1A.MBS.Rec$Year, SPA1A.MBS.Rec$Mean.nums, xout=2004) #6.9255

SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2003,"Mean.nums"] <- 5.1885
SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2004,"Mean.nums"] <- 6.9255
SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2003,"Pop"] <- SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2003,"Mean.nums"]*201138.52 #calculate population number from interpolated no/tow
SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2004,"Pop"] <- SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2004,"Mean.nums"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.Rec$Year, SPA1A.MBS.Rec$Mean.nums, xout=2020) #  5.4595 Mean numbers 
SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2020,"Mean.nums"] <- 5.4595
SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2020,"Pop"] <- SPA1A.MBS.Rec[SPA1A.MBS.Rec$Year==2020,"Mean.nums"]*201138.52 #calculate population number from interpolated no/to
#Check on number used to bump to population level 
SPA1A.MBS.Rec$Pop/SPA1A.MBS.Rec$Mean.nums


#Commercial no/tow and population
SPA1A.MBS.Com <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X) , method=rep("PED",X), Area=rep("MBS", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.MBS.Com$Year)){
  if (years[i] != 2020) {
temp.data<-livefreq[livefreq$YEAR==1996+i,]
SPA1A.MBS.Com[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
SPA1A.MBS.Com[i,3] <- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))*201138.52
} }
SPA1A.MBS.Com

#assume 1997 same at 2to8
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==1997,"Mean.nums"]<- 37.228

#interpolate missing years
approx(SPA1A.MBS.Com$Year, SPA1A.MBS.Com$Mean.nums, xout=2003) #86.304
approx(SPA1A.MBS.Com$Year, SPA1A.MBS.Com$Mean.nums, xout=2004) #95.14
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2003,"Mean.nums"] <- 86.304
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2004,"Mean.nums"] <- 95.14

#calculate population number from interpolated no/tow
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==1997,"Pop"] <- SPA1A.MBS.Com[SPA1A.MBS.Com$Year==1997,"Mean.nums"]*201138.52
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2003,"Pop"] <- SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2003,"Mean.nums"]*201138.52
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2004,"Pop"] <- SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2004,"Mean.nums"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.Com$Year, SPA1A.MBS.Com$Mean.nums, xout=2020) #  66.282 Mean numbers 
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2020,"Mean.nums"] <- 66.282
SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2020,"Pop"] <- SPA1A.MBS.Com[SPA1A.MBS.Com$Year==2020,"Mean.nums"]*201138.52 #calculate population number from interpolated no/to
#Check on number used to bump to population level 
SPA1A.MBS.Com$Pop/SPA1A.MBS.Com$Mean.nums


#combine Recruit and Commercial dataframes for 8to16
SPA1A.MBS.Numbers <- rbind(SPA1A.MBS.Rec, SPA1A.MBS.Com)

###
#  --- Make Numbers dataframe for SPA1A indices by strata area ----
###
SPA1A.Numbers <- rbind(SPA1A.2to8.Numbers, SPA1A.8to16.Numbers, SPA1A.MBS.Numbers)

write.csv(SPA1A.Numbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A.Index.Numbers.",surveyyear,".csv"))


###
###
### ---- RECRUIT AND COMMERCIAL WEIGHT PER TOW  ----
###
###

###
# ---- 2 to 8 mile ----
###
years <- 1997:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.2to8.RecWt <- data.frame(Year=years, Mean.wt=rep(NA,X), Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("2to8", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.2to8.RecWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.2to8.RecWt[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.RecWt[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA1A.2to8.RecWt[i,4]<-summary (PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst^2
} }
SPA1A.2to8.RecWt

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.RecWt$Year, SPA1A.2to8.RecWt$Mean.wt , xout=2020) #  22.664 Mean numbers 
SPA1A.2to8.RecWt[SPA1A.2to8.RecWt$Year==2020,"Mean.wt"] <- 22.664
SPA1A.2to8.RecWt[SPA1A.2to8.RecWt$Year==2020,"var"] <- 112.7074 #assume 2019 variance 
SPA1A.2to8.RecWt[SPA1A.2to8.RecWt$Year==2020,"Bmass"] <- SPA1A.2to8.RecWt[SPA1A.2to8.RecWt$Year==2020,"Mean.wt"]*51945 #calculate population number from interpolated no/to
#Check on number used to bump to population level ;  sum(strata.SPA1A.2to8.new$NH)
SPA1A.2to8.RecWt$Bmass/SPA1A.2to8.RecWt$Mean.wt


#Commercial no/tow and population
SPA1A.2to8.CommWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("2to8", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.2to8.CommWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.2to8.CommWt[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new,"STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.CommWt[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new,"STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA1A.2to8.CommWt[i,4]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst^2
} }
SPA1A.2to8.CommWt

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.CommWt$Year, SPA1A.2to8.CommWt$Mean.wt , xout=2020) #  XXXXX Mean numbers 
SPA1A.2to8.CommWt[SPA1A.2to8.CommWt$Year==2020,"Mean.wt"] <-  2040
SPA1A.2to8.CommWt[SPA1A.2to8.CommWt$Year==2020,"var"] <- 465559 #assume 2019 variance 
SPA1A.2to8.CommWt[SPA1A.2to8.CommWt$Year==2020,"Bmass"] <- SPA1A.2to8.CommWt[SPA1A.2to8.CommWt$Year==2020,"Mean.wt"]*51945 #calculate population number from interpolated no/to
#Check on number used to bump to population level ;  sum(strata.SPA1A.2to8.new$NH)
SPA1A.2to8.CommWt$Bmass/SPA1A.2to8.CommWt$Mean.wt

#combine Recruit and Commercial dataframes for 2 to 8
SPA1A.2to8.Weight <- rbind(SPA1A.2to8.RecWt, SPA1A.2to8.CommWt)
SPA1A.2to8.Weight$kg <- SPA1A.2to8.Weight$Mean.wt/1000

####
# ---- 8 to 16 mile ----
####
years <- 1997:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.8to16.RecWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.8to16.RecWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.8to16.RecWt[i,2]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.RecWt[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA1A.8to16.RecWt[i,4]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst^2
}}
SPA1A.8to16.RecWt

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.RecWt$Year, SPA1A.8to16.RecWt$Mean.wt , xout=2020) #  4.5351 Mean numbers 
SPA1A.8to16.RecWt[SPA1A.8to16.RecWt$Year==2020,"Mean.wt"] <- 4.5351
SPA1A.8to16.RecWt[SPA1A.8to16.RecWt$Year==2020,"var"] <- 9.8371e+00 #assume 2019 variance 
SPA1A.8to16.RecWt[SPA1A.8to16.RecWt$Year==2020,"Bmass"] <- SPA1A.8to16.RecWt[SPA1A.8to16.RecWt$Year==2020,"Mean.wt"]*199441 #calculate population number from interpolated no /tow
#Check on number used to bump to population level ;  sum(strata.SPA1A.8to16.noctrville.new$NH)
SPA1A.8to16.RecWt$Bmass/SPA1A.8to16.RecWt$Mean.wt

#Commercial no/tow and population
SPA1A.8to16.CommWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X),var=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.8to16.CommWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.8to16.CommWt[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.CommWt[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA1A.8to16.CommWt[i,4]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst^2
} }
SPA1A.8to16.CommWt

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.CommWt$Year, SPA1A.8to16.CommWt$Mean.wt , xout=2020) #  3018.2 Mean numbers 
SPA1A.8to16.CommWt[SPA1A.8to16.CommWt$Year==2020,"Mean.wt"] <- 3018.2
SPA1A.8to16.CommWt[SPA1A.8to16.CommWt$Year==2020,"var"] <- 53016.6 #assume 2019 variance 
SPA1A.8to16.CommWt[SPA1A.8to16.CommWt$Year==2020,"Bmass"] <- SPA1A.8to16.CommWt[SPA1A.8to16.CommWt$Year==2020,"Mean.wt"]*199441 #calculate population number from interpolated no /tow
#Check on number used to bump to population level ;  sum(strata.SPA1A.8to16.noctrville.new$NH)
SPA1A.8to16.CommWt$Bmass/SPA1A.8to16.CommWt$Mean.wt

#combine Recruit and Commercial dataframes for 8to16
SPA1A.8to16.Weight <- rbind(SPA1A.8to16.RecWt, SPA1A.8to16.CommWt)
SPA1A.8to16.Weight$kg <- SPA1A.8to16.Weight$Mean.wt/1000

###
# ---- MidBay South ----
###
years <- 1997:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.MBS.RecWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("MBS", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.MBS.RecWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.MBS.RecWt[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
SPA1A.MBS.RecWt[i,3]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))*201138.52
SPA1A.MBS.RecWt[i,4]<- var(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
} }
SPA1A.MBS.RecWt

#assume 1997 same at 2to8
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==1997,c(2,4)] <- c(12.0296,26.0181)
#interpolate missing years
approx(SPA1A.MBS.RecWt$Year, SPA1A.MBS.RecWt$Mean.wt, xout=2003) #19.922
approx(SPA1A.MBS.RecWt$Year, SPA1A.MBS.RecWt$Mean.wt, xout=2004) #26.303

SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2003,c(2,4)]<-c(19.922,11.3469) #assume variance from 2002
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2004,c(2,4)]<-c(26.303,11.3469) #assume variance from 2002
#calculate population number from interpolated no/tow
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==1997,"Bmass"]<-SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==1997,"Mean.wt"]*201138.52
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2003,"Bmass"]<-SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2003,"Mean.wt"]*201138.52
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2004,"Bmass"]<-SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2004,"Mean.wt"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.RecWt$Year, SPA1A.MBS.RecWt$Mean.wt , xout=2020) #  32.439 Mean numbers 
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2020,"Mean.wt"] <- 32.439
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2020,"var"] <- 1026.327 #assume 2019 variance 
SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2020,"Bmass"] <- SPA1A.MBS.RecWt[SPA1A.MBS.RecWt$Year==2020,"Mean.wt"]*201138.52 #calculate population number from interpolated no /tow
#Check on number used to bump to population level 
SPA1A.MBS.RecWt$Bmass/SPA1A.MBS.RecWt$Mean.wt


#Commercial no/tow and population
SPA1A.MBS.ComWt <- data.frame(Year=years, Mean.wt=rep(NA,X), Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("MBS", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.MBS.ComWt$Year)){
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.MBS.ComWt[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
SPA1A.MBS.ComWt[i,3]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))*201138.52
SPA1A.MBS.ComWt[i,4]<-var (apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
}
SPA1A.MBS.ComWt

#assume 1997 same at 2to8
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==1997,c(2,4)]<-c(556.20,14894)

#interpolate missing years
approx(SPA1A.MBS.ComWt$Year, SPA1A.MBS.ComWt$Mean.wt, xout=2003) #1253.1
approx(SPA1A.MBS.ComWt$Year, SPA1A.MBS.ComWt$Mean.wt, xout=2004) # 1373.7
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2003,c(2,4)]<-c(1253.1,70269.4)#assume 2002 var
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2004,c(2,4)]<-c(1373.7,70269.4)#assume 2002 var
#calculate population number from interpolated no/tow
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==1997,"Bmass"]<-SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==1997,"Mean.wt"]*201138.52
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2003,"Bmass"]<-SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2003,"Mean.wt"]*201138.52
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2004,"Bmass"]<-SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2004,"Mean.wt"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.ComWt$Year, SPA1A.MBS.ComWt$Mean.wt , xout=2020) #  1271.4 Mean numbers 
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2020,"Mean.wt"] <- 1271.4
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2020,"var"] <- 2544103  #assume 2019 variance 
SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2020,"Bmass"] <- SPA1A.MBS.ComWt[SPA1A.MBS.ComWt$Year==2020,"Mean.wt"]*201138.52 #calculate population number from interpolated no /tow
#Check on number used to bump to population level 
SPA1A.MBS.ComWt$Bmass/SPA1A.MBS.ComWt$Mean.wt


#combine Recruit and Commercial dataframes for MBS
SPA1A.MBS.Weight <- rbind(SPA1A.MBS.RecWt, SPA1A.MBS.ComWt)
SPA1A.MBS.Weight$kg <- SPA1A.MBS.Weight$Mean.wt/1000

###
# ---- Make weight dataframe for SPA1A ----
###

SPA1A.Weight <- rbind(SPA1A.2to8.Weight, SPA1A.8to16.Weight, SPA1A.MBS.Weight)

write.csv(SPA1A.Weight, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A.Index.Weight.",surveyyear,".csv"))

####
###
### ----  Calculate Commercial (N) Population size, Commercial (I) and Recruit (R) biomass for the model in tons ----
###
####
#model only needs 1997+

# Population Numbers of Commercial size (N)
SPA1A.2to8.Comm.pop <- SPA1A.2to8.Comm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1A.8to16.Comm.pop <- SPA1A.8to16.Comm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1A.MBS.Com.pop <- SPA1A.MBS.Com %>% filter(Year >= 1997) %>% select(Year, Pop)

#Ensure year ranges are the same: 
SPA1A.2to8.Comm.pop$Year == SPA1A.8to16.Comm.pop$Year
SPA1A.8to16.Comm.pop$Year == SPA1A.MBS.Com.pop$Year

#Create object for N (population size) : 
N <- data.frame(Year = SPA1A.2to8.Comm.pop$Year, N = SPA1A.2to8.Comm.pop$Pop + SPA1A.8to16.Comm.pop$Pop + SPA1A.MBS.Com.pop$Pop)
N$N 
N$N.millions <- N$N /1000000
N

#not calcuated above:
N$N.rec <- (SPA1A.2to8.Rec$Pop[SPA1A.2to8.Rec$Year>=1997] + SPA1A.8to16.Rec$Pop[SPA1A.8to16.Rec$Year>=1997]+ SPA1A.MBS.Rec$Pop[SPA1A.MBS.Rec$Year>=1997])
N$N.rec.millions <-  N$N.rec/1000000



# Commercial Biomass Index (I)
SPA1A.2to8.Comm.bmass <- SPA1A.2to8.CommWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1A.8to16.CommWt.bmass <- SPA1A.8to16.CommWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1A.MBS.ComWt.bmass <- SPA1A.MBS.ComWt %>% filter(Year >= 1997) %>% select(Year, Bmass)

#Ensure year ranges are the same: 
SPA1A.2to8.Comm.bmass$Year == SPA1A.8to16.CommWt.bmass$Year
SPA1A.8to16.CommWt.bmass$Year == SPA1A.MBS.ComWt.bmass$Year 

#Create object for I (Commercial biomass) 
I  <- data.frame(Year = SPA1A.2to8.Comm.bmass$Year, Bmass = (SPA1A.2to8.Comm.bmass$Bmass + SPA1A.8to16.CommWt.bmass$Bmass + SPA1A.MBS.ComWt.bmass$Bmass)/1000000)
I$Bmass

# Recruit Biomass Index (IR)
SPA1A.2to8.RecWt.bmass <- SPA1A.2to8.RecWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1A.8to16.RecWt.bmass <- SPA1A.8to16.RecWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1A.MBS.RecWt.bmass <- SPA1A.MBS.RecWt %>% filter(Year >= 1997) %>% select(Year, Bmass)

#Ensure year ranges are the same: 
SPA1A.2to8.RecWt.bmass$Year == SPA1A.8to16.RecWt.bmass$Year
SPA1A.8to16.RecWt.bmass$Year == SPA1A.MBS.RecWt.bmass$Year 

#Create object for I (Commercial biomass) for SPA 3 Model area:
IR  <- data.frame(Year = SPA1A.2to8.RecWt.bmass$Year, Bmass = (SPA1A.2to8.RecWt.bmass$Bmass + SPA1A.8to16.RecWt.bmass$Bmass + SPA1A.MBS.RecWt.bmass$Bmass)/1000000)
IR$Bmass

I 
IR

###
###  ----  Calculate CV for model I.cv and R.cv ----
###                

#commercial

#stratifed SE estimate
years <- 1997:surveyyear 
X <- length(years)
Nh <- 452591.6

tows.MBS <- livefreq %>% filter(STRATA_ID==39 & TOW_TYPE_ID == 1 & YEAR >=1997) %>% count(YEAR)
tows.MBS$n[tows.MBS$YEAR == 1997] <- 12
tows.MBS$n[tows.MBS$YEAR == 1998] <- 6
tows.MBS <- rbind(tows.MBS, data.frame(YEAR = c("2003","2004"), n = c(33,33))) #e.g. in 2002 n=33 and in 2003 and 2004 data interpolated so therefore n in 2003 and 2004 is assumed the same as n in 2002 - i.e. 33 
tows.MBS <- rbind(tows.MBS, data.frame(YEAR = c("2020"), n = c(42))) #assume same number of 2020 as 2019; since no survey in 2020 and are interpolating these data 
tows.MBS <- tows.MBS[order(tows.MBS$YEAR), ] #reorder dataframe based on year 
tows.MBS$n

#tows.MBS <-   c(12,  6 ,28 ,14, 41, 33,  33,  33, 24 ,19 ,32, 47, 60, 38 ,52 ,51 ,46 ,46, 41,42,42,42, 42) #update each year #Note are filling in data for 1997, 2003 and 2004 (e.g. in 2002 n=33 and in 2003 and 2004 data interpolated so therefore n in 2003 and 2004 is assumed the same as n in 2002 - i.e. 33 ) 

se.SPA1A <- data.frame(Year=(years), var.2to8=SPA1A.2to8.CommWt$var, var.8to16=SPA1A.8to16.CommWt$var, var.MBS=SPA1A.MBS.ComWt$var, MBS.tow=tows.MBS$n)
se.SPA1A$sum.var <- se.SPA1A$var.2to8*(51945^2) + se.SPA1A$var.8to16*(199441^2)+ (se.SPA1A$var.MBS*(201138.52^2)/se.SPA1A$MBS.tow)
se.SPA1A$se <- sqrt(se.SPA1A$sum.var)
se.SPA1A$I.cv <- se.SPA1A$se/(I$Bmass*1000000)
I.cv <- se.SPA1A %>% dplyr::select (Year, cv = I.cv) 
I.cv

########## RECRUIT

years <- 1997:surveyyear
X <- length(years)
Nh <- 452591.6

se.rec.SPA1A <- data.frame(Year=(years), var.2to8=SPA1A.2to8.RecWt$var, var.8to16=SPA1A.8to16.RecWt$var, var.MBS=SPA1A.MBS.RecWt$var, MBS.tow=tows.MBS$n)
se.rec.SPA1A$sum.var <- se.rec.SPA1A$var.2to8*(51945^2) + se.rec.SPA1A$var.8to16*(199441^2)+ (se.rec.SPA1A$var.MBS*(201138.52^2)/se.rec.SPA1A$MBS.tow)
se.rec.SPA1A$se <- sqrt(se.rec.SPA1A$sum.var)
se.rec.SPA1A$IR.cv <- se.rec.SPA1A$se/(IR$Bmass*1000000)
IR.cv <- se.rec.SPA1A %>% dplyr::select (Year, cv = IR.cv) 
IR.cv

#round cvs
I.cv$cv <- round(I.cv$cv, 4)
IR.cv$cv <- round(IR.cv$cv, 4)

#Write out population model inputs N, I, IR, I.cv, IR.cv 
#check all years match up 
cbind(N, I, IR, I.cv, IR.cv)
#Bind into object for export 
SPA1A.population.model.input <- cbind(N %>% dplyr::select(Year, N), I %>% dplyr::select(I = Bmass), IR %>% dplyr::select(IR = Bmass), I.cv %>% dplyr::select(I.cv = cv), IR.cv %>% dplyr::select(IR.cv = cv))

#export 
write.csv(SPA1A.population.model.input, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A.population.model.input.",surveyyear,".csv"))


####
### ---- Plot Numbers and weight per tow ----
###                   
# DO NOT PLOT 2020 data point since no survey data that year!!! 

#Number per tow 
data <- SPA1A.Numbers[SPA1A.Numbers$Year>=1980,]
data$Size <- data$Age
data$Mean.nums[data$Year==2020] <- NA 


num.per.tow.full.ts <- ggplot(data = data, aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + facet_wrap(~Area,  ncol=1) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
num.per.tow.full.ts

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_NumberPerTow_byStrata.png"), type="cairo", width=35, height=25, units = "cm", res=300)
num.per.tow.full.ts
dev.off()


#weight per tow 
data.kg <- SPA1A.Weight[SPA1A.Weight$Year>=1980,]
data.kg$Size <- data.kg$Age
data.kg$kg[data.kg$Year==2020] <- NA 


wt.per.tow.full.ts <- ggplot(data = data.kg, aes(x=Year, y= kg, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + facet_wrap(~Area, ncol=1) + 
  theme_bw() + ylab("Mean kg/tow") + xlab("Year") + 
  theme(legend.position = c(0.15, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
wt.per.tow.full.ts

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_WeightPerTow_byStrata.png"), type="cairo", width=35, height=25, units = "cm", res=300)
wt.per.tow.full.ts
dev.off()


###
### ---- Plot Survey Numbers and Biomass for all 1A ----
###                    


N.for.plot <- pivot_longer(N %>% select(Year, Commercial = N.millions, Recruit = N.rec.millions), 
                                      cols = c(Commercial, Recruit),
                                      names_to = "Size",
                                      values_to = "value",
                                      values_drop_na = FALSE)

#set values that are 2020 to NA so don't plot since didn't have survey that year 
N.for.plot$value[N.for.plot$Year == 2020] <- NA

survey.numbers <- ggplot(data = N.for.plot, aes(x=Year, y=value, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey numbers (millions)") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
survey.numbers

Bmass.for.plot <- merge(I %>% select(Year, Commercial = Bmass), IR %>% select(Year, Recruit = Bmass), by = "Year")

B.for.plot <- pivot_longer(Bmass.for.plot, 
                           cols = c(Commercial, Recruit),
                           names_to = "Size",
                           values_to = "value",
                           values_drop_na = FALSE)

#set values that are 2020 to NA so don't plot since didn't have survey that year 
B.for.plot$value[B.for.plot$Year == 2020] <- NA

survey.biomass <- ggplot(data = B.for.plot, aes(x=Year, y=value, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey biomass (mt)") + xlab("Year") + 
  theme(legend.position = c(0.15, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
survey.biomass

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_SurveyNumbersAndBiomass",surveyyear,".png"), type="cairo", width=30, height=25, units = "cm", res=300)
plot_grid(survey.numbers, survey.biomass, 
          nrow = 2, label_x = 0.15, label_y = 0.95)
dev.off() 


###
### ---- Strata 56 -----
### Look at tows outside normal survey starta
### Note need to be really careful with the interpretaion of this since it's a large area and tows can be in different spots - need to look at tow locations before can interpret. Also tow in this strata are generally exploratory - so not representative of whole area  

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#numbers by shell height bin;
sql.56 <- "SELECT * FROM scallsur.scliveres WHERE strata_id = 56"

# Select data from database; execute query with ROracle; numbers by shell height bin
spa1a.56 <- dbGetQuery(chan, quer1)

#add YEAR column to data
spa1a.56$YEAR <- as.numeric(substr(spa1a.56$CRUISE,3,6))

#n tows
TowsbyYear<-aggregate (TOW_NO ~ STRATA_ID + YEAR, data=spa1a.56, length)

#no/tow
years <- 1984:surveyyear
X <- length(years)
stderr <- function(x) sqrt(var(x)/length(x))

#simple means
spa1a.56.number<- data.frame(Year=years,  Mean.Com=rep(NA,X), sd.Com=rep(NA,X), Mean.Rec=rep(NA,X), sd.Rec=rep(NA,X))
for(i in 1:length(spa1a.56.number$Year)){
  temp.data <- spa1a.56[spa1a.56$YEAR==1983+i,]
  spa1a.56.number[i,2] <- mean(apply(temp.data[, 27:50],1,sum), na.rm=TRUE)
  spa1a.56.number[i,3] <- sd(apply(temp.data[, 27:50],1,sum))
  spa1a.56.number[i,4] <- mean(apply(temp.data[, 24:26],1,sum), na.rm=TRUE)
  spa1a.56.number[i,5] <- sd(apply(temp.data[, 24:26],1,sum))
}
spa1a.56.number

# prep data for plot 
strata56.for.plot <- pivot_longer(spa1a.56.number %>% select(Year, Commercial = Mean.Com, Recruit = Mean.Rec ) , 
                           cols = c(Commercial, Recruit),
                           names_to = "Size",
                           values_to = "value",
                           values_drop_na = FALSE)

num.per.tow.56 <- ggplot(data = strata56.for.plot, aes(x=Year, y=value, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
num.per.tow.56

#write out plot 
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_SurveyNumbers_strata56_",surveyyear,".png"), type="cairo", width=30, height=25, units = "cm", res=300)
num.per.tow.56
dev.off() 


### END OF SCRIPT ### 
