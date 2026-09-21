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
### ---- PRE-RECRUIT NUMBERS PER TOW AND POPULATION NUMBERS ---- 
###
####

###
# --- 2 to 8 mile ---- 
###
years <- 1984:surveyyear
X <- length(years)

#Pre-Recruit no/tow and population
SPA1A.2to8.PreRec <- data.frame(Year=years, Mean.nums=rep(NA,X), Pop=rep(NA,X) ,method=rep("PED",X), Area=rep("2to8", X), Age=rep("Pre-Recruit", X))
for(i in 1:length(SPA1A.2to8.PreRec$Year)){
  if (years[i] != 2020) { 
temp.data<-livefreq[livefreq$YEAR==1983+i,]
SPA1A.2to8.PreRec[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.PreRec[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
}}
SPA1A.2to8.PreRec

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.PreRec$Year, SPA1A.2to8.PreRec$Mean.nums, xout=2020) #  4.5221 Mean numbers 
SPA1A.2to8.PreRec[SPA1A.2to8.PreRec$Year==2020,"Mean.nums"] <- 4.5221
SPA1A.2to8.PreRec[SPA1A.2to8.PreRec$Year==2020,"Pop"] <- SPA1A.2to8.PreRec[SPA1A.2to8.PreRec$Year==2020,"Mean.nums"]*51945 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA1A.2to8.new$NH); check by seeing if matchs: 
SPA1A.2to8.PreRec$Pop/SPA1A.2to8.PreRec$Mean.nums
#Yes is 51945

###
# ---- 8 to 16 mile ----
###
years <- 1981:surveyyear
X <- length(years)

#Pre-Recruit no/tow and population
SPA1A.8to16.PreRec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Pre-Recruit", X))
for(i in 1:length(SPA1A.8to16.PreRec$Year)){
  if (years[i] != 2020) {
temp.data<-livefreq[livefreq$YEAR==1980+i,]
SPA1A.8to16.PreRec[i,2]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.PreRec[i,3]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA1A.8to16.PreRec

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.PreRec$Year, SPA1A.8to16.PreRec$Mean.nums, xout=2020) #  1.0001 Mean numbers 
SPA1A.8to16.PreRec[SPA1A.8to16.PreRec$Year==2020,"Mean.nums"] <- 1.0001
SPA1A.8to16.PreRec[SPA1A.8to16.PreRec$Year==2020,"Pop"] <- SPA1A.8to16.PreRec[SPA1A.8to16.PreRec$Year==2020,"Mean.nums"]*199441 #calculate population number from interpolated no/tow; MUST ; Bumping by sum(strata.SPA1A.8to16.noctrville.new$NH); check by seeing if matchs: 
SPA1A.8to16.PreRec$Pop/SPA1A.8to16.PreRec$Mean.nums

###
# --- MidBay South ----
###
years <- 1997:surveyyear
X <- length(years)

#Pre-Recruit no/tow and population
SPA1A.MBS.PreRec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("MBS", X), Age=rep("Pre-Recruit", X))
for(i in 1:length(SPA1A.MBS.PreRec$Year)){
  if (years[i] != 2020) {
temp.data<-livefreq[livefreq$YEAR==1996+i,]
SPA1A.MBS.PreRec[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 13:25],1,sum))
SPA1A.MBS.PreRec[i,3] <- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 13:25],1,sum))*201138.52
} }
SPA1A.MBS.PreRec

#interpolate missing years
approx(SPA1A.MBS.PreRec$Year, SPA1A.MBS.PreRec$Mean.nums, xout=2003) #5.1885
approx(SPA1A.MBS.PreRec$Year, SPA1A.MBS.PreRec$Mean.nums, xout=2004) #6.9255

SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2003,"Mean.nums"] <- 5.1885
SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2004,"Mean.nums"] <- 6.9255
SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2003,"Pop"] <- SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2003,"Mean.nums"]*201138.52 #calculate population number from interpolated no/tow
SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2004,"Pop"] <- SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2004,"Mean.nums"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.PreRec$Year, SPA1A.MBS.PreRec$Mean.nums, xout=2020) #  5.4595 Mean numbers 
SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2020,"Mean.nums"] <- 5.4595
SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2020,"Pop"] <- SPA1A.MBS.PreRec[SPA1A.MBS.PreRec$Year==2020,"Mean.nums"]*201138.52 #calculate population number from interpolated no/to
#Check on number used to bump to population level 
SPA1A.MBS.PreRec$Pop/SPA1A.MBS.PreRec$Mean.nums


###
#  --- Make Numbers dataframe for SPA1A indices by strata area ----
###
SPA1A.PreRec.Numbers <- rbind(SPA1A.2to8.PreRec, SPA1A.8to16.PreRec, SPA1A.MBS.PreRec)

write.csv(SPA1A.PreRec.Numbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A.Index.Pre-Recruit.Numbers.",surveyyear,".csv"))


###
###
### ---- PRE-RECRUIT WEIGHT PER TOW  ----
###
###

###
# ---- 2 to 8 mile ----
###
years <- 1997:surveyyear
X <- length(years)

#Pre-Recruit no/tow and population
SPA1A.2to8.PreRecWt <- data.frame(Year=years, Mean.wt=rep(NA,X), Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("2to8", X), Age=rep("Pre-Recruit", X))
for(i in 1:length(SPA1A.2to8.PreRecWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.2to8.PreRecWt[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.PreRecWt[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA1A.2to8.PreRecWt[i,4]<-summary (PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst^2
} }
SPA1A.2to8.PreRecWt

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.PreRecWt$Year, SPA1A.2to8.PreRecWt$Mean.wt , xout=2020) #  22.664 Mean numbers 
SPA1A.2to8.PreRecWt[SPA1A.2to8.PreRecWt$Year==2020,"Mean.wt"] <- 22.664
SPA1A.2to8.PreRecWt[SPA1A.2to8.PreRecWt$Year==2020,"var"] <- 112.7074 #assume 2019 variance 
SPA1A.2to8.PreRecWt[SPA1A.2to8.PreRecWt$Year==2020,"Bmass"] <- SPA1A.2to8.PreRecWt[SPA1A.2to8.PreRecWt$Year==2020,"Mean.wt"]*51945 #calculate population number from interpolated no/to
#Check on number used to bump to population level ;  sum(strata.SPA1A.2to8.new$NH)
SPA1A.2to8.PreRecWt$Bmass/SPA1A.2to8.PreRecWt$Mean.wt

#Calculate weight in kg
SPA1A.2to8.PreRecWt$kg <- SPA1A.2to8.PreRecWt$Mean.wt/1000

####
# ---- 8 to 16 mile ----
####
years <- 1997:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.8to16.PreRecWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Pre-Recruit", X))
for(i in 1:length(SPA1A.8to16.PreRecWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.8to16.PreRecWt[i,2]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.PreRecWt[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA1A.8to16.PreRecWt[i,4]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,13:25],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst^2
}}
SPA1A.8to16.PreRecWt

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.PreRecWt$Year, SPA1A.8to16.PreRecWt$Mean.wt , xout=2020) #  4.5351 Mean numbers 
SPA1A.8to16.PreRecWt[SPA1A.8to16.PreRecWt$Year==2020,"Mean.wt"] <- 4.5351
SPA1A.8to16.PreRecWt[SPA1A.8to16.PreRecWt$Year==2020,"var"] <- 9.8371e+00 #assume 2019 variance 
SPA1A.8to16.PreRecWt[SPA1A.8to16.PreRecWt$Year==2020,"Bmass"] <- SPA1A.8to16.PreRecWt[SPA1A.8to16.PreRecWt$Year==2020,"Mean.wt"]*199441 #calculate population number from interpolated no /tow
#Check on number used to bump to population level ;  sum(strata.SPA1A.8to16.noctrville.new$NH)
SPA1A.8to16.PreRecWt$Bmass/SPA1A.8to16.PreRecWt$Mean.wt

#Calculate weight in kg
SPA1A.8to16.PreRecWt$kg <- SPA1A.8to16.PreRecWt$Mean.wt/1000

###
# ---- MidBay South ----
###
years <- 1997:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.MBS.PreRecWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X), var=rep(NA,X), method=rep("PED",X), Area=rep("MBS", X), Age=rep("Pre-Recruit", X))
for(i in 1:length(SPA1A.MBS.PreRecWt$Year)){
  if (years[i] != 2020) {
temp.data<-BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1A.MBS.PreRecWt[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 13:25],1,sum))
SPA1A.MBS.PreRecWt[i,3]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 13:25],1,sum))*201138.52
SPA1A.MBS.PreRecWt[i,4]<- var(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 13:25],1,sum))
} }
SPA1A.MBS.PreRecWt

#assume 1997 same at 2to8
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==1997,c(2,4)] <- c(12.0296,26.0181)
#interpolate missing years
approx(SPA1A.MBS.PreRecWt$Year, SPA1A.MBS.PreRecWt$Mean.wt, xout=2003) #19.922
approx(SPA1A.MBS.PreRecWt$Year, SPA1A.MBS.PreRecWt$Mean.wt, xout=2004) #26.303

SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2003,c(2,4)]<-c(19.922,11.3469) #assume variance from 2002
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2004,c(2,4)]<-c(26.303,11.3469) #assume variance from 2002
#calculate population number from interpolated no/tow
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==1997,"Bmass"]<-SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==1997,"Mean.wt"]*201138.52
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2003,"Bmass"]<-SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2003,"Mean.wt"]*201138.52
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2004,"Bmass"]<-SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2004,"Mean.wt"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.PreRecWt$Year, SPA1A.MBS.PreRecWt$Mean.wt , xout=2020) #  32.439 Mean numbers 
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2020,"Mean.wt"] <- 32.439
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2020,"var"] <- 1026.327 #assume 2019 variance 
SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2020,"Bmass"] <- SPA1A.MBS.PreRecWt[SPA1A.MBS.PreRecWt$Year==2020,"Mean.wt"]*201138.52 #calculate population number from interpolated no /tow
#Check on number used to bump to population level 
SPA1A.MBS.PreRecWt$Bmass/SPA1A.MBS.PreRecWt$Mean.wt


#Calculate weight in kg
SPA1A.MBS.PreRecWt$kg <- SPA1A.MBS.PreRecWt$Mean.wt/1000

###
# ---- Make weight dataframe for SPA1A ----
###

SPA1A.Weight <- rbind(SPA1A.2to8.PreRecWt, SPA1A.8to16.PreRecWt, SPA1A.MBS.PreRecWt)

write.csv(SPA1A.Weight, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A.Index.Pre-Recruit.Weight.",surveyyear,".csv"))

####
### ---- Plot Numbers and weight per tow ----
###                   
# DO NOT PLOT 2020 data point since no survey data that year!!! 

#Number per tow 
data <- SPA1A.PreRec.Numbers [SPA1A.PreRec.Numbers $Year>=1980,]
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

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_NumberPerTow_byStrata.png"), type="cairo", width=35, height=25, units = "cm", res=300)
#num.per.tow.full.ts
#dev.off()


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

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_WeightPerTow_byStrata.png"), type="cairo", width=35, height=25, units = "cm", res=300)
#wt.per.tow.full.ts
#dev.off()


##########################################################################
############### EDITS END HER E############


###
### ---- Plot Survey Numbers and Biomass for all 1A ----
###                    


N.for.plot <- pivot_longer(N %>% select(Year, Pre-Recruit = N.pre.rec.millions), 
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
