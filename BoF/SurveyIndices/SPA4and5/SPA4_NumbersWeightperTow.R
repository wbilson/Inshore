###........................................###
###
###    SPA 4
###    Numbers per tow, Weight per tow
###    Population numbers and biomass
###
###
###    Rehauled July 2020 J.Sameoto
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(ROracle)
library(PEDstrata) #v.1.0.3
library(Hmisc)
library(tidyverse)
library(ggplot2)
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

  
# read in shell height and meat weight data from database
#strata.spa4<-c(1:5, 8:10)  #also strata_id 47 (inside 0-2 miles), but that is not included in the general SPA 4 analysis (see end of script)

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# numbers by shell height bin;
query1 <- ("SELECT * FROM scallsur.scliveres WHERE strata_id IN (1,2,3,4,5,8,9,10) AND (cruise LIKE 'BA%' OR cruise LIKE 'BF%' OR cruise LIKE 'BI%' OR cruise LIKE 'GM%' OR cruise LIKE 'RF%')")

# Select data from database; execute query with ROracle; numbers by shell height bin
livefreq <- dbGetQuery(chan, query1)

# add YEAR column to data
livefreq$YEAR <- as.numeric(substr(livefreq$CRUISE,3,6))

# check how many regular vs exploratory/experimental tows (tow type 3)
table(livefreq$TOW_TYPE_ID[livefreq$YEAR==surveyyear] )
livefreq[livefreq$YEAR==surveyyear&livefreq$TOW_TYPE_ID==3,]
table(livefreq$YEAR)

# read in meat weight data; this is output from the meat weight/shell height modelling
#1983-1995 modelled using data from 1996-2001; 1996+ modelled using data in the current year only

# code for reading in multiple csvs at once and combining into one dataframe from D.Keith (2015)
Year <- c(seq(1983,2019),seq(2021,surveyyear))  
num.years <- length(Year)

BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",Year[i],".csv",sep="") ,header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

#check data structure
summary(BFliveweight)
str(BFliveweight)
table(BFliveweight$CRUISE)
table(BFliveweight$YEAR)


# ---- RECRUIT AND COMMERCIAL NUMBERS PER TOW AND POPULATION NUMBERS ----

#.. Recruit Numbers per Tow 
years <- 1981:surveyyear
X <- length(years)

SPA4.Rec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X) ,method=rep("PED",X), Area=rep("SPA4", X), Age=rep("Recruit", X))
for(i in 1:length(SPA4.Rec$Year)){
  if (years[i] != 2020) { 
  temp.data <- livefreq[livefreq$YEAR==1980+i,]
  SPA4.Rec[i,2] <- summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,24:26],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst
  SPA4.Rec[i,3] <- summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,24:26],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA4.Rec

# in 2000, recruits were definded as 40-79 mm due to fast growth
BF2000 <- subset(livefreq, YEAR==2000)
rec.mod <- PEDstrata(BF2000,strata.SPA4.new,"STRATA_ID",catch=apply(BF2000[,19:26],1,sum), Subset=BF2000$TOW_TYPE_ID==1)  #,effic=T
summary(rec.mod)$yst   # 260.45
summary(rec.mod)$Yst # 40923470
# Set for 2000
SPA4.Rec[SPA4.Rec$Year==2000,"Mean.nums"] <- 260.45
SPA4.Rec[SPA4.Rec$Year==2000,"Pop"] <- 40923470

#in 2020 had no survey to linear interpolation 
approx(SPA4.Rec$Year, SPA4.Rec$Mean.nums, xout=2020) #  2.4138 Mean numbers 
SPA4.Rec[SPA4.Rec$Year==2020,"Mean.nums"] <- 2.4138
SPA4.Rec[SPA4.Rec$Year==2020,"Pop"] <- SPA4.Rec[SPA4.Rec$Year==2020,"Mean.nums"]*157128 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA4.new$NH); check by seeing if matchs: 
SPA4.Rec$Pop/SPA4.Rec$Mean.nums
#Yes is 157128
SPA4.Rec



#.. Commercial Number per Tow 
years <- 1981:surveyyear 
X <- length(years)

SPA4.Comm <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X) ,method=rep("PED",X), Area=rep("SPA4", X), Age=rep("Commercial", X))
for(i in 1:length(SPA4.Comm$Year)){
  if (years[i] != 2020) { 
  temp.data <- livefreq[livefreq$YEAR==1980+i,]
  SPA4.Comm[i,2]<-summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,27:50],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst
  SPA4.Comm[i,3]<-summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,27:50],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$Yst
}}
SPA4.Comm

#in 2020 had no survey to linear interpolation 
approx(SPA4.Comm$Year, SPA4.Comm$Mean.nums, xout=2020) #  XXXXX Mean numbers 
SPA4.Comm[SPA4.Comm$Year==2020,"Mean.nums"] <- 136.17
SPA4.Comm[SPA4.Comm$Year==2020,"Pop"] <- SPA4.Comm[SPA4.Comm$Year==2020,"Mean.nums"]*157128 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA4.new$NH); check by seeing if matchs: 
SPA4.Comm$Pop/SPA4.Comm$Mean.nums
#Yes is 157128
SPA4.Comm

#  Make Numbers as a dataframe 
SPA4.Numbers <- rbind(SPA4.Rec, SPA4.Comm)
# Note only years > 1982 are used used in population dynamic modelling and it is the "Pop" field associated with Commercial size scallops that is used in the Model Data Input file 

# Write Out Data
write.csv(SPA4.Numbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA4.Index.Numbers.",surveyyear,".csv"))



# ---- RECRUIT AND COMMERCIAL WEIGHT PER TOW AND POPULATION BIOMASS ----
#Use liveweight for years 1997+ (beginning of detailed sampling)

#.. Recruit Biomass Per Tow 
years <- 1983:surveyyear  
X <- length(years)

#Recruit no/tow and population
SPA4.RecWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X), se.yst=rep(NA,X), method=rep("PED",X), Area=rep("SPA4", X), Age=rep("Recruit", X))
for(i in 1:length(SPA4.RecWt$Year)){
  if (years[i] != 2020) { 
temp.data <- BFliveweight[BFliveweight$YEAR==1982+i,]
SPA4.RecWt[i,2] <- summary(PEDstrata(temp.data, strata.SPA4.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA4.RecWt[i,3] <- summary(PEDstrata(temp.data, strata.SPA4.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA4.RecWt[i,4] <- summary(PEDstrata(temp.data, strata.SPA4.new, "STRATA_ID",catch=apply(temp.data[,26:28],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst
}}
SPA4.RecWt

#in 2000, recruits were definded as 40-79 mm due to fast growth
BFwt2000 <- subset(BFliveweight, YEAR==2000)
rec.mod.wt <- PEDstrata(BF2000,strata.SPA4.new,"STRATA_ID",catch=apply(BFwt2000[,21:28],1,sum), Subset=BFwt2000$TOW_TYPE_ID==1)
summary(rec.mod.wt)$yst   # 301.44
summary(rec.mod.wt)$Yst  #47364461
#Set 
SPA4.RecWt[SPA4.RecWt$Year==2000,"Mean.wt"] <- 301.44
SPA4.RecWt[SPA4.RecWt$Year==2000,"Bmass"] <- 47364461

#in 2020 had no survey to linear interpolation 
approx(SPA4.RecWt$Year, SPA4.RecWt$Mean.wt, xout=2020) #  XXXXX Mean numbers 
SPA4.RecWt[SPA4.RecWt$Year==2020,"Mean.wt"] <-  12.862
SPA4.RecWt[SPA4.RecWt$Year==2020,"Bmass"] <- SPA4.RecWt[SPA4.RecWt$Year==2020,"Mean.wt"]*157128 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA4.new$NH); check by seeing if matchs: 
SPA4.RecWt$Bmass/SPA4.RecWt$Mean.wt
#Yes is 157128
SPA4.RecWt
# Assume same standard error (se.yst) as in 2019 
SPA4.RecWt[SPA4.RecWt$Year==2020,"se.yst"] <-  3.8193

#Commercial Biomass per Tow and Population Biomass 
SPA4.CommWt <- data.frame(Year=years, Mean.wt=rep(NA,X),Bmass=rep(NA,X), se.yst=rep(NA,X), method=rep("PED",X), Area=rep("SPA4", X), Age=rep("Commercial", X))
for(i in 1:length(SPA4.CommWt$Year)){
  if (years[i] != 2020) { 
temp.data <- BFliveweight[BFliveweight$YEAR==1982+i,]
SPA4.CommWt[i,2] <- summary(PEDstrata(temp.data, strata.SPA4.new, "STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA4.CommWt[i,3] <- summary(PEDstrata(temp.data, strata.SPA4.new, "STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$Yst
SPA4.CommWt[i,4] <- summary(PEDstrata(temp.data, strata.SPA4.new, "STRATA_ID",catch=apply(temp.data[,29:52],1,sum),Subset=temp.data$TOW_TYPE_ID==1))$se.yst
}}
SPA4.CommWt

#in 2020 had no survey to linear interpolation 
approx(SPA4.CommWt$Year, SPA4.CommWt$Mean.wt, xout=2020) #  XXXXX Mean numbers 
SPA4.CommWt[SPA4.CommWt$Year==2020,"Mean.wt"] <- 2507.9
SPA4.CommWt[SPA4.CommWt$Year==2020,"Bmass"] <- SPA4.CommWt[SPA4.CommWt$Year==2020,"Mean.wt"]*157128 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA4.new$NH); check by seeing if matchs: 
SPA4.CommWt$Bmass/SPA4.CommWt$Mean.wt
#Yes is 157128
SPA4.CommWt
# Assume same standard error (se.yst) as in 2019 
SPA4.CommWt[SPA4.CommWt$Year==2020,"se.yst"] <-  136.581


#  Make biomass as a dataframe 
SPA4.Weight <- rbind(SPA4.RecWt, SPA4.CommWt)
SPA4.Weight$kg <- SPA4.Weight$Mean.wt/1000

# Calculate I & R for the model in tons (commerical and Recruit biomass in metric tonnes)
SPA4.Weight$Biomass_mt <- SPA4.Weight$Bmass/1000000

# Calculate Commercial cv (I.cv) and Recruit (R.cv) biomass for the model in tons
SPA4.Weight$cv <- SPA4.Weight$se.yst/SPA4.Weight$Mean.wt

# Write Out Data
write.csv(SPA4.Weight, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA4.Index.Weight.",surveyyear,".csv"))


# ---- Plot numbers and weight per tow for SPA 4 ---- 
# DO NOT PLOT 2020 data point since no survey data that year!!! 
SPA4.Numbers$Size <- SPA4.Numbers$Age
SPA4.Numbers$Size[SPA4.Numbers$Age=="Comm"] <- "Commercial"
SPA4.Numbers$Mean.nums[SPA4.Numbers$Year==2020] <- NA 

SPA4.Weight$Size <- SPA4.Weight$Age
SPA4.Weight$Size[SPA4.Weight$Age=="Comm"] <- "Commercial"
SPA4.Weight$kg[SPA4.Weight$Year==2020] <- NA 


num.per.tow.full.ts <- ggplot(data = SPA4.Numbers, aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
num.per.tow.full.ts

wt.per.tow.full.ts <- ggplot(data = SPA4.Weight, aes(x=Year, y= kg, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Mean kg/tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
wt.per.tow.full.ts

num.per.tow.recent.ts <- ggplot(data = SPA4.Numbers[SPA4.Numbers$Year>=2004,], aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
num.per.tow.recent.ts

wt.per.tow.recent.ts <- ggplot(data = SPA4.Weight[SPA4.Weight$Year>=2004,], aes(x=Year, y= kg, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Mean kg/tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
wt.per.tow.recent.ts


png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA4_NumberWeightPerTow.png"), type="cairo", width=35, height=25, units = "cm", res=300)
plot_grid(num.per.tow.full.ts, num.per.tow.recent.ts, 
          wt.per.tow.full.ts, wt.per.tow.recent.ts, 
          ncol = 2, nrow = 2, label_x = 0.15, label_y = 0.95)
dev.off()



# ---- SPA 4 strata_id 47 (inside 0-2 miles) ----

#Define SQL query
query2 <- ("SELECT * FROM scallsur.scliveres WHERE strata_id = 47 AND (cruise LIKE 'BA%' OR cruise LIKE 'BF%' OR cruise LIKE 'BI%' OR cruise LIKE 'GM%' OR cruise LIKE 'RF%')")

# Select data from database; execute query with ROracle; data from tows in the 0-2 mile strata 
spa4inside <- dbGetQuery(chan, query2)

#add YEAR column to data
spa4inside$YEAR <- as.numeric(substr(spa4inside$CRUISE,3,6))

# n tows by year 
TowsbyYear <- aggregate (TOW_NO ~ STRATA_ID + YEAR, data=spa4inside, length)
TowsbyYear

#no/tow
years <- 1981:surveyyear 
X <- length(years)
#stderr <- function(x) sqrt(var(x)/length(x))

#simple means
Inside4.number <- data.frame(Year=years,  Mean.Com=rep(NA,X), sd.Com=rep(NA,X), Mean.Rec=rep(NA,X))
for(i in 1:length(Inside4.number$Year)){
	temp.data <- spa4inside[spa4inside$YEAR==1980+i,]
	Inside4.number[i,2] <- mean(apply(temp.data[, 27:50],1,sum), na.rm=TRUE)
	Inside4.number[i,3] <- sd(apply(temp.data[, 27:50],1,sum))
  Inside4.number[i,4] <- mean(apply(temp.data[, 24:26],1,sum), na.rm=TRUE)
	  }
Inside4.number

#in 2020 had no survey but since this area not used for model don't need to interpolate a value for 2020 

#Write out data
write.csv(Inside4.number, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA4.Inside0to2.Index.Numbers.",surveyyear,".csv"))


# Plot time series for numbers in 0 to 2 mile strata of SPA 4 - NOTE these are EXPLORATORY TOWS in recent time period, thus don't read into trends too much 

inside.2nm.for.plot <- pivot_longer(Inside4.number[,c("Year", "Mean.Com", "Mean.Rec")], 
             cols = c("Mean.Com","Mean.Rec"),
             names_to = "Size",
             values_to = "Value",
             values_drop_na = FALSE)
inside.2nm.for.plot


png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA4_NumberPerTow_0to2mile.png"), type="cairo", width=20, height=12, units = "cm", res=300)

ggplot(data = inside.2nm.for.plot, aes(x=Year, y= Value, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.8)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))

dev.off()
