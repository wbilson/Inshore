# Updated Growth Rate calculations for BoF Assessment Areas 
# J.Sameoto June 2020
# Includes Actual Growth rates and Predicted Growth Rates
# edited Sept 2021 JS 

# ---- SPA 6 GROWTH RATES ----

# required packages
library(lme4)
library(dplyr)
library(ggplot2)
library(tidyverse)

options(stringsAsFactors = FALSE)

# ///.... DEFINE THESE ENTRIES ....////

#DEFINE: year, area
year <- 2026  #this is the survey year
area <- "6"  # choice entry here is "1A1B4and5", "3", "6";  recall SPAs 1A, 1B and 4 and 5 all modelled together
assessmentyear <- 2026 #this is the year you are running your assessment in -- corresponds to the assessment folder year name e.g. INSHORE SCALLOP/2020/Assessment..


# DEFINE: load required workspace with model objects - current year 
load(paste0("Y:/Inshore/Assessment/BoF/",year,"/Assessment/Data/Growth/SPA6/GMgrowth",year,".RData"))
#load(paste0("Y:/Inshore/BoF/",year,"/Assessment/Data/Growth/SPA6/GMgrowth",year,".RData"))

# DEFINE: load shell height objects - current year 
source(paste0("Y:/Inshore/Assessment/BoF/",year,"/Assessment/Data/Growth/SPA6/SPA6.SHobj.IN.",year,".R"))
#source(paste0("Y:/Inshore/BoF/",year,"/Assessment/Data/Growth/SPA6/SPA6.SHobj.IN.",year,".R"))
#Identify Shell Height objects and confirm column names: 
#column names:    years SPA6.SHactual.Com.IN SPA6.SHactual.Rec.IN
SPA6.SHactual.IN <- sh.actual.in %>% select(years, SHactual.Com = SPA6.SHactual.Com.IN, SHactual.Rec = SPA6.SHactual.Rec.IN)
SPA6.SHactual.IN

# columns names:  years SPA6.SHpredict.Com SPA6.SHpredict.Rec
SPA6.SHpredict.IN <- sh.predict.in %>% select(years, SHpredict.Com = SPA6.SHpredict.Com.IN, SHpredict.Rec = SPA6.SHpredict.Rec.IN)
SPA6.SHpredict.IN

# set SH object: 
SH.object <- cbind(SPA6.SHactual.IN, SPA6.SHpredict.IN)
SH.object <- SH.object[,-4]  #2nd and duplicated row of years 
SH.object


# DEFINE: Source previous year meat weight and growth rate object for ACTUAL growth rates:
# if your year defined above it for YYYY, then you should be bringing in the YYYY-1 growth rate object.
spa6.growthrate <- read.csv(paste0("Y:/Inshore/Assessment/BoF/",year-1,"/Assessment/Data/Growth/SPA6/spa6.growthrate.",year-1,".csv"))
#spa6.growthrate <- read.csv(paste0("Y:/Inshore/BoF/",year-1,"/Assessment/Data/Growth/SPA6/spa6.growthrate.",year-1,".csv"))
spa6.growthrate <- spa6.growthrate[,-1]
spa6.growthrate
str(spa6.growthrate)

#break out data to object: 
spa6.growthrate.com <- spa6.growthrate[spa6.growthrate$Age == "Commercial" & spa6.growthrate$GrowthMethod == "Actual",]
spa6.growthrate.rec <- spa6.growthrate[spa6.growthrate$Age == "Recruit" & spa6.growthrate$GrowthMethod == "Actual",]
spa6.predictedgr.com <- spa6.growthrate[spa6.growthrate$Age == "Commercial" & spa6.growthrate$GrowthMethod == "Predict",]
spa6.predictedgr.rec <- spa6.growthrate[spa6.growthrate$Age == "Recruit" & spa6.growthrate$GrowthMethod == "Predict",]

#From 2021
# DEFINE: Source previous year meat weight and growth rate object for ACTUAL growth rates:
# if your year defined above it 2019, then you should be bringing in the 2018 growth rate object.
#Brings in objects spa1a.growthrate.com, spa1a.growthrate.rec
#source("Y:/Inshore/BoF/2021/Assessment/Scripts/Growth/SPA6_GrowthRatesRecalculated/GrowthRateObjects/SPA6.ActualGrowthRateObj.2021.R")
#check that you're objects are there: 
spa6.growthrate.com
spa6.growthrate.rec

# DEFINE: Source previous year meat weight and growth rate object for PREDICTED growth rates:
# if your year defined above it 2019, then you should be bringing in the 2018 growth rate object.
#Brings in objects spa1a.predictedgr.com, spa1a.predictedgr.rec
#source("Y:/Inshore/BoF/2020/Assessment/Scripts/Growth/SPA6_GrowthRatesRecalculated/GrowthRateObjects/SPA6.PredictedGrowthRateObj.2019.R")
#check that you're objects are there: 
spa6.predictedgr.com
spa6.predictedgr.rec


# DEFINE: Identify model objects: 
model.object <- MWTSHGM.YYYY

#DEFINE: Identify data object; NOTE if model above is MWTSHGM.2019 then this was run on 2019 data so you want the data as GMdetail2019:
data <- GMdetail.foryear #GMdetailYYYY

# DEFINE path for figures and dataouput to be saved; note expects within this folder that you've created a "dataoutput" and "Figures" folder under the following directory path;  MUST HAVE "/" at the end of your path! (shouldn't have to change this in most years with new folder structure)
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

#////... END OF DEFINE SECTION ...////


# set years: 
years <- SPA6.SHactual.IN$years


#prepare data object - log transform depth and height 
summary(data)
data <- data[complete.cases(data$HEIGHT),] #remove rows that have no height 
data$Log.HEIGHT <- log(data$HEIGHT)
data$Log.DEPTH <- log(abs(data$ADJ_DEPTH)) #take abs to keep value positive
summary(data)
table(data$CRUISE)


#Depth for prediction,: 
#For SPA 6 Inside VMS area: -54.74; see Y:\INSHORE SCALLOP\BoF\StandardDepth\BoFMeanDepths.csv
depth <- -54.74

# ---- Actual Growth Rates ----

# Calcuate mean weight of commercial and recruit animals:
#1. In year t ("actual") using mean SH in year t and meat weight shell height relationship in year t
#2. In year t+1 ("pred") using predicted mean SH in year t+1 (predicted from year t) and meat weight shell height relationship in year t+1

#objects to hold predicted meat weights and growth rate: 
growthrate.com <- data.frame(YEAR = c(year-1, year), MW.actual.com = NA,  MW.pred.com = NA)
growthrate.rec <- data.frame(YEAR = c(year-1, year), MW.actual.rec = NA, MW.pred.rec = NA)

#An assessment run from here: 
test.data <- subset(data, YEAR == year & HEIGHT > 40) #data subsetted as it was modelled

# predicted commercial mean meat weight in year t-1 using predicted SH (SH predicted from year t-1 to year t) and actual mw/sh relationship in year t
growthrate.com$MW.pred.com[growthrate.com$YEAR == year-1] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$years == year-1])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                             re.form=~0,type="response")  #  13.35408   NOTE: Can just run predict() and not assign - pull out number and manually append to list below

# predicted recruit mean meat weight in year t-1 using predicted SH (SH predicted from year t-1 to year t) and actual mw/sh relationship in year t
growthrate.rec$MW.pred.rec[growthrate.rec$YEAR == year-1] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$years == year-1])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                             re.form=~0,type="response") # 7.181799 NOTE: Can just run predict() and not assign - pull out number and manually append to list below


# actual commercial mean meat weight in year t
growthrate.com$MW.actual.com[growthrate.com$YEAR == year] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                               re.form=~0,type="response") #11.62429 

# actual recruit mean meat weight in year t
growthrate.rec$MW.actual.rec[growthrate.rec$YEAR == year] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                               re.form=~0,type="response") #4.888419



spa6.growthrate.com
#spa6.growthrate.com <- rbind(spa6.growthrate.com, c(2020, rep(NA,3))) # this line can be remove in future year (2022)
spa6.growthrate.com <- rbind(spa6.growthrate.com, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Commercial"), GrowthMethod =c("Actual"))) 
spa6.growthrate.com
spa6.growthrate.com$MW.Predict[spa6.growthrate.com$Year == year-1] <- growthrate.com$MW.pred.com[growthrate.com$YEAR == year-1]
spa6.growthrate.com$MW.Actual[spa6.growthrate.com$Year == year] <- growthrate.com$MW.actual.com[growthrate.com$YEAR == year]
spa6.growthrate.com$rate <- spa6.growthrate.com$MW.Predict/spa6.growthrate.com$MW.Actual
spa6.growthrate.com


spa6.growthrate.rec
#spa6.growthrate.rec <- rbind(spa6.growthrate.rec, c(2020, rep(NA,3))) # this line can be remove in future year (2022)
spa6.growthrate.rec <- rbind(spa6.growthrate.rec, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Recruit"), GrowthMethod =c("Actual")))
spa6.growthrate.rec
spa6.growthrate.rec$MW.Predict[spa6.growthrate.rec$Year == year-1] <- growthrate.rec$MW.pred.rec[growthrate.com$YEAR == year-1]
spa6.growthrate.rec$MW.Actual[spa6.growthrate.rec$Year == year] <- growthrate.rec$MW.actual.rec[growthrate.com$YEAR == year]
spa6.growthrate.rec$rate <- spa6.growthrate.rec$MW.Predict/spa6.growthrate.rec$MW.Actual
spa6.growthrate.rec

#Add columns
spa6.growthrate.com$Age <- "Commercial"
spa6.growthrate.rec$Age <- "Recruit"
spa6.growthrate.com$GrowthMethod <- "Actual"
spa6.growthrate.rec$GrowthMethod <- "Actual"


 
# Plot growth rate for commercial & recruit scallops

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA6_GrowthRate_ComRec_Actual.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x<-c(1996,year) 
y<-c(0.8,3)

plot (x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA6 Actual Growth Rate")
lines(spa6.growthrate.com[,c(1,4)], type="b", pch=1, lty=1, col=1)
lines(spa6.growthrate.rec[,c(1,4)], type="b", pch=2, lty=2, col=2)
abline(h=1, lty=3)
legend (1997, 2.7, bty="n", legend=c("Commerical","Recruit"), pch=c(1,2), col=c(1,2))

dev.off()

# ---- Predicted Growth Rates ---- 
# Calcuate mean weight of commercial and recruit animals:
#1. In year t ("actual") using mean SH in year t and meat weight shell height relationship in year t
#2. In year t+1 ("pred") using predicted mean SH in year t+1 (predicted from year t) and meat weight shell height relationship in year t

# For the prediction evaluation plots, need to know what the predicted g and gR were from year t to t+1, assuming we don't know (e.g., as if it were 2009 and we were predicting for 2010 based solely on 2009 data); In the current year (2015) I go back to 2009 in the modelling
#objects to hold predicted meat weights and growth rate: 
growthrate.com.pred <- data.frame(YEAR = c(year), MW.actual.com = NA,  MW.pred.com = NA)
growthrate.rec.pred <- data.frame(YEAR = c(year), MW.actual.rec = NA, MW.pred.rec = NA)

#commercial
growthrate.com.pred$MW.actual.com <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                                re.form=~0,type="response") 

growthrate.com.pred$MW.pred.com <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                              re.form=~0,type="response") 

#recruit
growthrate.rec.pred$MW.actual.rec <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                                re.form=~0,type="response")  


growthrate.rec.pred$MW.pred.rec <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                              re.form=~0,type="response") 

spa6.predictedgr.com
#spa6.predictedgr.com <- rbind(spa6.predictedgr.com, c(2020, rep(NA,3))) #can remove in 2022 
spa6.predictedgr.com <-rbind(spa6.predictedgr.com, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Commercial"), GrowthMethod =c("Predict")))
spa6.predictedgr.com
spa6.predictedgr.com$MW.Actual[spa6.predictedgr.com$Year == year] <- growthrate.com.pred$MW.actual.com[growthrate.com.pred$YEAR == year]
spa6.predictedgr.com$MW.Predict[spa6.predictedgr.com$Year == year] <- growthrate.com.pred$MW.pred.com[growthrate.com.pred$YEAR == year]
spa6.predictedgr.com$rate <- spa6.predictedgr.com$MW.Predict/spa6.predictedgr.com$MW.Actual
spa6.predictedgr.com



spa6.predictedgr.rec
#spa6.predictedgr.rec <- rbind(spa6.predictedgr.rec, c(2020, rep(NA,3))) #can remove in 2022 
spa6.predictedgr.rec <- rbind(spa6.predictedgr.rec,  data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Recruit"), GrowthMethod =c("Predict")))
spa6.predictedgr.rec
spa6.predictedgr.rec$MW.Actual[spa6.predictedgr.rec$Year == year] <- growthrate.rec.pred$MW.actual.rec[growthrate.rec.pred$YEAR == year]
spa6.predictedgr.rec$MW.Predict[spa6.predictedgr.rec$Year == year] <- growthrate.rec.pred$MW.pred.rec[growthrate.rec.pred$YEAR == year]
spa6.predictedgr.rec$rate <- spa6.predictedgr.rec$MW.Predict/spa6.predictedgr.rec$MW.Actual
spa6.predictedgr.rec


#Add columns
spa6.predictedgr.com$Age <- "Commercial"
spa6.predictedgr.rec$Age <- "Recruit"
spa6.predictedgr.com$GrowthMethod <- "Predict"
spa6.predictedgr.rec$GrowthMethod <- "Predict"

# Bind all growth rate objects into sinlge object for export
spa6.growthrate <- rbind(spa6.growthrate.com, spa6.growthrate.rec, spa6.predictedgr.com, spa6.predictedgr.rec) 
#export the objects
write.csv(spa6.growthrate, paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA",area,"/spa6.growthrate.",year,".csv"))


#plot actual vs predicted growth rate for commercial scallops
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA6_GrowthRate_Com_ActualPredicted.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x <- c(1996,year) 
y <- c(0.7,2.0)

plot (x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA6 Commerical Growth Rate")
lines(spa6.growthrate.com[,c(1,4)], type="b", pch=1, lty=1, col=1)
lines(spa6.predictedgr.com[,c(1,4)], type="b", pch=17, lty=1, col=1) # predicted value for 2015 to 2016
abline(h=1, lty=3)
legend (2015, 2.0, bty="n", legend=c("Actual", "Predicted"), pch=c(1,17), col=c(1,1))

dev.off()

#plot actual vs predicted growth rate for recruit scallops 
#windows()
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA6_GrowthRate_Rec_ActualPredicted.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x<-c(1996,year) 
y<-c(0.8,3)

plot (x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA6 Recruit Growth Rate")
lines(spa6.growthrate.rec[,c(1,4)], type="b", pch=1, lty=1, col=1)
lines(spa6.predictedgr.rec[,c(1,4)], type="b", pch=17, lty=1, col=1) 
abline(h=1, lty=3)
legend (2015, 2.85, bty="n", legend=c("Actual", "Predicted"), pch=c(1,17), col=c(1,1))

dev.off()

