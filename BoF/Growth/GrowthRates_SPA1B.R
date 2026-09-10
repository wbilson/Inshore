# Updated Growth Rate calculations for BoF Assessment Areas 
# J.Sameoto June 2020
# Includes Actual Growth rates and Predicted Growth Rates

# ---- SPA 1B GROWTH RATES ----

# required packages
library(lme4)
library(dplyr)
library(ggplot2)
library(tidyverse)

options(stringsAsFactors = FALSE)

# ---- Prep work to define objects that will be needed for growth rate calculations, actual & predicted ---- 

# ///.... DEFINE THESE ENTRIES ....////

#DEFINE: year, area
year <- 2026  #this is the survey year
area <- "1A1B4and5"  #SPAs 1A, 1B and 4 and 5 all modelled together, therefore choice entry here is "1A1B4and5", "3", "6"
assessmentyear <- 2026 #this is the year you are running your assessment in -- corresponds to the assessment folder year name e.g. INSHORE SCALLOP/2020/Assessment..

# DEFINE: load required workspace with model objects -- should be from current year of assessment, eg. if survey from 2021 and running assessment in 2021 this is in 2021 assessment folder 
load(paste0(paste0("Y:/Inshore/Assessment/BoF/",year,"/Assessment/Data/Growth/SPA1A1B4and5/BFgrowth",year,".RData")))
#load(paste0(paste0("Y:/Inshore/BoF/",year,"/Assessment/Data/Growth/SPA1A1B4and5/BFgrowth",year,".RData")))

# DEFINE: load shell height objects- again should be current year of assessment, e.g. if survey from 2021 and running assessment in 2021 this is in 2021 assessment folder 
source(paste0("Y:/Inshore/Assessment/BoF/",year,"/Assessment/Data/Growth/SPA1A1B4and5/SPA1B",year,".SHobj.R"))
#source(paste0("Y:/Inshore/BoF/",year,"/Assessment/Data/Growth/SPA1A1B4and5/SPA1B",year,".SHobj.R"))
#Check that you have correctly identified all required Shell Height objects and they are within the workspace (should have been loaded via the shell height object above)
sh.actual
sh.predict 

cbind(sh.actual, sh.predict) #check - assumes ordered correctly; check order of column names for renaming columns 
SH.object <- cbind(sh.actual %>% select(years, SHactual.Com = SPA1B.SHactual.Com, SHactual.Rec = SPA1B.SHactual.Rec), 
                   sh.predict %>% select(SHpredict.Com = SPA1B.SHpredict.Com, SHpredict.Rec = SPA1B.SHpredict.Rec))
SH.object

# DEFINE: Source previous year meat weight and growth rate object for ACTUAL & PREDITED growth rates:
# if your year defined above it 2019, then you should be bringing in the 2018 growth rate object.
spa1b.growthrate <- read.csv(paste0("Y:/Inshore/Assessment/BoF/",year-1,"/Assessment/Data/Growth/SPA1A1B4and5/spa1b.growthrate.",year-1,".csv"))
#spa1b.growthrate <- read.csv(paste0("Y:/Inshore/BoF/",year-1,"/Assessment/Data/Growth/SPA1A1B4and5/spa1b.growthrate.",year-1,".csv"))
spa1b.growthrate <- spa1b.growthrate[,-1]

#break out data to object: 
spa1b.growthrate.com <- spa1b.growthrate[spa1b.growthrate$Age == "Commercial" & spa1b.growthrate$GrowthMethod == "Actual",]
spa1b.growthrate.rec <- spa1b.growthrate[spa1b.growthrate$Age == "Recruit" & spa1b.growthrate$GrowthMethod == "Actual",]
spa1b.predictedgr.com <- spa1b.growthrate[spa1b.growthrate$Age == "Commercial" & spa1b.growthrate$GrowthMethod == "Predict",]
spa1b.predictedgr.rec <- spa1b.growthrate[spa1b.growthrate$Age == "Recruit" & spa1b.growthrate$GrowthMethod == "Predict",]


# DEFINE: Identify model objects: 
model.object <- MWTSHBF.YYYY

#DEFINE: Identify data object; NOTE if model above is MWTSHBF.2017 then this was run on 2017 data so you want the data as BFdetail2017: 
data <- BFdetail.foryear

# DEFINE path for figures and dataouput to be saved; note expects within this folder that you've created a "dataoutput" and "Figures" folder under the following directory path;  MUST HAVE "/" at the end of your path! 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

#////... END OF DEFINE SECTION ...////


#for SPA 1AB start at 1996
years <- 1996:year


#prepare data object - log transform depth and height 
summary(data)
data <- data[complete.cases(data$HEIGHT),] #remove rows that have no height 
data$Log.HEIGHT <- log(data$HEIGHT)
data$Log.DEPTH <- log(abs(data$ADJ_DEPTH)) #take abs to keep value positive
summary(data)


# Depth for prediction, 
#for SPA 1B -50.09 m; see Y:\INSHORE SCALLOP\BoF\StandardDepth\BoFMeanDepths.csv
depth <- -50.09


# ---- Actual Growth Rates ----

# Calcuate mean weight of commercial and recruit animals:
#1. In year t ("actual") using mean SH in year t and meat weight shell height relationship in year t
#2. In year t+1 ("pred") using predicted mean SH in year t+1 (predicted from year t) and meat weight shell height relationship in year t+1

#objects to hold predicted meat weights and growth rate: 
growthrate.com <- data.frame(YEAR = c(year-1, year), MW.actual.com = NA,  MW.pred.com = NA)
growthrate.rec <- data.frame(YEAR = c(year-1, year), MW.actual.rec = NA, MW.pred.rec = NA)

#create same data used to run model on 
test.data <- subset(data, YEAR == year & HEIGHT > 40) #data subsetted as it was modelled

# predicted commercial mean meat weight in year t-1 using predicted SH (SH predicted from year t-1 to year t) and actual mw/sh relationship in year t
growthrate.com$MW.pred.com[growthrate.com$YEAR == year-1] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$years == year-1])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                             re.form=~0,type="response")  

# predicted recruit mean meat weight in year t-1 using predicted SH (SH predicted from year t-1 to year t) and actual mw/sh relationship in year t
growthrate.rec$MW.pred.rec[growthrate.rec$YEAR == year-1] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$years == year-1])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                             re.form=~0,type="response") 


# actual commercial mean meat weight in year t
growthrate.com$MW.actual.com[growthrate.com$YEAR == year] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                               re.form=~0,type="response") # 15.654

# actual recruit mean meat weight in year t
growthrate.rec$MW.actual.rec[growthrate.rec$YEAR == year] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                               re.form=~0,type="response") #5.8877


#Commercial growth rate:
spa1b.growthrate.com
#spa1b.growthrate.com <- rbind(spa1b.growthrate.com, c(2020, rep(NA,3))) #can remove in 2022 
spa1b.growthrate.com <- rbind(spa1b.growthrate.com, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Commercial"), GrowthMethod =c("Actual")))
spa1b.growthrate.com
spa1b.growthrate.com$MW.Predict[spa1b.growthrate.com$Year == year-1] <- growthrate.com$MW.pred.com[growthrate.com$YEAR == year-1]
spa1b.growthrate.com$MW.Actual[spa1b.growthrate.com$Year == year] <- growthrate.com$MW.actual.com[growthrate.com$YEAR == year]
spa1b.growthrate.com$rate <- spa1b.growthrate.com$MW.Predict/spa1b.growthrate.com$MW.Actual
spa1b.growthrate.com


#Recruit Growth Rate: 
spa1b.growthrate.rec
#spa1b.growthrate.rec <- rbind(spa1b.growthrate.rec, c(2020, rep(NA,3)))#can remove in 2022 
spa1b.growthrate.rec <-  rbind(spa1b.growthrate.rec, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Recruit"), GrowthMethod =c("Actual")))
spa1b.growthrate.rec
spa1b.growthrate.rec$MW.Predict[spa1b.growthrate.rec$Year == year-1] <- growthrate.rec$MW.pred.rec[growthrate.com$YEAR == year-1]
spa1b.growthrate.rec$MW.Actual[spa1b.growthrate.rec$Year == year] <- growthrate.rec$MW.actual.rec[growthrate.com$YEAR == year]
spa1b.growthrate.rec$rate <- spa1b.growthrate.rec$MW.Predict/spa1b.growthrate.rec$MW.Actual
spa1b.growthrate.rec


#Add columns
spa1b.growthrate.com$Age <- "Commercial"
spa1b.growthrate.rec$Age <- "Recruit"
spa1b.growthrate.com$GrowthMethod <- "Actual"
spa1b.growthrate.rec$GrowthMethod <- "Actual"


#export the objects to use in predicting mean weight
#dump(c('spa1b.growthrate.com','spa1b.growthrate.rec'),paste0(path.directory,'dataoutput/SPA1B.ActualGrowthRateObj.',year,'.R'))
#as csv objects for model input: 
#write.csv(spa1b.growthrate.com, paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA",area,"/spa1b.growthrate.com.",year,".csv"))
#write.csv(spa1b.growthrate.rec, paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA",area,"/spa1b.growthrate.rec.",year,".csv"))


# Note: MW.pred.com is what meat weight became from the previous years mean SH and MW-SH relationship, given these scallop grew and were now with the current MW SH relationship 
# e.g. if MW.pred.com column of year 2018 =  17.10 ; this is the meat weight of the scallop in 2019 that had grown from the average shell height in 2018, given the 2019 mw-SH relationship
# MW.actual.com of year 2018 = 15.14 ; this is the meat weight of the average commerical size scallop in 2019 given the mw-sh relationship in 2019
# Taking the ratio of spa3.growthrate.com$MW.Predict/spa3.growthrate.com$MW.Actual  gives what the difference in meat weight is, given the scallops have grown in SH according to our VonB, and also the new mw-sh relationship

 
# Plot growth rate for commercial & recruit scallops

#Save out figure (be sure to view figure first)
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_GrowthRate_ComRec_Actual.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x <- c(1997, year) 
y <- c(0.6,2.5)

plot(x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA 1B Actual Growth Rate")
lines(spa1b.growthrate.com$rate ~ spa1b.growthrate.com$Year , type="b", pch=1, lty=1, col=1)
lines(spa1b.growthrate.rec$rate ~ spa1b.growthrate.rec$Year , type="b", pch=2, lty=2, col=2)
abline(h=1, lty=3)
legend (1997, 2.5, bty="n", legend=c("Commerical","Recruit"), pch=c(1,2), col=c(1,2))

dev.off() 


# ---- Predicted Growth Rates ---- 
# Calcuate mean weight of commercial and recruit animals:
#1. In year t ("actual") using mean SH in year t and meat weight shell height relationship in year t
#2. In year t+1 ("pred") using predicted mean SH in year t+1 (predicted from year t) and meat weight shell height relationship in year t
# Note is using previous years growth rate from growth rate object from previous year e.g. SPA1APredictedGrowthRateObj.2016.R

# For the prediction evaluation plots, need to know what the predicted g and gR were from year t to t+1, assuming we don't know (e.g., as if it were 2009 and we were predicting for 2010 based solely on 2009 data); In the current year (2015) I go back to 2009 in the modelling
#objects to hold predicted meat weights and growth rate: 
growthrate.com.pred <- data.frame(YEAR = c(year), MW.actual.com = NA,  MW.pred.com = NA)
growthrate.rec.pred <- data.frame(YEAR = c(year), MW.actual.rec = NA, MW.pred.rec = NA)

#commercial
growthrate.com.pred$MW.actual.com <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                                re.form=~0,type="response") #16.80894  

growthrate.com.pred$MW.pred.com <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                              re.form=~0,type="response") # 18.11063 

#recruit
growthrate.rec.pred$MW.actual.rec <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                                re.form=~0,type="response") #4.914763 


growthrate.rec.pred$MW.pred.rec <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                              re.form=~0,type="response") #7.322197


spa1b.predictedgr.com
#spa1b.predictedgr.com <- rbind(spa1b.predictedgr.com, c(2020, rep(NA,3))) #remove in 2022 
spa1b.predictedgr.com <- rbind(spa1b.predictedgr.com, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Commercial"), GrowthMethod =c("Predict")))
spa1b.predictedgr.com
spa1b.predictedgr.com$MW.Actual[spa1b.predictedgr.com$Year == year] <- growthrate.com.pred$MW.actual.com[growthrate.com.pred$YEAR == year]
spa1b.predictedgr.com$MW.Predict[spa1b.predictedgr.com$Year == year] <- growthrate.com.pred$MW.pred.com[growthrate.com.pred$YEAR == year]
spa1b.predictedgr.com$rate <- spa1b.predictedgr.com$MW.Predict/spa1b.predictedgr.com$MW.Actual
spa1b.predictedgr.com


#Recruit
spa1b.predictedgr.rec
#spa1b.predictedgr.rec <- rbind(spa1b.predictedgr.rec, c(2020, rep(NA,3))) #remove in 2022 
spa1b.predictedgr.rec <- rbind(spa1b.predictedgr.rec,  data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Recruit"), GrowthMethod =c("Predict")))
spa1b.predictedgr.rec
spa1b.predictedgr.rec$MW.Actual[spa1b.predictedgr.rec$Year == year] <- growthrate.rec.pred$MW.actual.rec[growthrate.rec.pred$YEAR == year]
spa1b.predictedgr.rec$MW.Predict[spa1b.predictedgr.rec$Year == year] <- growthrate.rec.pred$MW.pred.rec[growthrate.rec.pred$YEAR == year]
spa1b.predictedgr.rec$rate<-spa1b.predictedgr.rec$MW.Predict/spa1b.predictedgr.rec$MW.Actual
spa1b.predictedgr.rec



#Add columns
spa1b.predictedgr.com$Age <- "Commercial"
spa1b.predictedgr.rec$Age <- "Recruit"
spa1b.predictedgr.com$GrowthMethod <- "Predict"
spa1b.predictedgr.rec$GrowthMethod <- "Predict"

# Bind all growth rate objects into sinlge object for export
spa1b.growthrate <- rbind(spa1b.growthrate.com, spa1b.growthrate.rec, spa1b.predictedgr.com, spa1b.predictedgr.rec) 
#export the objects
write.csv(spa1b.growthrate, paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA",area,"/spa1b.growthrate.",year,".csv"))


#export the objects to use in predicting mean weight
#dump (c('spa1b.predictedgr.com','spa1b.predictedgr.rec'),paste0(path.directory,'dataoutput/SPA1B.PredictedGrowthRateObj.',year,'.R'))
#as csv objects for model input: 
#write.csv(spa1b.predictedgr.com, paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA",area,"/spa1b.predictedgr.com.",year,".csv"))
#write.csv(spa1b.predictedgr.rec, paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA",area,"/spa1b.predictedgr.rec.",year,".csv"))


#plot actual vs predicted growth rate for commercial scallops

#Save out figure (be sure to view figure first)
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_GrowthRate_Com_ActualPredicted.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x <- c(1996,year) 
y <- c(0.6,1.75)
plot (x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA 1B Commercial Growth Rate")
lines(spa1b.growthrate.com$rate ~ spa1b.growthrate.com$Year, type="b", pch=1, lty=1, col=1)
lines(spa1b.predictedgr.com$rate ~ spa1b.predictedgr.com$Year, type="b", pch=17, lty=1, col=1) 
abline(h=1, lty=3)
legend (1997, 1.75, bty="n", legend=c("Actual", "Predicted"), pch=c(1,17), col=c(1,1))

dev.off()
# Save out plot



#plot actual vs predicted growth rate for recruit scallops 

#Save out figure (be sure to view figure first)
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_GrowthRate_Rec_ActualPredicted.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x <- c(1996,year)
y <- c(0.6,2.5)

plot(x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA 1B Recruit Growth Rate")
lines(spa1b.growthrate.rec$rate ~ spa1b.growthrate.rec$Year, type="b", pch=1, lty=1, col=1)
lines(spa1b.predictedgr.rec$rate ~ spa1b.predictedgr.rec$Year, type="b", pch=17, lty=1, col=1) 
abline(h=1, lty=3)
legend(year-5, 2.5, bty="n", legend=c("Actual", "Predicted"), pch=c(1,17), col=c(1,1))

dev.off()

print("END OF GROWTH RATE SCRIPT")


