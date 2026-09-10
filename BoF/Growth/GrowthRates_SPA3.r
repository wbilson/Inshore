# Updated Growth Rate calculations for BoF Assessment Areas 
# J.Sameoto June 2020
# Includes Actual Growth rates and Predicted Growth Rates

# ---- SPA 3 GROWTH RATES ----

# required packages
library(lme4)
library(dplyr)
library(ggplot2)
library(tidyverse)

options(stringsAsFactors = FALSE)

# ///.... DEFINE THESE ENTRIES ....////

#DEFINE: year, area
year <- 2026  #this is the survey year
area <- "3"  #SPAs 1A, 1B and 4 and 5 all modelled together, therefore choice entry here is "1A1B4and5", "3", "6"
assessmentyear <- 2026 #this is the year you are running your assessment in -- corresponds to the assessment folder year name e.g. INSHORE SCALLOP/2020/Assessment..


# DEFINE: load required workspace with model objects (set for year YYYY)
load(paste0("Y:/Inshore/Assessment/BoF/",year,"/Assessment/Data/Growth/SPA3/BIgrowth",year,".RData"))
#load(paste0("Y:/Inshore/BoF/",year,"/Assessment/Data/Growth/SPA3/BIgrowth",year,".RData"))

# DEFINE: load shell height objects (set for year YYYY)
source(paste0("Y:/Inshore/Assessment/BoF/",year,"/Assessment/Data/Growth/SPA3/SPA3.SHobj.",year,".R"))
#source(paste0("Y:/Inshore/BoF/",year,"/Assessment/Data/Growth/SPA3/SPA3.SHobj.",year,".R"))
#Check that you have correctly identified all required Shell Height objects and they are within the workspace (should have been loaded via the shell height object above)
sh.actual
sh.predict

cbind(sh.actual, sh.predict) #check - assumes ordered correctly; check order of column names for renaming columns 
SH.object <- cbind(sh.actual %>% dplyr::select(years, SHactual.Com = SPA3.SHactual.Com, SHactual.Rec = SPA3.SHactual.Rec), 
                   sh.predict %>% dplyr::select(SHpredict.Com = SPA3.SHpredict.Com, SHpredict.Rec = SPA3.SHpredict.Rec))

# DEFINE: Source previous year meat weight and growth rate object for ACTUAL growth rates:
# if your year defined above it for YYYY, then you should be bringing in the YYYY-1 growth rate object.
spa3.growthrate <- read.csv(paste0("Y:/Inshore/Assessment/BoF/",year-1,"/Assessment/Data/Growth/SPA3/spa3.growthrate.",year-1,".csv"))
#spa3.growthrate <- read.csv(paste0("Y:/Inshore/BoF/",year-1,"/Assessment/Data/Growth/SPA3/spa3.growthrate.",year-1,".csv"))
spa3.growthrate <- spa3.growthrate[,-1]
spa3.growthrate
str(spa3.growthrate)

#break out data to object: 
spa3.growthrate.com <- spa3.growthrate[spa3.growthrate$Age == "Commercial" & spa3.growthrate$GrowthMethod == "Actual",]
spa3.growthrate.rec <- spa3.growthrate[spa3.growthrate$Age == "Recruit" & spa3.growthrate$GrowthMethod == "Actual",]
spa3.predictedgr.com <- spa3.growthrate[spa3.growthrate$Age == "Commercial" & spa3.growthrate$GrowthMethod == "Predict",]
spa3.predictedgr.rec <- spa3.growthrate[spa3.growthrate$Age == "Recruit" & spa3.growthrate$GrowthMethod == "Predict",]


# DEFINE: Identify model objects:
model.object <- MWTSHBI.YYYY

#DEFINE: Identify data object; NOTE if model above is MWTSHBF.2017 then this was run on 2017 data so you want the data as BFdetail2017:
data <- BIdetailYYYY

# DEFINE path for figures and dataouput to be saved; note expects within this folder that you've created a "dataoutput" and "Figures" folder under the following directory path;  MUST HAVE "/" at the end of your path!
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

#////... END OF DEFINE SECTION ...////



#prepare data object - log transform depth and height 
summary(data)
data <- data[complete.cases(data$HEIGHT),] #remove rows that have no height 
data$Log.HEIGHT <- log(data$HEIGHT)
data$Log.DEPTH <- log(abs(data$ADJ_DEPTH)) #take abs to keep value positive
summary(data)


# Depth for prediction, for SPA3 VMS IN Modelled Area -47.63	This is mean depth of area corresponding with full area modelled for SPA 3 -- SMB plus the inside VMS area in Brier Lurcher
depth <- -47.63

# ---- Actual Growth Rates ----

# Calculate mean weight of commercial and recruit animals:
#1. In year t ("actual") using mean SH in year t and meat weight shell height relationship in year t
#2. In year t+1 ("pred") using predicted mean SH in year t+1 (predicted from year t) and meat weight shell height relationship in year t+1

#objects to hold predicted meat weights and growth rate: 
growthrate.com <- data.frame(YEAR = c(year-1, year), MW.actual.com = NA,  MW.pred.com = NA)
growthrate.rec <- data.frame(YEAR = c(year-1, year), MW.actual.rec = NA, MW.pred.rec = NA)

#An assessment run from here: 
test.data <- subset(data, YEAR == year & HEIGHT > 40) #data subsetted as it was modelled

# predicted commercial mean meat weight in year t-1 using predicted SH (SH predicted from year t-1 to year t) and actual mw/sh relationship in year t
growthrate.com$MW.pred.com[growthrate.com$YEAR == year-1] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$years == year-1])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                             re.form=~0,type="response")  

# predicted recruit mean meat weight in year t-1 using predicted SH (SH predicted from year t-1 to year t) and actual mw/sh relationship in year t
growthrate.rec$MW.pred.rec[growthrate.rec$YEAR == year-1] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$years == year-1])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                             re.form=~0,type="response")


# actual commercial mean meat weight in year t
growthrate.com$MW.actual.com[growthrate.com$YEAR == year] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                               re.form=~0,type="response") 

# actual recruit mean meat weight in year t
growthrate.rec$MW.actual.rec[growthrate.rec$YEAR == year] <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                               re.form=~0,type="response") 

#Commercial growth rate:
spa3.growthrate.com
#spa3.growthrate.com <- rbind(spa3.growthrate.com, c(2020, rep(NA,3))) # this line can be remove in future year (2022)
spa3.growthrate.com <- rbind(spa3.growthrate.com, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Commercial"), GrowthMethod =c("Actual"))) 
spa3.growthrate.com
str(spa3.growthrate.com)
spa3.growthrate.com$MW.Predict[spa3.growthrate.com$Year == year-1] <- growthrate.com$MW.pred.com[growthrate.com$YEAR == year-1]
spa3.growthrate.com
spa3.growthrate.com$MW.Actual[spa3.growthrate.com$Year == year] <- growthrate.com$MW.actual.com[growthrate.com$YEAR == year]
spa3.growthrate.com
spa3.growthrate.com$rate <- spa3.growthrate.com$MW.Predict/spa3.growthrate.com$MW.Actual
spa3.growthrate.com


#Recruit Growth Rate: 
spa3.growthrate.rec
#spa3.growthrate.rec <- rbind(spa3.growthrate.rec, c(2020, rep(NA,3))) # this line can be remove in future year (2022)
spa3.growthrate.rec <- rbind(spa3.growthrate.rec, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Recruit"), GrowthMethod =c("Actual"))) 
spa3.growthrate.rec
str(spa3.growthrate.rec)
spa3.growthrate.rec$MW.Predict[spa3.growthrate.rec$Year == year-1] <- growthrate.rec$MW.pred.rec[growthrate.com$YEAR == year-1]
spa3.growthrate.rec$MW.Actual[spa3.growthrate.rec$Year == year] <- growthrate.rec$MW.actual.rec[growthrate.com$YEAR == year]
spa3.growthrate.rec$rate <- spa3.growthrate.rec$MW.Predict/spa3.growthrate.rec$MW.Actual
spa3.growthrate.rec


#Add columns
#spa3.growthrate.com$Age <- "Commercial"
#spa3.growthrate.rec$Age <- "Recruit"
#spa3.growthrate.com$GrowthMethod <- "Actual"
#spa3.growthrate.rec$GrowthMethod <- "Actual"


# Plot growth rate for commercial & recruit scallops
#Save out figure (be sure to view figure first)
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA3_GrowthRate_ComRec_Actual.png"), type="cairo", width=20, height=12, units = "cm", res=400)

actual.growth.plot.title <- "SPA3 Actual Growth Rate"

x<-c(1996,year) #update year
y<-c(0.6,max(spa3.growthrate.rec$rate, na.rm = T))

plot(x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main=actual.growth.plot.title)
lines(spa3.growthrate.com[,c(1,4)], type="b", pch=1, lty=1, col=1)
lines(spa3.growthrate.rec[,c(1,4)], type="b", pch=2, lty=2, col=2)
abline(h=1, lty=3)
legend (1997, 1.99, bty="n", legend=c("Commerical","Recruit"), pch=c(1,2), col=c(1,2))

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
                                re.form=~0,type="response") # 15.4808 

growthrate.com.pred$MW.pred.com <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                              re.form=~0,type="response") # 16.90129 

#recruit
growthrate.rec.pred$MW.actual.rec <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                                re.form=~0,type="response") #5.195234 


growthrate.rec.pred$MW.pred.rec <- predict(model.object,newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$years == year])-mean(test.data$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth))-mean(test.data$Log.DEPTH)),
                              re.form=~0,type="response") #7.475083


spa3.predictedgr.com
#spa3.predictedgr.com <- rbind(spa3.predictedgr.com, c(2020, rep(NA,3))) # this line can be remove in future year (2022)
spa3.predictedgr.com <- rbind(spa3.predictedgr.com, data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Commercial"), GrowthMethod =c("Predict")))
spa3.predictedgr.com
str(spa3.predictedgr.com)
spa3.predictedgr.com$MW.Actual[spa3.predictedgr.com$Year == year] <- growthrate.com.pred$MW.actual.com[growthrate.com.pred$YEAR == year]
spa3.predictedgr.com$MW.Predict[spa3.predictedgr.com$Year == year] <- growthrate.com.pred$MW.pred.com[growthrate.com.pred$YEAR == year]
spa3.predictedgr.com$rate <- spa3.predictedgr.com$MW.Predict/spa3.predictedgr.com$MW.Actual
spa3.predictedgr.com


#Recruit
spa3.predictedgr.rec
#spa3.predictedgr.rec <- rbind(spa3.predictedgr.rec, c(2020, rep(NA,3))) # this line can be remove in future year (2022)
spa3.predictedgr.rec <- rbind(spa3.predictedgr.rec,  data.frame(Year=year,  MW.Actual = NA,  MW.Predict = NA, rate = NA, Age = c("Recruit"), GrowthMethod =c("Predict")))
spa3.predictedgr.rec
str(spa3.predictedgr.rec)
spa3.predictedgr.rec$MW.Actual[spa3.predictedgr.rec$Year == year] <- growthrate.rec.pred$MW.actual.rec[growthrate.rec.pred$YEAR == year]
spa3.predictedgr.rec$MW.Predict[spa3.predictedgr.rec$Year == year] <- growthrate.rec.pred$MW.pred.rec[growthrate.rec.pred$YEAR == year]
spa3.predictedgr.rec$rate<-spa3.predictedgr.rec$MW.Predict/spa3.predictedgr.rec$MW.Actual
spa3.predictedgr.rec

#Add columns
#spa3.predictedgr.com$Age <- "Commercial"
#spa3.predictedgr.rec$Age <- "Recruit"
#spa3.predictedgr.com$GrowthMethod <- "Predict"
#spa3.predictedgr.rec$GrowthMethod <- "Predict"

# Bind all growth rate objects into sinlge object for export
spa3.growthrate <- rbind(spa3.growthrate.com, spa3.growthrate.rec, spa3.predictedgr.com, spa3.predictedgr.rec) 
#export the objects
write.csv(spa3.growthrate, paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA",area,"/spa3.growthrate.",year,".csv"))


#plot actual vs predicted growth rate for commercial scallops
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA3_GrowthRate_Com_ActualPredicted.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x<-c(1996,year) 
y<-c(0.6,2.0)

plot (x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA3 Commerical Growth Rate")
lines(spa3.growthrate.com[,c(1,4)], type="b", pch=1, lty=1, col=1)
lines(spa3.predictedgr.com[,c(1,4)], type="b", pch=17, lty=1, col=1) # predicted value for 2015 to 2016
abline(h=1, lty=3)
legend (2015, 1.5, bty="n", legend=c("Actual", "Predicted"), pch=c(1,17), col=c(1,1))

dev.off()

#plot actual vs predicted growth rate for recruit scallops 
#Save out figure (be sure to view figure first)
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA3_GrowthRate_Rec_ActualPredicted.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x<-c(1996,year)
y<-c(0.8,max(spa3.growthrate.rec$rate, na.rm = T))

plot (x,y, type="n",xlab="",ylab="Growth Rate", cex.axis=1.3, cex.lab=1.5, main="SPA3 Recruit Growth Rate")
lines(spa3.growthrate.rec[,c(1,4)], type="b", pch=1, lty=1, col=1)
lines(spa3.predictedgr.rec[,c(1,4)], type="b", pch=17, lty=1, col=1) 
abline(h=1, lty=3)
legend (2015, 1.95, bty="n", legend=c("Actual", "Predicted"), pch=c(1,17), col=c(1,1))

dev.off()

