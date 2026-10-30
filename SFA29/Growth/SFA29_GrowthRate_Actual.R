##........................................###
###
###    Growth Rate - Actual
###   i.e. use actual year t+1 mt-wt-SH relationship in year t+1 
###    SFA 29 
###    J. Sameoto March 2016 
###    Revamped Dec 2021 J.Sameoto
###........................................###

# required packages
library(lme4)
library(dplyr)
library(ggplot2)
library(tidyverse)

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

options(stringsAsFactors = FALSE)


# ---- Prep work to define objects that will be needed for growth rate calculations ----

# ///.... DEFINE THESE ENTRIES ....////

#DEFINE: year, area
year <- 2026  #this is the survey year
assessmentyear <- 2027 #this is the year you are running your assessment in -- corresponds to the assessment folder year name 
# DEFINE path for figures and dataouput to be saved; note expects within this folder that you've created a "dataoutput" and "Figures" folder under the following directory path;  MUST HAVE "/" at the end of your path! (shouldn't have to change this in most years with new folder structure)
path.directory <- "Y:/Inshore/Assessment/SFA29/"
#path.directory <- "Y:/Inshore/SFA29/"

# DEFINE: load shell height objects 
SFA29.SHactualpredict <- read.csv(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear,"/Assessment/Data/Growth/SFA29.SHobj.",year,".csv"))
#SFA29.SHactualpredict <- read.csv(paste0("Y:/Inshore/SFA29/",assessmentyear,"/Assessment/Data/Growth/SFA29.SHobj.",year,".csv"))
#field size identifies if commercial or recruit
#field SHF is actual shell height in year t, field SHF.pred is predicted shell height in year t+1 
SFA29.SHactual.Com <- SFA29.SHactualpredict %>% filter(size == "commercial")  %>% dplyr::select(years=year, STRATA, SDM, SHactual.Com = SHF)
SFA29.SHactual.Rec <- SFA29.SHactualpredict %>% filter(size == "recruit") %>% dplyr::select(years=year, STRATA, SDM, SHactual.Rec = SHF)
  
SFA29.SHpredict.Com <- SFA29.SHactualpredict %>% filter(size == "commercial")  %>% 
  dplyr::select(years=year, STRATA, SDM, SHpredict.Com = SHF.pred)
SFA29.SHpredict.Rec <- SFA29.SHactualpredict %>% filter(size == "recruit") %>% 
  dplyr::select(years=year, STRATA, SDM, SHpredict.Rec = SHF.pred)

sh.actual <- merge(SFA29.SHactual.Com, SFA29.SHactual.Rec, by=c("years", "STRATA","SDM") ) 
sh.predict <- merge(SFA29.SHpredict.Com, SFA29.SHpredict.Rec, by=c("years", "STRATA","SDM") ) 

head(sh.actual)
head(sh.predict)

## WHy NA for Recruits.. - check SHF script - it's bc there were 0 recruits 
tail(sh.actual)
tail(sh.predict)

sh.actual %>% filter(years  %in% c(2023, 2024))
sh.predict %>% filter(years  %in% c(2023, 2024))


SH.object <- merge(sh.actual, sh.predict, by=c("years", "STRATA","SDM"))
SH.object

 
# DEFINE: Source previous year meat weight and growth rate object for ACTUAL & PREDITED growth rates:
##COPY over the growth.actual.2001toYYYY and the SFA29growthYYYY.RData from previous year - where YYYY is year-1 into the assessment year directory 
#Y:/Inshore/SFA29/YYYY/Assessment/Data/Growth/

# if your year defined above it 2019, then you should be bringing in the 2018 growth rate object.

###### !!!!!! Be sure to bring this file over to teh current year assessment folder !!!!!! ######
sfa29.growthrate <- read.csv(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear,"/Assessment/Data/Growth/growth.actual.2001to",year-1,".csv"))
#sfa29.growthrate <- read.csv("Y:/INSHORE SCALLOP/BoF/2020/Assessment/Data/Growth/SPA1A1B4and5/spa1a.growthrate.2019.csv")
#sfa29.growthrate <- sfa29.growthrate[,-1]
sfa29.growthrate <- sfa29.growthrate %>% arrange(strata, sdm, Year)

# DEFINE: load required workspace with model objects 
# need current year model object AND previous year model object 
# current year 
load(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear,"/Assessment/Data/Growth/SFA29growth",year,".RData"))
model.object.Y <- get(paste0("MWTSHSFA29.",year)) #Assign model.object.Y to current year model object - MWTSHSFA29.YYYY
#Previous year;
load(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear-1,"/Assessment/Data/Growth/SFA29growth",year-1,".RData"))
model.object.Yminus1 <- get(paste0("MWTSHSFA29.",year-1))

summary(model.object.Y)
summary(model.object.Yminus1)

#DEFINE: Identify data object; NOTE if model above is MWTSHBF.2017 then this was run on 2017 data so you want the data as BFdetail2017:
data.Y <- get(paste0("SFA29detail.",year))
data.Yminus1 <- get(paste0("SFA29detail.",year-1))
#In a given year, traditionally would use the data from the previous 

#////... END OF DEFINE SECTION ...////

#data object to hold predicted meat weights and growth rate:
growth.YYYY <- data.frame(Year = rep(year,15), strata = c(rep("SFA29A",6), rep("SFA29B",6),rep("SFA29C",6), rep("SFA29D",6), rep("SFA29E",6)), sdm = c(rep(c("low","low", "med", "med","high","high"),5)), MW.Actual = rep(NA,30), MW.Predict = rep(NA,30),   rate = rep(NA,30),   Age = rep(c("Commercial","Recruit"),15), GrowthMethod = rep("Actual", 30))  

#prepare data object - log transform depth and height for each year of data 
summary(data.Y)
data.Y <- data.Y[complete.cases(data.Y$HEIGHT),] #remove rows that have no height
data.Y$Log.HEIGHT <- log(data.Y$HEIGHT)
data.Y$Log.DEPTH <- log(abs(data.Y$ADJ_DEPTH)) #take abs to keep value positive
summary(data.Y)

summary(data.Yminus1)
data.Yminus1 <- data.Yminus1[complete.cases(data.Yminus1$HEIGHT),] #remove rows that have no height
data.Yminus1$Log.HEIGHT <- log(data.Yminus1$HEIGHT)
data.Yminus1$Log.DEPTH <- log(abs(data.Yminus1$ADJ_DEPTH)) #take abs to keep value positive
summary(data.Yminus1)

#create same data used to run model on
test.data.Y <- subset(data.Y, YEAR == year & HEIGHT > 40) #data subsetted as it was modelled
test.data.Yminus1 <- subset(data.Yminus1, YEAR == year-1 & HEIGHT > 40) #data subsetted as it was modelled 

# ---- Actual Growth Rates - meat weight ----
# Calculate mean weight of commercial and recruit animals:
#1. In year t ("actual") using mean SH in year t and meat weight shell height relationship in year t
#2. In year t+1 ("pred") using predicted mean SH in year t+1 (predicted from year t) and meat weight shell height relationship in year t+1
#Depth to predict Growth is fixed
# see'Y:/Inshore/SFA29/SFA29DepthProfile/AreaMeanDepths.xlsx' #File for the constant depth to predict on by area
#subarea A  -75.05039
#subarea B  -46.36262
#subarea C  -37.33779
#subarea D  -47.18775
#subarea E  -67.65107

# ---- Subarea A ----
strataID <- "SFA29A"
# Depth for prediction
depth.a <-  -75.05039
#subarea A  -75.05039

#...
# MED
#...
sdm <- "med"

#A medium commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#A medium commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#A medium Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#A medium Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


#...
# LOW
#...
sdm <- "low"

#A low commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#A low commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#A low Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#A low Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.a))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


# ---- Subarea B ----
strataID <- "SFA29B"
# Depth for prediction
depth.b <-  -46.36262
#subarea B  -46.36262

#...
# HIGH
#...
sdm <- "high"

#B high commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#B high commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#B high Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#B high Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


#...
# MED
#...
sdm <- "med"

#B medium commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#B medium commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#B medium Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#B medium Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


#...
# LOW
#...
sdm <- "low"

#B low commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#B low commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#B low Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#B low Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.b))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")



# ---- Subarea C ----
strataID <- "SFA29C"
# Depth for prediction
depth.c <-   -37.33779
#subarea C  -37.33779

#...
# HIGH
#...
sdm <- "high"

#C high commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#C high commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#C high Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#C high Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


#...
# MED
#...
sdm <- "med"

#C medium commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#C medium commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#C medium Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#C medium Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


#...
# LOW
#...
sdm <- "low"

#C low commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#C low commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#C low Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#C low Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.c))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")



# ---- Subarea D ----
strataID <- "SFA29D"
# Depth for prediction
depth.d <-  -47.18775
#subarea D  -47.18775

#...
# HIGH
#...
sdm <- "high"

#D high commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#D high commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#D high Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#D high Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


#...
# MED
#...
sdm <- "med"

#D medium commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#D medium commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#D medium Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#D medium Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")


#...
# LOW
#...
sdm <- "low"

#D low commercial - ACTUAL 
#Year t-1 model to predict weight using using Year t-1 commercial size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0, type="response")

#D low commercial - PREDICT 
#Year t model to predict weight using Year t-1 SH grown up 1 year (i.e. grown to year t) (SHpredict.com)
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Commercial"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Com[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Y$Log.DEPTH)), re.form=~0, type="response")


#D low Recruit - ACTUAL
#Year t-1 model to predict weight using using Year t-1 recruit size
growth.YYYY$MW.Actual[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Yminus1, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHactual.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Yminus1$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Yminus1$Log.DEPTH)), re.form=~0,type="response")

#D low Recruit - PREDICT
growth.YYYY$MW.Predict[growth.YYYY$strata==strataID & growth.YYYY$sdm== sdm & growth.YYYY$Age == "Recruit"] <- predict(model.object.Y, newdata=data.frame(Log.HEIGHT.CTR=log(SH.object$SHpredict.Rec[SH.object$STRATA==strataID & SH.object$SDM==sdm & SH.object$years == year-1]) - mean(test.data.Y$Log.HEIGHT), Log.DEPTH.CTR=log(abs(depth.d))-mean(test.data.Y$Log.DEPTH)), re.form=~0,type="response")



# --- calculate rate ----- 

growth.YYYY$rate <- growth.YYYY$MW.Predict/growth.YYYY$MW.Actual
growth.YYYY
#recode med as medium 
growth.YYYY$sdm[growth.YYYY$sdm=="med"] <- "medium"

unique(sfa29.growthrate$sdm)

head(growth.YYYY)
head(sfa29.growthrate)

#reorder so can merge 
sfa29.growthrate <- sfa29.growthrate %>% dplyr::select( Year, strata,  sdm, MW.Actual, MW.Predict, rate, Age, GrowthMethod)

#append to all other years and write out 
xx <- rbind(sfa29.growthrate, growth.YYYY) #Note, this gets arranged by strata, sdm and year when read in, so don't need to sort it before saving

write.csv(xx, paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/growth.actual.2001to",year,".csv"), row.names = FALSE) 

# --- PLOTS ---- 

# COMMERCIAL  
#Save out figure (be sure to view figure first)
#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29W.GrowthRate.Commercial.Actual.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x <- c(2002,year)
y <- c(0.5,1.75)

ggplot(xx %>% filter(Age == "Commercial" & strata != "SFA29E"), aes(y=rate, x=Year, group_by(sdm), color=sdm )) +
  geom_point(size = 3, aes(shape=sdm)) +
  facet_wrap(~strata) +
  geom_line(aes(linetype=sdm)) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'),breaks = c("high", "medium", "low"))+
  scale_linetype_manual(values = c(1:3), breaks = c("high", "medium", "low")) + scale_shape_manual(values = c(15:17), breaks = c("high", "medium", "low"))+
  xlab("Year") + ylab("Growth rate") +
  theme_bw() +
  coord_cartesian(ylim=y) +
  geom_hline(yintercept=1, color="black", linetype="dashed") + 
  # scale_y_continuous(breaks=seq(5, 20, 5))+
  theme(axis.title = element_text(size =15),
        axis.text = element_text(size = 12),
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)),
        legend.key.size = unit(6,"mm"),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.position = c(.07, .14),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1.5))


ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29W.GrowthRate.Commercial.Actual.png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)
#dev.off()

# RECRUIT 
#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29W.GrowthRate.Recruit.Actual.png"), type="cairo", width=20, height=12, units = "cm", res=400)

x <- c(2002,year)
y <- c(0.5,1.75)

ggplot(xx %>% filter(Age == "Recruit" & strata != "SFA29E"), aes(y=rate, x=Year, group_by(sdm), color=sdm )) +
  geom_point(size = 3, aes(shape=sdm)) +
  facet_wrap(~strata) +
  geom_line(aes(linetype=sdm), size = 0.74) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'),breaks = c("high", "medium", "low"))+
  scale_linetype_manual(values = c(1:3), breaks = c("high", "medium", "low")) + scale_shape_manual(values = c(15:17), breaks = c("high", "medium", "low"))+
  xlab("Year") + ylab("Growth rate") +
  theme_bw() +
  coord_cartesian(ylim=y) +
  geom_hline(yintercept=1, color="black", linetype="dashed") + 
  # scale_y_continuous(breaks=seq(5, 20, 5))+
  theme(axis.title = element_text(size =15),
        axis.text = element_text(size = 12),
        legend.key.size = unit(6,"mm"),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.position = c(.75, .17),
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1.5))

ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29W.GrowthRate.Recruit.Actual.png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#dev.off()

## END ## 