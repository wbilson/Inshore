###........................................###
###
###    SPA 1B
###    Numbers per tow, Weight per tow
###    Population numbers and biomass
###
###   Rehauled July 2021J.Sameoto
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(PBSmapping)
library(spr) #version 1.04
library(cowplot)

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

#SQL - numbers by shell height bin;!NB! this code will exclude cruises that shouldn't be used: UB2000,UB1998,UB1999,UB1999b,JJ2004
quer1 <- ("SELECT *
FROM scallsur.scliveres
WHERE strata_id IN (35, 37, 38, 49, 51, 52, 53)
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
livefreq$CruiseID <- paste(livefreq$CRUISE,livefreq$TOW_NO,sep='.') #will be used to assign strata_id to cross ref files

####
# read in meat weight data; this is output from the meat weight/shell height modelling
####

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

#add YEAR column to data #NEW IN 2020 - check this doesn't break code!!!
BFliveweight$YEAR <- as.numeric(substr(BFliveweight$CRUISE,3,6))

#check data structure
table(BFliveweight$YEAR)
summary(BFliveweight)
str(BFliveweight)

#convert 
livefreq$lat <- convert.dd.dddd(livefreq$START_LAT) #format lat and lon
livefreq$lon <- convert.dd.dddd(livefreq$START_LONG)
livefreq$ID <- 1:nrow(livefreq)

BFliveweight$lat <- convert.dd.dddd(BFliveweight$START_LAT) #format lat and lon
BFliveweight$lon <- convert.dd.dddd(BFliveweight$START_LONG)
BFliveweight$ID <- 1:nrow(BFliveweight)


####
# post-stratify MBN for East/West line (do for both livefreq and liveweight)
####

# for Midbay North, need to assign strata to East/West
SPA1B.MBN.E <- data.frame(PID=58,POS=1:4,X=c(-65.710, -65.586, -65.197, -65.264),Y=c(45.280, 45.076, 45.237, 45.459)) #New strata
#SPA1B.MBN.E.sf <- st_as_sf(SPA1B.MBN.E, coords = c("X","Y"), crs = 4326) %>% summarise(do_union = FALSE) %>% st_cast("POLYGON")

# For Midbay North (strata id 38), separate into two areas, assign strata id "58" to MNB East   (strata_id 38 is now MBN West)
events <- subset(livefreq,STRATA_ID==38,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
livefreq$STRATA_ID[livefreq$ID%in%findPolys(events,SPA1B.MBN.E)$EID] <- 58

events <- subset(BFliveweight,STRATA_ID==38,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
BFliveweight$STRATA_ID[BFliveweight$ID%in%findPolys(events,SPA1B.MBN.E)$EID] <- 58

####
# read in cross reference files for spr
####

quer2 <- ( "SELECT *
          FROM scallsur.screpeatedtows
          WHERE screpeatedtows.cruise LIKE 'BF%'
          AND COMP_TYPE= 'R'")

crossref.BoF <- dbGetQuery(chan, quer2)

crossref.BoF$CruiseID <- paste(crossref.BoF$CRUISE_REF,crossref.BoF$TOW_NO_REF,sep='.')  #create CRUISE_ID on "parent/reference" tow

# merge STRATA_ID from BIlivefreq to the crosssref files based on parent/reference tow
crossref.BoF <- merge(crossref.BoF, subset (livefreq, select=c("STRATA_ID", "CruiseID")), by.x="CruiseID", all=FALSE)

str(crossref.BoF)

#Create ID column on cross reference files for spr
crossref.BoF$CruiseID <- paste(crossref.BoF$CRUISE_REF,crossref.BoF$TOW_NO_REF,sep='.')  #create CRUISE_ID on "parent" tow
crossref.BoF$CruiseID.current <- paste0(crossref.BoF$CRUISE,".",crossref.BoF$TOW_NO) #create CRUISE_ID on "child - current year" tow

#Check for potential errors that will break the SPR estimates
#merge STRATA_ID from BIlivefreq to the crosssref files based on parent/reference tow and find those that don't match for their strata_ID 
crossref.test <- left_join(crossref.BoF, livefreq %>% dplyr::select(CruiseID, STRATA_ID.current = STRATA_ID, TOW_TYPE_ID.current = TOW_TYPE_ID), by=c("CruiseID.current" = "CruiseID"))
crossref.test$STRATA_ID <- as.numeric(crossref.test$STRATA_ID)
crossref.test$STRATA_ID.current <- as.numeric(crossref.test$STRATA_ID.current)
crossref.test$strata_diff <- crossref.test$STRATA_ID -  crossref.test$STRATA_ID.current

#Strata IDs that need fixing 
crossref.test %>% filter(strata_diff!=0)

#tows that are not tow type 1 or 5 but are repeats that should not have been!
crossref.test %>% filter(!TOW_TYPE_ID.current%in%c(1,5))



# ---- correct data-sets for errors ----

#-some repearted tows will not be assigned to the same strata as the "parent" tow due to where the START LAT and LONG are
#-repeated tows should be corrected to match parent tow
#-some experimental tows were used as parent tows for repeats, these should be removed
#-these errors are identified by sleuthing after the spr function won't run

#1. In 2012, tow 323 assinged to 58, but parent tow in 38
livefreq[livefreq$CRUISE == "BF2012" & livefreq$TOW_NO == 323,]
livefreq$STRATA_ID[livefreq$CRUISE == "BF2012" & livefreq$TOW_NO == 323] <- "38"

BFliveweight[BFliveweight$CRUISE == "BF2012" & BFliveweight$TOW_NO == 323,]
BFliveweight$STRATA_ID[BFliveweight$CRUISE == "BF2012" & BFliveweight$TOW_NO == 323] <- "38"

#2. In 2013, Tow 288 was experimental but repeated in 2014.
crossref.BoF[crossref.BoF$CRUISE_REF == "BF2013" & crossref.BoF$TOW_NO_REF == 288,]
crossref.BoF <- crossref.BoF[-c(which(crossref.BoF$CRUISE_REF == "BF2013" & crossref.BoF$TOW_NO_REF == 288)),]

#3. In 2013, tow 96 was experimental, but repeated in 2014
crossref.BoF[crossref.BoF$CRUISE_REF == "BF2013" & crossref.BoF$TOW_NO_REF == "96",]
crossref.BoF <- crossref.BoF[-c(which(crossref.BoF$CRUISE_REF == "BF2013" & crossref.BoF$TOW_NO_REF == "96")),]

#4. In BF2019.211 was experimental but repated in BF2021 as tow 223
crossref.BoF[crossref.BoF$CRUISE_REF == "BF2019" & crossref.BoF$TOW_NO_REF == "211",]
crossref.BoF <- crossref.BoF[-c(which(crossref.BoF$CRUISE_REF == "BF2019" & crossref.BoF$TOW_NO_REF == "211")),]

#5. In 2026, tow 126 assinged to 38, but parent tow in 58
livefreq[livefreq$CRUISE == "BF2026" & livefreq$TOW_NO == 126,]
livefreq$STRATA_ID[livefreq$CRUISE == "BF2026" & livefreq$TOW_NO == 126] <- "58"

BFliveweight[BFliveweight$CRUISE == "BF2026" & BFliveweight$TOW_NO == 126,]
BFliveweight$STRATA_ID[BFliveweight$CRUISE == "BF2026" & BFliveweight$TOW_NO == 126] <- "58"


## ---- Subset data for individual years for SPR command (ONLY TOW_TYPE==1 AND  ONLY TOW_TYPE==5) ----
tows <- c(1,5)
#livefreq 
year <- c(seq(2008,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(livefreq, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("livefreq", i), sub)
}


#liveweight 
year <- c(seq(2008,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(BFliveweight, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("liveweight", i), sub)
}

#Crossref 
year <- c(seq(2009,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(crossref.BoF, CRUISE==paste0("BF",i))
  assign(paste0("crossref.BoF.", i), sub)
}

#Note: checking for mismatches between parent and child tows for repeats in a pain in the a$$... 	

####
###
### ---- RECRUIT AND COMMERCIAL NUMBERS PER TOW AND POPULATION NUMBERS ---- 
###
####


####
# ---- 1. Cape Spencer (simple mean for whole area) ----
####

############ Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 1997:surveyyear
X <- length(years)

SPA1B.CS.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep (191023.77,X), Area=rep("CS", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1B.CS.Rec.simple$Year)){
  temp.data <- livefreq[livefreq$YEAR==1996+i,]

SPA1B.CS.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1B.CS.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA1B.CS.Rec.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.CS.Rec.simple$Year, SPA1B.CS.Rec.simple$Mean.nums, xout=2020) #  7.677586
SPA1B.CS.Rec.simple[SPA1B.CS.Rec.simple$Year==2020,c("Mean.nums","var.y")] <- c(7.677586, 308.65456) #assume var from 2019

#calculate CV and Pop. estimate
SPA1B.CS.Rec.simple$sd <- sqrt(SPA1B.CS.Rec.simple$var.y)
SPA1B.CS.Rec.simple$Pop <- SPA1B.CS.Rec.simple$Mean.nums*SPA1B.CS.Rec.simple$NH


############ Commercial (80+ mm)

#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 1997:surveyyear
X <- length(years)

SPA1B.CS.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(191023.77, X), Area=rep("CS", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1B.CS.Comm.simple$Year))
  {
  temp.data <- livefreq[livefreq$YEAR==1996+i,]

SPA1B.CS.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
SPA1B.CS.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA1B.CS.Comm.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.CS.Comm.simple$Year, SPA1B.CS.Comm.simple$Mean.nums, xout=2020) #  249.2759
SPA1B.CS.Comm.simple[SPA1B.CS.Comm.simple$Year==2020,c("Mean.nums","var.y")] <- c(249.2759, 73551.825) #assume var from 2019

#calculate CV and Pop. estimate
SPA1B.CS.Comm.simple$sd <- sqrt(SPA1B.CS.Comm.simple$var.y)
SPA1B.CS.Comm.simple$Pop <- SPA1B.CS.Comm.simple$Mean.nums*SPA1B.CS.Comm.simple$NH

#combine Recruit and Commercial dataframes
SPA1B.CS.Numbers <- rbind(SPA1B.CS.Rec.simple,SPA1B.CS.Comm.simple)

####
# ---- 2. Middle Bay North (spr) ----
####

#2a. Middle Bay North - EAST

####Recruit (65-79)

years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNE.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), Prop=rep(0.732, X))
for(i in 1:length(SPA1B.MBNE.Rec.simple$Year)){
	temp.data <- livefreq[livefreq$YEAR==1996+i,]
SPA1B.MBNE.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==58 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1B.MBNE.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==58 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}

SPA1B.MBNE.Rec.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNE.Rec.simple$Year, SPA1B.MBNE.Rec.simple$Mean.nums, xout=2020) #   32.899
SPA1B.MBNE.Rec.simple[SPA1B.MBNE.Rec.simple$Year==2020,c("Mean.nums","var.y")] <- c( 32.899, 405.588433 ) #assume var from 2019


#SPR for recruits
#dataframe for SPR estimates Recruit
spryears <- 2010:surveyyear #update to most recent year
Y <- length(spryears)
MBNE.rec.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009
# mbErec<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==58],apply(livefreq2008[livefreq2008$STRATA_ID==58,21:23],1,sum),
#     livefreq2009$TOW_NO[livefreq2009$STRATA_ID==58],apply(livefreq2009[livefreq2009$STRATA_ID==58,24:26],1,sum),
#     crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
# No repeat tows in MBNE in 2009

#2009/2010 spr
mbErec1<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==58],apply(livefreq2009[livefreq2009$STRATA_ID==58,21:23],1,sum),
    livefreq2010$TOW_NO[livefreq2010$STRATA_ID==58],apply(livefreq2010[livefreq2010$STRATA_ID==58,24:26],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec1)  #

MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
# mbErec2<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==58],apply(livefreq2010[livefreq2010$STRATA_ID==58,21:23],1,sum),
#     livefreq2011$TOW_NO[livefreq2011$STRATA_ID==58],apply(livefreq2011[livefreq2011$STRATA_ID==58,24:26],1,sum),
#     crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
# K<-summary (mbErec2,summary (mbErec1)) # SD is zero, use simple mean
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2011,c(2:3)] <- c(0.9600000, 4.478345)

#2011/2012 spr
mbErec3<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==58],apply(livefreq2011[livefreq2011$STRATA_ID==58,21:23],1,sum),
    livefreq2012$TOW_NO[livefreq2012$STRATA_ID==58],apply(livefreq2012[livefreq2012$STRATA_ID==58,24:26],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec3)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
mbErec4<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==58],apply(livefreq2012[livefreq2012$STRATA_ID==58,21:23],1,sum),
    livefreq2013$TOW_NO[livefreq2013$STRATA_ID==58],apply(livefreq2013[livefreq2013$STRATA_ID==58,24:26],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec4)#
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
mbErec5<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==58],apply(livefreq2013[livefreq2013$STRATA_ID==58,21:23],1,sum),
    livefreq2014$TOW_NO[livefreq2014$STRATA_ID==58],apply(livefreq2014[livefreq2014$STRATA_ID==58,24:26],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec5)
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
mbErec6<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==58],apply(livefreq2014[livefreq2014$STRATA_ID==58,21:23],1,sum),
    livefreq2015$TOW_NO[livefreq2015$STRATA_ID==58],apply(livefreq2015[livefreq2015$STRATA_ID==58,24:26],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec6)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
mbErec7<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==58],apply(livefreq2015[livefreq2015$STRATA_ID==58,21:23],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID==58],apply(livefreq2016[livefreq2016$STRATA_ID==58,24:26],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec7)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
mbErec8<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==58],apply(livefreq2016[livefreq2016$STRATA_ID==58,21:23],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID==58],apply(livefreq2017[livefreq2017$STRATA_ID==58,24:26],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec8)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
mbErec9<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==58],apply(livefreq2017[livefreq2017$STRATA_ID==58,21:23],1,sum),
             livefreq2018$TOW_NO[livefreq2018$STRATA_ID==58],apply(livefreq2018[livefreq2018$STRATA_ID==58,24:26],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec9)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
mbErec10<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==58],apply(livefreq2018[livefreq2018$STRATA_ID==58,21:23],1,sum),
             livefreq2019$TOW_NO[livefreq2019$STRATA_ID==58],apply(livefreq2019[livefreq2019$STRATA_ID==58,24:26],1,sum),
             crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbErec10)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 spr #no survey in 2020 
mbErec11<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==58],apply(livefreq2019[livefreq2019$STRATA_ID==58,21:23],1,sum),
              livefreq2021$TOW_NO[livefreq2021$STRATA_ID==58],apply(livefreq2021[livefreq2021$STRATA_ID==58,24:26],1,sum),
              crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbErec11)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr #no survey in 2020 
mbErec12<-spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==58],apply(livefreq2021[livefreq2021$STRATA_ID==58,21:23],1,sum),
              livefreq2022$TOW_NO[livefreq2022$STRATA_ID==58],apply(livefreq2022[livefreq2022$STRATA_ID==58,24:26],1,sum),
              crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbErec12)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr 
mbErec13<-spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==58],apply(livefreq2022[livefreq2022$STRATA_ID==58,21:23],1,sum),
              livefreq2023$TOW_NO[livefreq2023$STRATA_ID==58],apply(livefreq2023[livefreq2023$STRATA_ID==58,24:26],1,sum),
              crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbErec13)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr 
#mbErec14<-spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==58],apply(livefreq2023[livefreq2023$STRATA_ID==58,21:23],1,sum),
#              livefreq2024$TOW_NO[livefreq2024$STRATA_ID==58],apply(livefreq2024[livefreq2024$STRATA_ID==58,24:26],1,sum),
#              crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
#K<-summary(mbErec14)  #
#MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#Check for all zero tows -- 2023 repeated tows all had zero scallop so breaks SPR - use simple mean for 2024 

#2024/2025 spr 
#mbErec15<-spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==58],apply(livefreq2024[livefreq2024$STRATA_ID==58,21:23],1,sum),
#              livefreq2025$TOW_NO[livefreq2025$STRATA_ID==58],apply(livefreq2025[livefreq2025$STRATA_ID==58,24:26],1,sum),
#              crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
#K<-summary(mbErec15)  #
#MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#Check for all zero tows -- 2024 repeated tows all had zero scallop so breaks SPR - use simple mean for 2025 

#test <- livefreq2024 %>% filter(TOW_NO %in% (crossref.BoF.2025$TOW_NO_REF[crossref.BoF.2025$STRATA_ID==58])) 
#apply(test[test$STRATA_ID==58,21:23],1,sum)

#2025/2026 spr 
mbErec16<-spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==58],apply(livefreq2025[livefreq2025$STRATA_ID==58,21:23],1,sum),
              livefreq2026$TOW_NO[livefreq2026$STRATA_ID==58],apply(livefreq2026[livefreq2026$STRATA_ID==58,24:26],1,sum),
              crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbErec16)  #
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#Check for all zero tows -- 2025 repeated tows all had zero scallop so breaks SPR - use simple mean for 2026 

#test <- livefreq2025 %>% filter(TOW_NO %in% (crossref.BoF.2026$TOW_NO_REF[crossref.BoF.2026$STRATA_ID==58])) 
#apply(test[test$STRATA_ID==58,21:23],1,sum)

MBNE.rec.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNE.rec.spr.est$Year, MBNE.rec.spr.est$Yspr, xout=2020) #  23.63333
MBNE.rec.spr.est[MBNE.rec.spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(23.63333, 13.0751548) #assume var from 2019


#make dataframe for all of MBN East Commercial No/tow

MBNE.rec.spr.est$method <- "spr"
MBNE.rec.spr.est$Prop <- 0.732
names(MBNE.rec.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "Prop")

SPA1B.MBNE.Rec <- rbind(SPA1B.MBNE.Rec.simple[SPA1B.MBNE.Rec.simple$Year<2010,], MBNE.rec.spr.est)
SPA1B.MBNE.Rec[SPA1B.MBNE.Rec$Year == 2024,] <- SPA1B.MBNE.Rec.simple[SPA1B.MBNE.Rec.simple$Year==2024,]
SPA1B.MBNE.Rec[SPA1B.MBNE.Rec$Year == 2025,] <- SPA1B.MBNE.Rec.simple[SPA1B.MBNE.Rec.simple$Year==2025,]
SPA1B.MBNE.Rec

############Commercial (80+ mm)

#simple mean for non-spr years
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNE.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), Prop=rep(0.732, X))
for(i in 1:length(SPA1B.MBNE.Comm.simple$Year)){
	temp.data <- livefreq[livefreq$YEAR==1996+i,]

SPA1B.MBNE.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==58 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
SPA1B.MBNE.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==58 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA1B.MBNE.Comm.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNE.Comm.simple$Year, SPA1B.MBNE.Comm.simple$Mean.nums, xout=2020) #  125.4992
SPA1B.MBNE.Comm.simple[SPA1B.MBNE.Comm.simple$Year==2020,c("Mean.nums","var.y")] <- c(125.4992, 16194.3900 ) #assume var from 2019


#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear #update to most recent year
Y <- length(spryears)
MBNE.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009
# mbEcom<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==58],apply(livefreq2008[livefreq2008$STRATA_ID==58,24:50],1,sum),
#     livefreq2009$TOW_NO[livefreq2009$STRATA_ID==58],apply(livefreq2009[livefreq2009$STRATA_ID==58,27:50],1,sum),
#     crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
# No repeat tows in MBNE in 2009

#2009/2010 spr
mbEcom1<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==58],apply(livefreq2009[livefreq2009$STRATA_ID==58,24:50],1,sum),
    livefreq2010$TOW_NO[livefreq2010$STRATA_ID==58],apply(livefreq2010[livefreq2010$STRATA_ID==58,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom1)  #94.59
MBNE.spr.est[MBNE.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
mbEcom2<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==58],apply(livefreq2010[livefreq2010$STRATA_ID==58,24:50],1,sum),
    livefreq2011$TOW_NO[livefreq2011$STRATA_ID==58],apply(livefreq2011[livefreq2011$STRATA_ID==58,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom2,summary (mbEcom1)) #81.22
MBNE.spr.est[MBNE.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
mbEcom3<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==58],apply(livefreq2011[livefreq2011$STRATA_ID==58,24:50],1,sum),
    livefreq2012$TOW_NO[livefreq2012$STRATA_ID==58],apply(livefreq2012[livefreq2012$STRATA_ID==58,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))  # 62.03
MBNE.spr.est[MBNE.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
mbEcom4<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==58],apply(livefreq2012[livefreq2012$STRATA_ID==58,24:50],1,sum),
    livefreq2013$TOW_NO[livefreq2013$STRATA_ID==58],apply(livefreq2013[livefreq2013$STRATA_ID==58,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1))))#69.46
MBNE.spr.est[MBNE.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
mbEcom5<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==58],apply(livefreq2013[livefreq2013$STRATA_ID==58,24:50],1,sum),
    livefreq2014$TOW_NO[livefreq2014$STRATA_ID==58],apply(livefreq2014[livefreq2014$STRATA_ID==58,27:50],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))  #79.8
MBNE.spr.est[MBNE.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
mbEcom6<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==58],apply(livefreq2014[livefreq2014$STRATA_ID==58,24:50],1,sum),
    livefreq2015$TOW_NO[livefreq2015$STRATA_ID==58],apply(livefreq2015[livefreq2015$STRATA_ID==58,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1))))))  # 82.34264
MBNE.spr.est[MBNE.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
mbEcom7<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==58],apply(livefreq2015[livefreq2015$STRATA_ID==58,24:50],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID==58],apply(livefreq2016[livefreq2016$STRATA_ID==58,27:50],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))))  
MBNE.spr.est[MBNE.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
mbEcom8<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==58],apply(livefreq2016[livefreq2016$STRATA_ID==58,24:50],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID==58],apply(livefreq2017[livefreq2017$STRATA_ID==58,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1))))))))  #112.26172
MBNE.spr.est[MBNE.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
mbEcom9<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==58],apply(livefreq2017[livefreq2017$STRATA_ID==58,24:50],1,sum),
             livefreq2018$TOW_NO[livefreq2018$STRATA_ID==58],apply(livefreq2018[livefreq2018$STRATA_ID==58,27:50],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))))))  #142.91042
MBNE.spr.est[MBNE.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
mbEcom10<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==58],apply(livefreq2018[livefreq2018$STRATA_ID==58,24:50],1,sum),
             livefreq2019$TOW_NO[livefreq2019$STRATA_ID==58],apply(livefreq2019[livefreq2019$STRATA_ID==58,27:50],1,sum),
             crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbEcom10, summary(mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))))))) #92.34925
MBNE.spr.est[MBNE.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 spr No survey in 2020 
mbEcom11 <- spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==58],apply(livefreq2019[livefreq2019$STRATA_ID==58,24:50],1,sum),
              livefreq2021$TOW_NO[livefreq2021$STRATA_ID==58],apply(livefreq2021[livefreq2021$STRATA_ID==58,27:50],1,sum),
              crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom11, summary (mbEcom10, summary(mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))))))))
MBNE.spr.est[MBNE.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

## No survey in 2020 

#2021/2022 spr 
mbEcom12 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==58],apply(livefreq2021[livefreq2021$STRATA_ID==58,24:50],1,sum),
                livefreq2022$TOW_NO[livefreq2022$STRATA_ID==58],apply(livefreq2022[livefreq2022$STRATA_ID==58,27:50],1,sum),
                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom12,summary(mbEcom11, summary (mbEcom10, summary(mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))))))))) 
MBNE.spr.est[MBNE.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2022/2023 spr 
mbEcom13 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==58],apply(livefreq2022[livefreq2022$STRATA_ID==58,24:50],1,sum),
                livefreq2023$TOW_NO[livefreq2023$STRATA_ID==58],apply(livefreq2023[livefreq2023$STRATA_ID==58,27:50],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom13,summary(mbEcom12,summary(mbEcom11, summary (mbEcom10, summary(mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))))))))))
MBNE.spr.est[MBNE.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr 
mbEcom14 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==58],apply(livefreq2023[livefreq2023$STRATA_ID==58,24:50],1,sum),
                livefreq2024$TOW_NO[livefreq2024$STRATA_ID==58],apply(livefreq2024[livefreq2024$STRATA_ID==58,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom14,summary(mbEcom13,summary(mbEcom12,summary(mbEcom11, summary (mbEcom10, summary(mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1))))))))))))))
MBNE.spr.est[MBNE.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 spr 
mbEcom15 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==58],apply(livefreq2024[livefreq2024$STRATA_ID==58,24:50],1,sum),
                livefreq2025$TOW_NO[livefreq2025$STRATA_ID==58],apply(livefreq2025[livefreq2025$STRATA_ID==58,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom15,summary(mbEcom14,summary(mbEcom13,summary(mbEcom12,summary(mbEcom11, summary (mbEcom10, summary(mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1)))))))))))))))
MBNE.spr.est[MBNE.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 spr 
mbEcom16 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==58],apply(livefreq2025[livefreq2025$STRATA_ID==58,24:50],1,sum),
                livefreq2026$TOW_NO[livefreq2026$STRATA_ID==58],apply(livefreq2026[livefreq2026$STRATA_ID==58,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom16,summary(mbEcom15,summary(mbEcom14,summary(mbEcom13,summary(mbEcom12,summary(mbEcom11, summary (mbEcom10, summary(mbEcom9, summary (mbEcom8, summary(mbEcom7, summary (mbEcom6, summary (mbEcom5, summary (mbEcom4,summary (mbEcom3, summary (mbEcom2,summary (mbEcom1))))))))))))))))
MBNE.spr.est[MBNE.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNE.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNE.spr.est$Year, MBNE.spr.est$Yspr, xout=2020) #  91.79212
MBNE.spr.est[MBNE.spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(91.79212, 379.56454) #assume var from 2019


#make dataframe for all of MBN East Commercial No/tow

MBNE.spr.est$method <- "spr"
MBNE.spr.est$Prop <- 0.732
names(MBNE.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "Prop")

SPA1B.MBNE.Comm<- rbind(SPA1B.MBNE.Comm.simple  [SPA1B.MBNE.Comm.simple$Year<2010,], MBNE.spr.est)
SPA1B.MBNE.Comm


#2b. Middle Bay North - WEST

############Recruit (65-79)

years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), Prop=rep(0.268, X))
for(i in 1:length(SPA1B.MBNW.Rec.simple$Year)){
	temp.data <- livefreq[livefreq$YEAR==1996+i,]

SPA1B.MBNW.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==38 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1B.MBNW.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==38 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA1B.MBNW.Rec.simple

#fill in missing years, assume 1997-2002 same as MBNE, interpolate 2004
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==1997,c(2,3)]<-c(17.9333333, 401.773333)
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==1998,c(2,3)]<-c(1.5333333, 6.426667)
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==1999,c(2,3)]<-c(3.7250000,   26.902500)
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==2000,c(2,3)]<-c(6.0666667,  117.342424)
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==2001,c(2,3)]<-c(4.6153846,    32.494744)
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==2002,c(2,3)]<-c(0.3454545,  1.312727) #only 1 tow in West

#interpolate 2004
approx(SPA1B.MBNW.Rec.simple$Year, SPA1B.MBNW.Rec.simple$Mean.nums, xout=2004) #6.807941; No tows in W
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==2004,c(2,3)]<-c(6.807941, 138.745588) #var from previous year

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNW.Rec.simple$Year, SPA1B.MBNW.Rec.simple$Mean.nums, xout=2020) #  23.09236
SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year==2020,c("Mean.nums","var.y")] <- c(23.09236, 18.931944 ) #assume var from 2019


#spr for recruits
#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
MBNW.rec.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009 spr
mbWrec<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==38],apply(livefreq2008[livefreq2008$STRATA_ID==38,21:23],1,sum),
    livefreq2009$TOW_NO[livefreq2009$STRATA_ID==38],apply(livefreq2009[livefreq2009$STRATA_ID==38,24:26],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbWrec)  #
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
mbWrec1<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==38],apply(livefreq2009[livefreq2009$STRATA_ID==38,21:23],1,sum),
    livefreq2010$TOW_NO[livefreq2010$STRATA_ID==38],apply(livefreq2010[livefreq2010$STRATA_ID==38,24:26],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbWrec1)  #
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
#mbWrec2<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==38],apply(livefreq2010[livefreq2010$STRATA_ID==38,21:23],1,sum),livefreq2011$TOW_NO[livefreq2011$STRATA_ID==38],apply(livefreq2011[livefreq2011$STRATA_ID==38,24:26],1,sum),crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#summary(mbWrec2, summary(mbWrec1))

#Only two tows from 2010 were repeated in 2011, use simple mean
#MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2011,c(2:3)] <- c( 4.6000000,  69.12000)

#2011/2012 spr
mbWrec3<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==38],apply(livefreq2011[livefreq2011$STRATA_ID==38,21:23],1,sum),
    livefreq2012$TOW_NO[livefreq2012$STRATA_ID==38],apply(livefreq2012[livefreq2012$STRATA_ID==38,24:26],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbWrec3)  #
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
#mbWrec4<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==38],apply(livefreq2012[livefreq2012$STRATA_ID==38,21:23],1,sum),livefreq2013$TOW_NO[livefreq2013$STRATA_ID==38],apply(livefreq2013[livefreq2013$STRATA_ID==38,24:26],1,sum),crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

#Only two tows from 2010 were repeated in 2013, use simple mean
#MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2013,c(2:3)] <- c(42.0222222, 7338.90444)

#2013/2014 spr
# mbWrec5<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==38],apply(livefreq2013[livefreq2013$STRATA_ID==38,21:23],1,sum),
#     livefreq2014$TOW_NO[livefreq2014$STRATA_ID==38],apply(livefreq2014[livefreq2014$STRATA_ID==38,24:26],1,sum),
#     crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#Only one tow from 2013 was repeated, use simple mean
#MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2014,c(2:3)] <- c(14.7909091,  403.910909)

#2014/2015 spr
mbWrec6<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==38],apply(livefreq2014[livefreq2014$STRATA_ID==38,21:23],1,sum),
    livefreq2015$TOW_NO[livefreq2015$STRATA_ID==38],apply(livefreq2015[livefreq2015$STRATA_ID==38,24:26],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

K<-summary (mbWrec6)  #34.22104
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
mbWrec7<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==38],apply(livefreq2015[livefreq2015$STRATA_ID==38,21:23],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID==38],apply(livefreq2016[livefreq2016$STRATA_ID==38,24:26],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

K<-summary (mbWrec7)  #12.99271
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
mbWrec8<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==38],apply(livefreq2016[livefreq2016$STRATA_ID==38,21:23],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID==38],apply(livefreq2017[livefreq2017$STRATA_ID==38,24:26],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

K<-summary (mbWrec8)  #4.06043
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
# mbWrec9<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==38],apply(livefreq2017[livefreq2017$STRATA_ID==38,21:23],1,sum),
#              livefreq2018$TOW_NO[livefreq2018$STRATA_ID==38],apply(livefreq2018[livefreq2018$STRATA_ID==38,24:26],1,sum),
#              crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
# K<-summary (mbWrec9)  # NA; In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
#MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2018,c(2:3)] <- c(6.06, 72.75)

#2018/2019 spr
# mbWrec10<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==38],apply(livefreq2018[livefreq2018$STRATA_ID==38,21:23],1,sum),
#               livefreq2019$TOW_NO[livefreq2019$STRATA_ID==38],apply(livefreq2019[livefreq2019$STRATA_ID==38,24:26],1,sum),
#               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#  K<-summary (mbWrec10) #NA In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
#MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2019,c(2:3)] <- c(4.32, 18.93)

#2019/2021 #No survey in 2020 -- try to see if works, if not use simple mean
mbWrec11<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==38],apply(livefreq2019[livefreq2019$STRATA_ID==38,21:23],1,sum),
               livefreq2021$TOW_NO[livefreq2021$STRATA_ID==38],apply(livefreq2021[livefreq2021$STRATA_ID==38,24:26],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
  K <- summary(mbWrec11) #NA In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 # try to see if works, if not use simple mean #only 1 matched can't run SPR 
#mbWrec12<-spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==38],apply(livefreq2021[livefreq2021$STRATA_ID==38,21:23],1,sum),
#              livefreq2022$TOW_NO[livefreq2022$STRATA_ID==38],apply(livefreq2022[livefreq2022$STRATA_ID==38,24:26],1,sum),
#              crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(mbWrec12) #NA In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
#MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023  # try to see if works, if not use simple mean #only 1 matched can't run SPR 
#mbWrec13<-spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==38],apply(livefreq2022[livefreq2022$STRATA_ID==38,21:23],1,sum),
#              livefreq2023$TOW_NO[livefreq2023$STRATA_ID==38],apply(livefreq2023[livefreq2023$STRATA_ID==38,24:26],1,sum),
#              crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(mbWrec13) #NA In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
#MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024  # try to see if works, if not use simple mean #only 1 matched can't run SPR 
mbWrec14<-spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==38],apply(livefreq2023[livefreq2023$STRATA_ID==38,21:23],1,sum),
              livefreq2024$TOW_NO[livefreq2024$STRATA_ID==38],apply(livefreq2024[livefreq2024$STRATA_ID==38,24:26],1,sum),
              crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWrec14) #NA In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025  # try to see if works, if not use simple mean #only 1 matched can't run SPR 
mbWrec15<-spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==38],apply(livefreq2024[livefreq2024$STRATA_ID==38,21:23],1,sum),
              livefreq2025$TOW_NO[livefreq2025$STRATA_ID==38],apply(livefreq2025[livefreq2025$STRATA_ID==38,24:26],1,sum),
              crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWrec15) #NA In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026  # try to see if works, if not use simple mean #only 1 matched can't run SPR 
mbWrec16<-spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==38],apply(livefreq2025[livefreq2025$STRATA_ID==38,21:23],1,sum),
              livefreq2026$TOW_NO[livefreq2026$STRATA_ID==38],apply(livefreq2026[livefreq2026$STRATA_ID==38,24:26],1,sum),
              crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWrec16) #NA In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero; use simple mean
MBNW.rec.spr.est[MBNW.rec.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNW.rec.spr.est

#Note since can't get SPR for 2011, 2013, 2014, 2018, 2019 -- use simple means for these years 

MBNW.rec.spr.est$method <- "spr"
MBNW.rec.spr.est$Prop <- 0.268
names(MBNW.rec.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "Prop")

#Note since can't get SPR for 2011, 2013, 2014, 2018, 2019, 2022, 2023 -- use simple means for these years 
replace.spr <- c(2011, 2013, 2014, 2018, 2019, 2022, 2023)
SPA1B.MBNW.Rec <- rbind(SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year<2009,], SPA1B.MBNW.Rec.simple[SPA1B.MBNW.Rec.simple$Year %in% replace.spr,], MBNW.rec.spr.est[!MBNW.rec.spr.est$Year %in% replace.spr,])
SPA1B.MBNW.Rec <- SPA1B.MBNW.Rec %>% arrange(Year)
SPA1B.MBNW.Rec

#in 2020 had no survey to linear interpolation from SPR estimate  
approx(SPA1B.MBNW.Rec$Year, SPA1B.MBNW.Rec$Mean.nums, xout=2020) #  15.38854
SPA1B.MBNW.Rec[SPA1B.MBNW.Rec$Year==2020,c("Mean.nums", "var.y")] <- c(15.38854, 18.931944 ) #assume var from 2019



############Commercial (80+ mm)

#simple mean for non-spr years
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X),Prop=rep(0.268, X))
for(i in 1:length(SPA1B.MBNW.Comm.simple $Year)){
temp.data <- livefreq[livefreq$YEAR==1996+i,]

SPA1B.MBNW.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==38 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
SPA1B.MBNW.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==38 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA1B.MBNW.Comm.simple

#fill in missing years, assume 1997-2002 same as MBNE, interpolate 2004
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==1997,c(2,3)]<-c(54.66667,556.8233)
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==1998,c(2,3)]<-c(58.70000,1493.9520)
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==1999,c(2,3)]<-c(83.47500, 4387.5825)
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==2000,c(2,3)]<-c(78.97500, 1987.2220)
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==2001,c(2,3)]<-c(98.67692, 5379.8336)
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==2002,c(2,3)]<-c(61.37273, 776.1102 ) #only 1 tow in W

approx(SPA1B.MBNW.Comm.simple$Year, SPA1B.MBNW.Comm.simple$Mean.nums, xout=2004) # 106.3575; No tows in W
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==2004,c(2,3)]<- c(106.3575,24415.300)

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.MBNW.Comm.simple$Year, SPA1B.MBNW.Comm.simple$Mean.nums, xout=2020) #  225.5778
SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year==2020,c("Mean.nums","var.y")] <- c(225.5778, 102360.4228 ) #assume var from 2019




#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
MBNW.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009 spr
mbWcom<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==38],apply(livefreq2008[livefreq2008$STRATA_ID==38,24:50],1,sum),
    livefreq2009$TOW_NO[livefreq2009$STRATA_ID==38],apply(livefreq2009[livefreq2009$STRATA_ID==38,27:50],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbWrec)  #  195.64445
MBNW.spr.est[MBNW.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
mbWcom1<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==38],apply(livefreq2009[livefreq2009$STRATA_ID==38,24:50],1,sum),
    livefreq2010$TOW_NO[livefreq2010$STRATA_ID==38],apply(livefreq2010[livefreq2010$STRATA_ID==38,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbWrec1,summary (mbWrec))  # 193.13248
MBNW.spr.est[MBNW.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
#mbWcom2<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==38],apply(livefreq2010[livefreq2010$STRATA_ID==38,27:50],1,sum),livefreq2011$TOW_NO[livefreq2011$STRATA_ID==38],apply(livefreq2011[livefreq2011$STRATA_ID==38,27:50],1,sum),crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#summary(mbWcom2, summary(mbWcom1))

#Only two tows from 2010 were repeated in 2011, use simple mean
#MBNW.spr.est[MBNW.spr.est$Year==2011,c(2:3)] <- c(174.71818, 52298.470)

#2011/2012 spr
mbWcom3<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==38],apply(livefreq2011[livefreq2011$STRATA_ID==38,24:50],1,sum),
    livefreq2012$TOW_NO[livefreq2012$STRATA_ID==38],apply(livefreq2012[livefreq2012$STRATA_ID==38,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (mbWcom3)  # 116.2
MBNW.spr.est[MBNW.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
#mbWcom4<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==38],apply(livefreq2012[livefreq2012$STRATA_ID==38,27:50],1,sum),livefreq2013$TOW_NO[livefreq2013$STRATA_ID==38],apply(livefreq2013[livefreq2013$STRATA_ID==38,27:50],1,sum),crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

#Only two tows from 2010 were repeated in 2013, use simple mean
#MBNW.spr.est[MBNW.spr.est$Year==2013,c(2:3)] <- c(130.62222, 11891.564)

#2013/2014 spr
#mbWcom5<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==38],apply(livefreq2013[livefreq2013$STRATA_ID==38,27:50],1,sum),
#   livefreq2014$TOW_NO[livefreq2014$STRATA_ID==38],apply(livefreq2014[livefreq2014$STRATA_ID==38,27:50],1,sum),
#    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#Only one tow from 2013 was repeated, use simple mean
#MBNW.spr.est[MBNW.spr.est$Year==2014,c(2:3)] <- c(154.63636, 20287.151)

#2014/2015 spr
mbWcom6<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==38],apply(livefreq2014[livefreq2014$STRATA_ID==38,27:50],1,sum),
    livefreq2015$TOW_NO[livefreq2015$STRATA_ID==38],apply(livefreq2015[livefreq2015$STRATA_ID==38,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

K<-summary (mbWcom6)  #93.42313
MBNW.spr.est[MBNW.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
mbWcom7<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==38],apply(livefreq2015[livefreq2015$STRATA_ID==38,27:50],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID==38],apply(livefreq2016[livefreq2016$STRATA_ID==38,27:50],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

K<-summary (mbWcom7, summary (mbWcom6))  #97.50733
MBNW.spr.est[MBNW.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
mbWcom8<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==38],apply(livefreq2016[livefreq2016$STRATA_ID==38,27:50],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID==38],apply(livefreq2017[livefreq2017$STRATA_ID==38,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

K<-summary (mbWcom8, summary(mbWcom7, summary (mbWcom6)))  #225.10234
MBNW.spr.est[MBNW.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
# mbWcom9<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==38],apply(livefreq2017[livefreq2017$STRATA_ID==38,27:50],1,sum),
#              livefreq2018$TOW_NO[livefreq2018$STRATA_ID==38],apply(livefreq2018[livefreq2018$STRATA_ID==38,27:50],1,sum),
#              crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#
# K<-summary (mbWcom9, summary (mbWcom8, summary(mbWcom7, summary (mbWcom6))))  #NAN; use simple mean

#MBNW.spr.est[MBNW.spr.est$Year==2018,c(2:3)] <- c(289.9300, 110464.289)

#2018/2019 spr
# mbWcom10<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==38],apply(livefreq2018[livefreq2018$STRATA_ID==38,27:50],1,sum),
#               livefreq2019$TOW_NO[livefreq2019$STRATA_ID==38],apply(livefreq2019[livefreq2019$STRATA_ID==38,27:50],1,sum),
#               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#
#  K<-summary (mbWcom10)  #NAN; use simple mean
#MBNW.spr.est[MBNW.spr.est$Year==2019,c(2:3)] <- c(243.1556, 102360.423)

#2019/2021 spr No survey in 2020 -- see if works, if not use simple mean 
mbWcom11 <- spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==38],apply(livefreq2019[livefreq2019$STRATA_ID==38,27:50],1,sum),
               livefreq2021$TOW_NO[livefreq2021$STRATA_ID==38],apply(livefreq2021[livefreq2021$STRATA_ID==38,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])

  K<-summary(mbWcom11)  #NAN; use simple mean
MBNW.spr.est[MBNW.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr see if works, if not use simple mean - only 1 matched tow 
#mbWcom12 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==38],apply(livefreq2021[livefreq2021$STRATA_ID==38,27:50],1,sum),
#                livefreq2022$TOW_NO[livefreq2022$STRATA_ID==38],apply(livefreq2022[livefreq2022$STRATA_ID==38,27:50],1,sum),
#                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K<-summary(mbWcom12)  #NAN; use simple mean
#MBNW.spr.est[MBNW.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr -- see if works, if not use simple mean 
#mbWcom13 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==38],apply(livefreq2022[livefreq2022$STRATA_ID==38,27:50],1,sum),
#                livefreq2023$TOW_NO[livefreq2023$STRATA_ID==38],apply(livefreq2023[livefreq2023$STRATA_ID==38,27:50],1,sum),
#                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K<-summary(mbWcom13)  #NAN; use simple mean
#MBNW.spr.est[MBNW.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr -- see if works, if not use simple mean 
mbWcom14 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==38],apply(livefreq2023[livefreq2023$STRATA_ID==38,27:50],1,sum),
                livefreq2024$TOW_NO[livefreq2024$STRATA_ID==38],apply(livefreq2024[livefreq2024$STRATA_ID==38,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom14)  #NAN; use simple mean
MBNW.spr.est[MBNW.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 spr -- see if works, if not use simple mean 
mbWcom15 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==38],apply(livefreq2024[livefreq2024$STRATA_ID==38,27:50],1,sum),
                livefreq2025$TOW_NO[livefreq2025$STRATA_ID==38],apply(livefreq2025[livefreq2025$STRATA_ID==38,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom15)  #NAN; use simple mean
MBNW.spr.est[MBNW.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 spr -- see if works, if not use simple mean 
mbWcom16 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==38],apply(livefreq2025[livefreq2025$STRATA_ID==38,27:50],1,sum),
                livefreq2026$TOW_NO[livefreq2026$STRATA_ID==38],apply(livefreq2026[livefreq2026$STRATA_ID==38,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom16)  #NAN; use simple mean
MBNW.spr.est[MBNW.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNW.spr.est

#Note since can't get SPR for 2011, 2013, 2014, 2018, 2019 -- use simple means for these years 

#make dataframe for all of MBN West Commercial No/tow
MBNW.spr.est$method <- "spr"
MBNW.spr.est$Prop <- 0.268
names(MBNW.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "Prop")

#SPA1B.MBNW.Comm<- rbind(SPA1B.MBNW.Comm.simple  [SPA1B.MBNW.Comm.simple$Year<2009,], MBNW.spr.est)

#Note since can't get SPR for 2011, 2013, 2014, 2018, 2019, 2022 -- use simple means for these years 
replace.spr <- c(2011, 2013, 2014, 2018, 2019, 2022, 2023)
SPA1B.MBNW.Comm <- rbind(SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year<2009,], SPA1B.MBNW.Comm.simple[SPA1B.MBNW.Comm.simple$Year %in% replace.spr,], MBNW.spr.est[!MBNW.spr.est$Year %in% replace.spr,])
SPA1B.MBNW.Comm <- SPA1B.MBNW.Comm %>% arrange(Year)
SPA1B.MBNW.Comm

#in 2020 had no survey to linear interpolation from SPR estimate  
approx(SPA1B.MBNW.Comm$Year, SPA1B.MBNW.Comm$Mean.nums , xout=2020) #  216.1266
SPA1B.MBNW.Comm[SPA1B.MBNW.Comm$Year==2020,c("Mean.nums", "var.y")] <- c(216.1266, 102360.42278  ) #assume var from 2019


#Calculate survey index for MBN (east and west combined proportionally)

#Recruit
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.Rec$prop.mean <- SPA1B.MBNW.Rec$Prop*SPA1B.MBNW.Rec$Mean.nums
SPA1B.MBNE.Rec$prop.mean <- SPA1B.MBNE.Rec$Prop*SPA1B.MBNE.Rec$Mean.nums

#SPA1B.MBN.Rec <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=c(rep("simple",10),rep("spr",13)), NH=rep (294145.4,X), Area=rep("MBN", X), Age=rep("Recruit", X)) #update the value in ("spr", value) each year; eg 11 in 2017, 13 in 2019

SPA1B.MBN.Rec <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep(NA,X), NH=rep(294145.4,X), Area=rep("MBN", X), Age=rep("Recruit", X)) 

SPA1B.MBN.Rec$Mean.nums <- SPA1B.MBNW.Rec$prop.mean + SPA1B.MBNE.Rec$prop.mean
SPA1B.MBN.Rec$sd <- sqrt(SPA1B.MBN.Rec$var.y)
SPA1B.MBN.Rec$Pop <- SPA1B.MBN.Rec$Mean.nums*SPA1B.MBN.Rec$NH
SPA1B.MBN.Rec$var.y <- (SPA1B.MBNW.Rec$Prop*SPA1B.MBNW.Rec$var.y)+(SPA1B.MBNE.Rec$Prop*SPA1B.MBNE.Rec$var.y)
SPA1B.MBN.Rec$method <- SPA1B.MBNW.Rec$method  #Note SPA1B.MBNW.Rec uses SPR since 2009 whereas SPA1B.MBNE.Rec uses SPR since 2010;  thus using SPA1B.MBNW.Rec$method should ensure that if spr is used it's written
  
  
#Commercial
SPA1B.MBNW.Comm.simple$prop.mean <- SPA1B.MBNW.Comm.simple$Prop*SPA1B.MBNW.Comm.simple$Mean.nums
SPA1B.MBNE.Comm.simple$prop.mean <- SPA1B.MBNE.Comm.simple$Prop*SPA1B.MBNE.Comm.simple$Mean.nums


SPA1B.MBN.Comm <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep(NA,X), NH=rep(294145.4,X), Area=rep("MBN", X), Age=rep("Commercial", X))

SPA1B.MBN.Comm$Mean.nums <- SPA1B.MBNW.Comm.simple$prop.mean + SPA1B.MBNE.Comm.simple$prop.mean
SPA1B.MBN.Comm$var.y <- (SPA1B.MBNW.Comm$Prop*SPA1B.MBNW.Comm$var.y)+(SPA1B.MBNE.Comm$Prop*SPA1B.MBNE.Comm$var.y)
SPA1B.MBN.Comm$sd <- sqrt(SPA1B.MBN.Comm$var.y)
SPA1B.MBN.Comm$Pop <- SPA1B.MBN.Comm$Mean.nums*SPA1B.MBN.Comm$NH
SPA1B.MBN.Comm$method <- SPA1B.MBNW.Rec$method  #Note SPA1B.MBNW.Comm uses SPR since 2009 whereas SPA1B.MBNE.Comm uses SPR since 2010; thus using SPA1B.MBNW.Rec$method should ensure that if spr is used it's written

#combine Recruit and Commercial dataframes
SPA1B.MBN.Numbers <- rbind(SPA1B.MBN.Rec,SPA1B.MBN.Comm)

####
# ---- 3. Upper Bay 28C (spr) ----
####

############Recruit (65-79)

years <- 2001:surveyyear
X <- length(years)

SPA1B.28C.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(88000.2,X),Area=rep("28C", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1B.28C.Rec.simple$Year)){
  temp.data <- livefreq[livefreq$YEAR==2000+i,]

SPA1B.28C.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==53 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1B.28C.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==53 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA1B.28C.Rec.simple

#assume 1997-2000 same as MBN
SPA1B.28C.Rec.simple <- rbind(SPA1B.MBN.Rec[SPA1B.MBN.Rec$Year<2001,c(1:7)], SPA1B.28C.Rec.simple)
SPA1B.28C.Rec.simple[c(1:4), "NH"] <- 88000.2
SPA1B.28C.Rec.simple[c(1:4), "Area"] <- "28C"

#interpolate for other years
approx(SPA1B.28C.Rec.simple$Year, SPA1B.28C.Rec.simple$Mean.nums, xout=2004) #10.881
SPA1B.28C.Rec.simple[SPA1B.28C.Rec.simple$Year==2004,"Mean.nums"] <- 10.881
SPA1B.28C.Rec.simple[SPA1B.28C.Rec.simple$Year==2004,"var.y"] <-  405.748057  #assume var from 2003 
#SPA1B.28C.Rec.simple$sd <- sqrt(SPA1B.28C.Rec.simple$var.y)
#SPA1B.28C.Rec.simple$Pop<- SPA1B.28C.Rec.simple$Mean.nums*SPA1B.28C.Rec.simple$NH

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.28C.Rec.simple$Year, SPA1B.28C.Rec.simple$Mean.nums, xout=2020) #  17.97353
SPA1B.28C.Rec.simple[SPA1B.28C.Rec.simple$Year==2020,c("Mean.nums","var.y")] <- c(17.97353, 222.077647  ) #assume var from 2019



#spr for recruits
#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
C28.rec.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009 spr
c28rec1<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==53],apply(livefreq2008[livefreq2008$STRATA_ID==53,21:23],1,sum),
 livefreq2009$TOW_NO[livefreq2009$STRATA_ID==53],apply(livefreq2009[livefreq2009$STRATA_ID==53,24:26],1,sum),
 crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary (c28rec1)
C28.rec.spr.est[C28.rec.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr  (tows in 2010 that were repeats of 2009)
c28rec2<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==53],apply(livefreq2009[livefreq2009$STRATA_ID==53,21:23],1,sum),
 livefreq2010$TOW_NO[livefreq2010$STRATA_ID==53],apply(livefreq2010[livefreq2010$STRATA_ID==53,24:26],1,sum),
 crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec2) #
C28.rec.spr.est[C28.rec.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr  (tows in 2011 that were repeats of 2010)
c28rec3<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==53],apply(livefreq2010[livefreq2010$STRATA_ID==53,21:23],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==53],apply(livefreq2011[livefreq2011$STRATA_ID==53,24:26],1,sum),
crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary (c28rec3)   #
C28.rec.spr.est[C28.rec.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr   (tows in 2012 that were repeats of 2011)
c28rec4<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==53],apply(livefreq2011[livefreq2011$STRATA_ID==53,21:23],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==53],apply(livefreq2012[livefreq2012$STRATA_ID==53,24:26],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec4)  #
C28.rec.spr.est[C28.rec.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
c28rec5<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==53],apply(livefreq2012[livefreq2012$STRATA_ID==53,21:23],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==53],apply(livefreq2013[livefreq2013$STRATA_ID==53,24:26],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec5)
C28.rec.spr.est[C28.rec.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
c28rec6<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==53],apply(livefreq2013[livefreq2013$STRATA_ID==53,21:23],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID==53],apply(livefreq2014[livefreq2014$STRATA_ID==53,24:26],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec6) #
C28.rec.spr.est[C28.rec.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
c28rec7<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==53],apply(livefreq2014[livefreq2014$STRATA_ID==53,21:23],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID==53],apply(livefreq2015[livefreq2015$STRATA_ID==53,24:26],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec7) # 6.936104
C28.rec.spr.est[C28.rec.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
c28rec8<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==53],apply(livefreq2015[livefreq2015$STRATA_ID==53,21:23],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID==53],apply(livefreq2016[livefreq2016$STRATA_ID==53,24:26],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec8) #8.020460
C28.rec.spr.est[C28.rec.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
c28rec9<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==53],apply(livefreq2016[livefreq2016$STRATA_ID==53,21:23],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID==53],apply(livefreq2017[livefreq2017$STRATA_ID==53,24:26],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary (c28rec9) #6.872433
C28.rec.spr.est[C28.rec.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
c28rec10<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==53],apply(livefreq2017[livefreq2017$STRATA_ID==53,21:23],1,sum),
             livefreq2018$TOW_NO[livefreq2018$STRATA_ID==53],apply(livefreq2018[livefreq2018$STRATA_ID==53,24:26],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec10) #12.3069
C28.rec.spr.est[C28.rec.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
c28rec11<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==53],apply(livefreq2018[livefreq2018$STRATA_ID==53,21:23],1,sum),
              livefreq2019$TOW_NO[livefreq2019$STRATA_ID==53],apply(livefreq2019[livefreq2019$STRATA_ID==53,24:26],1,sum),
              crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28rec11) # 9.017201
C28.rec.spr.est[C28.rec.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 No survey in 2020 
c28rec12<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==53],apply(livefreq2019[livefreq2019$STRATA_ID==53,21:23],1,sum),
              livefreq2021$TOW_NO[livefreq2021$STRATA_ID==53],apply(livefreq2021[livefreq2021$STRATA_ID==53,24:26],1,sum),
              crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary(c28rec12) # 
C28.rec.spr.est[C28.rec.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
c28rec13 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==53],apply(livefreq2021[livefreq2021$STRATA_ID==53,21:23],1,sum),
              livefreq2022$TOW_NO[livefreq2022$STRATA_ID==53],apply(livefreq2022[livefreq2022$STRATA_ID==53,24:26],1,sum),
              crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
C28.rec.spr.est[C28.rec.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
c28rec14 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==53],apply(livefreq2022[livefreq2022$STRATA_ID==53,21:23],1,sum),
                livefreq2023$TOW_NO[livefreq2023$STRATA_ID==53],apply(livefreq2023[livefreq2023$STRATA_ID==53,24:26],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K <- summary(c28rec14) # 
C28.rec.spr.est[C28.rec.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2023/2024 
c28rec15 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==53],apply(livefreq2023[livefreq2023$STRATA_ID==53,21:23],1,sum),
                livefreq2024$TOW_NO[livefreq2024$STRATA_ID==53],apply(livefreq2024[livefreq2024$STRATA_ID==53,24:26],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28rec15) # 
C28.rec.spr.est[C28.rec.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
c28rec16 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==53],apply(livefreq2024[livefreq2024$STRATA_ID==53,21:23],1,sum),
                livefreq2025$TOW_NO[livefreq2025$STRATA_ID==53],apply(livefreq2025[livefreq2025$STRATA_ID==53,24:26],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28rec16) # 
C28.rec.spr.est[C28.rec.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
c28rec17 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==53],apply(livefreq2025[livefreq2025$STRATA_ID==53,21:23],1,sum),
                livefreq2026$TOW_NO[livefreq2026$STRATA_ID==53],apply(livefreq2026[livefreq2026$STRATA_ID==53,24:26],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28rec17) # 
C28.rec.spr.est[C28.rec.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

C28.rec.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(C28.rec.spr.est$Year, C28.rec.spr.est$Yspr, xout=2020) #  29.08871
C28.rec.spr.est[C28.rec.spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(29.08871,  11.600382) #assume var from 2019


#make dataframe for all of 28C Upper Bay Commercial Abundance
C28.rec.spr.est$method <- "spr"
C28.rec.spr.est$NH <- 88000.2
C28.rec.spr.est$Area <- "28C"
C28.rec.spr.est$Age <- "Recruit"

names(C28.rec.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA1B.28C.Rec <- rbind(SPA1B.28C.Rec.simple[SPA1B.28C.Rec.simple$Year<2009,], C28.rec.spr.est)
SPA1B.28C.Rec$sd <- sqrt(SPA1B.28C.Rec$var.y)
SPA1B.28C.Rec$Pop <- SPA1B.28C.Rec$Mean.nums*SPA1B.28C.Rec$NH


############Commercial (65-79)

years <- 2001:surveyyear
X <- length(years)

SPA1B.28C.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(88000.2,X),Area=rep("28C", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1B.28C.Comm.simple$Year)){
	temp.data <- livefreq[livefreq$YEAR==2000+i,]

SPA1B.28C.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==53 & temp.data$TOW_TYPE_ID==1,27:50],1,sum))
SPA1B.28C.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==53 & temp.data$TOW_TYPE_ID==1,27:50],1,sum))
}
SPA1B.28C.Comm.simple

#assume 1997-2000 same as MBN
SPA1B.28C.Comm.simple <- rbind(SPA1B.MBN.Comm[SPA1B.MBN.Comm$Year<2001,c(1:7)], SPA1B.28C.Comm.simple)
SPA1B.28C.Comm.simple[c(1:4), "NH"] <- 88000.2
SPA1B.28C.Comm.simple[c(1:4), "Area"]<-"28C"

#interpolate missing years
approx(SPA1B.28C.Comm.simple$Year, SPA1B.28C.Comm.simple$Mean.nums, xout=2004) #125
SPA1B.28C.Comm.simple[SPA1B.28C.Comm.simple$Year==2004,c("Mean.nums","var.y")]<- c(125,42731.69)

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.28C.Comm.simple$Year, SPA1B.28C.Comm.simple$Mean.nums, xout=2020) #  86.44706
SPA1B.28C.Comm.simple[SPA1B.28C.Comm.simple$Year==2020,c("Mean.nums","var.y")] <- c(86.44706, 38420.49) #assume var from 2019



#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
C28.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009 spr
c28comm1<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==53],apply(livefreq2008[livefreq2008$STRATA_ID==53,24:50],1,sum),
 livefreq2009$TOW_NO[livefreq2009$STRATA_ID==53],apply(livefreq2009[livefreq2009$STRATA_ID==53,27:50],1,sum),
 crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary (c28comm1) #116.40219
C28.spr.est[C28.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr  (tows in 2010 that were repeats of 2009)
c28comm2<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==53],apply(livefreq2009[livefreq2009$STRATA_ID==53,24:50],1,sum),
 livefreq2010$TOW_NO[livefreq2010$STRATA_ID==53],apply(livefreq2010[livefreq2010$STRATA_ID==53,27:50],1,sum),
 crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm2,summary (c28comm1)) # 120.44630
C28.spr.est[C28.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr  (tows in 2011 that were repeats of 2010)
c28comm3<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==53],apply(livefreq2010[livefreq2010$STRATA_ID==53,24:50],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==53],apply(livefreq2011[livefreq2011$STRATA_ID==53,27:50],1,sum),
crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary (c28comm3,  summary (c28comm2, summary (c28comm1) ))   #130.66832
C28.spr.est[C28.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr   (tows in 2012 that were repeats of 2011)
c28comm4<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==53],apply(livefreq2011[livefreq2011$STRATA_ID==53,24:50],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==53],apply(livefreq2012[livefreq2012$STRATA_ID==53,27:50],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))  # 91.85300
C28.spr.est[C28.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
c28comm5<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==53],apply(livefreq2012[livefreq2012$STRATA_ID==53,24:50],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==53],apply(livefreq2013[livefreq2013$STRATA_ID==53,27:50],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))) #99.74844
C28.spr.est[C28.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
c28comm6<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==53],apply(livefreq2013[livefreq2013$STRATA_ID==53,24:50],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID==53],apply(livefreq2014[livefreq2014$STRATA_ID==53,27:50],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) ))))) #95.28026
C28.spr.est[C28.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
c28comm7<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==53],apply(livefreq2014[livefreq2014$STRATA_ID==53,24:50],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID==53],apply(livefreq2015[livefreq2015$STRATA_ID==53,27:50],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))))) #96.00754
C28.spr.est[C28.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
c28comm8<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==53],apply(livefreq2015[livefreq2015$STRATA_ID==53,24:50],1,sum),
              livefreq2016$TOW_NO[livefreq2016$STRATA_ID==53],apply(livefreq2016[livefreq2016$STRATA_ID==53,27:50],1,sum),
              crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) ))))))) #61.78287
C28.spr.est[C28.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
c28comm9<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==53],apply(livefreq2016[livefreq2016$STRATA_ID==53,24:50],1,sum),
              livefreq2017$TOW_NO[livefreq2017$STRATA_ID==53],apply(livefreq2016[livefreq2017$STRATA_ID==53,27:50],1,sum),
              crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))))))) #122.95140
C28.spr.est[C28.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
c28comm10<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==53],apply(livefreq2017[livefreq2017$STRATA_ID==53,24:50],1,sum),
              livefreq2018$TOW_NO[livefreq2018$STRATA_ID==53],apply(livefreq2018[livefreq2018$STRATA_ID==53,27:50],1,sum),
              crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) ))))))))) #112.76842
C28.spr.est[C28.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
c28comm11<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==53],apply(livefreq2018[livefreq2018$STRATA_ID==53,24:50],1,sum),
               livefreq2019$TOW_NO[livefreq2019$STRATA_ID==53],apply(livefreq2019[livefreq2019$STRATA_ID==53,27:50],1,sum),
               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28comm11, summary(c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))))))))) #83.77113
C28.spr.est[C28.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 No survey in 2020 
c28comm12<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==53],apply(livefreq2019[livefreq2019$STRATA_ID==53,24:50],1,sum),
               livefreq2021$TOW_NO[livefreq2021$STRATA_ID==53],apply(livefreq2021[livefreq2021$STRATA_ID==53,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K <- summary(c28comm12, summary (c28comm11, summary(c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) ))))))))))) #
C28.spr.est[C28.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2021/2022  
c28comm13 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==53],apply(livefreq2021[livefreq2021$STRATA_ID==53,24:50],1,sum),
               livefreq2022$TOW_NO[livefreq2022$STRATA_ID==53],apply(livefreq2022[livefreq2022$STRATA_ID==53,27:50],1,sum),
               crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K <- summary(c28comm13, summary(c28comm12, summary (c28comm11, summary(c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))))))))))) #
C28.spr.est[C28.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023  
c28comm14 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==53],apply(livefreq2022[livefreq2022$STRATA_ID==53,24:50],1,sum),
                 livefreq2023$TOW_NO[livefreq2023$STRATA_ID==53],apply(livefreq2023[livefreq2023$STRATA_ID==53,27:50],1,sum),
                 crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K <- summary(c28comm14, summary(c28comm13, summary(c28comm12, summary (c28comm11, summary(c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) ))))))))))))) #
C28.spr.est[C28.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2023/2024  
c28comm15 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==53],apply(livefreq2023[livefreq2023$STRATA_ID==53,24:50],1,sum),
                 livefreq2024$TOW_NO[livefreq2024$STRATA_ID==53],apply(livefreq2024[livefreq2024$STRATA_ID==53,27:50],1,sum),
                 crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K <- summary(c28comm15, summary(c28comm14, summary(c28comm13, summary(c28comm12, summary (c28comm11, summary(c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))))))))))))) #
C28.spr.est[C28.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025  
c28comm16 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==53],apply(livefreq2024[livefreq2024$STRATA_ID==53,24:50],1,sum),
                 livefreq2025$TOW_NO[livefreq2025$STRATA_ID==53],apply(livefreq2025[livefreq2025$STRATA_ID==53,27:50],1,sum),
                 crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K <- summary(c28comm16, summary(c28comm15, summary(c28comm14, summary(c28comm13, summary(c28comm12, summary (c28comm11, summary(c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) ))))))))))))))) #
C28.spr.est[C28.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026  
c28comm17 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==53],apply(livefreq2025[livefreq2025$STRATA_ID==53,24:50],1,sum),
                 livefreq2026$TOW_NO[livefreq2026$STRATA_ID==53],apply(livefreq2026[livefreq2026$STRATA_ID==53,27:50],1,sum),
                 crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K <- summary(c28comm17, summary(c28comm16, summary(c28comm15, summary(c28comm14, summary(c28comm13, summary(c28comm12, summary (c28comm11, summary(c28comm10, summary (c28comm9, summary (c28comm8, summary (c28comm7, summary (c28comm6, summary (c28comm5, summary (c28comm4,summary (c28comm3,  summary (c28comm2,summary (c28comm1) )))))))))))))))) #
C28.spr.est[C28.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

C28.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(C28.spr.est$Year, C28.spr.est$Yspr, xout=2020) #  76.32857
C28.spr.est[C28.spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(76.32857,   1003.7117) #assume var from 2019


#make dataframe for all of 28C Upper Bay Commercial Abundance
C28.spr.est$method <- "spr"
C28.spr.est$NH <- 88000.2
C28.spr.est$Area <- "28C"
C28.spr.est$Age <- "Commercial"

names(C28.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA1B.28C.Comm <- rbind(SPA1B.28C.Comm.simple[SPA1B.28C.Comm.simple$Year<2009,], C28.spr.est)
SPA1B.28C.Comm$sd <- sqrt(SPA1B.28C.Comm$var.y)
SPA1B.28C.Comm$Pop<- SPA1B.28C.Comm$Mean.nums*SPA1B.28C.Comm$NH

#combine Recruit and Commercial dataframes
SPA1B.28C.Numbers <- rbind(SPA1B.28C.Rec, SPA1B.28C.Comm)

####
# ---- 4. 28D Outer Bay ----
####

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 2001:surveyyear
X <- length(years)

SPA1B.Out.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(56009.89, X),Area=rep("Out", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1B.Out.Rec.simple$Year)){
  temp.data <- livefreq[livefreq$YEAR==2000+i,]
SPA1B.Out.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==49 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1B.Out.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==49 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA1B.Out.Rec.simple

#assume 1997-2000 same as MBN
SPA1B.Out.Rec.simple <- rbind(SPA1B.MBN.Rec[SPA1B.MBN.Rec$Year<2001,c(1:7)], SPA1B.Out.Rec.simple)
SPA1B.Out.Rec.simple[c(1:4), "NH"]<-56009.89
SPA1B.Out.Rec.simple[c(1:4), "Area"]<-"Out"

#interpolate missing year
approx(SPA1B.Out.Rec.simple $Year, SPA1B.Out.Rec.simple $Mean.nums, xout=2004)# 0.58333
SPA1B.Out.Rec.simple[SPA1B.Out.Rec.simple$Year==2004, c("Mean.nums", "var.y")]<- c(0.58333,4.083333) #assume same var as 2003 but since 0 in 2003 take var from 2005

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.Out.Rec.simple$Year, SPA1B.Out.Rec.simple$Mean.nums, xout=2020) #   9.94375
SPA1B.Out.Rec.simple[SPA1B.Out.Rec.simple$Year==2020,c("Mean.nums","var.y")] <- c( 9.94375, 2.420000) #assume var from 2019


SPA1B.Out.Rec.simple$sd <- sqrt(SPA1B.Out.Rec.simple$var.y)
SPA1B.Out.Rec.simple$Pop <- SPA1B.Out.Rec.simple$Mean.nums*SPA1B.Out.Rec.simple$NH


#SPR for recruits !NB for Outer 28D use simple mean for recruits! ---See past years codes for why - SPR doesn't work majority of years 


############ Commercial (80+ mm)
years <- 2001:surveyyear
X <- length(years)

SPA1B.Out.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(56009.89
, X),Area=rep("Out", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1B.Out.Comm.simple$Year)){
	temp.data <- livefreq[livefreq$YEAR==2000+i,]
SPA1B.Out.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==49 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
SPA1B.Out.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==49 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA1B.Out.Comm.simple

#assume 1997-2000 same as MBN
SPA1B.Out.Comm.simple <- rbind(SPA1B.MBN.Comm[SPA1B.MBN.Comm$Year<2001,c(1:7)], SPA1B.Out.Comm.simple)
SPA1B.Out.Comm.simple[c(1:4), "NH"]<-56009.89
SPA1B.Out.Comm.simple[c(1:4), "Area"]<-"Out"

#interpolate missing year
approx(SPA1B.Out.Comm.simple$Year, SPA1B.Out.Comm.simple$Mean.nums, xout=2004)#89.442
SPA1B.Out.Comm.simple[SPA1B.Out.Comm.simple$Year==2004,c("Mean.nums", "var.y")]<- c(89.442, 23566.2050) #take var from 2003 

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.Out.Comm.simple$Year, SPA1B.Out.Comm.simple$Mean.nums, xout=2020) #  25.81875
SPA1B.Out.Comm.simple[SPA1B.Out.Comm.simple$Year==2020,c("Mean.nums","var.y")] <- c(25.81875, 496.6514) #assume var from 2019



#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear #update to most recent year
Y <- length(spryears)
Out.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009 spr
# OUTcomm1<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==49],apply(livefreq2008[livefreq2008$STRATA_ID==49,24:50],1,sum),
#     livefreq2009$TOW_NO[livefreq2009$STRATA_ID==49],apply(livefreq2009[livefreq2009$STRATA_ID==49,27:50],1,sum),
#     crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
#Only 2 repeats

#2009/2010 spr
OUTcomm2<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==49],apply(livefreq2009[livefreq2009$STRATA_ID==49,24:50],1,sum),
    livefreq2010$TOW_NO[livefreq2010$STRATA_ID==49],apply(livefreq2010[livefreq2010$STRATA_ID==49,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm2)       #  3.849
Out.spr.est[Out.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
OUTcomm3<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==49],apply(livefreq2010[livefreq2010$STRATA_ID==49,24:50],1,sum),
    livefreq2011$TOW_NO[livefreq2011$STRATA_ID==49],apply(livefreq2011[livefreq2011$STRATA_ID==49,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm3, summary (OUTcomm2))  # 17.7
Out.spr.est[Out.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
OUTcomm4<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==49],apply(livefreq2011[livefreq2011$STRATA_ID==49,24:50],1,sum),
    livefreq2012$TOW_NO[livefreq2012$STRATA_ID==49],apply(livefreq2012[livefreq2012$STRATA_ID==49,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm4, summary (OUTcomm3, summary (OUTcomm2))) # 16.16
Out.spr.est[Out.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
OUTcomm5<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==49],apply(livefreq2012[livefreq2012$STRATA_ID==49,24:50],1,sum),
    livefreq2013$TOW_NO[livefreq2013$STRATA_ID==49],apply(livefreq2013[livefreq2013$STRATA_ID==49,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm5,summary (OUTcomm4, summary (OUTcomm3, summary (OUTcomm2))))#9.21
Out.spr.est[Out.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
# OUTcomm6<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==49],apply(livefreq2013[livefreq2013$STRATA_ID==49,24:50],1,sum),
#     livefreq2014$TOW_NO[livefreq2014$STRATA_ID==49],apply(livefreq2014[livefreq2014$STRATA_ID==49,27:50],1,sum),
#     crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
# K<-summary (OUTcomm6, summary (OUTcomm5,summary (OUTcomm4, summary (OUTcomm3, summary (OUTcomm2)))))
#Out.spr.est[Out.spr.est$Year==2014,c(2:3)] <- c(27.92500,2555.2107) # use simple mean, only two repeat tows

#2014/2015 spr
OUTcomm7<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==49],apply(livefreq2014[livefreq2014$STRATA_ID==49,24:50],1,sum),
    livefreq2015$TOW_NO[livefreq2015$STRATA_ID==49],apply(livefreq2015[livefreq2015$STRATA_ID==49,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm7) #22.452669
Out.spr.est[Out.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
OUTcomm8<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==49],apply(livefreq2015[livefreq2015$STRATA_ID==49,24:50],1,sum),
              livefreq2016$TOW_NO[livefreq2016$STRATA_ID==49],apply(livefreq2016[livefreq2016$STRATA_ID==49,27:50],1,sum),
              crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm8,summary (OUTcomm7)) # 17.504781
Out.spr.est[Out.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
OUTcomm9<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==49],apply(livefreq2016[livefreq2016$STRATA_ID==49,24:50],1,sum),
              livefreq2017$TOW_NO[livefreq2017$STRATA_ID==49],apply(livefreq2017[livefreq2017$STRATA_ID==49,27:50],1,sum),
              crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7))) #25.440549
Out.spr.est[Out.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
OUTcomm10<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==49],apply(livefreq2017[livefreq2017$STRATA_ID==49,24:50],1,sum),
              livefreq2018$TOW_NO[livefreq2018$STRATA_ID==49],apply(livefreq2018[livefreq2018$STRATA_ID==49,27:50],1,sum),
              crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7)))) # 36.04209
Out.spr.est[Out.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
OUTcomm11<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==49],apply(livefreq2018[livefreq2018$STRATA_ID==49,24:50],1,sum),
               livefreq2019$TOW_NO[livefreq2019$STRATA_ID==49],apply(livefreq2019[livefreq2019$STRATA_ID==49,27:50],1,sum),
               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm11, summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7)))))
Out.spr.est[Out.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #13.307152

#2019/2021 spr
OUTcomm12<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==49],apply(livefreq2019[livefreq2019$STRATA_ID==49,24:50],1,sum),
               livefreq2021$TOW_NO[livefreq2021$STRATA_ID==49],apply(livefreq2021[livefreq2021$STRATA_ID==49,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcomm12, summary (OUTcomm11, summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7))))))
Out.spr.est[Out.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #13.307152

#2021/2022 spr
OUTcomm13 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==49],apply(livefreq2021[livefreq2021$STRATA_ID==49,24:50],1,sum),
               livefreq2022$TOW_NO[livefreq2022$STRATA_ID==49],apply(livefreq2022[livefreq2022$STRATA_ID==49,27:50],1,sum),
               crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcomm13,summary(OUTcomm12, summary (OUTcomm11, summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7)))))))
Out.spr.est[Out.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #13.307152

#2022/2023 spr
OUTcomm14 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==49],apply(livefreq2022[livefreq2022$STRATA_ID==49,24:50],1,sum),
                 livefreq2023$TOW_NO[livefreq2023$STRATA_ID==49],apply(livefreq2023[livefreq2023$STRATA_ID==49,27:50],1,sum),
                 crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcomm14, summary(OUTcomm13,summary(OUTcomm12, summary (OUTcomm11, summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7))))))))
Out.spr.est[Out.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #13.307152


#2023/2024 spr
OUTcomm15 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==49],apply(livefreq2023[livefreq2023$STRATA_ID==49,24:50],1,sum),
                 livefreq2024$TOW_NO[livefreq2024$STRATA_ID==49],apply(livefreq2024[livefreq2024$STRATA_ID==49,27:50],1,sum),
                 crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcomm15,summary(OUTcomm14, summary(OUTcomm13,summary(OUTcomm12, summary (OUTcomm11, summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7)))))))))
Out.spr.est[Out.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #13.307152

#2024/2025 spr
OUTcomm16 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==49],apply(livefreq2024[livefreq2024$STRATA_ID==49,24:50],1,sum),
                 livefreq2025$TOW_NO[livefreq2025$STRATA_ID==49],apply(livefreq2025[livefreq2025$STRATA_ID==49,27:50],1,sum),
                 crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcomm16, summary(OUTcomm15,summary(OUTcomm14, summary(OUTcomm13,summary(OUTcomm12, summary (OUTcomm11, summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7))))))))))
Out.spr.est[Out.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #54.967350

#2025/2026 spr
OUTcomm17 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==49],apply(livefreq2025[livefreq2025$STRATA_ID==49,24:50],1,sum),
                 livefreq2026$TOW_NO[livefreq2026$STRATA_ID==49],apply(livefreq2026[livefreq2026$STRATA_ID==49,27:50],1,sum),
                 crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcomm17, summary(OUTcomm16, summary(OUTcomm15,summary(OUTcomm14, summary(OUTcomm13,summary(OUTcomm12, summary (OUTcomm11, summary (OUTcomm10,summary (OUTcomm9, summary (OUTcomm8,summary (OUTcomm7)))))))))))
Out.spr.est[Out.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) 

Out.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Out.spr.est$Year, Out.spr.est$Yspr, xout=2020) #  25.02988
Out.spr.est[Out.spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(25.02988,  64.52940) #assume var from 2019


#make dataframe for all of Outer 28D Outer Commercial Abundance
Out.spr.est$method <- "spr"
Out.spr.est$NH <- 56009.89
Out.spr.est$Area <-"Out"
Out.spr.est$Age <-"Commercial"
names(Out.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA1B.Out.Comm <- rbind(SPA1B.Out.Comm.simple[SPA1B.Out.Comm.simple$Year<2010,], Out.spr.est)
#use simple mean for 2014
SPA1B.Out.Comm$Mean.nums[SPA1B.Out.Comm$Year==2014] <- SPA1B.Out.Comm.simple$Mean.nums[SPA1B.Out.Comm.simple$Year==2014]
SPA1B.Out.Comm$var.y[SPA1B.Out.Comm$Year==2014] <- SPA1B.Out.Comm.simple$var.y[SPA1B.Out.Comm.simple$Year==2014]
SPA1B.Out.Comm$method[SPA1B.Out.Comm$Year==2014] <- "simple"
SPA1B.Out.Comm$sd <- sqrt(SPA1B.Out.Comm$var.y)
SPA1B.Out.Comm$Pop<- SPA1B.Out.Comm$Mean.nums*SPA1B.Out.Comm$NH

#combine Recruit and Commercial dataframes
SPA1B.Out.Numbers <- rbind(SPA1B.Out.Rec.simple,SPA1B.Out.Comm)

####
# ---- 5. Advocate Harbour ----
####

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 2001:surveyyear
X <- length(years)

SPA1B.AH.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(8626.27, X),Area=rep("AH", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1B.AH.Rec.simple$Year)){
  temp.data <- livefreq[livefreq$YEAR==2000+i,]

SPA1B.AH.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==35 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1B.AH.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==35 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA1B.AH.Rec.simple

#assume 1997-2000 same as MBN
SPA1B.AH.Rec.simple <- rbind(SPA1B.MBN.Rec[SPA1B.MBN.Rec$Year<2001,c(1:7)], SPA1B.AH.Rec.simple)
SPA1B.AH.Rec.simple[c(1:4), "NH"]<-8626.27
SPA1B.AH.Rec.simple[c(1:4), "Area"]<-"AH"

#interpolate missing year 2004 
approx(SPA1B.AH.Rec.simple$Year, SPA1B.AH.Rec.simple $Mean.nums, xout=2004)#20.683
SPA1B.AH.Rec.simple[SPA1B.AH.Rec.simple$Year==2004,c("Mean.nums", "var.y")]<-c(20.683, 204.843333) #assume var from 2003

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA1B.AH.Rec.simple$Year, SPA1B.AH.Rec.simple$Mean.nums, xout=2020) #   89.4
SPA1B.AH.Rec.simple[SPA1B.AH.Rec.simple$Year==2020,c("Mean.nums","var.y")] <- c(89.4, 33843.702500) #assume var from 2019



#spr for recruit
#dataframe for SPR estimates
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
AH.rec.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

## spr mean for commercial size (>80mm)
#2008/2009
AHrec<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==35],apply(livefreq2008[livefreq2008$STRATA_ID==35,21:23],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID==35],apply(livefreq2009[livefreq2009$STRATA_ID==35,24:26],1,sum),
crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrec)    #
AH.rec.spr.est[AH.rec.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
AHrec2<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==35],apply(livefreq2009[livefreq2009$STRATA_ID==35,21:23],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID==35],apply(livefreq2010[livefreq2010$STRATA_ID==35,24:26],1,sum),
crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHrec2)    #
AH.rec.spr.est[AH.rec.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
AHrec3<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==35],apply(livefreq2010[livefreq2010$STRATA_ID==35,21:23],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==35],apply(livefreq2011[livefreq2011$STRATA_ID==35,24:26],1,sum),
crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHrec3)
AH.rec.spr.est[AH.rec.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
AHrec4<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==35],apply(livefreq2011[livefreq2011$STRATA_ID==35,21:23],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==35],apply(livefreq2012[livefreq2012$STRATA_ID==35,24:26],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHrec4)   #
AH.rec.spr.est[AH.rec.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
AHrec5<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==35],apply(livefreq2012[livefreq2012$STRATA_ID==35,21:23],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==35],apply(livefreq2013[livefreq2013$STRATA_ID==35,24:26],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHrec5)     #
AH.rec.spr.est[AH.rec.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
AHrec6<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==35],apply(livefreq2013[livefreq2013$STRATA_ID==35,21:23],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID==35],apply(livefreq2014[livefreq2014$STRATA_ID==35,24:26],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHrec6)
AH.rec.spr.est[AH.rec.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
AHrec7<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==35],apply(livefreq2014[livefreq2014$STRATA_ID==35,21:23],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID==35],apply(livefreq2015[livefreq2015$STRATA_ID==35,24:26],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrec7)     #104.68855
AH.rec.spr.est[AH.rec.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
AHrec8<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==35],apply(livefreq2015[livefreq2015$STRATA_ID==35,21:23],1,sum),
            livefreq2016$TOW_NO[livefreq2016$STRATA_ID==35],apply(livefreq2016[livefreq2016$STRATA_ID==35,24:26],1,sum),
            crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrec8)     #95.26697
AH.rec.spr.est[AH.rec.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
AHrec9<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==35],apply(livefreq2016[livefreq2016$STRATA_ID==35,21:23],1,sum),
            livefreq2017$TOW_NO[livefreq2017$STRATA_ID==35],apply(livefreq2017[livefreq2017$STRATA_ID==35,24:26],1,sum),
            crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrec9)     #50.59308
AH.rec.spr.est[AH.rec.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
AHrec10<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==35],apply(livefreq2017[livefreq2017$STRATA_ID==35,21:23],1,sum),
            livefreq2018$TOW_NO[livefreq2018$STRATA_ID==35],apply(livefreq2018[livefreq2018$STRATA_ID==35,24:26],1,sum),
            crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrec10)     #68.20516
AH.rec.spr.est[AH.rec.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2018/2019
AHrec11<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==35],apply(livefreq2018[livefreq2018$STRATA_ID==35,21:23],1,sum),
             livefreq2019$TOW_NO[livefreq2019$STRATA_ID==35],apply(livefreq2019[livefreq2019$STRATA_ID==35,24:26],1,sum),
             crossref.BoF.2019[crossref.BoF.2019$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrec11)     #73.38924
AH.rec.spr.est[AH.rec.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
AHrec12<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==35],apply(livefreq2019[livefreq2019$STRATA_ID==35,21:23],1,sum),
             livefreq2021$TOW_NO[livefreq2021$STRATA_ID==35],apply(livefreq2021[livefreq2021$STRATA_ID==35,24:26],1,sum),
             crossref.BoF.2021[crossref.BoF.2021$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary(AHrec12)     
AH.rec.spr.est[AH.rec.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
AHrec13 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==35],apply(livefreq2021[livefreq2021$STRATA_ID==35,21:23],1,sum),
             livefreq2022$TOW_NO[livefreq2022$STRATA_ID==35],apply(livefreq2022[livefreq2022$STRATA_ID==35,24:26],1,sum),
             crossref.BoF.2022[crossref.BoF.2022$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrec13)     
AH.rec.spr.est[AH.rec.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
AHrec14 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==35],apply(livefreq2022[livefreq2022$STRATA_ID==35,21:23],1,sum),
               livefreq2023$TOW_NO[livefreq2023$STRATA_ID==35],apply(livefreq2023[livefreq2023$STRATA_ID==35,24:26],1,sum),
               crossref.BoF.2023[crossref.BoF.2023$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrec14)     
AH.rec.spr.est[AH.rec.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2023/2024
AHrec15 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==35],apply(livefreq2023[livefreq2023$STRATA_ID==35,21:23],1,sum),
               livefreq2024$TOW_NO[livefreq2024$STRATA_ID==35],apply(livefreq2024[livefreq2024$STRATA_ID==35,24:26],1,sum),
               crossref.BoF.2024[crossref.BoF.2024$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrec15)     
AH.rec.spr.est[AH.rec.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
AHrec16 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==35],apply(livefreq2024[livefreq2024$STRATA_ID==35,21:23],1,sum),
               livefreq2025$TOW_NO[livefreq2025$STRATA_ID==35],apply(livefreq2025[livefreq2025$STRATA_ID==35,24:26],1,sum),
               crossref.BoF.2025[crossref.BoF.2025$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrec16)     
AH.rec.spr.est[AH.rec.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
AHrec17 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==35],apply(livefreq2025[livefreq2025$STRATA_ID==35,21:23],1,sum),
               livefreq2026$TOW_NO[livefreq2026$STRATA_ID==35],apply(livefreq2026[livefreq2026$STRATA_ID==35,24:26],1,sum),
               crossref.BoF.2026[crossref.BoF.2026$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrec17)     
AH.rec.spr.est[AH.rec.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

AH.rec.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(AH.rec.spr.est$Year, AH.rec.spr.est$Yspr, xout=2020) #  58.43742
AH.rec.spr.est[AH.rec.spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(58.43742,  568.86836) #assume var from 2019


#make dataframe for all of AH Commercial Abundance
AH.rec.spr.est$method <- "spr"
AH.rec.spr.est$NH <- 8626.27
AH.rec.spr.est$Area <- "AH"
AH.rec.spr.est$Age <- "Recruit"
names(AH.rec.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA1B.AH.Rec <- rbind(SPA1B.AH.Rec.simple [SPA1B.AH.Rec.simple$Year<2009,], AH.rec.spr.est)
SPA1B.AH.Rec$sd <- sqrt(SPA1B.AH.Rec$var.y)
SPA1B.AH.Rec$Pop <- SPA1B.AH.Rec$Mean.nums*SPA1B.AH.Rec$NH

############ Commercial (80+ mm)
years <- 2001:surveyyear
X <- length(years)

SPA1B.AH.Comm.simple <- data.frame(Year = years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(8626.27, X),Area = rep("AH", X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.AH.Comm.simple$Year)) {
  temp.data <- livefreq[livefreq$YEAR == 2000 + i,]

SPA1B.AH.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
SPA1B.AH.Comm.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.AH.Comm.simple

#assume 1997-2000 same as MBN
SPA1B.AH.Comm.simple <- rbind(SPA1B.MBN.Comm[SPA1B.MBN.Comm$Year < 2001,c(1:7)], SPA1B.AH.Comm.simple)
SPA1B.AH.Comm.simple[c(1:4), "NH"] <- 8626.27
SPA1B.AH.Comm.simple[c(1:4), "Area"] <- "AH"

#interpolate missing year
approx(SPA1B.AH.Comm.simple$Year, SPA1B.AH.Comm.simple$Mean.nums, xout = 2004)#328.05
SPA1B.AH.Comm.simple[SPA1B.AH.Comm.simple$Year == 2004,c("Mean.nums", "var.y")] <- c(328.05, 98033.303) #assume 2003 variance 

#interpolate missing year
approx(SPA1B.AH.Comm.simple$Year, SPA1B.AH.Comm.simple$Mean.nums, xout = 2020)#604.8125
SPA1B.AH.Comm.simple[SPA1B.AH.Comm.simple$Year == 2020,c("Mean.nums", "var.y")] <- c(604.8125, 165010.8967) #assume 2019 variance 

SPA1B.AH.Comm.simple


#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
AH.spr.est <- data.frame(Year=spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

## spr mean for commercial size (>80mm)
#2008/2009
AHcomm1<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==35],apply(livefreq2008[livefreq2008$STRATA_ID==35,24:50],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID==35],apply(livefreq2009[livefreq2009$STRATA_ID==35,27:50],1,sum),
crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcomm1)    #158.9466
AH.spr.est[AH.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
AHcomm2<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==35],apply(livefreq2009[livefreq2009$STRATA_ID==35,24:50],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID==35],apply(livefreq2010[livefreq2010$STRATA_ID==35,27:50],1,sum),
crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHcomm2,summary (AHcomm1) )    # 235.508
AH.spr.est[AH.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
AHcomm3<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==35],apply(livefreq2010[livefreq2010$STRATA_ID==35,24:50],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==35],apply(livefreq2011[livefreq2011$STRATA_ID==35,27:50],1,sum),
crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1) ))   #217.5651
AH.spr.est[AH.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
AHcomm4<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==35],apply(livefreq2011[livefreq2011$STRATA_ID==35,24:50],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==35],apply(livefreq2012[livefreq2012$STRATA_ID==35,27:50],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHcomm4,summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1) )))   # 182.9774
AH.spr.est[AH.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
AHcomm5<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==35],apply(livefreq2012[livefreq2012$STRATA_ID==35,24:50],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==35],apply(livefreq2013[livefreq2013$STRATA_ID==35,27:50],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1) ))))     #188.32
AH.spr.est[AH.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
AHcomm6<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==35],apply(livefreq2013[livefreq2013$STRATA_ID==35,24:50],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID==35],apply(livefreq2014[livefreq2014$STRATA_ID==35,27:50],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1))))))   #354.56
AH.spr.est[AH.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
AHcomm7<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==35],apply(livefreq2014[livefreq2014$STRATA_ID==35,24:50],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID==35],apply(livefreq2015[livefreq2015$STRATA_ID==35,27:50],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1) ))))))     #315.8221
AH.spr.est[AH.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
AHcomm8<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==35],apply(livefreq2015[livefreq2015$STRATA_ID==35,24:50],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID==35],apply(livefreq2016[livefreq2016$STRATA_ID==35,27:50],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1) )))))))     #189.6051
AH.spr.est[AH.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
AHcomm9<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==35],apply(livefreq2016[livefreq2016$STRATA_ID==35,24:50],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID==35],apply(livefreq2017[livefreq2017$STRATA_ID==35,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1) ))))))))     #107.4146
AH.spr.est[AH.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
AHcomm10<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==35],apply(livefreq2017[livefreq2017$STRATA_ID==35,24:50],1,sum),
             livefreq2018$TOW_NO[livefreq2018$STRATA_ID==35],apply(livefreq2018[livefreq2018$STRATA_ID==35,27:50],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary (AHcomm2,summary (AHcomm1) )))))))))     # 211.3487
AH.spr.est[AH.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
AHcomm11 <- spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID == 35],apply(livefreq2018[livefreq2018$STRATA_ID == 35,24:50],1,sum),
                livefreq2019$TOW_NO[livefreq2019$STRATA_ID == 35],apply(livefreq2019[livefreq2019$STRATA_ID == 35,27:50],1,sum),
                crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcomm11, summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary(AHcomm2,summary (AHcomm1) ))))))))))     #154.6791
AH.spr.est[AH.spr.est$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
AHcomm12 <- spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID == 35],apply(livefreq2019[livefreq2019$STRATA_ID == 35,24:50],1,sum),
                livefreq2021$TOW_NO[livefreq2021$STRATA_ID == 35],apply(livefreq2021[livefreq2021$STRATA_ID == 35,27:50],1,sum),
                crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcomm12,summary(AHcomm11, summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary(AHcomm2,summary (AHcomm1) )))))))))))    
AH.spr.est[AH.spr.est$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 ##???!!! Value 303.1547 much higher than simple mean of 207.95 -- unusual - why? SPR based off only 4 tows of 8 tows total 
AHcomm13 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID == 35],apply(livefreq2021[livefreq2021$STRATA_ID == 35,24:50],1,sum),
                livefreq2022$TOW_NO[livefreq2022$STRATA_ID == 35],apply(livefreq2022[livefreq2022$STRATA_ID == 35,27:50],1,sum),
                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcomm13,summary(AHcomm12,summary(AHcomm11, summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary(AHcomm2,summary (AHcomm1) ))))))))))))    
AH.spr.est[AH.spr.est$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
AHcomm14 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID == 35],apply(livefreq2022[livefreq2022$STRATA_ID == 35,24:50],1,sum),
                livefreq2023$TOW_NO[livefreq2023$STRATA_ID == 35],apply(livefreq2023[livefreq2023$STRATA_ID == 35,27:50],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcomm14,summary(AHcomm13,summary(AHcomm12,summary(AHcomm11, summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary(AHcomm2,summary (AHcomm1) )))))))))))))  
AH.spr.est[AH.spr.est$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
AHcomm15 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID == 35],apply(livefreq2023[livefreq2023$STRATA_ID == 35,24:50],1,sum),
                livefreq2024$TOW_NO[livefreq2024$STRATA_ID == 35],apply(livefreq2024[livefreq2024$STRATA_ID == 35,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcomm15,summary(AHcomm14,summary(AHcomm13,summary(AHcomm12,summary(AHcomm11, summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary(AHcomm2,summary (AHcomm1) ))))))))))))))  
AH.spr.est[AH.spr.est$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
AHcomm16 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID == 35],apply(livefreq2024[livefreq2024$STRATA_ID == 35,24:50],1,sum),
                livefreq2025$TOW_NO[livefreq2025$STRATA_ID == 35],apply(livefreq2025[livefreq2025$STRATA_ID == 35,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcomm16,summary(AHcomm15,summary(AHcomm14,summary(AHcomm13,summary(AHcomm12,summary(AHcomm11, summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary(AHcomm2,summary (AHcomm1) )))))))))))))))  
AH.spr.est[AH.spr.est$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
AHcomm17 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID == 35],apply(livefreq2025[livefreq2025$STRATA_ID == 35,24:50],1,sum),
                livefreq2026$TOW_NO[livefreq2026$STRATA_ID == 35],apply(livefreq2026[livefreq2026$STRATA_ID == 35,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcomm17, summary(AHcomm16,summary(AHcomm15,summary(AHcomm14,summary(AHcomm13,summary(AHcomm12,summary(AHcomm11, summary (AHcomm10,summary (AHcomm9, summary (AHcomm8, summary (AHcomm7,summary (AHcomm6, summary (AHcomm5, summary (AHcomm4,summary (AHcomm3,  summary(AHcomm2,summary (AHcomm1) ))))))))))))))))  
AH.spr.est[AH.spr.est$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

AH.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(AH.spr.est$Year, AH.spr.est$Yspr, xout=2020) #   109.9197
AH.spr.est[AH.spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c( 109.9197,   6060.6056) #assume var from 2019


#make dataframe for all of AH Commercial Abundance
AH.spr.est$method <- "spr"
AH.spr.est$NH <- 8626.27
AH.spr.est$Area <- "AH"
AH.spr.est$Age <- "Commercial"
names(AH.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA1B.AH.Comm <- rbind(SPA1B.AH.Comm.simple[SPA1B.AH.Comm.simple$Year < 2009,], AH.spr.est)
SPA1B.AH.Comm$sd <- sqrt(SPA1B.AH.Comm$var.y)
SPA1B.AH.Comm$Pop <- SPA1B.AH.Comm$Mean.nums*SPA1B.AH.Comm$NH

#combine Recruit and Commercial dataframes

SPA1B.AH.Numbers <- rbind(SPA1B.AH.Rec,SPA1B.AH.Comm)

####
# ---- 6. Spencer's Island ----
####

############ Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 2005:surveyyear
X <- length(years)

SPA1B.SI.Rec.simple <- data.frame(Year=years, Mean.nums = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep (20337.96,X), Area = rep("SI", X),Age = rep("Recruit",X))
for (i in 1:length(SPA1B.SI.Rec.simple$Year)) {
  temp.data <- livefreq[livefreq$YEAR == 2004 + i,]

	SPA1B.SI.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
	SPA1B.SI.Rec.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 24:26],1,sum))
}
SPA1B.SI.Rec.simple

#assume 1997-2000 same as MBN; assume 2001-2004 same as Outer
SPA1B.SI.Rec.simple <- rbind(SPA1B.Out.Rec.simple[c(5:8),c(1:7)], SPA1B.SI.Rec.simple)
SPA1B.SI.Rec.simple <- rbind(SPA1B.MBN.Rec[SPA1B.MBN.Rec$Year < 2001,c(1:7)], SPA1B.SI.Rec.simple)
SPA1B.SI.Rec.simple[SPA1B.SI.Rec.simple$Year < 2005, "NH"] <- 20337.96
SPA1B.SI.Rec.simple[SPA1B.SI.Rec.simple$Year < 2005, "Area"] <- "SI"

#interpolate missing year 2020
approx(SPA1B.SI.Rec.simple$Year, SPA1B.SI.Rec.simple$Mean.nums, xout = 2020)#12.83
SPA1B.SI.Rec.simple[SPA1B.SI.Rec.simple$Year == 2020,c("Mean.nums", "var.y")] <- c(12.83, 385.442000) #assume 2019 variance 


#calculate CV and Pop. estimate
SPA1B.SI.Rec.simple$sd <- sqrt(SPA1B.SI.Rec.simple$var.y)
SPA1B.SI.Rec.simple$Pop <- SPA1B.SI.Rec.simple$Mean.nums*SPA1B.SI.Rec.simple$NH


############ Commercial (80+ mm)

#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 2005:surveyyear
X <- length(years)

SPA1B.SI.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(20337.96, X), Area=rep("SI", X), Age=rep("Commercial", X))
for (i in 1:length(SPA1B.SI.Comm.simple$Year))
  {
  temp.data <- livefreq[livefreq$YEAR == 2004 + i,]

	SPA1B.SI.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
	SPA1B.SI.Comm.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
}
SPA1B.SI.Comm.simple

#assume 1997-2000 same as MBN; assume 2001-2004 same as Outer
SPA1B.SI.Comm.simple <- rbind(SPA1B.Out.Comm.simple[c(5:8),c(1:7)], SPA1B.SI.Comm.simple)
SPA1B.SI.Comm.simple <- rbind(SPA1B.MBN.Comm[SPA1B.MBN.Rec$Year<2001,c(1:7)], SPA1B.SI.Comm.simple)
SPA1B.SI.Comm.simple[SPA1B.SI.Comm.simple$Year<2005, "NH"] <- 20337.96
SPA1B.SI.Comm.simple[SPA1B.SI.Comm.simple$Year<2005, "Area"] <- "SI"

#interpolate missing year 2020
approx(SPA1B.SI.Comm.simple$Year, SPA1B.SI.Comm.simple$Mean.nums, xout = 2020)#22.24
SPA1B.SI.Comm.simple[SPA1B.SI.Comm.simple$Year == 2020,c("Mean.nums", "var.y")] <- c(22.24, 1587.762) #assume 2019 variance 


#calculate CV and Pop.estimate
SPA1B.SI.Comm.simple$sd <- sqrt(SPA1B.SI.Comm.simple$var.y)
SPA1B.SI.Comm.simple$Pop <- SPA1B.SI.Comm.simple$Mean.nums*SPA1B.SI.Comm.simple$NH

#combine Recruit and Commercial dataframes
SPA1B.SI.Numbers <- rbind(SPA1B.SI.Rec.simple, SPA1B.SI.Comm.simple)

#livefreq[livefreq$YEAR==2024 & livefreq$STRATA_ID == 52,] no recruit or commercial size scallop in 2024 in Spencers Island 

####
# ---- 7. Scots Bay ----
####

############ Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 2005:surveyyear
X <- length(years)

SPA1B.SB.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep (19415.98,X), Area=rep("SB", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1B.SB.Rec.simple$Year)){
  temp.data <- livefreq[livefreq$YEAR==2004+i,]

	SPA1B.SB.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==51 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
	SPA1B.SB.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==51 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA1B.SB.Rec.simple

#assume 1997-2000 same as MBN; assume 2001-2004 same as Outer
SPA1B.SB.Rec.simple <- rbind(SPA1B.Out.Rec.simple[c(5:8),c(1:7)], SPA1B.SB.Rec.simple)
SPA1B.SB.Rec.simple <- rbind(SPA1B.MBN.Rec[SPA1B.MBN.Rec$Year<2001,c(1:7)], SPA1B.SB.Rec.simple)
SPA1B.SB.Rec.simple[SPA1B.SB.Rec.simple$Year<2005, "NH"] <- 19415.98
SPA1B.SB.Rec.simple[SPA1B.SB.Rec.simple$Year<2005, "Area"] <- "SB"

#interpolate missing year 2020
approx(SPA1B.SB.Rec.simple$Year, SPA1B.SB.Rec.simple$Mean.nums, xout = 2020)#19.4375
SPA1B.SB.Rec.simple[SPA1B.SB.Rec.simple$Year == 2020,c("Mean.nums", "var.y")] <- c(19.4375, 521.035833) #assume 2019 variance 

#calculate CV and Pop. estimate
SPA1B.SB.Rec.simple$sd <- sqrt(SPA1B.SB.Rec.simple$var.y)
SPA1B.SB.Rec.simple$Pop<-SPA1B.SB.Rec.simple$Mean.nums*SPA1B.SB.Rec.simple$NH


############ Commercial (80+ mm)

#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 2005:surveyyear
X <- length(years)

SPA1B.SB.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(19415.98, X), Area=rep("SB", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1B.SB.Comm.simple$Year))
  {
  temp.data <- livefreq[livefreq$YEAR==2004+i,]

	SPA1B.SB.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==51 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
	SPA1B.SB.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==51 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA1B.SB.Comm.simple

#assume 1997-2000 same as MBN; assume 2000-2004 same as Outer
SPA1B.SB.Comm.simple <- rbind(SPA1B.Out.Comm.simple[c(5:8),c(1:7)], SPA1B.SB.Comm.simple)
SPA1B.SB.Comm.simple <- rbind(SPA1B.MBN.Comm[SPA1B.MBN.Comm$Year < 2001, c(1:7)], SPA1B.SB.Comm.simple)
SPA1B.SB.Comm.simple[SPA1B.SB.Comm.simple$Year < 2005, "NH"] <- 19415.98
SPA1B.SB.Comm.simple[SPA1B.SB.Comm.simple$Year < 2005, "Area"] <- "SB"

#interpolate missing year 2020
approx(SPA1B.SB.Comm.simple$Year, SPA1B.SB.Comm.simple$Mean.nums, xout = 2020)#14.2375
SPA1B.SB.Comm.simple[SPA1B.SB.Comm.simple$Year == 2020,c("Mean.nums", "var.y")] <- c(14.2375, 43.15333) #assume 2019 variance 


#calculate CV and Pop.estimate
SPA1B.SB.Comm.simple$sd <- sqrt(SPA1B.SB.Comm.simple$var.y)
SPA1B.SB.Comm.simple$Pop <- SPA1B.SB.Comm.simple$Mean.nums*SPA1B.SB.Comm.simple$NH

#combine Recruit and Commercial dataframes
SPA1B.SB.Numbers <- rbind(SPA1B.SB.Rec.simple, SPA1B.SB.Comm.simple)

####
# ---- Make Numbers dataframe for SPA 1B ----
####

SPA1B.Numbers <- rbind (SPA1B.CS.Numbers, SPA1B.MBN.Numbers, SPA1B.28C.Numbers, SPA1B.Out.Numbers, SPA1B.AH.Numbers, SPA1B.SI.Numbers, SPA1B.SB.Numbers)

write.csv(SPA1B.Numbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1B.Index.Numbers.",surveyyear,".csv"))


###
###
### ---- RECRUIT AND COMMERCIAL WEIGHT PER TOW  ----
###
###


####
# ---- 1. Cape Spencer (simple mean for whole area) ----
####

############ Recruit (65-79 mm)
#use simple mean
years <- 1997:surveyyear
X <- length(years)

SPA1B.CS.RecWt.simple <- data.frame(Year=years, Mean.wt=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(191023.77, X),Area=rep("CS",X), Age=rep("Recruit", X))
for(i in 1:length(SPA1B.CS.RecWt.simple$Year)){
  temp.data <- BFliveweight[BFliveweight$YEAR==1996+i,]

	SPA1B.CS.RecWt.simple[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
	SPA1B.CS.RecWt.simple[i,3]<- var(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
 }
SPA1B.CS.RecWt.simple

#interpolate missing year 2020
approx(SPA1B.CS.RecWt.simple$Year, SPA1B.CS.RecWt.simple$Mean.wt, xout = 2020)#36.49725
SPA1B.CS.RecWt.simple[SPA1B.CS.RecWt.simple$Year == 2020,c("Mean.wt", "var.y")] <- c(36.49725, 5019.6677) #assume 2019 variance 

SPA1B.CS.RecWt.simple$cv <- sqrt(SPA1B.CS.RecWt.simple$var.y)/SPA1B.CS.RecWt.simple$Mean.wt
SPA1B.CS.RecWt.simple$kg.tow <- SPA1B.CS.RecWt.simple$Mean.wt/1000
SPA1B.CS.RecWt.simple$Bmass <- (SPA1B.CS.RecWt.simple$kg*SPA1B.CS.RecWt.simple$NH)/1000

############ Commercial (80+ mm)
#use simple mean
years <- 1997:surveyyear
X <- length(years)

SPA1B.CS.CommWt.simple <- data.frame(Year=years, Mean.wt=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(191023.77, X),Area=rep("CS",X), Age=rep("Commercial", X))
for(i in 1:length(SPA1B.CS.CommWt.simple$Year)){
  temp.data <- BFliveweight[BFliveweight$YEAR==1996+i,]

	SPA1B.CS.CommWt.simple[i,2]<-mean(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
	SPA1B.CS.CommWt.simple[i,3]<-var(apply(temp.data[temp.data$STRATA_ID==37 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
 }
SPA1B.CS.CommWt.simple

#interpolate missing year 2020
approx(SPA1B.CS.CommWt.simple$Year, SPA1B.CS.CommWt.simple$Mean.wt, xout = 2020)#3686.043
SPA1B.CS.CommWt.simple[SPA1B.CS.CommWt.simple$Year == 2020,c("Mean.wt", "var.y")] <- c(3686.043, 7973781.1) #assume 2019 variance 

SPA1B.CS.CommWt.simple$cv <- sqrt(SPA1B.CS.CommWt.simple$var.y)/SPA1B.CS.CommWt.simple$Mean.wt
SPA1B.CS.CommWt.simple$kg.tow <- SPA1B.CS.CommWt.simple$Mean.wt/1000
SPA1B.CS.CommWt.simple$Bmass <- (SPA1B.CS.CommWt.simple$kg*SPA1B.CS.CommWt.simple$NH)/1000

#combine Recruit and Commercial dataframes
SPA1B.CS.Weight <- rbind(SPA1B.CS.RecWt.simple, SPA1B.CS.CommWt.simple)

####
# ---- 2. Middle Bay North (spr) ----
####

#2a. Middle Bay North - EAST (fake strada_id==58)

############Recruit (65-79)
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNE.RecWt.simple <- data.frame(Year=years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), Prop = rep(0.732, X))
for (i in 1:length(SPA1B.MBNE.RecWt.simple$Year)){
	temp.data <- BFliveweight[BFliveweight$YEAR== 1996+i,]
SPA1B.MBNE.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==58 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
SPA1B.MBNE.RecWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID==58 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
}
SPA1B.MBNE.RecWt.simple

#interpolate missing year 2020
approx(SPA1B.MBNE.RecWt.simple$Year, SPA1B.MBNE.RecWt.simple$Mean.wt, xout = 2020)#186.5456
SPA1B.MBNE.RecWt.simple[SPA1B.MBNE.RecWt.simple$Year == 2020,c("Mean.wt", "var.y")] <- c(186.5456, 4752.52636) #assume 2019 variance 


#spr for recruits
#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear #update to most recent year
Y <- length(spryears)
MBNE.rec.sprwt.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009 spr
#Only two tows

#2009/2010 spr
Erecwt1<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==58],apply(liveweight2009[liveweight2009$STRATA_ID==58,23:25],1,sum), liveweight2010$TOW_NO[liveweight2010$STRATA_ID==58],apply(liveweight2010[liveweight2010$STRATA_ID==58,26:28],1,sum), crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt1)  #
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
# Erecwt2<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==58],apply(liveweight2010[liveweight2010$STRATA_ID==58,23:25],1,sum),
#     liveweight2011$TOW_NO[liveweight2011$STRATA_ID==58],apply(liveweight2011[liveweight2011$STRATA_ID==58,26:28],1,sum),
#     crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
  #SD is Zero, use simple mean
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2011,c(2:3)] <- c(3.669365,85.08345)

#2011/2012 spr
Erecwt3<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==58],apply(liveweight2011[liveweight2011$STRATA_ID==58,23:25],1,sum),
    liveweight2012$TOW_NO[liveweight2012$STRATA_ID==58],apply(liveweight2012[liveweight2012$STRATA_ID==58,26:28],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt3)  #
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
Erecwt4<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==58],apply(liveweight2012[liveweight2012$STRATA_ID==58,23:25],1,sum),
    liveweight2013$TOW_NO[liveweight2013$STRATA_ID==58],apply(liveweight2013[liveweight2013$STRATA_ID==58,26:28],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt4)#
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
Erecwt5<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==58],apply(liveweight2013[liveweight2013$STRATA_ID==58,23:25],1,sum),
    liveweight2014$TOW_NO[liveweight2014$STRATA_ID==58],apply(liveweight2014[liveweight2014$STRATA_ID==58,26:28],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt5)  #
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
Erecwt6<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==58],apply(liveweight2014[liveweight2014$STRATA_ID==58,23:25],1,sum),
    liveweight2015$TOW_NO[liveweight2015$STRATA_ID==58],apply(liveweight2015[liveweight2015$STRATA_ID==58,26:28],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt6)  #
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
Erecwt7<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==58],apply(liveweight2015[liveweight2015$STRATA_ID==58,23:25],1,sum),
             liveweight2016$TOW_NO[liveweight2016$STRATA_ID==58],apply(liveweight2016[liveweight2016$STRATA_ID==58,26:28],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt7)  #19.98820
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
Erecwt8<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==58],apply(liveweight2016[liveweight2016$STRATA_ID==58,23:25],1,sum),
             liveweight2017$TOW_NO[liveweight2017$STRATA_ID==58],apply(liveweight2017[liveweight2017$STRATA_ID==58,26:28],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt8)  #15.573044
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
Erecwt9<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==58],apply(liveweight2017[liveweight2017$STRATA_ID==58,23:25],1,sum),
             liveweight2018$TOW_NO[liveweight2018$STRATA_ID==58],apply(liveweight2018[liveweight2018$STRATA_ID==58,26:28],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt9)  #34.539185
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
Erecwt10<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 58],apply(liveweight2018[liveweight2018$STRATA_ID == 58,23:25],1,sum),
              liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 58],apply(liveweight2019[liveweight2019$STRATA_ID == 58,26:28],1,sum),
             crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Erecwt10)  #54.862386
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 spr
Erecwt11<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 58],apply(liveweight2019[liveweight2019$STRATA_ID == 58,23:25],1,sum),
              liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 58],apply(liveweight2021[liveweight2021$STRATA_ID == 58,26:28],1,sum),
              crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(Erecwt11)  #54.862386
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr
Erecwt12 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 58],apply(liveweight2021[liveweight2021$STRATA_ID == 58,23:25],1,sum),
              liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 58],apply(liveweight2022[liveweight2022$STRATA_ID == 58,26:28],1,sum),
              crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Erecwt12)  #54.862386
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr
Erecwt13 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 58],apply(liveweight2022[liveweight2022$STRATA_ID == 58,23:25],1,sum),
                liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 58],apply(liveweight2023[liveweight2023$STRATA_ID == 58,26:28],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Erecwt13)  #54.862386
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr
#Erecwt14 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 58],apply(liveweight2023[liveweight2023$STRATA_ID == 58,23:25],1,sum),
#                liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 58],apply(liveweight2024[liveweight2024$STRATA_ID == 58,26:28],1,sum),
#                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
#K <- summary(Erecwt14)  #54.862386
#MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
# 2023 repeats used for 2024 all had zero recruits so cannot use SPR 

#2024/2025 spr
#Erecwt15 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 58],apply(liveweight2024[liveweight2024$STRATA_ID == 58,23:25],1,sum),
#                liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 58],apply(liveweight2025[liveweight2025$STRATA_ID == 58,26:28],1,sum),
#                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
#K <- summary(Erecwt15)  #
#MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
# 2024 repeats used for 2025 all had zero recruits so cannot use SPR 

# See if tows assocaited with given year were all zero or not 
#test <- liveweight2024 %>% filter(TOW_NO %in% (crossref.BoF.2025$TOW_NO_REF[crossref.BoF.2025$STRATA_ID==58])) 
#apply(test[test$STRATA_ID==58,23:25],1,sum)

#2025/2026 spr
Erecwt16 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 58],apply(liveweight2025[liveweight2025$STRATA_ID == 58,23:25],1,sum),
                liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 58],apply(liveweight2026[liveweight2026$STRATA_ID == 58,26:28],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Erecwt16)  #
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNE.rec.sprwt.est

#interpolate missing year 2020
approx(MBNE.rec.sprwt.est$Year, MBNE.rec.sprwt.est$Yspr, xout = 2020)#118.3922
MBNE.rec.sprwt.est[MBNE.rec.sprwt.est$Year == 2020,c("Yspr", "var.Yspr.corrected")] <- c(118.3922,  152.49490) #assume 2019 variance 


#make dataframe for all of MBN East recruit Weight/tow
MBNE.rec.sprwt.est$method <- "spr"
MBNE.rec.sprwt.est$Prop <- 0.732
names(MBNE.rec.sprwt.est) <- c("Year", "Mean.wt", "var.y", "method", "Prop")


SPA1B.MBNE.RecWt <- rbind(SPA1B.MBNE.RecWt.simple[SPA1B.MBNE.RecWt.simple$Year<2010,],MBNE.rec.sprwt.est)
#Substitute simple mean for 2024 
SPA1B.MBNE.RecWt[SPA1B.MBNE.RecWt$Year == 2024,c("Mean.wt", "var.y","method")] <- SPA1B.MBNE.RecWt.simple[SPA1B.MBNE.RecWt.simple$Year==2024,c("Mean.wt", "var.y","method")]
SPA1B.MBNE.RecWt[SPA1B.MBNE.RecWt$Year == 2025,c("Mean.wt", "var.y","method")] <- SPA1B.MBNE.RecWt.simple[SPA1B.MBNE.RecWt.simple$Year==2025,c("Mean.wt", "var.y","method")]
SPA1B.MBNE.RecWt$cv <- sqrt(SPA1B.MBNE.RecWt$var.y)/SPA1B.MBNE.RecWt$Mean.wt
SPA1B.MBNE.RecWt

############# COMMERCIAL (>80mm)
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNE.CommWt.simple <- data.frame(Year=years, Mean.wt=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X),Prop=rep(0.732, X))
for(i in 1:length(SPA1B.MBNE.CommWt.simple$Year)){
	temp.data <- BFliveweight[BFliveweight$YEAR==1996+i,]
SPA1B.MBNE.CommWt.simple[i,2]<-mean(apply(temp.data[temp.data$STRATA_ID == 58 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
SPA1B.MBNE.CommWt.simple[i,3]<- var(apply(temp.data[temp.data$STRATA_ID == 58 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
}
SPA1B.MBNE.CommWt.simple

#interpolate missing year 2020
approx(SPA1B.MBNE.CommWt.simple$Year, SPA1B.MBNE.CommWt.simple$Mean.wt, xout = 2020)#1828.616
SPA1B.MBNE.CommWt.simple[SPA1B.MBNE.CommWt.simple$Year == 2020,c("Mean.wt", "var.y")] <- c(1828.616,  2961264.06) #assume 2019 variance 


#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear #update to most recent year
Y <- length(spryears)
MBNE.sprwt.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2019 spr
#Only two tows

#2009/2010 spr
Ecomwt1<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==58],apply(liveweight2009[liveweight2009$STRATA_ID==58,26:52],1,sum), liveweight2010$TOW_NO[liveweight2010$STRATA_ID==58],apply(liveweight2010[liveweight2010$STRATA_ID==58,29:52],1,sum), crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt1)  #1361
MBNE.sprwt.est[MBNE.sprwt.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
Ecomwt2<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==58],apply(liveweight2010[liveweight2010$STRATA_ID==58,26:52],1,sum),
    liveweight2011$TOW_NO[liveweight2011$STRATA_ID==58],apply(liveweight2011[liveweight2011$STRATA_ID==58,29:52],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt2,summary (Ecomwt1))  #1178
MBNE.sprwt.est[MBNE.sprwt.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
Ecomwt3<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==58],apply(liveweight2011[liveweight2011$STRATA_ID==58,26:52],1,sum),
    liveweight2012$TOW_NO[liveweight2012$STRATA_ID==58],apply(liveweight2012[liveweight2012$STRATA_ID==58,29:52],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt3, summary (Ecomwt2,summary (Ecomwt1)))  # 1088
MBNE.sprwt.est[MBNE.sprwt.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
Ecomwt4<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==58],apply(liveweight2012[liveweight2012$STRATA_ID==58,26:52],1,sum),
    liveweight2013$TOW_NO[liveweight2013$STRATA_ID==58],apply(liveweight2013[liveweight2013$STRATA_ID==58,29:52],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt4, summary (Ecomwt3, summary (Ecomwt2,summary (Ecomwt1))))#1506
MBNE.sprwt.est[MBNE.sprwt.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
Ecomwt5<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==58],apply(liveweight2013[liveweight2013$STRATA_ID==58,26:52],1,sum),
    liveweight2014$TOW_NO[liveweight2014$STRATA_ID==58],apply(liveweight2014[liveweight2014$STRATA_ID==58,29:52],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary (Ecomwt2,summary (Ecomwt1)))))  #1581
MBNE.sprwt.est[MBNE.sprwt.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
Ecomwt6<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==58],apply(liveweight2014[liveweight2014$STRATA_ID==58,26:52],1,sum),
    liveweight2015$TOW_NO[liveweight2015$STRATA_ID==58],apply(liveweight2015[liveweight2015$STRATA_ID==58,29:52],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary (Ecomwt2,summary (Ecomwt1))))))  #2104.004
MBNE.sprwt.est[MBNE.sprwt.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
Ecomwt7<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==58],apply(liveweight2015[liveweight2015$STRATA_ID==58,26:52],1,sum),
             liveweight2016$TOW_NO[liveweight2016$STRATA_ID==58],apply(liveweight2016[liveweight2016$STRATA_ID==58,29:52],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1)))))))  # 2010.832
MBNE.sprwt.est[MBNE.sprwt.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
Ecomwt8<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==58],apply(liveweight2016[liveweight2016$STRATA_ID==58,26:52],1,sum),
             liveweight2017$TOW_NO[liveweight2017$STRATA_ID==58],apply(liveweight2017[liveweight2017$STRATA_ID==58,29:52],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1))))))))  #2019.632
MBNE.sprwt.est[MBNE.sprwt.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
Ecomwt9<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==58],apply(liveweight2017[liveweight2017$STRATA_ID==58,26:52],1,sum),
             liveweight2018$TOW_NO[liveweight2018$STRATA_ID==58],apply(liveweight2018[liveweight2018$STRATA_ID==58,29:52],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==58,c("TOW_NO_REF","TOW_NO")])
K<-summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1)))))))))  # 2612.116
MBNE.sprwt.est[MBNE.sprwt.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
Ecomwt10 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 58],apply(liveweight2018[liveweight2018$STRATA_ID == 58,26:52],1,sum),
                liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 58],apply(liveweight2019[liveweight2019$STRATA_ID == 58,29:52],1,sum),
                crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary (Ecomwt10, summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1))))))))) ) #1364.150
MBNE.sprwt.est[MBNE.sprwt.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 spr
Ecomwt11 <- spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 58],apply(liveweight2019[liveweight2019$STRATA_ID == 58,26:52],1,sum),
                liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 58],apply(liveweight2021[liveweight2021$STRATA_ID == 58,29:52],1,sum),
                crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Ecomwt11, summary (Ecomwt10, summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1))))))))))) #
MBNE.sprwt.est[MBNE.sprwt.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr
Ecomwt12 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 58],apply(liveweight2021[liveweight2021$STRATA_ID == 58,26:52],1,sum),
                liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 58],apply(liveweight2022[liveweight2022$STRATA_ID == 58,29:52],1,sum),
                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Ecomwt12, summary(Ecomwt11, summary (Ecomwt10, summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1)))))))))))) #
MBNE.sprwt.est[MBNE.sprwt.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr
Ecomwt13 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 58],apply(liveweight2022[liveweight2022$STRATA_ID == 58,26:52],1,sum),
                liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 58],apply(liveweight2023[liveweight2023$STRATA_ID == 58,29:52],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Ecomwt13, summary(Ecomwt12, summary(Ecomwt11, summary (Ecomwt10, summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1))))))))))))) #
MBNE.sprwt.est[MBNE.sprwt.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr
Ecomwt14 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 58],apply(liveweight2023[liveweight2023$STRATA_ID == 58,26:52],1,sum),
                liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 58],apply(liveweight2024[liveweight2024$STRATA_ID == 58,29:52],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Ecomwt14,summary(Ecomwt13, summary(Ecomwt12, summary(Ecomwt11, summary (Ecomwt10, summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1)))))))))))))) #
MBNE.sprwt.est[MBNE.sprwt.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 spr
Ecomwt15 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 58],apply(liveweight2024[liveweight2024$STRATA_ID == 58,26:52],1,sum),
                liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 58],apply(liveweight2025[liveweight2025$STRATA_ID == 58,29:52],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Ecomwt15, summary(Ecomwt14,summary(Ecomwt13, summary(Ecomwt12, summary(Ecomwt11, summary (Ecomwt10, summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1))))))))))))))) #
MBNE.sprwt.est[MBNE.sprwt.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 spr
Ecomwt16 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 58],apply(liveweight2025[liveweight2025$STRATA_ID == 58,26:52],1,sum),
                liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 58],apply(liveweight2026[liveweight2026$STRATA_ID == 58,29:52],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(Ecomwt16, summary(Ecomwt15, summary(Ecomwt14,summary(Ecomwt13, summary(Ecomwt12, summary(Ecomwt11, summary (Ecomwt10, summary (Ecomwt9,summary (Ecomwt8, summary (Ecomwt7,summary (Ecomwt6, summary (Ecomwt5, summary (Ecomwt4, summary (Ecomwt3, summary(Ecomwt2,summary(Ecomwt1)))))))))))))))) #
MBNE.sprwt.est[MBNE.sprwt.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNE.sprwt.est

#interpolate missing year 2020
approx(MBNE.sprwt.est$Year, MBNE.sprwt.est$Yspr, xout = 2020)#1462.404
MBNE.sprwt.est[MBNE.sprwt.est$Year == 2020,c("Yspr", "var.Yspr.corrected")] <- c(1462.404,  87704.57) #assume 2019 variance 

#make dataframe for all of MBN East Commercial Weight/tow
MBNE.sprwt.est$method <- "spr"
MBNE.sprwt.est$Prop <- 0.732
names(MBNE.sprwt.est) <- c("Year", "Mean.wt", "var.y", "method", "Prop")

SPA1B.MBNE.CommWt <- rbind(SPA1B.MBNE.CommWt.simple[SPA1B.MBNE.CommWt.simple$Year<2010,],MBNE.sprwt.est)
SPA1B.MBNE.CommWt$cv <- sqrt(SPA1B.MBNE.CommWt$var.y)/SPA1B.MBNE.CommWt$Mean.wt

#2b. Middle Bay North - WEST

############Recruit (65-79)
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.RecWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), Prop = rep(0.268, X))
for (i in 1:length(SPA1B.MBNW.RecWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR == 1996 + i,]
SPA1B.MBNW.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
SPA1B.MBNW.RecWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
}
SPA1B.MBNW.RecWt.simple

#fill in missing years, assume 1997-2002 same as MBNE, interpolate 2004
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==1997, c(2,3)]<-c(57.563047 ,  3453.86860)
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==1998, c(2,3)]<-c(5.587555,     93.99806)
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==1999, c(2,3)]<-c(13.052969 ,   261.78059)
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==2000, c(2,3)]<-c(10.072877 ,   297.97121)
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==2001, c(2,3)]<-c(22.022054 ,   734.14100)
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==2002, c(2,3)]<-c( 1.071548  ,   12.63037)
#2004
approx(SPA1B.MBNW.RecWt.simple$Year, SPA1B.MBNW.RecWt.simple$Mean.wt, xout=2004) # 26.47331; No tows in W
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==2004,c(2,3)]<- c(26.47331,2316.80608 )#previous years variance
#2020
approx(SPA1B.MBNW.RecWt.simple$Year, SPA1B.MBNW.RecWt.simple$Mean.wt, xout=2020) # 119.7878
SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year==2020,c(2,3)]<- c(119.7878,111908.61881  )#Given large value - take 2021 var 

#SPR for recruits
#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
MBNW.rec.sprwt.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2008/2009 spr
Wrecwt<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==38],apply(liveweight2008[liveweight2008$STRATA_ID==38,23:25],1,sum),
    liveweight2009$TOW_NO[liveweight2009$STRATA_ID==38],apply(liveweight2009[liveweight2009$STRATA_ID==38,26:28],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wrecwt)  #2331
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #1865

#2009/2010 spr
Wrecwt1<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==38],apply(liveweight2009[liveweight2009$STRATA_ID==38,23:25],1,sum),
    liveweight2010$TOW_NO[liveweight2010$STRATA_ID==38],apply(liveweight2010[liveweight2010$STRATA_ID==38,26:28],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wrecwt1)
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #1865

#2010/2011 spr
#Wrecwt2<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==38],apply(liveweight2010[liveweight2010$STRATA_ID==38,23:25],1,sum),liveweight2011$TOW_NO[liveweight2011$STRATA_ID==38],apply(liveweight2011[liveweight2011$STRATA_ID==38,26:28],1,sum),crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#! Only two tows from 2010 repeated in 2011, use simple mean
#MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2011,c(2:3)] <- c(16.908146,943.7539)

#2011/2012 spr
Wrecwt3<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==38],apply(liveweight2011[liveweight2011$STRATA_ID==38,23:25],1,sum),
    liveweight2012$TOW_NO[liveweight2012$STRATA_ID==38],apply(liveweight2012[liveweight2012$STRATA_ID==38,26:28],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wrecwt3) #
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
#Wrecwt4<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==38],apply(liveweight2012[liveweight2012$STRATA_ID==38,23:25],1,sum),liveweight2013$TOW_NO[liveweight2013$STRATA_ID==38],apply(liveweight2013[liveweight2013$STRATA_ID==38,26:28],1,sum),crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K<-summary (Wrecwt4) #
#! Only two tows from 2012 repeated in 2013, use simple mean
#MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2013,c(2:3)] <- c( 172.413486, 117897.43970)

#2013/2014 spr
#Wrecwt5<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==38],apply(liveweight2013[liveweight2013$STRATA_ID==38,23:25],1,sum),liveweight2014$TOW_NO[liveweight2014$STRATA_ID==38],apply(liveweight2014[liveweight2013$STRATA_ID==38,26:28],1,sum),crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K<-summary (Wrecwt5) #
#! Only one repeat in 2014, use simple mean
#MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2014,c(2:3)] <- c(59.411865, 6368.14029)

#2014/2015 spr
Wrecwt6<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==38],apply(liveweight2014[liveweight2014$STRATA_ID==38,23:25],1,sum),liveweight2015$TOW_NO[liveweight2015$STRATA_ID==38],apply(liveweight2015[liveweight2015$STRATA_ID==38,26:28],1,sum),crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wrecwt6) #185.25045
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
Wrecwt7<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==38],apply(liveweight2015[liveweight2015$STRATA_ID==38,23:25],1,sum),               liveweight2016$TOW_NO[liveweight2016$STRATA_ID==38],apply(liveweight2016[liveweight2016$STRATA_ID==38,26:28],1,sum),                crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wrecwt7, summary (Wrecwt6)) # 141.77315
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
Wrecwt8<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==38],apply(liveweight2016[liveweight2016$STRATA_ID==38,23:25],1,sum),               liveweight2017$TOW_NO[liveweight2017$STRATA_ID==38],apply(liveweight2017[liveweight2017$STRATA_ID==38,26:28],1,sum),               crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wrecwt8, summary (Wrecwt7, summary (Wrecwt6))) # 98.10598
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
# Wrecwt9<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==38],apply(liveweight2017[liveweight2017$STRATA_ID==38,23:25],1,sum),              liveweight2018$TOW_NO[liveweight2018$STRATA_ID==38],apply(liveweight2018[liveweight2018$STRATA_ID==38,26:28],1,sum),               crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
# K<-summary (Wrecwt9, summary (Wrecwt8, summary (Wrecwt7, summary (Wrecwt6))))
#NA; In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
#MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2018,c(2:3)] <- c( 21.704821, 1110.51692)


#2018/2019 spr
# Wrecwt10 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 38],apply(liveweight2018[liveweight2018$STRATA_ID == 38,23:25],1,sum),
#                 liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 38],apply(liveweight2019[liveweight2019$STRATA_ID == 38,26:28],1,sum),
#                 crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
# K <- summary (Wrecwt10) #In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
#MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2019,c(2:3)] <- c(17.601308, 279.13384)

#2019/2021 spr
 Wrecwt11 <-  spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 38],apply(liveweight2019[liveweight2019$STRATA_ID == 38,23:25],1,sum),
                  liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 38],apply(liveweight2021[liveweight2021$STRATA_ID == 38,26:28],1,sum),
                  crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
 K <- summary(Wrecwt11) #In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr  #Only 1 matched tow - can't use spr 
#Wrecwt12 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 38],apply(liveweight2021[liveweight2021$STRATA_ID == 38,23:25],1,sum),
#               liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 38],apply(liveweight2022[liveweight2022$STRATA_ID == 38,26:28],1,sum),
#               crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(Wrecwt12) #In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
#MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr - can't use spr
#Wrecwt13 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 38],apply(liveweight2022[liveweight2022$STRATA_ID == 38,23:25],1,sum),
#               liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 38],apply(liveweight2023[liveweight2023$STRATA_ID == 38,26:28],1,sum),
#               crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(Wrecwt13) #In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
#MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr -
Wrecwt14 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 38],apply(liveweight2023[liveweight2023$STRATA_ID == 38,23:25],1,sum),
               liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 38],apply(liveweight2024[liveweight2024$STRATA_ID == 38,26:28],1,sum),
               crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(Wrecwt14) #In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 spr
Wrecwt15 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 38],apply(liveweight2024[liveweight2024$STRATA_ID == 38,23:25],1,sum),
                liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 38],apply(liveweight2025[liveweight2025$STRATA_ID == 38,26:28],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(Wrecwt15) #In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 spr
Wrecwt16 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 38],apply(liveweight2025[liveweight2025$STRATA_ID == 38,23:25],1,sum),
                liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 38],apply(liveweight2026[liveweight2026$STRATA_ID == 38,26:28],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(Wrecwt16) #In cor(x$matched.t1t2$y.last.year, x$matched.t1t2$y.this.year):the standard deviation is zero - use simple mean
MBNW.rec.sprwt.est[MBNW.rec.sprwt.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNW.rec.sprwt.est

#make dataframe for all of MBN West Commercial Weight/tow
MBNW.rec.sprwt.est$method <- "spr"
MBNW.rec.sprwt.est$Prop <- 0.268
names(MBNW.rec.sprwt.est) <- c("Year", "Mean.wt", "var.y", "method", "Prop")

#Note since can't get SPR for 2011, 2013, 2014, 2018, 2019, 2022 -- use simple means for these years 
replace.spr <- c(2011, 2013, 2014, 2018, 2019, 2022, 2023)
SPA1B.MBNW.RecWt <- rbind(SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year<2009,],SPA1B.MBNW.RecWt.simple[SPA1B.MBNW.RecWt.simple$Year %in% replace.spr,], MBNW.rec.sprwt.est[!MBNW.rec.sprwt.est$Year %in% replace.spr,])
SPA1B.MBNW.RecWt <- SPA1B.MBNW.RecWt %>% arrange(Year)
SPA1B.MBNW.RecWt

#in 2020 had no survey to linear interpolation from SPR estimate  
approx(SPA1B.MBNW.RecWt$Year, SPA1B.MBNW.RecWt$Mean.wt, xout=2020) #  62.66256
SPA1B.MBNW.RecWt[SPA1B.MBNW.RecWt$Year==2020,c("Mean.wt", "var.y")] <- c(62.66256, 279.13452 ) #assume var from 2019

############# COMMERCIAL (>80mm)
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.CommWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X),Prop = rep(0.268, X))
for (i in 1:length(SPA1B.MBNW.CommWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR == 1996 + i,]
SPA1B.MBNW.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
SPA1B.MBNW.CommWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
}
SPA1B.MBNW.CommWt.simple

#fill in missing years, assume 1997-2002 same as MBNE, interpolate 2004
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==1997, c(2,3)] <- c(642.6624 ,  26871.18 )
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==1998, c(2,3)] <- c( 602.8447,  133741.98)
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==1999, c(2,3)] <- c(1109.8296,  891758.98)
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==2000, c(2,3)] <- c( 491.3838 ,  73897.36)
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==2001, c(2,3)] <- c(1502.6099,  896772.88)
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==2002, c(2,3)] <- c(936.0020,  250651.16)
#2004
approx(SPA1B.MBNW.CommWt.simple$Year, SPA1B.MBNW.CommWt.simple$Mean.wt, xout=2004) # 1339.4545; No tows in W
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==2004,c(2,3)]<- c(1339.454,3716243.47) #var from previous year
#2020
approx(SPA1B.MBNW.CommWt.simple$Year, SPA1B.MBNW.CommWt.simple$Mean.wt, xout=2020) # 3049.43
SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year==2020,c(2,3)]<- c(3049.43,13498845.16) #var from previous year


#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
MBNW.sprwt.est <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009 spr
Wcomwt<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==38],apply(liveweight2008[liveweight2008$STRATA_ID==38,26:52],1,sum),
    liveweight2009$TOW_NO[liveweight2009$STRATA_ID==38],apply(liveweight2009[liveweight2009$STRATA_ID==38,29:52],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wcomwt)  #2331
MBNW.sprwt.est[MBNW.sprwt.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #1865

#2009/2010 spr
Wcomwt1<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==38],apply(liveweight2009[liveweight2009$STRATA_ID==38,26:52],1,sum),
    liveweight2010$TOW_NO[liveweight2010$STRATA_ID==38],apply(liveweight2010[liveweight2010$STRATA_ID==38,29:52],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wcomwt1,summary (Wcomwt))  #1933
MBNW.sprwt.est[MBNW.sprwt.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) #1865

#2010/2011 spr
#Wcomwt2<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==38],apply(liveweight2010[liveweight2010$STRATA_ID==38,29:52],1,sum),liveweight2011$TOW_NO[liveweight2011$STRATA_ID==38],apply(liveweight2011[liveweight2011$STRATA_ID==38,29:52],1,sum),crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#! Only two tows from 2010 repeated in 2011, use simple mean
#MBNW.sprwt.est[MBNW.sprwt.est$Year==2011,c(2:3)] <- c(1863.513,4723900.11)

#2011/2012 spr
Wcomwt3<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==38],apply(liveweight2011[liveweight2011$STRATA_ID==38,26:52],1,sum),
    liveweight2012$TOW_NO[liveweight2012$STRATA_ID==38],apply(liveweight2012[liveweight2012$STRATA_ID==38,29:52],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wcomwt3) #1521
MBNW.sprwt.est[MBNW.sprwt.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
#Wcomwt4<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==38],apply(liveweight2012[liveweight2012$STRATA_ID==38,29:52],1,sum),liveweight2013$TOW_NO[liveweight2013$STRATA_ID==38],apply(liveweight2013[liveweight2013$STRATA_ID==38,29:52],1,sum),crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K<-summary (Wcomwt4) #  1459
#! Only two tows from 2012 repeated in 2013, use simple mean

#MBNW.sprwt.est[MBNW.sprwt.est$Year==2013,c(2:3)] <- c(2021.325,1941257.5)

#2013/2014 spr
#Wcomwt5<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==38],apply(liveweight2013[liveweight2013$STRATA_ID==38,29:52],1,sum),liveweight2014$TOW_NO[liveweight2014$STRATA_ID==38],apply(liveweight2014[liveweight2013$STRATA_ID==38,29:52],1,sum),crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
#K<-summary (Wcomwt5) #
#! Only one repeat in 2014, use simple mean
#MBNW.sprwt.est[MBNW.sprwt.est$Year==2014,c(2:3)] <- c(1877.537,2558952.0)

#2014/2015 spr
Wcomwt6<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==38],apply(liveweight2014[liveweight2014$STRATA_ID==38,29:52],1,sum),liveweight2015$TOW_NO[liveweight2015$STRATA_ID==38],apply(liveweight2015[liveweight2015$STRATA_ID==38,29:52],1,sum),crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wcomwt6) #1363.958
MBNW.sprwt.est[MBNW.sprwt.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
Wcomwt7<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==38],apply(liveweight2015[liveweight2015$STRATA_ID==38,29:52],1,sum),liveweight2016$TOW_NO[liveweight2016$STRATA_ID==38],apply(liveweight2016[liveweight2016$STRATA_ID==38,29:52],1,sum),crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wcomwt7, summary (Wcomwt6)) #1345.650
MBNW.sprwt.est[MBNW.sprwt.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
Wcomwt8<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==38],apply(liveweight2016[liveweight2016$STRATA_ID==38,29:52],1,sum),              liveweight2017$TOW_NO[liveweight2017$STRATA_ID==38],apply(liveweight2017[liveweight2017$STRATA_ID==38,29:52],1,sum),              crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
K<-summary (Wcomwt8,summary (Wcomwt7, summary (Wcomwt6)))#3045.134
MBNW.sprwt.est[MBNW.sprwt.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
# Wcomwt9<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==38],apply(liveweight2017[liveweight2017$STRATA_ID==38,29:52],1,sum),              liveweight2018$TOW_NO[liveweight2018$STRATA_ID==38],apply(liveweight2018[liveweight2018$STRATA_ID==38,29:52],1,sum),                crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==38,c("TOW_NO_REF","TOW_NO")])
# K<-summary (Wcomwt9, summary (Wcomwt8,summary (Wcomwt7, summary (Wcomwt6))))
#NaN; use simple mean
#MBNW.sprwt.est[MBNW.sprwt.est$Year==2018,c(2:3)] <- c(4249.8795, 15534626.73)

#2018/2019 spr
# Wcomwt10<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 38],apply(liveweight2018[liveweight2018$STRATA_ID == 38,29:52],1,sum),
#               liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 38],apply(liveweight2019[liveweight2019$STRATA_ID == 38,29:52],1,sum),
#               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
# K<-summary (Wcomwt10) #NaN use simple mean
#MBNW.sprwt.est[MBNW.sprwt.est$Year==2019,c(2:3)] <- c(2930.0817,  13498838.48)

#2019/2021 spr
Wcomwt11<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 38],apply(liveweight2019[liveweight2019$STRATA_ID == 38,29:52],1,sum),
               liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 38],apply(liveweight2021[liveweight2021$STRATA_ID == 38,29:52],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
 K<-summary(Wcomwt11) 
MBNW.sprwt.est[MBNW.sprwt.est$Year==2021,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr  #only 1 matched 2 -can't use  spr 
#Wcomwt12 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 38],apply(liveweight2021[liveweight2021$STRATA_ID == 38,29:52],1,sum),
#              liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 38],apply(liveweight2022[liveweight2022$STRATA_ID == 38,29:52],1,sum),
#              crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(Wcomwt12) 
#MBNW.sprwt.est[MBNW.sprwt.est$Year==2022,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr -can't use  spr 
#Wcomwt13<-spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 38],apply(liveweight2022[liveweight2022$STRATA_ID == 38,29:52],1,sum),
#              liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 38],apply(liveweight2023[liveweight2023$STRATA_ID == 38,29:52],1,sum),
#              crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K<-summary(Wcomwt13) 
#MBNW.sprwt.est[MBNW.sprwt.est$Year==2023,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr -can't use  spr 
Wcomwt14<-spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 38],apply(liveweight2023[liveweight2023$STRATA_ID == 38,29:52],1,sum),
              liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 38],apply(liveweight2024[liveweight2024$STRATA_ID == 38,29:52],1,sum),
              crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(Wcomwt14) 
MBNW.sprwt.est[MBNW.sprwt.est$Year==2024,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 spr -can't use  spr 
Wcomwt15<-spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 38],apply(liveweight2024[liveweight2024$STRATA_ID == 38,29:52],1,sum),
              liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 38],apply(liveweight2025[liveweight2025$STRATA_ID == 38,29:52],1,sum),
              crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(Wcomwt15) 
MBNW.sprwt.est[MBNW.sprwt.est$Year==2025,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 spr - 
Wcomwt16<-spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 38],apply(liveweight2025[liveweight2025$STRATA_ID == 38,29:52],1,sum),
              liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 38],apply(liveweight2026[liveweight2026$STRATA_ID == 38,29:52],1,sum),
              crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(Wcomwt16) 
MBNW.sprwt.est[MBNW.sprwt.est$Year==2026,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

MBNW.sprwt.est

#make dataframe for all of MBN West Commercial Weight/tow
MBNW.sprwt.est$method <- "spr"
MBNW.sprwt.est$Prop <- 0.268
names(MBNW.sprwt.est) <- c("Year", "Mean.wt", "var.y", "method", "Prop")


#Note since can't get SPR for 2011, 2013, 2014, 2018, 2019, 2022 -- use simple means for these years 
replace.spr <- c(2011, 2013, 2014, 2018, 2019, 2022, 2023)
SPA1B.MBNW.CommWt <- rbind(SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year<2009,],SPA1B.MBNW.CommWt.simple[SPA1B.MBNW.CommWt.simple$Year %in% replace.spr,], MBNW.sprwt.est[!MBNW.sprwt.est$Year %in% replace.spr,])
SPA1B.MBNW.CommWt <- SPA1B.MBNW.CommWt %>% arrange(Year)
SPA1B.MBNW.CommWt

#in 2020 had no survey to linear interpolation from SPR estimate  
approx(SPA1B.MBNW.CommWt$Year, SPA1B.MBNW.CommWt$Mean.wt, xout=2020) #   2828.67
SPA1B.MBNW.CommWt[SPA1B.MBNW.CommWt$Year==2020,c("Mean.wt", "var.y")] <- c( 2828.67, 13498845.16 ) #assume var from 2019


#Calculate survey index for MBN

#Recruit

years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.RecWt$prop.mean <- SPA1B.MBNW.RecWt$Prop*SPA1B.MBNW.RecWt$Mean.wt
SPA1B.MBNE.RecWt$prop.mean <- SPA1B.MBNE.RecWt$Prop*SPA1B.MBNE.RecWt$Mean.wt

SPA1B.MBN.RecWt <- data.frame(Year=years, Mean.wt=rep(NA,X), var.y=rep(NA,X), method=rep(NA,X), NH=rep (294145.4,X), Area=rep("MBN", X), Age=rep("Recruit", X)) #

SPA1B.MBN.RecWt$Mean.wt <- SPA1B.MBNW.RecWt$prop.mean + SPA1B.MBNE.RecWt$prop.mean
SPA1B.MBN.RecWt$var.y <- (SPA1B.MBNW.RecWt$Prop*SPA1B.MBNW.RecWt$var.y) + (SPA1B.MBNE.RecWt$Prop*SPA1B.MBNE.RecWt$var.y)

SPA1B.MBN.RecWt$cv <- sqrt(SPA1B.MBN.RecWt$var.y)/SPA1B.MBN.RecWt$Mean.wt
SPA1B.MBN.RecWt$kg.tow <- SPA1B.MBN.RecWt$Mean.wt/1000
SPA1B.MBN.RecWt$Bmass <- (SPA1B.MBN.RecWt$kg.tow*SPA1B.MBN.RecWt$NH)/1000
SPA1B.MBN.RecWt$method <- SPA1B.MBNW.RecWt$method #MBW starts with SPR one year before MBE 

#Commercial
years <- 1997:surveyyear
X <- length(years)

SPA1B.MBNW.CommWt$prop.mean <- SPA1B.MBNW.CommWt$Prop*SPA1B.MBNW.CommWt$Mean.wt
SPA1B.MBNE.CommWt$prop.mean <- SPA1B.MBNE.CommWt$Prop*SPA1B.MBNE.CommWt$Mean.wt

SPA1B.MBN.CommWt <- data.frame(Year=years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep(NA, X), NH = rep (294145.4,X), Area = rep("MBN", X), Age = rep("Commercial", X)) 

SPA1B.MBN.CommWt$Mean.wt <- SPA1B.MBNW.CommWt$prop.mean+SPA1B.MBNE.CommWt$prop.mean
SPA1B.MBN.CommWt$var.y <- (SPA1B.MBNW.CommWt$Prop*SPA1B.MBNW.CommWt$var.y)+(SPA1B.MBNE.CommWt$Prop*SPA1B.MBNE.CommWt$var.y)

SPA1B.MBN.CommWt$cv <- sqrt(SPA1B.MBN.CommWt$var.y)/SPA1B.MBN.CommWt$Mean.wt
SPA1B.MBN.CommWt$kg.tow <- SPA1B.MBN.CommWt$Mean.wt/1000
SPA1B.MBN.CommWt$Bmass <- (SPA1B.MBN.CommWt$kg.tow*SPA1B.MBN.CommWt$NH)/1000
SPA1B.MBN.CommWt$method <- SPA1B.MBNW.CommWt$method #SPA1B.MBNE.CommWt started SPR in 2010,SPA1B.MBNW.CommWt started SPR in 2009, should work for method   
SPA1B.MBN.CommWt$method[SPA1B.MBN.CommWt$Year >= 2009] <- "spr"  #SPA1B.MBNE.CommWt started SPR in 2010,SPA1B.MBNW.CommWt started SPR in 2009,  but some year of s SPA1B.MBNW.CommWt had to use simple mean... so just set 

#combine Recruit and Commercial dataframes
SPA1B.MBN.Weight <- rbind(SPA1B.MBN.RecWt, SPA1B.MBN.CommWt)


####
# ---- 3. Upper Bay 28C (spr) ----
####

############Recruit (65-79)
years <- 2001:surveyyear
X <- length(years)

SPA1B.28C.RecWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(88000.2, X), Area = rep("28C",X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.28C.RecWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR == 2000 + i,]
SPA1B.28C.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
SPA1B.28C.RecWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
}
SPA1B.28C.RecWt.simple

#assume 1997:2000 was the same as MBN
SPA1B.28C.RecWt.simple <- rbind(SPA1B.MBN.RecWt[SPA1B.MBN.RecWt$Year<2001, c(1:7)], SPA1B.28C.RecWt.simple)
SPA1B.28C.RecWt.simple[SPA1B.28C.RecWt.simple$Year<2001, "NH"] <- 88000.2
SPA1B.28C.RecWt.simple[SPA1B.28C.RecWt.simple$Year<2001, "Area"] <- "28C"

#interpolate 2004
approx(SPA1B.28C.RecWt.simple$Year, SPA1B.28C.RecWt.simple$Mean.wt, xout=2004)#39.92428
SPA1B.28C.RecWt.simple[SPA1B.28C.RecWt.simple$Year==2004,c(2,3)]<-c(39.92428,4303.08215) #prev year var 

#interpolate 2020
approx(SPA1B.28C.RecWt.simple$Year, SPA1B.28C.RecWt.simple$Mean.wt, xout=2020)#98.32263
SPA1B.28C.RecWt.simple[SPA1B.28C.RecWt.simple$Year==2020,c(2,3)]<-c(98.32263,182367.11395) #since sig larger value use 2020 year var

#SPR for recruit
#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
C28.rec.wt.spr.est <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009 spr
c28recwt1<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==53],apply(liveweight2008[liveweight2008$STRATA_ID==53,23:25],1,sum),
 liveweight2009$TOW_NO[liveweight2009$STRATA_ID==53],apply(liveweight2009[liveweight2009$STRATA_ID==53,26:28],1,sum),
 crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary(c28recwt1)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
c28recwt2<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==53],apply(liveweight2009[liveweight2009$STRATA_ID==53,23:25],1,sum),
 liveweight2010$TOW_NO[liveweight2010$STRATA_ID==53],apply(liveweight2010[liveweight2010$STRATA_ID==53,26:28],1,sum),
 crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary(c28recwt2)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
c28recwt3<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==53],apply(liveweight2010[liveweight2010$STRATA_ID==53,23:25],1,sum),
    liveweight2011$TOW_NO[liveweight2011$STRATA_ID==53],apply(liveweight2011[liveweight2011$STRATA_ID==53,26:28],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary(c28recwt3)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
c28recwt4<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==53],apply(liveweight2011[liveweight2011$STRATA_ID==53,23:25],1,sum),
liveweight2012$TOW_NO[liveweight2012$STRATA_ID==53],apply(liveweight2012[liveweight2012$STRATA_ID==53,26:28],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary ( c28recwt4)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
c28recwt5<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==53],apply(liveweight2012[liveweight2012$STRATA_ID==53,23:25],1,sum),
liveweight2013$TOW_NO[liveweight2013$STRATA_ID==53],apply(liveweight2013[liveweight2013$STRATA_ID==53,26:28],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28recwt5)  #
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
c28recwt6<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==53],apply(liveweight2013[liveweight2013$STRATA_ID==53,23:25],1,sum),
liveweight2014$TOW_NO[liveweight2014$STRATA_ID==53],apply(liveweight2014[liveweight2014$STRATA_ID==53,26:28],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary(c28recwt6) #
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
c28recwt7<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==53],apply(liveweight2014[liveweight2014$STRATA_ID==53,23:25],1,sum),
liveweight2015$TOW_NO[liveweight2015$STRATA_ID==53],apply(liveweight2015[liveweight2015$STRATA_ID==53,26:28],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary(c28recwt7)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
c28recwt8<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==53],apply(liveweight2015[liveweight2015$STRATA_ID==53,23:25],1,sum),             liveweight2016$TOW_NO[liveweight2016$STRATA_ID==53],apply(liveweight2016[liveweight2016$STRATA_ID==53,26:28],1,sum),             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary(c28recwt8)#29.63570
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
c28recwt9<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==53],apply(liveweight2016[liveweight2016$STRATA_ID==53,23:25],1,sum),             liveweight2017$TOW_NO[liveweight2017$STRATA_ID==53],apply(liveweight2017[liveweight2017$STRATA_ID==53,26:28],1,sum),             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary(c28recwt9)#23.64268
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
c28recwt10<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==53],apply(liveweight2017[liveweight2017$STRATA_ID==53,23:25],1,sum),              liveweight2018$TOW_NO[liveweight2018$STRATA_ID==53],apply(liveweight2018[liveweight2018$STRATA_ID==53,26:28],1,sum),              crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])
K<-summary(c28recwt10)#54.64505
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
c28recwt11 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 53],apply(liveweight2018[liveweight2018$STRATA_ID == 53,23:25],1,sum),
                  liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 53],apply(liveweight2019[liveweight2019$STRATA_ID == 53,26:28],1,sum),
                  crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28recwt11)#37.04099
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 spr
c28recwt12 <- spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 53],apply(liveweight2019[liveweight2019$STRATA_ID == 53,23:25],1,sum),
                  liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 53],apply(liveweight2021[liveweight2021$STRATA_ID == 53,26:28],1,sum),
                  crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28recwt12)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr
c28recwt13 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 53],apply(liveweight2021[liveweight2021$STRATA_ID == 53,23:25],1,sum),
                  liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 53],apply(liveweight2022[liveweight2022$STRATA_ID == 53,26:28],1,sum),
                  crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28recwt13)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr
c28recwt14 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 53],apply(liveweight2022[liveweight2022$STRATA_ID == 53,23:25],1,sum),
                  liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 53],apply(liveweight2023[liveweight2023$STRATA_ID == 53,26:28],1,sum),
                  crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28recwt14)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr
c28recwt15 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 53],apply(liveweight2023[liveweight2023$STRATA_ID == 53,23:25],1,sum),
                  liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 53],apply(liveweight2024[liveweight2024$STRATA_ID == 53,26:28],1,sum),
                  crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28recwt15)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 spr
c28recwt16 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 53],apply(liveweight2024[liveweight2024$STRATA_ID == 53,23:25],1,sum),
                  liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 53],apply(liveweight2025[liveweight2025$STRATA_ID == 53,26:28],1,sum),
                  crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28recwt16)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 spr
c28recwt17 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 53],apply(liveweight2025[liveweight2025$STRATA_ID == 53,23:25],1,sum),
                  liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 53],apply(liveweight2026[liveweight2026$STRATA_ID == 53,26:28],1,sum),
                  crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28recwt17)
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

C28.rec.wt.spr.est

#interpolate 2020
approx(C28.rec.wt.spr.est$Year, C28.rec.wt.spr.est$Yspr, xout=2020)#159.2217
C28.rec.wt.spr.est[C28.rec.wt.spr.est$Year==2020,c("Yspr","var.Yspr.corrected")]<-c(159.2217, 2195.80898) #since large - use 2020 year var 

#make dataframe for all of 28C Upper Bay Commercial Biomass
C28.rec.wt.spr.est$method <- "spr"
C28.rec.wt.spr.est$NH <- 88000.2
C28.rec.wt.spr.est$Area <- "28C"
C28.rec.wt.spr.est$Age <- "Recruit"
names(C28.rec.wt.spr.est) <- c("Year", "Mean.wt", "var.y", "method", "NH", "Area", "Age")

SPA1B.28C.RecWt <- rbind(SPA1B.28C.RecWt.simple[SPA1B.28C.RecWt.simple$Year<2009,], C28.rec.wt.spr.est)
SPA1B.28C.RecWt$cv <- sqrt(SPA1B.28C.RecWt$var.y)/SPA1B.28C.RecWt$Mean.wt
SPA1B.28C.RecWt$kg.tow <- SPA1B.28C.RecWt$Mean.wt/1000
SPA1B.28C.RecWt$Bmass <- (SPA1B.28C.RecWt$kg.tow*SPA1B.28C.RecWt$NH)/1000

############Commercial(80+)
years <-2001:surveyyear
X <- length(years)

SPA1B.28C.CommWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(88000.2,X), Area = rep("28C", X), Age = rep("Commercial",X))
for (i in 1:length(SPA1B.28C.CommWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR==2000 + i,]
SPA1B.28C.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
SPA1B.28C.CommWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
}
SPA1B.28C.CommWt.simple

#assume 1997:2000 was the same as MBN
SPA1B.28C.CommWt.simple <- rbind(SPA1B.MBN.CommWt[SPA1B.MBN.CommWt$Year <2001, c(1:7)], SPA1B.28C.CommWt.simple)
SPA1B.28C.CommWt.simple[SPA1B.28C.CommWt.simple$Year<2001, "NH"] <- 88000.2
SPA1B.28C.CommWt.simple[SPA1B.28C.CommWt.simple$Year<2001, "Area"] <- "28C"

#interpolate 2004
approx(SPA1B.28C.CommWt.simple$Year, SPA1B.28C.CommWt.simple$Mean.wt, xout=2004)#1404.4
SPA1B.28C.CommWt.simple[SPA1B.28C.CommWt.simple$Year==2004, c("Mean.wt","var.y")]<-c(1404.4,4005128.6) #prev year var

#interpolate 2020
approx(SPA1B.28C.CommWt.simple$Year, SPA1B.28C.CommWt.simple$Mean.wt, xout=2020)#925.607
SPA1B.28C.CommWt.simple[SPA1B.28C.CommWt.simple$Year==2020, c("Mean.wt","var.y")]<-c(925.607,4033630.0) #prev year var

#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
C28.wt.spr.est <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009 spr
c28commcf1<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==53],apply(liveweight2008[liveweight2008$STRATA_ID==53,26:52],1,sum),
 liveweight2009$TOW_NO[liveweight2009$STRATA_ID==53],apply(liveweight2009[liveweight2009$STRATA_ID==53,29:52],1,sum),
 crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary(c28commcf1)       # 1506
C28.wt.spr.est[C28.wt.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
c28commcf2<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==53],apply(liveweight2009[liveweight2009$STRATA_ID==53,26:52],1,sum), liveweight2010$TOW_NO[liveweight2010$STRATA_ID==53],apply(liveweight2010[liveweight2010$STRATA_ID==53,29:52],1,sum),
 crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary(c28commcf2,summary(c28commcf1))         # 1225.345
C28.wt.spr.est[C28.wt.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
c28commcf3<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==53],apply(liveweight2010[liveweight2010$STRATA_ID==53,26:52],1,sum),    liveweight2011$TOW_NO[liveweight2011$STRATA_ID==53],apply(liveweight2011[liveweight2011$STRATA_ID==53,29:52],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))      #1393.096
C28.wt.spr.est[C28.wt.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
c28commcf4<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==53],apply(liveweight2011[liveweight2011$STRATA_ID==53,26:52],1,sum),liveweight2012$TOW_NO[liveweight2012$STRATA_ID==53],apply(liveweight2012[liveweight2012$STRATA_ID==53,29:52],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary ( c28commcf4,summary ( c28commcf3, summary( c28commcf2,summary(c28commcf1))))   #1203.334
C28.wt.spr.est[C28.wt.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
c28commcf5<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==53],apply(liveweight2012[liveweight2012$STRATA_ID==53,26:52],1,sum),liveweight2013$TOW_NO[liveweight2013$STRATA_ID==53],apply(liveweight2013[liveweight2013$STRATA_ID==53,29:52],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28commcf5,summary ( c28commcf4,summary ( c28commcf3, summary( c28commcf2,summary(c28commcf1)))))  #1557.523
C28.wt.spr.est[C28.wt.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
c28commcf6<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==53],apply(liveweight2013[liveweight2013$STRATA_ID==53,26:52],1,sum),liveweight2014$TOW_NO[liveweight2014$STRATA_ID==53],apply(liveweight2014[liveweight2014$STRATA_ID==53,29:52],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))))) # 1235.780
C28.wt.spr.est[C28.wt.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
c28commcf7<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==53],apply(liveweight2014[liveweight2014$STRATA_ID==53,26:52],1,sum),liveweight2015$TOW_NO[liveweight2015$STRATA_ID==53],apply(liveweight2015[liveweight2015$STRATA_ID==53,29:52],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))))))  #1442.174
C28.wt.spr.est[C28.wt.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
c28commcf8<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==53],apply(liveweight2015[liveweight2015$STRATA_ID==53,26:52],1,sum),              liveweight2016$TOW_NO[liveweight2016$STRATA_ID==53],apply(liveweight2016[liveweight2016$STRATA_ID==53,29:52],1,sum),              crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1))))))))  #749.0580
C28.wt.spr.est[C28.wt.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
c28commcf9<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==53],apply(liveweight2016[liveweight2016$STRATA_ID==53,26:52],1,sum),              liveweight2017$TOW_NO[liveweight2017$STRATA_ID==53],apply(liveweight2017[liveweight2017$STRATA_ID==53,29:52],1,sum),              crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28commcf9,summary (c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))))))))  #762.6556
C28.wt.spr.est[C28.wt.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
c28commcf10<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==53],apply(liveweight2017[liveweight2017$STRATA_ID==53,26:52],1,sum),               liveweight2018$TOW_NO[liveweight2018$STRATA_ID==53],apply(liveweight2018[liveweight2018$STRATA_ID==53,29:52],1,sum),               crossref.BoF.2018[crossref.BoF.2018$STRATA_ID==53,c("TOW_NO_REF","TOW_NO")])

K<-summary (c28commcf10, summary (c28commcf9,summary (c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1))))))))))  # 949.8090
C28.wt.spr.est[C28.wt.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
c28commcf11 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 53],apply(liveweight2018[liveweight2018$STRATA_ID == 53,26:52],1,sum),
                   liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 53],apply(liveweight2019[liveweight2019$STRATA_ID == 53,29:52],1,sum),
                   crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28commcf11 , summary(c28commcf10, summary(c28commcf9,summary(c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))))))))))  # 703.9979
C28.wt.spr.est[C28.wt.spr.est$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 spr
c28commcf12 <- spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 53],apply(liveweight2019[liveweight2019$STRATA_ID == 53,26:52],1,sum),
                   liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 53],apply(liveweight2021[liveweight2021$STRATA_ID == 53,29:52],1,sum),
                   crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28commcf12, summary(c28commcf11 , summary(c28commcf10, summary(c28commcf9,summary(c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1))))))))))))  # 
C28.wt.spr.est[C28.wt.spr.est$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 spr
c28commcf13 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 53],apply(liveweight2021[liveweight2021$STRATA_ID == 53,26:52],1,sum),
                   liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 53],apply(liveweight2022[liveweight2022$STRATA_ID == 53,29:52],1,sum),
                   crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28commcf13, summary(c28commcf12, summary(c28commcf11 , summary(c28commcf10, summary(c28commcf9,summary(c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))))))))))))  # 
C28.wt.spr.est[C28.wt.spr.est$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 spr
c28commcf14 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 53],apply(liveweight2022[liveweight2022$STRATA_ID == 53,26:52],1,sum),
                   liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 53],apply(liveweight2023[liveweight2023$STRATA_ID == 53,29:52],1,sum),
                   crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28commcf14,summary(c28commcf13, summary(c28commcf12, summary(c28commcf11 , summary(c28commcf10, summary(c28commcf9,summary(c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1))))))))))))))
C28.wt.spr.est[C28.wt.spr.est$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 spr
c28commcf15 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 53],apply(liveweight2023[liveweight2023$STRATA_ID == 53,26:52],1,sum),
                   liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 53],apply(liveweight2024[liveweight2024$STRATA_ID == 53,29:52],1,sum),
                   crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28commcf15,summary(c28commcf14,summary(c28commcf13, summary(c28commcf12, summary(c28commcf11 , summary(c28commcf10, summary(c28commcf9,summary(c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))))))))))))))
C28.wt.spr.est[C28.wt.spr.est$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 spr
c28commcf16 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 53],apply(liveweight2024[liveweight2024$STRATA_ID == 53,26:52],1,sum),
                   liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 53],apply(liveweight2025[liveweight2025$STRATA_ID == 53,29:52],1,sum),
                   crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28commcf16,summary(c28commcf15,summary(c28commcf14,summary(c28commcf13, summary(c28commcf12, summary(c28commcf11 , summary(c28commcf10, summary(c28commcf9,summary(c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1))))))))))))))))
C28.wt.spr.est[C28.wt.spr.est$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 spr
c28commcf17 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 53],apply(liveweight2025[liveweight2025$STRATA_ID == 53,26:52],1,sum),
                   liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 53],apply(liveweight2026[liveweight2026$STRATA_ID == 53,29:52],1,sum),
                   crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(c28commcf17,summary(c28commcf16,summary(c28commcf15,summary(c28commcf14,summary(c28commcf13, summary(c28commcf12, summary(c28commcf11 , summary(c28commcf10, summary(c28commcf9,summary(c28commcf8,summary(c28commcf7,summary(c28commcf6,summary(c28commcf5,summary(c28commcf4,summary(c28commcf3,summary(c28commcf2,summary(c28commcf1)))))))))))))))))
C28.wt.spr.est[C28.wt.spr.est$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

C28.wt.spr.est

#interpolate 2020
approx(C28.wt.spr.est$Year, C28.wt.spr.est$Yspr, xout=2020)#746.6088
C28.wt.spr.est[C28.wt.spr.est$Year==2020, c("Yspr","var.Yspr.corrected")]<-c(746.6088,78178.46) #prev year var

#make dataframe for all of 28C Upper Bay Commercial Biomass
C28.wt.spr.est$method <- "spr"
C28.wt.spr.est$NH <- 88000.2
C28.wt.spr.est$Area <- "28C"
C28.wt.spr.est$Age <- "Commercial"
names(C28.wt.spr.est) <- c("Year", "Mean.wt", "var.y", "method", "NH", "Area", "Age")

SPA1B.28C.CommWt <- rbind(SPA1B.28C.CommWt.simple[SPA1B.28C.CommWt.simple$Year < 2009,], C28.wt.spr.est)
SPA1B.28C.CommWt$cv <- sqrt(SPA1B.28C.CommWt$var.y)/SPA1B.28C.CommWt$Mean.wt
SPA1B.28C.CommWt$kg.tow <- SPA1B.28C.CommWt$Mean.wt/1000
SPA1B.28C.CommWt$Bmass <- (SPA1B.28C.CommWt$kg.tow*SPA1B.28C.CommWt$NH)/1000

#combine Recruit and Commercial dataframes
SPA1B.28C.Weight <- rbind(SPA1B.28C.RecWt, SPA1B.28C.CommWt)

####
#---- 4. 28D Outer Bay ----
####

############Recruit (65-79)
years <- 2001:surveyyear
X <- length(years)

SPA1B.Out.RecWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(56009.89, X),Area = rep("Out",X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.Out.RecWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR == 2000 + i,]
SPA1B.Out.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
SPA1B.Out.RecWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
}
SPA1B.Out.RecWt.simple

#assume 1997:2000 was the same as MBN
SPA1B.Out.RecWt.simple <- rbind(SPA1B.MBN.RecWt[SPA1B.MBN.RecWt$Year<2001, c(1:7)], SPA1B.Out.RecWt.simple)
SPA1B.Out.RecWt.simple[SPA1B.Out.RecWt.simple$Year<2001, "NH"] <- 56009.89
SPA1B.Out.RecWt.simple[SPA1B.Out.RecWt.simple$Year<2001, "Area"] <- "Out"

#interpolate 2004
approx(SPA1B.Out.RecWt.simple$Year, SPA1B.Out.RecWt.simple$Mean.wt, xout=2004)#2.828022
SPA1B.Out.RecWt.simple[SPA1B.Out.RecWt.simple$Year==2004,c("Mean.wt","var.y")] <- c(2.828022,47.73848)# var from 2003 since 0 in 2003 

#interpolate 2020
approx(SPA1B.Out.RecWt.simple$Year, SPA1B.Out.RecWt.simple$Mean.wt, xout=2020)#48.19643
SPA1B.Out.RecWt.simple[SPA1B.Out.RecWt.simple$Year==2020,c("Mean.wt","var.y")] <- c(48.19643,28371.98290) #larger so take 2021 var 2019 too small 


#spr for recruits  !NB use simple mean for Outer 28C

#! No usable spr years for rec from 2010-2017
spryears <- 2018:surveyyear  #update to most recent year
Y <- length(spryears)
Out.rec.wt.spr.est <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009 spr
#Only 2 tows

#2009/2010 spr
# OUTrecwt2<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==49],apply(liveweight2009[liveweight2009$STRATA_ID==49,23:25],1,sum),
#     liveweight2010$TOW_NO[liveweight2010$STRATA_ID==49],apply(liveweight2010[liveweight2010$STRATA_ID==49,26:28],1,sum),
#     crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
# #NAN
# Out.rec.wt.spr.est [Out.rec.wt.spr.est $Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
# OUTrecwt3<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==49],apply(liveweight2010[liveweight2010$STRATA_ID==49,23:25],1,sum),
#     liveweight2011$TOW_NO[liveweight2011$STRATA_ID==49],apply(liveweight2011[liveweight2011$STRATA_ID==49,26:28],1,sum),
#     crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
#SD is zero
#Out.rec.wt.spr.est [Out.rec.wt.spr.est $Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
# OUTrecwt4<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==49],apply(liveweight2011[liveweight2011$STRATA_ID==49,23:25],1,sum),
#     liveweight2012$TOW_NO[liveweight2012$STRATA_ID==49],apply(liveweight2012[liveweight2012$STRATA_ID==49,26:28],1,sum),
#      crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
#SD is zero
#Out.rec.wt.spr.est [Out.rec.wt.spr.est $Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
# OUTrecwt5<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==49],apply(liveweight2012[liveweight2012$STRATA_ID==49,23:25],1,sum),
#     liveweight2013$TOW_NO[liveweight2013$STRATA_ID==49],apply(liveweight2013[liveweight2013$STRATA_ID==49,26:28],1,sum),
#      crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
#SD is zero
#Out.rec.wt.spr.est [Out.rec.wt.spr.est $Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
# OUTrecwt6<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==49],apply(liveweight2013[liveweight2013$STRATA_ID==49,23:25],1,sum),
#     liveweight2014$TOW_NO[liveweight2014$STRATA_ID==49],apply(liveweight2014[liveweight2014$STRATA_ID==49,29:52],1,sum),
#      crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
#Out.rec.wt.spr.est [Out.rec.wt.spr.est $Year==2014,c(2:3)] <- c(941.75222, 1879785.07)# only two tows; use simple mean

#2014/2015
# OUTrecwt7<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==49],apply(liveweight2014[liveweight2014$STRATA_ID==49,23:25],1,sum),
#     liveweight2015$TOW_NO[liveweight2015$STRATA_ID==49],apply(liveweight2015[liveweight2015$STRATA_ID==49,26:28],1,sum),
#      crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
#SD is zero  #
#Out.rec.wt.spr.est [Out.rec.wt.spr.est $Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
# OUTrecwt9<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==49],apply(liveweight2016[liveweight2016$STRATA_ID==49,23:25],1,sum),             liveweight2017$TOW_NO[liveweight2017$STRATA_ID==49],apply(liveweight2017[liveweight2017$STRATA_ID==49,26:28],1,sum),             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
# Out.rec.wt.spr.est [Out.rec.wt.spr.est $Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#essentially perfect fit: summary may be unreliable

#2017/2018
OUTrecwt10 <- spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID == 49],apply(liveweight2017[liveweight2017$STRATA_ID == 49,23:25],1,sum),
                  liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 49],apply(liveweight2018[liveweight2018$STRATA_ID == 49,26:28],1,sum),
                  crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTrecwt10) #13.613417

Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
OUTrecwt11 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 49],apply(liveweight2018[liveweight2018$STRATA_ID == 49,23:25],1,sum),
                  liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 49],apply(liveweight2019[liveweight2019$STRATA_ID == 49,26:28],1,sum),
                  crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTrecwt11) # 1.946924

Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #Can't use bc of zeros 
#OUTrecwt12 <- spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 49],apply(liveweight2019[liveweight2019$STRATA_ID == 49,23:25],1,sum),
#                  liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 49],apply(liveweight2021[liveweight2021$STRATA_ID == 49,26:28],1,sum),
#                  crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
#K <- summary(OUTrecwt12) # 1.946924
#Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
OUTrecwt13 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 49],apply(liveweight2021[liveweight2021$STRATA_ID == 49,23:25],1,sum),
                  liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 49],apply(liveweight2022[liveweight2022$STRATA_ID == 49,26:28],1,sum),
                  crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTrecwt13) 
Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
OUTrecwt14 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 49],apply(liveweight2022[liveweight2022$STRATA_ID == 49,23:25],1,sum),
                  liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 49],apply(liveweight2023[liveweight2023$STRATA_ID == 49,26:28],1,sum),
                  crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTrecwt14) 
Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
OUTrecwt15 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 49],apply(liveweight2023[liveweight2023$STRATA_ID == 49,23:25],1,sum),
                  liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 49],apply(liveweight2024[liveweight2024$STRATA_ID == 49,26:28],1,sum),
                  crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTrecwt15) 
Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 #Can't use bc of zeros
#OUTrecwt16 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 49],apply(liveweight2024[liveweight2024$STRATA_ID == 49,23:25],1,sum),
#                  liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 49],apply(liveweight2025[liveweight2025$STRATA_ID == 49,26:28],1,sum),
#                  crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
#K <- summary(OUTrecwt16) 
#Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

# See if tows assocaited with given year were all zero or not 
#test <- liveweight2024 %>% filter(TOW_NO %in% (crossref.BoF.2025$TOW_NO_REF[crossref.BoF.2025$STRATA_ID==49])) 
#apply(test[test$STRATA_ID==49,23:25],1,sum)

#2025/2026 #Can't use bc of zeros
OUTrecwt17 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 49],apply(liveweight2025[liveweight2025$STRATA_ID == 49,23:25],1,sum),
                  liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 49],apply(liveweight2026[liveweight2026$STRATA_ID == 49,26:28],1,sum),
                  crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTrecwt17) 
Out.rec.wt.spr.est[Out.rec.wt.spr.est$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Out.rec.wt.spr.est

#make dataframe for all of Outer 28D Outer Recruit Biomass
Out.rec.wt.spr.est$method <- "spr"
Out.rec.wt.spr.est$NH <- 56009.89
Out.rec.wt.spr.est$Area <- "Out"
Out.rec.wt.spr.est$Age <- "Recruit"
names(Out.rec.wt.spr.est) <- c("Year", "Mean.wt", "var.y", "method", "NH", "Area", "Age")


#Note since can't get SPR for 2020,2021 -- can rarely get spr to work ever on recruits in this area - changed in 2021 to just use simple mean for all year for this area: 
#replace.spr <- c(2020, 2021)
#SPA1B.Out.RecWt <- rbind(SPA1B.Out.RecWt.simple[SPA1B.Out.RecWt.simple$Year<2018,],SPA1B.Out.RecWt.simple[SPA1B.Out.RecWt.simple$Year %in% replace.spr,], Out.rec.wt.spr.est[!Out.rec.wt.spr.est$Year %in% replace.spr,])
#SPA1B.Out.RecWt <- SPA1B.Out.RecWt %>% arrange(Year)
#SPA1B.Out.RecWt

#SPA1B.Out.RecWt <- rbind(SPA1B.Out.RecWt.simple[SPA1B.Out.RecWt.simple$Year < 2018,], Out.rec.wt.spr.est)
SPA1B.Out.RecWt <- SPA1B.Out.RecWt.simple #as of 2022 -- since no major difference in esimators - easier to deal with cvs 
SPA1B.Out.RecWt$cv <- sqrt(SPA1B.Out.RecWt$var.y)/SPA1B.Out.RecWt$Mean.wt
SPA1B.Out.RecWt$kg.tow <- SPA1B.Out.RecWt$Mean.wt/1000
SPA1B.Out.RecWt$Bmass <- (SPA1B.Out.RecWt$kg.tow*SPA1B.Out.RecWt$NH)/1000


############Commercial (80+ mm)
years <- 2001:surveyyear
X <- length(years)

SPA1B.Out.CommWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(56009.89, X),Area = rep("Out",X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.Out.CommWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR == 2000 + i,]
SPA1B.Out.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
SPA1B.Out.CommWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
}
SPA1B.Out.CommWt.simple

#assume 1997:2000 was the same as MBN
SPA1B.Out.CommWt.simple <- rbind(SPA1B.MBN.CommWt[SPA1B.MBN.CommWt$Year < 2001, c(1:7)], SPA1B.Out.CommWt.simple)
SPA1B.Out.CommWt.simple[SPA1B.Out.CommWt.simple$Year < 2001, "NH"] <- 56009.89
SPA1B.Out.CommWt.simple[SPA1B.Out.CommWt.simple$Year < 2001, "Area"] <- "Out"

#interpolate for 2004
approx(SPA1B.Out.CommWt.simple$Year, SPA1B.Out.CommWt.simple$Mean.wt, xout = 2004)#1370.434
SPA1B.Out.CommWt.simple [SPA1B.Out.CommWt.simple$Year==2004,c(2,3)] <- c(1370.434,6385405.834) #prev year var

#interpolate for 2020
approx(SPA1B.Out.CommWt.simple$Year, SPA1B.Out.CommWt.simple$Mean.wt, xout = 2020)#419.052
SPA1B.Out.CommWt.simple [SPA1B.Out.CommWt.simple$Year==2020,c(2,3)] <- c(419.052,852388.916) #since large took from 2020 year var


#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear #update to most recent year
Y <- length(spryears)
Out.wt.spr.est <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009 spr
#Only 2 tows

#2009/2010 spr
OUTcommcf2<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==49],apply(liveweight2009[liveweight2009$STRATA_ID==49,26:52],1,sum),
    liveweight2010$TOW_NO[liveweight2010$STRATA_ID==49],apply(liveweight2010[liveweight2010$STRATA_ID==49,29:52],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcommcf2)   #68.21
Out.wt.spr.est[Out.wt.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
OUTcommcf3<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==49],apply(liveweight2010[liveweight2010$STRATA_ID==49,26:52],1,sum),
    liveweight2011$TOW_NO[liveweight2011$STRATA_ID==49],apply(liveweight2011[liveweight2011$STRATA_ID==49,29:52],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcommcf3, summary (OUTcommcf2))#457.5
Out.wt.spr.est[Out.wt.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
OUTcommcf4<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==49],apply(liveweight2011[liveweight2011$STRATA_ID==49,26:52],1,sum),
    liveweight2012$TOW_NO[liveweight2012$STRATA_ID==49],apply(liveweight2012[liveweight2012$STRATA_ID==49,29:52],1,sum),
     crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcommcf4, summary (OUTcommcf3,summary (OUTcommcf2)))  #383.4
Out.wt.spr.est[Out.wt.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
OUTcommcf5<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==49],apply(liveweight2012[liveweight2012$STRATA_ID==49,26:52],1,sum),
    liveweight2013$TOW_NO[liveweight2013$STRATA_ID==49],apply(liveweight2013[liveweight2013$STRATA_ID==49,29:52],1,sum),
     crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcommcf5,summary (OUTcommcf4, summary (OUTcommcf3,summary (OUTcommcf2))))  #259
Out.wt.spr.est[Out.wt.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
#OUTcommcf6<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==49],apply(liveweight2013[liveweight2013$STRATA_ID==49,26:52],1,sum),
#    liveweight2014$TOW_NO[liveweight2014$STRATA_ID==49],apply(liveweight2014[liveweight2014$STRATA_ID==49,29:52],1,sum),
#     crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
#K<-summary (OUTcommcf6, summary (OUTcommcf5,summary (OUTcommcf4, summary (OUTcommcf3,summary (OUTcommcf2)))))  #
#Out.wt.spr.est[Out.wt.spr.est$Year==2014,c(2:3)] <- c(941.75222, 1879785.07)# only two tows; use simple mean

#2014/2015
OUTcommcf7<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==49],apply(liveweight2014[liveweight2014$STRATA_ID==49,26:52],1,sum),
    liveweight2015$TOW_NO[liveweight2015$STRATA_ID==49],apply(liveweight2015[liveweight2015$STRATA_ID==49,29:52],1,sum),
     crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcommcf7)  #434.21828
Out.wt.spr.est[Out.wt.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
OUTcommcf8<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==49],apply(liveweight2015[liveweight2015$STRATA_ID==49,26:52],1,sum),              liveweight2016$TOW_NO[liveweight2016$STRATA_ID==49],apply(liveweight2016[liveweight2016$STRATA_ID==49,29:52],1,sum),              crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcommcf8, summary (OUTcommcf7))  #333.80402
Out.wt.spr.est[Out.wt.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
OUTcommcf9<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==49],apply(liveweight2016[liveweight2016$STRATA_ID==49,26:52],1,sum),              liveweight2017$TOW_NO[liveweight2017$STRATA_ID==49],apply(liveweight2017[liveweight2017$STRATA_ID==49,29:52],1,sum),              crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==49,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTcommcf9,summary (OUTcommcf8, summary (OUTcommcf7)))  #413.06056
Out.wt.spr.est[Out.wt.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
OUTcommcf10 <- spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID == 49],apply(liveweight2017[liveweight2017$STRATA_ID == 49,26:52],1,sum),
                   liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 49],apply(liveweight2018[liveweight2018$STRATA_ID == 49,29:52],1,sum),
                   crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcommcf10, summary(OUTcommcf9, summary(OUTcommcf8, summary(OUTcommcf7))))  #572.85058
Out.wt.spr.est[Out.wt.spr.est$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
OUTcommcf11 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 49],apply(liveweight2018[liveweight2018$STRATA_ID == 49,26:52],1,sum),
                   liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 49],apply(liveweight2019[liveweight2019$STRATA_ID == 49,29:52],1,sum),
                   crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcommcf11, summary(OUTcommcf10, summary(OUTcommcf9, summary(OUTcommcf8, summary(OUTcommcf7)))))  #216.6
Out.wt.spr.est[Out.wt.spr.est$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
#OUTcommcf12 <- spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 49],apply(liveweight2019[liveweight2019$STRATA_ID == 49,26:52],1,sum),
#                   liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 49],apply(liveweight2021[liveweight2021$STRATA_ID == 49,29:52],1,sum),
#                   crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
#K <- summary(OUTcommcf12, summary(OUTcommcf11, summary(OUTcommcf10, summary(OUTcommcf9, summary(OUTcommcf8, summary(OUTcommcf7))))))  #
#Out.wt.spr.est[Out.wt.spr.est$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#plot(OUTcommcf12) #Perfect correlation - unreliable estimate  - go with simple mean 

#2021/2022 -- SPR is higher than simple mean.. based off 3 tows.. 
OUTcommcf13 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 49],apply(liveweight2021[liveweight2021$STRATA_ID == 49,26:52],1,sum),
                   liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 49],apply(liveweight2022[liveweight2022$STRATA_ID == 49,29:52],1,sum),
                   crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcommcf13)  #
Out.wt.spr.est[Out.wt.spr.est$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#plot(OUTcommcf12) #Perfect correlation - unreliable estimate  - go with simple mean 

#2022/2023
OUTcommcf14 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 49],apply(liveweight2022[liveweight2022$STRATA_ID == 49,26:52],1,sum),
                   liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 49],apply(liveweight2023[liveweight2023$STRATA_ID == 49,29:52],1,sum),
                   crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcommcf14)  #
Out.wt.spr.est[Out.wt.spr.est$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
OUTcommcf15 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 49],apply(liveweight2023[liveweight2023$STRATA_ID == 49,26:52],1,sum),
                   liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 49],apply(liveweight2024[liveweight2024$STRATA_ID == 49,29:52],1,sum),
                   crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcommcf15)  #
Out.wt.spr.est[Out.wt.spr.est$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
OUTcommcf16 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 49],apply(liveweight2024[liveweight2024$STRATA_ID == 49,26:52],1,sum),
                   liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 49],apply(liveweight2025[liveweight2025$STRATA_ID == 49,29:52],1,sum),
                   crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcommcf16)  #
Out.wt.spr.est[Out.wt.spr.est$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
OUTcommcf17 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 49],apply(liveweight2025[liveweight2025$STRATA_ID == 49,26:52],1,sum),
                   liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 49],apply(liveweight2026[liveweight2026$STRATA_ID == 49,29:52],1,sum),
                   crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(OUTcommcf17)  #
Out.wt.spr.est[Out.wt.spr.est$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Out.wt.spr.est

#make dataframe for all of Outer 28D Outer Commercial Biomass
Out.wt.spr.est$method <- "spr"
Out.wt.spr.est$NH <- 56009.89
Out.wt.spr.est$Area <- "Out"
Out.wt.spr.est$Age <- "Commercial"
names(Out.wt.spr.est) <- c("Year", "Mean.wt", "var.y", "method", "NH", "Area", "Age")

#Note since can't get SPR for 2021 - go with simple mean 
replace.spr <- c(2021)
SPA1B.Out.CommWt <- rbind(SPA1B.Out.CommWt.simple[SPA1B.Out.CommWt.simple$Year<2010,],SPA1B.Out.CommWt.simple[SPA1B.Out.CommWt.simple$Year %in% replace.spr,], Out.wt.spr.est[!Out.wt.spr.est$Year %in% replace.spr,])
SPA1B.Out.CommWt <- SPA1B.Out.CommWt %>% arrange(Year)
SPA1B.Out.CommWt

#interpolate 2020
approx(SPA1B.Out.CommWt$Year, SPA1B.Out.CommWt$Mean.wt, xout=2020)# 436.8086
SPA1B.Out.CommWt[SPA1B.Out.CommWt$Year==2020,c("Mean.wt","var.y")] <- c( 436.8086,852388.916) #larger so take 2021 var 2019 too small 

SPA1B.Out.CommWt$cv <- sqrt(SPA1B.Out.CommWt$var.y)/SPA1B.Out.CommWt$Mean.wt
SPA1B.Out.CommWt$kg.tow <-SPA1B.Out.CommWt$Mean.wt/1000
SPA1B.Out.CommWt$Bmass <- (SPA1B.Out.CommWt$kg.tow*SPA1B.Out.CommWt$NH)/1000

#combine Recruit and Commercial dataframes
SPA1B.Out.Weight <- rbind(SPA1B.Out.RecWt, SPA1B.Out.CommWt)


####
# ---- 5. Advocate Harbour -----
####

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 2001:surveyyear
X <- length(years)

SPA1B.AH.RecWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(8626.27, X), Area = rep("AH",X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.AH.RecWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR == 2000 + i,]
SPA1B.AH.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
SPA1B.AH.RecWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
}
SPA1B.AH.RecWt.simple

#assume 1997:2000 was the same as MBN
SPA1B.AH.RecWt.simple <- rbind(SPA1B.MBN.RecWt[SPA1B.MBN.RecWt$Year < 2001, c(1:7)], SPA1B.AH.RecWt.simple)
SPA1B.AH.RecWt.simple[SPA1B.AH.RecWt.simple$Year < 2001, "NH"] <- 8626.27
SPA1B.AH.RecWt.simple[SPA1B.AH.RecWt.simple$Year < 2001, "Area"] <- "AH"

#interpolate 2004
approx(SPA1B.AH.RecWt.simple$Year, SPA1B.AH.RecWt.simple$Mean.wt, xout = 2004) # 94.49175
SPA1B.AH.RecWt.simple[SPA1B.AH.RecWt.simple$Year == 2004,c("Mean.wt","var.y")] <- c( 94.49175, 6621.46294)

#interpolate 2020
approx(SPA1B.AH.RecWt.simple$Year, SPA1B.AH.RecWt.simple$Mean.wt, xout = 2020) # 94.49175
SPA1B.AH.RecWt.simple[SPA1B.AH.RecWt.simple$Year == 2020,c("Mean.wt","var.y")] <- c( 463.7016, 562129.71898)# prev year var 

#spr for recruit
#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
AH.rec.wt.spr.est <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009 spr
AHrecwt1<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==35],apply(liveweight2008[liveweight2008$STRATA_ID==35,23:25],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID==35],apply(liveweight2009[liveweight2009$STRATA_ID==35,26:28],1,sum),
crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHrecwt1)   #1855
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
AHrecwt2<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==35],apply(liveweight2009[liveweight2009$STRATA_ID==35,23:25],1,sum),
liveweight2010$TOW_NO[liveweight2010$STRATA_ID==35],apply(liveweight2010[liveweight2010$STRATA_ID==35,26:28],1,sum),
crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHrecwt2)
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
AHrecwt3<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==35],apply(liveweight2010[liveweight2010$STRATA_ID==35,23:25],1,sum),
liveweight2011$TOW_NO[liveweight2011$STRATA_ID==35],apply(liveweight2011[liveweight2011$STRATA_ID==35,26:28],1,sum),
crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHrecwt3)
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
AHrecwt4<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==35],apply(liveweight2011[liveweight2011$STRATA_ID==35,23:25],1,sum),
liveweight2012$TOW_NO[liveweight2012$STRATA_ID==35],apply(liveweight2012[liveweight2012$STRATA_ID==35,26:28],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHrecwt4)
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
AHrecwt5<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==35],apply(liveweight2012[liveweight2012$STRATA_ID==35,23:25],1,sum),
liveweight2013$TOW_NO[liveweight2013$STRATA_ID==35],apply(liveweight2013[liveweight2013$STRATA_ID==35,26:28],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrecwt5)
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
AHrecwt6<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==35],apply(liveweight2013[liveweight2013$STRATA_ID==35,23:25],1,sum),
liveweight2014$TOW_NO[liveweight2014$STRATA_ID==35],apply(liveweight2014[liveweight2014$STRATA_ID==35,26:28],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHrecwt6)
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
AHrecwt7<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==35],apply(liveweight2014[liveweight2014$STRATA_ID==35,23:25],1,sum),
liveweight2015$TOW_NO[liveweight2015$STRATA_ID==35],apply(liveweight2015[liveweight2015$STRATA_ID==35,26:28],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary(AHrecwt7)# 368.7736
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
AHrecwt8<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==35],apply(liveweight2015[liveweight2015$STRATA_ID==35,23:25],1,sum),
              liveweight2016$TOW_NO[liveweight2016$STRATA_ID==35],apply(liveweight2016[liveweight2016$STRATA_ID==35,26:28],1,sum),
              crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary(AHrecwt8)#450.0731
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
AHrecwt9<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==35],apply(liveweight2016[liveweight2016$STRATA_ID==35,23:25],1,sum),
              liveweight2017$TOW_NO[liveweight2017$STRATA_ID==35],apply(liveweight2017[liveweight2017$STRATA_ID==35,26:28],1,sum),
              crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary(AHrecwt9)#179.6195
AH.rec.wt.spr.est [AH.rec.wt.spr.est $Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
AHrecwt10 <- spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID == 35],apply(liveweight2017[liveweight2017$STRATA_ID == 35,23:25],1,sum),
                 liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 35],apply(liveweight2018[liveweight2018$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt10)  #288.2481
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
AHrecwt11 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 35],apply(liveweight2018[liveweight2018$STRATA_ID == 35,23:25],1,sum),
                 liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 35],apply(liveweight2019[liveweight2019$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt11)  # 310.2000
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
AHrecwt12 <- spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 35],apply(liveweight2019[liveweight2019$STRATA_ID == 35,23:25],1,sum),
                 liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 35],apply(liveweight2021[liveweight2021$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt12)  # 
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
AHrecwt13 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 35],apply(liveweight2021[liveweight2021$STRATA_ID == 35,23:25],1,sum),
                 liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 35],apply(liveweight2022[liveweight2022$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt13)  # 
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
AHrecwt14 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 35],apply(liveweight2022[liveweight2022$STRATA_ID == 35,23:25],1,sum),
                 liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 35],apply(liveweight2023[liveweight2023$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt14)  # 
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
AHrecwt15 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 35],apply(liveweight2023[liveweight2023$STRATA_ID == 35,23:25],1,sum),
                 liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 35],apply(liveweight2024[liveweight2024$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt15)  # 
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
AHrecwt16 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 35],apply(liveweight2024[liveweight2024$STRATA_ID == 35,23:25],1,sum),
                 liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 35],apply(liveweight2025[liveweight2025$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt16)  # 
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
AHrecwt17 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 35],apply(liveweight2025[liveweight2025$STRATA_ID == 35,23:25],1,sum),
                 liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 35],apply(liveweight2026[liveweight2026$STRATA_ID == 35,26:28],1,sum),
                 crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHrecwt17)  # 
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

AH.rec.wt.spr.est

#interpolate 2020
approx(AH.rec.wt.spr.est$Year, AH.rec.wt.spr.est$Yspr, xout = 2020) # 217.2041
AH.rec.wt.spr.est[AH.rec.wt.spr.est$Year == 2020,c("Yspr","var.Yspr.corrected")] <- c( 217.2041,  11783.2621)# 2020 year var 

#make dataframe for all of AH Commercial biomass
AH.rec.wt.spr.est$method <- "spr"
AH.rec.wt.spr.est$NH <- 8626.27
AH.rec.wt.spr.est$Area <- "AH"
AH.rec.wt.spr.est$Age <- "Recruit"
names(AH.rec.wt.spr.est ) <- c("Year", "Mean.wt", "var.y", "method", "NH", "Area", "Age")

SPA1B.AH.RecWt <- rbind(SPA1B.AH.RecWt.simple[SPA1B.AH.RecWt.simple$Year < 2009,], AH.rec.wt.spr.est )
SPA1B.AH.RecWt$cv <- sqrt(SPA1B.AH.RecWt$var.y)/SPA1B.AH.RecWt$Mean.wt
SPA1B.AH.RecWt$kg.tow <- SPA1B.AH.RecWt$Mean.wt/1000
SPA1B.AH.RecWt$Bmass <- SPA1B.AH.RecWt$kg.tow*SPA1B.AH.RecWt$NH/1000

############ Commercial (80+ mm)
years <- 2001:surveyyear
X <- length(years)

SPA1B.AH.CommWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(8626.27, X),Area = rep("AH",X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.AH.CommWt.simple$Year)) {
	temp.data <- BFliveweight[BFliveweight$YEAR == 2000 + i,]
SPA1B.AH.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
SPA1B.AH.CommWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
}
SPA1B.AH.CommWt.simple

#assume 1997:2000 was the same as MBN
SPA1B.AH.CommWt.simple <- rbind(SPA1B.MBN.CommWt[SPA1B.MBN.CommWt$Year < 2001, c(1:7)], SPA1B.AH.CommWt.simple)
SPA1B.AH.CommWt.simple[SPA1B.AH.CommWt.simple$Year < 2001, "NH"] <- 8626.27
SPA1B.AH.CommWt.simple[SPA1B.AH.CommWt.simple$Year < 2001, "Area"] <- "AH"

#interpolate missing year 2004
approx(SPA1B.AH.CommWt.simple$Year, SPA1B.AH.CommWt.simple$Mean.wt, xout = 2004)# 4075.205
SPA1B.AH.CommWt.simple[SPA1B.AH.CommWt.simple$Year == 2004,c("Mean.wt","var.y")] <- c(4075.205,22680828.18)

#interpolate missing year 2020
approx(SPA1B.AH.CommWt.simple$Year, SPA1B.AH.CommWt.simple$Mean.wt, xout = 2020)#  6986.728
SPA1B.AH.CommWt.simple[SPA1B.AH.CommWt.simple$Year == 2020,c("Mean.wt","var.y")] <- c(6986.728,150115242.98)

#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
AH.wt.spr.est <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009 spr
AHcommcf1<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==35],apply(liveweight2008[liveweight2008$STRATA_ID==35,26:52],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID==35],apply(liveweight2009[liveweight2009$STRATA_ID==35,29:52],1,sum),
crossref.BoF.2009[crossref.BoF.2009$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHcommcf1)   #1855
AH.wt.spr.est[AH.wt.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
AHcommcf2<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==35],apply(liveweight2009[liveweight2009$STRATA_ID==35,26:52],1,sum),             liveweight2010$TOW_NO[liveweight2010$STRATA_ID==35],apply(liveweight2010[liveweight2010$STRATA_ID==35,29:52],1,sum),crossref.BoF.2010[crossref.BoF.2010$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHcommcf2,summary(AHcommcf1))   #2225.909
AH.wt.spr.est[AH.wt.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
AHcommcf3<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==35],apply(liveweight2010[liveweight2010$STRATA_ID==35,26:52],1,sum),liveweight2011$TOW_NO[liveweight2011$STRATA_ID==35],apply(liveweight2011[liveweight2011$STRATA_ID==35,29:52],1,sum),
crossref.BoF.2011[crossref.BoF.2011$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHcommcf3,  summary(AHcommcf2,summary(AHcommcf1)))  #1828.15
AH.wt.spr.est[AH.wt.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
AHcommcf4<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==35],apply(liveweight2011[liveweight2011$STRATA_ID==35,26:52],1,sum),liveweight2012$TOW_NO[liveweight2012$STRATA_ID==35],apply(liveweight2012[liveweight2012$STRATA_ID==35,29:52],1,sum),
crossref.BoF.2012[crossref.BoF.2012$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])

K<-summary(AHcommcf4,  summary(AHcommcf3,  summary(AHcommcf2,summary(AHcommcf1))))   #1891.364
AH.wt.spr.est[AH.wt.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
AHcommcf5<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==35],apply(liveweight2012[liveweight2012$STRATA_ID==35,26:52],1,sum),liveweight2013$TOW_NO[liveweight2013$STRATA_ID==35],apply(liveweight2013[liveweight2013$STRATA_ID==35,29:52],1,sum),
crossref.BoF.2013[crossref.BoF.2013$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcommcf5, summary(AHcommcf4,  summary(AHcommcf3,  summary(AHcommcf2,summary(AHcommcf1)))))  # 2931.943
AH.wt.spr.est[AH.wt.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
AHcommcf6<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==35],apply(liveweight2013[liveweight2013$STRATA_ID==35,26:52],1,sum),liveweight2014$TOW_NO[liveweight2014$STRATA_ID==35],apply(liveweight2014[liveweight2014$STRATA_ID==35,29:52],1,sum),
crossref.BoF.2014[crossref.BoF.2014$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcommcf6, summary (AHcommcf5, summary(AHcommcf4,  summary(AHcommcf3,  summary(AHcommcf2,summary(AHcommcf1))))))  #3836.66
AH.wt.spr.est[AH.wt.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
AHcommcf7<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==35],apply(liveweight2014[liveweight2014$STRATA_ID==35,26:52],1,sum),liveweight2015$TOW_NO[liveweight2015$STRATA_ID==35],apply(liveweight2015[liveweight2015$STRATA_ID==35,29:52],1,sum),
crossref.BoF.2015[crossref.BoF.2015$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1)))))))  #3354.118
AH.wt.spr.est[AH.wt.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
AHcommcf8<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==35],apply(liveweight2015[liveweight2015$STRATA_ID==35,26:52],1,sum),               liveweight2016$TOW_NO[liveweight2016$STRATA_ID==35],apply(liveweight2016[liveweight2016$STRATA_ID==35,29:52],1,sum),               crossref.BoF.2016[crossref.BoF.2016$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1))))))))  #1943.012
AH.wt.spr.est[AH.wt.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
AHcommcf9<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==35],apply(liveweight2016[liveweight2016$STRATA_ID==35,26:52],1,sum),             liveweight2017$TOW_NO[liveweight2017$STRATA_ID==35],apply(liveweight2017[liveweight2017$STRATA_ID==35,29:52],1,sum),             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID==35,c("TOW_NO_REF","TOW_NO")])
K<-summary (AHcommcf9, summary (AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1)))))))))  #1437.095
AH.wt.spr.est[AH.wt.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
AHcommcf10 <- spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID == 35],apply(liveweight2017[liveweight2017$STRATA_ID == 35,26:52],1,sum),
                  liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 35],apply(liveweight2018[liveweight2018$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1))))))))))  #3311.064
AH.wt.spr.est[AH.wt.spr.est$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
AHcommcf11 <- spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID == 35],apply(liveweight2018[liveweight2018$STRATA_ID == 35,26:52],1,sum),
                  liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 35],apply(liveweight2019[liveweight2019$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcommcf11,summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1)))))))))))  #1522.623
AH.wt.spr.est[AH.wt.spr.est$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
AHcommcf12 <- spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID == 35],apply(liveweight2019[liveweight2019$STRATA_ID == 35,26:52],1,sum),
                  liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 35],apply(liveweight2021[liveweight2021$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcommcf12, summary(AHcommcf11,summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1))))))))))))  #
AH.wt.spr.est[AH.wt.spr.est$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
AHcommcf13 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID == 35],apply(liveweight2021[liveweight2021$STRATA_ID == 35,26:52],1,sum),
                  liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 35],apply(liveweight2022[liveweight2022$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcommcf13, summary(AHcommcf12, summary(AHcommcf11,summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1)))))))))))))  #
AH.wt.spr.est[AH.wt.spr.est$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
AHcommcf14 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID == 35],apply(liveweight2022[liveweight2022$STRATA_ID == 35,26:52],1,sum),
                  liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 35],apply(liveweight2023[liveweight2023$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcommcf14,summary(AHcommcf13, summary(AHcommcf12, summary(AHcommcf11,summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1))))))))))))))  #
AH.wt.spr.est[AH.wt.spr.est$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2023/2024
AHcommcf15 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID == 35],apply(liveweight2023[liveweight2023$STRATA_ID == 35,26:52],1,sum),
                  liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 35],apply(liveweight2024[liveweight2024$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcommcf15,summary(AHcommcf14,summary(AHcommcf13, summary(AHcommcf12, summary(AHcommcf11,summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1)))))))))))))))  
AH.wt.spr.est[AH.wt.spr.est$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
AHcommcf16 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID == 35],apply(liveweight2024[liveweight2024$STRATA_ID == 35,26:52],1,sum),
                  liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 35],apply(liveweight2025[liveweight2025$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(AHcommcf16, summary(AHcommcf15,summary(AHcommcf14,summary(AHcommcf13, summary(AHcommcf12, summary(AHcommcf11,summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1))))))))))))))))  
AH.wt.spr.est[AH.wt.spr.est$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
AHcommcf17 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID == 35],apply(liveweight2025[liveweight2025$STRATA_ID == 35,26:52],1,sum),
                  liveweight2026$TOW_NO[liveweight2026$STRATA_ID == 35],apply(liveweight2026[liveweight2026$STRATA_ID == 35,29:52],1,sum),
                  crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <-summary(AHcommcf17,summary(AHcommcf16, summary(AHcommcf15,summary(AHcommcf14,summary(AHcommcf13, summary(AHcommcf12, summary(AHcommcf11,summary(AHcommcf10, summary(AHcommcf9, summary(AHcommcf8,summary(AHcommcf7,summary(AHcommcf6,summary(AHcommcf5,summary(AHcommcf4,summary(AHcommcf3,summary(AHcommcf2,summary(AHcommcf1)))))))))))))))))
AH.wt.spr.est[AH.wt.spr.est$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

AH.wt.spr.est

#interpolate missing year 2020
approx(AH.wt.spr.est$Year, AH.wt.spr.est$Yspr, xout = 2020)#  
AH.wt.spr.est[AH.wt.spr.est$Year == 2020,c("Yspr","var.Yspr.corrected")] <- c(1144.839, 534560.1)


#make dataframe for all of AH Commercial biomass
AH.wt.spr.est$method <- "spr"
AH.wt.spr.est$NH <- 8626.27
AH.wt.spr.est$Area <- "AH"
AH.wt.spr.est$Age <- "Commercial"
names(AH.wt.spr.est) <- c("Year", "Mean.wt", "var.y", "method", "NH", "Area", "Age")

SPA1B.AH.CommWt <- rbind(SPA1B.AH.CommWt.simple[SPA1B.AH.CommWt.simple$Year < 2009,], AH.wt.spr.est)
SPA1B.AH.CommWt$cv <- sqrt(SPA1B.AH.CommWt$var.y)/SPA1B.AH.CommWt$Mean.wt
SPA1B.AH.CommWt$kg.tow <- SPA1B.AH.CommWt$Mean.wt/1000
SPA1B.AH.CommWt$Bmass <- SPA1B.AH.CommWt$kg.tow*SPA1B.AH.CommWt$NH/1000

#combine Recruit and Commercial dataframes

SPA1B.AH.Weight <- rbind(SPA1B.AH.RecWt, SPA1B.AH.CommWt)

####
#  ---- 6.Spencer's Island  (simple mean for whole area) ----
####

############ Recruit (65-79 mm)
#use simple mean
years <- 2005:surveyyear
X <- length(years)

SPA1B.SI.RecWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(20337.96, X), Area = rep("SI",X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.SI.RecWt.simple$Year)) {
  temp.data <- BFliveweight[BFliveweight$YEAR == 2004 + i,]
	SPA1B.SI.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
	SPA1B.SI.RecWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
}
SPA1B.SI.RecWt.simple

#assume 2001:2004 was the same as Outer Bay 28D
SPA1B.SI.RecWt.simple <- rbind(SPA1B.Out.RecWt.simple[c(5:8), c(1:7)], SPA1B.SI.RecWt.simple)

#assume 1997:2000 was the same as MBN
SPA1B.SI.RecWt.simple <- rbind(SPA1B.MBN.RecWt[SPA1B.MBN.RecWt$Year < 2001, c(1:7)], SPA1B.SI.RecWt.simple)

#interpolate missing year 2020
approx(SPA1B.SI.RecWt.simple$Year, SPA1B.SI.RecWt.simple$Mean.wt, xout = 2020)#  38.53037
SPA1B.SI.RecWt.simple[SPA1B.SI.RecWt.simple$Year == 2020,c("Mean.wt","var.y")] <- c(38.53037, 12874.79531)


SPA1B.SI.RecWt.simple[SPA1B.SI.RecWt.simple$Year < 2005, "NH"] <- 20337.96
SPA1B.SI.RecWt.simple[SPA1B.SI.RecWt.simple$Year < 2009, "Area"] <- "SI"

SPA1B.SI.RecWt.simple$cv <- sqrt(SPA1B.SI.RecWt.simple$var.y)/SPA1B.SI.RecWt.simple$Mean.wt
SPA1B.SI.RecWt.simple$kg.tow <- SPA1B.SI.RecWt.simple$Mean.wt/1000
SPA1B.SI.RecWt.simple$Bmass <- (SPA1B.SI.RecWt.simple$kg*SPA1B.SI.RecWt.simple$NH)/1000

############ Commercial (80+ mm)
#use simple mean
years <- 2005:surveyyear
X <- length(years)

SPA1B.SI.CommWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(20337.96, X), Area = rep("SI",X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.SI.CommWt.simple$Year)) {
  temp.data <- BFliveweight[BFliveweight$YEAR == 2004 + i,]
	SPA1B.SI.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
	SPA1B.SI.CommWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
}
SPA1B.SI.CommWt.simple

#assume 2001:2004 was the same as Outer Bay 28D
SPA1B.SI.CommWt.simple <- rbind(SPA1B.Out.CommWt.simple[c(5:8), c(1:7)], SPA1B.SI.CommWt.simple)
#assume 1997:2000 was the same as MBN
SPA1B.SI.CommWt.simple <- rbind(SPA1B.MBN.CommWt[SPA1B.MBN.CommWt$Year < 2001, c(1:7)], SPA1B.SI.CommWt.simple)
SPA1B.SI.CommWt.simple[SPA1B.SI.CommWt.simple$Year < 2005, "NH"] <- 20337.96
SPA1B.SI.CommWt.simple[SPA1B.SI.CommWt.simple$Year < 2009, "Area"] <- "SI"

#interpolate missing year 2020
approx(SPA1B.SI.CommWt.simple$Year, SPA1B.SI.CommWt.simple$Mean.wt, xout = 2020)#  153.9341
SPA1B.SI.CommWt.simple[SPA1B.SI.CommWt.simple$Year == 2020,c("Mean.wt","var.y")] <- c(153.9341, 1.926817e+05)


SPA1B.SI.CommWt.simple$cv <- sqrt(SPA1B.SI.CommWt.simple$var.y)/SPA1B.SI.CommWt.simple$Mean.wt
SPA1B.SI.CommWt.simple$kg.tow <- SPA1B.SI.CommWt.simple$Mean.wt/1000
SPA1B.SI.CommWt.simple$Bmass <- (SPA1B.SI.CommWt.simple$kg.tow*SPA1B.SI.CommWt.simple$NH)/1000

#combine Recruit and Commercial dataframes
SPA1B.SI.Weight <- rbind(SPA1B.SI.RecWt.simple, SPA1B.SI.CommWt.simple)

####
# ----- 7. Scots Bay (simple mean for whole area) ----
####

############ Recruit (65-79 mm)
#use simple mean
years <- 2005:surveyyear
X <- length(years)

SPA1B.SB.RecWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(19415.98,X), Area = rep("SB",X), Age = rep("Recruit", X))
for (i in 1:length(SPA1B.SB.RecWt.simple$Year)) {
  temp.data <- BFliveweight[BFliveweight$YEAR == 2004 + i,]
	SPA1B.SB.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
	SPA1B.SB.RecWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 26:28],1,sum))
}
SPA1B.SB.RecWt.simple

#assume 2001:2004 was the same as Outer Bay 28D
SPA1B.SB.RecWt.simple <- rbind(SPA1B.Out.RecWt.simple[c(5:8), c(1:7)], SPA1B.SB.RecWt.simple)
#assume 1997:2000 was the same as MBN
SPA1B.SB.RecWt.simple <- rbind(SPA1B.MBN.RecWt[SPA1B.MBN.RecWt$Year < 2001, c(1:7)], SPA1B.SB.RecWt.simple)
SPA1B.SB.RecWt.simple[SPA1B.SI.RecWt.simple$Year < 2005, "NH"] <- 19415.98
SPA1B.SB.RecWt.simple[SPA1B.SI.RecWt.simple$Year < 2009, "Area"] <- "SB"

#interpolate missing year 2020
approx(SPA1B.SB.RecWt.simple$Year, SPA1B.SB.RecWt.simple$Mean.wt, xout = 2020)#  59.03481
SPA1B.SB.RecWt.simple[SPA1B.SB.RecWt.simple$Year == 2020,c("Mean.wt","var.y")] <- c(59.03481,17442.36125)


SPA1B.SB.RecWt.simple$cv <- sqrt(SPA1B.SB.RecWt.simple$var.y)/SPA1B.SB.RecWt.simple$Mean.wt
SPA1B.SB.RecWt.simple$kg.tow <- SPA1B.SB.RecWt.simple$Mean.wt/1000
SPA1B.SB.RecWt.simple$Bmass <- (SPA1B.SB.RecWt.simple$kg.tow*SPA1B.SB.RecWt.simple$NH)/1000

############ Commercial (80+ mm)
#use simple mean
years <- 2005:surveyyear
X <- length(years)

SPA1B.SB.CommWt.simple <- data.frame(Year = years, Mean.wt = rep(NA,X), var.y = rep(NA,X), method = rep("simple",X), NH = rep(19415.98, X), Area = rep("SB",X), Age = rep("Commercial", X))
for (i in 1:length(SPA1B.SB.CommWt.simple$Year)) {
  temp.data <- BFliveweight[BFliveweight$YEAR == 2004 + i,]
	SPA1B.SB.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
	SPA1B.SB.CommWt.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 29:52],1,sum))
}
SPA1B.SB.CommWt.simple

#assume 2001:2004 was the same as Outer Bay 28D
SPA1B.SB.CommWt.simple <- rbind(SPA1B.Out.CommWt.simple[c(5:8), c(1:7)], SPA1B.SB.CommWt.simple) #assume 1997:2000 was the same as MBN
SPA1B.SB.CommWt.simple <- rbind(SPA1B.MBN.CommWt[SPA1B.MBN.CommWt$Year < 2001, c(1:7)], SPA1B.SB.CommWt.simple)

SPA1B.SB.CommWt.simple[SPA1B.SI.CommWt.simple$Year < 2005, "NH"] <- 19415.98
SPA1B.SB.CommWt.simple[SPA1B.SI.CommWt.simple$Year < 2009, "Area"] <- "SB"

#interpolate missing year 2020
approx(SPA1B.SB.CommWt.simple$Year, SPA1B.SB.CommWt.simple$Mean.wt, xout = 2020)#  152.2999
SPA1B.SB.CommWt.simple[SPA1B.SB.CommWt.simple$Year == 2020,c("Mean.wt","var.y")] <- c(152.2999,294778.230)


SPA1B.SB.CommWt.simple$cv <- sqrt(SPA1B.SB.CommWt.simple$var.y)/SPA1B.SB.CommWt.simple$Mean.wt
SPA1B.SB.CommWt.simple$kg.tow <- SPA1B.SB.CommWt.simple$Mean.wt/1000
SPA1B.SB.CommWt.simple$Bmass <- (SPA1B.SB.CommWt.simple$kg.tow*SPA1B.SB.CommWt.simple$NH)/1000

#combine Recruit and Commercial dataframes
SPA1B.SB.Weight <- rbind(SPA1B.SB.RecWt.simple, SPA1B.SB.CommWt.simple)

####
#  ---- Make Weight dataframe for SPA 1B ----
####

SPA1B.Weight <- rbind(SPA1B.CS.Weight, SPA1B.MBN.Weight, SPA1B.28C.Weight, 
                      SPA1B.Out.Weight, SPA1B.AH.Weight, SPA1B.SI.Weight, 
                      SPA1B.SB.Weight)

write.csv(SPA1B.Weight, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1B.Index.Weight.",surveyyear,".csv"))

####
###
### ----  Calculate Commercial (N) Population size, Commercial (I) and Recruit (R) biomass for the model in tons ----
###
####
#model only needs 1996+

###
### ----     Calculate Commercial (N) Population size for all 1B   ----
###
#
#model only needs 1997+

#.... Population Numbers of Commercial size (N)
SPA1B.CS.Comm.simple.1997on <- SPA1B.CS.Comm.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.MBN.Comm.1997on <- SPA1B.MBN.Comm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.28C.Comm.1997on <- SPA1B.28C.Comm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.Out.Comm.1997on <- SPA1B.Out.Comm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.AH.Comm.1997on <- SPA1B.AH.Comm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.SI.Comm.simple.1997on <- SPA1B.SI.Comm.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.SB.Comm.simple.1997on <- SPA1B.SB.Comm.simple %>% filter(Year >= 1997) %>% select(Year, Pop)

#Ensure year ranges are the same: Should all come back TRUE 
all(SPA1B.CS.Comm.simple.1997on$Year == SPA1B.MBN.Comm.1997on$Year)
all(SPA1B.MBN.Comm.1997on$Year == SPA1B.28C.Comm.1997on$Year)
all(SPA1B.28C.Comm.1997on$Year == SPA1B.Out.Comm.1997on$Year)
all(SPA1B.Out.Comm.1997on$Year == SPA1B.AH.Comm.1997on$Year)
all(SPA1B.AH.Comm.1997on$Year == SPA1B.SI.Comm.simple.1997on$Year)
all(SPA1B.SI.Comm.simple.1997on$Year == SPA1B.SB.Comm.simple.1997on$Year)

#Create object for N (population size): 
N <- data.frame(Year = SPA1B.CS.Comm.simple.1997on$Year, N = SPA1B.CS.Comm.simple.1997on$Pop + 
                  SPA1B.MBN.Comm.1997on$Pop + 
                  SPA1B.28C.Comm.1997on$Pop + 
                  SPA1B.Out.Comm.1997on$Pop + 
                  SPA1B.AH.Comm.1997on$Pop + 
                  SPA1B.SI.Comm.simple.1997on$Pop + 
                  SPA1B.SB.Comm.simple.1997on$Pop)
N$N 
N$N.millions <- N$N /1000000
N

#.... Population Numbers of Recruits
SPA1B.CS.Rec.simple.1997on <- SPA1B.CS.Rec.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.MBN.Rec.1997on <- SPA1B.MBN.Rec %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.28C.Rec.1997on <- SPA1B.28C.Rec %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.Out.Rec.1997on <- SPA1B.Out.Rec.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.AH.Rec.1997on <- SPA1B.AH.Rec %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.SI.Rec.simple.1997on <- SPA1B.SI.Rec.simple %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1B.SB.Rec.simple.1997on <- SPA1B.SB.Rec.simple %>% filter(Year >= 1997) %>% select(Year, Pop)

#Ensure year ranges are the same: Should all come back TRUE 
all(SPA1B.CS.Rec.simple.1997on$Year == SPA1B.MBN.Rec.1997on$Year)
all(SPA1B.MBN.Rec.1997on$Year == SPA1B.28C.Rec.1997on$Year)
all(SPA1B.28C.Rec.1997on$Year == SPA1B.Out.Rec.1997on$Year)
all(SPA1B.Out.Rec.1997on$Year == SPA1B.AH.Rec.1997on$Year)
all(SPA1B.AH.Rec.1997on$Year == SPA1B.SI.Rec.simple.1997on$Year)
all(SPA1B.SI.Rec.simple.1997on$Year == SPA1B.SB.Rec.simple.1997on$Year)

#Create object for N (population size): 
N.rec  <- data.frame(Year = SPA1B.CS.Rec.simple.1997on$Year, N.rec = SPA1B.CS.Rec.simple.1997on$Pop + 
                  SPA1B.MBN.Rec.1997on$Pop + 
                  SPA1B.28C.Rec.1997on$Pop + 
                  SPA1B.Out.Rec.1997on$Pop + 
                  SPA1B.AH.Rec.1997on$Pop + 
                  SPA1B.SI.Rec.simple.1997on$Pop + 
                  SPA1B.SB.Rec.simple.1997on$Pop)
N.rec 
N$N.rec <- N.rec$N.rec
N$N.rec.millions <-  N$N.rec/1000000
N



#.... Commercial biomass (I)
SPA1B.CS.CommWt.simple.1997on <- SPA1B.CS.CommWt.simple %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.MBN.CommWt.1997on <- SPA1B.MBN.CommWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.28C.CommWt.1997on <- SPA1B.28C.CommWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.Out.CommWt.1997on <- SPA1B.Out.CommWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.AH.CommWt.1997on <- SPA1B.AH.CommWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.SI.CommWt.simple.1997on <- SPA1B.SI.CommWt.simple %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.SB.CommWt.simple.1997on <- SPA1B.SB.CommWt.simple %>% filter(Year >= 1997) %>% select(Year, Bmass)

#Ensure year ranges are the same: Should all come back TRUE 
all(SPA1B.CS.CommWt.simple.1997on$Year == SPA1B.MBN.CommWt.1997on$Year)
all(SPA1B.MBN.CommWt.1997on$Year == SPA1B.28C.CommWt.1997on$Year)
all(SPA1B.28C.CommWt.1997on$Year == SPA1B.Out.CommWt.1997on$Year)
all(SPA1B.Out.CommWt.1997on$Year == SPA1B.AH.CommWt.1997on$Year)
all(SPA1B.AH.CommWt.1997on$Year == SPA1B.SI.CommWt.simple.1997on$Year)
all(SPA1B.SI.CommWt.simple.1997on$Year == SPA1B.SB.CommWt.simple.1997on$Year)

#Create object for I (commercial biomass): 
I <- data.frame(Year = SPA1B.CS.CommWt.simple.1997on$Year, Bmass  = SPA1B.CS.CommWt.simple.1997on$Bmass + 
                  SPA1B.MBN.CommWt.1997on$Bmass + 
                  SPA1B.28C.CommWt.1997on$Bmass + 
                  SPA1B.Out.CommWt.1997on$Bmass + 
                  SPA1B.AH.CommWt.1997on$Bmass + 
                  SPA1B.SI.CommWt.simple.1997on$Bmass + 
                  SPA1B.SB.CommWt.simple.1997on$Bmass)
I$Bmass 


#... Recruit biomass
SPA1B.CS.RecWt.simple.1997on <- SPA1B.CS.RecWt.simple %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.MBN.RecWt.1997on  <- SPA1B.MBN.RecWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.28C.RecWt.1997on  <- SPA1B.28C.RecWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.Out.RecWt.1997on  <- SPA1B.Out.RecWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.AH.RecWt.1997on  <- SPA1B.AH.RecWt %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.SI.RecWt.simple.1997on  <- SPA1B.SI.RecWt.simple %>% filter(Year >= 1997) %>% select(Year, Bmass)
SPA1B.SB.RecWt.simple.1997on  <- SPA1B.SB.RecWt.simple %>% filter(Year >= 1997) %>% select(Year, Bmass)

#Ensure year ranges are the same: Should all come back TRUE 
all(SPA1B.CS.RecWt.simple.1997on $Year == SPA1B.MBN.RecWt.1997on $Year)
all(SPA1B.MBN.RecWt.1997on $Year == SPA1B.28C.RecWt.1997on $Year)
all(SPA1B.28C.RecWt.1997on $Year == SPA1B.Out.RecWt.1997on $Year)
all(SPA1B.Out.RecWt.1997on $Year == SPA1B.AH.RecWt.1997on $Year)
all(SPA1B.AH.RecWt.1997on $Year == SPA1B.SI.RecWt.simple.1997on $Year)
all(SPA1B.SI.RecWt.simple.1997on $Year == SPA1B.SB.RecWt.simple.1997on $Year)


#Create object for IR (Recruit biomass): 
IR <- data.frame(Year = SPA1B.CS.RecWt.simple.1997on$Year, Bmass  = SPA1B.CS.RecWt.simple.1997on$Bmass + 
                  SPA1B.MBN.RecWt.1997on$Bmass + 
                  SPA1B.28C.RecWt.1997on$Bmass + 
                  SPA1B.Out.RecWt.1997on$Bmass + 
                  SPA1B.AH.RecWt.1997on$Bmass + 
                  SPA1B.SI.RecWt.simple.1997on$Bmass + 
                  SPA1B.SB.RecWt.simple.1997on$Bmass)
IR$Bmass 



###
###  ----  Calculate CV for model I.cv and R.cv ---- 
###                

# NB! This was not done in 2019 because modelling was not done
#Done on 25 JUne 2020 L. Nasmith

#n tows (update each year)
years <- 1997:surveyyear
X <- length(years)
tow.SPA1B <- data.frame(Year=(years), CS.tow=rep(NA,X),MBNE.tow=rep(NA,X),MBNW.tow=rep(NA,X),C28.tow=rep(NA,X),Out.tow=rep(NA,X),AH.tow=rep(NA,X),SI.tow=rep(NA,X),SB.tow=rep(NA,X))
for(i in 1:length(years)){
temp.data<-livefreq[livefreq$TOW_TYPE_ID%in%c(1,5) & livefreq$YEAR==1996+i,]
 tow.SPA1B [i,2] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==37])) #CS
 tow.SPA1B [i,3] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==58])) #MBNE
 tow.SPA1B [i,4] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==38])) #MBNW
 tow.SPA1B [i,5] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==53])) #28C
 tow.SPA1B [i,6] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==49])) #Outer
 tow.SPA1B [i,7] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==35])) #aH
 tow.SPA1B [i,8] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==52])) #SI
 tow.SPA1B [i,9] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==51])) #SB
}
tow.SPA1B

#for areas with missing years, the no.tows in those years is taken from the area used to estimate the mean (e.g. MBN used for SB in 1997-2000), see code above for each area to see how missing years in the  time series are filled in.
#MBNW.tow<-c(3,  6 , 4, 12 ,13, 11, 17,  17, 20, 26, 19,18 ,22 ,15, 13, 14, 11, 12, 13,13,9,12,11)
MBNW.tow.missing.year <- data.frame(Year = c(1997:2002, 2004), MBNW.tow = c(3,  6 , 4, 12 ,13, 11, 17))
tow.SPA1B$MBNW.tow[tow.SPA1B$Year %in% MBNW.tow.missing.year$Year] <- MBNW.tow.missing.year$MBNW.tow

#C28.tow<-c(3 , 6 , 4, 12, 19, 21 ,30,  30, 21, 39, 23, 33, 33, 30, 31 ,31, 28, 28, 27,27,27,27,27)
C28.tow.missing.year <- data.frame(Year = c(1997:2000, 2004), C28.tow = c(3 , 6 , 4, 12, 30))
tow.SPA1B$C28.tow[tow.SPA1B$Year %in% C28.tow.missing.year$Year] <- C28.tow.missing.year$C28.tow

#Out.tow <- c(3 , 6 , 4, 12,  5 , 6 , 2 , 2,  3,  8 ,12, 23, 13,  6 ,12 ,12, 11, 11, 11,11,11,11,11)
Out.tow.missing.year <- data.frame(Year = c(1997:2000, 2004), Out.tow = c(3 , 6 , 4, 12, 2))
tow.SPA1B$Out.tow[tow.SPA1B$Year %in% Out.tow.missing.year$Year] <- Out.tow.missing.year$Out.tow

#AH.tow<-c(3 , 6 , 4, 12, 3, 4, 3, 2, 4 ,6, 6 ,5, 7, 9 ,9, 9 ,8, 8, 8,8,8,8,8)
AH.tow.missing.year <- data.frame(Year = c(1997:2000, 2004), AH.tow = c(3 , 6 , 4, 12, 2))
tow.SPA1B$AH.tow[tow.SPA1B$Year %in% AH.tow.missing.year$Year] <- AH.tow.missing.year$AH.tow

#SI.tow<-c(3 , 6 , 4, 12  ,5 , 6 , 2 , 2 , 8 ,11,  6,  7,  6 , 5 , 5 , 5,  5 , 5, 5,5,5,5,5)
SI.tow.missing.year <- data.frame(Year = c(1997:2004), SI.tow = c(3 , 6 , 4, 12, 5, 6, 2, 2))
tow.SPA1B$SI.tow[tow.SPA1B$Year %in% SI.tow.missing.year$Year] <- SI.tow.missing.year$SI.tow

#SB.tow<-c(3 , 6 , 4, 12 , 5 , 6 , 2 ,2, 27, 4, 4, 5 ,5 ,4, 4 ,4 ,4 ,4 ,4,4,4,4,4) #27 here is a typo 
SB.tow.missing.year <- data.frame(Year = c(1997:2004), SB.tow = c(3 , 6 , 4, 12, 5, 6, 2, 2))
tow.SPA1B$SB.tow[tow.SPA1B$Year %in% SB.tow.missing.year$Year] <- SB.tow.missing.year$SB.tow

#in 2020 assume same tow numbers as in 2019 
tow.SPA1B[tow.SPA1B$Year==2020,2:9] <- tow.SPA1B[tow.SPA1B$Year==2019,2:9]
tow.SPA1B


##### COMMERCIAL I.CV

#simple mean estimates (years 1997 to 2006) 
years <- 1997:2008
X <- length(years)

se.SPA1B <- data.frame(Year=(years), 
                       CS.var=SPA1B.CS.CommWt.simple$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)],
                       MBNE.var=SPA1B.MBNE.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)],
                       MBNW.var=SPA1B.MBNW.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)],
                       C28.var=SPA1B.28C.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)],
                       Out.var=SPA1B.Out.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)],
                       AH.var=SPA1B.AH.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)],
                       SI.var=SPA1B.SI.CommWt.simple$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)],
                       SB.var=SPA1B.SB.CommWt.simple$var.y[which(SPA1B.CS.CommWt.simple$Year == 1997):which(SPA1B.CS.CommWt.simple$Year == 2008)])


se.SPA1B$sum.var <- (se.SPA1B$CS.var*191023.77^2)/tow.SPA1B$CS.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)] + 
  (se.SPA1B$MBNE.var*215314^2)/tow.SPA1B$MBNE.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)] + 
  (se.SPA1B$MBNW.var*78830.97^2)/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)] + 
  (se.SPA1B$C28.var*88000.2^2)/tow.SPA1B$C28.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)] + 
  (se.SPA1B$Out.var*56009.889^2)/tow.SPA1B$Out.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)] + 
  (se.SPA1B$AH.var*8626.27^2)/tow.SPA1B$AH.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)] + 
  (se.SPA1B$SB.var*19415.98^2)/tow.SPA1B$SB.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)] + 
  (se.SPA1B$SI.var*20337.96^2)/tow.SPA1B$SI.tow[which(tow.SPA1B$Year == 1997):which(tow.SPA1B$Year == 2008)]

se.SPA1B$se <- sqrt(se.SPA1B$sum.var)
se.SPA1B$I.cv <- se.SPA1B$se/(I$Bmass[which(I$Year==1997):which(I$Year==2008)]*1000000)
I.cv.upto2008 <- se.SPA1B %>% select(Year, cv = I.cv) 
I.cv.upto2008
round(I.cv.upto2008$cv,4)

#rename df to merge with below
#se.SPA1B.1 <- se.SPA1B

#! Need to take into account spr years that simple mean is used (e.g., MBNW)
#spr years
#spr: MBNE, C28, Out (simple in 2009), AH; MBNW: spr in 2009, 2010, 2012, 2015-2016
#simple mean CS, SI, SB


years <- 2009:surveyyear
X <- length(years)

se.SPA1B <- data.frame(Year=(years), 
                       CS.var=SPA1B.CS.CommWt.simple$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)], 
                       MBNE.var=SPA1B.MBNE.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)], 
                       MBNW.var=SPA1B.MBNW.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)],
                       C28.var=SPA1B.28C.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)],
                       Out.var=SPA1B.Out.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)],
                       AH.var=SPA1B.AH.CommWt$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)],
                       SI.var=SPA1B.SI.CommWt.simple$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)],
                       SB.var=SPA1B.SB.CommWt.simple$var.y[which(SPA1B.CS.CommWt.simple$Year == 2009):which(SPA1B.CS.CommWt.simple$Year == surveyyear)])

#UPDATES NEEDED EVERY YEAR CHECK SIMPLE MEANS OR SPR!!
#if simple means used, need to divide by number of tows
se.SPA1B$sum.var <- (se.SPA1B$CS.var*191023.77^2)/tow.SPA1B$CS.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==surveyyear)] + #Simple means for all years
      (se.SPA1B$MBNE.var*215314^2)+
      c((se.SPA1B$MBNW.var[which(se.SPA1B$Year==2009):which(se.SPA1B$Year==2010)]*78830.97^2),
        (se.SPA1B$MBNW.var[which(se.SPA1B$Year==2011)]*78830.97^2)/tow.SPA1B$MBNW.tow[tow.SPA1B$Year==2011], 
        se.SPA1B$MBNW.var[which(se.SPA1B$Year==2012)]*78830.97^2,
        (se.SPA1B$MBNW.var[which(se.SPA1B$Year==2013):which(se.SPA1B$Year==2014)]*78830.97^2)/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==2013):which(tow.SPA1B$Year==2014)],
        se.SPA1B$MBNW.var[which(se.SPA1B$Year==2015):which(se.SPA1B$Year==2017)]*78830.97^2, 
        (se.SPA1B$MBNW.var[which(se.SPA1B$Year==2018):which(se.SPA1B$Year==2020)]*78830.97^2)/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==2018):which(tow.SPA1B$Year==2020)],
        se.SPA1B$MBNW.var[which(se.SPA1B$Year==2021)]*78830.97^2, 
        (se.SPA1B$MBNW.var[which(se.SPA1B$Year==2022):which(se.SPA1B$Year==2023)]*78830.97^2)/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==2022):which(tow.SPA1B$Year==2023)],
        se.SPA1B$MBNW.var[which(se.SPA1B$Year==2024):which(se.SPA1B$Year==surveyyear)]*78830.97^2)+
      (se.SPA1B$C28.var*88000.2^2) +
      c((se.SPA1B$Out.var[which(se.SPA1B$Year==2009)]*56009.889^2)/tow.SPA1B$Out.tow[tow.SPA1B$Year==2009] , 
        se.SPA1B$Out.var[which(se.SPA1B$Year==2010):which(se.SPA1B$Year==2013)]*56009.889^2, 
        (se.SPA1B$Out.var[which(se.SPA1B$Year==2014)]*56009.889^2)/tow.SPA1B$Out.tow[tow.SPA1B$Year==2014], 
        se.SPA1B$Out.var[which(se.SPA1B$Year==2015):which(se.SPA1B$Year==2020)]*56009.889^2,
        (se.SPA1B$Out.var[which(se.SPA1B$Year==2021)]*56009.889^2)/tow.SPA1B$Out.tow[tow.SPA1B$Year==2021],
        se.SPA1B$Out.var[which(se.SPA1B$Year==2022):which(se.SPA1B$Year==surveyyear)]*56009.889^2) +
      (se.SPA1B$AH.var*8626.27^2) +
      (se.SPA1B$SB.var*19415.98^2)/tow.SPA1B$SB.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==surveyyear)] + #Simple means for all years
      (se.SPA1B$SI.var*20337.96^2)/tow.SPA1B$SI.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==surveyyear)] #Simple means for all years

se.SPA1B$se <- sqrt(se.SPA1B$sum.var)
se.SPA1B$I.cv <- se.SPA1B$se/(I$Bmass[which(I$Year==2009):which(I$Year==surveyyear)]*1000000)
I.cv.2009on <- se.SPA1B %>% select(Year, cv = I.cv) 
I.cv.2009on
round(I.cv.2009on$cv,4)

#merge 1997-2008 and 2009-current year
I.cv <- rbind(I.cv.upto2008, I.cv.2009on)
I.cv$cv <- round(I.cv$cv, 4)
I.cv 

##### RECRUIT IR.CV
#Simple means years

years <- 1997:2008 #these years just use simple mean 
X <- length(years)

se.SPA1B.rec <- data.frame(Year=(years), CS.var=SPA1B.CS.RecWt.simple$var.y[which(SPA1B.CS.RecWt.simple$Year==1997):which(SPA1B.CS.RecWt.simple$Year==2008)],
                           MBNE.var=SPA1B.MBNE.RecWt$var.y[which(SPA1B.MBNE.RecWt$Year==1997):which(SPA1B.MBNE.RecWt$Year==2008)],
                           MBNW.var=SPA1B.MBNW.RecWt$var.y[which(SPA1B.MBNW.RecWt$Year==1997):which(SPA1B.MBNW.RecWt$Year==2008)],
                           C28.var=SPA1B.28C.RecWt$var.y[which(SPA1B.28C.RecWt$Year==1997):which(SPA1B.28C.RecWt$Year==2008)],
                           Out.var=SPA1B.Out.RecWt.simple$var.y[which(SPA1B.Out.RecWt.simple$Year==1997):which(SPA1B.Out.RecWt.simple$Year==2008)],
                           AH.var=SPA1B.AH.RecWt$var.y[which(SPA1B.AH.RecWt$Year==1997):which(SPA1B.AH.RecWt$Year==2008)],
                           SI.var= SPA1B.SI.RecWt.simple$var.y[which(SPA1B.SI.RecWt.simple$Year==1997):which(SPA1B.SI.RecWt.simple$Year==2008)],
                           SB.var=SPA1B.SB.RecWt.simple$var.y[which(SPA1B.SB.RecWt.simple$Year==1997):which(SPA1B.SB.RecWt.simple$Year==2008)])

se.SPA1B.rec$sum.var <- (se.SPA1B.rec$CS.var*191023.77^2)/tow.SPA1B$CS.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)] + 
         (se.SPA1B.rec$MBNE.var*215314^2)/tow.SPA1B$MBNE.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)] + 
         (se.SPA1B.rec$MBNW.var*78830.97^2)/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)]+
         (se.SPA1B.rec$C28.var*88000.2^2)/tow.SPA1B$C28.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)] + 
         (se.SPA1B.rec$Out.var*56009.889^2)/tow.SPA1B$Out.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)] + 
         (se.SPA1B.rec$AH.var*8626.27^2)/tow.SPA1B$AH.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)] + 
         (se.SPA1B.rec$SB.var*20337.96^2)/tow.SPA1B$SB.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)] + 
         (se.SPA1B.rec$SI.var*19415.98^2)/tow.SPA1B$SI.tow[which(tow.SPA1B$Year==1997):which(tow.SPA1B$Year==2008)]

se.SPA1B.rec$se <- sqrt(se.SPA1B.rec$sum.var)
se.SPA1B.rec$IR.cv <- se.SPA1B.rec$se/(IR$Bmass[which(IR$Year==1997):which(IR$Year==2008)]*1000000)
round(se.SPA1B.rec$IR.cv,4) #0.2330 0.1098 0.1902 0.1431 0.1099 0.1799 0.1441 0.1065 0.1437 0.1820 0.1414 0.1240

#rename df to merge with below
IR.cv.upto2008 <- se.SPA1B.rec %>% select(Year, cv = IR.cv) 
IR.cv.upto2008
round(IR.cv.upto2008$cv, 4)



#spr years
#spr: MBNE (not spr in 2009,2001), MBNW (not spr in 2011,2013, 2014, 2018), C28, Out (not spr 2009-2017), AH
#simple mean CS, SI, SB
years <- 2009:surveyyear
X <- length(years)

se.SPA1B.rec <- data.frame(Year = (years), CS.var = SPA1B.CS.RecWt.simple$var.y[which(SPA1B.CS.RecWt.simple$Year==2009):which(SPA1B.CS.RecWt.simple$Year==surveyyear)],
                           MBNE.var = SPA1B.MBNE.RecWt$var.y[which(SPA1B.MBNE.RecWt$Year==2009):which(SPA1B.MBNE.RecWt$Year==surveyyear)],
                           MBNW.var = SPA1B.MBNW.RecWt$var.y[which(SPA1B.MBNW.RecWt$Year==2009):which(SPA1B.MBNW.RecWt$Year==surveyyear)],
                           C28.var = SPA1B.28C.RecWt$var.y[which(SPA1B.28C.RecWt$Year==2009):which(SPA1B.28C.RecWt$Year==surveyyear)],
                           Out.var = SPA1B.Out.RecWt.simple$var.y[which(SPA1B.Out.RecWt.simple$Year==2009):which(SPA1B.Out.RecWt.simple$Year==surveyyear)],
                           AH.var = SPA1B.AH.RecWt$var.y[which(SPA1B.AH.RecWt$Year==2009):which(SPA1B.AH.RecWt$Year==surveyyear)],
                           SI.var = SPA1B.SI.RecWt.simple$var.y[which(SPA1B.SI.RecWt.simple$Year==2009):which(SPA1B.SI.RecWt.simple$Year==surveyyear)],
                           SB.var = SPA1B.SB.RecWt.simple$var.y[which(SPA1B.SB.RecWt.simple$Year==2009):which(SPA1B.SB.RecWt.simple$Year==surveyyear)])

#UPDATES NEEDED EVERY YEAR CHECK SIMPLE MEANS OR SPR!!#

se.SPA1B.rec$sum.var <- (se.SPA1B.rec$CS.var*191023.77^2)/tow.SPA1B$CS.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==surveyyear)] + #2009 
        c(se.SPA1B.rec$MBNE.var[which(se.SPA1B.rec$Year==2009)]*215314^2/tow.SPA1B$MBNE.tow[which(tow.SPA1B$Year==2009)], #$2009
          (se.SPA1B.rec$MBNE.var[which(se.SPA1B.rec$Year==2010)]*215314^2),  #2010
          (se.SPA1B.rec$MBNE.var[which(se.SPA1B.rec$Year==2011)]*215314^2)/tow.SPA1B$MBNE.tow[which(tow.SPA1B$Year==2011)],  #2011
          (se.SPA1B.rec$MBNE.var[which(se.SPA1B.rec$Year==2012):which(se.SPA1B.rec$Year==2023)]*215314^2), #2012 to 2023
          se.SPA1B.rec$MBNE.var[which(se.SPA1B.rec$Year==2024):which(se.SPA1B.rec$Year==surveyyear)]*215314^2/tow.SPA1B$MBNE.tow[which(tow.SPA1B$Year==2024):which(tow.SPA1B$Year==surveyyear)]) +  
        c(se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2009):which(se.SPA1B.rec$Year==2010)]*78830.97^2, # 2009 and 2010: Used SPR mean
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2011)]*78830.97^2/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==2011)], # 2011: Simple
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2012)]*78830.97^2, #2012: SPR
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2013):which(se.SPA1B.rec$Year==2014)]*78830.97^2/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==2013):which(tow.SPA1B$Year==2014)], #2013-2014
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2015):which(se.SPA1B.rec$Year==2017)]*78830.97^2, #2015-2017
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2018):which(se.SPA1B.rec$Year==2020)]*78830.97^2/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==2018):which(tow.SPA1B$Year==2020)], #2018-2020 SIMPLE
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2021)]*78830.97^2,
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2022):which(se.SPA1B.rec$Year==2023)]*78830.97^2/tow.SPA1B$MBNW.tow[which(tow.SPA1B$Year==2022):which(tow.SPA1B$Year==2023)],
          se.SPA1B.rec$MBNW.var[which(se.SPA1B.rec$Year==2024):which(se.SPA1B.rec$Year==surveyyear)]*78830.97^2) +   
        (se.SPA1B.rec$C28.var*88000.2^2) +
        (se.SPA1B.rec$Out.var*56009.889^2)/tow.SPA1B$Out.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==surveyyear)]+ #in 2021, switched to using simple means for all years
        #c(se.SPA1B.rec$Out.var[which(se.SPA1B.rec$Year==2009):which(se.SPA1B.rec$Year==2017)]*56009.889^2/tow.SPA1B$Out.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==2017)],  #2009-2017
          #se.SPA1B.rec$Out.var[which(se.SPA1B.rec$Year==2018):which(se.SPA1B.rec$Year==2019)]*56009.889^2,
          #se.SPA1B.rec$Out.var[which(se.SPA1B.rec$Year==2020):which(se.SPA1B.rec$Year==2021)]*56009.889^2/tow.SPA1B$Out.tow[which(tow.SPA1B$Year==2020):which(tow.SPA1B$Year==2021)], #2020-2021 sm
          #se.SPA1B.rec$Out.var[which(se.SPA1B.rec$Year==2022):which(se.SPA1B.rec$Year==2024)]*56009.889^2, #2022-2024 spr
          #se.SPA1B.rec$Out.var[which(se.SPA1B.rec$Year==2025)]*56009.889^2/tow.SPA1B$Out.tow[which(tow.SPA1B$Year==2025)]) +  #2025 sm
        (se.SPA1B.rec$AH.var*8626.27^2) +
        (se.SPA1B.rec$SB.var*20337.96^2)/tow.SPA1B$SB.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==surveyyear)] +  #2009-2019
        (se.SPA1B.rec$SI.var*19415.98^2)/tow.SPA1B$SI.tow[which(tow.SPA1B$Year==2009):which(tow.SPA1B$Year==surveyyear)]  #2009-2019

se.SPA1B.rec$se <- sqrt(se.SPA1B.rec$sum.var)
se.SPA1B.rec$IR.cv <- se.SPA1B.rec$se/(IR$Bmass[which(IR$Year==2009):which(IR$Year==surveyyear)]*1000000)
round(se.SPA1B.rec$IR.cv,4)

#rename df to merge with below
IR.cv.2009on <- se.SPA1B.rec
IR.cv.2009on <- IR.cv.2009on %>% select(Year, cv = IR.cv)
I.cv.2009on
round(I.cv.2009on$cv,4)

#merge 1997-2008 and 2009-current year
IR.cv <- rbind(IR.cv.upto2008, IR.cv.2009on)
IR.cv$cv <- round(IR.cv$cv, 4)
IR.cv 

#Write out population model inputs N, I, IR, I.cv, IR.cv 
#check all years match up 
cbind(N, I, IR, I.cv, IR.cv)
#Bind into object for export 
SPA1B.population.model.input <- cbind(N, I %>% select(I = Bmass), IR %>% select(IR = Bmass), I.cv %>% select(I.cv = cv), IR.cv %>% select(IR.cv = cv))

#export 
write.csv(SPA1B.population.model.input, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1B.population.model.input.",surveyyear,".csv"))


####
### ---- Plot Numbers and weight per tow ----
###                   


###
### ----  Plot mean number per tow by strata group ----
###

# DO NOT PLOT 2020 data point since no survey data that year!!! 

#...Number per tow... 
data <- SPA1B.Numbers
data$Size <- data$Age
data$Mean.nums[data$Year==2020] <- NA 

#name strata for display  "CS"  "MBN" "28C" "Out" "AH"  "SI"  "SB" 
data$strata.name <- NA 
data$strata.name[data$Area=="CS"] <- "Cape Spencer"
data$strata.name[data$Area=="MBN"] <- "Mid Bay North"
data$strata.name[data$Area=="28C"] <- "Upper Bay (28C)"
data$strata.name[data$Area=="Out"] <- "28D Outer"
data$strata.name[data$Area=="AH"] <- "Advocate Harbour"
data$strata.name[data$Area=="SI"] <- "Spencer's Island"
data$strata.name[data$Area=="SB"] <- "Scots Bay"
  
#restrict ts to those year only with data (don't show interpolated data) 
#CS - full TS 
#MBN - full TS 
#28C (2001+)
data$Mean.nums[data$Year < 2001 & data$Area=="28C"] <- NA
#Outer (2001+)
data$Mean.nums[data$Year < 2001 & data$Area=="Out"] <- NA
#AH (2001+)
data$Mean.nums[data$Year < 2001 & data$Area=="AH"] <- NA
#SI (2005+)
data$Mean.nums[data$Year < 2005 & data$Area=="SI"] <- NA
#SB (2005+)
data$Mean.nums[data$Year < 2005 & data$Area=="SB"] <- NA

num.per.tow.full.ts <- ggplot(data = data, aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + facet_wrap(~strata.name,  ncol=1) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.05, 0.95)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
num.per.tow.full.ts

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_NumberPerTow_byStrata.png"), type="cairo", width=35, height=25, units = "cm", res=300)
num.per.tow.full.ts
dev.off()


#...weight per tow... 
data.kg <- SPA1B.Weight
data.kg$Size <- data.kg$Age
data.kg$kg.tow[data.kg$Year==2020] <- NA 

#name strata for display  "CS"  "MBN" "28C" "Out" "AH"  "SI"  "SB" 
data.kg$strata.name <- NA 
data.kg$strata.name[data.kg$Area=="CS"] <- "Cape Spencer"
data.kg$strata.name[data.kg$Area=="MBN"] <- "Mid Bay North"
data.kg$strata.name[data.kg$Area=="28C"] <- "Upper Bay (28C)"
data.kg$strata.name[data.kg$Area=="Out"] <- "28D Outer"
data.kg$strata.name[data.kg$Area=="AH"] <- "Advocate Harbour"
data.kg$strata.name[data.kg$Area=="SI"] <- "Spencer's Island"
data.kg$strata.name[data.kg$Area=="SB"] <- "Scots Bay"

#restrict ts to those year only with data (don't show interpolated data) 
#CS - full TS 
#MBN - full TS 
#28C (2001+)
data.kg$kg.tow[data.kg$Year < 2001 & data.kg$Area=="28C"] <- NA
#Outer (2001+)
data.kg$kg.tow[data.kg$Year < 2001 & data.kg$Area=="Out"] <- NA
#AH (2001+)
data.kg$kg.tow[data.kg$Year < 2001 & data.kg$Area=="AH"] <- NA
#SI (2005+)
data.kg$kg.tow[data.kg$Year < 2005 & data.kg$Area=="SI"] <- NA
#SB (2005+)
data.kg$kg.tow[data.kg$Year < 2005 & data.kg$Area=="SB"] <- NA


wt.per.tow.full.ts <- ggplot(data = data.kg, aes(x=Year, y= kg.tow, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + facet_wrap(~strata.name, ncol=1) + 
  theme_bw() + ylab("Mean kg/tow") + xlab("Year") + 
  theme(legend.position = c(0.05, 0.95)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
wt.per.tow.full.ts

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_WeightPerTow_byStrata.png"), type="cairo", width=35, height=25, units = "cm", res=300)
wt.per.tow.full.ts
dev.off()

###
## Survey biomass by strata 2 panel 
### 


biomass.by.strata.comm <- ggplot(data = data.kg %>% filter(Size=="Commercial"), aes(x=Year, y= Bmass, col=strata.name, pch=strata.name)) + 
  geom_point() + 
  geom_line(aes(linetype = strata.name))  + 
  theme_bw() + ylab("Survey biomass (mt)") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.75)) #+ 
  #scale_linetype_manual(values=c("solid", "dotted"))+
  #scale_color_manual( values=c('black','red'))
biomass.by.strata.comm

biomass.by.strata.rec <- ggplot(data = data.kg %>% filter(Size=="Recruit"), aes(x=Year, y= Bmass, col=strata.name, pch=strata.name)) + 
  geom_point() + 
  geom_line(aes(linetype = strata.name))  + 
  theme_bw() + ylab("Survey biomass (mt)") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.75)) #+ 
#scale_linetype_manual(values=c("solid", "dotted"))+
#scale_color_manual( values=c('black','red'))
biomass.by.strata.rec


png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_SurveyBiomass_byStrata",surveyyear,".png"), type="cairo", width=35, height=25, units = "cm", res=300)
plot_grid(biomass.by.strata.comm, biomass.by.strata.rec, 
          nrow = 2, label_x = 0.15, label_y = 0.95, labels = c('Commercial Size', 'Recruit Size'))
dev.off()


###
## Survey numbers by strata 2 panel 
### 


numbers.by.strata.comm <- ggplot(data = data %>% filter(Size=="Commercial"), aes(x=Year, y= Mean.nums, col=strata.name, pch=strata.name)) + 
  geom_point() + 
  geom_line(aes(linetype = strata.name))  + 
  theme_bw() + ylab("Survey Numbers per tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.75)) #+
#scale_linetype_manual(values=c("solid", "dotted"))+
#scale_color_manual( values=c('black','red'))
numbers.by.strata.comm

numbers.by.strata.rec <- ggplot(data = data %>% filter(Size=="Recruit"), aes(x=Year, y= Mean.nums, col=strata.name, pch=strata.name)) + 
  geom_point() + 
  geom_line(aes(linetype = strata.name))  + 
  theme_bw() + ylab("Survey Numbers per tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.75)) #+ 
#scale_linetype_manual(values=c("solid", "dotted"))+
#scale_color_manual( values=c('black','red'))
numbers.by.strata.rec


png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_SurveyNumbers_byStrata",surveyyear,".png"), type="cairo", width=35, height=25, units = "cm", res=300)
plot_grid(numbers.by.strata.comm, numbers.by.strata.rec, 
          nrow = 2, label_x = 0.15, label_y = 0.95, labels = c('Commercial Size', 'Recruit Size'))
dev.off()



###
### ---- Plot Survey Numbers and Biomass for all 1B----
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
  theme(legend.position = c(0.1, 0.9)) + 
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
  theme(legend.position = c(0.1, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
survey.biomass

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1B_SurveyNumbersAndBiomass",surveyyear,".png"), type="cairo", width=30, height=25, units = "cm", res=300)
plot_grid(survey.numbers, survey.biomass, 
          nrow = 2, label_x = 0.15, label_y = 0.95)
dev.off() 




