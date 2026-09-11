###........................................###
###
###    SPA3
###    Numbers per tow, Weight per tow
###    Population numbers and biomass
###    Revamped July 2021 J.Sameoto
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(PBSmapping)
library(spr) #version 1.04

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

#FYI Strata IDs from db in SPA 3, but remember, we use VMS strata and those strata defs aren't in the database
#strata.spa3<-c(22:24)

# Define: 
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#uid <- un.sameotoj
#pwd <- pw.sameotoj
surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "3"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6"
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"


#Query shell height data from database
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')
live.freq.query <- ("SELECT * FROM scallsur.scliveres WHERE strata_id IN (22,23,24)")
cross.ref.query <-  ("SELECT * FROM scallsur.sccomparisontows WHERE sccomparisontows.cruise LIKE 'BI%' AND COMP_TYPE= 'R'")
BIlivefreq.dat <- dbGetQuery(chan, live.freq.query)
crossref.spa3 <- dbGetQuery(chan,cross.ref.query)
dbDisconnect(chan)

#add YEAR and CRUISEID column to data
BIlivefreq.dat$YEAR <- as.numeric(substr(BIlivefreq.dat$CRUISE,3,6))
BIlivefreq.dat$CruiseID <- paste(BIlivefreq.dat$CRUISE,BIlivefreq.dat$TOW_NO,sep='.') #will be used to assign strata_id to cross ref files


# read in meat weight data; this is output from the meat weight/shell height modelling
#code for reading in multiple csvs at once and combining into one dataframe from D.Keith
Year <- c(seq(1996,2019),seq(2021,surveyyear))
num.years <- length(Year)

BIliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/BIliveweight",Year[i],".csv",sep="") ,header=T)
  BIliveweight <- rbind( BIliveweight,temp)
}

#check data structure
summary(BIliveweight)
str(BIliveweight)
# You may lose column names in the rbind step if so just do this.
#colnames(data) <- c("Year","ID","etc")

unique(BIliveweight$YEAR)
table(BIliveweight$YEAR)


# ---- post-stratify SPA3 for VMS strata ----
#polygon to for assigning new strata to data
#spa3area <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA3_VMSpoly.csv")
#spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")
spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")

#adjust data files for subsequent analysis
BIlivefreq.dat$lat<-convert.dd.dddd(BIlivefreq.dat$START_LAT)
BIlivefreq.dat$lon<-convert.dd.dddd(BIlivefreq.dat$START_LONG)
BIlivefreq.dat$ID<-1:nrow(BIlivefreq.dat)

BIliveweight$lat<-convert.dd.dddd(BIliveweight$START_LAT)
BIliveweight$lon<-convert.dd.dddd(BIliveweight$START_LONG)
BIliveweight$ID<-1:nrow(BIliveweight)
BIliveweight$CRUISE.ID <- paste0(BIliveweight$CRUISE,".",BIliveweight$TOW_NO)

#Check same number of records in BIlivefreq.dat and BIliveweight for cruises from 1996 on: 
table(BIlivefreq.dat$CRUISE) 
table(BIliveweight$CRUISE) 

#identify tows "inside" the VMS strata and assign new STRATA_ID to them (99)
events <- subset(BIlivefreq.dat,STRATA_ID%in%23:24,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
BIlivefreq.dat$STRATA_ID[BIlivefreq.dat$ID%in%findPolys(events,spa3area)$EID]<-99

events <- subset(BIliveweight,STRATA_ID%in%23:24,c("ID","lon","lat"))
names(events)<-c("EID","X","Y")
BIliveweight$STRATA_ID[BIliveweight$ID%in%findPolys(events,spa3area)$EID]<-99


#Create ID column on cross reference files for spr
crossref.spa3$CruiseID <- paste(crossref.spa3$CRUISE_REF,crossref.spa3$TOW_NO_REF,sep='.')  #create CRUISE_ID on "parent" tow
crossref.spa3$CruiseID.current <- paste0(crossref.spa3$CRUISE,".",crossref.spa3$TOW_NO) #create CRUISE_ID on "child - current year" tow

#Check for potential errors that will break the SPR estimates
#merge STRATA_ID from BIlivefreq to the crosssref files based on parent/reference tow and find those that don't match for their strata_ID 
crossref.spa3 <- left_join(crossref.spa3, BIlivefreq.dat %>% dplyr::select(CruiseID, STRATA_ID, TOW_TYPE_ID), by="CruiseID")

crossref.spa3 <- left_join(crossref.spa3, BIlivefreq.dat %>% dplyr::select(CruiseID, STRATA_ID.current = STRATA_ID, TOW_TYPE_ID.current = TOW_TYPE_ID), by=c("CruiseID.current" = "CruiseID"))

crossref.spa3$strata_diff <- crossref.spa3$STRATA_ID -  crossref.spa3$STRATA_ID.current

#Strata IDs that need fixing 
crossref.spa3 %>% filter(strata_diff!=0)

#tows that are not tow type 1 or 5 but are repeats that should not have been!
crossref.spa3.tow.type.2 <- crossref.spa3 %>% filter(!TOW_TYPE_ID%in%c(1,5) | !TOW_TYPE_ID.current%in%c(1,5))

##Subset data for individual years for SPR command (ONLY TOW_TYPE==1 AND  ONLY TOW_TYPE==5)
tows <- c(1,5)
#livefreq 
year <- c(seq(2006,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(BIlivefreq.dat, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("livefreq", i), sub)
}

#liveweight 
year <- c(seq(2006,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(BIliveweight, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("liveweight", i), sub)
}

#Crossref 
year <- c(seq(2007,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(crossref.spa3, CRUISE==paste0("BI",i))
  assign(paste0("crossref.spa3.", i), sub)
}

####
# ---- correct data-sets for errors -----
####
#some repeated tows will not be assigned to the same strata as the "parent" tow due to where the START LAT and LONG are
#repeated tows should be corrected to match parent tow
#some experimental tows were used as parent tows for repeats, these should be removed

#1. Tow 19 in 2007 (159 in 2006) not assigned to 23
#   Tow 12 in 2007 (73 in 2006) not assigned to 23
# BI2007.12,  BI2007.19
livefreq2007$STRATA_ID[livefreq2007$TOW_NO %in% c(12,19)] <- 23
liveweight2007$STRATA_ID[liveweight2007$TOW_NO %in% c(12,19)] <- 23

#2. In 2008 tow 10 (77 in 2007) not assigned to 22; BI2008.10
livefreq2008$STRATA_ID[livefreq2008$TOW_NO %in% c(10) ] <- 22
liveweight2008$STRATA_ID[liveweight2008$TOW_NO %in% c(10) ] <- 22

#3. In 2009, tow 52 (repeat of tow 12 in 2008) was not assigned to 99.; BI2009.52
livefreq2009$STRATA_ID[livefreq2009$TOW_NO %in% c(52) ] <- 99
liveweight2009$STRATA_ID[liveweight2009$TOW_NO %in% c(52) ] <- 99

#4. Tow 116 in 2008 (140 in 2009)  <- 2008 tow was adaptive (TOW_TYPE_ID==2) and can't be used
#   tow 117 in 2008 (138 in 2009) <-2008 tows was adaptive (TOW_TYPE_ID==2) and can't be used
crossref.spa3.2009 <- crossref.spa3.2009[!crossref.spa3.2009$TOW_NO%in%c(140,138),]

#5. In 2009, tow 52 (repeated in 2010 as tow 51) was not assigned to 99, so the crossref file was not properly assigned.
crossref.spa3.2010$STRATA_ID[crossref.spa3.2010$CRUISE_REF=="BI2009"&crossref.spa3.2010$TOW_NO_REF==52] <- 99

#6. In 2010, tow 90 (repeated in 2011 as tow 66) is experimental (TOW_TYPE_ID==3). Removed from crossref
crossref.spa3.2011 <- crossref.spa3.2011[!(crossref.spa3.2011$CRUISE_REF=="BI2010"&crossref.spa3.2011$TOW_NO_REF==90),]

#7. In 2011, Tows 54,58,67 were experimental but repeated in 2012.
crossref.spa3.2012 <- crossref.spa3.2012[!(crossref.spa3.2012$CRUISE_REF=="BI2011"&crossref.spa3.2012$TOW_NO_REF%in%c(54,58,67)),]

#8. In 2013, tow 35 (repeat of tow 50 in 2012) was not assigned to 99.
#In 2013, tow 85 (repeat of tow 109 in 2012) was not assigned to 99.
#In 2013, tow 101 (repeat of tow 1118 in 2012) was not assigned to 99.
livefreq2013$STRATA_ID[livefreq2013$TOW_NO%in%c(35,85,101)] <- 99
liveweight2013$STRATA_ID[liveweight2013$TOW_NO%in%c(35,85,101)] <- 99

#9. In 2013 tow 109 (tow 92 in 2012) not assigned to 23
livefreq2013$STRATA_ID[livefreq2013$TOW_NO%in%c(109)] <- 23
liveweight2013$STRATA_ID[liveweight2013$TOW_NO%in%c(109)] <- 23

#10. In 2016, tow 90 (96 in 2015) not assigned to 99
livefreq2016$STRATA_ID[livefreq2016$TOW_NO%in%c(90)] <- 99
liveweight2016$STRATA_ID[liveweight2016$TOW_NO%in%c(90)] <- 99

#11. In 2017, tow 28 (4 in 2016) not assigned to 99
#In 2017, tow 61 (47 in 2016) not assigned to 99
#In 2017, tow 75 (69 in 2016) not assigned to 99
livefreq2017$STRATA_ID[livefreq2017$TOW_NO%in%c(28,61,75)] <-99
liveweight2017$STRATA_ID[liveweight2017$TOW_NO%in%c(28,61,75)] <-99

#12. In 2022, Tows 82 and 110 were experimental but repeated in 2023 (Tows 70 and 82 respectively).
crossref.spa3.2023 <- crossref.spa3.2023[!(crossref.spa3.2023$CRUISE_REF=="BI2022"&crossref.spa3.2023$TOW_NO_REF%in%c(82,110)),]

#write.csv(livefreq2017 %>% select(YEAR, CruiseID, CRUISE, TOW_NO, STRATA_ID), paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3.Corrected.SPR.STRATA_IDs.csv"))

###
#---- RECRUIT AND COMMERCIAL NUMBERS PER TOW AND POPULATION NUMBERS ----
###


#---- St. Mary's Bay ----

# Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 1991:surveyyear
X <- length(years)

SPA3.SMB.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep (117875.8905,X), Area=rep("SMB", X), Age=rep("Recruit", X))
for(i in 1:length(SPA3.SMB.Rec.simple$Year)){
  temp.data <- BIlivefreq.dat[BIlivefreq.dat$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.SMB.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
	SPA3.SMB.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA3.SMB.Rec.simple

#interpolate for missing data
approx(SPA3.SMB.Rec.simple$Year, SPA3.SMB.Rec.simple$Mean.nums, xout=1997) # 4.629825
approx(SPA3.SMB.Rec.simple$Year, SPA3.SMB.Rec.simple$Mean.nums, xout=1998) # 7.309649
approx(SPA3.SMB.Rec.simple$Year, SPA3.SMB.Rec.simple$Mean.nums, xout=2002) # 1.814875
approx(SPA3.SMB.Rec.simple$Year, SPA3.SMB.Rec.simple$Mean.nums, xout=2003) # 1.949104
approx(SPA3.SMB.Rec.simple$Year, SPA3.SMB.Rec.simple$Mean.nums, xout=2020) #  6.037719

#add to dataframe
SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==1997,c(2,3)]<-c(4.62982, 7.605) #assume var from 1996
SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==1998,c(2,3)]<-c(7.309649, 7.605) #assume var from 1996
SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2002,c(2,3)]<-c(1.814875, 26.207) #assume var from 2001
SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2003,c(2,3)]<-c(1.949104, 26.207) #assume var from 2001
SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2020,c(2,3)]<-c(6.037719, 253.46952) #assume var from 2019

#spr(Test for 2015, correlate with prerecruits: 50-65 mm)

#dataframe for SPR estimates Recruits
spryears <- 2007:surveyyear 
Y <- length(spryears)
SMB.rec.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007
smbrec1<-spr(livefreq2006$TOW_NO[livefreq2006$STRATA_ID==22],apply(livefreq2006[livefreq2006$STRATA_ID==22,21:23],1,sum),
livefreq2007$TOW_NO[livefreq2007$STRATA_ID==22],apply(livefreq2007[livefreq2007$STRATA_ID==22,24:26],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 7 repeats in SMB ; TOW_NO_REF TOW_NO

K<-summary (smbrec1)
SMB.rec.spr.est[SMB.rec.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008 spr
smbrec2<-spr(livefreq2007$TOW_NO[livefreq2007$STRATA_ID==22],apply(livefreq2007[livefreq2007$STRATA_ID==22,21:23],1,sum),
livefreq2008$TOW_NO[livefreq2008$STRATA_ID==22],apply(livefreq2008[livefreq2008$STRATA_ID==22,24:26],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 7 repeats in SMB

K<-summary (smbrec2)
SMB.rec.spr.est[SMB.rec.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009 spr
smbrec3<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==22],apply(livefreq2008[livefreq2008$STRATA_ID==22,21:23],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID==22],apply(livefreq2009[livefreq2009$STRATA_ID==22,24:26],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 7 repeats in SMB

K<-summary (smbrec3)
SMB.rec.spr.est[SMB.rec.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
smbrec4<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==22],apply(livefreq2009[livefreq2009$STRATA_ID==22,21:23],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID==22],apply(livefreq2010[livefreq2010$STRATA_ID==22,24:26],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) #10 repeats in SMB

K<-summary (smbrec4)
SMB.rec.spr.est[SMB.rec.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
smbrec5<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==22],apply(livefreq2010[livefreq2010$STRATA_ID==22,21:23],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==22],apply(livefreq2011[livefreq2011$STRATA_ID==22,24:26],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 6 repeats in SMB

K<-summary (smbrec5)#
SMB.rec.spr.est[SMB.rec.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
smbrec6<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==22],apply(livefreq2011[livefreq2011$STRATA_ID==22,21:23],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==22],apply(livefreq2012[livefreq2012$STRATA_ID==22,24:26],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 9 repeats in SMB

K<-summary (smbrec6)  #
SMB.rec.spr.est[SMB.rec.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
smbrec7<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==22],apply(livefreq2012[livefreq2012$STRATA_ID==22,21:23],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==22],apply(livefreq2013[livefreq2013$STRATA_ID==22,24:26],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 5 repeats in SMB

K<-summary (smbrec7)  #
SMB.rec.spr.est[SMB.rec.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
smbrec8<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==22],apply(livefreq2013[livefreq2013$STRATA_ID==22,21:23],1,sum),
          livefreq2014$TOW_NO[livefreq2014$STRATA_ID==22],apply(livefreq2014[livefreq2014$STRATA_ID==22,24:26],1,sum),
          crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 5 repeats in SMB

K<-summary (smbrec8) #
SMB.rec.spr.est[SMB.rec.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
smbrec9<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==22],apply(livefreq2014[livefreq2014$STRATA_ID==22,21:23],1,sum),
          livefreq2015$TOW_NO[livefreq2015$STRATA_ID==22],apply(livefreq2015[livefreq2015$STRATA_ID==22,24:26],1,sum),
          crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 3 repeats in SMB

K<-summary (smbrec9) #58.01062
SMB.rec.spr.est[SMB.rec.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
smbrec10<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==22],apply(livefreq2015[livefreq2015$STRATA_ID==22,21:23],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID==22],apply(livefreq2016[livefreq2016$STRATA_ID==22,24:26],1,sum),
             crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 6 repeats in SMB

K<-summary (smbrec10) 
SMB.rec.spr.est[SMB.rec.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
smbrec11<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==22],apply(livefreq2016[livefreq2016$STRATA_ID==22,21:23],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID==22],apply(livefreq2017[livefreq2017$STRATA_ID==22,24:26],1,sum),
             crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) #5 repeats in SMB

K<-summary (smbrec11)  #Note no recruits found in the 5 repeated tows in 2017 in SMB - need to use SIMPLE estimate
#plot (smbrec11) 
SMB.rec.spr.est[SMB.rec.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#livefreq2017[livefreq2017$TOW_NO%in%c(118,123,124,126,129),21:23]

#2017/2018
smbrec12<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==22],apply(livefreq2017[livefreq2017$STRATA_ID==22,21:23],1,sum),
              livefreq2018$TOW_NO[livefreq2018$STRATA_ID==22],apply(livefreq2018[livefreq2018$STRATA_ID==22,24:26],1,sum),
              crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 3 repeats in SMB

K<-summary (smbrec12)  #Note no recruits found in the 3 repeated tows in 2018 in SMB - need to use SIMPLE estimate
#plot (smbrec11) 
SMB.rec.spr.est[SMB.rec.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
smbrec13<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==22],apply(livefreq2018[livefreq2018$STRATA_ID==22,21:23],1,sum),
              livefreq2019$TOW_NO[livefreq2019$STRATA_ID==22],apply(livefreq2019[livefreq2019$STRATA_ID==22,24:26],1,sum),
              crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 7 repeats in SMB

K<-summary (smbrec13) 
#plot (smbrec13) 
SMB.rec.spr.est[SMB.rec.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2020
##NO DATA IN 2020##

#2019/2021 Since no data in 2020 SPR in 2021 is cross referencing against 2019 tows 
#CAN'T USE BC ALL TOWS IN 2019 had ZERO recruits must use simple mean 
#smbrec14<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==22],apply(livefreq2019[livefreq2019$STRATA_ID==22,21:23],1,sum),
#              livefreq2021$TOW_NO[livefreq2021$STRATA_ID==22],apply(livefreq2021[livefreq2021$STRATA_ID==22,24:26],1,sum),
#              crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 7 repeats in SMB
#K<-summary(smbrec14) 
#plot (smbrec14) 
#SMB.rec.spr.est[SMB.rec.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
# See if tows assocaited with given year were all zero or not 
#test <- livefreq2019 %>% filter(TOW_NO %in% (crossref.spa3.2021$TOW_NO_REF[crossref.spa3.2021$STRATA_ID==22])) 
#apply(test[test$STRATA_ID==22,21:23],1,sum)

#2021/2022
smbrec15 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==22],apply(livefreq2021[livefreq2021$STRATA_ID==22,21:23],1,sum),
              livefreq2022$TOW_NO[livefreq2022$STRATA_ID==22],apply(livefreq2022[livefreq2022$STRATA_ID==22,24:26],1,sum),
              crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 6 repeats in SMB

K<-summary(smbrec15) 
#plot (smbrec13) 
SMB.rec.spr.est[SMB.rec.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
smbrec16 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==22],apply(livefreq2022[livefreq2022$STRATA_ID==22,21:23],1,sum),
                livefreq2023$TOW_NO[livefreq2023$STRATA_ID==22],apply(livefreq2023[livefreq2023$STRATA_ID==22,24:26],1,sum),
                crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 

K<-summary(smbrec16) 
#plot(smbrec16) 
SMB.rec.spr.est[SMB.rec.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
#smbrec17 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==22],apply(livefreq2023[livefreq2023$STRATA_ID==22,21:23],1,sum),
#                livefreq2024$TOW_NO[livefreq2024$STRATA_ID==22],apply(livefreq2024[livefreq2024$STRATA_ID==22,24:26],1,sum),
#                crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 

#K<-summary(smbrec17) 
#plot(smbrec17) 
#SMB.rec.spr.est[SMB.rec.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#data.frame(tow = livefreq2023$TOW_NO[livefreq2023$STRATA_ID==22],num = apply(livefreq2023[livefreq2023$STRATA_ID==22,21:23],1,sum))
#data.frame(tow = livefreq2024$TOW_NO[livefreq2024$STRATA_ID==22],num = apply(livefreq2024[livefreq2024$STRATA_ID==22,24:26],1,sum))
#crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")
#all cross ref tows from 2023 were 0, so can't use SPR for 2024 


#2024/2025
smbrec18 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==22],apply(livefreq2024[livefreq2024$STRATA_ID==22,21:23],1,sum),
                livefreq2025$TOW_NO[livefreq2025$STRATA_ID==22],apply(livefreq2025[livefreq2025$STRATA_ID==22,24:26],1,sum),
                crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 

K<-summary(smbrec18) 
#plot(smbrec16) 
SMB.rec.spr.est[SMB.rec.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
smbrec19 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==22],apply(livefreq2025[livefreq2025$STRATA_ID==22,21:23],1,sum),
                livefreq2026$TOW_NO[livefreq2026$STRATA_ID==22],apply(livefreq2026[livefreq2026$STRATA_ID==22,24:26],1,sum),
                crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")]) # 

K<-summary(smbrec19)
SMB.rec.spr.est[SMB.rec.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


SMB.rec.spr.est

#make dataframe for all of SMB Commercial (must match recruit dataframe)
SMB.rec.spr.est$method <- "spr"
SMB.rec.spr.est$NH <- 117875.8905
SMB.rec.spr.est$Area <- "SMB"
SMB.rec.spr.est$Age <- "Recruit"
names(SMB.rec.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

#combine simple and spr estimates
SPA3.SMB.Rec <- rbind(SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year<2007,], SMB.rec.spr.est)
SPA3.SMB.Rec[SPA3.SMB.Rec$Year==2017,] <- SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2017,] ##Note no recruits found in the 5 repeated tows in 2017 in SMB - need to use SIMPLE estimate
SPA3.SMB.Rec[SPA3.SMB.Rec$Year==2018,] <- SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2018,] ##Note no recruits found in the 3 repeated tows in 2018 in SMB - need to use SIMPLE estimate
SPA3.SMB.Rec[SPA3.SMB.Rec$Year==2020,] <- SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2020,] 
SPA3.SMB.Rec[SPA3.SMB.Rec$Year==2021,] <- SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2021,] 
SPA3.SMB.Rec[SPA3.SMB.Rec$Year==2024,] <- SPA3.SMB.Rec.simple[SPA3.SMB.Rec.simple$Year==2024,] 



SPA3.SMB.Rec$cv <- sqrt(SPA3.SMB.Rec$var.y)/SPA3.SMB.Rec$Mean.nums
SPA3.SMB.Rec$Pop <- SPA3.SMB.Rec$Mean.nums*SPA3.SMB.Rec$NH
SPA3.SMB.Rec

############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

# Simple means for Non-spr years use but calculate for all years to see impact of SPR estimator 
years <- 1991:surveyyear
X <- length(years)

SPA3.SMB.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(117875.8905, X), Area=rep("SMB", X), Age=rep("Comm", X))
for(i in 1:length(SPA3.SMB.Comm.simple$Year))  {
  temp.data <- BIlivefreq.dat[BIlivefreq.dat$YEAR==1990+i,]
SPA3.SMB.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
SPA3.SMB.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA3.SMB.Comm.simple

#interpolate for missing data
approx(SPA3.SMB.Comm.simple$Year, SPA3.SMB.Comm.simple$Mean.nums, xout=1997) # 28.825
approx(SPA3.SMB.Comm.simple$Year, SPA3.SMB.Comm.simple$Mean.nums, xout=1998) # 36.199
approx(SPA3.SMB.Comm.simple$Year, SPA3.SMB.Comm.simple$Mean.nums, xout=2002) # 29.993
approx(SPA3.SMB.Comm.simple$Year, SPA3.SMB.Comm.simple$Mean.nums, xout=2003) # 23.219
approx(SPA3.SMB.Comm.simple$Year, SPA3.SMB.Comm.simple$Mean.nums, xout=2020) # 92.66965

#add to dataframe
SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year==1997,c(2,3)]<-c(28.825, 920.20) #assume var from 1996
SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year==1998,c(2,3)]<-c(36.199, 920.20) #assume var from 1996
SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year==2002,c(2,3)]<-c(29.993, 2239.06) #assume var from 2001
SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year==2003,c(2,3)]<-c(23.219, 2239.06) #assume var from 2001
SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year==2020,c(2,3)]<-c(92.66965, 50818.5555) #assume var from 2019

#spr

#dataframe for SPR estimates
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
SMB.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007
smb1<-spr(livefreq2006$TOW_NO[livefreq2006$STRATA_ID==22],apply(livefreq2006[livefreq2006$STRATA_ID==22,24:50],1,sum),
livefreq2007$TOW_NO[livefreq2007$STRATA_ID==22],apply(livefreq2007[livefreq2007$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb1)  #37.36
SMB.spr.est[SMB.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008 spr
smb2<-spr(livefreq2007$TOW_NO[livefreq2007$STRATA_ID==22],apply(livefreq2007[livefreq2007$STRATA_ID==22,24:50],1,sum),
livefreq2008$TOW_NO[livefreq2008$STRATA_ID==22],apply(livefreq2008[livefreq2008$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb2, summary (smb1))   #16.68
SMB.spr.est[SMB.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009 spr
smb3<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==22],apply(livefreq2008[livefreq2008$STRATA_ID==22,24:50],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID==22],apply(livefreq2009[livefreq2009$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb3, summary (smb2, summary (smb1)))  #27.59472
SMB.spr.est[SMB.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
smb4<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==22],apply(livefreq2009[livefreq2009$STRATA_ID==22,24:50],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID==22],apply(livefreq2010[livefreq2010$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb4, summary (smb3, summary (smb2, summary (smb1))))  #19.24782
SMB.spr.est[SMB.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
smb5<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==22],apply(livefreq2010[livefreq2010$STRATA_ID==22,24:50],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==22],apply(livefreq2011[livefreq2011$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1)))))#38.31
SMB.spr.est[SMB.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
smb6<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==22],apply(livefreq2011[livefreq2011$STRATA_ID==22,24:50],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==22],apply(livefreq2012[livefreq2012$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))  # 63.12
SMB.spr.est[SMB.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
smb7<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==22],apply(livefreq2012[livefreq2012$STRATA_ID==22,24:50],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==22],apply(livefreq2013[livefreq2013$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1)))))))  # 62.2
SMB.spr.est[SMB.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
smb8<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==22],apply(livefreq2013[livefreq2013$STRATA_ID==22,24:50],1,sum),
          livefreq2014$TOW_NO[livefreq2014$STRATA_ID==22],apply(livefreq2014[livefreq2014$STRATA_ID==22,27:50],1,sum),
          crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb8,summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1)))))))) #89.71
SMB.spr.est[SMB.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
smb9<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==22],apply(livefreq2014[livefreq2014$STRATA_ID==22,24:50],1,sum),
          livefreq2015$TOW_NO[livefreq2015$STRATA_ID==22],apply(livefreq2015[livefreq2015$STRATA_ID==22,27:50],1,sum),
          crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))))) #58.01062
SMB.spr.est[SMB.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
smb10<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==22],apply(livefreq2015[livefreq2015$STRATA_ID==22,24:50],1,sum),
          livefreq2016$TOW_NO[livefreq2016$STRATA_ID==22],apply(livefreq2016[livefreq2016$STRATA_ID==22,27:50],1,sum),
          crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1)))))))))) 
SMB.spr.est[SMB.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
smb11<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==22],apply(livefreq2016[livefreq2016$STRATA_ID==22,24:50],1,sum),
          livefreq2017$TOW_NO[livefreq2017$STRATA_ID==22],apply(livefreq2017[livefreq2017$STRATA_ID==22,27:50],1,sum),
          crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))))))) 
SMB.spr.est[SMB.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
smb12<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==22],apply(livefreq2017[livefreq2017$STRATA_ID==22,24:50],1,sum),
           livefreq2018$TOW_NO[livefreq2018$STRATA_ID==22],apply(livefreq2018[livefreq2018$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1)))))))))))) 
SMB.spr.est[SMB.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
smb13<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==22],apply(livefreq2018[livefreq2018$STRATA_ID==22,24:50],1,sum),
           livefreq2019$TOW_NO[livefreq2019$STRATA_ID==22],apply(livefreq2019[livefreq2019$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb13, summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))))))))) 
SMB.spr.est[SMB.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#No survey in 2020; survey in 2021 repeats of 2019 
#2019/2021
smb14<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==22],apply(livefreq2019[livefreq2019$STRATA_ID==22,24:50],1,sum),
           livefreq2021$TOW_NO[livefreq2021$STRATA_ID==22],apply(livefreq2021[livefreq2021$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb14, summary(smb13, summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))))))))))
SMB.spr.est[SMB.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2021/2022
smb15<-spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==22],apply(livefreq2021[livefreq2021$STRATA_ID==22,24:50],1,sum),
           livefreq2022$TOW_NO[livefreq2022$STRATA_ID==22],apply(livefreq2022[livefreq2022$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb15, summary(smb14, summary(smb13, summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))))))))))) 
SMB.spr.est[SMB.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
smb16<-spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==22],apply(livefreq2022[livefreq2022$STRATA_ID==22,24:50],1,sum),
           livefreq2023$TOW_NO[livefreq2023$STRATA_ID==22],apply(livefreq2023[livefreq2023$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb16, summary(smb15, summary(smb14, summary(smb13, summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))))))))))))
SMB.spr.est[SMB.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
smb17<-spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==22],apply(livefreq2023[livefreq2023$STRATA_ID==22,24:50],1,sum),
           livefreq2024$TOW_NO[livefreq2024$STRATA_ID==22],apply(livefreq2024[livefreq2024$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb17,summary(smb16, summary(smb15, summary(smb14, summary(smb13, summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1)))))))))))))))))

SMB.spr.est[SMB.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
smb18<-spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==22],apply(livefreq2024[livefreq2024$STRATA_ID==22,24:50],1,sum),
           livefreq2025$TOW_NO[livefreq2025$STRATA_ID==22],apply(livefreq2025[livefreq2025$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb18,summary(smb17,summary(smb16, summary(smb15, summary(smb14, summary(smb13, summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1))))))))))))))))))

SMB.spr.est[SMB.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
smb19<-spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==22],apply(livefreq2025[livefreq2025$STRATA_ID==22,24:50],1,sum),
           livefreq2026$TOW_NO[livefreq2026$STRATA_ID==22],apply(livefreq2026[livefreq2026$STRATA_ID==22,27:50],1,sum),
           crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smb19,summary(smb18,summary(smb17,summary(smb16, summary(smb15, summary(smb14, summary(smb13, summary(smb12, summary (smb11, summary( smb10, summary (smb9, summary (smb8, summary (smb7, summary (smb6,summary (smb5,summary (smb4, summary (smb3, summary (smb2, summary (smb1)))))))))))))))))))

SMB.spr.est[SMB.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

SMB.spr.est

#make dataframe for all of SMB Commercial (must match recruit dataframe)
SMB.spr.est$method <- "spr"
SMB.spr.est$NH <- 117875.8905
SMB.spr.est$Area <- "SMB"
SMB.spr.est$Age <- "Comm"
names(SMB.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

#combine simple and spr estimates
#SPA3.SMB.Comm <- rbind(SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year<2007,], SMB.spr.est[SMB.spr.est$Year>=2007&SMB.spr.est$Year<=2019,],SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year==2020,], SMB.spr.est[SMB.spr.est$Year>=2021,])
SPA3.SMB.Comm <- rbind(SPA3.SMB.Comm.simple[SPA3.SMB.Comm.simple$Year<2007,], SMB.spr.est)

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.SMB.Comm$Year, SPA3.SMB.Comm$Mean.nums, xout=2020) #  53.25464
SPA3.SMB.Comm[SPA3.SMB.Comm$Year==2020,c(2,3)]<-c(53.25464, 299.18281) #assume var from 2019

SPA3.SMB.Comm$cv <- sqrt(SPA3.SMB.Comm$var.y)/SPA3.SMB.Comm$Mean.nums
SPA3.SMB.Comm$Pop <- SPA3.SMB.Comm$Mean.nums*SPA3.SMB.Comm$NH
SPA3.SMB.Comm

#combine Recruit and Commercial dataframes for SMB
SPA3.SMB.Numbers <- rbind(SPA3.SMB.Rec, SPA3.SMB.Comm)
SPA3.SMB.Numbers

####
# ---- Inside VMS Strata ----
####

# Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 1991:surveyyear
X <- length(years)

#simple means
SPA3.Inner.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(197553.4308, X), Area=rep("InVMS", X), Age=rep("Recruit",X))
for(i in 1:length(SPA3.Inner.Rec.simple$Year)){
  temp.data <- BIlivefreq.dat[BIlivefreq.dat$YEAR==1990+i,]
	SPA3.Inner.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
	SPA3.Inner.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA3.Inner.Rec.simple
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Inner.Rec.simple$Year, SPA3.Inner.Rec.simple$Mean.nums, xout=2020) #  4.213636
SPA3.Inner.Rec.simple[SPA3.Inner.Rec.simple$Year==2020,c(2,3)]<-c(4.213636, 4.812061e+01) #assume var from 2019

SPA3.Inner.Rec.simple

# RECRUIT SPR

## dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Inner.rec.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr  (tows in 2007 that were repeats of 2006)     #previous, current
inrec1<-spr(livefreq2006$TOW_NO[livefreq2006$STRATA_ID==99],apply(livefreq2006[livefreq2006$STRATA_ID==99,21:23],1,sum),
livefreq2007$TOW_NO[livefreq2007$STRATA_ID==99],apply(livefreq2007[livefreq2007$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec1)    #
Inner.rec.spr.est[Inner.rec.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008   #if issues, check that tow stata_ids agree
inrec2<-spr(livefreq2007$TOW_NO[livefreq2007$STRATA_ID==99],apply(livefreq2007[livefreq2007$STRATA_ID==99,21:23],1,sum),
livefreq2008$TOW_NO[livefreq2008$STRATA_ID==99],apply(livefreq2008[livefreq2008$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec2)   #
Inner.rec.spr.est[Inner.rec.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
inrec3<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==99],apply(livefreq2008[livefreq2008$STRATA_ID==99,21:23],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID==99],apply(livefreq2009[livefreq2009$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec3)   #
Inner.rec.spr.est[Inner.rec.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
inrec4<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==99],apply(livefreq2009[livefreq2009$STRATA_ID==99,21:23],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID==99],apply(livefreq2010[livefreq2010$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec4)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
inrec5<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==99],apply(livefreq2010[livefreq2010$STRATA_ID==99,21:23],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==99],apply(livefreq2011[livefreq2011$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec5)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
inrec6<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==99],apply(livefreq2011[livefreq2011$STRATA_ID==99,21:23],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==99],apply(livefreq2012[livefreq2012$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec6)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
inrec7<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==99],apply(livefreq2012[livefreq2012$STRATA_ID==99,21:23],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==99],apply(livefreq2013[livefreq2013$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec7)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
inrec8<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==99],apply(livefreq2013[livefreq2013$STRATA_ID==99,21:23],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID==99],apply(livefreq2014[livefreq2014$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec8)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
inrec9<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==99],apply(livefreq2014[livefreq2014$STRATA_ID==99,21:23],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID==99],apply(livefreq2015[livefreq2015$STRATA_ID==99,24:26],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec9)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
inrec10<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==99],apply(livefreq2015[livefreq2015$STRATA_ID==99,21:23],1,sum),
            livefreq2016$TOW_NO[livefreq2016$STRATA_ID==99],apply(livefreq2016[livefreq2016$STRATA_ID==99,24:26],1,sum),
            crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec10)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
inrec11<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==99],apply(livefreq2016[livefreq2016$STRATA_ID==99,21:23],1,sum),
            livefreq2017$TOW_NO[livefreq2017$STRATA_ID==99],apply(livefreq2017[livefreq2017$STRATA_ID==99,24:26],1,sum),
            crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec11)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
inrec12<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==99],apply(livefreq2017[livefreq2017$STRATA_ID==99,21:23],1,sum),
             livefreq2018$TOW_NO[livefreq2018$STRATA_ID==99],apply(livefreq2018[livefreq2018$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec12)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2018/2019
inrec13<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==99],apply(livefreq2018[livefreq2018$STRATA_ID==99,21:23],1,sum),
             livefreq2019$TOW_NO[livefreq2019$STRATA_ID==99],apply(livefreq2019[livefreq2019$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec13)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020 - 2021 repeats of 2019 
inrec14<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==99],apply(livefreq2019[livefreq2019$STRATA_ID==99,21:23],1,sum),
             livefreq2021$TOW_NO[livefreq2021$STRATA_ID==99],apply(livefreq2021[livefreq2021$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec14)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 #no survey in 2020 - 2021 repeats of 2019 
inrec15<-spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==99],apply(livefreq2021[livefreq2021$STRATA_ID==99,21:23],1,sum),
             livefreq2022$TOW_NO[livefreq2022$STRATA_ID==99],apply(livefreq2022[livefreq2022$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec15)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 #
inrec16<-spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==99],apply(livefreq2022[livefreq2022$STRATA_ID==99,21:23],1,sum),
             livefreq2023$TOW_NO[livefreq2023$STRATA_ID==99],apply(livefreq2023[livefreq2023$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec16)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 #
inrec17<-spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==99],apply(livefreq2023[livefreq2023$STRATA_ID==99,21:23],1,sum),
             livefreq2024$TOW_NO[livefreq2024$STRATA_ID==99],apply(livefreq2024[livefreq2024$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec17)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 #
inrec18<-spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==99],apply(livefreq2024[livefreq2024$STRATA_ID==99,21:23],1,sum),
             livefreq2025$TOW_NO[livefreq2025$STRATA_ID==99],apply(livefreq2025[livefreq2025$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec18)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 #
inrec19<-spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==99],apply(livefreq2025[livefreq2025$STRATA_ID==99,21:23],1,sum),
             livefreq2026$TOW_NO[livefreq2026$STRATA_ID==99],apply(livefreq2026[livefreq2026$STRATA_ID==99,24:26],1,sum),
             crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (inrec19)
Inner.rec.spr.est[Inner.rec.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Inner.rec.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Inner.rec.spr.est$Year, Inner.rec.spr.est$Mean.nums, xout=2020) #  4.153759
Inner.rec.spr.est[Inner.rec.spr.est$Year==2020,c(2,3)]<-c(4.153759, 0.6763821) #assume var from 2019

Inner.rec.spr.est$method <- "spr"
Inner.rec.spr.est$NH <- 197553.4308
Inner.rec.spr.est$Area <- "InVMS"
Inner.rec.spr.est$Age <-"Recruit"
names(Inner.rec.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA3.Inner.Rec <- rbind(SPA3.Inner.Rec.simple[SPA3.Inner.Rec.simple$Year<2007,], Inner.rec.spr.est)
SPA3.Inner.Rec$cv <- sqrt(SPA3.Inner.Rec$var.y)/SPA3.Inner.Rec$Mean.nums
SPA3.Inner.Rec$Pop <- SPA3.Inner.Rec$Mean.nums*SPA3.Inner.Rec$NH
SPA3.Inner.Rec

############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

# Simple means for Non-spr years
years <- 1991:surveyyear
X <- length(years)

SPA3.Inner.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(197553.4308, X), Area=rep("InVMS", X), Age=rep("Comm", X))
for(i in 1:length(SPA3.Inner.Comm.simple$Year)){
  temp.data <- BIlivefreq.dat[BIlivefreq.dat$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.Inner.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
	SPA3.Inner.Comm.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA3.Inner.Comm.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Inner.Comm.simple$Year, SPA3.Inner.Comm.simple$Mean.nums, xout=2020) #  4.153759
SPA3.Inner.Comm.simple[SPA3.Inner.Comm.simple$Year==2020,c(2,3)]<-c(170.7841, 17998.447) #assume var from 2019


## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Inner.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr  (tows in 2007 that were repeats of 2006)     #previous, current
incom1<-spr(livefreq2006$TOW_NO[livefreq2006$STRATA_ID==99],apply(livefreq2006[livefreq2006$STRATA_ID==99,24:50],1,sum),
livefreq2007$TOW_NO[livefreq2007$STRATA_ID==99],apply(livefreq2007[livefreq2007$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom1)    # 109.71
Inner.spr.est[Inner.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008   #if issues, check that tow stata_ids agree
incom2<-spr(livefreq2007$TOW_NO[livefreq2007$STRATA_ID==99],apply(livefreq2007[livefreq2007$STRATA_ID==99,24:50],1,sum),
livefreq2008$TOW_NO[livefreq2008$STRATA_ID==99],apply(livefreq2008[livefreq2008$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom2, summary (incom1))   # 98.698
Inner.spr.est[Inner.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
incom3<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID==99],apply(livefreq2008[livefreq2008$STRATA_ID==99,24:50],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID==99],apply(livefreq2009[livefreq2009$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom3, summary (incom2, summary (incom1)))   # 63.14
Inner.spr.est[Inner.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
incom4<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID==99],apply(livefreq2009[livefreq2009$STRATA_ID==99,24:50],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID==99],apply(livefreq2010[livefreq2010$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom4, summary (incom3, summary (incom2, summary (incom1)))) #73.69
Inner.spr.est[Inner.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
incom5<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID==99],apply(livefreq2010[livefreq2010$STRATA_ID==99,24:50],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID==99],apply(livefreq2011[livefreq2011$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1)))))  #105.85
Inner.spr.est[Inner.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
incom6<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID==99],apply(livefreq2011[livefreq2011$STRATA_ID==99,24:50],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID==99],apply(livefreq2012[livefreq2012$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))   #102.2
Inner.spr.est[Inner.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
incom7<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID==99],apply(livefreq2012[livefreq2012$STRATA_ID==99,24:50],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID==99],apply(livefreq2013[livefreq2013$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))) # 159.7
Inner.spr.est[Inner.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
incom8<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID==99],apply(livefreq2013[livefreq2013$STRATA_ID==99,24:50],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID==99],apply(livefreq2014[livefreq2014$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1)))))))) #154.7
Inner.spr.est[Inner.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
incom9<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID==99],apply(livefreq2014[livefreq2014$STRATA_ID==99,24:50],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID==99],apply(livefreq2015[livefreq2015$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))))) #146.933
Inner.spr.est[Inner.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
incom10<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID==99],apply(livefreq2015[livefreq2015$STRATA_ID==99,24:50],1,sum),
            livefreq2016$TOW_NO[livefreq2016$STRATA_ID==99],apply(livefreq2016[livefreq2016$STRATA_ID==99,27:50],1,sum),
            crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1)))))))))) 
Inner.spr.est[Inner.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
incom11<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID==99],apply(livefreq2016[livefreq2016$STRATA_ID==99,24:50],1,sum),
            livefreq2017$TOW_NO[livefreq2017$STRATA_ID==99],apply(livefreq2017[livefreq2017$STRATA_ID==99,27:50],1,sum),
            crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))))))) 
Inner.spr.est[Inner.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
incom12<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID==99],apply(livefreq2017[livefreq2017$STRATA_ID==99,24:50],1,sum),
             livefreq2018$TOW_NO[livefreq2018$STRATA_ID==99],apply(livefreq2018[livefreq2018$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))))))))
Inner.spr.est[Inner.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
incom13<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID==99],apply(livefreq2018[livefreq2018$STRATA_ID==99,24:50],1,sum),
             livefreq2019$TOW_NO[livefreq2019$STRATA_ID==99],apply(livefreq2019[livefreq2019$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary(incom13, summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1)))))))))))))
Inner.spr.est[Inner.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020 
incom14<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID==99],apply(livefreq2019[livefreq2019$STRATA_ID==99,24:50],1,sum),
             livefreq2021$TOW_NO[livefreq2021$STRATA_ID==99],apply(livefreq2021[livefreq2021$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary(incom14, summary(incom13, summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))))))))))
Inner.spr.est[Inner.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2021/2022 
incom15<-spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID==99],apply(livefreq2021[livefreq2021$STRATA_ID==99,24:50],1,sum),
             livefreq2022$TOW_NO[livefreq2022$STRATA_ID==99],apply(livefreq2022[livefreq2022$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(incom15,summary(incom14, summary(incom13, summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))))))))))) 
Inner.spr.est[Inner.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
incom16<-spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID==99],apply(livefreq2022[livefreq2022$STRATA_ID==99,24:50],1,sum),
             livefreq2023$TOW_NO[livefreq2023$STRATA_ID==99],apply(livefreq2023[livefreq2023$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(incom16, summary(incom15,summary(incom14, summary(incom13, summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))))))))))))
Inner.spr.est[Inner.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
incom17<-spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID==99],apply(livefreq2023[livefreq2023$STRATA_ID==99,24:50],1,sum),
             livefreq2024$TOW_NO[livefreq2024$STRATA_ID==99],apply(livefreq2024[livefreq2024$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <-  summary(incom17,summary(incom16, summary(incom15,summary(incom14, summary(incom13, summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1)))))))))))))))))

Inner.spr.est[Inner.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2024/2025 
incom18<-spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID==99],apply(livefreq2024[livefreq2024$STRATA_ID==99,24:50],1,sum),
             livefreq2025$TOW_NO[livefreq2025$STRATA_ID==99],apply(livefreq2025[livefreq2025$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <-  summary(incom18,summary(incom17,summary(incom16, summary(incom15,summary(incom14, summary(incom13, summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1))))))))))))))))))

Inner.spr.est[Inner.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
incom19<-spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID==99],apply(livefreq2025[livefreq2025$STRATA_ID==99,24:50],1,sum),
             livefreq2026$TOW_NO[livefreq2026$STRATA_ID==99],apply(livefreq2026[livefreq2026$STRATA_ID==99,27:50],1,sum),
             crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <-  summary(incom19,summary(incom18,summary(incom17,summary(incom16, summary(incom15,summary(incom14, summary(incom13, summary(incom12, summary (incom11, summary(incom10, summary (incom9, summary (incom8,summary (incom7,summary (incom6, summary (incom5, summary (incom4, summary (incom3, summary (incom2, summary (incom1)))))))))))))))))))

Inner.spr.est[Inner.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Inner.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Inner.spr.est$Year, Inner.spr.est$Yspr, xout=2020) #  164.1229
Inner.spr.est[Inner.spr.est$Year==2020,c(2,3)]<-c(164.1229,  207.49474) #assume var from 2019


Inner.spr.est$method <- "spr"
Inner.spr.est$NH <- 197553.4308
Inner.spr.est$Area <- "InVMS"
Inner.spr.est$Age <-"Comm"
names(Inner.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA3.Inner.Comm <- rbind(SPA3.Inner.Comm.simple[SPA3.Inner.Comm.simple$Year<2007,], Inner.spr.est)
SPA3.Inner.Comm$cv <- sqrt(SPA3.Inner.Comm$var.y)/SPA3.Inner.Comm$Mean.nums
SPA3.Inner.Comm$Pop <- SPA3.Inner.Comm$Mean.nums*SPA3.Inner.Comm$NH

#combine Recruit and Commercial dataframes for InVMS
SPA3.InVMS.Numbers <- rbind(SPA3.Inner.Rec, SPA3.Inner.Comm)
SPA3.InVMS.Numbers

###
# ---- Outside VMS Strata ----
###

# Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID==1

years <- 1991:surveyyear
X <- length(years)

SPA3.Outer.Rec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(555399.33, X), Area=rep("OutVMS", X), Age=rep("Recruit", X)
)
for(i in 1:length(SPA3.Outer.Rec.simple$Year)){
  temp.data <- BIlivefreq.dat[BIlivefreq.dat$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.Outer.Rec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
	SPA3.Outer.Rec.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}
SPA3.Outer.Rec.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Outer.Rec.simple$Year, SPA3.Outer.Rec.simple$Mean.nums, xout=2020) #  1.107456
SPA3.Outer.Rec.simple[SPA3.Outer.Rec.simple$Year==2020,c(2,3)]<-c(1.107456,  4.216911e+00) #assume var from 2019

#spr estimates for recruits
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Outer.rec.spr.est  <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr
outrec1<-spr(livefreq2006$TOW_NO[livefreq2006$STRATA_ID%in%23:24],apply(livefreq2006[livefreq2006$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2007$TOW_NO[livefreq2007$STRATA_ID%in%23:24],apply(livefreq2007[livefreq2007$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec1)   #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
outrec2<-spr(livefreq2007$TOW_NO[livefreq2007$STRATA_ID%in%23:24],apply(livefreq2007[livefreq2007$STRATA_ID%in%23:24,21:23],1,sum),
livefreq2008$TOW_NO[livefreq2008$STRATA_ID%in%23:24],apply(livefreq2008[livefreq2008$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec2)  #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
outrec3<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID%in%23:24],apply(livefreq2008[livefreq2008$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID%in%23:24],apply(livefreq2009[livefreq2009$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec3) #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
outrec4<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID%in%23:24],apply(livefreq2009[livefreq2009$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID%in%23:24],apply(livefreq2010[livefreq2010$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec4)#
Outer.rec.spr.est [Outer.rec.spr.est $Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
outrec5<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID%in%23:24],apply(livefreq2010[livefreq2010$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID%in%23:24],apply(livefreq2011[livefreq2011$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec5)  #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011.2012
outrec6<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID%in%23:24],apply(livefreq2011[livefreq2011$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID%in%23:24],apply(livefreq2012[livefreq2012$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec6)  #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
outrec7<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID%in%23:24],apply(livefreq2012[livefreq2012$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID%in%23:24],apply(livefreq2013[livefreq2013$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec7) #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
outrec8<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID%in%23:24],apply(livefreq2013[livefreq2013$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID%in%23:24],apply(livefreq2014[livefreq2014$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec8) #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
outrec9<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID%in%23:24],apply(livefreq2014[livefreq2014$STRATA_ID%in%23:24, 21:23],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID%in%23:24],apply(livefreq2015[livefreq2015$STRATA_ID%in%23:24, 24:26],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec9) #
Outer.rec.spr.est [Outer.rec.spr.est $Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
outrec10<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID%in%23:24],apply(livefreq2015[livefreq2015$STRATA_ID%in%23:24, 21:23],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID%in%23:24],apply(livefreq2016[livefreq2016$STRATA_ID%in%23:24, 24:26],1,sum),
             crossref.spa3.2016[crossref.spa3.2016$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec10) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
outrec11<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID%in%23:24],apply(livefreq2016[livefreq2016$STRATA_ID%in%23:24, 21:23],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID%in%23:24],apply(livefreq2017[livefreq2017$STRATA_ID%in%23:24, 24:26],1,sum),
             crossref.spa3.2017[crossref.spa3.2017$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec11) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
outrec12<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID%in%23:24],apply(livefreq2017[livefreq2017$STRATA_ID%in%23:24, 21:23],1,sum),
       livefreq2018$TOW_NO[livefreq2018$STRATA_ID%in%23:24],apply(livefreq2018[livefreq2018$STRATA_ID%in%23:24, 24:26],1,sum),
              crossref.spa3.2018[crossref.spa3.2018$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outrec12) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
#outrec13<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID%in%23:24],apply(livefreq2018[livefreq2018$STRATA_ID%in%23:24, 21:23],1,sum),
#              livefreq2019$TOW_NO[livefreq2019$STRATA_ID%in%23:24],apply(livefreq2019[livefreq2019$STRATA_ID%in%23:24, 24:26],1,sum),
#              crossref.spa3.2019[crossref.spa3.2019$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
#
#K<-summary (outrec13) #NOTE SELECTION OF REPEATS FROM 2018 had 0 RECRUITS IN 2018 so SPR with 2019 breaks 
#Outer.rec.spr.est [Outer.rec.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

# Check why SPR failed for 2019 estimate: see that in 2018 all repeated had 0 scallop for recruit size - will need to use simple mean estimate for 2019 VMS Outside Recruit size
#outsiderefs <- crossref.spa3.2019[crossref.spa3.2019$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")]
#apply(livefreq2018[livefreq2018$TOW_NO%in%outsiderefs$TOW_NO_REF, 21:23],1,sum)


#2019/2021  #no survey in 2020 
outrec13<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID%in%23:24],apply(livefreq2019[livefreq2019$STRATA_ID%in%23:24, 21:23],1,sum),
              livefreq2021$TOW_NO[livefreq2021$STRATA_ID%in%23:24],apply(livefreq2021[livefreq2021$STRATA_ID%in%23:24, 24:26],1,sum),
              crossref.spa3.2021[crossref.spa3.2021$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outrec13) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022  
outrec14<-spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID%in%23:24],apply(livefreq2021[livefreq2021$STRATA_ID%in%23:24, 21:23],1,sum),
              livefreq2022$TOW_NO[livefreq2022$STRATA_ID%in%23:24],apply(livefreq2022[livefreq2022$STRATA_ID%in%23:24, 24:26],1,sum),
              crossref.spa3.2022[crossref.spa3.2022$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outrec14) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023  
outrec15<-spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID%in%23:24],apply(livefreq2021[livefreq2022$STRATA_ID%in%23:24, 21:23],1,sum),
              livefreq2023$TOW_NO[livefreq2023$STRATA_ID%in%23:24],apply(livefreq2022[livefreq2023$STRATA_ID%in%23:24, 24:26],1,sum),
              crossref.spa3.2023[crossref.spa3.2023$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outrec15) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024  
outrec16<-spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID%in%23:24],apply(livefreq2023[livefreq2023$STRATA_ID%in%23:24, 21:23],1,sum),
              livefreq2024$TOW_NO[livefreq2024$STRATA_ID%in%23:24],apply(livefreq2024[livefreq2024$STRATA_ID%in%23:24, 24:26],1,sum),
              crossref.spa3.2024[crossref.spa3.2024$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outrec16) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025  
outrec17<-spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID%in%23:24],apply(livefreq2024[livefreq2024$STRATA_ID%in%23:24, 21:23],1,sum),
              livefreq2025$TOW_NO[livefreq2025$STRATA_ID%in%23:24],apply(livefreq2025[livefreq2025$STRATA_ID%in%23:24, 24:26],1,sum),
              crossref.spa3.2025[crossref.spa3.2025$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outrec17) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026  
outrec18<-spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID%in%23:24],apply(livefreq2025[livefreq2025$STRATA_ID%in%23:24, 21:23],1,sum),
              livefreq2026$TOW_NO[livefreq2026$STRATA_ID%in%23:24],apply(livefreq2026[livefreq2026$STRATA_ID%in%23:24, 24:26],1,sum),
              crossref.spa3.2026[crossref.spa3.2026$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outrec18) #
Outer.rec.spr.est [Outer.rec.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Outer.rec.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Outer.rec.spr.est$Year, Outer.rec.spr.est$Yspr, xout=2020) #  0.9501718
Outer.rec.spr.est[Outer.rec.spr.est$Year==2020,c(2,3)]<-c(0.9501718,  0.09156759) #assume var from 2019

#make dataframe for all of Outer Commercial
Outer.rec.spr.est $method <- "spr"
Outer.rec.spr.est $NH <- 555399.33
Outer.rec.spr.est $Area <- "OutVMS"
Outer.rec.spr.est $Age <- "Recruit"
names(Outer.rec.spr.est ) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA3.Outer.Rec <- rbind(SPA3.Outer.Rec.simple[SPA3.Outer.Rec.simple$Year<2007,], Outer.rec.spr.est )
#Sub in simple estimates for years where SPR failed:
SPA3.Outer.Rec[SPA3.Outer.Rec$Year==2019,] <- SPA3.Outer.Rec.simple[SPA3.Outer.Rec.simple$Year==2019,] ##Note no recruits found in the 11 repeated tows in 2018 in Outer VMS strata in 2018 so breaks 2019 SPR estimate - need to use SIMPLE estimate
SPA3.Outer.Rec[SPA3.Outer.Rec$Year==2022,] <- SPA3.Outer.Rec.simple[SPA3.Outer.Rec.simple$Year==2022,]
SPA3.Outer.Rec[SPA3.Outer.Rec$Year==2026,] <- SPA3.Outer.Rec.simple[SPA3.Outer.Rec.simple$Year==2026,]


SPA3.Outer.Rec$cv <-sqrt(SPA3.Outer.Rec$var.y)/SPA3.Outer.Rec$Mean.nums
SPA3.Outer.Rec$Pop <-SPA3.Outer.Rec$Mean.nums*SPA3.Outer.Rec$NH

SPA3.Outer.Rec


############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

# Simple means for Non-spr years
years <- 1991:surveyyear
X <- length(years)

SPA3.Outer.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(555399.33, X), Area=rep("OutVMS", X), Age=rep("Comm", X))
for(i in 1:length(SPA3.Outer.Comm.simple$Year)){
  temp.data <- BIlivefreq.dat[BIlivefreq.dat$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.Outer.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
	SPA3.Outer.Comm.simple[i,3] <-  var(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA3.Outer.Comm.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Outer.Comm.simple$Year, SPA3.Outer.Comm.simple$Mean.nums, xout=2020) #  67.31817
SPA3.Outer.Comm.simple[SPA3.Outer.Comm.simple$Year==2020,c(2,3)]<-c(67.31817,  5354.656) #assume var from 2019


## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Outer.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr
outcom1<-spr(livefreq2006$TOW_NO[livefreq2006$STRATA_ID%in%23:24],apply(livefreq2006[livefreq2006$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2007$TOW_NO[livefreq2007$STRATA_ID%in%23:24],apply(livefreq2007[livefreq2007$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom1)   # 63.57
Outer.spr.est[Outer.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
outcom2<-spr(livefreq2007$TOW_NO[livefreq2007$STRATA_ID%in%23:24],apply(livefreq2007[livefreq2007$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2008$TOW_NO[livefreq2008$STRATA_ID%in%23:24],apply(livefreq2008[livefreq2008$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom2, summary (outcom1))  #54.96
Outer.spr.est[Outer.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
outcom3<-spr(livefreq2008$TOW_NO[livefreq2008$STRATA_ID%in%23:24],apply(livefreq2008[livefreq2008$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2009$TOW_NO[livefreq2009$STRATA_ID%in%23:24],apply(livefreq2009[livefreq2009$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom3, summary (outcom2, summary (outcom1))) #41.24
Outer.spr.est[Outer.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
outcom4<-spr(livefreq2009$TOW_NO[livefreq2009$STRATA_ID%in%23:24],apply(livefreq2009[livefreq2009$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2010$TOW_NO[livefreq2010$STRATA_ID%in%23:24],apply(livefreq2010[livefreq2010$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))#45.28
Outer.spr.est[Outer.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
outcom5<-spr(livefreq2010$TOW_NO[livefreq2010$STRATA_ID%in%23:24],apply(livefreq2010[livefreq2010$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2011$TOW_NO[livefreq2011$STRATA_ID%in%23:24],apply(livefreq2011[livefreq2011$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1))))))  #41.75
Outer.spr.est[Outer.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011.2012
outcom6<-spr(livefreq2011$TOW_NO[livefreq2011$STRATA_ID%in%23:24],apply(livefreq2011[livefreq2011$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2012$TOW_NO[livefreq2012$STRATA_ID%in%23:24],apply(livefreq2012[livefreq2012$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))  #65.4
Outer.spr.est[Outer.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
outcom7<-spr(livefreq2012$TOW_NO[livefreq2012$STRATA_ID%in%23:24],apply(livefreq2012[livefreq2012$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2013$TOW_NO[livefreq2013$STRATA_ID%in%23:24],apply(livefreq2013[livefreq2013$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))) #40.02
Outer.spr.est[Outer.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
outcom8<-spr(livefreq2013$TOW_NO[livefreq2013$STRATA_ID%in%23:24],apply(livefreq2013[livefreq2013$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2014$TOW_NO[livefreq2014$STRATA_ID%in%23:24],apply(livefreq2014[livefreq2014$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1))))))))) #96.38
Outer.spr.est[Outer.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
outcom9<-spr(livefreq2014$TOW_NO[livefreq2014$STRATA_ID%in%23:24],apply(livefreq2014[livefreq2014$STRATA_ID%in%23:24, 24:50],1,sum),
livefreq2015$TOW_NO[livefreq2015$STRATA_ID%in%23:24],apply(livefreq2015[livefreq2015$STRATA_ID%in%23:24, 27:50],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))))) #96.38
Outer.spr.est[Outer.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
outcom10<-spr(livefreq2015$TOW_NO[livefreq2015$STRATA_ID%in%23:24],apply(livefreq2015[livefreq2015$STRATA_ID%in%23:24, 24:50],1,sum),
             livefreq2016$TOW_NO[livefreq2016$STRATA_ID%in%23:24],apply(livefreq2016[livefreq2016$STRATA_ID%in%23:24, 27:50],1,sum),
             crossref.spa3.2016[crossref.spa3.2016$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom10, summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1))))))))))) 
Outer.spr.est[Outer.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
outcom11<-spr(livefreq2016$TOW_NO[livefreq2016$STRATA_ID%in%23:24],apply(livefreq2016[livefreq2016$STRATA_ID%in%23:24, 24:50],1,sum),
             livefreq2017$TOW_NO[livefreq2017$STRATA_ID%in%23:24],apply(livefreq2017[livefreq2017$STRATA_ID%in%23:24, 27:50],1,sum),
             crossref.spa3.2017[crossref.spa3.2017$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))))))) 
Outer.spr.est[Outer.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
outcom12<-spr(livefreq2017$TOW_NO[livefreq2017$STRATA_ID%in%23:24],apply(livefreq2017[livefreq2017$STRATA_ID%in%23:24, 24:50],1,sum),
              livefreq2018$TOW_NO[livefreq2018$STRATA_ID%in%23:24],apply(livefreq2018[livefreq2018$STRATA_ID%in%23:24, 27:50],1,sum),
              crossref.spa3.2018[crossref.spa3.2018$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))))))))
Outer.spr.est[Outer.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#12 tow in outside vms strata in 2018

#2018/2019
outcom13<-spr(livefreq2018$TOW_NO[livefreq2018$STRATA_ID%in%23:24],apply(livefreq2018[livefreq2018$STRATA_ID%in%23:24, 24:50],1,sum),
              livefreq2019$TOW_NO[livefreq2019$STRATA_ID%in%23:24],apply(livefreq2019[livefreq2019$STRATA_ID%in%23:24, 27:50],1,sum),
              crossref.spa3.2019[crossref.spa3.2019$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outcom13,summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1))))))))))))))
Outer.spr.est[Outer.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#11 tow in outside vms strata in 2019

#2019/2021 #No survey in 2020 
outcom14<-spr(livefreq2019$TOW_NO[livefreq2019$STRATA_ID%in%23:24],apply(livefreq2019[livefreq2019$STRATA_ID%in%23:24, 24:50],1,sum),
              livefreq2021$TOW_NO[livefreq2021$STRATA_ID%in%23:24],apply(livefreq2021[livefreq2021$STRATA_ID%in%23:24, 27:50],1,sum),
              crossref.spa3.2021[crossref.spa3.2021$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(outcom14,summary(outcom13,summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))))))))))
Outer.spr.est[Outer.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
outcom15 <- spr(livefreq2021$TOW_NO[livefreq2021$STRATA_ID%in%23:24],apply(livefreq2021[livefreq2021$STRATA_ID%in%23:24, 24:50],1,sum),
              livefreq2022$TOW_NO[livefreq2022$STRATA_ID%in%23:24],apply(livefreq2022[livefreq2022$STRATA_ID%in%23:24, 27:50],1,sum),
              crossref.spa3.2022[crossref.spa3.2022$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <- summary(outcom15,summary(outcom14,summary(outcom13,summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))))))))))) 
Outer.spr.est[Outer.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
outcom16 <- spr(livefreq2022$TOW_NO[livefreq2022$STRATA_ID%in%23:24],apply(livefreq2022[livefreq2022$STRATA_ID%in%23:24, 24:50],1,sum),
                livefreq2023$TOW_NO[livefreq2023$STRATA_ID%in%23:24],apply(livefreq2023[livefreq2023$STRATA_ID%in%23:24, 27:50],1,sum),
                crossref.spa3.2023[crossref.spa3.2023$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <- summary(outcom16, summary(outcom15,summary(outcom14,summary(outcom13,summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1))))))))))))))))) 
Outer.spr.est[Outer.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
outcom17 <- spr(livefreq2023$TOW_NO[livefreq2023$STRATA_ID%in%23:24],apply(livefreq2023[livefreq2023$STRATA_ID%in%23:24, 24:50],1,sum),
                livefreq2024$TOW_NO[livefreq2024$STRATA_ID%in%23:24],apply(livefreq2024[livefreq2024$STRATA_ID%in%23:24, 27:50],1,sum),
                crossref.spa3.2024[crossref.spa3.2024$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <- summary(outcom17,summary(outcom16, summary(outcom15,summary(outcom14,summary(outcom13,summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1)))))))))))))))))) 

Outer.spr.est[Outer.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
outcom18 <- spr(livefreq2024$TOW_NO[livefreq2024$STRATA_ID%in%23:24],apply(livefreq2024[livefreq2024$STRATA_ID%in%23:24, 24:50],1,sum),
                livefreq2025$TOW_NO[livefreq2025$STRATA_ID%in%23:24],apply(livefreq2025[livefreq2025$STRATA_ID%in%23:24, 27:50],1,sum),
                crossref.spa3.2025[crossref.spa3.2025$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <-summary(outcom18, summary(outcom17,summary(outcom16, summary(outcom15,summary(outcom14,summary(outcom13,summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1))))))))))))))))))) 

Outer.spr.est[Outer.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
outcom19 <- spr(livefreq2025$TOW_NO[livefreq2025$STRATA_ID%in%23:24],apply(livefreq2025[livefreq2025$STRATA_ID%in%23:24, 24:50],1,sum),
                livefreq2026$TOW_NO[livefreq2026$STRATA_ID%in%23:24],apply(livefreq2026[livefreq2026$STRATA_ID%in%23:24, 27:50],1,sum),
                crossref.spa3.2026[crossref.spa3.2026$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <-summary(outcom19,summary(outcom18, summary(outcom17,summary(outcom16, summary(outcom15,summary(outcom14,summary(outcom13,summary(outcom12,summary (outcom11, summary(outcom10,summary (outcom9, summary (outcom8, summary (outcom7,summary (outcom6,summary (outcom5, summary (outcom4, (summary (outcom3, summary (outcom2, summary (outcom1))))))))))))))))))))

Outer.spr.est[Outer.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


Outer.spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Outer.spr.est$Year, Outer.spr.est$Yspr, xout=2020) #  52.04166
Outer.spr.est[Outer.spr.est$Year==2020,c(2,3)]<-c(52.04166,    44.44828) #assume var from 2019


#make dataframe for all of Outer Commercial
Outer.spr.est$method <- "spr"
Outer.spr.est$NH <- 555399.33
Outer.spr.est$Area <- "OutVMS"
Outer.spr.est$Age<- "Comm"
names(Outer.spr.est) <- c("Year", "Mean.nums", "var.y", "method", "NH", "Area", "Age")

SPA3.Outer.Comm <- rbind(SPA3.Outer.Comm.simple[SPA3.Outer.Comm.simple$Year<2007,], Outer.spr.est)
SPA3.Outer.Comm$cv <- sqrt(SPA3.Outer.Comm$var.y)/SPA3.Outer.Comm$Mean.nums
SPA3.Outer.Comm$Pop <- SPA3.Outer.Comm$Mean.nums*SPA3.Outer.Comm$NH

#combine Recruit and Commercial dataframes for OutVMS

SPA3.OutVMS.Numbers <- rbind(SPA3.Outer.Rec, SPA3.Outer.Comm)
SPA3.OutVMS.Numbers


###
#  Make Numbers dataframe for SPA3
####
SPA3.Numbers <- rbind(SPA3.SMB.Numbers, SPA3.InVMS.Numbers, SPA3.OutVMS.Numbers)

write.csv(SPA3.Numbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3.Index.Numbers.",surveyyear,".csv"))



###
###  ---- RECRUIT AND COMMERCIAL WEIGHT PER TOW AND POPULATION BIOMASS ----
###

####
# ---- St. Mary's Bay Weights ----
####

# Recruit (65-79 mm)

# use simple mean for recruit, only TOW_TYPE_ID==1
years <- 1991:surveyyear
X <- length(years)

SPA3.SMB.RecWt.simple <- data.frame(Year=years, Mean.weight=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(117875.8905, X),Area=rep("SMB",X), Age=rep("Recruit", X))
for(i in 1:length(SPA3.SMB.RecWt.simple$Year)){
  temp.data <- BIliveweight[BIliveweight$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.SMB.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
	SPA3.SMB.RecWt.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
}
SPA3.SMB.RecWt.simple

#interpolate missing values
approx(SPA3.SMB.RecWt.simple$Year, SPA3.SMB.RecWt.simple$Mean.weight, xout=1997) #  24.65999
approx(SPA3.SMB.RecWt.simple$Year, SPA3.SMB.RecWt.simple$Mean.weight, xout=1998) #  40.27443
approx(SPA3.SMB.RecWt.simple$Year, SPA3.SMB.RecWt.simple$Mean.weight, xout=2002) #9.684233
approx(SPA3.SMB.RecWt.simple$Year, SPA3.SMB.RecWt.simple$Mean.weight, xout=2003) # 10.95828
approx(SPA3.SMB.RecWt.simple$Year, SPA3.SMB.RecWt.simple$Mean.weight, xout=2020) # 35.35036

SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==1997, c(2,3)]<- c(24.65999,163.64) #var from 1996
SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==1998, c(2,3)]<- c(40.27443,163.64) #var from 1996
SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2002, c(2,3)]<-c(9.684233,658.13) #var from 2001
SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2003, c(2,3)]<-c(10.95828, 658.13)#var from 2001
SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2020, c(2,3)]<-c(35.35036, 7764.2531)#var from 2001


#SPR for recruit
## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
SMB.rec.spr.estWt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr  (tows in 2007 that were repeats of 2006)
SMBr1<-spr(liveweight2006$TOW_NO[liveweight2006$STRATA_ID==22],apply(liveweight2006[liveweight2006$STRATA_ID==22,23:25],1,sum),
liveweight2007$TOW_NO[liveweight2007$STRATA_ID==22],apply(liveweight2007[liveweight2007$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr1)   #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
SMBr2<-spr(liveweight2007$TOW_NO[liveweight2007$STRATA_ID==22],apply(liveweight2007[liveweight2007$STRATA_ID==22,23:25],1,sum),
liveweight2008$TOW_NO[liveweight2008$STRATA_ID==22],apply(liveweight2008[liveweight2008$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr2)    #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
SMBr3<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==22],apply(liveweight2008[liveweight2008$STRATA_ID==22,23:25],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID==22],apply(liveweight2009[liveweight2009$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr3) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
SMBr4<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==22],apply(liveweight2009[liveweight2009$STRATA_ID==22,23:25],1,sum),
liveweight2010$TOW_NO[liveweight2010$STRATA_ID==22],apply(liveweight2010[liveweight2010$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr4)   #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
SMBr5<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==22],apply(liveweight2010[liveweight2010$STRATA_ID==22,23:25],1,sum),
liveweight2011$TOW_NO[liveweight2011$STRATA_ID==22],apply(liveweight2011[liveweight2011$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr5)  #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
SMBr6<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==22],apply(liveweight2011[liveweight2011$STRATA_ID==22,23:25],1,sum),
liveweight2012$TOW_NO[liveweight2012$STRATA_ID==22],apply(liveweight2012[liveweight2012$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr6)   #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
SMBr7<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==22],apply(liveweight2012[liveweight2012$STRATA_ID==22,23:25],1,sum),
liveweight2013$TOW_NO[liveweight2013$STRATA_ID==22],apply(liveweight2013[liveweight2013$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr7)  #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
SMBr8<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==22],apply(liveweight2013[liveweight2013$STRATA_ID==22,23:25],1,sum),
liveweight2014$TOW_NO[liveweight2014$STRATA_ID==22],apply(liveweight2014[liveweight2014$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr8) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
SMBr9<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==22],apply(liveweight2014[liveweight2014$STRATA_ID==22,23:25],1,sum),
liveweight2015$TOW_NO[liveweight2015$STRATA_ID==22],apply(liveweight2015[liveweight2015$STRATA_ID==22,26:28],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr9) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
SMBr10<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==22],apply(liveweight2015[liveweight2015$STRATA_ID==22,23:25],1,sum),
           liveweight2016$TOW_NO[liveweight2016$STRATA_ID==22],apply(liveweight2016[liveweight2016$STRATA_ID==22,26:28],1,sum),
           crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBr10) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
#SMBr11<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==22],apply(liveweight2016[liveweight2016$STRATA_ID==22,23:25],1,sum),
#           liveweight2017$TOW_NO[liveweight2017$STRATA_ID==22],apply(liveweight2017[liveweight2017$STRATA_ID==22,26:28],1,sum),
#           crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
#K<-summary (SMBr11) #
#SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2017,c(2:3)] <- c(NA, NA) #NOTE: no recruits found in the 5 repeated tows in 2017 in SMB - need to use SIMPLE estimate

#2017/2018
#SMBr12<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==22],apply(liveweight2017[liveweight2017$STRATA_ID==22,23:25],1,sum),
#            liveweight2018$TOW_NO[liveweight2018$STRATA_ID==22],apply(liveweight2018[liveweight2018$STRATA_ID==22,26:28],1,sum),
#            crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
#K<-summary (SMBr12) #
#SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2018,c(2:3)] <- c(NA, NA) #NOTE: no recruits found in the 3 repeated tows in 2018 in SMB - need to use SIMPLE estimate

#2018/2019
#SMBr13<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID==22],apply(liveweight2018[liveweight2018$STRATA_ID==22,23:25],1,sum),
#            liveweight2019$TOW_NO[liveweight2019$STRATA_ID==22],apply(liveweight2019[liveweight2019$STRATA_ID==22,26:28],1,sum),
#            crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
#K<-summary (SMBr13) #
#SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2019,c(2:3)] <- c(NA, NA) #NOTE: no recruits found in the 3 repeated tows in 2018 in SMB - need to use SIMPLE estimate

#2019/2021
#SMBr14<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID==22],apply(liveweight2019[liveweight2019$STRATA_ID==22,23:25],1,sum),
#            liveweight2021$TOW_NO[liveweight2021$STRATA_ID==22],apply(liveweight2021[liveweight2021$STRATA_ID==22,26:28],1,sum),
#            crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
#K<-summary (SMBr14) #
#SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2019,c(2:3)] <- c(NA, NA) #NOTE: no recruits found in the 3 repeated tows in 2018 in SMB - need to use SIMPLE estimate

#2021/2022
SMBr15<-spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID==22],apply(liveweight2021[liveweight2021$STRATA_ID==22,23:25],1,sum),
            liveweight2022$TOW_NO[liveweight2022$STRATA_ID==22],apply(liveweight2022[liveweight2022$STRATA_ID==22,26:28],1,sum),
            crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <- summary(SMBr15) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) 

#2022/2023
SMBr16<-spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID==22],apply(liveweight2022[liveweight2022$STRATA_ID==22,23:25],1,sum),
            liveweight2023$TOW_NO[liveweight2023$STRATA_ID==22],apply(liveweight2023[liveweight2023$STRATA_ID==22,26:28],1,sum),
            crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <- summary(SMBr16) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) 

#2023/2024
#SMBr17<-spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID==22],apply(liveweight2023[liveweight2023$STRATA_ID==22,23:25],1,sum),
#            liveweight2024$TOW_NO[liveweight2024$STRATA_ID==22],apply(liveweight2024[liveweight2024$STRATA_ID==22,26:28],1,sum),
#            crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
#K <- summary(SMBr17) #
#SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) 
#all cross ref tows from 2023 were 0, so can't use SPR for 2024 

#2024/2025
SMBr18<-spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID==22],apply(liveweight2024[liveweight2024$STRATA_ID==22,23:25],1,sum),
            liveweight2025$TOW_NO[liveweight2025$STRATA_ID==22],apply(liveweight2025[liveweight2025$STRATA_ID==22,26:28],1,sum),
            crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <- summary(SMBr18) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) 

#2025/2026
SMBr19<-spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID==22],apply(liveweight2025[liveweight2025$STRATA_ID==22,23:25],1,sum),
            liveweight2026$TOW_NO[liveweight2026$STRATA_ID==22],apply(liveweight2026[liveweight2026$STRATA_ID==22,26:28],1,sum),
            crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <- summary(SMBr19) #
SMB.rec.spr.estWt[SMB.rec.spr.estWt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected) 

SMB.rec.spr.estWt

#make dataframe for all of SMB Recruit
SMB.rec.spr.estWt$method <- "spr"
SMB.rec.spr.estWt$NH <- 117875.8905
SMB.rec.spr.estWt$Area <- "SMB"
SMB.rec.spr.estWt$Age <- "Recruit"
names(SMB.rec.spr.estWt) <- c("Year", "Mean.weight", "var.y", "method", "NH", "Area", "Age")

SPA3.SMB.RecWt<- rbind(SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year<2007,], SMB.rec.spr.estWt)
SPA3.SMB.RecWt[SPA3.SMB.RecWt$Year==2017,] <- SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2017,] ##Note no recruits found in the 5 repeated tows in 2017 in SMB - need to use SIMPLE estimate
SPA3.SMB.RecWt[SPA3.SMB.RecWt$Year==2018,] <- SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2018,] ##Note no recruits found in the 3 repeated tows in 2018 in SMB - need to use SIMPLE estimate
SPA3.SMB.RecWt[SPA3.SMB.RecWt$Year==2019,] <- SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2019,] ##Note no recruits found in the unmatched tows in 2018 in SMB - need to use SIMPLE estimate
SPA3.SMB.RecWt[SPA3.SMB.RecWt$Year==2020,] <- SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2020,] ##Note no recruits found in the unmatched tows in 2018 in SMB - need to use SIMPLE estimate
SPA3.SMB.RecWt[SPA3.SMB.RecWt$Year==2021,] <- SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2021,] ##Note no recruits found in the unmatched tows in 2018 in SMB - need to use SIMPLE estimate
SPA3.SMB.RecWt[SPA3.SMB.RecWt$Year==2024,] <- SPA3.SMB.RecWt.simple[SPA3.SMB.RecWt.simple$Year==2024,] ##Note no recruits found in the unmatched tows in 2018 in SMB - need to use SIMPLE estimate

SPA3.SMB.RecWt$cv <- sqrt(SPA3.SMB.RecWt$var.y)/SPA3.SMB.RecWt$Mean.weight
SPA3.SMB.RecWt$kg<-SPA3.SMB.RecWt$Mean.weight/1000
SPA3.SMB.RecWt$Bmass<-SPA3.SMB.RecWt$kg*SPA3.SMB.RecWt$NH

SPA3.SMB.RecWt

############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

years <- 1991:surveyyear
X <- length(years)

SPA3.SMB.CommWt.simple <- data.frame(Year=years, Mean.weight=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(117875.8905, X), Area=rep("SMB",X), Age=rep("Comm", X))
for(i in 1:length(SPA3.SMB.CommWt.simple$Year)){
  temp.data <- BIliveweight[BIliveweight$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.SMB.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
	SPA3.SMB.CommWt.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
}
SPA3.SMB.CommWt.simple

approx(SPA3.SMB.CommWt.simple$Year, SPA3.SMB.CommWt.simple$Mean.weight, xout=1997) #  464.574
approx(SPA3.SMB.CommWt.simple$Year, SPA3.SMB.CommWt.simple$Mean.weight, xout=1998) #642.0408
approx(SPA3.SMB.CommWt.simple$Year, SPA3.SMB.CommWt.simple$Mean.weight, xout=2002) #  650.3116
approx(SPA3.SMB.CommWt.simple$Year, SPA3.SMB.CommWt.simple$Mean.weight, xout=2003) #  495.592
approx(SPA3.SMB.CommWt.simple$Year, SPA3.SMB.CommWt.simple$Mean.weight, xout=2020) #   1779.52

SPA3.SMB.CommWt.simple[SPA3.SMB.CommWt.simple$Year==1997,c(2,3)]<-c(464.574,164861) #var from 1996
SPA3.SMB.CommWt.simple[SPA3.SMB.CommWt.simple$Year==1998,c(2,3)]<-c(642.0408,164861) #var from 1996
SPA3.SMB.CommWt.simple[SPA3.SMB.CommWt.simple$Year==2002,c(2,3)]<-c(650.3116,775312)#var from 2001
SPA3.SMB.CommWt.simple[SPA3.SMB.CommWt.simple$Year==2003,c(2,3)]<-c(495.592,775312)#var from 2001
SPA3.SMB.CommWt.simple[SPA3.SMB.CommWt.simple$Year==2020,c(2,3)]<-c(1779.52,11300180.2)#var from 2001

## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
SMB.spr.estWt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr  (tows in 2007 that were repeats of 2006)
SMBc1<-spr(liveweight2006$TOW_NO[liveweight2006$STRATA_ID==22],apply(liveweight2006[liveweight2006$STRATA_ID==22,26:52],1,sum),
liveweight2007$TOW_NO[liveweight2007$STRATA_ID==22],apply(liveweight2007[liveweight2007$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc1)   # 766
SMB.spr.estWt[SMB.spr.estWt$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
SMBc2<-spr(liveweight2007$TOW_NO[liveweight2007$STRATA_ID==22],apply(liveweight2007[liveweight2007$STRATA_ID==22,26:52],1,sum),
liveweight2008$TOW_NO[liveweight2008$STRATA_ID==22],apply(liveweight2008[liveweight2008$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc2,summary (SMBc1))    #379.4
SMB.spr.estWt[SMB.spr.estWt$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
SMBc3<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==22],apply(liveweight2008[liveweight2008$STRATA_ID==22,26:52],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID==22],apply(liveweight2009[liveweight2009$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc3,summary (SMBc2,summary (SMBc1))) #644.4
SMB.spr.estWt[SMB.spr.estWt$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
SMBc4<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==22],apply(liveweight2009[liveweight2009$STRATA_ID==22,26:52],1,sum),
liveweight2010$TOW_NO[liveweight2010$STRATA_ID==22],apply(liveweight2010[liveweight2010$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))   # 404.8564
SMB.spr.estWt[SMB.spr.estWt$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
SMBc5<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==22],apply(liveweight2010[liveweight2010$STRATA_ID==22,26:52],1,sum),
liveweight2011$TOW_NO[liveweight2011$STRATA_ID==22],apply(liveweight2011[liveweight2011$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))  #760.37
SMB.spr.estWt[SMB.spr.estWt$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
SMBc6<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==22],apply(liveweight2011[liveweight2011$STRATA_ID==22,26:52],1,sum),
liveweight2012$TOW_NO[liveweight2012$STRATA_ID==22],apply(liveweight2012[liveweight2012$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))))   #1406
SMB.spr.estWt[SMB.spr.estWt$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
SMBc7<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==22],apply(liveweight2012[liveweight2012$STRATA_ID==22,26:52],1,sum),
liveweight2013$TOW_NO[liveweight2013$STRATA_ID==22],apply(liveweight2013[liveweight2013$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))))  #1428
SMB.spr.estWt[SMB.spr.estWt$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
SMBc8<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==22],apply(liveweight2013[liveweight2013$STRATA_ID==22,26:52],1,sum),
liveweight2014$TOW_NO[liveweight2014$STRATA_ID==22],apply(liveweight2014[liveweight2014$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))))) #3024
SMB.spr.estWt[SMB.spr.estWt$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
SMBc9<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==22],apply(liveweight2014[liveweight2014$STRATA_ID==22,26:52],1,sum),
liveweight2015$TOW_NO[liveweight2015$STRATA_ID==22],apply(liveweight2015[liveweight2015$STRATA_ID==22,29:52],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))))))) #
SMB.spr.estWt[SMB.spr.estWt$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
SMBc10<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==22],apply(liveweight2015[liveweight2015$STRATA_ID==22,26:52],1,sum),
           liveweight2016$TOW_NO[liveweight2016$STRATA_ID==22],apply(liveweight2016[liveweight2016$STRATA_ID==22,29:52],1,sum),
           crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))))))) #
SMB.spr.estWt[SMB.spr.estWt$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
SMBc11<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==22],apply(liveweight2016[liveweight2016$STRATA_ID==22,26:52],1,sum),
           liveweight2017$TOW_NO[liveweight2017$STRATA_ID==22],apply(liveweight2017[liveweight2017$STRATA_ID==22,29:52],1,sum),
           crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))))))))) #
SMB.spr.estWt[SMB.spr.estWt$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
SMBc12<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==22],apply(liveweight2017[liveweight2017$STRATA_ID==22,26:52],1,sum),
            liveweight2018$TOW_NO[liveweight2018$STRATA_ID==22],apply(liveweight2018[liveweight2018$STRATA_ID==22,29:52],1,sum),
            crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))))))))) #
SMB.spr.estWt[SMB.spr.estWt$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
SMBc13<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID==22],apply(liveweight2018[liveweight2018$STRATA_ID==22,26:52],1,sum),
            liveweight2019$TOW_NO[liveweight2019$STRATA_ID==22],apply(liveweight2019[liveweight2019$STRATA_ID==22,29:52],1,sum),
            crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary(SMBc13,summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))))))))))) #
SMB.spr.estWt[SMB.spr.estWt$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020
SMBc14<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID==22],apply(liveweight2019[liveweight2019$STRATA_ID==22,26:52],1,sum),
            liveweight2021$TOW_NO[liveweight2021$STRATA_ID==22],apply(liveweight2021[liveweight2021$STRATA_ID==22,29:52],1,sum),
            crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary(SMBc14, summary(SMBc13,summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))))))))))))
SMB.spr.estWt[SMB.spr.estWt$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
SMBc15 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID==22],apply(liveweight2021[liveweight2021$STRATA_ID==22,26:52],1,sum),
            liveweight2022$TOW_NO[liveweight2022$STRATA_ID==22],apply(liveweight2022[liveweight2022$STRATA_ID==22,29:52],1,sum),
            crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <-  summary(SMBc15, summary(SMBc14, summary(SMBc13,summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))))))))))))
SMB.spr.estWt[SMB.spr.estWt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
SMBc16 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID==22],apply(liveweight2022[liveweight2022$STRATA_ID==22,26:52],1,sum),
              liveweight2023$TOW_NO[liveweight2023$STRATA_ID==22],apply(liveweight2023[liveweight2023$STRATA_ID==22,29:52],1,sum),
              crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <-  summary(SMBc16, summary(SMBc15, summary(SMBc14, summary(SMBc13,summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))))))))))))))
SMB.spr.estWt[SMB.spr.estWt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2023/2024 
SMBc17 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID==22],apply(liveweight2023[liveweight2023$STRATA_ID==22,26:52],1,sum),
              liveweight2024$TOW_NO[liveweight2024$STRATA_ID==22],apply(liveweight2024[liveweight2024$STRATA_ID==22,29:52],1,sum),
              crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <-  summary(SMBc17,summary(SMBc16, summary(SMBc15, summary(SMBc14, summary(SMBc13,summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))))))))))))))
SMB.spr.estWt[SMB.spr.estWt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2024/2025 
SMBc18 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID==22],apply(liveweight2024[liveweight2024$STRATA_ID==22,26:52],1,sum),
              liveweight2025$TOW_NO[liveweight2025$STRATA_ID==22],apply(liveweight2025[liveweight2025$STRATA_ID==22,29:52],1,sum),
              crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <-  summary(SMBc18,summary(SMBc17,summary(SMBc16, summary(SMBc15, summary(SMBc14, summary(SMBc13,summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1))))))))))))))))))
SMB.spr.estWt[SMB.spr.estWt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
SMBc19 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID==22],apply(liveweight2025[liveweight2025$STRATA_ID==22,26:52],1,sum),
              liveweight2026$TOW_NO[liveweight2026$STRATA_ID==22],apply(liveweight2026[liveweight2026$STRATA_ID==22,29:52],1,sum),
              crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <-  summary(SMBc19,summary(SMBc18,summary(SMBc17,summary(SMBc16, summary(SMBc15, summary(SMBc14, summary(SMBc13,summary(SMBc12, summary (SMBc11,summary(SMBc10, summary (SMBc9, summary (SMBc8, summary (SMBc7,summary (SMBc6,summary (SMBc5, summary (SMBc4,summary (SMBc3,summary (SMBc2,summary (SMBc1)))))))))))))))))))
SMB.spr.estWt[SMB.spr.estWt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

SMB.spr.estWt

#since no survey in 2020 interpolate 
approx(SMB.spr.estWt$Year, SMB.spr.estWt$Mean.weight, xout=2020) #   1184.632
SMB.spr.estWt[SMB.spr.estWt$Year==2020,c(2,3)]<-c(1184.632, 133855.77 ) #var from 1996

#make dataframe for all of SMB Commercial
SMB.spr.estWt$method <- "spr"
SMB.spr.estWt$NH <- 117875.8905
SMB.spr.estWt$Area <- "SMB"
SMB.spr.estWt$Age <- "Comm"
names(SMB.spr.estWt) <- c("Year", "Mean.weight", "var.y", "method", "NH", "Area", "Age")

SPA3.SMB.CommWt <- rbind(SPA3.SMB.CommWt.simple[SPA3.SMB.CommWt.simple$Year<2007,], SMB.spr.estWt)

SPA3.SMB.CommWt$cv <- sqrt(SPA3.SMB.CommWt$var.y)/SPA3.SMB.CommWt$Mean.weight
SPA3.SMB.CommWt$kg<-SPA3.SMB.CommWt$Mean.weight/1000
SPA3.SMB.CommWt$Bmass<-SPA3.SMB.CommWt$kg*SPA3.SMB.CommWt$NH

#combine Recruit and Commercial dataframes for SMB
SPA3.SMB.Weight<-rbind(SPA3.SMB.RecWt, SPA3.SMB.CommWt)
SPA3.SMB.Weight

####
# --- Inside VMS Strata Weights ---- 
###

############ Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 1991:surveyyear
X <- length(years)

SPA3.Inner.RecWt.simple <- data.frame(Year=years, Mean.weight=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(197553.4308, X), Area=rep("InVMS", X), Age=rep("Recruit",X))
for(i in 1:length(SPA3.Inner.RecWt.simple$Year)){
  temp.data <- BIliveweight[BIliveweight$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.Inner.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
	SPA3.Inner.RecWt.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
}
SPA3.Inner.RecWt.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Inner.RecWt.simple$Year, SPA3.Inner.RecWt.simple$Mean.weight, xout=2020) #  21.6116
SPA3.Inner.RecWt.simple[SPA3.Inner.RecWt.simple$Year==2020,c(2,3)]<-c(21.6116,1011.2255) #assume var from 2019


#spr for Recruit

## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Inner.rec.spr.estWt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr  (tows in 2007 that were repeats of 2006)
INr1<-spr(liveweight2006$TOW_NO[liveweight2006$STRATA_ID==99],apply(liveweight2006[liveweight2006$STRATA_ID==99,23:25],1,sum),
liveweight2007$TOW_NO[liveweight2007$STRATA_ID==99],apply(liveweight2007[liveweight2007$STRATA_ID==99,26:28],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INr1)            #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
INr2<-spr(liveweight2007$TOW_NO[liveweight2007$STRATA_ID==99],apply(liveweight2007[liveweight2007$STRATA_ID==99,23:25],1,sum),
liveweight2008$TOW_NO[liveweight2008$STRATA_ID==99],apply(liveweight2008[liveweight2008$STRATA_ID==99,26:28],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr2)
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
INr3<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==99],apply(liveweight2008[liveweight2008$STRATA_ID==99,23:25],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID==99],apply(liveweight2009[liveweight2009$STRATA_ID==99,26:28],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr3)    #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
INr4<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==99],apply(liveweight2009[liveweight2009$STRATA_ID==99,23:25],1,sum),
liveweight2010$TOW_NO[liveweight2010$STRATA_ID==99],apply(liveweight2010[liveweight2010$STRATA_ID==99,26:28],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr4)
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
INr5<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==99],apply(liveweight2010[liveweight2010$STRATA_ID==99,23:25],1,sum),
liveweight2011$TOW_NO[liveweight2011$STRATA_ID==99],apply(liveweight2011[liveweight2011$STRATA_ID==99,26:28],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr5) #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
INr6<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==99],apply(liveweight2011[liveweight2011$STRATA_ID==99,23:25],1,sum),
liveweight2012$TOW_NO[liveweight2012$STRATA_ID==99],apply(liveweight2012[liveweight2012$STRATA_ID==99,26:28],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr6)    #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
INr7<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==99],apply(liveweight2012[liveweight2012$STRATA_ID==99,23:25],1,sum),
liveweight2013$TOW_NO[liveweight2013$STRATA_ID==99],apply(liveweight2013[liveweight2013$STRATA_ID==99,26:28],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr7)
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
INr8<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==99],apply(liveweight2013[liveweight2013$STRATA_ID==99,23:25],1,sum),
          liveweight2014$TOW_NO[liveweight2014$STRATA_ID==99],apply(liveweight2014[liveweight2014$STRATA_ID==99,26:28],1,sum),
          crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr8)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
INr9<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==99],apply(liveweight2014[liveweight2014$STRATA_ID==99,23:25],1,sum),
          liveweight2015$TOW_NO[liveweight2015$STRATA_ID==99],apply(liveweight2015[liveweight2015$STRATA_ID==99,26:28],1,sum),
          crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr9)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
INr10<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==99],apply(liveweight2015[liveweight2015$STRATA_ID==99,23:25],1,sum),
          liveweight2016$TOW_NO[liveweight2016$STRATA_ID==99],apply(liveweight2016[liveweight2016$STRATA_ID==99,26:28],1,sum),
          crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr10)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
INr11<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==99],apply(liveweight2016[liveweight2016$STRATA_ID==99,23:25],1,sum),
          liveweight2017$TOW_NO[liveweight2017$STRATA_ID==99],apply(liveweight2017[liveweight2017$STRATA_ID==99,26:28],1,sum),
          crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr11)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
INr12<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==99],apply(liveweight2017[liveweight2017$STRATA_ID==99,23:25],1,sum),
           liveweight2018$TOW_NO[liveweight2018$STRATA_ID==99],apply(liveweight2018[liveweight2018$STRATA_ID==99,26:28],1,sum),
           crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr12)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
INr13<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID==99],apply(liveweight2018[liveweight2018$STRATA_ID==99,23:25],1,sum),
           liveweight2019$TOW_NO[liveweight2019$STRATA_ID==99],apply(liveweight2019[liveweight2019$STRATA_ID==99,26:28],1,sum),
           crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr13)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020 
INr14<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID==99],apply(liveweight2019[liveweight2019$STRATA_ID==99,23:25],1,sum),
           liveweight2021$TOW_NO[liveweight2021$STRATA_ID==99],apply(liveweight2021[liveweight2021$STRATA_ID==99,26:28],1,sum),
           crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INr14)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt $Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/20222 
INr15 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID==99],apply(liveweight2021[liveweight2021$STRATA_ID==99,23:25],1,sum),
           liveweight2022$TOW_NO[liveweight2022$STRATA_ID==99],apply(liveweight2022[liveweight2022$STRATA_ID==99,26:28],1,sum),
           crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INr15)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
INr16 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID==99],apply(liveweight2022[liveweight2022$STRATA_ID==99,23:25],1,sum),
             liveweight2023$TOW_NO[liveweight2023$STRATA_ID==99],apply(liveweight2023[liveweight2023$STRATA_ID==99,26:28],1,sum),
             crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INr16)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
INr17 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID==99],apply(liveweight2023[liveweight2023$STRATA_ID==99,23:25],1,sum),
             liveweight2024$TOW_NO[liveweight2024$STRATA_ID==99],apply(liveweight2024[liveweight2024$STRATA_ID==99,26:28],1,sum),
             crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INr17)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
INr18 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID==99],apply(liveweight2024[liveweight2024$STRATA_ID==99,23:25],1,sum),
             liveweight2025$TOW_NO[liveweight2025$STRATA_ID==99],apply(liveweight2025[liveweight2025$STRATA_ID==99,26:28],1,sum),
             crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INr18)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
INr19 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID==99],apply(liveweight2025[liveweight2025$STRATA_ID==99,23:25],1,sum),
             liveweight2026$TOW_NO[liveweight2026$STRATA_ID==99],apply(liveweight2026[liveweight2026$STRATA_ID==99,26:28],1,sum),
             crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INr19)  #
Inner.rec.spr.estWt [Inner.rec.spr.estWt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Inner.rec.spr.estWt

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Inner.rec.spr.estWt$Year, Inner.rec.spr.estWt$Yspr, xout=2020) #  20.46698
Inner.rec.spr.estWt[Inner.rec.spr.estWt$Year==2020,c(2,3)]<-c(20.46698,12.47321) #assume var from 2019


#make dataframe for all of Inner Commercial
Inner.rec.spr.estWt$method <- "spr"
Inner.rec.spr.estWt$NH <- 197553.4308
Inner.rec.spr.estWt$Area <- "InVMS"
Inner.rec.spr.estWt$Age <- "Recruit"
names(Inner.rec.spr.estWt ) <- c("Year", "Mean.weight", "var.y", "method", "NH", "Area", "Age")

SPA3.Inner.RecWt <- rbind(SPA3.Inner.RecWt.simple[SPA3.Inner.RecWt.simple$Year<2007,], Inner.rec.spr.estWt )

SPA3.Inner.RecWt$cv <- sqrt(SPA3.Inner.RecWt$var.y)/SPA3.Inner.RecWt$Mean.weight
SPA3.Inner.RecWt$kg<-SPA3.Inner.RecWt$Mean.weight/1000
SPA3.Inner.RecWt$Bmass<-SPA3.Inner.RecWt$kg*SPA3.Inner.RecWt$NH
SPA3.Inner.RecWt

############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

years <- 1991:surveyyear
X <- length(years)

SPA3.Inner.CommWt.simple <- data.frame(Year=years, Mean.weight=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(197553.4308, X),Area=rep("InVMS", X), Age=rep("Comm",X))
for(i in 1:length(SPA3.Inner.CommWt.simple$Year)){
  temp.data <- BIliveweight[BIliveweight$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.Inner.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
	SPA3.Inner.CommWt.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
}

SPA3.Inner.CommWt.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Inner.CommWt.simple$Year, SPA3.Inner.CommWt.simple$Mean.weight, xout=2020) #  2662.952
SPA3.Inner.CommWt.simple[SPA3.Inner.CommWt.simple$Year==2020,c(2,3)]<-c(2662.952,4322021.8) #assume var from 2019


## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Inner.spr.estWt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr  (tows in 2007 that were repeats of 2006)
INc1<-spr(liveweight2006$TOW_NO[liveweight2006$STRATA_ID==99],apply(liveweight2006[liveweight2006$STRATA_ID==99,26:52],1,sum),
liveweight2007$TOW_NO[liveweight2007$STRATA_ID==99],apply(liveweight2007[liveweight2007$STRATA_ID==99,29:52],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INc1)            # 1630
Inner.spr.estWt[Inner.spr.estWt$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
INc2<-spr(liveweight2007$TOW_NO[liveweight2007$STRATA_ID==99],apply(liveweight2007[liveweight2007$STRATA_ID==99,26:52],1,sum),
liveweight2008$TOW_NO[liveweight2008$STRATA_ID==99],apply(liveweight2008[liveweight2008$STRATA_ID==99,29:52],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc2, summary (INc1))   #1424
Inner.spr.estWt[Inner.spr.estWt$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
INc3<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID==99],apply(liveweight2008[liveweight2008$STRATA_ID==99,26:52],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID==99],apply(liveweight2009[liveweight2009$STRATA_ID==99,29:52],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc3,summary (INc2, summary (INc1)))    #929.8
Inner.spr.estWt[Inner.spr.estWt$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
INc4<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID==99],apply(liveweight2009[liveweight2009$STRATA_ID==99,26:52],1,sum),
liveweight2010$TOW_NO[liveweight2010$STRATA_ID==99],apply(liveweight2010[liveweight2010$STRATA_ID==99,29:52],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc4, summary (INc3,summary (INc2, summary (INc1))))    #902.8
Inner.spr.estWt[Inner.spr.estWt$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
INc5<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID==99],apply(liveweight2010[liveweight2010$STRATA_ID==99,26:52],1,sum),
liveweight2011$TOW_NO[liveweight2011$STRATA_ID==99],apply(liveweight2011[liveweight2011$STRATA_ID==99,29:52],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))) # 1394
Inner.spr.estWt[Inner.spr.estWt$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
INc6<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID==99],apply(liveweight2011[liveweight2011$STRATA_ID==99,26:52],1,sum),
liveweight2012$TOW_NO[liveweight2012$STRATA_ID==99],apply(liveweight2012[liveweight2012$STRATA_ID==99,29:52],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))    # 1544
Inner.spr.estWt[Inner.spr.estWt$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
INc7<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID==99],apply(liveweight2012[liveweight2012$STRATA_ID==99,26:52],1,sum),
liveweight2013$TOW_NO[liveweight2013$STRATA_ID==99],apply(liveweight2013[liveweight2013$STRATA_ID==99,29:52],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1)))))))   #2460
Inner.spr.estWt[Inner.spr.estWt$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
INc8<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID==99],apply(liveweight2013[liveweight2013$STRATA_ID==99,26:52],1,sum),
          liveweight2014$TOW_NO[liveweight2014$STRATA_ID==99],apply(liveweight2014[liveweight2014$STRATA_ID==99,29:52],1,sum),
          crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))))  #2461
Inner.spr.estWt[Inner.spr.estWt$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
INc9<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID==99],apply(liveweight2014[liveweight2014$STRATA_ID==99,26:52],1,sum),
          liveweight2015$TOW_NO[liveweight2015$STRATA_ID==99],apply(liveweight2015[liveweight2015$STRATA_ID==99,29:52],1,sum),
          crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1)))))))))  #
Inner.spr.estWt[Inner.spr.estWt$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
INc10<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID==99],apply(liveweight2015[liveweight2015$STRATA_ID==99,26:52],1,sum),
          liveweight2016$TOW_NO[liveweight2016$STRATA_ID==99],apply(liveweight2016[liveweight2016$STRATA_ID==99,29:52],1,sum),
          crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))))))  #
Inner.spr.estWt[Inner.spr.estWt$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
INc11<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID==99],apply(liveweight2016[liveweight2016$STRATA_ID==99,26:52],1,sum),
          liveweight2017$TOW_NO[liveweight2017$STRATA_ID==99],apply(liveweight2017[liveweight2017$STRATA_ID==99,29:52],1,sum),
          crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1)))))))))))  #
Inner.spr.estWt[Inner.spr.estWt$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
INc12<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID==99],apply(liveweight2017[liveweight2017$STRATA_ID==99,26:52],1,sum),
           liveweight2018$TOW_NO[liveweight2018$STRATA_ID==99],apply(liveweight2018[liveweight2018$STRATA_ID==99,29:52],1,sum),
           crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))))))))  #
Inner.spr.estWt[Inner.spr.estWt$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
INc13<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID==99],apply(liveweight2018[liveweight2018$STRATA_ID==99,26:52],1,sum),
           liveweight2019$TOW_NO[liveweight2019$STRATA_ID==99],apply(liveweight2019[liveweight2019$STRATA_ID==99,29:52],1,sum),
           crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary(INc13,summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1)))))))))))))  #
Inner.spr.estWt[Inner.spr.estWt$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020 
INc14<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID==99],apply(liveweight2019[liveweight2019$STRATA_ID==99,26:52],1,sum),
           liveweight2021$TOW_NO[liveweight2021$STRATA_ID==99],apply(liveweight2021[liveweight2021$STRATA_ID==99,29:52],1,sum),
           crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K<-summary(INc14, summary(INc13,summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))))))))))
Inner.spr.estWt[Inner.spr.estWt$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
INc15 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID==99],apply(liveweight2021[liveweight2021$STRATA_ID==99,26:52],1,sum),
           liveweight2022$TOW_NO[liveweight2022$STRATA_ID==99],apply(liveweight2022[liveweight2022$STRATA_ID==99,29:52],1,sum),
           crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INc15, summary(INc14, summary(INc13,summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))))))))))) 
Inner.spr.estWt[Inner.spr.estWt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
INc16 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID==99],apply(liveweight2022[liveweight2022$STRATA_ID==99,26:52],1,sum),
             liveweight2023$TOW_NO[liveweight2023$STRATA_ID==99],apply(liveweight2023[liveweight2023$STRATA_ID==99,29:52],1,sum),
             crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INc16, summary(INc15, summary(INc14, summary(INc13,summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))))))))))))
Inner.spr.estWt[Inner.spr.estWt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
INc17 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID==99],apply(liveweight2023[liveweight2023$STRATA_ID==99,26:52],1,sum),
             liveweight2024$TOW_NO[liveweight2024$STRATA_ID==99],apply(liveweight2024[liveweight2024$STRATA_ID==99,29:52],1,sum),
             crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INc17,summary(INc16, summary(INc15, summary(INc14, summary(INc13,summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1)))))))))))))))))
Inner.spr.estWt[Inner.spr.estWt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2024/2025 
INc18 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID==99],apply(liveweight2024[liveweight2024$STRATA_ID==99,26:52],1,sum),
             liveweight2025$TOW_NO[liveweight2025$STRATA_ID==99],apply(liveweight2025[liveweight2025$STRATA_ID==99,29:52],1,sum),
             crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INc18,summary(INc17,summary(INc16, summary(INc15, summary(INc14, summary(INc13,summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1))))))))))))))))))
Inner.spr.estWt[Inner.spr.estWt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
INc19 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID==99],apply(liveweight2025[liveweight2025$STRATA_ID==99,26:52],1,sum),
             liveweight2026$TOW_NO[liveweight2026$STRATA_ID==99],apply(liveweight2026[liveweight2026$STRATA_ID==99,29:52],1,sum),
             crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INc19,summary(INc18,summary(INc17,summary(INc16, summary(INc15, summary(INc14, summary(INc13,summary(INc12, summary (INc11,summary(INc10, summary (INc9, summary (INc8, summary (INc7,summary (INc6,summary (INc5, summary (INc4, summary (INc3,summary (INc2, summary (INc1)))))))))))))))))))
Inner.spr.estWt[Inner.spr.estWt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Inner.spr.estWt

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Inner.spr.estWt$Year, Inner.spr.estWt$Yspr, xout=2020) #  2554.759
Inner.spr.estWt[Inner.spr.estWt$Year==2020,c(2,3)]<-c(2554.759,44840.25) #assume var from 2019

#make dataframe for all of Inner Commercial
Inner.spr.estWt$method <- "spr"
Inner.spr.estWt$NH <- 197553.4308
Inner.spr.estWt$Area <- "InVMS"
Inner.spr.estWt$Age <- "Comm"
names(Inner.spr.estWt) <- c("Year", "Mean.weight", "var.y", "method", "NH", "Area", "Age")

SPA3.Inner.CommWt <- rbind(SPA3.Inner.CommWt.simple[SPA3.Inner.CommWt.simple$Year<2007,], Inner.spr.estWt)

SPA3.Inner.CommWt$cv <- sqrt(SPA3.Inner.CommWt$var.y)/SPA3.Inner.CommWt$Mean.weight
SPA3.Inner.CommWt$kg<-SPA3.Inner.CommWt$Mean.weight/1000
SPA3.Inner.CommWt$Bmass<-SPA3.Inner.CommWt$kg*SPA3.Inner.CommWt$NH

#combine Recruit and Commercial dataframes for Inner
SPA3.InVMS.Weight<-rbind(SPA3.Inner.RecWt, SPA3.Inner.CommWt)
SPA3.InVMS.Weight

###
# ---- Outside VMS Strata ---- 
###

############ Recruit (65-79 mm)
#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 1991:surveyyear
X <- length(years)

SPA3.Outer.RecWt.simple <- data.frame(Year=years, Mean.weight=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(555399.33, X),Area=rep("OutVMS", X), Age=rep("Recruit",X))
for(i in 1:length(SPA3.Outer.RecWt.simple$Year)){
  temp.data <- BIliveweight[BIliveweight$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.Outer.RecWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
	SPA3.Outer.RecWt.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 26:28],1,sum))
}

SPA3.Outer.RecWt.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Outer.RecWt.simple$Year, SPA3.Outer.RecWt.simple$Mean.weight, xout=2020) #  6.256195
SPA3.Outer.RecWt.simple[SPA3.Outer.RecWt.simple$Year==2020,c(2,3)]<-c(6.256195,125.24515) #assume var from 2019

#spr for Recruits
## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Outer.rec.spr.estWt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007 spr  (tows in 2007 that were repeats of 2006)
OUTr1<-spr(liveweight2006$TOW_NO[liveweight2006$STRATA_ID%in%23:24],apply(liveweight2006[liveweight2006$STRATA_ID%in%23:24,23:25],1,sum),liveweight2007$TOW_NO[liveweight2007$STRATA_ID%in%23:24],apply(liveweight2007[liveweight2007$STRATA_ID%in%23:24,26:28],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTr1)   #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007.2008
OUTr2<-spr(liveweight2007$TOW_NO[liveweight2007$STRATA_ID%in%23:24],apply(liveweight2007[liveweight2007$STRATA_ID%in%23:24,23:25],1,sum),
liveweight2008$TOW_NO[liveweight2008$STRATA_ID%in%23:24],apply(liveweight2008[liveweight2008$STRATA_ID%in%23:24,26:28],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTr2)   #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008.2009
OUTr3<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID%in%23:24],apply(liveweight2008[liveweight2008$STRATA_ID%in%23:24,23:25],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID%in%23:24],apply(liveweight2009[liveweight2009$STRATA_ID%in%23:24,26:28],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTr3)   #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009.2010
OUTr4<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID%in%23:24],apply(liveweight2009[liveweight2009$STRATA_ID%in%23:24,23:25],1,sum),
liveweight2010$TOW_NO[liveweight2010$STRATA_ID%in%23:24],apply(liveweight2010[liveweight2010$STRATA_ID%in%23:24,26:28],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTr4)  #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010.2011
OUTr5<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID%in%23:24],apply(liveweight2010[liveweight2010$STRATA_ID%in%23:24,23:25],1,sum),liveweight2011$TOW_NO[liveweight2011$STRATA_ID%in%23:24],apply(liveweight2011[liveweight2011$STRATA_ID%in%23:24,26:28],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTr5)  # 581.5
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011.2012
OUTr6<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID%in%23:24],apply(liveweight2011[liveweight2011$STRATA_ID%in%23:24,23:25],1,sum),liveweight2012$TOW_NO[liveweight2012$STRATA_ID%in%23:24],apply(liveweight2012[liveweight2012$STRATA_ID%in%23:24,26:28],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTr6)  #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
OUTr7<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID%in%23:24],apply(liveweight2012[liveweight2012$STRATA_ID%in%23:24,23:25],1,sum),liveweight2013$TOW_NO[liveweight2013$STRATA_ID%in%23:24],apply(liveweight2013[liveweight2013$STRATA_ID%in%23:24,26:28],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (OUTr7)    #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
OUTr8<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID%in%23:24],apply(liveweight2013[liveweight2013$STRATA_ID%in%23:24,23:25],1,sum),liveweight2014$TOW_NO[liveweight2014$STRATA_ID%in%23:24],apply(liveweight2014[liveweight2014$STRATA_ID%in%23:24,26:28],1,sum), crossref.spa3.2014[crossref.spa3.2014$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr8)    #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
OUTr9<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID%in%23:24],apply(liveweight2014[liveweight2014$STRATA_ID%in%23:24,23:25],1,sum),liveweight2015$TOW_NO[liveweight2015$STRATA_ID%in%23:24],apply(liveweight2015[liveweight2015$STRATA_ID%in%23:24,26:28],1,sum), crossref.spa3.2015[crossref.spa3.2015$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr9)    #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
OUTr10<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID%in%23:24],apply(liveweight2015[liveweight2015$STRATA_ID%in%23:24,23:25],1,sum),liveweight2016$TOW_NO[liveweight2016$STRATA_ID%in%23:24],apply(liveweight2016[liveweight2016$STRATA_ID%in%23:24,26:28],1,sum), crossref.spa3.2016[crossref.spa3.2016$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr10)    #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
OUTr11<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID%in%23:24],apply(liveweight2016[liveweight2016$STRATA_ID%in%23:24,23:25],1,sum),liveweight2017$TOW_NO[liveweight2017$STRATA_ID%in%23:24],apply(liveweight2017[liveweight2017$STRATA_ID%in%23:24,26:28],1,sum), crossref.spa3.2017[crossref.spa3.2017$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr11)    #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
OUTr12<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID%in%23:24],apply(liveweight2017[liveweight2017$STRATA_ID%in%23:24,23:25],1,sum),liveweight2018$TOW_NO[liveweight2018$STRATA_ID%in%23:24],apply(liveweight2018[liveweight2018$STRATA_ID%in%23:24,26:28],1,sum), crossref.spa3.2018[crossref.spa3.2018$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr12,summary(OUTr11) )    #
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
#OUTr13<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID%in%23:24],apply(liveweight2018[liveweight2018$STRATA_ID%in%23:24,23:25],1,sum),liveweight2019$TOW_NO[liveweight2019$STRATA_ID%in%23:24],apply(liveweight2019[liveweight2019$STRATA_ID%in%23:24,26:28],1,sum), crossref.spa3.2019[crossref.spa3.2019$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

#K<-summary(OUTr13)     #tows matched to in 2018 had mean 0 - SPR breaks, used simple mean for 2019 
#Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 # no survey in 2020 
OUTr14<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID%in%23:24],apply(liveweight2019[liveweight2019$STRATA_ID%in%23:24,23:25],1,sum),liveweight2021$TOW_NO[liveweight2021$STRATA_ID%in%23:24],apply(liveweight2021[liveweight2021$STRATA_ID%in%23:24,26:28],1,sum), crossref.spa3.2021[crossref.spa3.2021$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr14)     
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
OUTr15 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID%in%23:24],apply(liveweight2021[liveweight2021$STRATA_ID%in%23:24,23:25],1,sum),liveweight2022$TOW_NO[liveweight2022$STRATA_ID%in%23:24],apply(liveweight2022[liveweight2022$STRATA_ID%in%23:24,26:28],1,sum), 
              crossref.spa3.2022[crossref.spa3.2022$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr15)     
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
OUTr16 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID%in%23:24],apply(liveweight2022[liveweight2022$STRATA_ID%in%23:24,23:25],1,sum),liveweight2023$TOW_NO[liveweight2023$STRATA_ID%in%23:24],apply(liveweight2023[liveweight2023$STRATA_ID%in%23:24,26:28],1,sum), 
              crossref.spa3.2023[crossref.spa3.2023$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr16)     
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
OUTr17 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID%in%23:24],apply(liveweight2023[liveweight2023$STRATA_ID%in%23:24,23:25],1,sum),liveweight2024$TOW_NO[liveweight2024$STRATA_ID%in%23:24],apply(liveweight2024[liveweight2024$STRATA_ID%in%23:24,26:28],1,sum), 
              crossref.spa3.2024[crossref.spa3.2024$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr17)     
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
OUTr18 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID%in%23:24],apply(liveweight2024[liveweight2024$STRATA_ID%in%23:24,23:25],1,sum),liveweight2025$TOW_NO[liveweight2025$STRATA_ID%in%23:24],apply(liveweight2025[liveweight2025$STRATA_ID%in%23:24,26:28],1,sum), 
              crossref.spa3.2025[crossref.spa3.2025$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr18)     
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
OUTr19 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID%in%23:24],apply(liveweight2025[liveweight2025$STRATA_ID%in%23:24,23:25],1,sum),liveweight2026$TOW_NO[liveweight2026$STRATA_ID%in%23:24],apply(liveweight2026[liveweight2026$STRATA_ID%in%23:24,26:28],1,sum), 
              crossref.spa3.2026[crossref.spa3.2026$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTr19)     
Outer.rec.spr.estWt[Outer.rec.spr.estWt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Outer.rec.spr.estWt

Outer.rec.spr.estWt$method <- "spr"
Outer.rec.spr.estWt$NH <- 555399.33
Outer.rec.spr.estWt$Area <- "OutVMS"
Outer.rec.spr.estWt$Age <- "Recruit"
names(Outer.rec.spr.estWt) <- c("Year", "Mean.weight", "var.y", "method", "NH", "Area", "Age")

SPA3.Outer.RecWt<- rbind(SPA3.Outer.RecWt.simple[SPA3.Outer.RecWt.simple$Year<2007,], Outer.rec.spr.estWt)
SPA3.Outer.RecWt[SPA3.Outer.RecWt$Year==2019,] <- SPA3.Outer.RecWt.simple[SPA3.Outer.RecWt.simple$Year==2019,] ##Note no recruits found in the repeated tows in 2018 in outer vms area - need to use SIMPLE estimate
SPA3.Outer.RecWt[SPA3.Outer.RecWt$Year==2020,] <- SPA3.Outer.RecWt.simple[SPA3.Outer.RecWt.simple$Year==2020,] #Bc no estimate in 2019 from SPR - need to use simple mean; must use simple mean for interpolation for 2020 
SPA3.Outer.RecWt[SPA3.Outer.RecWt$Year==2022,] <- SPA3.Outer.RecWt.simple[SPA3.Outer.RecWt.simple$Year==2022,] 
SPA3.Outer.RecWt[SPA3.Outer.RecWt$Year==2023,] <- SPA3.Outer.RecWt.simple[SPA3.Outer.RecWt.simple$Year==2023,] #Simple mean used because two repeated tows in 2023 were Exploratory in 2022.
SPA3.Outer.RecWt[SPA3.Outer.RecWt$Year==2026,] <- SPA3.Outer.RecWt.simple[SPA3.Outer.RecWt.simple$Year==2026,]

SPA3.Outer.RecWt$cv <- sqrt(SPA3.Outer.RecWt$var.y)/SPA3.Outer.RecWt$Mean.weight
SPA3.Outer.RecWt$kg<-SPA3.Outer.RecWt$Mean.weight/1000
SPA3.Outer.RecWt$Bmass<-SPA3.Outer.RecWt$kg*SPA3.Outer.RecWt$NH
SPA3.Outer.RecWt

############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

years <- 1991:surveyyear
X <- length(years)

SPA3.Outer.CommWt.simple <- data.frame(Year=years, Mean.weight=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X), NH=rep(555399.33, X), Area=rep("OutVMS", X), Age=rep("Comm",X))
for(i in 1:length(SPA3.Outer.CommWt.simple$Year)){
  temp.data <- BIliveweight[BIliveweight$YEAR==1990+i,]
	#add calculation of variance if required
	SPA3.Outer.CommWt.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
	SPA3.Outer.CommWt.simple[i,3] <- var(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 29:52],1,sum))
}

SPA3.Outer.CommWt.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA3.Outer.CommWt.simple$Year, SPA3.Outer.CommWt.simple$Mean.weight, xout=2020) #  1036.542
SPA3.Outer.CommWt.simple[SPA3.Outer.CommWt.simple$Year==2020,c(2,3)]<-c(1036.542,1646442.6) #assume var from 2019

## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
Outer.spr.estWt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))


#2006/2007 spr  (tows in 2007 that were repeats of 2006)
OUTc1<-spr(liveweight2006$TOW_NO[liveweight2006$STRATA_ID%in%23:24],apply(liveweight2006[liveweight2006$STRATA_ID%in%23:24,26:52],1,sum),
liveweight2007$TOW_NO[liveweight2007$STRATA_ID%in%23:24],apply(liveweight2007[liveweight2007$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTc1)   # 929.7
Outer.spr.estWt[Outer.spr.estWt$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007.2008
OUTc2<-spr(liveweight2007$TOW_NO[liveweight2007$STRATA_ID%in%23:24],apply(liveweight2007[liveweight2007$STRATA_ID%in%23:24,26:52],1,sum),
liveweight2008$TOW_NO[liveweight2008$STRATA_ID%in%23:24],apply(liveweight2008[liveweight2008$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTc2,summary (OUTc1))   #  754.5
Outer.spr.estWt[Outer.spr.estWt$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008.2009
OUTc3<-spr(liveweight2008$TOW_NO[liveweight2008$STRATA_ID%in%23:24],apply(liveweight2008[liveweight2008$STRATA_ID%in%23:24,26:52],1,sum),
liveweight2009$TOW_NO[liveweight2009$STRATA_ID%in%23:24],apply(liveweight2009[liveweight2009$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTc3, summary (OUTc2,summary (OUTc1)))   # 626.
Outer.spr.estWt[Outer.spr.estWt$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009.2010
OUTc4<-spr(liveweight2009$TOW_NO[liveweight2009$STRATA_ID%in%23:24],apply(liveweight2009[liveweight2009$STRATA_ID%in%23:24,24:55],1,sum),
liveweight2010$TOW_NO[liveweight2010$STRATA_ID%in%23:24],apply(liveweight2010[liveweight2010$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))  # 533.2
Outer.spr.estWt[Outer.spr.estWt$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010.2011
OUTc5<-spr(liveweight2010$TOW_NO[liveweight2010$STRATA_ID%in%23:24],apply(liveweight2010[liveweight2010$STRATA_ID%in%23:24,26:52],1,sum),
liveweight2011$TOW_NO[liveweight2011$STRATA_ID%in%23:24],apply(liveweight2011[liveweight2011$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))  # 581.5
Outer.spr.estWt[Outer.spr.estWt$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011.2012
OUTc6<-spr(liveweight2011$TOW_NO[liveweight2011$STRATA_ID%in%23:24],apply(liveweight2011[liveweight2011$STRATA_ID%in%23:24,26:52],1,sum),
liveweight2012$TOW_NO[liveweight2012$STRATA_ID%in%23:24],apply(liveweight2012[liveweight2012$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])
K<-summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))))  #  1143
Outer.spr.estWt[Outer.spr.estWt$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
OUTc7<-spr(liveweight2012$TOW_NO[liveweight2012$STRATA_ID%in%23:24],apply(liveweight2012[liveweight2012$STRATA_ID%in%23:24,26:52],1,sum),
liveweight2013$TOW_NO[liveweight2013$STRATA_ID%in%23:24],apply(liveweight2013[liveweight2013$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))))    #666.5
Outer.spr.estWt[Outer.spr.estWt$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
OUTc8<-spr(liveweight2013$TOW_NO[liveweight2013$STRATA_ID%in%23:24],apply(liveweight2013[liveweight2013$STRATA_ID%in%23:24,26:52],1,sum),
           liveweight2014$TOW_NO[liveweight2014$STRATA_ID%in%23:24],apply(liveweight2014[liveweight2014$STRATA_ID%in%23:24,29:52],1,sum),
           crossref.spa3.2014[crossref.spa3.2014$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))))))    #1478
Outer.spr.estWt[Outer.spr.estWt$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
OUTc9<-spr(liveweight2014$TOW_NO[liveweight2014$STRATA_ID%in%23:24],apply(liveweight2014[liveweight2014$STRATA_ID%in%23:24,26:52],1,sum),
           liveweight2015$TOW_NO[liveweight2015$STRATA_ID%in%23:24],apply(liveweight2015[liveweight2015$STRATA_ID%in%23:24,29:52],1,sum),
           crossref.spa3.2015[crossref.spa3.2015$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))))))    #1099.0881
Outer.spr.estWt[Outer.spr.estWt$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
OUTc10<-spr(liveweight2015$TOW_NO[liveweight2015$STRATA_ID%in%23:24],apply(liveweight2015[liveweight2015$STRATA_ID%in%23:24,26:52],1,sum), liveweight2016$TOW_NO[liveweight2016$STRATA_ID%in%23:24],apply(liveweight2016[liveweight2016$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2016[crossref.spa3.2016$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))))))))    #
Outer.spr.estWt[Outer.spr.estWt$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
OUTc11<-spr(liveweight2016$TOW_NO[liveweight2016$STRATA_ID%in%23:24],apply(liveweight2016[liveweight2016$STRATA_ID%in%23:24,26:52],1,sum), liveweight2017$TOW_NO[liveweight2017$STRATA_ID%in%23:24],apply(liveweight2017[liveweight2017$STRATA_ID%in%23:24,29:52],1,sum),
crossref.spa3.2017[crossref.spa3.2017$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))))))))    #
Outer.spr.estWt[Outer.spr.estWt$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
OUTc12<-spr(liveweight2017$TOW_NO[liveweight2017$STRATA_ID%in%23:24],apply(liveweight2017[liveweight2017$STRATA_ID%in%23:24,26:52],1,sum), liveweight2018$TOW_NO[liveweight2018$STRATA_ID%in%23:24],apply(liveweight2018[liveweight2018$STRATA_ID%in%23:24,29:52],1,sum),
            crossref.spa3.2018[crossref.spa3.2018$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))))))))))   #
Outer.spr.estWt[Outer.spr.estWt$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
OUTc13<-spr(liveweight2018$TOW_NO[liveweight2018$STRATA_ID%in%23:24],apply(liveweight2018[liveweight2018$STRATA_ID%in%23:24,26:52],1,sum), liveweight2019$TOW_NO[liveweight2019$STRATA_ID%in%23:24],apply(liveweight2019[liveweight2019$STRATA_ID%in%23:24,29:52],1,sum),
            crossref.spa3.2019[crossref.spa3.2019$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTc13, summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))))))))))   #
Outer.spr.estWt[Outer.spr.estWt$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020 
OUTc14<-spr(liveweight2019$TOW_NO[liveweight2019$STRATA_ID%in%23:24],apply(liveweight2019[liveweight2019$STRATA_ID%in%23:24,26:52],1,sum), liveweight2021$TOW_NO[liveweight2021$STRATA_ID%in%23:24],apply(liveweight2021[liveweight2021$STRATA_ID%in%23:24,29:52],1,sum),
            crossref.spa3.2021[crossref.spa3.2021$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K<-summary(OUTc14, summary(OUTc13, summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))))))))))) 
Outer.spr.estWt[Outer.spr.estWt$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 #no survey in 2020 
OUTc15 <- spr(liveweight2021$TOW_NO[liveweight2021$STRATA_ID%in%23:24],apply(liveweight2021[liveweight2021$STRATA_ID%in%23:24,26:52],1,sum), liveweight2022$TOW_NO[liveweight2022$STRATA_ID%in%23:24],apply(liveweight2022[liveweight2022$STRATA_ID%in%23:24,29:52],1,sum),
            crossref.spa3.2022[crossref.spa3.2022$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <- summary(OUTc15, summary(OUTc14, summary(OUTc13, summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))))))))))))) 
Outer.spr.estWt[Outer.spr.estWt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
OUTc16 <- spr(liveweight2022$TOW_NO[liveweight2022$STRATA_ID%in%23:24],apply(liveweight2022[liveweight2022$STRATA_ID%in%23:24,26:52],1,sum), liveweight2023$TOW_NO[liveweight2023$STRATA_ID%in%23:24],apply(liveweight2023[liveweight2023$STRATA_ID%in%23:24,29:52],1,sum),
              crossref.spa3.2023[crossref.spa3.2023$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <- summary(OUTc16, summary(OUTc15, summary(OUTc14, summary(OUTc13, summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))))))))))))))
Outer.spr.estWt[Outer.spr.estWt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
OUTc17 <- spr(liveweight2023$TOW_NO[liveweight2023$STRATA_ID%in%23:24],apply(liveweight2023[liveweight2023$STRATA_ID%in%23:24,26:52],1,sum), liveweight2024$TOW_NO[liveweight2024$STRATA_ID%in%23:24],apply(liveweight2024[liveweight2024$STRATA_ID%in%23:24,29:52],1,sum),
              crossref.spa3.2024[crossref.spa3.2024$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <- summary(OUTc17,summary(OUTc16, summary(OUTc15, summary(OUTc14, summary(OUTc13, summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))))))))))))))
Outer.spr.estWt[Outer.spr.estWt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
OUTc18 <- spr(liveweight2024$TOW_NO[liveweight2024$STRATA_ID%in%23:24],apply(liveweight2024[liveweight2024$STRATA_ID%in%23:24,26:52],1,sum), liveweight2025$TOW_NO[liveweight2025$STRATA_ID%in%23:24],apply(liveweight2025[liveweight2025$STRATA_ID%in%23:24,29:52],1,sum),
              crossref.spa3.2025[crossref.spa3.2025$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <-  summary(OUTc18, summary(OUTc17,summary(OUTc16, summary(OUTc15, summary(OUTc14, summary(OUTc13, summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1))))))))))))))))))
Outer.spr.estWt[Outer.spr.estWt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
OUTc19 <- spr(liveweight2025$TOW_NO[liveweight2025$STRATA_ID%in%23:24],apply(liveweight2025[liveweight2025$STRATA_ID%in%23:24,26:52],1,sum), liveweight2026$TOW_NO[liveweight2026$STRATA_ID%in%23:24],apply(liveweight2026[liveweight2026$STRATA_ID%in%23:24,29:52],1,sum),
              crossref.spa3.2026[crossref.spa3.2026$STRATA_ID%in%23:24,c("TOW_NO_REF","TOW_NO")])

K <-  summary(OUTc19, summary(OUTc18, summary(OUTc17,summary(OUTc16, summary(OUTc15, summary(OUTc14, summary(OUTc13, summary(OUTc12, summary (OUTc11,summary(OUTc10, summary (OUTc9, summary (OUTc8, summary (OUTc7,summary (OUTc6,summary (OUTc5, summary (OUTc4, summary (OUTc3, summary (OUTc2,summary (OUTc1)))))))))))))))))))
Outer.spr.estWt[Outer.spr.estWt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


Outer.spr.estWt

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Outer.spr.estWt$Year, Outer.spr.estWt$Yspr, xout=2020) #   814.1649
Outer.spr.estWt[Outer.spr.estWt$Year==2020,c(2,3)]<-c( 814.1649,12176.429) #assume var from 2019


Outer.spr.estWt$method <- "spr"
Outer.spr.estWt$NH <- 555399.33
Outer.spr.estWt$Area <- "OutVMS"
Outer.spr.estWt$Age <- "Comm"
names(Outer.spr.estWt) <- c("Year", "Mean.weight", "var.y", "method", "NH", "Area", "Age")

SPA3.Outer.CommWt <- rbind(SPA3.Outer.CommWt.simple[SPA3.Outer.CommWt.simple$Year<2007,], Outer.spr.estWt)

SPA3.Outer.CommWt$cv <- sqrt(SPA3.Outer.CommWt$var.y)/SPA3.Outer.CommWt$Mean.weight
SPA3.Outer.CommWt$kg <- SPA3.Outer.CommWt$Mean.weight/1000
SPA3.Outer.CommWt$Bmass <- SPA3.Outer.CommWt$kg*SPA3.Outer.CommWt$NH
SPA3.Outer.CommWt

# combine Recruit and Commercial dataframes for Outer

SPA3.OutVMS.Weight<-rbind(SPA3.Outer.RecWt, SPA3.Outer.CommWt)
SPA3.OutVMS.Weight

####
#  Make Weight indicies dataframe for SPA3 and export 
####
SPA3.Weight <- rbind(SPA3.SMB.Weight, SPA3.InVMS.Weight, SPA3.OutVMS.Weight)

write.csv(SPA3.Weight, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3.Index.Weight.",surveyyear,".csv"))

####
### ----  Calculate Commercial (N) Population size, Commercial (I) and Recruit (R) biomass for the model in tons ----
# Assessment model only uses SMB and Inner to estimate biomass
# model only needs 1996+

# Population Numbers of Commercial size (N)
spa3.pop.smb <- SPA3.SMB.Comm %>% filter(Year >= 1996) %>% dplyr::select(Year, Pop)
spa3.pop.inner <- SPA3.Inner.Comm %>% filter(Year >= 1996) %>% dplyr::select(Year, Pop)
#Ensure year ranges are the same: 
spa3.pop.smb$Year == spa3.pop.inner$Year
#Create object for N (population size) for SPA 3 Model area: 
N <- data.frame(Year = spa3.pop.smb$Year, N = spa3.pop.smb$Pop + spa3.pop.inner$Pop)
N 

# Commercial Biomass Index (I)
spa3.bmass.smb <- SPA3.SMB.CommWt %>% filter(Year >= 1996) %>% dplyr::select(Year, Bmass)
spa3.bmass.inner <- SPA3.Inner.CommWt %>% filter(Year >= 1996) %>% dplyr::select(Year, Bmass)
#Ensure year ranges are the same: 
spa3.bmass.smb$Year == spa3.bmass.inner$Year
#Create object for I (Commercial biomass) for SPA 3 Model area:
I  <- data.frame(Year = spa3.bmass.smb$Year, Bmass = (spa3.bmass.smb$Bmass + spa3.bmass.inner$Bmass)/1000)
I

# Recruit Biomass Index (IR)
spa3.bmass.rec.smb <- SPA3.SMB.RecWt %>% filter(Year >= 1996) %>% dplyr::select(Year, Bmass)
spa3.bmass.rec.inner <- SPA3.Inner.RecWt %>% filter(Year >= 1996) %>% dplyr::select(Year, Bmass)
#Ensure year ranges are the same: 
spa3.bmass.rec.smb$Year == spa3.bmass.rec.inner$Year
#Create object for I (Commercial biomass) for SPA 3 Model area:
IR  <- data.frame(Year = spa3.bmass.rec.smb$Year, Bmass = (spa3.bmass.rec.smb$Bmass + spa3.bmass.rec.inner$Bmass)/1000)
IR 


# ----  Calculate CV for model I.cv and R.cv ----
#n tows (update each year) (technically only need 1996-2006 for simple means years)

years <- 1996:surveyyear
X <- length(years)
tow.SPA3 <- data.frame(Year=(years), SMB.tow=rep(NA,X),InVMS.tow=rep(NA,X))
for(i in 1:length(years)){
temp.data<-BIlivefreq.dat[BIlivefreq.dat$TOW_TYPE_ID%in%c(1,5) & BIlivefreq.dat$YEAR==1995+i,]
 tow.SPA3 [i,2] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==22])) #SMB
 tow.SPA3 [i,3] <-  length(unique(temp.data$TOW_NO[temp.data$STRATA_ID==99])) #InVMS
}
tow.SPA3

#for areas with missing years, the no.tows in those years is from the area/year before - used to estimate the mean (e.g. interpolation in SMB), see code above for SMB to see how missing years in the  time series are filled in.
tow.SPA3$SMB.tow[tow.SPA3$Year%in%c(1997,1998)] <- tow.SPA3$SMB.tow[tow.SPA3$Year==1996]
tow.SPA3$SMB.tow[tow.SPA3$Year%in%c(2002,2003)] <- tow.SPA3$SMB.tow[tow.SPA3$Year==2001]
tow.SPA3$SMB.tow[tow.SPA3$Year%in%c(2020)] <- tow.SPA3$SMB.tow[tow.SPA3$Year==2019]
tow.SPA3$InVMS.tow[tow.SPA3$Year%in%c(2020)] <- tow.SPA3$InVMS.tow[tow.SPA3$Year==2019]

tow.SMB <- tow.SPA3$SMB.tow
tow.VMS <- tow.SPA3$InVMS.tow #inside VMS only

#...COMMERCIAL I.CV
# Check that year ranges match with Method used for estimate (e.g. simple vs SPR)
SPA3.SMB.CommWt
#assuming all from 2007 on is SPR and prior simple: 
#simple mean estimates (years 1996 to 2006)
years <- 1996:2006
X <- length(years)

se.SPA3 <- data.frame(Year=(years), SMB.var=SPA3.SMB.CommWt$var.y[6:16],InVMS.var=SPA3.Inner.CommWt$var.y[6:16])
se.SPA3$sum.var <- (se.SPA3$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[1:11] + (se.SPA3$InVMS.var*197553.4308^2)/tow.VMS[1:11]
se.SPA3$se <- sqrt(se.SPA3$sum.var)
se.SPA3$I.cv <- se.SPA3$se/(I$Bmass[I$Year<=2006]*1000000)
round(se.SPA3$I.cv,4) #0.1855 0.1713 0.1625 0.1103 0.1420 0.0838 0.2338 0.1142 0.2474 0.0968 0.0865

se.SPA3.1996to2006 <- se.SPA3

#spr estimates (years 2007 on)
#Each year need to update the column range to include the current year: e.g. update [17:26] and [12:22] - NOTE automated in 2018

years <- 2007:surveyyear
X <- length(years)

se.SPA3 <- data.frame(Year=(years), SMB.var=SPA3.SMB.CommWt$var.y[17:dim(SPA3.SMB.CommWt)[1]],InVMS.var=SPA3.Inner.CommWt$var.y[17:dim(SPA3.SMB.CommWt)[1]])# in 2018 update code so didn't need to enter row end index -- be sure to check

se.SPA3$sum.var<-(se.SPA3$SMB.var*117875.8905^2) + (se.SPA3$InVMS.var*197553.4308^2)

se.SPA3$se<-sqrt(se.SPA3$sum.var)
se.SPA3$I.cv<-se.SPA3$se/(I$Bmass[I$Year>=2007]*1000000) #update end - Note in 2018 modified so should not need to update end of vector length I
round(se.SPA3$I.cv,4) 
#0.0914 0.1014 0.0974 0.1034 0.0927 0.0872 0.0749 0.0691 0.0678 0.0901 0.0906 0.0741 0.0932 0.0932 0.0899

#combine cv for full year range 
se.SPA3.1996to2006.cv <- se.SPA3.1996to2006  %>% dplyr::select(Year,  I.cv)
se.SPA3.cv <- se.SPA3 %>% dplyr::select(Year, I.cv)

#Create object for I.cv (Commercial biomass cv) for SPA 3 Model area:
I.cv   <- rbind(se.SPA3.1996to2006.cv, se.SPA3.cv)
I.cv$I.cv <- round(I.cv$I.cv,4 )
I.cv


#...RECRUIT IR.CV

#simple mean estimates (years 1997 to 2006)
years <- 1996:2006
X <- length(years)

se.SPA3.rec <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[6:16],InVMS.var=SPA3.Inner.RecWt$var.y[6:16])
se.SPA3.rec$sum.var<-(se.SPA3.rec$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[1:11] + (se.SPA3.rec$InVMS.var*197553.4308^2)/tow.VMS[1:11]
se.SPA3.rec$se<-sqrt(se.SPA3.rec$sum.var)
se.SPA3.rec$R.cv<-se.SPA3.rec$se/(IR$Bmass[IR$Year<=2006]*1000000)
round(se.SPA3.rec$R.cv,4) #0.3403 0.2314 0.1876 0.4801 0.5849 0.4136 0.3734 0.2195 0.3833 0.1843 0.2983

se.SPA3.rec.1996to2006 <- se.SPA3.rec

#spr years
#Each year need to update the column range to include the current year: e.g. update! [17:27]and [12:22]
years <- 2007:surveyyear
X <- length(years)

se.SPA3.rec <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[17:dim(SPA3.SMB.RecWt)[1]],InVMS.var=SPA3.Inner.RecWt$var.y[17:dim(SPA3.SMB.RecWt)[1]])# in 2018 update code so didn't need to enter row end index -- be sure to check

se.SPA3.rec$sum.var <- (se.SPA3.rec$SMB.var*117875.8905^2) + (se.SPA3.rec$InVMS.var*197553.4308^2)
se.SPA3.rec$se <- sqrt(se.SPA3.rec$sum.var)
se.SPA3.rec$R.cv <- se.SPA3.rec$se/(IR$Bmass[IR$Year>=2007]*1000000)
round(se.SPA3.rec$R.cv,4) #0.2191 0.1952 0.1853 0.2201 0.1692 0.1683 0.1605 0.1510 0.1067,0.2241 --- 2017 to 2019 values are a PROBLEM!! See next steps:

##....For 2017: Exceptional Case: SMB *Recruit* Estimate is the SIMPLE mean - Calculate SUM.VAR for SMB using SIMPLE estimator, Calculate SUM.VAR for INNER using SPR estimator, then combined
years <- 2017
X <- length(years)
xx <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[SPA3.SMB.RecWt$Year==2017],InVMS.var=SPA3.Inner.RecWt$var.y[SPA3.SMB.RecWt$Year==2017])
xx$sum.var<-(xx$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[tow.SPA3$Year==2017] + (xx$InVMS.var*197553.4308^2)
xx$se<-sqrt(xx$sum.var)
xx$R.cv<-xx$se/(IR$Bmass[IR$Year==2017]*1000000)
round(xx$R.cv,4) #0.3985
se.SPA3.rec[se.SPA3.rec$Year==2017,] <- xx #Replace 2017 
se.SPA3.rec
#round(se.SPA3.rec$R.cv, 4) 

##....For 2018: Exceptional Case: SMB Recruit Estimate is the SIMPLE mean - Calculate SUM.VAR for SMB using SIMPLE estimator, Calculate SUM.VAR for INNER using SPR estimator, then combined
years <- 2018
X <- length(years)
xx <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[SPA3.SMB.RecWt$Year==2018],InVMS.var=SPA3.Inner.RecWt$var.y[SPA3.SMB.RecWt$Year==2018])
xx$sum.var<-(xx$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[tow.SPA3$Year==2018] + (xx$InVMS.var*197553.4308^2)
xx$se<-sqrt(xx$sum.var)
xx$R.cv<-xx$se/(IR$Bmass[IR$Year==2018]*1000000)
round(xx$R.cv,4) #0.1722
se.SPA3.rec[se.SPA3.rec$Year==2018,] <- xx #Replace 2017 
se.SPA3.rec

##....For 2019: Exceptional Case: SMB Recruit Estimate is the SIMPLE mean - Calculate SUM.VAR for SMB using SIMPLE estimator, Calculate SUM.VAR for INNER using SPR estimator, then combined
years <- 2019
X <- length(years)
xx <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[SPA3.SMB.RecWt$Year==2019],InVMS.var=SPA3.Inner.RecWt$var.y[SPA3.SMB.RecWt$Year==2019])
xx$sum.var <- (xx$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[tow.SPA3$Year==2019] + (xx$InVMS.var*197553.4308^2)
xx$se <- sqrt(xx$sum.var)
xx$R.cv <- xx$se/(IR$Bmass[IR$Year==2019]*1000000)
round(xx$R.cv,4) #0.246
se.SPA3.rec[se.SPA3.rec$Year==2019,] <- xx #Replace 2019 
se.SPA3.rec

##....For 2020: Exceptional Case: SMB Recruit Estimate is the SIMPLE mean - Calculate SUM.VAR for SMB using SIMPLE estimator, Calculate SUM.VAR for INNER using SPR estimator, then combined
years <- 2020
X <- length(years)
xx <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[SPA3.SMB.RecWt$Year==2020],InVMS.var=SPA3.Inner.RecWt$var.y[SPA3.SMB.RecWt$Year==2020])
xx$sum.var <- (xx$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[tow.SPA3$Year==2020] + (xx$InVMS.var*197553.4308^2)
xx$se <- sqrt(xx$sum.var)
xx$R.cv <- xx$se/(IR$Bmass[IR$Year==2020]*1000000)
round(xx$R.cv,4) #0.2828
se.SPA3.rec[se.SPA3.rec$Year==2020,] <- xx #Replace 2020 
se.SPA3.rec

##....For 2021: Exceptional Case: SMB Recruit Estimate is the SIMPLE mean - Calculate SUM.VAR for SMB using SIMPLE estimator, Calculate SUM.VAR for INNER using SPR estimator, then combined
years <- 2021
X <- length(years)
xx <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[SPA3.SMB.RecWt$Year==2021],InVMS.var=SPA3.Inner.RecWt$var.y[SPA3.SMB.RecWt$Year==2021])
xx$sum.var <- (xx$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[tow.SPA3$Year==2021] + (xx$InVMS.var*197553.4308^2)
xx$se <- sqrt(xx$sum.var)
xx$R.cv <- xx$se/(IR$Bmass[IR$Year==2021]*1000000)
round(xx$R.cv,4) #0.1848
se.SPA3.rec[se.SPA3.rec$Year==2021,] <- xx #Replace 2021 
se.SPA3.rec

##....For 2024: Exceptional Case: SMB Recruit Estimate is the SIMPLE mean - Calculate SUM.VAR for SMB using SIMPLE estimator, Calculate SUM.VAR for INNER using SPR estimator, then combined
years <- 2024
X <- length(years)
xx <- data.frame(Year=(years), SMB.var=SPA3.SMB.RecWt$var.y[SPA3.SMB.RecWt$Year==2024],InVMS.var=SPA3.Inner.RecWt$var.y[SPA3.SMB.RecWt$Year==2024])
xx$sum.var <- (xx$SMB.var*117875.8905^2)/tow.SPA3$SMB.tow[tow.SPA3$Year==2024] + (xx$InVMS.var*197553.4308^2)
xx$se <- sqrt(xx$sum.var)
xx$R.cv <- xx$se/(IR$Bmass[IR$Year==2024]*1000000)
round(xx$R.cv,4) #0.2758
se.SPA3.rec[se.SPA3.rec$Year==2024,] <- xx #Replace 2024 
se.SPA3.rec



#combine cv for full year range 
se.SPA3.rec.1996to2006.cv <- se.SPA3.rec.1996to2006  %>% dplyr::select(Year,  R.cv)
se.SPA3.rec.cv <- se.SPA3.rec %>% dplyr::select(Year, R.cv)

#Create object for I.cv (Commercial biomass cv) for SPA 3 Model area:
IR.cv   <- rbind(se.SPA3.rec.1996to2006.cv, se.SPA3.rec.cv)
IR.cv$R.cv <- round(IR.cv$R.cv,4 )
IR.cv
#c(0.3403, 0.2314 ,0.1876 ,0.4801, 0.5849 ,0.4136, 0.3734, 0.2195, 0.3833, 0.1843, 0.2983,0.2191, 0.1952 ,0.1853 ,0.2201 ,0.1692, 0.1683 ,0.1605, 0.1510 ,0.1067,0.2241, 0.3985, 0.1722, 0.2462, 0.2828, 0.1848)

#Write out population model inputs N, I, IR, I.cv, IR.cv 
#check all years match up 
cbind(N, I, IR, I.cv, IR.cv)
#Bind into object for export 
SPA3.population.model.input <- cbind(N, I %>% dplyr::select(I = Bmass), IR %>% dplyr::select(IR = Bmass), I.cv %>% dplyr::select(I.cv), IR.cv %>% dplyr::select(IR.cv = R.cv))

#export 
write.csv(SPA3.population.model.input, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3.population.model.input.",surveyyear,".csv"))


####
# ---- FIGURE PLOTS -----
##
# ---- Plot No. per Tow ---- 

data.number <- SPA3.Numbers[SPA3.Numbers$Year>=1996,]
data.number$Size <- data.number$Age
data.number$Size[data.number$Age=="Comm"] <- "Commercial"
data.number$Area_label <- data.number$Area
data.number$Area_label[data.number$Area=="SMB"] <- "Saint Mary's Bay"
data.number$Area_label[data.number$Area=="InVMS"] <- "BILU: Inside VMS"
data.number$Area_label[data.number$Area=="OutVMS"] <- "BILU: Outside VMS"
data.number$Area_label <- factor(data.number$Area_label, levels=c( "Saint Mary's Bay", "BILU: Inside VMS","BILU: Outside VMS" ))

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA",area,"_Numberpertow",surveyyear,".png"),width=8,height=11,units = "in",res=920)

ggplot(data = data.number, aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + facet_wrap(~Area_label, ncol = 1) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.92)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))

dev.off()

#---- Plot weight. per Tow ----

data.weight <- SPA3.Weight[SPA3.Weight$Year>=1996,]
data.weight$Size <- data.weight$Age
data.weight$Size[data.weight$Age=="Comm"] <- "Commercial"
data.weight$Area_label <- data.weight$Area
data.weight$Area_label[data.weight$Area=="SMB"] <- "Saint Mary's Bay"
data.weight$Area_label[data.weight$Area=="InVMS"] <- "BILU: Inside VMS"
data.weight$Area_label[data.weight$Area=="OutVMS"] <- "BILU: Outside VMS"
data.weight$Area_label <- factor(data.weight$Area_label, levels=c( "Saint Mary's Bay", "BILU: Inside VMS","BILU: Outside VMS" ))

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA",area,"_Weightpertow",surveyyear,".png"),width=8,height=11,units = "in",res=920)

ggplot(data = data.weight, aes(x=Year, y=kg  , col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + facet_wrap(~Area_label, ncol = 1) + 
  theme_bw() + ylab("Mean kg/tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.95)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))

dev.off()



 




