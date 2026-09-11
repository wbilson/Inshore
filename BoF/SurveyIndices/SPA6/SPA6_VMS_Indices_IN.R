###.............................###
### Survey No/tow & Weight/tow  ###
###    SPA 6 INSIDE VMS STRATA  ###
###                             ###
###    Revamped July 2021      ###
###     J.Sameoto              ###
###.............................###

#NOTES:
#2005 data in BF cruise data
#6A - SPR design started in 6A officially in 2007 (CSAS Res Doc: 2008/002)
# Tows from 2006 SPA 6B SPR design were reassigned to 'new' strata areas including 6A during the 2008 SCALLSUR restratification and cleanup. Some are now in 6A but are not used for the mean estimates.

library(tidyverse)
library(ROracle)
library(lubridate)
library(PBSmapping)
library(spr)
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

# polygons to for assigning new strata to data
inVMS <- read.csv("Y:/Inshore/Assessment/BoF/2015/SPA6/Survey/SPA6_VMS_IN_R_final_MOD.csv")
#inVMS <- read.csv("Y:/Inshore/BoF/2015/SPA6/Survey/SPA6_VMS_IN_R_final_MOD.csv")
outvms <- read.csv("Y:/Inshore/Assessment/BoF/2015/SPA6/Survey/SPA6_VMS_OUT_R_final_MOD.csv")
#outvms <- read.csv("Y:/Inshore/BoF/2015/SPA6/Survey/SPA6_VMS_OUT_R_final_MOD.csv")

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "6"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

### Livefreq data ###
#SQL query 2
quer2 <- "SELECT * 			                
FROM scallsur.scliveres s			
LEFT JOIN                         
(SELECT tow_date, cruise, tow_no     		
FROM SCALLSUR.sctows) t             
on (s.cruise = t.cruise and s.tow_no = t.tow_no)
where strata_id in (30, 31, 32)"

livefreq <- dbGetQuery(chan, quer2)

livefreq <- livefreq[,1:51]
livefreq$YEAR <- year(livefreq$TOW_DATE) 
livefreq$lat <- convert.dd.dddd(livefreq$START_LAT)
livefreq$lon <- convert.dd.dddd(livefreq$START_LONG)
livefreq.all <- livefreq

livefreq.all$lat <- convert.dd.dddd(livefreq.all$START_LAT)
livefreq.all$lon <- convert.dd.dddd(livefreq.all$START_LONG)
livefreq.all$ID <- 1:nrow(livefreq.all)

#identify tows "inside" the VMS strata and call them "IN"
livefreq.all$VMSSTRATA <- ""
events <- subset(livefreq.all,STRATA_ID%in%30:32,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
livefreq.all$VMSSTRATA[livefreq.all$ID%in%findPolys(events,inVMS)$EID] <- "IN"
livefreq.all$VMSSTRATA[livefreq.all$ID%in%findPolys(events,outvms)$EID] <- "OUT"
livefreq <- livefreq.all
livefreq$CruiseID <- paste0(livefreq$CRUISE,"." ,livefreq$TOW_NO)

#Clean up VMSSTRATA for GM2017
livefreq$VMSSTRATA[livefreq$TOW_NO==86&livefreq$CRUISE=='GM2017'] <- "OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==23&livefreq$CRUISE=='GM2017'] <- "OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==104&livefreq$CRUISE=='GM2017'] <- "OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==120&livefreq$CRUISE=='GM2017'] <- "OUT"

### Get Cross Reference Data ### 
#Set SQL 
quer1 <- "SELECT * FROM scallsur.screpeatedtows"

# Select data from database; execute query with ROracle
crossref <- dbGetQuery(chan, quer1)

# Clean crossref table - remove all cross ref tows where tow type ID in year t-1 was 3
	type3tows <- merge(crossref[,c('CRUISE_REF','TOW_NO_REF')], livefreq[,c('CRUISE','TOW_NO','TOW_TYPE_ID','VMSSTRATA')], by.x=c('CRUISE_REF','TOW_NO_REF'), by.y=c('CRUISE','TOW_NO'))
	type3tows <- type3tows[type3tows$TOW_TYPE_ID==3,]
	type3tows$ID <- paste(type3tows$CRUISE_REF, type3tows$TOW_NO_REF, sep='.')
	crossref$ID <- paste(crossref$CRUISE_REF, crossref$TOW_NO_REF, sep='.')
	crossref <- crossref[!crossref$ID %in% type3tows$ID,]

	
#merge STRATA_ID from livefreq to the crosssref files based on parent/reference tow
	livefreq$ID <- paste(livefreq$CRUISE, livefreq$TOW_NO, sep=".")
	crossref <-merge (crossref, subset(livefreq, select=c("VMSSTRATA", "ID")), by=c("ID"), all=FALSE)
	livefreq <- livefreq[livefreq$TOW_TYPE_I==1|livefreq$TOW_TYPE_I==5,] #limit tows to tow type 1 and 5. Requires that tow type 3 repeated tows be removed from crossref table (see above)

# Change name of VMSSTRATA to VMSAREA in crossref and livefreq  - so will match code from previous year
#	names(livefreq)[56] <- 'VMSAREA' #check column numbers!
#	names(crossref)[7] <- 'VMSAREA'
	names(livefreq)[names(livefreq)=="VMSSTRATA"] <- "VMSAREA"
	names(crossref)[names(crossref)=="VMSSTRATA"] <- "VMSAREA"
	
### Get meat weight data ### 
# read in meat weight data; this is output from the meat weight/shell height modelling
#code for reading in multiple csvs at once and combining into one dataframe from D.Keith
	#Year <- c(seq(1997,2003),seq(2005,2019))
	Year <- c(seq(1997,2003),seq(2005,2019),seq(2021,surveyyear))
	num.years <- length(Year)
	
	GMliveweight <- NULL
	for(i in 1:num.years)
	{
	  # Make a list with each years data in it, extract it as needed later
	  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/GMliveweight",Year[i],".csv",sep="") ,header=T)
	  GMliveweight <- rbind( GMliveweight,temp)
	}
	
	spa6shw.dat <- GMliveweight
	spa6shw.dat[,13:52] <-  spa6shw.dat[,13:52]/1000 #CHECK - convert tow size bins from grams to kg #SHOULD CHANGE THIS TO GREP 
	spa6shw.dat$lat <- convert.dd.dddd(spa6shw.dat$START_LAT)
	spa6shw.dat$lon <- convert.dd.dddd(spa6shw.dat$START_LONG)
	spa6shw.dat$ID <- 1:nrow(spa6shw.dat)
	
	#assign new strata based on VMS
	spa6shw.dat$VMSSTRATA <- ""
	
	events=subset(spa6shw.dat,STRATA_ID%in%30:32,c("ID","lon","lat"))
	names(events)<-c("EID","X","Y")
	spa6shw.dat$VMSSTRATA[spa6shw.dat$ID%in%findPolys(events,inVMS)$EID]<-"IN"
	spa6shw.dat$VMSSTRATA[spa6shw.dat$ID%in%findPolys(events,outvms)$EID]<-"OUT"
	
	spa6shw.dat <- spa6shw.dat[spa6shw.dat$TOW_TYPE_I==1|spa6shw.dat$TOW_TYPE_I==5,] #limit tows to tow type 1 and 5. Requires that tow type 3 repeated tows be removed from crossref table (see above)
	
	spa6shw.dat <- select(spa6shw.dat, !c("X","ID"))
	
	# Change name of VMSSTRATA to VMSAREA in crossref and livefreq  - so will match code from previous year
#	spa6shw.dat <- rename(spa6shw.dat, VMSAREA = VMSSTRATA)
	names(spa6shw.dat)[names(spa6shw.dat)=="VMSSTRATA"] <- "VMSAREA"
	
	
	###
	# ---- correct data-sets for errors ----
	###
	#some repearted tows will not be assigned to the same strata as the "parent" tow due to where the START LAT and LONG are
	#repeated tows should be corrected to match parent tow
	#some experimental tows were used as parent tows for repeats, these should be removed
	
	## Crossreference errors
	crossref$VMSAREA[crossref$TOW_NO==55 & crossref$CRUISE=='GM2013'] <- "IN"	
	crossref$VMSAREA[crossref$TOW_NO==20 & crossref$CRUISE == "GM2015"] <- "IN"
	crossref$VMSAREA[crossref$TOW_NO==56 & crossref$CRUISE == "GM2017"] <- "OUT"
	crossref$VMSAREA[crossref$TOW_NO==95 & crossref$CRUISE == "GM2017"] <- "IN"
	crossref$VMSAREA[crossref$TOW_NO==40 & crossref$CRUISE == "GM2018"] <- "OUT"
	crossref$VMSAREA[crossref$TOW_NO==95 & crossref$CRUISE == "GM2019"] <- "OUT"
	
	
	## LiveFreq errors 
	#1. Tow 32 in 2011 (73 in 2010) not assinged to IN
	#   Tow 70 in 2011 (90 in 2010) not assinged to IN
	livefreq$VMSAREA[livefreq$TOW_NO==32 & livefreq$CRUISE == "GM2011"] <- "IN"
	livefreq$VMSAREA[livefreq$TOW_NO==70 & livefreq$CRUISE == "GM2011"] <-"IN"
	
	#2. In 2012 tow 9, 27, 111 not assinged to IN
	livefreq$VMSAREA[livefreq$TOW_NO==9 & livefreq$CRUISE == "GM2012"] <- "IN"
	livefreq$VMSAREA[livefreq$TOW_NO==27 & livefreq$CRUISE == "GM2012"] <-"IN"
	livefreq$VMSAREA[livefreq$TOW_NO==111 & livefreq$CRUISE == "GM2012"] <-"IN"
	
	#3. In 2013 tow 9, 27, 111 not assinged to IN
	livefreq$VMSAREA[livefreq$TOW_NO==55 & livefreq$CRUISE == "GM2013"] <- "IN"
	livefreq$VMSAREA[livefreq$TOW_NO==109 & livefreq$CRUISE == "GM2013"] <-"IN"
	livefreq$VMSAREA[livefreq$TOW_NO==116 & livefreq$CRUISE == "GM2013"] <-"IN"
	
	#4. In 2014 tow 9, 27, 111 not assinged to IN
	livefreq$VMSAREA[livefreq$TOW_NO==31 & livefreq$CRUISE == "GM2014"] <- "IN"
	livefreq$VMSAREA[livefreq$TOW_NO==42 & livefreq$CRUISE == "GM2014"] <-"OUT"
	
	#5. In 2015 tow 20 not assigned to IN ( 2015 tow 20 links to 2014 tow 31 which links to a 2013 tow )
	livefreq$VMSAREA[livefreq$TOW_NO==20 & livefreq$CRUISE == "GM2015"] <-"IN"
	livefreq$VMSAREA[livefreq$TOW_NO==25 & livefreq$CRUISE == "GM2015"] <-"OUT"
	
	#6. In 2016 tow 15,75,92 not assinged to OUT
	livefreq$VMSAREA[livefreq$TOW_NO==15 & livefreq$CRUISE == "GM2016"] <-"OUT"
	livefreq$VMSAREA[livefreq$TOW_NO==75 & livefreq$CRUISE == "GM2016"] <-"OUT"
	livefreq$VMSAREA[livefreq$TOW_NO==92 & livefreq$CRUISE == "GM2016"] <-"OUT"
	
	#7. In 2017 tow 15 (is a repeat of GM2016 tow 15) assigned to OUT
	livefreq$VMSAREA[livefreq$TOW_NO==56 & livefreq$CRUISE == "GM2017"] <-"OUT"
	#8 In 2017, tow 95 needs to be IN
	livefreq$VMSAREA[livefreq$TOW_NO==95 & livefreq$CRUISE == "GM2017"] <- "IN"

	#Clean up VMSSTRATA for GM2017
	livefreq$VMSAREA[livefreq$TOW_NO==86 & livefreq$CRUISE == "GM2017"] <- "OUT"
	livefreq$VMSAREA[livefreq$TOW_NO==23 & livefreq$CRUISE == "GM2017"] <- "OUT"
	livefreq$VMSAREA[livefreq$TOW_NO==104 & livefreq$CRUISE == "GM2017"] <- "OUT"
	livefreq$VMSAREA[livefreq$TOW_NO==120 & livefreq$CRUISE == "GM2017"] <- "OUT"
	
	#8. In 2018 tow 40 (is a repeat of GM2017 tow 56) assigned to IN but needs to be OUT - a repeated repeat
	livefreq$VMSAREA[livefreq$TOW_NO==40 & livefreq$CRUISE == "GM2018"] <-"OUT"
	
	#9. In 2019 tow 95 assogmed to IN but needs to be OUT 
	livefreq$VMSAREA[livefreq$TOW_NO==95 & livefreq$CRUISE == "GM2019"] <-"OUT"
	
	
	## Weightfreq errors 
	#1. Tow 32 in 2011 (73 in 2010) not assinged to IN
	#   Tow 70 in 2011 (90 in 2010) not assinged to IN
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==32 & spa6shw.dat$CRUISE == "GM2011"] <- "IN"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==70 & spa6shw.dat$CRUISE == "GM2011"] <-"IN"
	
	#2. In 2012 tow 9, 27, 111 not assinged to IN
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==9 & spa6shw.dat$CRUISE == "GM2012"] <- "IN"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==27 & spa6shw.dat$CRUISE == "GM2012"] <-"IN"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==111 & spa6shw.dat$CRUISE == "GM2012"] <-"IN"
	
	#3. In 2013 tow 9, 27, 111 not assinged to IN
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==55 & spa6shw.dat$CRUISE == "GM2013"] <- "IN"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==109 & spa6shw.dat$CRUISE == "GM2013"] <-"IN"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==116 & spa6shw.dat$CRUISE == "GM2013"] <-"IN"
	
	#4. In 2014 tow 9, 27, 111 not assinged to IN
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==31 & spa6shw.dat$CRUISE == "GM2014"] <- "IN"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==42 & spa6shw.dat$CRUISE == "GM2014"] <-"OUT"
	
	#5. In 2015 tow 20 not assigned to IN ( 2015 tow 20 links to 2014 tow 31 which links to a 2013 tow )
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==20 & spa6shw.dat$CRUISE == "GM2015"] <-"IN"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==25 & spa6shw.dat$CRUISE == "GM2015"] <-"OUT"
	
	#6. In 2016 tow 15,75,92 not assinged to OUT
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==15 & spa6shw.dat$CRUISE == "GM2016"] <-"OUT"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==75 & spa6shw.dat$CRUISE == "GM2016"] <-"OUT"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==92 & spa6shw.dat$CRUISE == "GM2016"] <-"OUT"
	
	#7. In 2017 tow 15 (is a repeat of GM2016 tow 15) assigned to OUT
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==56 & spa6shw.dat$CRUISE == "GM2017"] <-"OUT"
	#8 In 2017, tow 95 needs to be IN
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==95 & spa6shw.dat$CRUISE == "GM2017"] <- "IN"
	
	#Clean up VMSSTRATA for GM2017
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==86 & spa6shw.dat$CRUISE == "GM2017"] <- "OUT"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==23 & spa6shw.dat$CRUISE == "GM2017"] <- "OUT"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==104 & spa6shw.dat$CRUISE == "GM2017"] <- "OUT"
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==120 & spa6shw.dat$CRUISE == "GM2017"] <- "OUT"
	
	#8. In 2018 tow 40 (is a repeat of GM2017 tow 56) assigned to IN but needs to be OUT - a repeated repeat
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==40 & spa6shw.dat$CRUISE == "GM2018"] <-"OUT"
	
	#9. In 2019 tow 95 assogmed to IN but needs to be OUT 
	spa6shw.dat$VMSAREA[spa6shw.dat$TOW_NO==95 & spa6shw.dat$CRUISE == "GM2019"] <-"OUT"

	#Check 
	table(livefreq$VMSAREA)
	table(spa6shw.dat$VMSAREA)
	#determine mismatched strataIDs - pull paired tows from crossref.GM.2019 and compare STRATAID
	#crossref.GM.2019#review matching stations for mis-matched strata id 
	
	
	#determine mismatched strataIDs - pull paired tows from crossref.GM.2019 and compare STRATAID
	#crossref.GM.2019#review matching stations for mis-matched strata id 
	
	#df <- NA
	#for (i in 1:dim(crossref.GM.2011)[1]){
	#tow.no <- crossref.GM.2011$TOW_NO[i]
	#tow.no.ref <- crossref.GM.2011$TOW_NO_REF[i]
	#df[i] <- crossref.GM.2011$VMSAREA[crossref.GM.2011$TOW_NO==tow.no] == GMlivefreq2010$VMSAREA[GMlivefreq2010$TOW_NO==tow.no.ref]
	#}
	
	#which(df=='FALSE')
	#crossref.GM.2019[17,]
	#crossref.GM.2019$TOW_NO[17]
	#which(GMlivefreq2019$TOW_NO==95) #Use to determine which row number to call 
	
	
## ---- Subset data for individual years for SPR command (ONLY TOW_TYPE==1 AND  ONLY TOW_TYPE==5) ----
	tows <- c(1,5)
#livefreq 
	year <- c(seq(2005,2019),seq(2021,surveyyear)) 
	for(i in unique(year)){
	  sub <- subset(livefreq, YEAR==i & TOW_TYPE_ID%in%tows)
	  assign(paste0("GMlivefreq", i), sub)
	}
	#create object for pre2005
	sub <- subset(livefreq, YEAR<2005 & TOW_TYPE_ID%in%tows)
	assign("GMlivefreqpre2005", sub)
	
	
#liveweight 
	year <- c(seq(2005,2019),seq(2021,surveyyear)) 
	for(i in unique(year)){
	  sub <- subset(spa6shw.dat, YEAR==i & TOW_TYPE_ID%in%tows)
	  assign(paste0("spa6shw", i), sub)
	}
	
#Crossref 
	year <- c(seq(2006,2019),seq(2021,surveyyear)) 
	for(i in unique(year)){
	  sub <- subset(crossref, CRUISE==paste0("GM",i))
	  assign(paste0("crossref.GM.", i), sub)
	}
	
	
#Note: checking for mismatches between parent and child tows for repeats in a pain in the a$$... 	



### ---- INSIDE VMS STRATA ----
### ---- Survey index - NUMBERS ---- 
###
#set size ranges
#CS <- 27:50 #commercial size (>=80mm)
#RS <- 24:50 #recruit size that grows to commercial size in 1 year (65-79.9mm)
#PS <-    #precrecruit size that grows to recuit size in 1 year (50-64.9mm)


### ---- SPA 6 IN VMS commercial size (>=80mm) ----
STRATA.ID <- "IN"
years <- 1997:surveyyear
X <- length(years)

#simple means
SPA6.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Comm.simple$Year)){
	temp.data <- livefreq[livefreq$YEAR==1996+i,]
	#add calculation of variance if required
	SPA6.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
	SPA6.Comm.simple[i,3] <- var(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
}
SPA6.Comm.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA6.Comm.simple$Year, SPA6.Comm.simple$Mean.nums, xout=2020) #  230.7911
SPA6.Comm.simple[SPA6.Comm.simple$Year==2020,c("Mean.nums","var.y")] <- c(230.7911, 286582.802) #assume var from 2019



#dataframe for SPR estimates Commercial
spryears <- 2006:surveyyear #update to most recent year
Y <- length(spryears)
spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2005/2006
test.2006 <- spr(GMlivefreq2005$TOW_NO[GMlivefreq2005$VMSAREA==STRATA.ID],apply(GMlivefreq2005[GMlivefreq2005$VMSAREA==STRATA.ID,24:50],1,sum), #recruits included in previous yr
GMlivefreq2006$TOW_NO[GMlivefreq2006$VMSAREA==STRATA.ID],apply(GMlivefreq2006[GMlivefreq2006$VMSAREA==STRATA.ID,27:50],1,sum), #just commercial size for current yr
crossref.GM.2006[crossref.GM.2006$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])

K <- summary(test.2006)  #
    spr.est[spr.est$Year==2006,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2006/2007
    test.2007 <- spr(GMlivefreq2006$TOW_NO[GMlivefreq2006$VMSAREA==STRATA.ID],apply(GMlivefreq2006[GMlivefreq2006$VMSAREA==STRATA.ID,24:50],1,sum), #recruits included in previous yr
    GMlivefreq2007$TOW_NO[GMlivefreq2007$VMSAREA==STRATA.ID],apply(GMlivefreq2007[GMlivefreq2007$VMSAREA==STRATA.ID,27:50],1,sum), #just commercial size for current yr
    crossref.GM.2007[crossref.GM.2007$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary(test.2007,summary(test.2006))  #
    spr.est[spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
    test.2008 <- spr(GMlivefreq2007$TOW_NO[GMlivefreq2007$VMSAREA==STRATA.ID],apply(GMlivefreq2007[GMlivefreq2007$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2008$TOW_NO[GMlivefreq2008$VMSAREA==STRATA.ID],apply(GMlivefreq2008[GMlivefreq2008$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2008[crossref.GM.2008$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2008, summary(test.2007,summary(test.2006)))  #
    spr.est[spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
	test.2009 <- spr(GMlivefreq2008$TOW_NO[GMlivefreq2008$VMSAREA==STRATA.ID],apply(GMlivefreq2008[GMlivefreq2008$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2009$TOW_NO[GMlivefreq2009$VMSAREA==STRATA.ID],apply(GMlivefreq2009[GMlivefreq2009$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2009[crossref.GM.2009$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))) #
    spr.est[spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
	test.2010 <- spr(GMlivefreq2009$TOW_NO[GMlivefreq2009$VMSAREA==STRATA.ID],apply(GMlivefreq2009[GMlivefreq2009$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2010$TOW_NO[GMlivefreq2010$VMSAREA==STRATA.ID],apply(GMlivefreq2010[GMlivefreq2010$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2010[crossref.GM.2010$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))   #
    spr.est[spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
	test.2011 <- spr(GMlivefreq2010$TOW_NO[GMlivefreq2010$VMSAREA==STRATA.ID],apply(GMlivefreq2010[GMlivefreq2010$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2011$TOW_NO[GMlivefreq2011$VMSAREA==STRATA.ID],apply(GMlivefreq2011[GMlivefreq2011$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2011[crossref.GM.2011$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))) #
    spr.est[spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
	test.2012 <- spr(GMlivefreq2011$TOW_NO[GMlivefreq2011$VMSAREA==STRATA.ID],apply(GMlivefreq2011[GMlivefreq2011$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2012$TOW_NO[GMlivefreq2012$VMSAREA==STRATA.ID],apply(GMlivefreq2012[GMlivefreq2012$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2012[crossref.GM.2012$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))   # 46.62
	spr.est[spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
	test.2013 <- spr(GMlivefreq2012$TOW_NO[GMlivefreq2012$VMSAREA==STRATA.ID],apply(GMlivefreq2012[GMlivefreq2012$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2013$TOW_NO[GMlivefreq2013$VMSAREA==STRATA.ID],apply(GMlivefreq2013[GMlivefreq2013$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2013[crossref.GM.2013$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))   #77.3
    spr.est[spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
	test.2014 <- spr(GMlivefreq2013$TOW_NO[GMlivefreq2013$VMSAREA==STRATA.ID],apply(GMlivefreq2013[GMlivefreq2013$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2014$TOW_NO[GMlivefreq2014$VMSAREA==STRATA.ID],apply(GMlivefreq2014[GMlivefreq2014$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2014[crossref.GM.2014$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))   #134.7
	spr.est[spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2014/2015
	test.2015 <- spr(GMlivefreq2014$TOW_NO[GMlivefreq2014$VMSAREA==STRATA.ID],apply(GMlivefreq2014[GMlivefreq2014$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlivefreq2015$TOW_NO[GMlivefreq2015$VMSAREA==STRATA.ID],apply(GMlivefreq2015[GMlivefreq2015$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2015[crossref.GM.2015$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))   #134.7
	spr.est[spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

	#2015/2016
test.2016 <- spr(GMlivefreq2015$TOW_NO[GMlivefreq2015$VMSAREA==STRATA.ID],apply(GMlivefreq2015[GMlivefreq2015$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlivefreq2016$TOW_NO[GMlivefreq2016$VMSAREA==STRATA.ID],apply(GMlivefreq2016[GMlivefreq2016$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2016[crossref.GM.2016$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))   #
	spr.est[spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

	#2016/2017
test.2017 <- spr(GMlivefreq2016$TOW_NO[GMlivefreq2016$VMSAREA==STRATA.ID],apply(GMlivefreq2016[GMlivefreq2016$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlivefreq2017$TOW_NO[GMlivefreq2017$VMSAREA==STRATA.ID],apply(GMlivefreq2017[GMlivefreq2017$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2017[crossref.GM.2017$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))   #
	spr.est[spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
  #2017/2018
test.2018 <- spr(GMlivefreq2017$TOW_NO[GMlivefreq2017$VMSAREA==STRATA.ID],apply(GMlivefreq2017[GMlivefreq2017$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2018$TOW_NO[GMlivefreq2018$VMSAREA==STRATA.ID],apply(GMlivefreq2018[GMlivefreq2018$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2018[crossref.GM.2018$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))   #
spr.est[spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
test.2019 <- spr(GMlivefreq2018$TOW_NO[GMlivefreq2018$VMSAREA==STRATA.ID],apply(GMlivefreq2018[GMlivefreq2018$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2019$TOW_NO[GMlivefreq2019$VMSAREA==STRATA.ID],apply(GMlivefreq2019[GMlivefreq2019$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2019[crossref.GM.2019$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary (test.2019,summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))   #
spr.est[spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(test.2018)

#2019/2021 #No survey in 2020 
test.2021 <- spr(GMlivefreq2019$TOW_NO[GMlivefreq2019$VMSAREA==STRATA.ID],apply(GMlivefreq2019[GMlivefreq2019$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2021$TOW_NO[GMlivefreq2021$VMSAREA==STRATA.ID],apply(GMlivefreq2021[GMlivefreq2021$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2021[crossref.GM.2021$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(test.2021, summary (test.2019,summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))))   #
spr.est[spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(test.2021)

#2021/2022 
test.2022 <- spr(GMlivefreq2021$TOW_NO[GMlivefreq2021$VMSAREA==STRATA.ID],apply(GMlivefreq2021[GMlivefreq2021$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2022$TOW_NO[GMlivefreq2022$VMSAREA==STRATA.ID],apply(GMlivefreq2022[GMlivefreq2022$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2022[crossref.GM.2022$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(test.2022, summary(test.2021, summary (test.2019,summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))))   #
spr.est[spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(test.2022)

#2022/2023 
test.2023 <- spr(GMlivefreq2022$TOW_NO[GMlivefreq2022$VMSAREA==STRATA.ID],apply(GMlivefreq2022[GMlivefreq2022$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2023$TOW_NO[GMlivefreq2023$VMSAREA==STRATA.ID],apply(GMlivefreq2023[GMlivefreq2023$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2023[crossref.GM.2023$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(test.2023, summary(test.2022, summary(test.2021, summary (test.2019,summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))))))   #
spr.est[spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(test.2023)


#2023/2024 
test.2024 <- spr(GMlivefreq2023$TOW_NO[GMlivefreq2023$VMSAREA==STRATA.ID],apply(GMlivefreq2023[GMlivefreq2023$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2024$TOW_NO[GMlivefreq2024$VMSAREA==STRATA.ID],apply(GMlivefreq2024[GMlivefreq2024$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2024[crossref.GM.2024$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(test.2024, summary(test.2023, summary(test.2022, summary(test.2021, summary (test.2019,summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))))))   #
spr.est[spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(test.2024)


#2024/2025 
test.2025 <- spr(GMlivefreq2024$TOW_NO[GMlivefreq2024$VMSAREA==STRATA.ID],apply(GMlivefreq2024[GMlivefreq2024$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2025$TOW_NO[GMlivefreq2025$VMSAREA==STRATA.ID],apply(GMlivefreq2025[GMlivefreq2025$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2025[crossref.GM.2025$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(test.2025,summary(test.2024, summary(test.2023, summary(test.2022, summary(test.2021, summary (test.2019,summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))))))))   #
spr.est[spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(test.2024)

#2025/2026 
test.2026 <- spr(GMlivefreq2025$TOW_NO[GMlivefreq2025$VMSAREA==STRATA.ID],apply(GMlivefreq2025[GMlivefreq2025$VMSAREA==STRATA.ID,24:50],1,sum),
                 GMlivefreq2026$TOW_NO[GMlivefreq2026$VMSAREA==STRATA.ID],apply(GMlivefreq2026[GMlivefreq2026$VMSAREA==STRATA.ID,27:50],1,sum),
                 crossref.GM.2026[crossref.GM.2026$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(test.2026,summary(test.2025,summary(test.2024, summary(test.2023, summary(test.2022, summary(test.2021, summary (test.2019,summary (test.2018,summary (test.2017,summary(test.2016,summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))))))))  #
spr.est[spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(spr.est$Year, spr.est$Yspr, xout=2020) #  189.6707
spr.est[spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(189.6707, 752.08071) #assume var from 2019

spr.est$method <- "spr"
names(spr.est) <- c("Year", "Mean.nums", "var.y", "method")

SPA6.Comm.IN <- rbind(SPA6.Comm.simple[SPA6.Comm.simple$Year<2006,], spr.est)
SPA6.Comm.IN$cv <- sqrt(SPA6.Comm.IN$var.y)/SPA6.Comm.IN$Mean.nums   #USE THIS CV ASSOCIATED WITH THE SPR ESTIMATES FOR MODEL; for Simple estimates - need to do more - see CV section below
SPA6.Comm.IN$VMSAREA <- STRATA.ID



###---- SPA 6  IN VMS Recruit size (>=65mm&<80mm) ----
# bins 65,70,75; Recruits (65-79mm); 2007+ use spr
STRATA.ID <- "IN"

#simple means
years <- 1997:surveyyear
X <- length(years)
SPA6.Recruit.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Recruit.simple$Year)){
	temp.data <- livefreq[livefreq$YEAR==1996+i,]
	SPA6.Recruit.simple[i,2]<- mean(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
	SPA6.Recruit.simple[i,3]<- var(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
}
SPA6.Recruit.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA6.Recruit.simple$Year, SPA6.Recruit.simple$Mean.nums, xout=2020) #  1.503588
SPA6.Recruit.simple[SPA6.Recruit.simple$Year==2020,c("Mean.nums","var.y")] <- c(1.503588, 5.911451) #assume var from 2019


#dataframe for SPR estimates Recruits
spryears <- 2006:surveyyear #update to most recent year
Y <- length(spryears)
spr.est.rec <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2005/2006
    rec.2006 <- spr(GMlivefreq2005$TOW_NO[GMlivefreq2005$VMSAREA==STRATA.ID],apply(GMlivefreq2005[GMlivefreq2005$VMSAREA==STRATA.ID,21:23],1,sum), #21:26 in year t-1 incorporates prerecruits into est that will grow to recruit size in 1 year (from vonB estimates).
    GMlivefreq2006$TOW_NO[GMlivefreq2006$VMSAREA==STRATA.ID],apply(GMlivefreq2006[GMlivefreq2006$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2006[crossref.GM.2006$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary(rec.2006)  #
    spr.est.rec[spr.est.rec$Year==2006,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2006/2007
	rec.2007 <- spr(GMlivefreq2006$TOW_NO[GMlivefreq2006$VMSAREA==STRATA.ID],apply(GMlivefreq2006[GMlivefreq2006$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2007$TOW_NO[GMlivefreq2007$VMSAREA==STRATA.ID],apply(GMlivefreq2007[GMlivefreq2007$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2007[crossref.GM.2007$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2007, summary(rec.2006))  #
    spr.est.rec[spr.est.rec$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
	rec.2008 <- spr(GMlivefreq2007$TOW_NO[GMlivefreq2007$VMSAREA==STRATA.ID],apply(GMlivefreq2007[GMlivefreq2007$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2008$TOW_NO[GMlivefreq2008$VMSAREA==STRATA.ID],apply(GMlivefreq2008[GMlivefreq2008$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2008[crossref.GM.2008$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2008, summary(rec.2007,summary(rec.2006))) #
    spr.est.rec[spr.est.rec$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
	rec.2009 <- spr(GMlivefreq2008$TOW_NO[GMlivefreq2008$VMSAREA==STRATA.ID],apply(GMlivefreq2008[GMlivefreq2008$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2009$TOW_NO[GMlivefreq2009$VMSAREA==STRATA.ID],apply(GMlivefreq2009[GMlivefreq2009$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2009[crossref.GM.2009$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))  #
    spr.est.rec[spr.est.rec$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
	rec.2010 <- spr(GMlivefreq2009$TOW_NO[GMlivefreq2009$VMSAREA==STRATA.ID],apply(GMlivefreq2009[GMlivefreq2009$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2010$TOW_NO[GMlivefreq2010$VMSAREA==STRATA.ID],apply(GMlivefreq2010[GMlivefreq2010$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2010[crossref.GM.2010$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))) #
    spr.est.rec[spr.est.rec$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
	rec.2011 <- spr(GMlivefreq2010$TOW_NO[GMlivefreq2010$VMSAREA==STRATA.ID],apply(GMlivefreq2010[GMlivefreq2010$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2011$TOW_NO[GMlivefreq2011$VMSAREA==STRATA.ID],apply(GMlivefreq2011[GMlivefreq2011$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2011[crossref.GM.2011$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))) #
    spr.est.rec[spr.est.rec$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
	rec.2012 <- spr(GMlivefreq2011$TOW_NO[GMlivefreq2011$VMSAREA==STRATA.ID],apply(GMlivefreq2011[GMlivefreq2011$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2012$TOW_NO[GMlivefreq2012$VMSAREA==STRATA.ID],apply(GMlivefreq2012[GMlivefreq2012$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2012[crossref.GM.2012$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))))) #
    spr.est.rec[spr.est.rec$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
	rec.2013 <- spr(GMlivefreq2012$TOW_NO[GMlivefreq2012$VMSAREA==STRATA.ID],apply(GMlivefreq2012[GMlivefreq2012$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2013$TOW_NO[GMlivefreq2013$VMSAREA==STRATA.ID],apply(GMlivefreq2013[GMlivefreq2013$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2013[crossref.GM.2013$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))))) #
    spr.est.rec[spr.est.rec$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
	rec.2014 <- spr(GMlivefreq2013$TOW_NO[GMlivefreq2013$VMSAREA==STRATA.ID],apply(GMlivefreq2013[GMlivefreq2013$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2014$TOW_NO[GMlivefreq2014$VMSAREA==STRATA.ID],apply(GMlivefreq2014[GMlivefreq2014$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2014[crossref.GM.2014$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))))))) #
    spr.est.rec[spr.est.rec$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
	rec.2015 <- spr(GMlivefreq2014$TOW_NO[GMlivefreq2014$VMSAREA==STRATA.ID],apply(GMlivefreq2014[GMlivefreq2014$VMSAREA==STRATA.ID,21:23],1,sum),
    GMlivefreq2015$TOW_NO[GMlivefreq2015$VMSAREA==STRATA.ID],apply(GMlivefreq2015[GMlivefreq2015$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2015[crossref.GM.2015$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))))))))) #
    spr.est.rec[spr.est.rec$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

 #2015/2016
    rec.2016 <- spr(GMlivefreq2015$TOW_NO[GMlivefreq2015$VMSAREA==STRATA.ID],apply(GMlivefreq2015[GMlivefreq2015$VMSAREA==STRATA.ID,21:23],1,sum),
                    GMlivefreq2016$TOW_NO[GMlivefreq2016$VMSAREA==STRATA.ID],apply(GMlivefreq2016[GMlivefreq2016$VMSAREA==STRATA.ID,24:26],1,sum),
                    crossref.GM.2016[crossref.GM.2016$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))))))))))#
    spr.est.rec[spr.est.rec$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

 #2016/2017
    rec.2017 <- spr(GMlivefreq2016$TOW_NO[GMlivefreq2016$VMSAREA==STRATA.ID],apply(GMlivefreq2016[GMlivefreq2016$VMSAREA==STRATA.ID,21:23],1,sum),
                    GMlivefreq2017$TOW_NO[GMlivefreq2017$VMSAREA==STRATA.ID],apply(GMlivefreq2017[GMlivefreq2017$VMSAREA==STRATA.ID,24:26],1,sum),
                    crossref.GM.2017[crossref.GM.2017$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))))))))))#
    spr.est.rec[spr.est.rec$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
rec.2018 <- spr(GMlivefreq2017$TOW_NO[GMlivefreq2017$VMSAREA==STRATA.ID],apply(GMlivefreq2017[GMlivefreq2017$VMSAREA==STRATA.ID,21:23],1,sum),
                GMlivefreq2018$TOW_NO[GMlivefreq2018$VMSAREA==STRATA.ID],apply(GMlivefreq2018[GMlivefreq2018$VMSAREA==STRATA.ID,24:26],1,sum),
                crossref.GM.2018[crossref.GM.2018$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))))))))))))#
spr.est.rec[spr.est.rec$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
rec.2019 <- spr(GMlivefreq2018$TOW_NO[GMlivefreq2018$VMSAREA==STRATA.ID],apply(GMlivefreq2018[GMlivefreq2018$VMSAREA==STRATA.ID,21:23],1,sum),
                GMlivefreq2019$TOW_NO[GMlivefreq2019$VMSAREA==STRATA.ID],apply(GMlivefreq2019[GMlivefreq2019$VMSAREA==STRATA.ID,24:26],1,sum),
                crossref.GM.2019[crossref.GM.2019$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary (rec.2019,summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))))))))))))#
spr.est.rec[spr.est.rec$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
rec.2021 <- spr(GMlivefreq2019$TOW_NO[GMlivefreq2019$VMSAREA==STRATA.ID],apply(GMlivefreq2019[GMlivefreq2019$VMSAREA==STRATA.ID,21:23],1,sum),
                GMlivefreq2021$TOW_NO[GMlivefreq2021$VMSAREA==STRATA.ID],apply(GMlivefreq2021[GMlivefreq2021$VMSAREA==STRATA.ID,24:26],1,sum),
                crossref.GM.2021[crossref.GM.2021$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(rec.2021, summary (rec.2019,summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))))))))))))) #
spr.est.rec[spr.est.rec$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
rec.2022 <- spr(GMlivefreq2021$TOW_NO[GMlivefreq2021$VMSAREA==STRATA.ID],apply(GMlivefreq2021[GMlivefreq2021$VMSAREA==STRATA.ID,21:23],1,sum),
                GMlivefreq2022$TOW_NO[GMlivefreq2022$VMSAREA==STRATA.ID],apply(GMlivefreq2022[GMlivefreq2022$VMSAREA==STRATA.ID,24:26],1,sum),
                crossref.GM.2022[crossref.GM.2022$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(rec.2022, summary(rec.2021, summary (rec.2019,summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))))))))))))))  #
spr.est.rec[spr.est.rec$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
rec.2023 <- spr(GMlivefreq2022$TOW_NO[GMlivefreq2022$VMSAREA==STRATA.ID],apply(GMlivefreq2022[GMlivefreq2022$VMSAREA==STRATA.ID,21:23],1,sum),
                GMlivefreq2023$TOW_NO[GMlivefreq2023$VMSAREA==STRATA.ID],apply(GMlivefreq2023[GMlivefreq2023$VMSAREA==STRATA.ID,24:26],1,sum),
                crossref.GM.2023[crossref.GM.2023$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(rec.2023, summary(rec.2022, summary(rec.2021, summary (rec.2019,summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))))))))))))))))  #
spr.est.rec[spr.est.rec$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
rec.2024 <- spr(GMlivefreq2023$TOW_NO[GMlivefreq2023$VMSAREA==STRATA.ID],apply(GMlivefreq2023[GMlivefreq2023$VMSAREA==STRATA.ID,21:23],1,sum),
                GMlivefreq2024$TOW_NO[GMlivefreq2024$VMSAREA==STRATA.ID],apply(GMlivefreq2024[GMlivefreq2024$VMSAREA==STRATA.ID,24:26],1,sum),
                crossref.GM.2024[crossref.GM.2024$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(rec.2024,summary(rec.2023, summary(rec.2022, summary(rec.2021, summary (rec.2019,summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006)))))))))))))))))))  #
spr.est.rec[spr.est.rec$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
#rec.2025 <- spr(GMlivefreq2024$TOW_NO[GMlivefreq2024$VMSAREA==STRATA.ID],apply(GMlivefreq2024[GMlivefreq2024$VMSAREA==STRATA.ID,21:23],1,sum),
#                GMlivefreq2025$TOW_NO[GMlivefreq2025$VMSAREA==STRATA.ID],apply(GMlivefreq2025[GMlivefreq2025$VMSAREA==STRATA.ID,24:26],1,sum),
#                crossref.GM.2025[crossref.GM.2025$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
#K <- summary(rec.2025, summary(rec.2024,summary(rec.2023, summary(rec.2022, summary(rec.2021, summary (rec.2019,summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))))))))))))))))))  #
#spr.est.rec[spr.est.rec$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#Need to use simple mean for 2025 rec since mean of matched tows from last year (2024) is zero
# See if tows assocaited with given year were all zero or not 
#test <- GMlivefreq2024 %>% filter(TOW_NO %in% (crossref.GM.2025$TOW_NO_REF[crossref.GM.2025$VMSAREA==STRATA.ID])) 
#apply(test[,21:23],1,sum)

#2025/2026
#rec.2026 <- spr(GMlivefreq2025$TOW_NO[GMlivefreq2025$VMSAREA==STRATA.ID],apply(GMlivefreq2025[GMlivefreq2025$VMSAREA==STRATA.ID,21:23],1,sum),
#                GMlivefreq2026$TOW_NO[GMlivefreq2026$VMSAREA==STRATA.ID],apply(GMlivefreq2026[GMlivefreq2026$VMSAREA==STRATA.ID,24:26],1,sum),
#                crossref.GM.2026[crossref.GM.2026$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
#K <- summary(rec.2026,summary(rec.2024,summary(rec.2023, summary(rec.2022, summary(rec.2021, summary (rec.2019,summary (rec.2018,summary (rec.2017,summary (rec.2016, summary(rec.2015,summary(rec.2014, (summary(rec.2013, summary(rec.2012, summary(rec.2011, summary(rec.2010, summary(rec.2009,summary(rec.2008, summary(rec.2007,summary(rec.2006))))))))))))))))))))  #
spr.est.rec[spr.est.rec$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
# See if tows assocaited with given year were all zero or not 
#test <- GMlivefreq2025 %>% filter(TOW_NO %in% (crossref.GM.2026$TOW_NO_REF[crossref.GM.2026$VMSAREA==STRATA.ID])) 
#apply(test[,21:23],1,sum)

spr.est.rec

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(spr.est.rec$Year, spr.est.rec$Yspr, xout=2020) #  1.449193
spr.est.rec[spr.est.rec$Year==2020,c("Yspr",  "var.Yspr.corrected")] <- c(1.449193, 0.09963137) #assume var from 2019

spr.est.rec$method <- "spr"
names(spr.est.rec) <- c("Year", "Mean.nums", "var.y", "method")

SPA6.Recruit.IN <- rbind(SPA6.Recruit.simple[SPA6.Recruit.simple$Year<2006,], spr.est.rec)
SPA6.Recruit.IN[SPA6.Recruit.IN$Year==2025,] <- SPA6.Recruit.simple[SPA6.Recruit.simple$Year==2025,] 
SPA6.Recruit.IN[SPA6.Recruit.IN$Year==2026,] <- SPA6.Recruit.simple[SPA6.Recruit.simple$Year==2026,] 

SPA6.Recruit.IN$cv <- sqrt(SPA6.Recruit.IN$var.y)/SPA6.Recruit.IN$Mean.nums #USE THIS CV ASSOCIATED WITH THE SPR ESTIMATES FOR MODEL; for Simple estimates - need to do more - see CV section below
SPA6.Recruit.IN$VMSAREA <- STRATA.ID


### ---- Survey Index WEIGHTS - wt/tow CFh ----
### simple mean 1997 to 2006, spr mean from 2007+  ###

### ---- Weight per tow commercial size (>=80mm) ----
# Get simple mean of all years - Commercial size (>80mm) weights per tow
STRATA.ID <- "IN"

years <- 1997:surveyyear
X <- length(years)
SPA6.cfComm.simple <- data.frame(Year=years, Mean.wt=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.cfComm.simple$Year)){
	temp.data <- spa6shw.dat[spa6shw.dat$YEAR==1996+i,]
	SPA6.cfComm.simple[i,2] <- mean(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
	SPA6.cfComm.simple[i,3] <- var(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
}
SPA6.cfComm.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA6.cfComm.simple$Year, SPA6.cfComm.simple$Mean.wt, xout=2020) #  3.017258
SPA6.cfComm.simple[SPA6.cfComm.simple$Year==2020,c("Mean.wt","var.y")] <- c(3.017258, 25.1131021) #assume var from 2019


#dataframe for SPR estimates commercial weights per tow
spryears <- 2006:surveyyear #update to most recent year
Y <- length(spryears)
spr.est.wt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2005/2006  spr
	A6comcf.2006 <- spr(spa6shw2005$TOW_NO[spa6shw2005$VMSAREA==STRATA.ID],apply(spa6shw2005[spa6shw2005$VMSAREA==STRATA.ID,24:50],1,sum),  #recruits included in previous yr
    spa6shw2006$TOW_NO[spa6shw2006$VMSAREA==STRATA.ID],apply(spa6shw2006[spa6shw2006$VMSAREA==STRATA.ID,27:50],1,sum),  #just commercial size for current yr
    crossref.GM.2006[crossref.GM.2006$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2006) #
    spr.est.wt[spr.est.wt$Year==2006,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2006/2007  spr
	A6comcf.2007 <- spr(spa6shw2006$TOW_NO[spa6shw2006$VMSAREA==STRATA.ID],apply(spa6shw2006[spa6shw2006$VMSAREA==STRATA.ID,24:50],1,sum),  #recruits included in previous yr
    spa6shw2007$TOW_NO[spa6shw2007$VMSAREA==STRATA.ID],apply(spa6shw2007[spa6shw2007$VMSAREA==STRATA.ID,27:50],1,sum),  #just commercial size for current yr
    crossref.GM.2007[crossref.GM.2007$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2007,summary (A6comcf.2006)) #
    spr.est.wt[spr.est.wt$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008 spr
	A6comcf.2008 <- spr(spa6shw2007$TOW_NO[spa6shw2007$VMSAREA==STRATA.ID],apply(spa6shw2007[spa6shw2007$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2008$TOW_NO[spa6shw2008$VMSAREA==STRATA.ID],apply(spa6shw2008[spa6shw2008$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2008[crossref.GM.2008$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))) #
    spr.est.wt[spr.est.wt$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009  spr
	A6comcf.2009 <- spr(spa6shw2008$TOW_NO[spa6shw2008$VMSAREA==STRATA.ID],apply(spa6shw2008[spa6shw2008$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2009$TOW_NO[spa6shw2009$VMSAREA==STRATA.ID],apply(spa6shw2009[spa6shw2009$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2009[crossref.GM.2009$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006)))) #
    spr.est.wt[spr.est.wt$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
	A6comcf.2010 <- spr(spa6shw2009$TOW_NO[spa6shw2009$VMSAREA==STRATA.ID],apply(spa6shw2009[spa6shw2009$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2010$TOW_NO[spa6shw2010$VMSAREA==STRATA.ID],apply(spa6shw2010[spa6shw2010$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2010[crossref.GM.2010$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))) #
    spr.est.wt[spr.est.wt$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
	A6comcf.2011 <- spr(spa6shw2010$TOW_NO[spa6shw2010$VMSAREA==STRATA.ID],apply(spa6shw2010[spa6shw2010$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2011$TOW_NO[spa6shw2011$VMSAREA==STRATA.ID],apply(spa6shw2011[spa6shw2011$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2011[crossref.GM.2011$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))  #
    spr.est.wt[spr.est.wt$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
	A6comcf.2012 <- spr(spa6shw2011$TOW_NO[spa6shw2011$VMSAREA==STRATA.ID],apply(spa6shw2011[spa6shw2011$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2012$TOW_NO[spa6shw2012$VMSAREA==STRATA.ID],apply(spa6shw2012[spa6shw2012$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2012[crossref.GM.2012$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2012, summary (A6comcf.2011, summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006)))))))#
    spr.est.wt[spr.est.wt$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
	A6comcf.2013 <- spr(spa6shw2012$TOW_NO[spa6shw2012$VMSAREA==STRATA.ID],apply(spa6shw2012[spa6shw2012$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2013$TOW_NO[spa6shw2013$VMSAREA==STRATA.ID],apply(spa6shw2013[spa6shw2013$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2013[crossref.GM.2013$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))#
    spr.est.wt[spr.est.wt$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
	A6comcf.2014 <- spr(spa6shw2013$TOW_NO[spa6shw2013$VMSAREA==STRATA.ID],apply(spa6shw2013[spa6shw2013$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2014$TOW_NO[spa6shw2014$VMSAREA==STRATA.ID],apply(spa6shw2014[spa6shw2014$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2014[crossref.GM.2014$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006)))))))))#????
    spr.est.wt[spr.est.wt$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
	A6comcf.2015 <- spr(spa6shw2014$TOW_NO[spa6shw2014$VMSAREA==STRATA.ID],apply(spa6shw2014[spa6shw2014$VMSAREA==STRATA.ID,24:50],1,sum),
    spa6shw2015$TOW_NO[spa6shw2015$VMSAREA==STRATA.ID],apply(spa6shw2015[spa6shw2015$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2015[crossref.GM.2015$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))))#????
    spr.est.wt[spr.est.wt$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016 spr
A6comcf.2016 <- spr(spa6shw2015$TOW_NO[spa6shw2015$VMSAREA==STRATA.ID],apply(spa6shw2015[spa6shw2015$VMSAREA==STRATA.ID,24:50],1,sum),
                        spa6shw2016$TOW_NO[spa6shw2016$VMSAREA==STRATA.ID],apply(spa6shw2016[spa6shw2016$VMSAREA==STRATA.ID,27:50],1,sum),
                        crossref.GM.2016[crossref.GM.2016$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006)))))))))))#????
    spr.est.wt[spr.est.wt$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017 spr
A6comcf.2017 <- spr(spa6shw2016$TOW_NO[spa6shw2016$VMSAREA==STRATA.ID],apply(spa6shw2016[spa6shw2016$VMSAREA==STRATA.ID,24:50],1,sum),
                        spa6shw2017$TOW_NO[spa6shw2017$VMSAREA==STRATA.ID],apply(spa6shw2017[spa6shw2017$VMSAREA==STRATA.ID,27:50],1,sum),
                        crossref.GM.2017[crossref.GM.2017$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))))))#????
    spr.est.wt[spr.est.wt$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018 spr
A6comcf.2018 <- spr(spa6shw2017$TOW_NO[spa6shw2017$VMSAREA==STRATA.ID],apply(spa6shw2017[spa6shw2017$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2018$TOW_NO[spa6shw2018$VMSAREA==STRATA.ID],apply(spa6shw2018[spa6shw2018$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2018[crossref.GM.2018$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))))))) #????
spr.est.wt[spr.est.wt$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
 
#2018/2019 spr
A6comcf.2019 <- spr(spa6shw2018$TOW_NO[spa6shw2018$VMSAREA==STRATA.ID],apply(spa6shw2018[spa6shw2018$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2019$TOW_NO[spa6shw2019$VMSAREA==STRATA.ID],apply(spa6shw2019[spa6shw2019$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2019[crossref.GM.2019$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary (A6comcf.2019,summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006)))))))))))))) #????
spr.est.wt[spr.est.wt$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 spr #no survey in 2020 
A6comcf.2021 <- spr(spa6shw2019$TOW_NO[spa6shw2019$VMSAREA==STRATA.ID],apply(spa6shw2019[spa6shw2019$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2021$TOW_NO[spa6shw2021$VMSAREA==STRATA.ID],apply(spa6shw2021[spa6shw2021$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2021[crossref.GM.2021$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary(A6comcf.2021, summary (A6comcf.2019,summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))))))))) 
spr.est.wt[spr.est.wt$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(A6comcf.2019 )

#2021/2022
A6comcf.2022 <- spr(spa6shw2021$TOW_NO[spa6shw2021$VMSAREA==STRATA.ID],apply(spa6shw2021[spa6shw2021$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2022$TOW_NO[spa6shw2022$VMSAREA==STRATA.ID],apply(spa6shw2022[spa6shw2022$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2022[crossref.GM.2022$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary(A6comcf.2022, summary(A6comcf.2021, summary (A6comcf.2019,summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006)))))))))))))))) 
spr.est.wt[spr.est.wt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(A6comcf.2022 )

#2022/2023
A6comcf.2023 <- spr(spa6shw2022$TOW_NO[spa6shw2022$VMSAREA==STRATA.ID],apply(spa6shw2022[spa6shw2022$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2023$TOW_NO[spa6shw2023$VMSAREA==STRATA.ID],apply(spa6shw2023[spa6shw2023$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2023[crossref.GM.2023$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary(A6comcf.2023, summary(A6comcf.2022, summary(A6comcf.2021, summary (A6comcf.2019,summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))))))))))) 
spr.est.wt[spr.est.wt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(A6comcf.2023)

#2023/2024
A6comcf.2024 <- spr(spa6shw2023$TOW_NO[spa6shw2023$VMSAREA==STRATA.ID],apply(spa6shw2023[spa6shw2023$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2024$TOW_NO[spa6shw2024$VMSAREA==STRATA.ID],apply(spa6shw2024[spa6shw2024$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2024[crossref.GM.2024$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary(A6comcf.2024,summary(A6comcf.2023, summary(A6comcf.2022, summary(A6comcf.2021, summary (A6comcf.2019,summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006)))))))))))))))))) 
spr.est.wt[spr.est.wt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(A6comcf.2024)

#2024/2025
A6comcf.2025 <- spr(spa6shw2024$TOW_NO[spa6shw2024$VMSAREA==STRATA.ID],apply(spa6shw2024[spa6shw2024$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2025$TOW_NO[spa6shw2025$VMSAREA==STRATA.ID],apply(spa6shw2025[spa6shw2025$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2025[crossref.GM.2025$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary(A6comcf.2025, summary(A6comcf.2024,summary(A6comcf.2023, summary(A6comcf.2022, summary(A6comcf.2021, summary (A6comcf.2019,summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))))))))))))) 
spr.est.wt[spr.est.wt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(A6comcf.2024)

#2025/2026
A6comcf.2026 <- spr(spa6shw2025$TOW_NO[spa6shw2025$VMSAREA==STRATA.ID],apply(spa6shw2025[spa6shw2025$VMSAREA==STRATA.ID,24:50],1,sum),
                    spa6shw2026$TOW_NO[spa6shw2026$VMSAREA==STRATA.ID],apply(spa6shw2026[spa6shw2026$VMSAREA==STRATA.ID,27:50],1,sum),
                    crossref.GM.2026[crossref.GM.2026$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <-  summary(A6comcf.2026, summary(A6comcf.2025, summary(A6comcf.2024,summary(A6comcf.2023, summary(A6comcf.2022, summary(A6comcf.2021, summary (A6comcf.2019,summary (A6comcf.2018,summary (A6comcf.2017,summary (A6comcf.2016, summary(A6comcf.2015 ,summary(A6comcf.2014, summary (A6comcf.2013, summary (A6comcf.2012, summary (A6comcf.2011,summary (A6comcf.2010, summary (A6comcf.2009, summary (A6comcf.2008, summary (A6comcf.2007,summary (A6comcf.2006))))))))))))))))))))
spr.est.wt[spr.est.wt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(A6comcf.2026)

spr.est.wt

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(spr.est.wt$Year, spr.est.wt$Yspr, xout=2020) #  2.72379
spr.est.wt[spr.est.wt$Year==2020,c("Yspr",  "var.Yspr.corrected")] <- c(2.72379, 0.103858541) #assume var from 2019

spr.est.wt$method <- "spr"
names(spr.est.wt) <- c("Year", "Mean.wt", "var.y", "method")

SPA6.cfComm.IN <- rbind(SPA6.cfComm.simple[SPA6.cfComm.simple$Year<2006,], spr.est.wt)
SPA6.cfComm.IN$cv <- sqrt(SPA6.cfComm.IN$var.y)/SPA6.cfComm.IN$Mean.wt  #USE THIS CV ASSOCIATED WITH THE SPR ESTIMATES FOR MODEL; for Simple estimates - need to do more - see CV section below
SPA6.cfComm.IN$Age <- "Commercial"
SPA6.cfComm.IN$VMSAREA <- STRATA.ID


### ---- Weight per tow recruit size (>=65mm&<80mm) ----
# Recruits (65-79mm); 2006+ use spr
STRATA.ID <- "IN"

# Get simple mean of all years - weights per tow
years <- 1997:surveyyear
X <- length(years)
SPA6.cfRecruit.simple <- data.frame(Year=years, Mean.wt=rep(NA,X),var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.cfRecruit.simple$Year)){
	temp.data <- spa6shw.dat[spa6shw.dat$YEAR==1996+i,]
	SPA6.cfRecruit.simple[i,2] <- mean (apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
	SPA6.cfRecruit.simple[i,3] <- var(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
}

SPA6.cfRecruit.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA6.cfRecruit.simple$Year, SPA6.cfRecruit.simple$Mean.wt, xout=2020) #  0.007608864
SPA6.cfRecruit.simple[SPA6.cfRecruit.simple$Year==2020,c("Mean.wt",  "var.y")] <- c(0.007608864, 0.0001680191) #assume var from 2019



#dataframe for SPR estimates recruit weight per tow
spryears <- 2006:surveyyear #update to most recent year
Y <- length(spryears)
spr.est.rec.wt <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2005/2006  spr
	Areccf.2006 <- spr(spa6shw2005$TOW_NO[spa6shw2005$VMSAREA==STRATA.ID],apply(spa6shw2005[spa6shw2005$VMSAREA==STRATA.ID,21:23],1,sum), #bins 50, 55, 60
    spa6shw2006$TOW_NO[spa6shw2006$VMSAREA==STRATA.ID],apply(spa6shw2006[spa6shw2006$VMSAREA==STRATA.ID,24:26],1,sum), #bins 65, 70, 75
    crossref.GM.2006[crossref.GM.2006$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2006)  #
    spr.est.rec.wt[spr.est.rec.wt$Year==2006,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2006/2007  spr
	Areccf.2007 <- spr(spa6shw2006$TOW_NO[spa6shw2006$VMSAREA==STRATA.ID],apply(spa6shw2006[spa6shw2006$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2007$TOW_NO[spa6shw2007$VMSAREA==STRATA.ID],apply(spa6shw2007[spa6shw2007$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2007[crossref.GM.2007$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2007,summary (Areccf.2006) )  #
    spr.est.rec.wt[spr.est.rec.wt$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008 spr
	Areccf.2008 <- spr(spa6shw2007$TOW_NO[spa6shw2007$VMSAREA==STRATA.ID],apply(spa6shw2007[spa6shw2007$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2008$TOW_NO[spa6shw2008$VMSAREA==STRATA.ID],apply(spa6shw2008[spa6shw2008$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2008[crossref.GM.2008$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))) #
    spr.est.rec.wt[spr.est.rec.wt$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009  spr
	Areccf.2009 <- spr(spa6shw2008$TOW_NO[spa6shw2008$VMSAREA==STRATA.ID],apply(spa6shw2008[spa6shw2008$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2009$TOW_NO[spa6shw2009$VMSAREA==STRATA.ID],apply(spa6shw2009[spa6shw2009$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2009[crossref.GM.2009$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))  #
	spr.est.rec.wt[spr.est.rec.wt$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010 spr
	Areccf.2010 <- spr(spa6shw2009$TOW_NO[spa6shw2009$VMSAREA==STRATA.ID],apply(spa6shw2009[spa6shw2009$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2010$TOW_NO[spa6shw2010$VMSAREA==STRATA.ID],apply(spa6shw2010[spa6shw2010$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2010[crossref.GM.2010$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))) #
    spr.est.rec.wt[spr.est.rec.wt$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011 spr
	Areccf.2011 <- spr(spa6shw2010$TOW_NO[spa6shw2010$VMSAREA==STRATA.ID],apply(spa6shw2010[spa6shw2010$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2011$TOW_NO[spa6shw2011$VMSAREA==STRATA.ID],apply(spa6shw2011[spa6shw2011$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2011[crossref.GM.2011$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))) #
    spr.est.rec.wt[spr.est.rec.wt$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012 spr
	Areccf.2012 <- spr(spa6shw2011$TOW_NO[spa6shw2011$VMSAREA==STRATA.ID],apply(spa6shw2011[spa6shw2011$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2012$TOW_NO[spa6shw2012$VMSAREA==STRATA.ID],apply(spa6shw2012[spa6shw2012$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2012[crossref.GM.2012$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))))) #
    spr.est.rec.wt[spr.est.rec.wt$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013 spr
	Areccf.2013 <- spr(spa6shw2012$TOW_NO[spa6shw2012$VMSAREA==STRATA.ID],apply(spa6shw2012[spa6shw2012$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2013$TOW_NO[spa6shw2013$VMSAREA==STRATA.ID],apply(spa6shw2013[spa6shw2013$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2013[crossref.GM.2013$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))) #
    spr.est.rec.wt[spr.est.rec.wt$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014 spr
	Areccf.2014 <- spr(spa6shw2013$TOW_NO[spa6shw2013$VMSAREA==STRATA.ID],apply(spa6shw2013[spa6shw2013$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2014$TOW_NO[spa6shw2014$VMSAREA==STRATA.ID],apply(spa6shw2014[spa6shw2014$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2014[crossref.GM.2014$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))))))) #????
    spr.est.rec.wt[spr.est.rec.wt$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015 spr
	Areccf.2015 <- spr(spa6shw2014$TOW_NO[spa6shw2014$VMSAREA==STRATA.ID],apply(spa6shw2014[spa6shw2014$VMSAREA==STRATA.ID,21:23],1,sum),
    spa6shw2015$TOW_NO[spa6shw2015$VMSAREA==STRATA.ID],apply(spa6shw2015[spa6shw2015$VMSAREA==STRATA.ID,24:26],1,sum),
    crossref.GM.2015[crossref.GM.2015$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))))) #????
    spr.est.rec.wt[spr.est.rec.wt$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

    #2015/2016 spr
Areccf.2016 <- spr(spa6shw2015$TOW_NO[spa6shw2015$VMSAREA==STRATA.ID],apply(spa6shw2015[spa6shw2015$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2016$TOW_NO[spa6shw2016$VMSAREA==STRATA.ID],apply(spa6shw2016[spa6shw2016$VMSAREA==STRATA.ID,24:26],1,sum),
                       crossref.GM.2016[crossref.GM.2016$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))))))))) #????
    spr.est.rec.wt[spr.est.rec.wt$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

	  #2016/2017 spr
Areccf.2017 <- spr(spa6shw2016$TOW_NO[spa6shw2016$VMSAREA==STRATA.ID],apply(spa6shw2016[spa6shw2016$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2017$TOW_NO[spa6shw2017$VMSAREA==STRATA.ID],apply(spa6shw2017[spa6shw2017$VMSAREA==STRATA.ID,24:26],1,sum),
                       crossref.GM.2017[crossref.GM.2017$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))))))) #????
    spr.est.rec.wt[spr.est.rec.wt$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

   #2017/2018 spr
Areccf.2018 <- spr(spa6shw2017$TOW_NO[spa6shw2017$VMSAREA==STRATA.ID],apply(spa6shw2017[spa6shw2017$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2018$TOW_NO[spa6shw2018$VMSAREA==STRATA.ID],apply(spa6shw2018[spa6shw2018$VMSAREA==STRATA.ID,24:26],1,sum),
                   crossref.GM.2018[crossref.GM.2018$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))))))))))) #????
spr.est.rec.wt[spr.est.rec.wt$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019 spr
Areccf.2019 <- spr(spa6shw2018$TOW_NO[spa6shw2018$VMSAREA==STRATA.ID],apply(spa6shw2018[spa6shw2018$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2019$TOW_NO[spa6shw2019$VMSAREA==STRATA.ID],apply(spa6shw2019[spa6shw2019$VMSAREA==STRATA.ID,24:26],1,sum),
                   crossref.GM.2019[crossref.GM.2019$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary (Areccf.2019,summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))))))))) #????
spr.est.rec.wt[spr.est.rec.wt$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2019/2021 spr  No survey in 2020 
Areccf.2021 <- spr(spa6shw2019$TOW_NO[spa6shw2019$VMSAREA==STRATA.ID],apply(spa6shw2019[spa6shw2019$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2021$TOW_NO[spa6shw2021$VMSAREA==STRATA.ID],apply(spa6shw2021[spa6shw2021$VMSAREA==STRATA.ID,24:26],1,sum),
                   crossref.GM.2021[crossref.GM.2021$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(Areccf.2021 , summary (Areccf.2019,summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))))))))))
spr.est.rec.wt[spr.est.rec.wt$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(Areccf.2019)

#2021/2022 spr  
Areccf.2022 <- spr(spa6shw2021$TOW_NO[spa6shw2021$VMSAREA==STRATA.ID],apply(spa6shw2021[spa6shw2021$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2022$TOW_NO[spa6shw2022$VMSAREA==STRATA.ID],apply(spa6shw2022[spa6shw2022$VMSAREA==STRATA.ID,24:26],1,sum),
                   crossref.GM.2022[crossref.GM.2022$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(Areccf.2022 , summary(Areccf.2021 , summary (Areccf.2019,summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))))))))))))))
spr.est.rec.wt[spr.est.rec.wt$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(Areccf.2022)

#2022/2023 spr  
Areccf.2023 <- spr(spa6shw2022$TOW_NO[spa6shw2022$VMSAREA==STRATA.ID],apply(spa6shw2022[spa6shw2022$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2023$TOW_NO[spa6shw2023$VMSAREA==STRATA.ID],apply(spa6shw2023[spa6shw2023$VMSAREA==STRATA.ID,24:26],1,sum),
                   crossref.GM.2023[crossref.GM.2023$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(Areccf.2023 ,summary(Areccf.2022 , summary(Areccf.2021 , summary (Areccf.2019,summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))))))))))))
spr.est.rec.wt[spr.est.rec.wt$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(Areccf.2022)

#2023/2024 spr  
Areccf.2024 <- spr(spa6shw2023$TOW_NO[spa6shw2023$VMSAREA==STRATA.ID],apply(spa6shw2023[spa6shw2023$VMSAREA==STRATA.ID,21:23],1,sum),
                   spa6shw2024$TOW_NO[spa6shw2024$VMSAREA==STRATA.ID],apply(spa6shw2024[spa6shw2024$VMSAREA==STRATA.ID,24:26],1,sum),
                   crossref.GM.2024[crossref.GM.2024$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
K <- summary(Areccf.2024,summary(Areccf.2023, summary(Areccf.2022 , summary(Areccf.2021 , summary (Areccf.2019,summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006))))))))))))))))))
spr.est.rec.wt[spr.est.rec.wt$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#plot(Areccf.2024)

#2024/2025 spr  
#Areccf.2025 <- spr(spa6shw2024$TOW_NO[spa6shw2024$VMSAREA==STRATA.ID],apply(spa6shw2024[spa6shw2024$VMSAREA==STRATA.ID,21:23],1,sum),
#                   spa6shw2025$TOW_NO[spa6shw2025$VMSAREA==STRATA.ID],apply(spa6shw2025[spa6shw2025$VMSAREA==STRATA.ID,24:26],1,sum),
#                   crossref.GM.2025[crossref.GM.2025$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
#K <- summary(Areccf.2025,summary(Areccf.2024,summary(Areccf.2023, summary(Areccf.2022 , summary(Areccf.2021 , summary (Areccf.2019,summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))))))))))))))
#spr.est.rec.wt[spr.est.rec.wt$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
## Zeros for all repeat tows in 2024, need to use simple mean 

#plot(Areccf.2024)

#2025/2026 spr  
#Areccf.2026 <- spr(spa6shw2025$TOW_NO[spa6shw2025$VMSAREA==STRATA.ID],apply(spa6shw2025[spa6shw2025$VMSAREA==STRATA.ID,21:23],1,sum),
#                   spa6shw2026$TOW_NO[spa6shw2026$VMSAREA==STRATA.ID],apply(spa6shw2026[spa6shw2026$VMSAREA==STRATA.ID,24:26],1,sum),
#                   crossref.GM.2026[crossref.GM.2026$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
#K <- summary(Areccf.2026,summary(Areccf.2024,summary(Areccf.2023, summary(Areccf.2022 , summary(Areccf.2021 , summary (Areccf.2019,summary (Areccf.2018,summary (Areccf.2017, summary (Areccf.2016, summary(Areccf.2015, summary(Areccf.2014 , summary (Areccf.2013, summary (Areccf.2012, summary(Areccf.2011, summary (Areccf#.2010, summary (Areccf.2009, summary (Areccf.2008, summary (Areccf.2007,summary (Areccf.2006)))))))))))))))))))
#spr.est.rec.wt[spr.est.rec.wt$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
## Zeros for all repeat tows in 2024, need to use simple mean 

#plot(Areccf.2026)


spr.est.rec.wt

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(spr.est.rec.wt$Year, spr.est.rec.wt$Yspr, xout=2020) #  0.009323864
spr.est.rec.wt[spr.est.rec.wt$Year==2020,c("Yspr",  "var.Yspr.corrected")] <- c(0.009323864, 2.808730e-06) #assume var from 2019

spr.est.rec.wt$method <- "spr"
names(spr.est.rec.wt) <- c("Year", "Mean.wt", "var.y", "method")

SPA6.cfRecruit.IN <- rbind(SPA6.cfRecruit.simple[SPA6.cfRecruit.simple$Year<2006,], spr.est.rec.wt)
SPA6.cfRecruit.IN[SPA6.cfRecruit.IN$Year==2025,] <- SPA6.cfRecruit.simple[SPA6.cfRecruit.simple$Year==2025,] 
SPA6.cfRecruit.IN[SPA6.cfRecruit.IN$Year==2026,] <- SPA6.cfRecruit.simple[SPA6.cfRecruit.simple$Year==2026,] 

SPA6.cfRecruit.IN$cv <- sqrt(SPA6.cfRecruit.IN$var.y)/SPA6.cfRecruit.IN$Mean.wt  #USE THIS CV ASSOCIATED WITH THE SPR ESTIMATES FOR MODEL; for Simple estimates - need to do more - see CV section below
SPA6.cfRecruit.IN$Age <- "Recruit"
SPA6.cfRecruit.IN$VMSAREA <- STRATA.ID



# ---- Calculate CV for model for Commercial Biomass Index and Recruit Biomass Index ----
#            I.cv and R.cv
#    Note 'CV' is actually relative error (SE/mean)
# cvs from SPR estimates calculated above are proper 'CVs' for model input, but cvs calculated above for simple estimates are only standard errors. Need to  calculate as sqrt(var)/sqrt(n))/mean to get equivalent 'CVs' to model and to compare to spr CVs.

biomass.n <- spa6shw.dat %>% filter(VMSAREA==STRATA.ID) %>% group_by(Year = YEAR) %>%  summarise(ntows = n())
biomass.n <- as.data.frame(biomass.n)
#for 2020 when no survey, assume same number of tows as in 2019 
biomass.n <- rbind(biomass.n, data.frame(Year=as.numeric(2020), ntows=74))
biomass.n <- arrange(biomass.n, Year) #reorder 
biomass.n


#merge ntows to estimates so can calculate CVs for simple estimators
SPA6.cfComm.IN <- merge(SPA6.cfComm.IN, biomass.n, by=c('Year'), all.x=TRUE)
SPA6.cfRecruit.IN <- merge(SPA6.cfRecruit.IN, biomass.n, by=c('Year'), all.x=TRUE)

SPA6.Comm.IN <- merge(SPA6.Comm.IN, biomass.n, by=c('Year'), all.x=TRUE)
SPA6.Recruit.IN <- merge(SPA6.Recruit.IN, biomass.n, by=c('Year'), all.x=TRUE)


#commercial Biomass
SPA6.cfComm.IN$Icv[SPA6.cfComm.IN$method=='simple'] <- (sqrt(SPA6.cfComm.IN$var.y[SPA6.cfComm.IN$method=='simple'])/sqrt(SPA6.cfComm.IN$ntows[SPA6.cfComm.IN$method=='simple']))/SPA6.cfComm.IN$Mean.wt[SPA6.cfComm.IN$method=='simple']   #calculate CV for simple estimates
SPA6.cfComm.IN$Icv[SPA6.cfComm.IN$method=='spr'] <- SPA6.cfComm.IN$cv[SPA6.cfComm.IN$method=='spr'] #cv calculated for spr above is correct for spr estimates - assign to Icv

#recruit Biomass
SPA6.cfRecruit.IN$Rcv[SPA6.cfRecruit.IN$method=='simple'] <- (sqrt(SPA6.cfRecruit.IN$var.y[SPA6.cfRecruit.IN$method=='simple'])/sqrt(SPA6.cfRecruit.IN$ntows[SPA6.cfRecruit.IN$method=='simple']))/SPA6.cfRecruit.IN$Mean.wt[SPA6.cfRecruit.IN$method=='simple']   #calculate CV for simple estimates
SPA6.cfRecruit.IN$Rcv[SPA6.cfRecruit.IN$method=='spr'] <- SPA6.cfRecruit.IN$cv[SPA6.cfRecruit.IN$method=='spr'] #cv calculated for spr above is correct for spr estimates - assign to Rcv

#commercial Numbers
SPA6.Comm.IN$Icv[SPA6.Comm.IN$method=='simple'] <- (sqrt(SPA6.Comm.IN$var.y[SPA6.Comm.IN$method=='simple'])/sqrt(SPA6.Comm.IN$ntows[SPA6.Comm.IN$method=='simple']))/SPA6.Comm.IN$Mean.nums[SPA6.Comm.IN$method=='simple']   #calculate CV for simple estimates
SPA6.Comm.IN$Icv[SPA6.Comm.IN$method=='spr'] <- SPA6.Comm.IN$cv[SPA6.Comm.IN$method=='spr'] #cv calculated for spr above is correct for spr estimates - assign to Icv

#recruit Numbers
SPA6.Recruit.IN$Rcv[SPA6.Recruit.IN$method=='simple'] <- (sqrt(SPA6.Recruit.IN$var.y[SPA6.Recruit.IN$method=='simple'])/sqrt(SPA6.Recruit.IN$ntows[SPA6.Recruit.IN$method=='simple']))/SPA6.Recruit.IN$Mean.nums[SPA6.Recruit.IN$method=='simple']   #calculate CV for simple estimates
SPA6.Recruit.IN$Rcv[SPA6.Recruit.IN$method=='spr'] <- SPA6.Recruit.IN$cv[SPA6.Recruit.IN$method=='spr'] #cv calculated for spr above is correct for spr estimates - assign to Rcv


## ---- Population N ----
##Bump Numbers per tow to Population total (N) in modelled (IN) area of SPA 6 
#NOTE: in Inside VMS area of SPA 6, area is 623.94 sq km, standard tow is 0.0042672 sq km; therefore towable units = 146217.6603
SPA6.Comm.IN$Age <-  "Commercial"
SPA6.Recruit.IN$Age <- "Recruit"
SPA6.Comm.IN$Pop <-  (SPA6.Comm.IN$Mean.nums*146217.6603)
SPA6.Recruit.IN$Pop <- (SPA6.Recruit.IN$Mean.nums*146217.6603)

SPA6.Comm.IN.forexport <- SPA6.Comm.IN %>% select(Year,Mean.nums, var.y, method, Age, VMSAREA, ntows, cv = Icv, Pop)
SPA6.Recruit.IN.forexport <- SPA6.Recruit.IN %>% select(Year,Mean.nums, var.y, method, Age, VMSAREA, ntows, cv = Rcv, Pop)

SPA6.Numbers.IN <- rbind(SPA6.Comm.IN.forexport, SPA6.Recruit.IN.forexport)

write.csv(SPA6.Numbers.IN, paste0(path.directory, assessmentyear,"/Assessment/Data/SurveyIndices/SPA",area,"/SPA6.Index.Numbers.IN.",surveyyear,".csv"))


#---- Population Biomass (I and IR) ---- 
#Bump weight per tow to biomass total in modelled (IN) area of SPA 6 
#NOTE: in Inside VMS area of SPA 6, area is 623.94 sq km, standard tow is 0.0042672 sq km; therefore towable units = 146217.6603
SPA6.cfComm.IN$Bmass <-  (SPA6.cfComm.IN$Mean.wt*146217.6603)/1000
SPA6.cfRecruit.IN$Bmass <- (SPA6.cfRecruit.IN$Mean.wt*146217.6603)/1000

SPA6.cfComm.IN.forexport <- SPA6.cfComm.IN %>% select(Year,Mean.wt, var.y, method, Age, VMSAREA, ntows, cv = Icv, Bmass)
SPA6.cfRecruit.IN.forexport <- SPA6.cfRecruit.IN %>% select(Year,Mean.wt, var.y, method, Age, VMSAREA, ntows, cv = Rcv, Bmass)

SPA6.weights.IN <- rbind(SPA6.cfComm.IN.forexport, SPA6.cfRecruit.IN.forexport)

write.csv(SPA6.weights.IN,paste0(path.directory, assessmentyear,"/Assessment/Data/SurveyIndices/SPA",area,"/SPA6.Index.Weight.IN.",surveyyear,".csv"))	


###
### ----  Plots ----
###

data.number <- SPA6.Numbers.IN
data.number$Size <- data.number$Age
data.number$Mean.nums[data.number$Year==2020] <- NA
  
data.weight <- SPA6.weights.IN
data.weight$Size <- data.weight$Age
data.weight$Mean.wt[data.weight$Year==2020] <- NA


number.per.tow <- ggplot(data = data.number, aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
number.per.tow

weight.per.tow <- ggplot(data = data.weight, aes(x=Year, y=Mean.wt , col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Mean kg/tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))
weight.per.tow

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA",area,"InVMS_NumberWeightpertow",surveyyear,".png"),width=8,height=11,units = "in",res=920)
plot_grid(number.per.tow, weight.per.tow, 
          ncol = 1, nrow = 2)
dev.off()

### END OF SCRIPT ###



### EXTRA 
###
### ---- PLOT REPEATED TOWS ----
###

#subset for years
#GMlivefreq2018 (from above)
#GMlivefreq2019(from above)



crossref.GM.2019[ crossref.GM.2019$VMSAREA=='IN',]
crossref.GM.2019$TOW_NO_REF[ crossref.GM.2019$VMSAREA=='IN'] #before year 
crossref.GM.2019$TOW_NO[ crossref.GM.2019$VMSAREA=='IN'] #after year 


#Inside VMS
before<-crossref.GM.2019$TOW_NO_REF[ crossref.GM.2019$VMSAREA=='IN'] #before year #2017; update (tow_no_ref)
after<-crossref.GM.2019$TOW_NO[ crossref.GM.2019$VMSAREA=='IN']  #2018; update

data.before<-GMlivefreq2018 #update
data.after<-GMlivefreq2019 #update

In.Before<-subset (data.before, TOW_NO%in%c(before))
In.After<-subset (data.after, TOW_NO%in%c(after))

In.beforemeans<-sapply(split(In.Before[c(11:50)], In.Before$YEAR), function(x){apply(x,2,mean)})
In.aftermeans<-sapply(split(In.After[c(11:50)], In.After$YEAR), function(x){apply(x,2,mean)})

###plot

y.lim <-c(0,60)
data.ref<-In.beforemeans
data.year<-In.aftermeans
year=2017 #before year 
tows= dim(In.After)[1] #18 

windows()
par(mfrow=c(2, 1), mar = c(2,2,0,0), omi =  c(0.75, 0.75, 0.1, 0.1), plt=c(0.1,1,0.1,1))

barplot(data.ref[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3, paste (year))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

a<-barplot(data.year[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3,paste (year+1))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

axis(side=1,at=a[seq(1,35,by=2)],labels=seq(2.5,172.5,by=10),outer=F)
mtext(side=1,line=2.5,"Shell height (mm)",outer=TRUE)
mtext("Survey mean no./tow", 2, -1, outer = T)

### ............................ ###
### Comparisons of SPR estimates ###
### ............................ ###

# commerical size
windows()
par(mfrow=c(4,3))
plot(test.2006, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2007, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2008, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2009, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2010, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2011, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2012, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2013, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2014, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2015, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2016, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2017, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2018, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2019, ylim=c(0,2500),xlim=c(0,2500))
plot(test.2021, ylim=c(0,2500),xlim=c(0,2500))



# ONCE IN and OUT VMS Estimates are BOTH run - can run this combined plot: 

#Plot all IN VMS and OUT VMS indicies on one plot
setwd("Y:/INSHORE SCALLOP/BoF/")
maxyear #check that it's the right year
com.OUT <- read.csv(paste0('dataoutput/SPA6.Commercial.OUT.',maxyear,'.csv'))
rec.OUT <- read.csv(paste0('dataoutput/SPA6.Recruit.OUT.',maxyear,'.csv'))
comcf.OUT <- read.csv(paste0('dataoutput/SPA6.cfComm.OUT.',maxyear,'.csv'))
reccf.OUT <- read.csv(paste0('dataoutput/SPA6.cfRecruit.OUT.',maxyear,'.csv'))

windows()
text1 <- c("Commercial Inside", "Recruits Inside", "Commercial Outside", "Recruits Outside")
x <- c(1997,maxyear) #update to most recent year
y1 <- c(0,300)
y2 <- c(0,6)
#panel mean number per tow
par(mfrow=c(2,1), mar = c(0,4,1,1), omi =  c(0.75, 0.75, 0.1, 0.1))
plot (x,y1, type="n",xlab="",xaxt="n", ylab= "Survey Index (mean no./tow)")
lines(SPA6.Comm[,c("Year","Mean.nums")], type="b", pch=1, lty=1)
lines(SPA6.Recruit[,c("Year","Mean.nums")], type="b", pch=3, lty=3)
lines(com.OUT[,c("Year","Mean.nums")], type="b", pch=2, lty=2)
lines(rec.OUT[,c("Year","Mean.nums")], type="b", pch=4, lty=4)
legend (1997,300, legend=text1, pch=c(1,3,2,4), bty="n", lty=c(1,3,2,4))

#panel mean kg per tow
text2 <- c("Commercial Inside", "Recruits Inside", "Commercial Outside", "Recruits Outside")
plot (x,y2, type="n",xlab="Year", ylab= "Survey Index (mean kg/tow)")
lines(SPA6.cfComm[,c("Year","Mean.wt")], type="b", pch=1, lty=1)
lines(SPA6.cfRecruit[,c("Year","Mean.wt")], type="b", pch=3, lty=3)
lines(comcf.OUT[,c("Year","Mean.wt")], type="b", pch=2, lty=2)
lines(reccf.OUT [,c("Year","Mean.wt")], type="b", pch=4, lty=4)
legend (1997,6, legend=text2, pch=c(1,3,2,4), bty="n", lty=c(1,3,2,4))













