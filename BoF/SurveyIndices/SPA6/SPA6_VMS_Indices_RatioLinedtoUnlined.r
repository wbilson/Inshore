###.............................###
###     Ratio Lined to Unlined  ###
###  Commercial size no./tow
###                             ###
###    Revamped July 2021      ###
###     J.Sameoto              ###
###.............................###

#NOTES:
#2005 data in BF cruise data
#6A - SPR design started in 6A officially in 2007 (CSAS Res Doc: 2008/002)
# Tows from 2006 SPA 6B SPR design were reassigned to 'new' strata areas including 6A during the 2008 SCALLSUR restratification and cleanup. Some are now in 6A but are not used for the mean estimates.
	
library(ROracle)
library(rgeos)
library(PBSmapping)
library(lubridate)
library(spr)
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


### GET CROSS REFERENCE TABLE FOR SPR ###
#Set SQL 
quer1 <- "SELECT * FROM scallsur.screpeatedtows"

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle
crossref <- dbGetQuery(chan, quer1)

#SHF data
#SPA 6C(30), 6B(31), 6A(32)
# NOTE: 2005 data in BF cruise data;

### LINED DATA ### 
quer2 <- "SELECT * 			                
	        FROM scallsur.sclinedlive_std s			
	        LEFT JOIN                         
	 	      (SELECT tow_date, cruise, tow_no     		
          FROM SCALLSUR.sctows) t             
	        on (s.cruise = t.cruise and s.tow_no = t.tow_no)
	        where strata_id in (30, 31, 32)"

lined <- dbGetQuery(chan, quer2)

lined <- lined[,1:51]
lined$YEAR <- year(lined$TOW_DATE) #add year 

lined$lat <- convert.dd.dddd(lined$START_LAT)
lined$lon <- convert.dd.dddd(lined$START_LONG)
lined$ID <- 1:nrow(lined)

### UNLINED DATA ###
quer3 <- "SELECT * 			                
          FROM scallsur.scunlinedlive_std s			
          LEFT JOIN                         
   	      (SELECT tow_date, cruise, tow_no    
  	      FROM SCALLSUR.sctows) t             
          on (s.cruise = t.cruise and s.tow_no = t.tow_no)
          where strata_id in (30, 31, 32)"

unlined <- dbGetQuery(chan, quer3)
dbDisconnect(chan)

unlined <- unlined[,1:51]
unlined$YEAR <- year(unlined$TOW_DATE) # add year column 

unlined$lat <- convert.dd.dddd(unlined$START_LAT)
unlined$lon <- convert.dd.dddd(unlined$START_LONG)
unlined$ID <- 1:nrow(unlined)


### Identify tows inside (IN) or outside (OUT) the VMS strata ###
# LINED 
lined$VMSSTRATA <- ""
events <- subset(lined,STRATA_ID%in%30:32,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
lined$VMSSTRATA[lined$ID%in%findPolys(events,inVMS)$EID] <- "IN"
lined$VMSSTRATA[lined$ID%in%findPolys(events,outvms)$EID] <- "OUT"

# UNLINED 
unlined$VMSSTRATA <- ""
events <- subset(unlined,STRATA_ID%in%30:32,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
unlined$VMSSTRATA[unlined$ID%in%findPolys(events,inVMS)$EID] <- "IN"
unlined$VMSSTRATA[unlined$ID%in%findPolys(events,outvms)$EID] <- "OUT"


### Clean crossref table - remove all cross ref tows where tow type ID in year t-1 was 3  ###
type3tows <- merge(crossref[,c('CRUISE_REF','TOW_NO_REF')], lined[,c('CRUISE','TOW_NO','TOW_TYPE_ID','VMSSTRATA')], by.x=c('CRUISE_REF','TOW_NO_REF'), by.y=c('CRUISE','TOW_NO'))
type3tows <- type3tows[type3tows$TOW_TYPE_ID==3,]
type3tows$ID <- paste(type3tows$CRUISE_REF, type3tows$TOW_NO_REF, sep='.')

crossref$ID <- paste(crossref$CRUISE_REF, crossref$TOW_NO_REF, sep='.')
crossref <- crossref[!crossref$ID %in% type3tows$ID,]


# Clean crossref table - remove all cross ref tows where tow type ID in year t-1 was 3
#type3tows <- merge(crossref[,c('CRUISE_REF','TOW_NO_REF')], unlined[,c('CRUISE','TOW_NO','TOW_TYPE_ID','VMSSTRATA')], by.x=c('CRUISE_REF','TOW_NO_REF'), by.y=c('CRUISE','TOW_NO'))
#type3tows <- type3tows[type3tows$TOW_TYPE_ID==3,]
#type3tows$ID <- paste(type3tows$CRUISE_REF, type3tows$TOW_NO_REF, sep='.')
#crossref$ID <- paste(crossref$CRUISE_REF, crossref$TOW_NO_REF, sep='.')
#crossref <- crossref[!crossref$ID %in% type3tows$ID,]


#merge STRATA_ID from lined to the crosssref files based on parent/reference tow
lined$ID <- paste(lined$CRUISE, lined$TOW_NO, sep=".")
lined <- lined[lined$TOW_TYPE_I==1|lined$TOW_TYPE_I==5,] #limit tows to tow type 1 and 5. Requires that tow type 3 repeated tows be removed from crossref table (see above)
crossref <- merge (crossref, subset(lined, select=c("VMSSTRATA", "ID")), by=c("ID"), all=FALSE)

#merge STRATA_ID from unlined to the crosssref files based on parent/reference tow
unlined$ID <- paste(unlined$CRUISE, unlined$TOW_NO, sep=".")
#crossref <-merge (crossref, subset(unlined, select=c("VMSSTRATA", "ID")), by=c("ID"), all=FALSE)
unlined <- unlined[unlined$TOW_TYPE_I==1|unlined$TOW_TYPE_I==5,] #limit tows to tow type 1 and 5. Requires that tow type 3 repeated tows be removed from crossref table (see above)

# Change name of VMSSTRATA to VMSAREA in crossref, lined, unlined  - so will match code from previous year
names(lined)[grep("VMSSTRATA", names(lined))] <- 'VMSAREA' #col 56
names(unlined)[grep("VMSSTRATA", names(unlined))] <- 'VMSAREA' #col 56
names(crossref)[grep("VMSSTRATA", names(crossref))] <- 'VMSAREA' # col 7 

###
# ---- correct data-sets for errors ----
###
#some repeated tows will not be assigned to the same strata as the "parent" tow due to where the START LAT and LONG are
#repeated tows should be corrected to match parent tow
#some experimental tows were used as parent tows for repeats, these should be removed

## Crossreference errors
crossref$VMSAREA[crossref$TOW_NO==55 & crossref$CRUISE=='GM2013'] <- "IN"	
crossref$VMSAREA[crossref$TOW_NO==20 & crossref$CRUISE == "GM2015"] <- "IN"
crossref$VMSAREA[crossref$TOW_NO==56 & crossref$CRUISE == "GM2017"] <- "OUT"
crossref$VMSAREA[crossref$TOW_NO==95 & crossref$CRUISE == "GM2017"] <- "IN"
crossref$VMSAREA[crossref$TOW_NO==40 & crossref$CRUISE == "GM2018"] <- "OUT"
crossref$VMSAREA[crossref$TOW_NO==95 & crossref$CRUISE == "GM2019"] <- "OUT"


## lined livefreq errors 
#1. Tow 32 in 2011 (73 in 2010) not assinged to IN
#   Tow 70 in 2011 (90 in 2010) not assinged to IN
lined$VMSAREA[lined$TOW_NO==32 & lined$CRUISE == "GM2011"] <- "IN"
lined$VMSAREA[lined$TOW_NO==70 & lined$CRUISE == "GM2011"] <-"IN"

#2. In 2012 tow 9, 27, 111 not assinged to IN
lined$VMSAREA[lined$TOW_NO==9 & lined$CRUISE == "GM2012"] <- "IN"
lined$VMSAREA[lined$TOW_NO==27 & lined$CRUISE == "GM2012"] <-"IN"
lined$VMSAREA[lined$TOW_NO==111 & lined$CRUISE == "GM2012"] <-"IN"

#3. In 2013 tow 9, 27, 111 not assinged to IN
lined$VMSAREA[lined$TOW_NO==55 & lined$CRUISE == "GM2013"] <- "IN"
lined$VMSAREA[lined$TOW_NO==109 & lined$CRUISE == "GM2013"] <-"IN"
lined$VMSAREA[lined$TOW_NO==116 & lined$CRUISE == "GM2013"] <-"IN"

#4. In 2014 tow 9, 27, 111 not assinged to IN
lined$VMSAREA[lined$TOW_NO==31 & lined$CRUISE == "GM2014"] <- "IN"
lined$VMSAREA[lined$TOW_NO==42 & lined$CRUISE == "GM2014"] <-"OUT"

#5. In 2015 tow 20 not assigned to IN ( 2015 tow 20 links to 2014 tow 31 which links to a 2013 tow )
lined$VMSAREA[lined$TOW_NO==20 & lined$CRUISE == "GM2015"] <-"IN"
lined$VMSAREA[lined$TOW_NO==25 & lined$CRUISE == "GM2015"] <-"OUT"

#6. In 2016 tow 15,75,92 not assinged to OUT
lined$VMSAREA[lined$TOW_NO==15 & lined$CRUISE == "GM2016"] <-"OUT"
lined$VMSAREA[lined$TOW_NO==75 & lined$CRUISE == "GM2016"] <-"OUT"
lined$VMSAREA[lined$TOW_NO==92 & lined$CRUISE == "GM2016"] <-"OUT"

#7. In 2017 tow 15 (is a repeat of GM2016 tow 15) assigned to OUT
lined$VMSAREA[lined$TOW_NO==56 & lined$CRUISE == "GM2017"] <-"OUT"
#8 In 2017, tow 95 needs to be IN
lined$VMSAREA[lined$TOW_NO==95 & lined$CRUISE == "GM2017"] <- "IN"

#Clean up VMSSTRATA for GM2017
lined$VMSAREA[lined$TOW_NO==86 & lined$CRUISE == "GM2017"] <- "OUT"
lined$VMSAREA[lined$TOW_NO==23 & lined$CRUISE == "GM2017"] <- "OUT"
lined$VMSAREA[lined$TOW_NO==104 & lined$CRUISE == "GM2017"] <- "OUT"
lined$VMSAREA[lined$TOW_NO==120 & lined$CRUISE == "GM2017"] <- "OUT"

#8. In 2018 tow 40 (is a repeat of GM2017 tow 56) assigned to IN but needs to be OUT - a repeated repeat
lined$VMSAREA[lined$TOW_NO==40 & lined$CRUISE == "GM2018"] <-"OUT"

#9. In 2019 tow 95 assigned to IN but needs to be OUT 
lined$VMSAREA[lined$TOW_NO==95 & lined$CRUISE == "GM2019"] <-"OUT"


## ununlined livefreq errors 
#1. Tow 32 in 2011 (73 in 2010) not assinged to IN
#   Tow 70 in 2011 (90 in 2010) not assinged to IN
unlined$VMSAREA[unlined$TOW_NO==32 & unlined$CRUISE == "GM2011"] <- "IN"
unlined$VMSAREA[unlined$TOW_NO==70 & unlined$CRUISE == "GM2011"] <-"IN"

#2. In 2012 tow 9, 27, 111 not assinged to IN
unlined$VMSAREA[unlined$TOW_NO==9 & unlined$CRUISE == "GM2012"] <- "IN"
unlined$VMSAREA[unlined$TOW_NO==27 & unlined$CRUISE == "GM2012"] <-"IN"
unlined$VMSAREA[unlined$TOW_NO==111 & unlined$CRUISE == "GM2012"] <-"IN"

#3. In 2013 tow 9, 27, 111 not assinged to IN
unlined$VMSAREA[unlined$TOW_NO==55 & unlined$CRUISE == "GM2013"] <- "IN"
unlined$VMSAREA[unlined$TOW_NO==109 & unlined$CRUISE == "GM2013"] <-"IN"
unlined$VMSAREA[unlined$TOW_NO==116 & unlined$CRUISE == "GM2013"] <-"IN"

#4. In 2014 tow 9, 27, 111 not assinged to IN
unlined$VMSAREA[unlined$TOW_NO==31 & unlined$CRUISE == "GM2014"] <- "IN"
unlined$VMSAREA[unlined$TOW_NO==42 & unlined$CRUISE == "GM2014"] <-"OUT"

#5. In 2015 tow 20 not assigned to IN ( 2015 tow 20 links to 2014 tow 31 which links to a 2013 tow )
unlined$VMSAREA[unlined$TOW_NO==20 & unlined$CRUISE == "GM2015"] <-"IN"
unlined$VMSAREA[unlined$TOW_NO==25 & unlined$CRUISE == "GM2015"] <-"OUT"

#6. In 2016 tow 15,75,92 not assinged to OUT
unlined$VMSAREA[unlined$TOW_NO==15 & unlined$CRUISE == "GM2016"] <-"OUT"
unlined$VMSAREA[unlined$TOW_NO==75 & unlined$CRUISE == "GM2016"] <-"OUT"
unlined$VMSAREA[unlined$TOW_NO==92 & unlined$CRUISE == "GM2016"] <-"OUT"

#7. In 2017 tow 15 (is a repeat of GM2016 tow 15) assigned to OUT
unlined$VMSAREA[unlined$TOW_NO==56 & unlined$CRUISE == "GM2017"] <-"OUT"
#8 In 2017, tow 95 needs to be IN
unlined$VMSAREA[unlined$TOW_NO==95 & unlined$CRUISE == "GM2017"] <- "IN"

#Clean up VMSSTRATA for GM2017
unlined$VMSAREA[unlined$TOW_NO==86 & unlined$CRUISE == "GM2017"] <- "OUT"
unlined$VMSAREA[unlined$TOW_NO==23 & unlined$CRUISE == "GM2017"] <- "OUT"
unlined$VMSAREA[unlined$TOW_NO==104 & unlined$CRUISE == "GM2017"] <- "OUT"
unlined$VMSAREA[unlined$TOW_NO==120 & unlined$CRUISE == "GM2017"] <- "OUT"

#8. In 2018 tow 40 (is a repeat of GM2017 tow 56) assigned to IN but needs to be OUT - a repeated repeat
unlined$VMSAREA[unlined$TOW_NO==40 & unlined$CRUISE == "GM2018"] <-"OUT"

#9. In 2019 tow 95 assogmed to IN but needs to be OUT 
unlined$VMSAREA[unlined$TOW_NO==95 & unlined$CRUISE == "GM2019"] <-"OUT"


#Check 
table(lined$VMSAREA)
table(unlined$VMSAREA)


### Subset data for individual years for SPR command (ONLY TOW_TYPE==1 AND  ONLY TOW_TYPE==5) ###
tows <- c(1,5)
#lined live freq  
year <- c(seq(2005,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(lined, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("GMlined", i), sub)
}

#Crossref 
year <- c(seq(2006,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(crossref, CRUISE==paste0("GM",i))
  assign(paste0("crossref.GM.", i), sub)
}

##Subset data for individual years for SPR command (ONLY TOW_TYPE==1 AND  ONLY TOW_TYPE==5)
tows <- c(1,5)
#unlined live freq  
year <- c(seq(2005,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(unlined, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("GMunlined", i), sub)
}


####
##  ----  Calculate Commercial number per tow from LINED tows ----
##
#set size ranges
#CS <- 27:50 #commercial size (>=80mm)
#RS <- 24:50 #recruit size that grows to commercial size in 1 year (65-79.9mm)
#PS <-    #precrecruit size that grows to recuit size in 1 year (50-64.9mm)


###... SPA 6 IN VMS commercial size (>=80mm) ...###
STRATA.ID <- "IN"
years <- 1997:surveyyear
X <- length(years)

#simple means
SPA6.Comm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Comm.simple$Year)){
	temp.data <- lined[lined$YEAR==1996+i,]
	#add calculation of variance if required
	SPA6.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
	SPA6.Comm.simple[i,3] <- var(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SPA6.Comm.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA6.Comm.simple$Year, SPA6.Comm.simple$Mean.nums, xout=2020) #  136.6691
SPA6.Comm.simple[SPA6.Comm.simple$Year==2020,c("Mean.nums","var.y")] <- c(136.6691, 84955.507) #assume var from 2019


#dataframe for SPR estimates Commercial
spryears <- 2006:surveyyear #update to most recent year
Y <- length(spryears)
spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2005/2006
    test.2006 <- spr(GMlined2005$TOW_NO[GMlined2005$VMSAREA==STRATA.ID],apply(GMlined2005[GMlined2005$VMSAREA==STRATA.ID,24:50],1,sum), #recruits included in previous yr
    GMlined2006$TOW_NO[GMlined2006$VMSAREA==STRATA.ID],apply(GMlined2006[GMlined2006$VMSAREA==STRATA.ID,27:50],1,sum), #just commercial size for current yr
    crossref.GM.2006[crossref.GM.2006$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary(test.2006)  #
    spr.est[spr.est$Year==2006,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2006/2007
    test.2007 <- spr(GMlined2006$TOW_NO[GMlined2006$VMSAREA==STRATA.ID],apply(GMlined2006[GMlined2006$VMSAREA==STRATA.ID,24:50],1,sum), #recruits included in previous yr
    GMlined2007$TOW_NO[GMlined2007$VMSAREA==STRATA.ID],apply(GMlined2007[GMlined2007$VMSAREA==STRATA.ID,27:50],1,sum), #just commercial size for current yr
    crossref.GM.2007[crossref.GM.2007$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary(test.2007,summary(test.2006))  #
    spr.est[spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
    test.2008 <- spr(GMlined2007$TOW_NO[GMlined2007$VMSAREA==STRATA.ID],apply(GMlined2007[GMlined2007$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2008$TOW_NO[GMlined2008$VMSAREA==STRATA.ID],apply(GMlined2008[GMlined2008$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2008[crossref.GM.2008$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2008, summary(test.2007,summary(test.2006)))  #
    spr.est[spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
	test.2009 <- spr(GMlined2008$TOW_NO[GMlined2008$VMSAREA==STRATA.ID],apply(GMlined2008[GMlined2008$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2009$TOW_NO[GMlined2009$VMSAREA==STRATA.ID],apply(GMlined2009[GMlined2009$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2009[crossref.GM.2009$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))) #
    spr.est[spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
	test.2010 <- spr(GMlined2009$TOW_NO[GMlined2009$VMSAREA==STRATA.ID],apply(GMlined2009[GMlined2009$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2010$TOW_NO[GMlined2010$VMSAREA==STRATA.ID],apply(GMlined2010[GMlined2010$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2010[crossref.GM.2010$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))   #
    spr.est[spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
	test.2011 <- spr(GMlined2010$TOW_NO[GMlined2010$VMSAREA==STRATA.ID],apply(GMlined2010[GMlined2010$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2011$TOW_NO[GMlined2011$VMSAREA==STRATA.ID],apply(GMlined2011[GMlined2011$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2011[crossref.GM.2011$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))) #
    spr.est[spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
	test.2012 <- spr(GMlined2011$TOW_NO[GMlined2011$VMSAREA==STRATA.ID],apply(GMlined2011[GMlined2011$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2012$TOW_NO[GMlined2012$VMSAREA==STRATA.ID],apply(GMlined2012[GMlined2012$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2012[crossref.GM.2012$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))   # 46.62
	spr.est[spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
	test.2013 <- spr(GMlined2012$TOW_NO[GMlined2012$VMSAREA==STRATA.ID],apply(GMlined2012[GMlined2012$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2013$TOW_NO[GMlined2013$VMSAREA==STRATA.ID],apply(GMlined2013[GMlined2013$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2013[crossref.GM.2013$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))   #77.3
    spr.est[spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
	test.2014 <- spr(GMlined2013$TOW_NO[GMlined2013$VMSAREA==STRATA.ID],apply(GMlined2013[GMlined2013$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2014$TOW_NO[GMlined2014$VMSAREA==STRATA.ID],apply(GMlined2014[GMlined2014$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2014[crossref.GM.2014$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))   #134.7
	spr.est[spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
	test.2015 <- spr(GMlined2014$TOW_NO[GMlined2014$VMSAREA==STRATA.ID],apply(GMlined2014[GMlined2014$VMSAREA==STRATA.ID,24:50],1,sum),
    GMlined2015$TOW_NO[GMlined2015$VMSAREA==STRATA.ID],apply(GMlined2015[GMlined2015$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2015[crossref.GM.2015$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))   #134.7
	spr.est[spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
test.2016 <- spr(GMlined2015$TOW_NO[GMlined2015$VMSAREA==STRATA.ID],apply(GMlined2015[GMlined2015$VMSAREA==STRATA.ID,24:50],1,sum),
	               GMlined2016$TOW_NO[GMlined2016$VMSAREA==STRATA.ID],apply(GMlined2016[GMlined2016$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2016[crossref.GM.2016$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))   #134.7
	spr.est[spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

	
#2016/2017
test.2017 <- spr(GMlined2016$TOW_NO[GMlined2016$VMSAREA==STRATA.ID],apply(GMlined2016[GMlined2016$VMSAREA==STRATA.ID,24:50],1,sum),
	               GMlined2017$TOW_NO[GMlined2017$VMSAREA==STRATA.ID],apply(GMlined2017[GMlined2017$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2017[crossref.GM.2017$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))   #134.7
	spr.est[spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
	test.2018 <- spr(GMlined2017$TOW_NO[GMlined2017$VMSAREA==STRATA.ID],apply(GMlined2017[GMlined2017$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2018$TOW_NO[GMlined2018$VMSAREA==STRATA.ID],apply(GMlined2018[GMlined2018$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2018[crossref.GM.2018$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))   
	spr.est[spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2018/2019
	test.2019 <- spr(GMlined2018$TOW_NO[GMlined2018$VMSAREA==STRATA.ID],apply(GMlined2018[GMlined2018$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2019$TOW_NO[GMlined2019$VMSAREA==STRATA.ID],apply(GMlined2019[GMlined2019$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2019[crossref.GM.2019$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (test.2019,summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))  
	spr.est[spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2019/2021
	test.2021 <- spr(GMlined2019$TOW_NO[GMlined2019$VMSAREA==STRATA.ID],apply(GMlined2019[GMlined2019$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2021$TOW_NO[GMlined2021$VMSAREA==STRATA.ID],apply(GMlined2021[GMlined2021$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2021[crossref.GM.2021$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(	test.2021 , summary (test.2019,summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))))  
	spr.est[spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2021/2022
	test.2022 <- spr(GMlined2021$TOW_NO[GMlined2021$VMSAREA==STRATA.ID],apply(GMlined2021[GMlined2021$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2022$TOW_NO[GMlined2022$VMSAREA==STRATA.ID],apply(GMlined2022[GMlined2022$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2022[crossref.GM.2022$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(	test.2022 , summary(	test.2021 , summary (test.2019,summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))))  
	spr.est[spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2022/2023
	test.2023 <- spr(GMlined2022$TOW_NO[GMlined2022$VMSAREA==STRATA.ID],apply(GMlined2022[GMlined2022$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2023$TOW_NO[GMlined2023$VMSAREA==STRATA.ID],apply(GMlined2023[GMlined2023$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2023[crossref.GM.2023$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2023,summary(	test.2022 , summary(	test.2021 , summary (test.2019,summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))))))
	spr.est[spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2023/2024
	test.2024 <- spr(GMlined2023$TOW_NO[GMlined2023$VMSAREA==STRATA.ID],apply(GMlined2023[GMlined2023$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2024$TOW_NO[GMlined2024$VMSAREA==STRATA.ID],apply(GMlined2024[GMlined2024$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2024[crossref.GM.2024$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2024,summary(test.2023,summary(	test.2022 , summary(	test.2021 , summary (test.2019,summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))))))
	spr.est[spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2024/2025
	test.2025 <- spr(GMlined2024$TOW_NO[GMlined2024$VMSAREA==STRATA.ID],apply(GMlined2024[GMlined2024$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2025$TOW_NO[GMlined2025$VMSAREA==STRATA.ID],apply(GMlined2025[GMlined2025$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2025[crossref.GM.2025$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2025,summary(test.2024,summary(test.2023,summary(	test.2022 , summary(	test.2021 , summary (test.2019,summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006)))))))))))))))))))
	spr.est[spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2025/2026
	test.2026 <- spr(GMlined2025$TOW_NO[GMlined2025$VMSAREA==STRATA.ID],apply(GMlined2025[GMlined2025$VMSAREA==STRATA.ID,24:50],1,sum),
	                 GMlined2026$TOW_NO[GMlined2026$VMSAREA==STRATA.ID],apply(GMlined2026[GMlined2026$VMSAREA==STRATA.ID,27:50],1,sum),
	                 crossref.GM.2026[crossref.GM.2026$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(test.2026,summary(test.2025,summary(test.2024,summary(test.2023,summary(	test.2022 , summary(	test.2021 , summary (test.2019,summary (test.2018, summary (test.2017, summary (test.2016, summary(test.2015, summary(test.2014, summary(test.2013, summary(test.2012, summary(test.2011, summary(test.2010, summary(test.2009, summary (test.2008, summary(test.2007,summary(test.2006))))))))))))))))))))
	spr.est[spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
spr.est

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(spr.est$Year, spr.est$Yspr, xout=2020) #  127.1816
spr.est[spr.est$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c(127.1816, 447.30610) #assume var from 2019

spr.est$method <- "spr"
names(spr.est) <- c("Year", "Mean.nums", "var.y", "method")

SPA6.Comm.IN.lined <- rbind(SPA6.Comm.simple[SPA6.Comm.simple$Year<2006,], spr.est)
SPA6.Comm.IN.lined$cv <- sqrt(SPA6.Comm.IN.lined$var.y)/SPA6.Comm.IN.lined$Mean.nums
SPA6.Comm.IN.lined$VMSAREA <- STRATA.ID
SPA6.Comm.IN.lined$GEAR <- 'lined'


####
##  ----  Calculate Commercial number per tow from UNLINED tows ----
##
#SPA 6C(30), 6B(31), 6A(32)
# NOTE: 2005 data in BF cruise data;

#set size ranges
#CS <- 27:50 #commercial size (>=80mm)
#RS <- 24:50 #recruit size that grows to commercial size in 1 year (65-79.9mm)
#PS <-    #precrecruit size that grows to recuit size in 1 year (50-64.9mm)


###... SPA 6 IN VMS commercial size (>=80mm) ...###
STRATA.ID <- "IN"
years <- 1997:surveyyear
X <- length(years)

#simple means
SPA6.Comm.simple.unlined <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Comm.simple.unlined$Year)){
	temp.data <- unlined[unlined$YEAR==1996+i,]
	#add calculation of variance if required
	SPA6.Comm.simple.unlined[i,2] <- mean(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
	SPA6.Comm.simple.unlined[i,3] <- var(apply(temp.data[temp.data$VMSAREA==STRATA.ID & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}

SPA6.Comm.simple.unlined

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(SPA6.Comm.simple.unlined$Year, SPA6.Comm.simple.unlined$Mean.nums, xout=2020) #  230.7911
SPA6.Comm.simple.unlined[SPA6.Comm.simple.unlined$Year==2020,c("Mean.nums","var.y")] <- c(230.7911, 286582.802) #assume var from 2019


#dataframe for SPR estimates Commercial
spryears <- 2006:surveyyear #update to most recent year
Y <- length(spryears)
spr.est.unlined <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2005/2006
    testunlined.2006 <- spr(GMunlined2005$TOW_NO[GMunlined2005$VMSAREA==STRATA.ID],apply(GMunlined2005[GMunlined2005$VMSAREA==STRATA.ID,24:50],1,sum), #recruits included in previous yr
    GMunlined2006$TOW_NO[GMunlined2006$VMSAREA==STRATA.ID],apply(GMunlined2006[GMunlined2006$VMSAREA==STRATA.ID,27:50],1,sum), #just commercial size for current yr
    crossref.GM.2006[crossref.GM.2006$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary(testunlined.2006)  #
    spr.est.unlined[spr.est.unlined$Year==2006,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2006/2007
    testunlined.2007 <- spr(GMunlined2006$TOW_NO[GMunlined2006$VMSAREA==STRATA.ID],apply(GMunlined2006[GMunlined2006$VMSAREA==STRATA.ID,24:50],1,sum), #recruits included in previous yr
    GMunlined2007$TOW_NO[GMunlined2007$VMSAREA==STRATA.ID],apply(GMunlined2007[GMunlined2007$VMSAREA==STRATA.ID,27:50],1,sum), #just commercial size for current yr
    crossref.GM.2007[crossref.GM.2007$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
    K <- summary(testunlined.2007,summary(testunlined.2006))  #
    spr.est.unlined[spr.est.unlined$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
    testunlined.2008 <- spr(GMunlined2007$TOW_NO[GMunlined2007$VMSAREA==STRATA.ID],apply(GMunlined2007[GMunlined2007$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2008$TOW_NO[GMunlined2008$VMSAREA==STRATA.ID],apply(GMunlined2008[GMunlined2008$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2008[crossref.GM.2008$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))  #
    spr.est.unlined[spr.est.unlined$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
	testunlined.2009 <- spr(GMunlined2008$TOW_NO[GMunlined2008$VMSAREA==STRATA.ID],apply(GMunlined2008[GMunlined2008$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2009$TOW_NO[GMunlined2009$VMSAREA==STRATA.ID],apply(GMunlined2009[GMunlined2009$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2009[crossref.GM.2009$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))) #
    spr.est.unlined[spr.est.unlined$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
	testunlined.2010 <- spr(GMunlined2009$TOW_NO[GMunlined2009$VMSAREA==STRATA.ID],apply(GMunlined2009[GMunlined2009$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2010$TOW_NO[GMunlined2010$VMSAREA==STRATA.ID],apply(GMunlined2010[GMunlined2010$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2010[crossref.GM.2010$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))   #
    spr.est.unlined[spr.est.unlined$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
	testunlined.2011 <- spr(GMunlined2010$TOW_NO[GMunlined2010$VMSAREA==STRATA.ID],apply(GMunlined2010[GMunlined2010$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2011$TOW_NO[GMunlined2011$VMSAREA==STRATA.ID],apply(GMunlined2011[GMunlined2011$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2011[crossref.GM.2011$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))) #
    spr.est.unlined[spr.est.unlined$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
	testunlined.2012 <- spr(GMunlined2011$TOW_NO[GMunlined2011$VMSAREA==STRATA.ID],apply(GMunlined2011[GMunlined2011$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2012$TOW_NO[GMunlined2012$VMSAREA==STRATA.ID],apply(GMunlined2012[GMunlined2012$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2012[crossref.GM.2012$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))   # 46.62
	spr.est.unlined[spr.est.unlined$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
	testunlined.2013 <- spr(GMunlined2012$TOW_NO[GMunlined2012$VMSAREA==STRATA.ID],apply(GMunlined2012[GMunlined2012$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2013$TOW_NO[GMunlined2013$VMSAREA==STRATA.ID],apply(GMunlined2013[GMunlined2013$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2013[crossref.GM.2013$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006))))))))   #77.3
    spr.est.unlined[spr.est.unlined$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
	testunlined.2014 <- spr(GMunlined2013$TOW_NO[GMunlined2013$VMSAREA==STRATA.ID],apply(GMunlined2013[GMunlined2013$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2014$TOW_NO[GMunlined2014$VMSAREA==STRATA.ID],apply(GMunlined2014[GMunlined2014$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2014[crossref.GM.2014$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))))   #134.7
	spr.est.unlined[spr.est.unlined$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2014/2015
	testunlined.2015 <- spr(GMunlined2014$TOW_NO[GMunlined2014$VMSAREA==STRATA.ID],apply(GMunlined2014[GMunlined2014$VMSAREA==STRATA.ID,24:50],1,sum),
    GMunlined2015$TOW_NO[GMunlined2015$VMSAREA==STRATA.ID],apply(GMunlined2015[GMunlined2015$VMSAREA==STRATA.ID,27:50],1,sum),
    crossref.GM.2015[crossref.GM.2015$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006))))))))))   #134.7
	spr.est.unlined[spr.est.unlined$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
testunlined.2016 <- spr(GMunlined2015$TOW_NO[GMunlined2015$VMSAREA==STRATA.ID],apply(GMunlined2015[GMunlined2015$VMSAREA==STRATA.ID,24:50],1,sum),
	                     GMunlined2016$TOW_NO[GMunlined2016$VMSAREA==STRATA.ID],apply(GMunlined2016[GMunlined2016$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2016[crossref.GM.2016$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))))))   #134.7
	spr.est.unlined[spr.est.unlined$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2016/2017
testunlined.2017 <- spr(GMunlined2016$TOW_NO[GMunlined2016$VMSAREA==STRATA.ID],apply(GMunlined2016[GMunlined2016$VMSAREA==STRATA.ID,24:50],1,sum),
	                     GMunlined2017$TOW_NO[GMunlined2017$VMSAREA==STRATA.ID],apply(GMunlined2017[GMunlined2017$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2017[crossref.GM.2017$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006))))))))))))   
	spr.est.unlined[spr.est.unlined$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2017/2018
	testunlined.2018 <- spr(GMunlined2017$TOW_NO[GMunlined2017$VMSAREA==STRATA.ID],apply(GMunlined2017[GMunlined2017$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2018$TOW_NO[GMunlined2018$VMSAREA==STRATA.ID],apply(GMunlined2018[GMunlined2018$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2018[crossref.GM.2018$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <-summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))))))))   
	spr.est.unlined[spr.est.unlined$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2018/2019
	testunlined.2019 <- spr(GMunlined2018$TOW_NO[GMunlined2018$VMSAREA==STRATA.ID],apply(GMunlined2018[GMunlined2018$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2019$TOW_NO[GMunlined2019$VMSAREA==STRATA.ID],apply(GMunlined2019[GMunlined2019$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2019[crossref.GM.2019$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <-summary (testunlined.2019, summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006))))))))))))))  
	spr.est.unlined[spr.est.unlined$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2019/2021
	testunlined.2021 <- spr(GMunlined2019$TOW_NO[GMunlined2019$VMSAREA==STRATA.ID],apply(GMunlined2019[GMunlined2019$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2021$TOW_NO[GMunlined2021$VMSAREA==STRATA.ID],apply(GMunlined2021[GMunlined2021$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2021[crossref.GM.2021$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2021, summary (testunlined.2019, summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))))))))))  
	spr.est.unlined[spr.est.unlined$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2021/2022
	testunlined.2022 <- spr(GMunlined2021$TOW_NO[GMunlined2021$VMSAREA==STRATA.ID],apply(GMunlined2021[GMunlined2021$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2022$TOW_NO[GMunlined2022$VMSAREA==STRATA.ID],apply(GMunlined2022[GMunlined2022$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2022[crossref.GM.2022$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2022, summary(testunlined.2021, summary (testunlined.2019, summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006))))))))))))))))  
	spr.est.unlined[spr.est.unlined$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2022/2023
	testunlined.2023 <- spr(GMunlined2022$TOW_NO[GMunlined2022$VMSAREA==STRATA.ID],apply(GMunlined2022[GMunlined2022$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2023$TOW_NO[GMunlined2023$VMSAREA==STRATA.ID],apply(GMunlined2023[GMunlined2023$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2023[crossref.GM.2023$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2023,summary(testunlined.2022, summary(testunlined.2021, summary (testunlined.2019, summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))))))))))))  
	spr.est.unlined[spr.est.unlined$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
#2023/2024
	testunlined.2024 <- spr(GMunlined2023$TOW_NO[GMunlined2023$VMSAREA==STRATA.ID],apply(GMunlined2023[GMunlined2023$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2024$TOW_NO[GMunlined2024$VMSAREA==STRATA.ID],apply(GMunlined2024[GMunlined2024$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2024[crossref.GM.2024$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2024,summary(testunlined.2023,summary(testunlined.2022, summary(testunlined.2021, summary (testunlined.2019, summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006))))))))))))))))))  
	spr.est.unlined[spr.est.unlined$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

	
#2024/2025
	testunlined.2025 <- spr(GMunlined2024$TOW_NO[GMunlined2024$VMSAREA==STRATA.ID],apply(GMunlined2024[GMunlined2024$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2025$TOW_NO[GMunlined2025$VMSAREA==STRATA.ID],apply(GMunlined2025[GMunlined2025$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2025[crossref.GM.2025$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2025, summary(testunlined.2024,summary(testunlined.2023,summary(testunlined.2022, summary(testunlined.2021, summary (testunlined.2019, summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))))))))))))))  
	spr.est.unlined[spr.est.unlined$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
	
#2025/2026
	testunlined.2026 <- spr(GMunlined2025$TOW_NO[GMunlined2025$VMSAREA==STRATA.ID],apply(GMunlined2025[GMunlined2025$VMSAREA==STRATA.ID,24:50],1,sum),
	                        GMunlined2026$TOW_NO[GMunlined2026$VMSAREA==STRATA.ID],apply(GMunlined2026[GMunlined2026$VMSAREA==STRATA.ID,27:50],1,sum),
	                        crossref.GM.2026[crossref.GM.2026$VMSAREA==STRATA.ID,c("TOW_NO_REF","TOW_NO")])
	K <- summary(testunlined.2026,summary(testunlined.2025, summary(testunlined.2024,summary(testunlined.2023,summary(testunlined.2022, summary(testunlined.2021, summary (testunlined.2019, summary (testunlined.2018, summary (testunlined.2017, summary (testunlined.2016, summary(testunlined.2015, summary(testunlined.2014, summary(testunlined.2013, summary(testunlined.2012, summary(testunlined.2011, summary(testunlined.2010, summary(testunlined.2009, summary (testunlined.2008, summary(testunlined.2007,summary(testunlined.2006)))))))))))))))))))) 
	spr.est.unlined[spr.est.unlined$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
	
spr.est.unlined

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(spr.est.unlined$Year, spr.est.unlined$Yspr, xout=2020) #   193.1981
spr.est.unlined[spr.est.unlined$Year==2020,c("Yspr", "var.Yspr.corrected")]<-c( 193.1981,  749.00114) #assume var from 2019

spr.est.unlined$method <- "spr"
names(spr.est.unlined) <- c("Year", "Mean.nums", "var.y", "method")

SPA6.Comm.IN.unlined <- rbind(SPA6.Comm.simple.unlined[SPA6.Comm.simple.unlined$Year<2006,], spr.est.unlined)
SPA6.Comm.IN.unlined$cv <- sqrt(SPA6.Comm.IN.unlined$var.y)/SPA6.Comm.IN.unlined$Mean.nums
SPA6.Comm.IN.unlined$VMSAREA <- STRATA.ID
SPA6.Comm.IN.unlined$GEAR <- 'unlined'


###
### ----  CALCUALTE RATIO ----
###

#merge for ratio
names(SPA6.Comm.IN.lined)[grep("Mean.nums", names(SPA6.Comm.IN.lined))] <- 'Mean.nums.lined' #col 2 
names(SPA6.Comm.IN.unlined)[grep("Mean.nums", names(SPA6.Comm.IN.unlined))] <- 'Mean.nums.unlined' # col 2 

ratio <- merge(SPA6.Comm.IN.lined[,c("Year","Mean.nums.lined")], SPA6.Comm.IN.unlined[,c("Year","Mean.nums.unlined")],by.x="Year")
ratio$ratiolined <- ratio$Mean.nums.lined/ratio$Mean.nums.unlined
# Make names consistent with other inshore areas
names(ratio) <- c("Year","Lined","Unlined","ratiolined")

# Create a new Ratio file, the most recent year will be added to the model using the SPA4_ModelFile.R
write.csv(ratio,paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA6_ratioLinedtoUnlined",surveyyear,".csv"))



### ---- Plots ----


#Survey Ratio per year 
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA6_ratio",surveyyear,".png"),width=11,height=8,units = "in",res=920)

ggplot(data = ratio, aes(x=Year, y=ratiolined)) + 
  geom_point() + 
  geom_line() +  
  theme_bw() + ylab("Ratio") + xlab("Year") 

dev.off()


#Survey numbers per year from lined vs unlined 
unlined.lined.for.plot <- pivot_longer(ratio[,c("Year","Lined","Unlined")], 
                                       cols = c("Lined","Unlined"),
                                       names_to = "Gear",
                                       values_to = "Value",
                                       values_drop_na = FALSE)
unlined.lined.for.plot$Year <- as.numeric(unlined.lined.for.plot$Year)

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA6_lined.vs.unlined",surveyyear,".png"),width=11,height=8,units = "in",res=920)

ggplot(data = unlined.lined.for.plot, aes(x=Year, y=Value, col=Gear, pch=Gear)) + 
  geom_point() + 
  geom_line(aes(linetype = Gear)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.05, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))

dev.off() 

### END OF SCRIPT #### 

# EXTRA 

### ............................ ###
### Comparisons of SPR estimates ###
### ............................ ###

# commerical size
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
plot(test.2021, ylim=c(0,2500),xlim=c(0,2500))




