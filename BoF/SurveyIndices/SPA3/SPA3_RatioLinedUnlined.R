###........................................###
###
###    SPA 3
###    Ratio Lined/Unlined
###
###
###    L.Nasmith
###    July 2016
###		Modified by J. Sameoto Aug 2017, Aug 2018
###   DK snuck in Aug 2018 to automate the output of the ratio's
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(PBSmapping) ### used for findPolys() -- will need to review and replace this soon.. 
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
lined.sql <- ("SELECT * FROM scallsur.sclinedlive_std WHERE strata_id IN (22,23,24)")
unlined.sql <- ("SELECT * FROM scallsur.scunlinedlive_std WHERE strata_id IN (22,23,24)")
cross.ref.query <-  ("SELECT * FROM scallsur.sccomparisontows WHERE sccomparisontows.cruise LIKE 'BI%' AND COMP_TYPE= 'R'")

lined <- dbGetQuery(chan, lined.sql)
unlined <- dbGetQuery(chan, unlined.sql)
crossref.spa3 <- dbGetQuery(chan, cross.ref.query)
dbDisconnect(chan)

#add YEAR and CRUISEID column to data
lined$YEAR <- as.numeric(substr(lined$CRUISE,3,6))
lined$CruiseID <- paste(lined$CRUISE,lined$TOW_NO,sep='.')
unlined$YEAR <- as.numeric(substr(unlined$CRUISE,3,6))
unlined$CruiseID <- paste(unlined$CRUISE,unlined$TOW_NO,sep='.')
crossref.spa3$CruiseID<-paste(crossref.spa3$CRUISE_REF,crossref.spa3$TOW_NO_REF,sep='.')  #create CRUISE_ID on "parent/reference" tow

#
# ---- post-stratify SPA3 for VMS strata ----
#
#polygon to for assigning new strata to data
spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")
#spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")

#adjust data files for subsequent analysis
lined$lat<-convert.dd.dddd(lined$START_LAT)
lined$lon<-convert.dd.dddd(lined$START_LONG)
lined$ID<-1:nrow(lined)

unlined$lat<-convert.dd.dddd(unlined$START_LAT)
unlined$lon<-convert.dd.dddd(unlined$START_LONG)
unlined$ID<-1:nrow(unlined)

#identify tows "inside" the VMS strata and assign new STRATA_ID to them (99)
events=subset(lined,STRATA_ID%in%23:24,c("ID","lon","lat"))
names(events)<-c("EID","X","Y")
lined$STRATA_ID[lined$ID%in%findPolys(events,spa3area)$EID]<-99

events=subset(unlined,STRATA_ID%in%23:24,c("ID","lon","lat"))
names(events)<-c("EID","X","Y")
unlined$STRATA_ID[unlined$ID%in%findPolys(events,spa3area)$EID]<-99

#Create ID column on cross reference files for spr
crossref.spa3$CruiseID <- paste(crossref.spa3$CRUISE_REF,crossref.spa3$TOW_NO_REF,sep='.')  #create CRUISE_ID on "parent" tow
crossref.spa3$CruiseID.current <- paste0(crossref.spa3$CRUISE,".",crossref.spa3$TOW_NO) #create CRUISE_ID on "child - current year" tow

#Check for potential errors that will break the SPR estimates
#merge STRATA_ID from BIlivefreq to the crosssref files based on parent/reference tow and find those that don't match for their strata_ID 
crossref.spa3 <- left_join(crossref.spa3, lined %>% dplyr::select(CruiseID, STRATA_ID, TOW_TYPE_ID), by="CruiseID")
crossref.spa3 <- left_join(crossref.spa3, lined %>% dplyr::select(CruiseID, STRATA_ID.current = STRATA_ID, TOW_TYPE_ID.current = TOW_TYPE_ID), by=c("CruiseID.current" = "CruiseID"))
crossref.spa3$strata_diff <- crossref.spa3$STRATA_ID -  crossref.spa3$STRATA_ID.current
#Strata IDs that need fixing 
crossref.spa3 %>% filter(strata_diff!=0)
#tows that are not tow type 1 or 5 but are repeats that should not have been!
crossref.spa3.tow.type.2 <- crossref.spa3 %>% filter(!TOW_TYPE_ID%in%c(1,5) | !TOW_TYPE_ID.current%in%c(1,5))


##Subset data for individual years for SPR command (ONLY TOW_TYPE==1 AND  ONLY TOW_TYPE==5)
tows <- c(1,5)
#lined 
year <- c(seq(2006,2019),seq(2021,surveyyear))
for(i in unique(year)){
  sub <- subset(lined, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("lined", i), sub)
}

#liveweight 
for(i in unique(year)){
  sub <- subset(unlined, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("unlined", i), sub)
}

#Crossref 
year <- c(seq(2007,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(crossref.spa3, CRUISE==paste0("BI",i))
  assign(paste0("crossref.spa3.", i), sub)
}

####
# ---- correct data-sets for errors ----
###
#some repeated tows will not be assigned to the same strata as the "parent" tow due to where the START LAT and LONG are
#repeated tows should be corrected to match parent tow
#some experimental tows were used as parent tows for repeats, these should be removed

#1. Tow 19 in 2007 (159 in 2006) not assigned to 23
#   Tow 12 in 2007 (73 in 2006) not assigned to 23
# BI2007.12,  BI2007.19
lined2007$STRATA_ID[lined2007$TOW_NO %in% c(12,19)] <- 23
unlined2007$STRATA_ID[unlined2007$TOW_NO %in% c(12,19)] <- 23

#2. In 2008 tow 10 (77 in 2007) not assigned to 22; BI2008.10
lined2008$STRATA_ID[lined2008$TOW_NO %in% c(10) ] <- 22
unlined2008$STRATA_ID[unlined2008$TOW_NO %in% c(10) ] <- 22

#3. In 2009, tow 52 (repeat of tow 12 in 2008) was not assinged to 99.; BI2009.52
lined2009$STRATA_ID[lined2009$TOW_NO %in% c(52) ] <- 99
unlined2009$STRATA_ID[unlined2009$TOW_NO %in% c(52) ] <- 99

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
lined2013$STRATA_ID[lined2013$TOW_NO%in%c(35,85,101)] <- 99
unlined2013$STRATA_ID[unlined2013$TOW_NO%in%c(35,85,101)] <- 99

#9. In 2013 tow 109 (tow 92 in 2012) not assigned to 23
lined2013$STRATA_ID[lined2013$TOW_NO%in%c(109)] <- 23
unlined2013$STRATA_ID[unlined2013$TOW_NO%in%c(109)] <- 23

#10. In 2016, tow 90 (96 in 2015) not assigned to 99
lined2016$STRATA_ID[lined2016$TOW_NO%in%c(90)] <- 99
unlined2016$STRATA_ID[unlined2016$TOW_NO%in%c(90)] <- 99

#11. In 2017, tow 28 (4 in 2016) not assigned to 99
#In 2017, tow 61 (47 in 2016) not assigned to 99
#In 2017, tow 75 (69 in 2016) not assigned to 99
lined2017$STRATA_ID[lined2017$TOW_NO%in%c(28,61,75)] <-99
unlined2017$STRATA_ID[unlined2017$TOW_NO%in%c(28,61,75)] <-99

#12.In 2022, Tows 82 and 110 were experimental but repeated in 2023 (Tows 70 and 82 respectively).
crossref.spa3.2023 <- crossref.spa3.2023[!(crossref.spa3.2023$CRUISE_REF=="BI2022"&crossref.spa3.2023$TOW_NO_REF%in%c(82,110)),]

#write.csv(livefreq2016, 'livefreq2016.csv') #use to plot in ArcGIS - determine which repeated tows in 2017 need strata assignment adjusted 
#write.csv(livefreq2017, 'livefreq2017.csv') #use to plot in ArcGIS - determine which repeated tows in 2017 need strata assignment adjusted 


####
###
### ---- LINED GEAR ----
###
###
# model uses years 1996+, just SMB and Inside VMS

###
# ---- St. Mary's Bay ----
###
#simple mean for non-spr years
years <- 1996:surveyyear
X <- length(years)

SMB.lined.Comm.simple<- data.frame(Year=years, SMB.lined=rep(NA,X),method=rep("simple",X))
for(i in 1:length(SMB.lined.Comm.simple$Year)){
  temp.data <- lined[lined$YEAR==1995+i,]
  #add calculation of variance if required (not needed for ratio lined/unlined)
  SMB.lined.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SMB.lined.Comm.simple

approx(SMB.lined.Comm.simple$Year, SMB.lined.Comm.simple$SMB.lined, xout=1997) # 16.372
approx(SMB.lined.Comm.simple$Year, SMB.lined.Comm.simple$SMB.lined, xout=1998) #  22.99561
approx(SMB.lined.Comm.simple$Year, SMB.lined.Comm.simple$SMB.lined, xout=2002) #  18.3181
approx(SMB.lined.Comm.simple$Year, SMB.lined.Comm.simple$SMB.lined, xout=2003) #  13.70072
approx(SMB.lined.Comm.simple$Year, SMB.lined.Comm.simple$SMB.lined, xout=2020) #  59.66684


SMB.lined.Comm.simple[SMB.lined.Comm.simple$Year==1997,"SMB.lined"]<-16.372
SMB.lined.Comm.simple[SMB.lined.Comm.simple$Year==1998,"SMB.lined"]<-22.99561
SMB.lined.Comm.simple[SMB.lined.Comm.simple$Year==2002,"SMB.lined"]<-18.3181
SMB.lined.Comm.simple[SMB.lined.Comm.simple$Year==2003,"SMB.lined"]<-13.70072
SMB.lined.Comm.simple[SMB.lined.Comm.simple$Year==2020,"SMB.lined"]<- 59.66684

##dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
SMBlined.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006.2007
smblined1<-spr(lined2006$TOW_NO[lined2006$STRATA_ID==22],apply(lined2006[lined2006$STRATA_ID==22,24:50],1,sum),
lined2007$TOW_NO[lined2007$STRATA_ID==22],apply(lined2007[lined2007$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (smblined1)
SMBlined.spr.est[SMBlined.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007.2008
smblined2<-spr(lined2007$TOW_NO[lined2007$STRATA_ID==22],apply(lined2007[lined2007$STRATA_ID==22,24:50],1,sum),
lined2008$TOW_NO[lined2008$STRATA_ID==22],apply(lined2008[lined2008$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined2,summary (smblined1))
SMBlined.spr.est[SMBlined.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008.2009
smblined3<-spr(lined2008$TOW_NO[lined2008$STRATA_ID==22],apply(lined2008[lined2008$STRATA_ID==22,24:50],1,sum),
lined2009$TOW_NO[lined2009$STRATA_ID==22],apply(lined2009[lined2009$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined3,summary (smblined2,summary (smblined1)))
SMBlined.spr.est[SMBlined.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009.2010
smblined4<-spr(lined2009$TOW_NO[lined2009$STRATA_ID==22],apply(lined2009[lined2009$STRATA_ID==22,24:50],1,sum),
lined2010$TOW_NO[lined2010$STRATA_ID==22],apply(lined2010[lined2010$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))
SMBlined.spr.est[SMBlined.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010.2011
smblined5<-spr(lined2010$TOW_NO[lined2010$STRATA_ID==22],apply(lined2010[lined2010$STRATA_ID==22,24:50],1,sum),
lined2011$TOW_NO[lined2011$STRATA_ID==22],apply(lined2011[lined2011$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))
SMBlined.spr.est[SMBlined.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011.2012
smblined6<-spr(lined2011$TOW_NO[lined2011$STRATA_ID==22],apply(lined2011[lined2011$STRATA_ID==22,24:50],1,sum),
lined2012$TOW_NO[lined2012$STRATA_ID==22],apply(lined2012[lined2012$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))))
SMBlined.spr.est[SMBlined.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
smblined7<-spr(lined2012$TOW_NO[lined2012$STRATA_ID==22],apply(lined2012[lined2012$STRATA_ID==22,24:50],1,sum),
lined2013$TOW_NO[lined2013$STRATA_ID==22],apply(lined2013[lined2013$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))))
SMBlined.spr.est[SMBlined.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
smblined8<-spr(lined2013$TOW_NO[lined2013$STRATA_ID==22],apply(lined2013[lined2013$STRATA_ID==22,24:50],1,sum),
lined2014$TOW_NO[lined2014$STRATA_ID==22],apply(lined2014[lined2014$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
smblined9<-spr(lined2014$TOW_NO[lined2014$STRATA_ID==22],apply(lined2014[lined2014$STRATA_ID==22,24:50],1,sum),
lined2015$TOW_NO[lined2015$STRATA_ID==22],apply(lined2015[lined2015$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smblined9, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
smblined10<-spr(lined2015$TOW_NO[lined2015$STRATA_ID==22],apply(lined2015[lined2015$STRATA_ID==22,24:50],1,sum),
               lined2016$TOW_NO[lined2016$STRATA_ID==22],apply(lined2016[lined2016$STRATA_ID==22,27:50],1,sum),
               crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
smblined11<-spr(lined2016$TOW_NO[lined2016$STRATA_ID==22],apply(lined2016[lined2016$STRATA_ID==22,24:50],1,sum),
               lined2017$TOW_NO[lined2017$STRATA_ID==22],apply(lined2017[lined2017$STRATA_ID==22,27:50],1,sum),
               crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
smblined12<-spr(lined2017$TOW_NO[lined2017$STRATA_ID==22],apply(lined2017[lined2017$STRATA_ID==22,24:50],1,sum),
                lined2018$TOW_NO[lined2018$STRATA_ID==22],apply(lined2018[lined2018$STRATA_ID==22,27:50],1,sum),
                crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
smblined13<-spr(lined2018$TOW_NO[lined2018$STRATA_ID==22],apply(lined2018[lined2018$STRATA_ID==22,24:50],1,sum),
                lined2019$TOW_NO[lined2019$STRATA_ID==22],apply(lined2019[lined2019$STRATA_ID==22,27:50],1,sum),
                crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smblined13,summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #NO SURVEY IN 2020 
smblined14<-spr(lined2019$TOW_NO[lined2019$STRATA_ID==22],apply(lined2019[lined2019$STRATA_ID==22,24:50],1,sum),
                lined2021$TOW_NO[lined2021$STRATA_ID==22],apply(lined2021[lined2021$STRATA_ID==22,27:50],1,sum),
                crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K<-summary(smblined14, summary(smblined13,summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2021/2022 
smblined15 <- spr(lined2021$TOW_NO[lined2021$STRATA_ID==22],apply(lined2021[lined2021$STRATA_ID==22,24:50],1,sum),
                lined2022$TOW_NO[lined2022$STRATA_ID==22],apply(lined2022[lined2022$STRATA_ID==22,27:50],1,sum),
                crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K <- summary(smblined15,summary(smblined14, summary(smblined13,summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
smblined16 <- spr(lined2022$TOW_NO[lined2022$STRATA_ID==22],apply(lined2022[lined2022$STRATA_ID==22,24:50],1,sum),
                  lined2023$TOW_NO[lined2023$STRATA_ID==22],apply(lined2023[lined2023$STRATA_ID==22,27:50],1,sum),
                  crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K <- summary(smblined16, summary(smblined15,summary(smblined14, summary(smblined13,summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
smblined17 <- spr(lined2023$TOW_NO[lined2023$STRATA_ID==22],apply(lined2023[lined2023$STRATA_ID==22,24:50],1,sum),
                  lined2024$TOW_NO[lined2024$STRATA_ID==22],apply(lined2024[lined2024$STRATA_ID==22,27:50],1,sum),
                  crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
 
K <- summary(smblined17,summary(smblined16, summary(smblined15,summary(smblined14, summary(smblined13,summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
smblined18 <- spr(lined2024$TOW_NO[lined2024$STRATA_ID==22],apply(lined2024[lined2024$STRATA_ID==22,24:50],1,sum),
                  lined2025$TOW_NO[lined2025$STRATA_ID==22],apply(lined2025[lined2025$STRATA_ID==22,27:50],1,sum),
                  crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K <- summary(smblined18, summary(smblined17,summary(smblined16, summary(smblined15,summary(smblined14, summary(smblined13,summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1))))))))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
smblined19 <- spr(lined2025$TOW_NO[lined2025$STRATA_ID==22],apply(lined2025[lined2025$STRATA_ID==22,24:50],1,sum),
                  lined2026$TOW_NO[lined2026$STRATA_ID==22],apply(lined2026[lined2026$STRATA_ID==22,27:50],1,sum),
                  crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K <- summary(smblined19, summary(smblined18, summary(smblined17,summary(smblined16, summary(smblined15,summary(smblined14, summary(smblined13,summary(smblined12, summary (smblined11, summary(smblined10, summary(smblined10, summary (smblined8,summary (smblined7, summary (smblined6,summary (smblined5, summary (smblined4,summary (smblined3,summary (smblined2,summary (smblined1)))))))))))))))))))

SMBlined.spr.est[SMBlined.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


SMBlined.spr.est

#no data in 2020 - approximate 
approx(SMBlined.spr.est$Year, SMBlined.spr.est$Yspr, xout=2020) # 38.93327
SMBlined.spr.est[SMBlined.spr.est$Year==2020,c(2,3)]<-c(38.93327, 185.13740) #assume var from 2019

SMBlined.spr.est$method<-"spr"
#make one dataframe for simple and spr estimates
names(SMBlined.spr.est) <- c("Year", "SMB.lined", "var.y", "method")
SMB.Lined <- rbind(SMB.lined.Comm.simple [SMB.lined.Comm.simple$Year<2007,], SMBlined.spr.est[,c(1,2,4)])

###
#  ---- Inside VMS Strata ---- 
###

#Simple means for Non-spr years
years <- 1996:surveyyear
X <- length(years)

InVMS.lined.Comm.simple<- data.frame(Year=years, Inner.lined=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SMB.lined.Comm.simple$Year)){
  temp.data <- lined[lined$YEAR==1995+i,]
  InVMS.lined.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
InVMS.lined.Comm.simple

#approx 2020 since no survey in 2020 
approx(InVMS.lined.Comm.simple$Year, InVMS.lined.Comm.simple$Inner.lined, xout=2020) #  59.66684
InVMS.lined.Comm.simple[InVMS.lined.Comm.simple$Year==2020,"Inner.lined"]<-127.8932


#dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
innerlined.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007
INlined1<-spr(lined2006$TOW_NO[lined2006$STRATA_ID==99],apply(lined2006[lined2006$STRATA_ID==99,24:50],1,sum),
lined2007$TOW_NO[lined2007$STRATA_ID==99],apply(lined2007[lined2007$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined1)
innerlined.spr.est [innerlined.spr.est $Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
INlined2<-spr(lined2007$TOW_NO[lined2007$STRATA_ID==99],apply(lined2007[lined2007$STRATA_ID==99,24:50],1,sum),
lined2008$TOW_NO[lined2008$STRATA_ID==99],apply(lined2008[lined2008$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined2,summary (INlined1))
innerlined.spr.est [innerlined.spr.est $Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
INlined3<-spr(lined2008$TOW_NO[lined2008$STRATA_ID==99],apply(lined2008[lined2008$STRATA_ID==99,24:50],1,sum),
lined2009$TOW_NO[lined2009$STRATA_ID==99],apply(lined2009[lined2009$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined3,summary (INlined2,summary (INlined1)))
innerlined.spr.est [innerlined.spr.est $Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
INlined4<-spr(lined2009$TOW_NO[lined2009$STRATA_ID==99],apply(lined2009[lined2009$STRATA_ID==99,24:50],1,sum),
lined2010$TOW_NO[lined2010$STRATA_ID==99],apply(lined2010[lined2010$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined4,summary (INlined3,summary (INlined2,summary (INlined1))))
innerlined.spr.est [innerlined.spr.est $Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
INlined5<-spr(lined2010$TOW_NO[lined2010$STRATA_ID==99],apply(lined2010[lined2010$STRATA_ID==99,24:50],1,sum),
lined2011$TOW_NO[lined2011$STRATA_ID==99],apply(lined2011[lined2011$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined5,summary (INlined4,summary (INlined3,summary (INlined2,summary (INlined1)))))
innerlined.spr.est [innerlined.spr.est $Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
INlined6<-spr(lined2011$TOW_NO[lined2011$STRATA_ID==99],apply(lined2011[lined2011$STRATA_ID==99,24:50],1,sum),
lined2012$TOW_NO[lined2012$STRATA_ID==99],apply(lined2012[lined2012$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined6,summary (INlined5,summary (INlined4,summary (INlined3,summary (INlined2,summary (INlined1))))))  # 20.1
innerlined.spr.est [innerlined.spr.est $Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
INlined7<-spr(lined2012$TOW_NO[lined2012$STRATA_ID==99],apply(lined2012[lined2012$STRATA_ID==99,24:50],1,sum),
lined2013$TOW_NO[lined2013$STRATA_ID==99],apply(lined2013[lined2013$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined7,summary (INlined6,summary (INlined5,summary (INlined4,summary (INlined3,summary (INlined2,summary (INlined1)))))))
innerlined.spr.est [innerlined.spr.est $Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
INlined8<-spr(lined2013$TOW_NO[lined2013$STRATA_ID==99],apply(lined2013[lined2013$STRATA_ID==99,24:50],1,sum),
lined2014$TOW_NO[lined2014$STRATA_ID==99],apply(lined2014[lined2014$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1))))))))
innerlined.spr.est [innerlined.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
INlined9<-spr(lined2014$TOW_NO[lined2014$STRATA_ID==99],apply(lined2014[lined2014$STRATA_ID==99,24:50],1,sum),
lined2015$TOW_NO[lined2015$STRATA_ID==99],apply(lined2015[lined2015$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1)))))))))
innerlined.spr.est [innerlined.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
INlined10<-spr(lined2015$TOW_NO[lined2015$STRATA_ID==99],apply(lined2015[lined2015$STRATA_ID==99,24:50],1,sum),
              lined2016$TOW_NO[lined2016$STRATA_ID==99],apply(lined2016[lined2016$STRATA_ID==99,27:50],1,sum),
              crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1))))))))))
innerlined.spr.est [innerlined.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
INlined11<-spr(lined2016$TOW_NO[lined2016$STRATA_ID==99],apply(lined2016[lined2016$STRATA_ID==99,24:50],1,sum),
              lined2017$TOW_NO[lined2017$STRATA_ID==99],apply(lined2017[lined2017$STRATA_ID==99,27:50],1,sum),
              crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1)))))))))))
innerlined.spr.est [innerlined.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
INlined12<-spr(lined2017$TOW_NO[lined2017$STRATA_ID==99],apply(lined2017[lined2017$STRATA_ID==99,24:50],1,sum),
               lined2018$TOW_NO[lined2018$STRATA_ID==99],apply(lined2018[lined2018$STRATA_ID==99,27:50],1,sum),
               crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1))))))))))))
innerlined.spr.est [innerlined.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
INlined13<-spr(lined2018$TOW_NO[lined2018$STRATA_ID==99],apply(lined2018[lined2018$STRATA_ID==99,24:50],1,sum),
               lined2019$TOW_NO[lined2019$STRATA_ID==99],apply(lined2019[lined2019$STRATA_ID==99,27:50],1,sum),
               crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INlined13, summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1)))))))))))))
innerlined.spr.est [innerlined.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #No survey in 2020 
INlined14<-spr(lined2019$TOW_NO[lined2019$STRATA_ID==99],apply(lined2019[lined2019$STRATA_ID==99,24:50],1,sum),
               lined2021$TOW_NO[lined2021$STRATA_ID==99],apply(lined2021[lined2021$STRATA_ID==99,27:50],1,sum),
               crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INlined14, summary(INlined13, summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1))))))))))))))
innerlined.spr.est [innerlined.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
INlined15 <- spr(lined2021$TOW_NO[lined2021$STRATA_ID==99],apply(lined2021[lined2021$STRATA_ID==99,24:50],1,sum),
               lined2022$TOW_NO[lined2022$STRATA_ID==99],apply(lined2022[lined2022$STRATA_ID==99,27:50],1,sum),
               crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K <- summary(INlined15,summary(INlined14, summary(INlined13, summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1))))))))))))))) 
innerlined.spr.est [innerlined.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
INlined16 <- spr(lined2022$TOW_NO[lined2022$STRATA_ID==99],apply(lined2022[lined2022$STRATA_ID==99,24:50],1,sum),
                 lined2023$TOW_NO[lined2023$STRATA_ID==99],apply(lined2023[lined2023$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INlined16,summary(INlined15,summary(INlined14, summary(INlined13, summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1))))))))))))))))
innerlined.spr.est [innerlined.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
INlined17 <- spr(lined2023$TOW_NO[lined2023$STRATA_ID==99],apply(lined2023[lined2023$STRATA_ID==99,24:50],1,sum),
                 lined2024$TOW_NO[lined2024$STRATA_ID==99],apply(lined2024[lined2024$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INlined17,summary(INlined16,summary(INlined15,summary(INlined14, summary(INlined13, summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1)))))))))))))))))

innerlined.spr.est [innerlined.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
INlined18 <- spr(lined2024$TOW_NO[lined2024$STRATA_ID==99],apply(lined2024[lined2024$STRATA_ID==99,24:50],1,sum),
                 lined2025$TOW_NO[lined2025$STRATA_ID==99],apply(lined2025[lined2025$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INlined18, summary(INlined17,summary(INlined16,summary(INlined15,summary(INlined14, summary(INlined13, summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1))))))))))))))))))

innerlined.spr.est [innerlined.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
INlined19 <- spr(lined2025$TOW_NO[lined2025$STRATA_ID==99],apply(lined2025[lined2025$STRATA_ID==99,24:50],1,sum),
                 lined2026$TOW_NO[lined2026$STRATA_ID==99],apply(lined2026[lined2026$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <- summary(INlined19, summary(INlined18, summary(INlined17,summary(INlined16,summary(INlined15,summary(INlined14, summary(INlined13, summary(INlined12, summary (INlined11,summary(INlined10,summary (INlined9,summary (INlined8,summary(INlined7,summary(INlined6,summary(INlined5,summary(INlined4,summary(INlined3,summary(INlined2,summary (INlined1)))))))))))))))))))

innerlined.spr.est [innerlined.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

innerlined.spr.est

#no data in 2020 - approximate 
approx(innerlined.spr.est$Year, innerlined.spr.est$Yspr, xout=2020) #  124.1184
innerlined.spr.est[innerlined.spr.est$Year==2020,c(2,3)]<-c( 124.1184, 117.20722) #assume var from 2019

#make one dataframe for simple and spr estimates
innerlined.spr.est$method <- "spr"
names(innerlined.spr.est) <- c("Year", "Inner.lined", "var.y", "method")

InVMS.Lined <- rbind(InVMS.lined.Comm.simple [InVMS.lined.Comm.simple$Year<2007,], innerlined.spr.est[,c(1,2,4)])

###  MERGE ALL LINED DATAFRAMES
###

spa3.lined<-merge (SMB.Lined[,c(1,2)],InVMS.Lined[,c(1,2)], by.x="Year")
spa3.lined$Lined.SPA3<-(spa3.lined$SMB.lined*0.3737)+(spa3.lined$Inner.lined*0.6263)

####
###
### ---- UNLINED GEAR ---- 
###
####

###
# ---- St. Mary's Bay ----
###
#simple mean for non-spr years
years <- 1996:surveyyear
X <- length(years)

SMB.unlined.Comm.simple<- data.frame(Year=years, SMB.unlined=rep(NA,X),method=rep("simple",X))
for(i in 1:length(SMB.unlined.Comm.simple$Year)){
  temp.data <- unlined[unlined$YEAR==1995+i,]
  SMB.unlined.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
SMB.unlined.Comm.simple

approx(SMB.unlined.Comm.simple$Year, SMB.unlined.Comm.simple$SMB.unlined, xout=1997) # 28.825
approx(SMB.unlined.Comm.simple$Year, SMB.unlined.Comm.simple$SMB.unlined, xout=1998) # 36.199
approx(SMB.unlined.Comm.simple$Year, SMB.unlined.Comm.simple$SMB.unlined, xout=2002) #  29.993
approx(SMB.unlined.Comm.simple$Year, SMB.unlined.Comm.simple$SMB.unlined, xout=2003) # 23.219
approx(SMB.unlined.Comm.simple$Year, SMB.unlined.Comm.simple$SMB.unlined, xout=2020) # 92.66965

SMB.unlined.Comm.simple[SMB.unlined.Comm.simple$Year==1997,"SMB.unlined"]<-28.825
SMB.unlined.Comm.simple[SMB.unlined.Comm.simple$Year==1998,"SMB.unlined"]<-36.199
SMB.unlined.Comm.simple[SMB.unlined.Comm.simple$Year==2002,"SMB.unlined"]<-29.9931
SMB.unlined.Comm.simple[SMB.unlined.Comm.simple$Year==2003,"SMB.unlined"]<-23.219
SMB.unlined.Comm.simple[SMB.unlined.Comm.simple$Year==2020,"SMB.unlined"]<-92.66965

## #dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
SMBUNlined.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007
SMBun1<-spr(unlined2006$TOW_NO[unlined2006$STRATA_ID==22],apply(unlined2006[unlined2006$STRATA_ID==22,24:50],1,sum),
unlined2007$TOW_NO[unlined2007$STRATA_ID==22],apply(unlined2007[unlined2007$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun1)
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
SMBun2<-spr(unlined2007$TOW_NO[unlined2007$STRATA_ID==22],apply(unlined2007[unlined2007$STRATA_ID==22,24:50],1,sum),
unlined2008$TOW_NO[unlined2008$STRATA_ID==22],apply(unlined2008[unlined2008$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun2,summary (SMBun1))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
SMBun3<-spr(unlined2008$TOW_NO[unlined2008$STRATA_ID==22],apply(unlined2008[unlined2008$STRATA_ID==22,24:50],1,sum),
unlined2009$TOW_NO[unlined2009$STRATA_ID==22],apply(unlined2009[unlined2009$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun3,summary (SMBun2,summary (SMBun1)))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2009/2010
SMBun4<-spr(unlined2009$TOW_NO[unlined2009$STRATA_ID==22],apply(unlined2009[unlined2009$STRATA_ID==22,24:50],1,sum),
unlined2010$TOW_NO[unlined2010$STRATA_ID==22],apply(unlined2010[unlined2010$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun4,summary (SMBun3,summary (SMBun2,summary (SMBun1))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
SMBun5<-spr(unlined2010$TOW_NO[unlined2010$STRATA_ID==22],apply(unlined2010[unlined2010$STRATA_ID==22,24:50],1,sum),
unlined2011$TOW_NO[unlined2011$STRATA_ID==22],apply(unlined2011[unlined2011$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun5,summary (SMBun4,summary (SMBun3,summary (SMBun2,summary (SMBun1)))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
SMBun6<-spr(unlined2011$TOW_NO[unlined2011$STRATA_ID==22],apply(unlined2011[unlined2011$STRATA_ID==22,24:50],1,sum),
unlined2012$TOW_NO[unlined2012$STRATA_ID==22],apply(unlined2012[unlined2012$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun6,summary (SMBun5,summary (SMBun4,summary (SMBun3,summary (SMBun2,summary (SMBun1))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
SMBun7<-spr(unlined2012$TOW_NO[unlined2012$STRATA_ID==22],apply(unlined2012[unlined2012$STRATA_ID==22,24:50],1,sum),
unlined2013$TOW_NO[unlined2013$STRATA_ID==22],apply(unlined2013[unlined2013$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun7,summary (SMBun6,summary (SMBun5,summary (SMBun4,summary (SMBun3,summary (SMBun2,summary (SMBun1)))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
SMBun8<-spr(unlined2013$TOW_NO[unlined2013$STRATA_ID==22],apply(unlined2013[unlined2013$STRATA_ID==22,24:50],1,sum),
unlined2014$TOW_NO[unlined2014$STRATA_ID==22],apply(unlined2014[unlined2014$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
SMBun9<-spr(unlined2014$TOW_NO[unlined2014$STRATA_ID==22],apply(unlined2014[unlined2014$STRATA_ID==22,24:50],1,sum),
unlined2015$TOW_NO[unlined2015$STRATA_ID==22],apply(unlined2015[unlined2015$STRATA_ID==22,27:50],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1)))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
SMBun10<-spr(unlined2015$TOW_NO[unlined2015$STRATA_ID==22],apply(unlined2015[unlined2015$STRATA_ID==22,24:50],1,sum),
            unlined2016$TOW_NO[unlined2016$STRATA_ID==22],apply(unlined2016[unlined2016$STRATA_ID==22,27:50],1,sum),
            crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1))))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
SMBun11<-spr(unlined2016$TOW_NO[unlined2016$STRATA_ID==22],apply(unlined2016[unlined2016$STRATA_ID==22,24:50],1,sum),
            unlined2017$TOW_NO[unlined2017$STRATA_ID==22],apply(unlined2017[unlined2017$STRATA_ID==22,27:50],1,sum),
            crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1)))))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
SMBun12<-spr(unlined2017$TOW_NO[unlined2017$STRATA_ID==22],apply(unlined2017[unlined2017$STRATA_ID==22,24:50],1,sum),
             unlined2018$TOW_NO[unlined2018$STRATA_ID==22],apply(unlined2018[unlined2018$STRATA_ID==22,27:50],1,sum),
             crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1))))))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
SMBun13<-spr(unlined2018$TOW_NO[unlined2018$STRATA_ID==22],apply(unlined2018[unlined2018$STRATA_ID==22,24:50],1,sum),
             unlined2019$TOW_NO[unlined2019$STRATA_ID==22],apply(unlined2019[unlined2019$STRATA_ID==22,27:50],1,sum),
             crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary(SMBun13, summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1)))))))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020 
SMBun14<-spr(unlined2019$TOW_NO[unlined2019$STRATA_ID==22],apply(unlined2019[unlined2019$STRATA_ID==22,24:50],1,sum),
             unlined2021$TOW_NO[unlined2021$STRATA_ID==22],apply(unlined2021[unlined2021$STRATA_ID==22,27:50],1,sum),
             crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K<-summary(SMBun14,summary(SMBun13, summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1))))))))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
SMBun15 <- spr(unlined2021$TOW_NO[unlined2021$STRATA_ID==22],apply(unlined2021[unlined2021$STRATA_ID==22,24:50],1,sum),
             unlined2022$TOW_NO[unlined2022$STRATA_ID==22],apply(unlined2022[unlined2022$STRATA_ID==22,27:50],1,sum),
             crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <-  summary(SMBun15,summary(SMBun14,summary(SMBun13, summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1))))))))))))))) 
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
SMBun16 <- spr(unlined2022$TOW_NO[unlined2022$STRATA_ID==22],apply(unlined2022[unlined2022$STRATA_ID==22,24:50],1,sum),
               unlined2023$TOW_NO[unlined2023$STRATA_ID==22],apply(unlined2023[unlined2023$STRATA_ID==22,27:50],1,sum),
               crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])
K <-  summary(SMBun16,summary(SMBun15,summary(SMBun14,summary(SMBun13, summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1))))))))))))))))
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
SMBun17 <- spr(unlined2023$TOW_NO[unlined2023$STRATA_ID==22],apply(unlined2023[unlined2023$STRATA_ID==22,24:50],1,sum),
               unlined2024$TOW_NO[unlined2024$STRATA_ID==22],apply(unlined2024[unlined2024$STRATA_ID==22,27:50],1,sum),
               crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K <-  summary(SMBun17,summary(SMBun16,summary(SMBun15,summary(SMBun14,summary(SMBun13, summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1)))))))))))))))))

SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
SMBun18 <- spr(unlined2024$TOW_NO[unlined2024$STRATA_ID==22],apply(unlined2024[unlined2024$STRATA_ID==22,24:50],1,sum),
               unlined2025$TOW_NO[unlined2025$STRATA_ID==22],apply(unlined2025[unlined2025$STRATA_ID==22,27:50],1,sum),
               crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K <-  summary(SMBun18,summary(SMBun17,summary(SMBun16,summary(SMBun15,summary(SMBun14,summary(SMBun13, summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1))))))))))))))))))

SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
SMBun19 <- spr(unlined2025$TOW_NO[unlined2025$STRATA_ID==22],apply(unlined2025[unlined2025$STRATA_ID==22,24:50],1,sum),
               unlined2026$TOW_NO[unlined2026$STRATA_ID==22],apply(unlined2026[unlined2026$STRATA_ID==22,27:50],1,sum),
               crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==22,c("TOW_NO_REF","TOW_NO")])

K <-  summary(SMBun19,summary(SMBun18,summary(SMBun17,summary(SMBun16,summary(SMBun15,summary(SMBun14,summary(SMBun13, summary(SMBun12, summary (SMBun11,summary(SMBun10,summary (SMBun9,summary (SMBun8,summary(SMBun7,summary(SMBun6,summary(SMBun5,summary(SMBun4,summary(SMBun3,summary(SMBun2,summary(SMBun1)))))))))))))))))))

SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

SMBUNlined.spr.est

#no data in 2020 - approximate 
approx(SMBUNlined.spr.est$Year, SMBUNlined.spr.est$Yspr, xout=2020) #  55.36452
SMBUNlined.spr.est[SMBUNlined.spr.est$Year==2020,c(2,3)]<-c( 55.36452, 308.59867) #assume var from 2019

SMBUNlined.spr.est$method<-"spr"
names(SMBUNlined.spr.est) <- c("Year", "SMB.unlined", "var.y", "method")
SMB.Unlined <- rbind(SMB.unlined.Comm.simple [SMB.unlined.Comm.simple$Year<2007,], SMBUNlined.spr.est[,c(1,2,4)])

###
#  ---- Inside VMS Strata ---- 
####

#Simple means for Non-spr years
years <- 1996:surveyyear
X <- length(years)

InVMS.unlined.Comm.simple<- data.frame(Year=years, Inner.unlined=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SMB.unlined.Comm.simple$Year)){
  temp.data <- unlined[unlined$YEAR==1995+i,]
  InVMS.unlined.Comm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}
InVMS.unlined.Comm.simple

#no data in 2020 - approximate 
approx(InVMS.unlined.Comm.simple$Year, InVMS.unlined.Comm.simple$Inner.unlined, xout=2020) #  170.7841
InVMS.unlined.Comm.simple[InVMS.unlined.Comm.simple$Year==2020,"Inner.unlined"]<-c( 170.7841) #assume var from 2019

#dataframe for SPR estimates Commercial
spryears <- 2007:surveyyear #update to most recent year
Y <- length(spryears)
InnerUnlined.spr.est <- data.frame(Year=spryears, Yspr=rep(NA,Y), var.Yspr.corrected=rep(NA,Y))

#2006/2007
INunlined1<-spr(unlined2006$TOW_NO[unlined2006$STRATA_ID==99],apply(unlined2006[unlined2006$STRATA_ID==99,24:50],1,sum),
unlined2007$TOW_NO[unlined2007$STRATA_ID==99],apply(unlined2007[unlined2007$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2007[crossref.spa3.2007$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INunlined1)
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2007,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2007/2008
INunlined2<-spr(unlined2007$TOW_NO[unlined2007$STRATA_ID==99],apply(unlined2007[unlined2007$STRATA_ID==99,24:50],1,sum),
unlined2008$TOW_NO[unlined2008$STRATA_ID==99],apply(unlined2008[unlined2008$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2008[crossref.spa3.2008$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined2,summary(INunlined1))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2008,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2008/2009
INunlined3<-spr(unlined2008$TOW_NO[unlined2008$STRATA_ID==99],apply(unlined2008[unlined2008$STRATA_ID==99,24:50],1,sum),
unlined2009$TOW_NO[unlined2009$STRATA_ID==99],apply(unlined2009[unlined2009$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2009[crossref.spa3.2009$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined3,summary(INunlined2,summary(INunlined1)))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
INunlined4<-spr(unlined2009$TOW_NO[unlined2009$STRATA_ID==99],apply(unlined2009[unlined2009$STRATA_ID==99,24:50],1,sum),
unlined2010$TOW_NO[unlined2010$STRATA_ID==99],apply(unlined2010[unlined2010$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2010[crossref.spa3.2010$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
INunlined5<-spr(unlined2010$TOW_NO[unlined2010$STRATA_ID==99],apply(unlined2010[unlined2010$STRATA_ID==99,24:50],1,sum),
unlined2011$TOW_NO[unlined2011$STRATA_ID==99],apply(unlined2011[unlined2011$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2011[crossref.spa3.2011$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1)))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
INunlined6<-spr(unlined2011$TOW_NO[unlined2011$STRATA_ID==99],apply(unlined2011[unlined2011$STRATA_ID==99,24:50],1,sum),
unlined2012$TOW_NO[unlined2012$STRATA_ID==99],apply(unlined2012[unlined2012$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2012[crossref.spa3.2012$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
INunlined7<-spr(unlined2012$TOW_NO[unlined2012$STRATA_ID==99],apply(unlined2012[unlined2012$STRATA_ID==99,24:50],1,sum),
unlined2013$TOW_NO[unlined2013$STRATA_ID==99],apply(unlined2013[unlined2013$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2013[crossref.spa3.2013$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1)))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
INunlined8<-spr(unlined2013$TOW_NO[unlined2013$STRATA_ID==99],apply(unlined2013[unlined2013$STRATA_ID==99,24:50],1,sum),
unlined2014$TOW_NO[unlined2014$STRATA_ID==99],apply(unlined2014[unlined2014$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2014[crossref.spa3.2014$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
INunlined9<-spr(unlined2014$TOW_NO[unlined2014$STRATA_ID==99],apply(unlined2014[unlined2014$STRATA_ID==99,24:50],1,sum),
unlined2015$TOW_NO[unlined2015$STRATA_ID==99],apply(unlined2015[unlined2015$STRATA_ID==99,27:50],1,sum),
crossref.spa3.2015[crossref.spa3.2015$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1)))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
INunlined10<-spr(unlined2015$TOW_NO[unlined2015$STRATA_ID==99],apply(unlined2015[unlined2015$STRATA_ID==99,24:50],1,sum),
                unlined2016$TOW_NO[unlined2016$STRATA_ID==99],apply(unlined2016[unlined2016$STRATA_ID==99,27:50],1,sum),
                crossref.spa3.2016[crossref.spa3.2016$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
INunlined11<-spr(unlined2016$TOW_NO[unlined2016$STRATA_ID==99],apply(unlined2016[unlined2016$STRATA_ID==99,24:50],1,sum),
                unlined2017$TOW_NO[unlined2017$STRATA_ID==99],apply(unlined2017[unlined2017$STRATA_ID==99,27:50],1,sum),
                crossref.spa3.2017[crossref.spa3.2017$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1)))))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
INunlined12<-spr(unlined2017$TOW_NO[unlined2017$STRATA_ID==99],apply(unlined2017[unlined2017$STRATA_ID==99,24:50],1,sum),
                 unlined2018$TOW_NO[unlined2018$STRATA_ID==99],apply(unlined2018[unlined2018$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2018[crossref.spa3.2018$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
INunlined13<-spr(unlined2018$TOW_NO[unlined2018$STRATA_ID==99],apply(unlined2018[unlined2018$STRATA_ID==99,24:50],1,sum),
                 unlined2019$TOW_NO[unlined2019$STRATA_ID==99],apply(unlined2019[unlined2019$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2019[crossref.spa3.2019$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined13, summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1)))))))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2020 #No survey in 2020 
INunlined14<-spr(unlined2019$TOW_NO[unlined2019$STRATA_ID==99],apply(unlined2019[unlined2019$STRATA_ID==99,24:50],1,sum),
                 unlined2021$TOW_NO[unlined2021$STRATA_ID==99],apply(unlined2021[unlined2021$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2021[crossref.spa3.2021$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K<-summary(INunlined14, summary(INunlined13, summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
INunlined15 <- spr(unlined2021$TOW_NO[unlined2021$STRATA_ID==99],apply(unlined2021[unlined2021$STRATA_ID==99,24:50],1,sum),
                 unlined2022$TOW_NO[unlined2022$STRATA_ID==99],apply(unlined2022[unlined2022$STRATA_ID==99,27:50],1,sum),
                 crossref.spa3.2022[crossref.spa3.2022$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K <-  summary(INunlined15,summary(INunlined14, summary(INunlined13, summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))))))))))) 
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
INunlined16 <- spr(unlined2022$TOW_NO[unlined2022$STRATA_ID==99],apply(unlined2022[unlined2022$STRATA_ID==99,24:50],1,sum),
                   unlined2023$TOW_NO[unlined2023$STRATA_ID==99],apply(unlined2023[unlined2023$STRATA_ID==99,27:50],1,sum),
                   crossref.spa3.2023[crossref.spa3.2023$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])
K <-  summary(INunlined16,summary(INunlined15,summary(INunlined14, summary(INunlined13, summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))))))))))))
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
INunlined17 <- spr(unlined2023$TOW_NO[unlined2023$STRATA_ID==99],apply(unlined2023[unlined2023$STRATA_ID==99,24:50],1,sum),
                   unlined2024$TOW_NO[unlined2024$STRATA_ID==99],apply(unlined2024[unlined2024$STRATA_ID==99,27:50],1,sum),
                   crossref.spa3.2024[crossref.spa3.2024$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <-  summary(INunlined17,summary(INunlined16,summary(INunlined15,summary(INunlined14, summary(INunlined13, summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1)))))))))))))))))

InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
INunlined18 <- spr(unlined2024$TOW_NO[unlined2024$STRATA_ID==99],apply(unlined2024[unlined2024$STRATA_ID==99,24:50],1,sum),
                   unlined2025$TOW_NO[unlined2025$STRATA_ID==99],apply(unlined2025[unlined2025$STRATA_ID==99,27:50],1,sum),
                   crossref.spa3.2025[crossref.spa3.2025$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <-   summary(INunlined18,summary(INunlined17,summary(INunlined16,summary(INunlined15,summary(INunlined14, summary(INunlined13, summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1))))))))))))))))))

InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
INunlined19 <- spr(unlined2025$TOW_NO[unlined2025$STRATA_ID==99],apply(unlined2025[unlined2025$STRATA_ID==99,24:50],1,sum),
                   unlined2026$TOW_NO[unlined2026$STRATA_ID==99],apply(unlined2026[unlined2026$STRATA_ID==99,27:50],1,sum),
                   crossref.spa3.2026[crossref.spa3.2026$STRATA_ID==99,c("TOW_NO_REF","TOW_NO")])

K <-   summary(INunlined19, summary(INunlined18,summary(INunlined17,summary(INunlined16,summary(INunlined15,summary(INunlined14, summary(INunlined13, summary(INunlined12, summary (INunlined11, summary(INunlined10, summary (INunlined9,summary(INunlined8,summary (INunlined7,summary(INunlined6,summary(INunlined5,summary (INunlined4,summary(INunlined3,summary(INunlined2,summary(INunlined1)))))))))))))))))))

InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

InnerUnlined.spr.est

#no data in 2020 - approximate 
approx(InnerUnlined.spr.est$Year, InnerUnlined.spr.est$Yspr, xout=2020) #  164.2289
InnerUnlined.spr.est[InnerUnlined.spr.est$Year==2020,c(2,3)]<-c( 164.2289, 206.78702) #assume var from 2019

InnerUnlined.spr.est$method<-"spr"
names(InnerUnlined.spr.est) <- c("Year", "Inner.unlined", "var.y", "method")
InVMS.Unlined <- rbind(InVMS.unlined.Comm.simple[InVMS.unlined.Comm.simple$Year<2007,], InnerUnlined.spr.est[,c("Year", "Inner.unlined", "method")])

###  MERGE ALL UNLINED DATAFRAMES
###

spa3.unlined <- merge(SMB.Unlined[,c("Year", "SMB.unlined")],InVMS.Unlined[,c("Year","Inner.unlined")], by.x="Year")
spa3.unlined$Unlined.SPA3 <- (spa3.unlined$SMB.unlined*0.3737)+(spa3.unlined$Inner.unlined*0.6263)

####
###
### ---- CALCUALTE RATIO ----
###

#merge for ratio
ratio <- merge(spa3.lined[,c("Year","Lined.SPA3")], spa3.unlined[,c("Year","Unlined.SPA3")],by.x="Year")
ratio$ratiolined <- ratio$Lined.SPA3/ratio$Unlined.SPA3
names(ratio) <- c("Year","Lined","Unlined","ratiolined")

# Output this ratio
# Create a new Ratio file, the most recent year will be added to the model using the SPA4_ModelFile.R
#write.csv(ratio,paste0("Y:/INSHORE SCALLOP/BoF/dataoutput/SPA3_ratioLinedtoUnlined",max(ratio$Year),".csv"))
write.csv(ratio, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3_ratioLinedtoUnlined",surveyyear,".csv"))

# Now save the entire R data file as well so you have a record of what you did, first clean up the data a bit
#rm(i,db.con,pwd.ID,un.ID,X)
#save.image(paste0("Y:/INSHORE SCALLOP/BoF/dataoutput/R_data_files/SPA3_ratioLinedtoUnlined",max(ratio$Year),".RData"))


###
### ---- Plots ----
###

#Survey Ratio per year 
png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA",area,"_ratio",surveyyear,".png"),width=11,height=8,units = "in",res=920)

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

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA",area,"_lined.vs.unlined",surveyyear,".png"),width=11,height=8,units = "in",res=920)

ggplot(data = unlined.lined.for.plot, aes(x=Year, y=Value, col=Gear, pch=Gear)) + 
  geom_point() + 
  geom_line(aes(linetype = Gear)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.05, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))

dev.off() 




