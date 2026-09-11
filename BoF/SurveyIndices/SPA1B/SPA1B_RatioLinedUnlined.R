###........................................###
###
###    SPA 1B
###    Ratio Lined/Unlined
###
###   Rehauled July 2020 J.Sameoto
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(PEDstrata) #v.1.0.2
library(spr) #version 1.04
library(PBSmapping)
library(tidyverse)
library(ggplot2)

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

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#SQL - numbers by shell height bin LINED GEAR
#numbers by shell height bin in the lined drags;!NB! this code will exclude cruises that shouldn't be used: UB2000,UB1998,UB1999,UB1999b,JJ2004
lined.query <-  "SELECT *
                 FROM scallsur.sclinedlive_std
                 WHERE strata_id IN (35, 37, 38, 49, 51, 52, 53)
                 AND (cruise LIKE 'BA%'
                 OR cruise LIKE 'BF%'
                 OR cruise LIKE 'BI%'
                 OR cruise LIKE 'GM%'
                 OR cruise LIKE 'RF%')"


# Select data from database; execute query with ROracle; numbers by shell height bin LINED drags
lined <- dbGetQuery(chan, lined.query)

#SQL - numbers by shell height bin UNLINED GEAR
#numbers by shell height bin in the unlined drags;!NB! this code will exclude cruises that shouldn't be used: UB2000,UB1998,UB1999,UB1999b,JJ2004
unlined.query <-  "SELECT *
                   FROM scallsur.scunlinedlive_std
                   WHERE strata_id IN (35, 37, 38, 49, 51, 52, 53)
                   AND (cruise LIKE 'BA%'
                   OR cruise LIKE 'BF%'
                   OR cruise LIKE 'BI%'
                   OR cruise LIKE 'GM%'
                   OR cruise LIKE 'RF%')"

# Select data from database; execute query with ROracle; numbers by shell height bin UNLINED drags
unlined <- dbGetQuery(chan,unlined.query)

#add YEAR column to data
lined$YEAR <- as.numeric(substr(lined$CRUISE,3,6))
unlined$YEAR <- as.numeric(substr(unlined$CRUISE,3,6))

###
# ---- Post-stratify MBN for East/West line ----
###

# for Midbay North, need to assign strata to East/West
SPA1B.MBN.E <- data.frame(PID = 58,POS = 1:4,X = c(-65.710, -65.586, -65.197, -65.264),
                          Y = c(45.280, 45.076, 45.237, 45.459)) #New strata

lined$lat <- convert.dd.dddd(lined$START_LAT) #format lat and lon
lined$lon <- convert.dd.dddd(lined$START_LONG)
lined$ID <- 1:nrow(lined)

unlined$lat <- convert.dd.dddd(unlined$START_LAT) #format lat and lon
unlined$lon <- convert.dd.dddd(unlined$START_LONG)
unlined$ID <- 1:nrow(unlined)

#assign strata id "58" to Midbay North East   (strata_id 38 to MBN West)
events = subset(lined,STRATA_ID  ==  38,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
lined$STRATA_ID[lined$ID %in% findPolys(events,SPA1B.MBN.E)$EID] <- 58

events = subset(unlined,STRATA_ID  ==  38,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
unlined$STRATA_ID[unlined$ID %in% findPolys(events,SPA1B.MBN.E)$EID] <- 58

###
# ---- Read in cross reference files for spr ----
###

crossref.query <-  "SELECT *
                 FROM scallsur.sccomparisontows
                 WHERE sccomparisontows.cruise LIKE 'BF%'
                 AND COMP_TYPE= 'R'"

crossref.BoF <- dbGetQuery(chan,crossref.query)

dbDisconnect(chan)

crossref.BoF$CruiseID <- paste(crossref.BoF$CRUISE_REF,crossref.BoF$TOW_NO_REF,sep = '.')  #create CRUISE_ID on "parent/reference" tow
lined$CruiseID <- paste(lined$CRUISE,lined$TOW_NO,sep = '.')  #create CRUISE_ID on "parent/reference" tow

#merge STRATA_ID from lined to the crosssref files based on parent/reference tow
crossref.BoF <- merge(crossref.BoF, subset(lined, select = c("STRATA_ID", "CruiseID")), by.x = "CruiseID", all = FALSE)


# ---- correct data-sets for errors ----
#-some repearted tows will not be assigned to the same strata as the "parent" tow due to where the START LAT and LONG are
#-repeated tows should be corrected to match parent tow
#-some experimental tows were used as parent tows for repeats, these should be removed
#-these errors are identified by sleuthing after the spr function won't run

#1. In 2012, tow 323 assinged to 58, but parent tow in 38
lined[lined$CRUISE == "BF2012" & lined$TOW_NO == 323,]
lined$STRATA_ID[lined$CRUISE == "BF2012" & lined$TOW_NO == 323] <- "38"

unlined[unlined$CRUISE == "BF2012" & unlined$TOW_NO == 323,]
unlined$STRATA_ID[unlined$CRUISE == "BF2012" & unlined$TOW_NO == 323] <- "38"

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
lined[lined$CRUISE == "BF2026" & lined$TOW_NO == 126,]
lined$STRATA_ID[lined$CRUISE == "BF2026" & lined$TOW_NO == 126] <- "58"

unlined[unlined$CRUISE == "BF2026" & unlined$TOW_NO == 126,]
unlined$STRATA_ID[unlined$CRUISE == "BF2026" & unlined$TOW_NO == 126] <- "58"


## ---- Subset data for individual years for SPR command (ONLY TOW_TYPE==1 AND  ONLY TOW_TYPE==5) ----
tows <- c(1,5)
#lined 
year <- c(seq(2008,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(lined, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("lined", i), sub)
}


#unlined  
year <- c(seq(2008,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(unlined, YEAR==i & TOW_TYPE_ID%in%tows)
  assign(paste0("unlined", i), sub)
}

#Crossref 
year <- c(seq(2009,2019),seq(2021,surveyyear)) 
for(i in unique(year)){
  sub <- subset(crossref.BoF, CRUISE==paste0("BF",i))
  assign(paste0("crossref.BoF.", i), sub)
}


#Note: checking for mismatches between parent and child tows for repeats in a pain in the a$$... 	



###
### ---- LINED GEAR ----
###
#Use Commercial size only
#model runs from 1997+

###
# --- 1. Middle Bay North (spr) ----
###

#1a. Middle Bay North - EAST

#simple means estimates for pre-SPR years (Prop for East =0.732)
years <- 1997:surveyyear
X <- length(years)

MBNE.lined.com.simple <-  data.frame(Year = years, lined.MBNE = rep(NA,X))
for (i in 1:length(MBNE.lined.com.simple$Year)) {
  temp.data <- lined[lined$YEAR  ==  1996 + i,]
    MBNE.lined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID  ==  58 & temp.data$TOW_TYPE_ID  ==  1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))],1,sum))
  }
MBNE.lined.com.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNE.lined.com.simple$Year, MBNE.lined.com.simple$lined.MBNE, xout=2020) #  135.1377
MBNE.lined.com.simple[MBNE.lined.com.simple$Year==2020,c("lined.MBNE")] <- 135.1377 


#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear 
Y <- length(spryears)
MBNE.lined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009
#no tows in 2009

#2009/2010
mbEcom1<-spr(lined2009$TOW_NO[lined2009$STRATA_ID  ==  58],apply(lined2009[lined2009$STRATA_ID  ==  58,24:50],1,sum),
    lined2010$TOW_NO[lined2010$STRATA_ID  ==  58],apply(lined2010[lined2010$STRATA_ID  ==  58,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID  ==  58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom1)
MBNE.lined.spr[MBNE.lined.spr$Year  ==  2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
mbEcom2<-spr(lined2010$TOW_NO[lined2010$STRATA_ID  ==  58],apply(lined2010[lined2010$STRATA_ID  ==  58,24:50],1,sum),
    lined2011$TOW_NO[lined2011$STRATA_ID  ==  58],apply(lined2011[lined2011$STRATA_ID  ==  58,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID  ==  58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom2, summary(mbEcom2))
MBNE.lined.spr[MBNE.lined.spr$Year  ==  2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
mbEcom3<-spr(lined2011$TOW_NO[lined2011$STRATA_ID  ==  58],apply(lined2011[lined2011$STRATA_ID == 58,24:50],1,sum),
    lined2012$TOW_NO[lined2012$STRATA_ID == 58],apply(lined2012[lined2012$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom3, summary(mbEcom2, summary(mbEcom2)))
MBNE.lined.spr[MBNE.lined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
mbEcom4<-spr(lined2012$TOW_NO[lined2012$STRATA_ID == 58],apply(lined2012[lined2012$STRATA_ID == 58,24:50],1,sum),
    lined2013$TOW_NO[lined2013$STRATA_ID == 58],apply(lined2013[lined2013$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
mbEcom5<-spr(lined2013$TOW_NO[lined2013$STRATA_ID == 58],apply(lined2013[lined2013$STRATA_ID == 58,24:50],1,sum),
    lined2014$TOW_NO[lined2014$STRATA_ID == 58],apply(lined2014[lined2014$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2)))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
mbEcom6<-spr(lined2014$TOW_NO[lined2014$STRATA_ID == 58],apply(lined2014[lined2014$STRATA_ID == 58,24:50],1,sum),
    lined2015$TOW_NO[lined2015$STRATA_ID == 58],apply(lined2015[lined2015$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
mbEcom7<-spr(lined2015$TOW_NO[lined2015$STRATA_ID == 58],apply(lined2015[lined2015$STRATA_ID == 58,24:50],1,sum),
             lined2016$TOW_NO[lined2016$STRATA_ID == 58],apply(lined2016[lined2016$STRATA_ID == 58,27:50],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2)))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
mbEcom8<-spr(lined2016$TOW_NO[lined2016$STRATA_ID == 58],apply(lined2016[lined2016$STRATA_ID == 58,24:50],1,sum),
             lined2017$TOW_NO[lined2017$STRATA_ID == 58],apply(lined2017[lined2017$STRATA_ID == 58,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
mbEcom9 <- spr(lined2017$TOW_NO[lined2017$STRATA_ID == 58],apply(lined2017[lined2017$STRATA_ID == 58,24:50],1,sum),
               lined2018$TOW_NO[lined2018$STRATA_ID == 58],apply(lined2018[lined2018$STRATA_ID == 58,27:50],1,sum),
            crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2)))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
mbEcom10 <- spr(lined2018$TOW_NO[lined2018$STRATA_ID == 58],apply(lined2018[lined2018$STRATA_ID == 58,24:50],1,sum),
                lined2019$TOW_NO[lined2019$STRATA_ID == 58],apply(lined2019[lined2019$STRATA_ID == 58,27:50],1,sum),
               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2))))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #No survey in 2020 ## TOW 211 in BF2011 is in cross ref table but it not lined2019 data.. so can't run SPR --was it tow type 3? - need to remove it's match from SPR table to run which is BF2021.223
mbEcom11 <- spr(lined2019$TOW_NO[lined2019$STRATA_ID == 58],apply(lined2019[lined2019$STRATA_ID == 58,24:50],1,sum),
                lined2021$TOW_NO[lined2021$STRATA_ID == 58],apply(lined2021[lined2021$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2)))))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2021/2022 
mbEcom12 <- spr(lined2021$TOW_NO[lined2021$STRATA_ID == 58],apply(lined2021[lined2021$STRATA_ID == 58,24:50],1,sum),
                lined2022$TOW_NO[lined2022$STRATA_ID == 58],apply(lined2022[lined2022$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2))))))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
mbEcom13 <- spr(lined2022$TOW_NO[lined2022$STRATA_ID == 58],apply(lined2022[lined2022$STRATA_ID == 58,24:50],1,sum),
                lined2023$TOW_NO[lined2023$STRATA_ID == 58],apply(lined2023[lined2023$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2)))))))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
mbEcom14 <- spr(lined2023$TOW_NO[lined2023$STRATA_ID == 58],apply(lined2023[lined2023$STRATA_ID == 58,24:50],1,sum),
                lined2024$TOW_NO[lined2024$STRATA_ID == 58],apply(lined2024[lined2024$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom14,summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2))))))))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
mbEcom15 <- spr(lined2024$TOW_NO[lined2024$STRATA_ID == 58],apply(lined2024[lined2024$STRATA_ID == 58,24:50],1,sum),
                lined2025$TOW_NO[lined2025$STRATA_ID == 58],apply(lined2025[lined2025$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom15,summary(mbEcom14,summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2)))))))))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 
mbEcom16 <- spr(lined2025$TOW_NO[lined2025$STRATA_ID == 58],apply(lined2025[lined2025$STRATA_ID == 58,24:50],1,sum),
                lined2026$TOW_NO[lined2026$STRATA_ID == 58],apply(lined2026[lined2026$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom16,summary(mbEcom15,summary(mbEcom14,summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom2))))))))))))))))
MBNE.lined.spr[MBNE.lined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNE.lined.spr

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNE.lined.spr$Year, MBNE.lined.spr$Yspr, xout=2020) #  98.53342
MBNE.lined.spr[MBNE.lined.spr$Year==2020,c("Yspr", "var.Yspr.corrected")] <- c(98.53342, 519.81379) #assume var from 2019


#make one dataframe for simple and spr estimates
names(MBNE.lined.spr) <- c("Year", "lined.MBNE", "var.y")
MBNE.Lined <- rbind(MBNE.lined.com.simple[MBNE.lined.com.simple$Year < 2010,], MBNE.lined.spr[,c(1,2)])

#1a. Middle Bay North - WEST

#simple means estimates for pre-SPR years (Prop for East =0.732)
years <- 1997:surveyyear
X <- length(years)

MBNW.lined.com.simple <- data.frame(Year = years, lined.MBNW = rep(NA,X))
for (i in 1:length(MBNW.lined.com.simple$Year)) {
  temp.data <- lined[lined$YEAR == 1996 + i,]
    MBNW.lined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))
],1,sum))
  }
MBNW.lined.com.simple

#fill in missing years, assume 1997-2002 as MBNE, interpolate 2004
MBNW.lined.com.simple[MBNW.lined.com.simple$Year == 1997,"lined.MBNW"] <- 28.53333
MBNW.lined.com.simple[MBNW.lined.com.simple$Year == 1998,"lined.MBNW"] <- 23.11667
MBNW.lined.com.simple[MBNW.lined.com.simple$Year == 1999,"lined.MBNW"] <- 27.05000
MBNW.lined.com.simple[MBNW.lined.com.simple$Year == 2000,"lined.MBNW"] <- 30.54167
MBNW.lined.com.simple[MBNW.lined.com.simple$Year == 2001,"lined.MBNW"] <- 49.25385
MBNW.lined.com.simple[MBNW.lined.com.simple$Year == 2002,"lined.MBNW"] <- 27.50000
#2004
approx(MBNW.lined.com.simple$Year, MBNW.lined.com.simple$lined.MBNW, xout = 2004) # 36.04485; No tows in W
MBNW.lined.com.simple[MBNW.lined.com.simple$Year == 2004,"lined.MBNW"] <- 36.04485
#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNW.lined.com.simple$Year, MBNW.lined.com.simple$lined.MBNW, xout=2020) #  130.7764
MBNW.lined.com.simple[MBNW.lined.com.simple$Year==2020,c("lined.MBNW")] <- 130.7764 



#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
MBNW.lined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009
mbWcom<-spr(lined2008$TOW_NO[lined2008$STRATA_ID == 38],apply(lined2008[lined2008$STRATA_ID == 38,24:50],1,sum),
    lined2009$TOW_NO[lined2009$STRATA_ID == 38],apply(lined2009[lined2009$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom)

MBNW.lined.spr[MBNW.lined.spr$Year == 2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)
#2009/2010
mbWcom1<-spr(lined2009$TOW_NO[lined2009$STRATA_ID == 38],apply(lined2009[lined2009$STRATA_ID == 38,24:50],1,sum),
    lined2010$TOW_NO[lined2010$STRATA_ID == 38],apply(lined2010[lined2010$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom1, summary(mbWcom))

MBNW.lined.spr[MBNW.lined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
#mbWcom2<-spr(lined2010$TOW_NO[lined2010$STRATA_ID == 38],apply(lined2010[lined2010$STRATA_ID == 38,24:50],1,sum),
#    lined2011$TOW_NO[lined2011$STRATA_ID == 38],apply(lined2011[lined2011$STRATA_ID == 38,27:50],1,sum),
#    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K<-summary(mbWcom2)
#MBNW.lined.spr[MBNW.lined.spr$Year == 2011,c(2:3)] <- c(82.491,NA)

#2011/2012
mbWcom3<-spr(lined2011$TOW_NO[lined2011$STRATA_ID == 38],apply(lined2011[lined2011$STRATA_ID == 38,24:50],1,sum),
    lined2012$TOW_NO[lined2012$STRATA_ID == 38],apply(lined2012[lined2012$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom3)

MBNW.lined.spr[MBNW.lined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
#mbWcom4<-spr(lined2012$TOW_NO[lined2012$STRATA_ID == 38],apply(lined2012[lined2012$STRATA_ID == 38,24:50],1,sum),
#    lined2013$TOW_NO[lined2013$STRATA_ID == 38],apply(lined2013[lined2013$STRATA_ID == 38,27:50],1,sum),
#    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K<-summary(mbWcom4)
#MBNW.lined.spr[MBNW.lined.spr$Year == 2013,c(2:3)] <- c(102.922, NA)

#2013/2014
#mbWcom5<-spr(lined2013$TOW_NO[lined2013$STRATA_ID == 38],apply(lined2013[lined2013$STRATA_ID == 38,24:50],1,sum),
#    lined2014$TOW_NO[lined2014$STRATA_ID == 38],apply(lined2014[lined2014$STRATA_ID == 38,27:50],1,sum),
#    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K<-summary(mbWcom5)
#MBNW.lined.spr[MBNW.lined.spr$Year == 2014,c(2:3)] <- c(66.491, NA)

#2014/2015
mbWcom6<-spr(lined2014$TOW_NO[lined2014$STRATA_ID == 38],apply(lined2014[lined2014$STRATA_ID == 38,24:50],1,sum),
    lined2015$TOW_NO[lined2015$STRATA_ID == 38],apply(lined2015[lined2015$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom6)

MBNW.lined.spr[MBNW.lined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
mbWcom7<-spr(lined2015$TOW_NO[lined2015$STRATA_ID == 38],apply(lined2015[lined2015$STRATA_ID == 38,24:50],1,sum),
             lined2016$TOW_NO[lined2016$STRATA_ID == 38],apply(lined2016[lined2016$STRATA_ID == 38,27:50],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom7, summary(mbWcom6))

MBNW.lined.spr[MBNW.lined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
mbWcom8<-spr(lined2016$TOW_NO[lined2016$STRATA_ID == 38],apply(lined2016[lined2016$STRATA_ID == 38,24:50],1,sum),
             lined2017$TOW_NO[lined2017$STRATA_ID == 38],apply(lined2017[lined2017$STRATA_ID == 38,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom8, summary(mbWcom7, summary(mbWcom6)))
MBNW.lined.spr[MBNW.lined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
# mbWcom9 <- spr(lined2017$TOW_NO[lined2017$STRATA_ID == 38],apply(lined2017[lined2017$STRATA_ID == 38,24:50],1,sum),
#                lined2018$TOW_NO[lined2018$STRATA_ID == 38],apply(lined2018[lined2018$STRATA_ID == 38,27:50],1,sum),
#              crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
# K<-summary(mbWcom9, summary(mbWcom8, summary(mbWcom7, summary(mbWcom6))))
#Use simple mean
#MBNW.lined.spr[MBNW.lined.spr$Year == 2018,c(2:3)] <- c(125.0300, NA)

#2018/2019
#mbWcom10 <- spr(lined2018$TOW_NO[lined2018$STRATA_ID == 38],apply(lined2018[lined2018$STRATA_ID == 38,24:50],1,sum),
#                lined2019$TOW_NO[lined2019$STRATA_ID == 38],apply(lined2019[lined2019$STRATA_ID == 38,27:50],1,sum),
#              crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
# K <- summary(mbWcom10) #NaN; use simple mean
#MBNW.lined.spr[MBNW.lined.spr$Year == 2019,c(2:3)] <- c( 106.67778, NA)

#2019/2021 
mbWcom11 <- spr(lined2019$TOW_NO[lined2019$STRATA_ID == 38],apply(lined2019[lined2019$STRATA_ID == 38,24:50],1,sum),
                lined2021$TOW_NO[lined2021$STRATA_ID == 38],apply(lined2021[lined2021$STRATA_ID == 38,27:50],1,sum),
              crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
 K <- summary(mbWcom11) 
MBNW.lined.spr[MBNW.lined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 #Only 1 matched tow - can't use SPR 
#mbWcom12 <- spr(lined2021$TOW_NO[lined2021$STRATA_ID == 38],apply(lined2021[lined2021$STRATA_ID == 38,24:50],1,sum),
#                lined2022$TOW_NO[lined2022$STRATA_ID == 38],apply(lined2022[lined2022$STRATA_ID == 38,27:50],1,sum),
#                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(mbWcom12) 
#MBNW.lined.spr[MBNW.lined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 - can't use SPR 
#mbWcom13 <- spr(lined2022$TOW_NO[lined2022$STRATA_ID == 38],apply(lined2022[lined2022$STRATA_ID == 38,24:50],1,sum),
#                lined2023$TOW_NO[lined2023$STRATA_ID == 38],apply(lined2023[lined2023$STRATA_ID == 38,27:50],1,sum),
#                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(mbWcom13) 
#MBNW.lined.spr[MBNW.lined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
mbWcom14 <- spr(lined2023$TOW_NO[lined2023$STRATA_ID == 38],apply(lined2023[lined2023$STRATA_ID == 38,24:50],1,sum),
                lined2024$TOW_NO[lined2024$STRATA_ID == 38],apply(lined2024[lined2024$STRATA_ID == 38,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWcom14) 
MBNW.lined.spr[MBNW.lined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
mbWcom15 <- spr(lined2024$TOW_NO[lined2024$STRATA_ID == 38],apply(lined2024[lined2024$STRATA_ID == 38,24:50],1,sum),
                lined2025$TOW_NO[lined2025$STRATA_ID == 38],apply(lined2025[lined2025$STRATA_ID == 38,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWcom15) 
MBNW.lined.spr[MBNW.lined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026 # Get negative number - one tow in 2026 had 0's -  use simple mean
#mbWcom16 <- spr(lined2025$TOW_NO[lined2025$STRATA_ID == 38],apply(lined2025[lined2025$STRATA_ID == 38,24:50],1,sum),
#                lined2026$TOW_NO[lined2026$STRATA_ID == 38],apply(lined2026[lined2026$STRATA_ID == 38,27:50],1,sum),
#                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(mbWcom16) 
#MBNW.lined.spr[MBNW.lined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#get negative number??:
#crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")]

#TOW_NO_REF TOW_NO
#439        165    312
#441        217    127
#444        232    132
#test <- lined2025 %>% filter(TOW_NO %in% (crossref.BoF.2026$TOW_NO_REF[crossref.BoF.2026$STRATA_ID==38]))
#apply(test[test$STRATA_ID==38,24:50],1,sum)
#lined2026 %>% filter(TOW_NO %in% c(312,127,132))

MBNW.lined.spr

#make one dataframe for simple and spr estimates
names(MBNW.lined.spr) <- c("Year", "lined.MBNW", "var.y")
#use simple mean for pre 2009 and also 2011, 2013, 2014, 2018, 2019, 2022
MBNW.Lined <- rbind(MBNW.lined.com.simple[MBNW.lined.com.simple$Year < 2009,], MBNW.lined.spr[,c(1,2)])

#use simple mean for pre 2009 and also 2011, 2013, 2014, 2018, 2019
replace.spr <- c(2011, 2013, 2014, 2018, 2019, 2022, 2023, 2026)
MBNW.Lined <- rbind(MBNW.lined.com.simple[MBNW.lined.com.simple$Year<2009,], MBNW.lined.com.simple[MBNW.lined.com.simple$Year %in% replace.spr,], MBNW.lined.spr[!MBNW.lined.spr$Year %in% replace.spr,c("Year","lined.MBNW")])
MBNW.Lined <- MBNW.Lined %>% arrange(Year)
MBNW.Lined

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNW.Lined$Year, MBNW.Lined$lined.MBNW, xout=2020) #   100.8473
MBNW.Lined[MBNW.Lined$Year==2020,"lined.MBNW"] <-  100.8473 


#make one dataframe for Midbay North Lined
MBN.Lined <- merge(MBNE.Lined[,c("Year","lined.MBNE")],MBNW.Lined[,c("Year","lined.MBNW")], by.x = "Year" )
MBN.Lined$lined.MBN <- MBN.Lined$lined.MBNE*0.732 + MBN.Lined$lined.MBNW*0.268

###
# ---- 2.  Upper Bay 28C ---- 
###
years <- 1997:surveyyear
X <- length(years)

UB.lined.com.simple <- data.frame(Year = years, lined.UB = rep(NA,X))
for (i in 1:length(UB.lined.com.simple$Year)) {
  temp.data <- lined[lined$YEAR == 1996 + i,]
    UB.lined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))
],1,sum))
  }
UB.lined.com.simple

#assume 1997-2000 same as MBN
UB.lined.com.simple[UB.lined.com.simple$Year < 2001,"lined.UB"] <- MBN.Lined[MBN.Lined$Year < 2001,4]
#interpolate for other years
approx(UB.lined.com.simple$Year, UB.lined.com.simple$lined, xout = 2004) # 58.54429
UB.lined.com.simple[UB.lined.com.simple$Year == 2004,"lined.UB"] <- 58.54429

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(UB.lined.com.simple$Year, UB.lined.com.simple$lined.UB, xout=2020) #  67.22647
UB.lined.com.simple[UB.lined.com.simple$Year==2020,c("lined.UB")] <- 67.22647 



#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
UB.lined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009
ubcom<-spr(lined2008$TOW_NO[lined2008$STRATA_ID == 53],apply(lined2008[lined2008$STRATA_ID == 53,24:50],1,sum),
    lined2009$TOW_NO[lined2009$STRATA_ID == 53],apply(lined2009[lined2009$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom)
UB.lined.spr[UB.lined.spr$Year == 2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
ubcom1<-spr(lined2009$TOW_NO[lined2009$STRATA_ID == 53],apply(lined2009[lined2009$STRATA_ID == 53,24:50],1,sum),
    lined2010$TOW_NO[lined2010$STRATA_ID == 53],apply(lined2010[lined2010$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom1, summary(ubcom))
UB.lined.spr[UB.lined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
ubcom2<-spr(lined2010$TOW_NO[lined2010$STRATA_ID == 53],apply(lined2010[lined2010$STRATA_ID == 53,24:50],1,sum),
    lined2011$TOW_NO[lined2011$STRATA_ID == 53],apply(lined2011[lined2011$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom2, summary(ubcom1, summary(ubcom)))
UB.lined.spr[UB.lined.spr$Year == 2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
ubcom3<-spr(lined2011$TOW_NO[lined2011$STRATA_ID == 53],apply(lined2011[lined2011$STRATA_ID == 53,24:50],1,sum),
    lined2012$TOW_NO[lined2012$STRATA_ID == 53],apply(lined2012[lined2012$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))
UB.lined.spr[UB.lined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
ubcom4<-spr(lined2012$TOW_NO[lined2012$STRATA_ID == 53],apply(lined2012[lined2012$STRATA_ID == 53,24:50],1,sum),
    lined2013$TOW_NO[lined2013$STRATA_ID == 53],apply(lined2013[lined2013$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))
UB.lined.spr[UB.lined.spr$Year == 2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
ubcom5<-spr(lined2013$TOW_NO[lined2013$STRATA_ID == 53],apply(lined2013[lined2013$STRATA_ID == 53,24:50],1,sum),
    lined2014$TOW_NO[lined2014$STRATA_ID == 53],apply(lined2014[lined2014$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))
UB.lined.spr[UB.lined.spr$Year == 2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
ubcom6<-spr(lined2014$TOW_NO[lined2014$STRATA_ID == 53],apply(lined2014[lined2014$STRATA_ID == 53,24:50],1,sum),
    lined2015$TOW_NO[lined2015$STRATA_ID == 53],apply(lined2015[lined2015$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))
UB.lined.spr[UB.lined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
ubcom7<-spr(lined2015$TOW_NO[lined2015$STRATA_ID == 53],apply(lined2015[lined2015$STRATA_ID == 53,24:50],1,sum),
            lined2016$TOW_NO[lined2016$STRATA_ID == 53],apply(lined2016[lined2016$STRATA_ID == 53,27:50],1,sum),
            crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))
UB.lined.spr[UB.lined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
ubcom8<-spr(lined2016$TOW_NO[lined2016$STRATA_ID == 53],apply(lined2016[lined2016$STRATA_ID == 53,24:50],1,sum),
            lined2017$TOW_NO[lined2017$STRATA_ID == 53],apply(lined2017[lined2017$STRATA_ID == 53,27:50],1,sum),
            crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))
UB.lined.spr[UB.lined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
ubcom9 <- spr(lined2017$TOW_NO[lined2017$STRATA_ID == 53],apply(lined2017[lined2017$STRATA_ID == 53,24:50],1,sum),
             lined2018$TOW_NO[lined2018$STRATA_ID == 53],apply(lined2018[lined2018$STRATA_ID == 53,27:50],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))
UB.lined.spr[UB.lined.spr$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2018/2019
ubcom10 <- spr(lined2018$TOW_NO[lined2018$STRATA_ID == 53],apply(lined2018[lined2018$STRATA_ID == 53,24:50],1,sum),
              lined2019$TOW_NO[lined2019$STRATA_ID == 53],apply(lined2019[lined2019$STRATA_ID == 53,27:50],1,sum),
              crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))
UB.lined.spr[UB.lined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
ubcom11 <- spr(lined2019$TOW_NO[lined2019$STRATA_ID == 53],apply(lined2019[lined2019$STRATA_ID == 53,24:50],1,sum),
               lined2021$TOW_NO[lined2021$STRATA_ID == 53],apply(lined2021[lined2021$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom11,summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))))
UB.lined.spr[UB.lined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
ubcom12 <- spr(lined2021$TOW_NO[lined2021$STRATA_ID == 53],apply(lined2021[lined2021$STRATA_ID == 53,24:50],1,sum),
               lined2022$TOW_NO[lined2022$STRATA_ID == 53],apply(lined2022[lined2022$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom12,summary(ubcom11,summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))))
UB.lined.spr[UB.lined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
ubcom13 <- spr(lined2022$TOW_NO[lined2022$STRATA_ID == 53],apply(lined2022[lined2022$STRATA_ID == 53,24:50],1,sum),
               lined2023$TOW_NO[lined2023$STRATA_ID == 53],apply(lined2023[lined2023$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom13,summary(ubcom12,summary(ubcom11,summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))))))
UB.lined.spr[UB.lined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
ubcom14 <- spr(lined2023$TOW_NO[lined2023$STRATA_ID == 53],apply(lined2023[lined2023$STRATA_ID == 53,24:50],1,sum),
               lined2024$TOW_NO[lined2024$STRATA_ID == 53],apply(lined2024[lined2024$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom14,summary(ubcom13,summary(ubcom12,summary(ubcom11,summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))))))
UB.lined.spr[UB.lined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
ubcom15 <- spr(lined2024$TOW_NO[lined2024$STRATA_ID == 53],apply(lined2024[lined2024$STRATA_ID == 53,24:50],1,sum),
               lined2025$TOW_NO[lined2025$STRATA_ID == 53],apply(lined2025[lined2025$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom15,summary(ubcom14,summary(ubcom13,summary(ubcom12,summary(ubcom11,summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))))))))
UB.lined.spr[UB.lined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
ubcom16 <- spr(lined2025$TOW_NO[lined2025$STRATA_ID == 53],apply(lined2025[lined2025$STRATA_ID == 53,24:50],1,sum),
               lined2026$TOW_NO[lined2026$STRATA_ID == 53],apply(lined2026[lined2026$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom16, summary(ubcom15,summary(ubcom14,summary(ubcom13,summary(ubcom12,summary(ubcom11,summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))))))))
UB.lined.spr[UB.lined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

UB.lined.spr

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(UB.lined.spr$Year, UB.lined.spr$Yspr, xout=2020) #  59.47597
UB.lined.spr[UB.lined.spr$Year==2020,"Yspr"] <- 59.47597 


#make one dataframe for simple and spr estimates
names(UB.lined.spr) <- c("Year", "lined.UB", "var.y")
UB.Lined <- rbind(UB.lined.com.simple[UB.lined.com.simple$Year < 2009,],UB.lined.spr[,c("Year","lined.UB")])

###
# ---- 3.  28D Outer ----
###
years <- 1997:surveyyear
X <- length(years)

Out.lined.com.simple <- data.frame(Year = years,lined.Out = rep(NA,X))
for (i in 1:length(Out.lined.com.simple$Year)) {
  temp.data <- lined[lined$YEAR == 1996 + i,]
  Out.lined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))
],1,sum))
}
Out.lined.com.simple

#assume 1997-2000 same as MBN
Out.lined.com.simple[Out.lined.com.simple$Year < 2001,"lined.Out"] <- MBN.Lined[MBN.Lined$Year < 2001,4]
#interpolate for other years
approx(UB.lined.com.simple$Year, Out.lined.com.simple$lined, xout = 2004) # 35.425
Out.lined.com.simple[Out.lined.com.simple$Year == 2004,"lined.Out"] <- 35.425

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Out.lined.com.simple$Year, Out.lined.com.simple$lined.Out, xout=2020) #  38
Out.lined.com.simple[Out.lined.com.simple$Year==2020,c("lined.Out")] <- 38 



#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear #update to most recent year
Y <- length(spryears)
Out.lined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009
#Only 2 tows

#2009/2010
outcom1<-spr(lined2009$TOW_NO[lined2009$STRATA_ID == 49],apply(lined2009[lined2009$STRATA_ID == 49,24:50],1,sum),
   lined2010$TOW_NO[lined2010$STRATA_ID == 49],apply(lined2010[lined2010$STRATA_ID == 49,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom1)
Out.lined.spr[Out.lined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
outcom2<-spr(lined2010$TOW_NO[lined2010$STRATA_ID == 49],apply(lined2010[lined2010$STRATA_ID == 49,24:50],1,sum),
   lined2011$TOW_NO[lined2011$STRATA_ID == 49],apply(lined2011[lined2011$STRATA_ID == 49,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom2, summary(outcom1))
Out.lined.spr[Out.lined.spr$Year == 2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
outcom3<-spr(lined2011$TOW_NO[lined2011$STRATA_ID == 49],apply(lined2011[lined2011$STRATA_ID == 49,24:50],1,sum),
   lined2012$TOW_NO[lined2012$STRATA_ID == 49],apply(lined2012[lined2012$STRATA_ID == 49,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom3, summary(outcom2, summary(outcom1)))
Out.lined.spr[Out.lined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
outcom4<-spr(lined2012$TOW_NO[lined2012$STRATA_ID == 49],apply(lined2012[lined2012$STRATA_ID == 49,24:50],1,sum),
   lined2013$TOW_NO[lined2013$STRATA_ID == 49],apply(lined2013[lined2013$STRATA_ID == 49,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom4, summary(outcom3, summary(outcom2, summary(outcom1))))
Out.lined.spr[Out.lined.spr$Year == 2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
#outcom5<-spr(lined2013$TOW_NO[lined2013$STRATA_ID == 49],apply(lined2013[lined2013$STRATA_ID == 49,24:50],1,sum),
#   lined2014$TOW_NO[lined2014$STRATA_ID == 49],apply(lined2014[lined2014$STRATA_ID == 49,27:50],1,sum),
#    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
#K<-summary(outcom5) #only 2 paired tows
#Out.lined.spr[Out.lined.spr$Year == 2014,c(2:3)] <- c(40.375, NA)

#2014/2015
outcom6<-spr(lined2014$TOW_NO[lined2014$STRATA_ID == 49],apply(lined2014[lined2014$STRATA_ID == 49,24:50],1,sum),
   lined2015$TOW_NO[lined2015$STRATA_ID == 49],apply(lined2015[lined2015$STRATA_ID == 49,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom6)
Out.lined.spr[Out.lined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
outcom7<-spr(lined2015$TOW_NO[lined2015$STRATA_ID == 49],apply(lined2015[lined2015$STRATA_ID == 49,24:50],1,sum),
             lined2016$TOW_NO[lined2016$STRATA_ID == 49],apply(lined2016[lined2016$STRATA_ID == 49,27:50],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom7, summary(outcom6))
Out.lined.spr[Out.lined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
outcom8<-spr(lined2016$TOW_NO[lined2016$STRATA_ID == 49],apply(lined2016[lined2016$STRATA_ID == 49,24:50],1,sum),
             lined2017$TOW_NO[lined2017$STRATA_ID == 49],apply(lined2017[lined2017$STRATA_ID == 49,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom8, summary(outcom7, summary(outcom6)))
Out.lined.spr[Out.lined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
outcom9 <- spr(lined2017$TOW_NO[lined2017$STRATA_ID == 49],apply(lined2017[lined2017$STRATA_ID == 49,24:50],1,sum),
              lined2018$TOW_NO[lined2018$STRATA_ID == 49],apply(lined2018[lined2018$STRATA_ID == 49,27:50],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6))))
Out.lined.spr[Out.lined.spr$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
outcom10 <- spr(lined2018$TOW_NO[lined2018$STRATA_ID == 49],apply(lined2018[lined2018$STRATA_ID == 49,24:50],1,sum),
                lined2019$TOW_NO[lined2019$STRATA_ID == 49],apply(lined2019[lined2019$STRATA_ID == 49,27:50],1,sum),
               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom10, summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6)))))
Out.lined.spr[Out.lined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
outcom11 <- spr(lined2019$TOW_NO[lined2019$STRATA_ID == 49],apply(lined2019[lined2019$STRATA_ID == 49,24:50],1,sum),
                lined2021$TOW_NO[lined2021$STRATA_ID == 49],apply(lined2021[lined2021$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom11, summary(outcom10, summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6))))))
Out.lined.spr[Out.lined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


#2021/2022
outcom12 <- spr(lined2021$TOW_NO[lined2021$STRATA_ID == 49],apply(lined2021[lined2021$STRATA_ID == 49,24:50],1,sum),
                lined2022$TOW_NO[lined2022$STRATA_ID == 49],apply(lined2022[lined2022$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom12, summary(outcom11, summary(outcom10, summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6)))))))
Out.lined.spr[Out.lined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
outcom13 <- spr(lined2022$TOW_NO[lined2022$STRATA_ID == 49],apply(lined2022[lined2022$STRATA_ID == 49,24:50],1,sum),
                lined2023$TOW_NO[lined2023$STRATA_ID == 49],apply(lined2023[lined2023$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom13,summary(outcom12, summary(outcom11, summary(outcom10, summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6))))))))
Out.lined.spr[Out.lined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
outcom14 <- spr(lined2023$TOW_NO[lined2023$STRATA_ID == 49],apply(lined2023[lined2023$STRATA_ID == 49,24:50],1,sum),
                lined2024$TOW_NO[lined2024$STRATA_ID == 49],apply(lined2024[lined2024$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom14,summary(outcom13,summary(outcom12, summary(outcom11, summary(outcom10, summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6)))))))))
Out.lined.spr[Out.lined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
outcom15 <- spr(lined2024$TOW_NO[lined2024$STRATA_ID == 49],apply(lined2024[lined2024$STRATA_ID == 49,24:50],1,sum),
                lined2025$TOW_NO[lined2025$STRATA_ID == 49],apply(lined2025[lined2025$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <-  summary(outcom15, summary(outcom14,summary(outcom13,summary(outcom12, summary(outcom11, summary(outcom10, summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6))))))))))
Out.lined.spr[Out.lined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
outcom16 <- spr(lined2025$TOW_NO[lined2025$STRATA_ID == 49],apply(lined2025[lined2025$STRATA_ID == 49,24:50],1,sum),
                lined2026$TOW_NO[lined2026$STRATA_ID == 49],apply(lined2026[lined2026$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <-  summary(outcom16,summary(outcom15, summary(outcom14,summary(outcom13,summary(outcom12, summary(outcom11, summary(outcom10, summary(outcom9, summary(outcom8, summary(outcom7, summary(outcom6)))))))))))
Out.lined.spr[Out.lined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

Out.lined.spr

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Out.lined.spr$Year, Out.lined.spr$Yspr, xout=2020) #  24.3761
Out.lined.spr[Out.lined.spr$Year==2020,"Yspr"] <- 24.3761 

#make one dataframe for simple and spr estimates
names(Out.lined.spr) <- c("Year", "lined.Out", "var.y")
Out.Lined <- rbind(Out.lined.com.simple[Out.lined.com.simple$Year < 2010,],Out.lined.com.simple[Out.lined.com.simple$Year == 2014,], Out.lined.spr[Out.lined.spr$Year!=2014,c("Year","lined.Out")])
Out.Lined <- Out.Lined %>% arrange(Year)
Out.Lined

###
# ---- 4.  Advocate ---- 
###
years <- 1997:surveyyear
X <- length(years)

AH.lined.com.simple <- data.frame(Year = years, lined.AH = rep(NA,X))
for (i in 1:length(AH.lined.com.simple$Year)) {
  temp.data <- lined[lined$YEAR == 1996 + i,]
    AH.lined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))
],1,sum))
  }
AH.lined.com.simple

#assume 1997-2000 same as MBN
AH.lined.com.simple[AH.lined.com.simple$Year < 2001,"lined.AH"] <- MBN.Lined[MBN.Lined$Year < 2001,4]
#interpolate for other years
approx(AH.lined.com.simple$Year, AH.lined.com.simple$lined, xout = 2004) # 188.4583
AH.lined.com.simple[AH.lined.com.simple$Year == 2004,"lined.AH"] <- 188.4583

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(AH.lined.com.simple$Year, AH.lined.com.simple$lined.AH, xout=2020) #  415.6
AH.lined.com.simple[AH.lined.com.simple$Year==2020,c("lined.AH")] <- 415.6 



#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
AH.lined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2009/2010
ahcom<-spr(lined2008$TOW_NO[lined2008$STRATA_ID == 35],apply(lined2008[lined2008$STRATA_ID == 35,24:50],1,sum),
    lined2009$TOW_NO[lined2009$STRATA_ID == 35],apply(lined2009[lined2009$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom)

AH.lined.spr[AH.lined.spr$Year == 2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
ahcom1<-spr(lined2009$TOW_NO[lined2009$STRATA_ID == 35],apply(lined2009[lined2009$STRATA_ID == 35,24:50],1,sum),
    lined2010$TOW_NO[lined2010$STRATA_ID == 35],apply(lined2010[lined2010$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom1, summary(ahcom))

AH.lined.spr[AH.lined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
ahcom2<-spr(lined2010$TOW_NO[lined2010$STRATA_ID == 35],apply(lined2010[lined2010$STRATA_ID == 35,24:50],1,sum),
    lined2011$TOW_NO[lined2011$STRATA_ID == 35],apply(lined2011[lined2011$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom2, summary(ahcom1, summary(ahcom)))

AH.lined.spr[AH.lined.spr$Year == 2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
ahcom3<-spr(lined2011$TOW_NO[lined2011$STRATA_ID == 35],apply(lined2011[lined2011$STRATA_ID == 35,24:50],1,sum),
    lined2012$TOW_NO[lined2012$STRATA_ID == 35],apply(lined2012[lined2012$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))

AH.lined.spr[AH.lined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
ahcom4<-spr(lined2012$TOW_NO[lined2012$STRATA_ID == 35],apply(lined2012[lined2012$STRATA_ID == 35,24:50],1,sum),
    lined2013$TOW_NO[lined2013$STRATA_ID == 35],apply(lined2013[lined2013$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))

AH.lined.spr[AH.lined.spr$Year == 2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
ahcom5<-spr(lined2013$TOW_NO[lined2013$STRATA_ID == 35],apply(lined2013[lined2013$STRATA_ID == 35,24:50],1,sum),
    lined2014$TOW_NO[lined2014$STRATA_ID == 35],apply(lined2014[lined2014$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))

AH.lined.spr[AH.lined.spr$Year == 2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
ahcom6<-spr(lined2014$TOW_NO[lined2014$STRATA_ID == 35],apply(lined2014[lined2014$STRATA_ID == 35,24:50],1,sum),
    lined2015$TOW_NO[lined2015$STRATA_ID == 35],apply(lined2015[lined2015$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))

AH.lined.spr[AH.lined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
ahcom7<-spr(lined2015$TOW_NO[lined2015$STRATA_ID == 35],apply(lined2015[lined2015$STRATA_ID == 35,24:50],1,sum),
            lined2016$TOW_NO[lined2016$STRATA_ID == 35],apply(lined2016[lined2016$STRATA_ID == 35,27:50],1,sum),
            crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))

AH.lined.spr[AH.lined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
ahcom8<-spr(lined2016$TOW_NO[lined2016$STRATA_ID == 35],apply(lined2016[lined2016$STRATA_ID == 35,24:50],1,sum),
            lined2017$TOW_NO[lined2017$STRATA_ID == 35],apply(lined2017[lined2017$STRATA_ID == 35,27:50],1,sum),
            crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))

AH.lined.spr[AH.lined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
ahcom9 <- spr(lined2017$TOW_NO[lined2017$STRATA_ID == 35],apply(lined2017[lined2017$STRATA_ID == 35,24:50],1,sum),
              lined2018$TOW_NO[lined2018$STRATA_ID == 35],apply(lined2018[lined2018$STRATA_ID == 35,27:50],1,sum),
            crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))

AH.lined.spr[AH.lined.spr$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
ahcom10 <- spr(lined2018$TOW_NO[lined2018$STRATA_ID == 35],apply(lined2018[lined2018$STRATA_ID == 35,24:50],1,sum),
               lined2019$TOW_NO[lined2019$STRATA_ID == 35],apply(lined2019[lined2019$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))

AH.lined.spr[AH.lined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #No survey in 2020
ahcom11 <- spr(lined2019$TOW_NO[lined2019$STRATA_ID == 35],apply(lined2019[lined2019$STRATA_ID == 35,24:50],1,sum),
               lined2021$TOW_NO[lined2021$STRATA_ID == 35],apply(lined2021[lined2021$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))))
AH.lined.spr[AH.lined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022 
ahcom12 <- spr(lined2021$TOW_NO[lined2021$STRATA_ID == 35],apply(lined2021[lined2021$STRATA_ID == 35,24:50],1,sum),
               lined2022$TOW_NO[lined2022$STRATA_ID == 35],apply(lined2022[lined2022$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))))
AH.lined.spr[AH.lined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023 
ahcom13 <- spr(lined2022$TOW_NO[lined2022$STRATA_ID == 35],apply(lined2022[lined2022$STRATA_ID == 35,24:50],1,sum),
               lined2023$TOW_NO[lined2023$STRATA_ID == 35],apply(lined2023[lined2023$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))))))
AH.lined.spr[AH.lined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024 
ahcom14 <- spr(lined2023$TOW_NO[lined2023$STRATA_ID == 35],apply(lined2023[lined2023$STRATA_ID == 35,24:50],1,sum),
               lined2024$TOW_NO[lined2024$STRATA_ID == 35],apply(lined2024[lined2024$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom14, summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))))))
AH.lined.spr[AH.lined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025 
ahcom15 <- spr(lined2024$TOW_NO[lined2024$STRATA_ID == 35],apply(lined2024[lined2024$STRATA_ID == 35,24:50],1,sum),
               lined2025$TOW_NO[lined2025$STRATA_ID == 35],apply(lined2025[lined2025$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom15,summary(ahcom14, summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))))))))
AH.lined.spr[AH.lined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
ahcom16 <- spr(lined2025$TOW_NO[lined2025$STRATA_ID == 35],apply(lined2025[lined2025$STRATA_ID == 35,24:50],1,sum),
               lined2026$TOW_NO[lined2026$STRATA_ID == 35],apply(lined2026[lined2026$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom16,summary(ahcom15,summary(ahcom14, summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))))))))
AH.lined.spr[AH.lined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

AH.lined.spr

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(AH.lined.spr$Year, AH.lined.spr$Yspr, xout=2020) #  141.0042
AH.lined.spr[AH.lined.spr$Year==2020,"Yspr"] <- 141.0042 

#make one dataframe for simple and spr estimates
names(AH.lined.spr) <- c("Year", "lined.AH", "var.y")
AH.Lined <- rbind(AH.lined.com.simple[AH.lined.com.simple$Year < 2009,],AH.lined.spr[,c("Year","lined.AH")])
AH.Lined

###
# ---- 5-7.  Cape Spencer, Scots Bay, Spencers Island (simple means) ---- 
###
years <- 1997:surveyyear
X <- length(years)

lined.nonSPR.com <- data.frame(Year = years, lined.CS = rep(NA,X),lined.SI = rep(NA,X), lined.SB = rep(NA,X))
for (i in 1:length(lined.nonSPR.com$Year)) {
  temp.data <- lined[lined$YEAR == 1996 + i,]
  lined.nonSPR.com[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 37 & temp.data$TOW_TYPE_ID == 1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))],1,sum)) #CS
  lined.nonSPR.com[i,3] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))],1,sum)) #SI
  lined.nonSPR.com[i,4] <- mean(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, grep("BIN_ID_80", colnames(temp.data)):grep("BIN_ID_195", colnames(temp.data))],1,sum)) #SB
  }
lined.nonSPR.com

#assume 2001-2004 same as Outer
lined.nonSPR.com[c(5:8), "lined.SI"] <- Out.lined.com.simple[c(5:8),2]
lined.nonSPR.com[c(5:8), "lined.SB"] <- Out.lined.com.simple[c(5:8),2]
#assume 1997-2000 same as MBN
lined.nonSPR.com[c(1:4), "lined.SI"] <- MBN.Lined[MBN.Lined$Year < 2001,4]
lined.nonSPR.com[c(1:4), "lined.SB"] <- MBN.Lined[MBN.Lined$Year < 2001,4]

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(lined.nonSPR.com$Year, lined.nonSPR.com$lined.CS, xout=2020) #  206.4897
lined.nonSPR.com[lined.nonSPR.com$Year==2020,c("lined.CS")] <- 206.4897 
approx(lined.nonSPR.com$Year, lined.nonSPR.com$lined.SI, xout=2020) #  28.3
lined.nonSPR.com[lined.nonSPR.com$Year==2020,c("lined.SI")] <- 28.3 
approx(lined.nonSPR.com$Year, lined.nonSPR.com$lined.SB, xout=2020) #   20.7375
lined.nonSPR.com[lined.nonSPR.com$Year==2020,c("lined.SB")] <-  20.7375 


###
# ---- calculate stratified mean for Lined ---- 
###

temp <- merge(lined.nonSPR.com, MBN.Lined[,c(1,4)], by = "Year", all = TRUE)
temp2 <- merge(temp, UB.Lined, by.x = "Year", all = TRUE)
temp3 <- merge(temp2, AH.Lined, by.x = "Year", all = TRUE)
Lined.1B <- merge(temp3, Out.Lined, by.x = "Year", all = TRUE)

Lined.1B$strat.Lined <- (Lined.1B$lined.CS*0.261411226) + (Lined.1B$lined.SI*0.027831987) + (Lined.1B$lined.SB*0.02657028) + (Lined.1B$lined.MBN*0.402530584) + (Lined.1B$lined.UB*0.120426061) + (Lined.1B$lined.AH*0.011804834) + (Lined.1B$lined.Out*0.149425015)

###
### ---- UNLINED GEAR ---- 
###
#Use Commercial size only
#model runs from 1997+

###
# ---- 1. Middle Bay North (spr) ---- 
###
#1a. Middle Bay North - EAST

years <- 1997:surveyyear
X <- length(years)

MBNE.unlined.com.simple <- data.frame(Year = years, unlined.MBNE = rep(NA,X))
for (i in 1:length(MBNE.unlined.com.simple$Year)){
  temp.data <- unlined[unlined$YEAR == 1996 + i,]
    MBNE.unlined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 58 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
  }
MBNE.unlined.com.simple

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNE.unlined.com.simple$Year, MBNE.unlined.com.simple$unlined.MBNE, xout=2020) #  125.4992
MBNE.unlined.com.simple[MBNE.unlined.com.simple$Year==2020,c("unlined.MBNE")] <- 125.4992 


#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear  
Y <- length(spryears)
MBNE.unlined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2009/2010
mbEcom1<-spr(unlined2009$TOW_NO[unlined2009$STRATA_ID == 58],apply(unlined2009[unlined2009$STRATA_ID == 58,24:50],1,sum),unlined2010$TOW_NO[unlined2010$STRATA_ID == 58],apply(unlined2010[unlined2010$STRATA_ID == 58,27:50],1,sum),
crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom1)
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
mbEcom2<-spr(unlined2010$TOW_NO[unlined2010$STRATA_ID == 58],apply(unlined2010[unlined2010$STRATA_ID == 58,24:50],1,sum),unlined2011$TOW_NO[unlined2011$STRATA_ID == 58],apply(unlined2011[unlined2011$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom2, summary(mbEcom1))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
mbEcom3<-spr(unlined2011$TOW_NO[unlined2011$STRATA_ID == 58],apply(unlined2011[unlined2011$STRATA_ID == 58,24:50],1,sum),unlined2012$TOW_NO[unlined2012$STRATA_ID == 58],apply(unlined2012[unlined2012$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom3, summary(mbEcom2, summary(mbEcom1)))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
mbEcom4<-spr(unlined2012$TOW_NO[unlined2012$STRATA_ID == 58],apply(unlined2012[unlined2012$STRATA_ID == 58,24:50],1,sum),unlined2013$TOW_NO[unlined2013$STRATA_ID == 58],apply(unlined2013[unlined2013$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
mbEcom5<-spr(unlined2013$TOW_NO[unlined2013$STRATA_ID == 58],apply(unlined2013[unlined2013$STRATA_ID == 58,24:50],1,sum),unlined2014$TOW_NO[unlined2014$STRATA_ID == 58],apply(unlined2014[unlined2014$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1)))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
mbEcom6<-spr(unlined2014$TOW_NO[unlined2014$STRATA_ID == 58],apply(unlined2014[unlined2014$STRATA_ID == 58,24:50],1,sum),unlined2015$TOW_NO[unlined2015$STRATA_ID == 58],apply(unlined2015[unlined2015$STRATA_ID == 58,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
mbEcom7<-spr(unlined2015$TOW_NO[unlined2015$STRATA_ID == 58],apply(unlined2015[unlined2015$STRATA_ID == 58,24:50],1,sum),unlined2016$TOW_NO[unlined2016$STRATA_ID == 58],apply(unlined2016[unlined2016$STRATA_ID == 58,27:50],1,sum), crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1)))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
mbEcom8<-spr(unlined2016$TOW_NO[unlined2016$STRATA_ID == 58],apply(unlined2016[unlined2016$STRATA_ID == 58,24:50],1,sum),
             unlined2017$TOW_NO[unlined2017$STRATA_ID == 58],apply(unlined2017[unlined2017$STRATA_ID == 58,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
mbEcom9 <- spr(unlined2017$TOW_NO[unlined2017$STRATA_ID == 58],apply(unlined2017[unlined2017$STRATA_ID == 58,24:50],1,sum),
             unlined2018$TOW_NO[unlined2018$STRATA_ID == 58],apply(unlined2018[unlined2018$STRATA_ID == 58,27:50],1,sum),
             crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1)))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
mbEcom10 <- spr(unlined2018$TOW_NO[unlined2018$STRATA_ID == 58],apply(unlined2018[unlined2018$STRATA_ID == 58,24:50],1,sum),
                unlined2019$TOW_NO[unlined2019$STRATA_ID == 58],apply(unlined2019[unlined2019$STRATA_ID == 58,27:50],1,sum),
              crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1))))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
mbEcom11 <- spr(unlined2019$TOW_NO[unlined2019$STRATA_ID == 58],apply(unlined2019[unlined2019$STRATA_ID == 58,24:50],1,sum),
                unlined2021$TOW_NO[unlined2021$STRATA_ID == 58],apply(unlined2021[unlined2021$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1)))))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
mbEcom12 <- spr(unlined2021$TOW_NO[unlined2021$STRATA_ID == 58],apply(unlined2021[unlined2021$STRATA_ID == 58,24:50],1,sum),
                unlined2022$TOW_NO[unlined2022$STRATA_ID == 58],apply(unlined2022[unlined2022$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1))))))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
mbEcom13 <- spr(unlined2022$TOW_NO[unlined2022$STRATA_ID == 58],apply(unlined2022[unlined2022$STRATA_ID == 58,24:50],1,sum),
                unlined2023$TOW_NO[unlined2023$STRATA_ID == 58],apply(unlined2023[unlined2023$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1)))))))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
mbEcom14 <- spr(unlined2023$TOW_NO[unlined2023$STRATA_ID == 58],apply(unlined2023[unlined2023$STRATA_ID == 58,24:50],1,sum),
                unlined2024$TOW_NO[unlined2024$STRATA_ID == 58],apply(unlined2024[unlined2024$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom14,summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1))))))))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
mbEcom15 <- spr(unlined2024$TOW_NO[unlined2024$STRATA_ID == 58],apply(unlined2024[unlined2024$STRATA_ID == 58,24:50],1,sum),
                unlined2025$TOW_NO[unlined2025$STRATA_ID == 58],apply(unlined2025[unlined2025$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom15,summary(mbEcom14,summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1)))))))))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
mbEcom16 <- spr(unlined2025$TOW_NO[unlined2025$STRATA_ID == 58],apply(unlined2025[unlined2025$STRATA_ID == 58,24:50],1,sum),
                unlined2026$TOW_NO[unlined2026$STRATA_ID == 58],apply(unlined2026[unlined2026$STRATA_ID == 58,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 58,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbEcom16,summary(mbEcom15,summary(mbEcom14,summary(mbEcom13, summary(mbEcom12, summary(mbEcom11, summary(mbEcom10, summary(mbEcom9, summary(mbEcom8, summary(mbEcom7, summary(mbEcom6, summary(mbEcom5, summary(mbEcom4, summary(mbEcom3, summary(mbEcom2, summary(mbEcom1))))))))))))))))
MBNE.unlined.spr[MBNE.unlined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

MBNE.unlined.spr

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNE.unlined.spr$Year, MBNE.unlined.spr$Yspr, xout=2020) #   97.51285
MBNE.unlined.spr[MBNE.unlined.spr$Year==2020,"Yspr"] <-  97.51285 

#make one dataframe for simple and spr estimates
names(MBNE.unlined.spr) <- c("Year", "unlined.MBNE", "var.y")
MBNE.Unlined <- rbind(MBNE.unlined.com.simple[MBNE.unlined.com.simple$Year < 2010,], MBNE.unlined.spr[,c("Year","unlined.MBNE")])



#1b. Middle Bay North - WEST
years <- 1997:surveyyear
X <- length(years)

MBNW.unlined.com.simple <- data.frame(Year = years, unlined.MBNW = rep(NA,X))
for (i in 1:length(MBNW.unlined.com.simple$Year)) {
  temp.data <- unlined[unlined$YEAR == 1996 + i,]
    MBNW.unlined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 38 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
  }
MBNW.unlined.com.simple

#fill in missing years, assume 1997-2002 as MBNE, interpolate 2004
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 1997,"unlined.MBNW"] <- 54.66667
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 1998,"unlined.MBNW"] <- 58.70000
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 1999,"unlined.MBNW"] <- 83.47500
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 2000,"unlined.MBNW"] <- 78.97500
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 2001,"unlined.MBNW"] <- 98.67692
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 2002,"unlined.MBNW"] <- 61.3727
#2004
approx(MBNW.unlined.com.simple$Year, MBNW.unlined.com.simple$unlined.MBNW, xout = 2004) # 106.3575; No tows in W
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 2004,"unlined.MBNW"] <- 106.3575
#2020
approx(MBNW.unlined.com.simple$Year, MBNW.unlined.com.simple$unlined.MBNW, xout = 2020) # 225.5778; No tows in W
MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year == 2020,"unlined.MBNW"] <- 225.5778



#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
MBNW.unlined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009
mbWcom<-spr(unlined2008$TOW_NO[unlined2008$STRATA_ID == 38],apply(unlined2008[unlined2008$STRATA_ID == 38,24:50],1,sum),unlined2009$TOW_NO[unlined2009$STRATA_ID == 38],apply(unlined2009[unlined2009$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom)
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
mbWcom1<-spr(unlined2009$TOW_NO[unlined2009$STRATA_ID == 38],apply(unlined2009[unlined2009$STRATA_ID == 38,24:50],1,sum),unlined2010$TOW_NO[unlined2010$STRATA_ID == 38],apply(unlined2010[unlined2010$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom1, summary(mbWcom))
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2011,c(2:3)] <- c(174.718,NA)

#2011/2012
mbWcom3<-spr(unlined2011$TOW_NO[unlined2011$STRATA_ID == 38],apply(unlined2011[unlined2011$STRATA_ID == 38,24:50],1,sum),unlined2012$TOW_NO[unlined2012$STRATA_ID == 38],apply(unlined2012[unlined2012$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom3)
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2013,c(2:3)] <- c(130.622, NA)

#2013/2014
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2014,c(2:3)] <- c(154.636, NA)

#2014/2015
mbWcom6<-spr(unlined2014$TOW_NO[unlined2014$STRATA_ID == 38],apply(unlined2014[unlined2014$STRATA_ID == 38,24:50],1,sum),unlined2015$TOW_NO[unlined2015$STRATA_ID == 38],apply(unlined2015[unlined2015$STRATA_ID == 38,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom6)
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
mbWcom7<-spr(unlined2015$TOW_NO[unlined2015$STRATA_ID == 38],apply(unlined2015[unlined2015$STRATA_ID == 38,24:50],1,sum),unlined2016$TOW_NO[unlined2016$STRATA_ID == 38],apply(unlined2016[unlined2016$STRATA_ID == 38,27:50],1,sum), crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom7, summary(mbWcom6))
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
mbWcom8<-spr(unlined2016$TOW_NO[unlined2016$STRATA_ID == 38],apply(unlined2016[unlined2016$STRATA_ID == 38,24:50],1,sum),
             unlined2017$TOW_NO[unlined2017$STRATA_ID == 38],apply(unlined2017[unlined2017$STRATA_ID == 38,27:50],1,sum),
             crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K<-summary(mbWcom8, summary(mbWcom7, summary(mbWcom6)))
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
# mbWcom9 <- spr(unlined2017$TOW_NO[unlined2017$STRATA_ID == 38],apply(unlined2017[unlined2017$STRATA_ID == 38,24:50],1,sum),
#                unlined2018$TOW_NO[unlined2018$STRATA_ID == 38],apply(unlined2018[unlined2018$STRATA_ID == 38,27:50],1,sum),
#               crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
# K<-summary(mbWcom9, summary(mbWcom8, summary(mbWcom7, summary(mbWcom6))))
#Use simple mean
#MBNW.unlined.spr[MBNW.unlined.spr$Year == 2018,c(2:3)] <- c(289.930, NA)

#2018/2019
# mbWcom10 <- spr(unlined2018$TOW_NO[unlined2018$STRATA_ID == 38],apply(unlined2018[unlined2018$STRATA_ID == 38,24:50],1,sum),
#                 unlined2019$TOW_NO[unlined2019$STRATA_ID == 38],apply(unlined2019[unlined2019$STRATA_ID == 38,27:50],1,sum),
#               crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
# K <- summary(mbWcom10) #Use simple mean
#MBNW.unlined.spr[MBNW.unlined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
 mbWcom11 <- spr(unlined2019$TOW_NO[unlined2019$STRATA_ID == 38],apply(unlined2019[unlined2019$STRATA_ID == 38,24:50],1,sum),
                 unlined2021$TOW_NO[unlined2021$STRATA_ID == 38],apply(unlined2021[unlined2021$STRATA_ID == 38,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
 K <- summary(mbWcom11) #Use simple mean
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2021,c(2:3)] <- c(243.1556, NA)

#2021/2022  #only one matched tow - can't use SPR 
#mbWcom12 <- spr(unlined2021$TOW_NO[unlined2021$STRATA_ID == 38],apply(unlined2021[unlined2021$STRATA_ID == 38,24:50],1,sum),
#                unlined2022$TOW_NO[unlined2022$STRATA_ID == 38],apply(unlined2022[unlined2022$STRATA_ID == 38,27:50],1,sum),
#                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(mbWcom12) #Use simple mean
#MBNW.unlined.spr[MBNW.unlined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023  #only one matched tow - can't use SPR 
#mbWcom13 <- spr(unlined2022$TOW_NO[unlined2022$STRATA_ID == 38],apply(unlined2022[unlined2022$STRATA_ID == 38,24:50],1,sum),
#                unlined2023$TOW_NO[unlined2023$STRATA_ID == 38],apply(unlined2023[unlined2023$STRATA_ID == 38,27:50],1,sum),
#                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
#K <- summary(mbWcom13) #Use simple mean
#MBNW.unlined.spr[MBNW.unlined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024  
mbWcom14 <- spr(unlined2023$TOW_NO[unlined2023$STRATA_ID == 38],apply(unlined2023[unlined2023$STRATA_ID == 38,24:50],1,sum),
                unlined2024$TOW_NO[unlined2024$STRATA_ID == 38],apply(unlined2024[unlined2024$STRATA_ID == 38,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWcom14) #Use simple mean
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025  
mbWcom15 <- spr(unlined2024$TOW_NO[unlined2024$STRATA_ID == 38],apply(unlined2024[unlined2024$STRATA_ID == 38,24:50],1,sum),
                unlined2025$TOW_NO[unlined2025$STRATA_ID == 38],apply(unlined2025[unlined2025$STRATA_ID == 38,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWcom15) #Use simple mean
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026  
mbWcom16 <- spr(unlined2025$TOW_NO[unlined2025$STRATA_ID == 38],apply(unlined2025[unlined2025$STRATA_ID == 38,24:50],1,sum),
                unlined2026$TOW_NO[unlined2026$STRATA_ID == 38],apply(unlined2026[unlined2026$STRATA_ID == 38,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 38,c("TOW_NO_REF","TOW_NO")])
K <- summary(mbWcom16) #Use simple mean
MBNW.unlined.spr[MBNW.unlined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)


MBNW.unlined.spr

#make one dataframe for simple and spr estimates
names(MBNW.unlined.spr) <- c("Year", "unlined.MBNW", "var.y")
#use simple mean for pre 2009 and also 2018, 2019, 2022
replace.spr <- c(2018, 2019, 2022, 2023)
MBNW.Unlined <- rbind(MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year < 2009,], MBNW.unlined.com.simple[MBNW.unlined.com.simple$Year %in% replace.spr,] , MBNW.unlined.spr[!MBNW.unlined.spr$Year %in% replace.spr,c("Year","unlined.MBNW")])
MBNW.Unlined <- MBNW.Unlined %>% arrange(Year)
MBNW.Unlined

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(MBNW.Unlined$Year, MBNW.Unlined$unlined.MBNW, xout=2020) #  243.1556
MBNW.Unlined[MBNW.Unlined$Year==2020,"unlined.MBNW"] <- 243.1556 

#make one dataframe for Midbay North Lined
MBN.Unlined <- merge(MBNE.Unlined[,c("Year","unlined.MBNE")],MBNW.Unlined[,c("Year","unlined.MBNW")], by.x = "Year" )
MBN.Unlined$unlined.MBN <- MBN.Unlined$unlined.MBNE*0.732 + MBN.Unlined$unlined.MBNW*0.268


# ---- 2. Upper Bay 28 c ---- 
years <- 1997:surveyyear
X <- length(years)

UB.unlined.com.simple <- data.frame(Year = years, unlined.UB = rep(NA,X))
for (i in 1:length(UB.unlined.com.simple$Year)) {
  temp.data <- unlined[unlined$YEAR == 1996 + i,]
    UB.unlined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 53 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
  }
UB.unlined.com.simple

#assume 1997-2000 same as MBN
UB.unlined.com.simple[UB.unlined.com.simple$Year < 2001,"unlined.UB"] <- MBN.Unlined[MBN.Unlined$Year < 2001,4]
#interpolate for other years
approx(UB.unlined.com.simple$Year, UB.unlined.com.simple$unlined.UB, xout = 2004) #  125.0005
UB.unlined.com.simple[UB.unlined.com.simple$Year == 2004,"unlined.UB"] <- 125.0005

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(UB.unlined.com.simple$Year, UB.unlined.com.simple$unlined.UB, xout=2020) #  86.44706
UB.unlined.com.simple[UB.unlined.com.simple$Year==2020,"unlined.UB"] <- 86.44706 


#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
UB.unlined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009
ubcom<-spr(unlined2008$TOW_NO[unlined2008$STRATA_ID == 53],apply(unlined2008[unlined2008$STRATA_ID == 53,24:50],1,sum),
   unlined2009$TOW_NO[unlined2009$STRATA_ID == 53],apply(unlined2009[unlined2009$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom)
UB.unlined.spr[UB.unlined.spr$Year == 2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
ubcom1<-spr(unlined2009$TOW_NO[unlined2009$STRATA_ID == 53],apply(unlined2009[unlined2009$STRATA_ID == 53,24:50],1,sum),
   unlined2010$TOW_NO[unlined2010$STRATA_ID == 53],apply(unlined2010[unlined2010$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom1, summary(ubcom))
UB.unlined.spr[UB.unlined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
ubcom2<-spr(unlined2010$TOW_NO[unlined2010$STRATA_ID == 53],apply(unlined2010[unlined2010$STRATA_ID == 53,24:50],1,sum),
   unlined2011$TOW_NO[unlined2011$STRATA_ID == 53],apply(unlined2011[unlined2011$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom2, summary(ubcom1, summary(ubcom)))
UB.unlined.spr[UB.unlined.spr$Year == 2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
ubcom3<-spr(unlined2011$TOW_NO[unlined2011$STRATA_ID == 53],apply(unlined2011[unlined2011$STRATA_ID == 53,24:50],1,sum),
   unlined2012$TOW_NO[unlined2012$STRATA_ID == 53],apply(unlined2012[unlined2012$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))
UB.unlined.spr[UB.unlined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
ubcom4<-spr(unlined2012$TOW_NO[unlined2012$STRATA_ID == 53],apply(unlined2012[unlined2012$STRATA_ID == 53,24:50],1,sum),
   unlined2013$TOW_NO[unlined2013$STRATA_ID == 53],apply(unlined2013[unlined2013$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))
UB.unlined.spr[UB.unlined.spr$Year == 2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
ubcom5<-spr(unlined2013$TOW_NO[unlined2013$STRATA_ID == 53],apply(unlined2013[unlined2013$STRATA_ID == 53,24:50],1,sum),
   unlined2014$TOW_NO[unlined2014$STRATA_ID == 53],apply(unlined2014[unlined2014$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))
UB.unlined.spr[UB.unlined.spr$Year == 2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
ubcom6<-spr(unlined2014$TOW_NO[unlined2014$STRATA_ID == 53],apply(unlined2014[unlined2014$STRATA_ID == 53,24:50],1,sum),
   unlined2015$TOW_NO[unlined2015$STRATA_ID == 53],apply(unlined2015[unlined2015$STRATA_ID == 53,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))
UB.unlined.spr[UB.unlined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
ubcom7<-spr(unlined2015$TOW_NO[unlined2015$STRATA_ID == 53],apply(unlined2015[unlined2015$STRATA_ID == 53,24:50],1,sum),
            unlined2016$TOW_NO[unlined2016$STRATA_ID == 53],apply(unlined2016[unlined2016$STRATA_ID == 53,27:50],1,sum),
            crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
ubcom8<-spr(unlined2016$TOW_NO[unlined2016$STRATA_ID == 53],apply(unlined2016[unlined2016$STRATA_ID == 53,24:50],1,sum),
            unlined2017$TOW_NO[unlined2017$STRATA_ID == 53],apply(unlined2017[unlined2017$STRATA_ID == 53,27:50],1,sum),
            crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K<-summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
ubcom9 <- spr(unlined2017$TOW_NO[unlined2017$STRATA_ID == 53],apply(unlined2017[unlined2017$STRATA_ID == 53,24:50],1,sum),
              unlined2018$TOW_NO[unlined2018$STRATA_ID == 53],apply(unlined2018[unlined2018$STRATA_ID == 53,27:50],1,sum),
            crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
ubcom10 <- spr(unlined2018$TOW_NO[unlined2018$STRATA_ID == 53],apply(unlined2018[unlined2018$STRATA_ID == 53,24:50],1,sum),
              unlined2019$TOW_NO[unlined2019$STRATA_ID == 53],apply(unlined2019[unlined2019$STRATA_ID == 53,27:50],1,sum),
              crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
ubcom11 <- spr(unlined2019$TOW_NO[unlined2019$STRATA_ID == 53],apply(unlined2019[unlined2019$STRATA_ID == 53,24:50],1,sum),
               unlined2021$TOW_NO[unlined2021$STRATA_ID == 53],apply(unlined2021[unlined2021$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom11, summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022
ubcom12 <- spr(unlined2021$TOW_NO[unlined2021$STRATA_ID == 53],apply(unlined2021[unlined2021$STRATA_ID == 53,24:50],1,sum),
               unlined2022$TOW_NO[unlined2022$STRATA_ID == 53],apply(unlined2022[unlined2022$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom12, summary(ubcom11, summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
ubcom13 <- spr(unlined2022$TOW_NO[unlined2022$STRATA_ID == 53],apply(unlined2022[unlined2022$STRATA_ID == 53,24:50],1,sum),
               unlined2023$TOW_NO[unlined2023$STRATA_ID == 53],apply(unlined2023[unlined2023$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom13,summary(ubcom12, summary(ubcom11, summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
ubcom14 <- spr(unlined2023$TOW_NO[unlined2023$STRATA_ID == 53],apply(unlined2023[unlined2023$STRATA_ID == 53,24:50],1,sum),
               unlined2024$TOW_NO[unlined2024$STRATA_ID == 53],apply(unlined2024[unlined2024$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom14, summary(ubcom13,summary(ubcom12, summary(ubcom11, summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
ubcom15 <- spr(unlined2024$TOW_NO[unlined2024$STRATA_ID == 53],apply(unlined2024[unlined2024$STRATA_ID == 53,24:50],1,sum),
               unlined2025$TOW_NO[unlined2025$STRATA_ID == 53],apply(unlined2025[unlined2025$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom15,summary(ubcom14, summary(ubcom13,summary(ubcom12, summary(ubcom11, summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom))))))))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
ubcom16 <- spr(unlined2025$TOW_NO[unlined2025$STRATA_ID == 53],apply(unlined2025[unlined2025$STRATA_ID == 53,24:50],1,sum),
               unlined2026$TOW_NO[unlined2026$STRATA_ID == 53],apply(unlined2026[unlined2026$STRATA_ID == 53,27:50],1,sum),
               crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 53,c("TOW_NO_REF","TOW_NO")])
K <- summary(ubcom16,summary(ubcom15,summary(ubcom14, summary(ubcom13,summary(ubcom12, summary(ubcom11, summary(ubcom10, summary(ubcom9, summary(ubcom8, summary(ubcom7, summary(ubcom6, summary(ubcom5, summary(ubcom4, summary(ubcom3, summary(ubcom2, summary(ubcom1, summary(ubcom)))))))))))))))))
UB.unlined.spr[UB.unlined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

UB.unlined.spr

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(UB.unlined.spr$Year, UB.unlined.spr$Yspr, xout=2020) #   65.83011
UB.unlined.spr[UB.unlined.spr$Year==2020,"Yspr"] <-  65.83011 

#make one dataframe for simple and spr estimates
names(UB.unlined.spr) <- c("Year", "unlined.UB", "var.y")
UB.Unlined <- rbind(UB.unlined.com.simple[UB.unlined.com.simple$Year < 2009,],UB.unlined.spr[,c("Year","unlined.UB")])



# ---- 3. 28D Outer ----
years <- 1997:surveyyear 
X <- length(years)

Out.unlined.com.simple <- data.frame(Year = years, unlined.Out = rep(NA,X))
for (i in 1:length(Out.unlined.com.simple$Year)) {
  temp.data <- unlined[unlined$YEAR == 1996 + i,]
    Out.unlined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 49 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
  }
Out.unlined.com.simple

#assume 1997-2000 same as MBN
Out.unlined.com.simple[Out.unlined.com.simple$Year < 2001,"unlined.Out"] <- MBN.Unlined[MBN.Unlined$Year < 2001,4]
#interpolate for other years
#2004
approx(Out.unlined.com.simple$Year, Out.unlined.com.simple$unlined.Out, xout = 2004) #89.44167
Out.unlined.com.simple[Out.unlined.com.simple$Year == 2004,"unlined.Out"] <- 89.44167
#2020
approx(Out.unlined.com.simple$Year, Out.unlined.com.simple$unlined.Out, xout = 2020) #25.81875
Out.unlined.com.simple[Out.unlined.com.simple$Year == 2020,"unlined.Out"] <- 25.81875


#dataframe for SPR estimates Commercial
spryears <- 2010:surveyyear 
Y <- length(spryears)
Out.unlined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2009/2010
outcom1<-spr(unlined2009$TOW_NO[unlined2009$STRATA_ID == 49],apply(unlined2009[unlined2009$STRATA_ID == 49,24:50],1,sum),
   unlined2010$TOW_NO[unlined2010$STRATA_ID == 49],apply(unlined2010[unlined2010$STRATA_ID == 49,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom1)
Out.unlined.spr[Out.unlined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
# outcom2<-spr(unlined2010$TOW_NO[unlined2010$STRATA_ID == 49],apply(unlined2010[unlined2010$STRATA_ID == 49,24:50],1,sum),
#    unlined2011$TOW_NO[unlined2011$STRATA_ID == 49],apply(unlined2011[unlined2011$STRATA_ID == 49,27:50],1,sum),
#     crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
# K<-summary(outcom2, summary(outcom1))
#essentially perfect fit: summary may be unreliable, use simple mean
#Out.unlined.spr[Out.unlined.spr$Year == 2011,c(2:3)] <- c(33.22222, NA)

#2011/2012
# outcom3<-spr(unlined2011$TOW_NO[unlined2011$STRATA_ID == 49],apply(unlined2011[unlined2011$STRATA_ID == 49,24:50],1,sum),
#    unlined2012$TOW_NO[unlined2012$STRATA_ID == 49],apply(unlined2012[unlined2012$STRATA_ID == 49,27:50],1,sum),
#     crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
# K<-summary(outcom3, summary(outcom2, summary(outcom1)))
#essentially perfect fit: summary may be unreliable, use simple mean
#Out.unlined.spr[Out.unlined.spr$Year == 2012,c(2:3)] <- c(18.21111, NA)

#2012/2013
# outcom4<-spr(unlined2012$TOW_NO[unlined2012$STRATA_ID == 49],apply(unlined2012[unlined2012$STRATA_ID == 49,24:50],1,sum),
#    unlined2013$TOW_NO[unlined2013$STRATA_ID == 49],apply(unlined2013[unlined2013$STRATA_ID == 49,27:50],1,sum),
#     crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
# K<-summary(outcom4, summary(outcom3, summary(outcom2, summary(outcom1))))
#essentially perfect fit: summary may be unreliable, use simple mean
#Out.unlined.spr[Out.unlined.spr$Year == 2013,c(2:3)] <- c(30.53750, NA)

#2013/2014
#outcom5<-spr(unlined2013$TOW_NO[unlined2013$STRATA_ID == 49],apply(unlined2013[unlined2013$STRATA_ID == 49,24:50],1,sum),
#   unlined2014$TOW_NO[unlined2014$STRATA_ID == 49],apply(unlined2014[unlined2014$STRATA_ID == 49,27:50],1,sum),
#    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])

#only 2 paired tows
#Out.unlined.spr[Out.unlined.spr$Year == 2014,c(2:3)] <- c(65.4375,NA)

#2014/2015
outcom5<-spr(unlined2014$TOW_NO[unlined2014$STRATA_ID == 49],apply(unlined2014[unlined2014$STRATA_ID == 49,24:50],1,sum),
   unlined2015$TOW_NO[unlined2015$STRATA_ID == 49],apply(unlined2015[unlined2015$STRATA_ID == 49,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom5)
Out.unlined.spr[Out.unlined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
outcom6<-spr(unlined2015$TOW_NO[unlined2015$STRATA_ID == 49],apply(unlined2015[unlined2015$STRATA_ID == 49,24:50],1,sum),
             unlined2016$TOW_NO[unlined2016$STRATA_ID == 49],apply(unlined2016[unlined2016$STRATA_ID == 49,27:50],1,sum),
             crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K<-summary(outcom6, summary(outcom5))
Out.unlined.spr[Out.unlined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
# outcom7<-spr(unlined2016$TOW_NO[unlined2016$STRATA_ID == 49],apply(unlined2016[unlined2016$STRATA_ID == 49,24:50],1,sum),
#              unlined2017$TOW_NO[unlined2017$STRATA_ID == 49],apply(unlined2017[unlined2017$STRATA_ID == 49,27:50],1,sum),
#              crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
# K<-summary(outcom7, summary(outcom6, summary(outcom5)))
#  essentially perfect fit: summary may be unreliable - use simplea mean
#Out.unlined.spr[Out.unlined.spr$Year == 2017,c(2:3)] <- c( 30.32500, NA)

#2017/2018
# outcom8<-spr(unlined2017$TOW_NO[unlined2017$STRATA_ID == 49],apply(unlined2017[unlined2017$STRATA_ID == 49,24:50],1,sum),
#              unlined2018$TOW_NO[unlined2018$STRATA_ID == 49],apply(unlined2018[unlined2018$STRATA_ID == 49,27:50],1,sum),
#              crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
# K<-summary(outcom8, summary(outcom7, summary(outcom6, summary(outcom5))))
# essentially perfect fit: summary may be unreliable - use simple mean
#Out.unlined.spr[Out.unlined.spr$Year == 2018,c(2:3)] <- c(122.73750, NA)

#2018/2019
outcom9 <- spr(unlined2018$TOW_NO[unlined2018$STRATA_ID == 49],apply(unlined2018[unlined2018$STRATA_ID == 49,24:50],1,sum),
              unlined2019$TOW_NO[unlined2019$STRATA_ID == 49],apply(unlined2019[unlined2019$STRATA_ID == 49,27:50],1,sum),
              crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom9)
Out.unlined.spr[Out.unlined.spr$Year == 2019,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2019/2021
outcom10 <- spr(unlined2019$TOW_NO[unlined2019$STRATA_ID == 49],apply(unlined2019[unlined2019$STRATA_ID == 49,24:50],1,sum),
               unlined2021$TOW_NO[unlined2021$STRATA_ID == 49],apply(unlined2021[unlined2021$STRATA_ID == 49,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom10)
Out.unlined.spr[Out.unlined.spr$Year == 2021,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)


#2021/2022
outcom11 <- spr(unlined2021$TOW_NO[unlined2021$STRATA_ID == 49],apply(unlined2021[unlined2021$STRATA_ID == 49,24:50],1,sum),
                unlined2022$TOW_NO[unlined2022$STRATA_ID == 49],apply(unlined2022[unlined2022$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom11)
Out.unlined.spr[Out.unlined.spr$Year == 2022,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2022/2023
outcom12 <- spr(unlined2022$TOW_NO[unlined2022$STRATA_ID == 49],apply(unlined2022[unlined2022$STRATA_ID == 49,24:50],1,sum),
                unlined2023$TOW_NO[unlined2023$STRATA_ID == 49],apply(unlined2023[unlined2023$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom12)
Out.unlined.spr[Out.unlined.spr$Year == 2023,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2023/2024
outcom13 <- spr(unlined2023$TOW_NO[unlined2023$STRATA_ID == 49],apply(unlined2023[unlined2023$STRATA_ID == 49,24:50],1,sum),
                unlined2024$TOW_NO[unlined2024$STRATA_ID == 49],apply(unlined2024[unlined2024$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom13)
Out.unlined.spr[Out.unlined.spr$Year == 2024,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2024/2025
outcom14 <- spr(unlined2024$TOW_NO[unlined2024$STRATA_ID == 49],apply(unlined2024[unlined2024$STRATA_ID == 49,24:50],1,sum),
                unlined2025$TOW_NO[unlined2025$STRATA_ID == 49],apply(unlined2025[unlined2025$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom14)
Out.unlined.spr[Out.unlined.spr$Year == 2025,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

#2025/2026
outcom15 <- spr(unlined2025$TOW_NO[unlined2025$STRATA_ID == 49],apply(unlined2025[unlined2025$STRATA_ID == 49,24:50],1,sum),
                unlined2026$TOW_NO[unlined2026$STRATA_ID == 49],apply(unlined2026[unlined2026$STRATA_ID == 49,27:50],1,sum),
                crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 49,c("TOW_NO_REF","TOW_NO")])
K <- summary(outcom15)
Out.unlined.spr[Out.unlined.spr$Year == 2026,c(2:3)] <-  c(K$Yspr, K$var.Yspr.corrected)

Out.unlined.spr

#make one dataframe for simple and spr estimates
names(Out.unlined.spr) <- c("Year", "unlined.Out", "var.y")
#use simple mean for pre 2009 and also 2018, 2019
replace.spr <- c(2011, 2012, 2013, 2014, 2017, 2018)
Out.Unlined <- rbind(Out.unlined.com.simple[Out.unlined.com.simple$Year < 2010,], Out.unlined.com.simple[Out.unlined.com.simple$Year %in% replace.spr,],Out.unlined.spr[!Out.unlined.spr$Year %in% replace.spr,c("Year","unlined.Out")])
Out.Unlined <- Out.Unlined %>% arrange(Year)
Out.Unlined

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(Out.Unlined$Year, Out.Unlined$unlined.Out, xout=2020) #  43.94173
Out.Unlined[Out.Unlined$Year==2020,"unlined.Out"] <- 43.94173 #assume var from 2019




# ---- 4. Advocate Harbour -----
years <- 1997:surveyyear 
X <- length(years)

AH.unlined.com.simple <- data.frame(Year = years, unlined.AH = rep(NA,X))
for (i in 1:length(AH.unlined.com.simple$Year)) {
  temp.data <- unlined[unlined$YEAR == 1996 + i,]
AH.unlined.com.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 35 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum))
  }
AH.unlined.com.simple

#assume 1997-2000 same as MBN
AH.unlined.com.simple[AH.unlined.com.simple$Year < 2001,"unlined.AH"] <- MBN.Unlined[MBN.Unlined$Year < 2001,4]
#interpolate for other years
#2004
approx(AH.unlined.com.simple$Year, AH.unlined.com.simple$unlined.AH, xout = 2004) #328.0458
AH.unlined.com.simple[AH.unlined.com.simple$Year == 2004,"unlined.AH"] <- 328.0458
#2020
approx(AH.unlined.com.simple$Year, AH.unlined.com.simple$unlined.AH, xout = 2020) # 604.8125
AH.unlined.com.simple[AH.unlined.com.simple$Year == 2020,"unlined.AH"] <-  604.8125



#dataframe for SPR estimates Commercial
spryears <- 2009:surveyyear #update to most recent year
Y <- length(spryears)
AH.unlined.spr <- data.frame(Year = spryears, Yspr = rep(NA,Y), var.Yspr.corrected = rep(NA,Y))

#2008/2009
ahcom<-spr(unlined2008$TOW_NO[unlined2008$STRATA_ID == 35],apply(unlined2008[unlined2008$STRATA_ID == 35,24:50],1,sum),
    unlined2009$TOW_NO[unlined2009$STRATA_ID == 35],apply(unlined2009[unlined2009$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2009[crossref.BoF.2009$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom)
AH.unlined.spr[AH.unlined.spr$Year == 2009,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2009/2010
ahcom1<-spr(unlined2009$TOW_NO[unlined2009$STRATA_ID == 35],apply(unlined2009[unlined2009$STRATA_ID == 35,24:50],1,sum),
    unlined2010$TOW_NO[unlined2010$STRATA_ID == 35],apply(unlined2010[unlined2010$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2010[crossref.BoF.2010$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom1, summary(ahcom))
AH.unlined.spr[AH.unlined.spr$Year == 2010,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2010/2011
ahcom2<-spr(unlined2010$TOW_NO[unlined2010$STRATA_ID == 35],apply(unlined2010[unlined2010$STRATA_ID == 35,24:50],1,sum),
    unlined2011$TOW_NO[unlined2011$STRATA_ID == 35],apply(unlined2011[unlined2011$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2011[crossref.BoF.2011$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom2, summary(ahcom1, summary(ahcom)))
AH.unlined.spr[AH.unlined.spr$Year == 2011,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2011/2012
ahcom3<-spr(unlined2011$TOW_NO[unlined2011$STRATA_ID == 35],apply(unlined2011[unlined2011$STRATA_ID == 35,24:50],1,sum),
    unlined2012$TOW_NO[unlined2012$STRATA_ID == 35],apply(unlined2012[unlined2012$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2012[crossref.BoF.2012$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))
AH.unlined.spr[AH.unlined.spr$Year == 2012,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2012/2013
ahcom4<-spr(unlined2012$TOW_NO[unlined2012$STRATA_ID == 35],apply(unlined2012[unlined2012$STRATA_ID == 35,24:50],1,sum),
    unlined2013$TOW_NO[unlined2013$STRATA_ID == 35],apply(unlined2013[unlined2013$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2013[crossref.BoF.2013$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))
AH.unlined.spr[AH.unlined.spr$Year == 2013,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2013/2014
ahcom5<-spr(unlined2013$TOW_NO[unlined2013$STRATA_ID == 35],apply(unlined2013[unlined2013$STRATA_ID == 35,24:50],1,sum),
    unlined2014$TOW_NO[unlined2014$STRATA_ID == 35],apply(unlined2014[unlined2014$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2014[crossref.BoF.2014$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))
AH.unlined.spr[AH.unlined.spr$Year == 2014,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2014/2015
ahcom6<-spr(unlined2014$TOW_NO[unlined2014$STRATA_ID == 35],apply(unlined2014[unlined2014$STRATA_ID == 35,24:50],1,sum),
    unlined2015$TOW_NO[unlined2015$STRATA_ID == 35],apply(unlined2015[unlined2015$STRATA_ID == 35,27:50],1,sum),
    crossref.BoF.2015[crossref.BoF.2015$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))
AH.unlined.spr[AH.unlined.spr$Year == 2015,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2015/2016
ahcom7<-spr(unlined2015$TOW_NO[unlined2015$STRATA_ID == 35],apply(unlined2015[unlined2015$STRATA_ID == 35,24:50],1,sum),
            unlined2016$TOW_NO[unlined2016$STRATA_ID == 35],apply(unlined2016[unlined2016$STRATA_ID == 35,27:50],1,sum),
            crossref.BoF.2016[crossref.BoF.2016$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2016,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2016/2017
ahcom8<-spr(unlined2016$TOW_NO[unlined2016$STRATA_ID == 35],apply(unlined2016[unlined2016$STRATA_ID == 35,24:50],1,sum),
            unlined2017$TOW_NO[unlined2017$STRATA_ID == 35],apply(unlined2017[unlined2017$STRATA_ID == 35,27:50],1,sum),
            crossref.BoF.2017[crossref.BoF.2017$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K<-summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2017,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2017/2018
ahcom9 <- spr(unlined2017$TOW_NO[unlined2017$STRATA_ID == 35],apply(unlined2017[unlined2017$STRATA_ID == 35,24:50],1,sum),
            unlined2018$TOW_NO[unlined2018$STRATA_ID == 35],apply(unlined2018[unlined2018$STRATA_ID == 35,27:50],1,sum),
            crossref.BoF.2018[crossref.BoF.2018$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2018,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2018/2019
ahcom10 <- spr(unlined2018$TOW_NO[unlined2018$STRATA_ID == 35],apply(unlined2018[unlined2018$STRATA_ID == 35,24:50],1,sum),
              unlined2019$TOW_NO[unlined2019$STRATA_ID == 35],apply(unlined2019[unlined2019$STRATA_ID == 35,27:50],1,sum),
              crossref.BoF.2019[crossref.BoF.2019$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2019,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2019/2021 #no survey in 2020 
ahcom11 <- spr(unlined2019$TOW_NO[unlined2019$STRATA_ID == 35],apply(unlined2019[unlined2019$STRATA_ID == 35,24:50],1,sum),
               unlined2021$TOW_NO[unlined2021$STRATA_ID == 35],apply(unlined2021[unlined2021$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2021[crossref.BoF.2021$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2021,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2021/2022  
ahcom12 <- spr(unlined2021$TOW_NO[unlined2021$STRATA_ID == 35],apply(unlined2021[unlined2021$STRATA_ID == 35,24:50],1,sum),
               unlined2022$TOW_NO[unlined2022$STRATA_ID == 35],apply(unlined2022[unlined2022$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2022[crossref.BoF.2022$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2022,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2022/2023  
ahcom13 <- spr(unlined2022$TOW_NO[unlined2022$STRATA_ID == 35],apply(unlined2022[unlined2022$STRATA_ID == 35,24:50],1,sum),
               unlined2023$TOW_NO[unlined2023$STRATA_ID == 35],apply(unlined2023[unlined2023$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2023[crossref.BoF.2023$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2023,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2023/2024  
ahcom14 <- spr(unlined2023$TOW_NO[unlined2023$STRATA_ID == 35],apply(unlined2023[unlined2023$STRATA_ID == 35,24:50],1,sum),
               unlined2024$TOW_NO[unlined2024$STRATA_ID == 35],apply(unlined2024[unlined2024$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2024[crossref.BoF.2024$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom14, summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2024,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2024/2025  
ahcom15 <- spr(unlined2024$TOW_NO[unlined2024$STRATA_ID == 35],apply(unlined2024[unlined2024$STRATA_ID == 35,24:50],1,sum),
               unlined2025$TOW_NO[unlined2025$STRATA_ID == 35],apply(unlined2025[unlined2025$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2025[crossref.BoF.2025$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom15, summary(ahcom14, summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom))))))))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2025,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

#2025/2026  
ahcom16 <- spr(unlined2025$TOW_NO[unlined2025$STRATA_ID == 35],apply(unlined2025[unlined2025$STRATA_ID == 35,24:50],1,sum),
               unlined2026$TOW_NO[unlined2026$STRATA_ID == 35],apply(unlined2026[unlined2026$STRATA_ID == 35,27:50],1,sum),
               crossref.BoF.2026[crossref.BoF.2026$STRATA_ID == 35,c("TOW_NO_REF","TOW_NO")])
K <- summary(ahcom16,summary(ahcom15, summary(ahcom14, summary(ahcom13, summary(ahcom12, summary(ahcom11, summary(ahcom10, summary(ahcom9, summary(ahcom8, summary(ahcom7, summary(ahcom6, summary(ahcom5, summary(ahcom4, summary(ahcom3, summary(ahcom2, summary(ahcom1, summary(ahcom)))))))))))))))))
AH.unlined.spr[AH.unlined.spr$Year == 2026,c(2:3)] <- c(K$Yspr, K$var.Yspr.corrected)

AH.unlined.spr

#in 2020 had no survey to linear interpolation from SPR estimate (note very different result from simple estimate)
approx(AH.unlined.spr$Year, AH.unlined.spr$Yspr, xout=2020) #   158.9014
AH.unlined.spr[AH.unlined.spr$Year==2020,"Yspr"] <-  158.9014 #assume var from 2019


#make one dataframe for simple and spr estimates
names(AH.unlined.spr) <- c("Year", "unlined.AH", "var.y")
AH.Unlined <- rbind(AH.unlined.com.simple[AH.unlined.com.simple$Year < 2009,],AH.unlined.spr[,c("Year","unlined.AH")])


# ---- 5-7.  Cape Spencer, Scots Bay, Spencers Island - simple mean ----
years <- 1997:surveyyear
X <- length(years)

unlined.nonSPR.com <- data.frame(Year = years, unlined.CS = rep(NA,X),unlined.SI = rep(NA,X), unlined.SB = rep(NA,X))
for (i in 1:length(unlined.nonSPR.com$Year)) {
  temp.data <- unlined[unlined$YEAR == 1996 + i,]
  unlined.nonSPR.com[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID == 37 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum)) #CS
  unlined.nonSPR.com[i,3] <- mean(apply(temp.data[temp.data$STRATA_ID == 52 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum)) #SI
  unlined.nonSPR.com[i,4] <- mean(apply(temp.data[temp.data$STRATA_ID == 51 & temp.data$TOW_TYPE_ID == 1, 27:50],1,sum)) #SB
  }
unlined.nonSPR.com

#assume 2001-2004 same as Outer
unlined.nonSPR.com[unlined.nonSPR.com$Year %in% c(2001,2002,2003,2004), "unlined.SI"] <- Out.unlined.com.simple[Out.unlined.com.simple$Year %in% c(2001,2002,2003,2004),"unlined.Out"]
unlined.nonSPR.com[unlined.nonSPR.com$Year %in% c(2001,2002,2003,2004), "unlined.SB"] <- Out.unlined.com.simple[Out.unlined.com.simple$Year %in% c(2001,2002,2003,2004),"unlined.Out"]
#assume 1997-2000 same as MBN
unlined.nonSPR.com[unlined.nonSPR.com$Year %in% c(1997,1998,1999,2000), "unlined.SI"] <- MBN.Unlined[MBN.Unlined$Year %in% c(1997, 1998, 1999, 2000),"unlined.MBN"]
unlined.nonSPR.com[unlined.nonSPR.com$Year %in% c(1997,1998,1999,2000), "unlined.SB"] <- MBN.Unlined[MBN.Unlined$Year %in% c(1997, 1998, 1999, 2000),"unlined.MBN"]
# 2020 interpolate 
approx(unlined.nonSPR.com$Year, unlined.nonSPR.com$unlined.CS, xout=2020) #  249.2759
unlined.nonSPR.com[unlined.nonSPR.com$Year==2020,"unlined.CS"] <- 249.2759 
approx(unlined.nonSPR.com$Year, unlined.nonSPR.com$unlined.SI , xout=2020) #  22.24
unlined.nonSPR.com[unlined.nonSPR.com$Year==2020,"unlined.SI"] <- 22.24 
approx(unlined.nonSPR.com$Year, unlined.nonSPR.com$unlined.SB , xout=2020) #  14.2375
unlined.nonSPR.com[unlined.nonSPR.com$Year==2020,"unlined.SB"] <- 14.2375 


unlined.nonSPR.com



# ---- calculate stratified mean for Lined ---- 
temp <- merge(unlined.nonSPR.com, MBN.Unlined[,c("Year","unlined.MBN")],by.x = "Year", all = TRUE)
temp2 <- merge(temp, UB.Unlined,by.x = "Year", all = TRUE)
temp3 <- merge(temp2, AH.Unlined,by.x = "Year", all = TRUE)
Unlined.1B <- merge(temp3, Out.Unlined, by.x = "Year", all = TRUE)

Unlined.1B$strat.Unlined <- (Unlined.1B$unlined.CS*0.261411226) + (Unlined.1B$unlined.SI*0.027831987) + (Unlined.1B$unlined.SB*0.02657028) + (Unlined.1B$unlined.MBN*0.402530584) + (Unlined.1B$unlined.UB*0.120426061) + (Unlined.1B$unlined.AH*0.011804834) + (Unlined.1B$unlined.Out*0.149425015)

ratiolined <- Lined.1B$strat.Lined/Unlined.1B$strat.Unlined


# Ouput a final object that is consistent with other areas
ratio <- data.frame(Year = years,Lined = Lined.1B$strat.Lined, Unlined = Unlined.1B$strat.Unlined,ratiolined = ratiolined)

# Create a new Ratio file, the most recent year will be added to the model using the SPA1B_ModelFile.R
write.csv(ratio, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1B_ratioLinedtoUnlined",surveyyear,".csv"))


# ---- Plot ratio over time ----
ratio.plot <- ggplot(ratio, aes(x=Year, y=ratiolined)) + 
  geom_point() + 
  geom_line() + ylab("Ratio") + 
  scale_x_continuous(limits = c(min(ratio$Year), max(ratio$Year+1))) + 
  theme_bw()
ratio.plot


png(paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA1B_Ratio.png"), type="cairo", width=15, height=15, units = "cm", res=400)
ratio.plot
dev.off()
