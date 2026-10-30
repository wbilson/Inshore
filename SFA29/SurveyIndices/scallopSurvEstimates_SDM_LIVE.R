###..........................###
### Survey Scallop estimates ###
###         SDM              ###
###     J. Sameoto           ###
###..........................###
# Updated Jan 2016 . J.Sameoto
# Updated Jan 2019 A.Taylor
# Rehauled Dec 2021 J.Sameoto

# Note: in PEDStrata for R v.3 must have:  obj.Strata <- as.character(obj.Strata[s.obj])
## NO LONGER REQUIRED: Bring in library but then bring in PEDstrata function manually from file: PEDStrata.txt under: Y:\Jessica\SFA29\2014\r
## PEDstrata v 1.0.1 updated

options(stringsAsFactors=FALSE)
library(PEDstrata)
library(lubridate)
library(ROracle)
library(tidyverse)
library(ggplot2)

setwd('Y:/Inshore/Assessment/BoF/Assessment_fns/SFA29W')
source('Geophysicalareas.R')
source('SedimentareasSFA29.R')
source('Domainestimates.R')
source('post.stratify.all.R')
source('SDMareas.R')
#source('SFA29sediment2005-2007.R') #tow numbers with assigned surficial substrata strata - done pre-Jessica (likely by J.Black)
# Tow with surficial assignments From 2001 to 2013 done by J.Sameoto:
# Use new file: SFA2920012013_Surficial_FinalJuly162014.csv
surf.all <- read.csv('SFA2920012013_Surficial_FinalJuly162014.csv')
# note but will only use surficial data use for 2005, 2006, 2007:
surf.all <- surf.all[,c("uid","surf")]
names(surf.all) <- c("uid","Start.Bottom")


# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year for which you want to include  - not should match year of cruise below 
cruise <- "SFA292026"  #note should match year for surveyyear set above 
assessmentyear <- 2027 #year in which you are conducting the survey 
path.directory <- "Y:/Inshore/Assessment/SFA29/"
years <- c(2001:surveyyear) #when have 2021 data ready with SDM value then can use line of code below 
#yr.crnt <- surveyyear-1

#Bring in survey tow data with SDM value (note - SFA29_SDM_LWM.R script must be run to get updated survey tows with SDM values prior to running this script)
sdmtows <- read.csv("Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv")
table(sdmtows$CRUISE)
sdmtows$uid <- paste(sdmtows$CRUISE, sdmtows$TOW_NO, sep=".")
sdmtows <- sdmtows[,c("uid","SDM")]

#towable units by subarea and strata - Copy over from previous years directory.
towable.units <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Scripts/Model/SFA29W_model_areas_towable_units.csv")) 


###.............###
### Import data ###
###.............###
# Get the cruises
cruise.list <- paste0("SFA29",(2001:(surveyyear)))
cruise.list <- paste(cruise.list,collapse="','")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Note: data standardized to 800m tow length and 17.5 feet width (17.5 feet width consistent with scallop proration) #

quer2 <- paste(
		"SELECT *                                     ",
		"FROM scallsur.SCLIVERES                      ",
		"WHERE strata_id IN (41, 42, 43, 44, 45)      ",
		"AND (cruise in ('",cruise.list,"'))          ",
		sep=""
	    )

# Run for ROracle
data.obj <- dbGetQuery(chan, quer2)

# Check to make sure all req cruises are loaded
unique(data.obj$CRUISE)
# Check to see if any NA's in data & remove records with NAs in data - but should never have NAs in SHF field direct from database 
dim(data.obj)
summary(data.obj)
#  data.obj <- data.obj[!is.na(rowSums(data.obj[,c(grep("BIN_ID_0",names(data.obj)):grep("BIN_ID_195",names(data.obj)))])),]
#	dim(data.obj)
# Assign subarea names
strata.names <- paste0("SFA29",LETTERS[1:5])
strata.ids <- sort(unique(data.obj$STRATA_ID))
data.obj$STRATA <- NA
for(i in 1:length(strata.ids)) data.obj$STRATA[data.obj$STRATA_ID==strata.ids[i]] <- strata.names[i]

# Assign year
data.obj$YEAR <- as.numeric(substr(data.obj$CRUISE,6,9))

# Calc unique id
data.obj$uid <- paste(data.obj$CRUISE, data.obj$TOW_NO, sep=".")

#Pre-recruits per tow 0 to 89 mm SH;   note for Prerecruits for 2013 assessment - just used 20-60mm#
data.obj$prerec <-  data.obj %>% dplyr::select(grep("BIN_ID_0",names(data.obj)):grep("BIN_ID_85",names(data.obj))) %>%  rowSums(na.rm=TRUE)

#recruits per tow 90 to 99 mm SH 
data.obj$rec <- data.obj %>% dplyr::select(grep("BIN_ID_90",names(data.obj)):grep("BIN_ID_95",names(data.obj))) %>%  rowSums(na.rm=TRUE)

#commercial	per tow >= 100 mm SH 
data.obj$comm <- data.obj %>% dplyr::select(grep("BIN_ID_100",names(data.obj)):grep("BIN_ID_195",names(data.obj))) %>%  rowSums(na.rm=TRUE)

# Left join survey tows to surficial substrate tows on uid
data.obj <- merge(data.obj, surf.all, by.x='uid', by.y='uid', all.x=TRUE)

# Left join survey tows to SDM level on uid
data.obj <- merge(data.obj, sdmtows, by.x='uid', by.y='uid', all.x=TRUE)
dim(data.obj)
summary(data.obj)

#In 2024 did tows outside 29W and also in non-MBES covered part of B; remove these so apples and apples with previous estimates 
data.obj %>% filter(CRUISE == "SFA292024" & is.na(SDM) == TRUE)
dim(data.obj)

data.obj <- data.obj[!(data.obj$CRUISE == "SFA292024" & is.na(data.obj$SDM) == TRUE),]
dim(data.obj)

#Remove tow 92 from B low - large number of commercial size.
#data.obj <- data.obj[!(data.obj$CRUISE == "SFA292025" & data.obj$TOW_NO == 92),]
#dim(data.obj)

data.obj.all <- data.obj


###
###  ----    Calculate Stratified Random Survey Estimates  ---- 
###   PEDstrata(data.obj, strata.group, strata.name, catch, Subset)                            ###
###   Domain.estimates(data, Strata, Domain, strata.obj, domain.obj, Nd = NULL)                ###
###   post.stratify.all(data, OldStrata, pStrata, Strata.info, oldstrata.name, newstrata.name) ###
###


# ---- PRE-RECRUITS ----	
# NOTE this section of code is run once for each size (comm, rec, prerec). Define below
strata.group <- SDMareas
size <- "prerec" # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column prerec, rec, or comm)
data.obj$STDTOTALCAUGHT <- data.obj$prerec # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column prerec, rec, or comm)

# Only use regular survey tows for estimation (TOW_TYPE_ID = 1)
data.obj <- data.obj[data.obj$TOW_TYPE_ID==1,]
dim(data.obj)
head(data.obj)

###... For Years:  2001-2004 Calculate Stratified Random Survey Estimate - SDM strata ...###
# NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
### DATA: scall.levels.2001to2004 (data by low, med, high), out.strat (stratified est over all subarea)
ab <- unique(strata.group$Subarea)
year <- c(2001,2002,2003,2004) # update year list to add a new year and update "rep(NA,#) below
out <- data.frame(YEAR=rep(NA,16),SUBAREA=rep(NA,16),yst=rep(NA,16),se.yst=rep(NA,16),descrip=rep("post.stratified",16)) # update number in rep(NA,#), #+5 when add new year of data
m <- 0 #index
scall.levels.2001to2004 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      #stratified estimates
      scall.sum <- post.stratify.all(data.obj.i$STDTOTALCAUGHT, data.obj.i$STRATA, data.obj.i$SDM, Strata.info=strata.group,  oldstrata.name="Subarea",newstrata.name="Strata")
      out[m,(3:4)] <- as.data.frame(scall.sum[1])[,2:3]
      
      #estimates by strata
      scall.str <- as.data.frame(t(as.data.frame(scall.sum[2])))
      names(scall.str) <- "ybd"
      scall.str$LEVEL <-  row.names(scall.str)
      scall.str$YEAR <- rep(year[i],dim(scall.str)[1])
      scall.str$SUBAREA <- rep(ab[j],dim(scall.str)[1])
      var.obs <- tapply(data.obj.i$STDTOTALCAUGHT,data.obj.i$SDM, var) #Calculate variance by strata
      var.ybd <- as.data.frame(var.obs)
      var.ybd$LEVEL <- row.names(var.ybd)
      #var.ybd$var.ybd <- as.vector(var.ybd$var.ybd)
      scall.str <- merge(scall.str, var.ybd, by.x=c("LEVEL"),by.y=("LEVEL"), all.x=TRUE)
      scall.n <- as.data.frame(t(as.data.frame(scall.sum[3])))
      names(scall.n) <-"n"
      scall.n$LEVEL <- row.names(scall.n)
      scall.str <- merge(scall.str, scall.n, by.x=c("LEVEL"),by.y=("LEVEL"), all.x=TRUE)
      scall.levels.2001to2004[[m]] <- scall.str
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}
out
out.strat <- out
scall.levels.2001to2004 <- as.data.frame(do.call(rbind,scall.levels.2001to2004))
scall.levels.2001to2004$se.yst <- sqrt(scall.levels.2001to2004$var.obs)/sqrt(scall.levels.2001to2004$n)
scall.levels.2001to2004$var.est <- scall.levels.2001to2004$var.obs/scall.levels.2001to2004$n
scall.levels.2001to2004 <- scall.levels.2001to2004[scall.levels.2001to2004$n!=0,]# Remove records where n = 0; i.e. remove records for which there were no tows

###... For Years: 2005-2007 Calculate Domain Estimate by subarea for SURFICIAL to SDM strata ...###
###... SUBAREA A ...### DATA ITEMS: scall.est.A (all data), scall.levels.A.2005to2007 (data by low, med, high), out.domain.surf.a  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29A'
scall.est.A <- list()
scall.levels.A.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.A, strata.group.i)
    scall.est.A[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.A <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.A$LEVEL <- row.names(scall.levels.A)
    scall.levels.A$YEAR <- rep(year[i],3)
    scall.levels.A$SUBAREA <- rep(subarea,3)
    scall.levels.A.2005to2007[[m]] <- scall.levels.A
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.a <- out
scall.levels.A.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.A.2005to2007 ))


###... SUBAREA B ...### DATA ITEMS: scall.est.B (all data), scall.levels.B.2005to2007 (data by low, med, high), out.domain.surf.b  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29B'
scall.est.B <- list()
scall.levels.B.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.B, strata.group.i)
    scall.est.B[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.B <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.B$LEVEL <- row.names(scall.levels.B)
    scall.levels.B$YEAR <- rep(year[i],3)
    scall.levels.B$SUBAREA <- rep(subarea,3)
    scall.levels.B.2005to2007[[m]] <- scall.levels.B
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.b <- out
scall.levels.B.2005to2007  <- as.data.frame(do.call(rbind,scall.levels.B.2005to2007 ))


###... SUBAREA C ...### DATA ITEMS: scall.est.C (all data), scall.levels.C.2005to2007 (data by low, med, high), out.domain.surf.c  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29C'
scall.est.C <- list()
scall.levels.C.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.C, strata.group.i)
    scall.est.C[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.C <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.C$LEVEL <- row.names(scall.levels.C)
    scall.levels.C$YEAR <- rep(year[i],3)
    scall.levels.C$SUBAREA <- rep(subarea,3)
    scall.levels.C.2005to2007[[m]] <- scall.levels.C
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.c <- out
scall.levels.C.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.C.2005to2007))


###... SUBAREA D ...### DATA ITEMS: scall.est.D (all data), scall.levels.D.2005to2007 (data by low, med, high), out.domain.surf.d  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29D'
scall.est.D <- list()
scall.levels.D.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.D, strata.group.i)
    scall.est.D[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.D <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.D$LEVEL <- row.names(scall.levels.D)
    scall.levels.D$YEAR <- rep(year[i],3)
    scall.levels.D$SUBAREA <- rep(subarea,3)
    scall.levels.D.2005to2007[[m]] <- scall.levels.D
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.d <- out
scall.levels.D.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.D.2005to2007))

# Merge all 2005 to 2007 data together
scall.levels.2005to2007 <- rbind(scall.levels.A.2005to2007, scall.levels.B.2005to2007, scall.levels.C.2005to2007, scall.levels.D.2005to2007)
out.domain.surf <- rbind(out.domain.surf.a, out.domain.surf.b, out.domain.surf.c, 	out.domain.surf.d)

###... For Years: 2008-2013 Calculate Domain Estimate by subarea for GEOPHYSICAL (GEOPHYS_ID) to SDM strata ...###
### DATA: scall.dom.2008to2013(data all), scall.levels.2008to2013 (data by low, med, high), out.domain.geophys (stratified est over all subarea)

ab <- unique(strata.group$Subarea)
year <- c(2008, 2009, 2010, 2011, 2012, 2013)
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),var.yst=rep(NA,(length(year)*length(ab))),descrip=rep('domain',(length(year)*length(ab))))
m <- 0 #index
scall.dom.2008to2013 <- list()
scall.levels.2008to2013 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      
      scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$GEOPHYS_ID, data.obj.i$SDM, RevisedstrataareasSFA29.revAugust2[RevisedstrataareasSFA29.revAugust2$Subarea==ab[j],], strata.group.i)
      scall.dom.2008to2013[[m]] <- c(YR=year[i], SUBAREA=ab[j], scall.dom)
      scall.sum <- summary.domain.est(scall.dom)
      out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
      
      scall.levels <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
      scall.levels$LEVEL <- row.names(scall.levels)
      scall.levels$YEAR <- rep(year[i],3)
      scall.levels$SUBAREA <- rep(ab[j],3)
      scall.levels.2008to2013[[m]] <- scall.levels
      
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}

out
out.domain.geophys <- out
scall.levels.2008to2013 <- as.data.frame(do.call(rbind,scall.levels.2008to2013))


###... For Years: 2014-YYYY Calculate Stratified Random Survey Estimate - SDM strata ...###
### NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
### DATA: scall.strat.2014toYYYY(data all - stratified SDM estimate), scall.levels.2014toYYYY (data by low, med, high)

ab <- unique(strata.group$Subarea)
year <- c(2014:2019,2021:surveyyear)
#out <- #data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(#length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),effic.alloc=rep(NA,(length(year)*length(ab))),effic.str=rep(NA,(length(year)*length(ab))),var.ran=rep(NA,(length(year)*length(ab))),max.eff=rep(NA,(length(yea#r)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))
#Paired down output bc must set effic = FALSE and nopt = FALSE if zero catch in all tows. Get a reduced output from summary.PEDstrata when set these 2 options to FALSE
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))

descrip=rep(NA,(length(year)*length(ab)))
m <- 0 #index
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      scall.sum <- summary(PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT),effic = FALSE, nopt = FALSE) #set effic and nopt to TRUE to get Vrandom
      #out[m,-(1:2)] <- scall.sum[c(-6,-11)] #use if set effic=TRUE and nopt=TRUE
      out[m,-(1:2)] <- scall.sum[c(-6)]
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}
out
out.strat.2014toCRNT <- out

#Calculate mean and variance within each SDM strata (High, Medium, Low)
#NOTE: could also obtain this if added scall.levels <- PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT) in the above loop and then assigned all outputs within each loop to a list

#View(data.obj %>% filter(YEAR == 2024 & STRATA == "SFA29B")) #getting NA for B in 2024; getting NAs in dataframe but not coming from database.. 

sdmlevels <- c("high", "med", "low")
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab)*length(sdmlevels))),SUBAREA=rep(NA,(length(year)*length(ab)*length(sdmlevels))),Strata=rep(NA,(length(year)*length(ab)*length(sdmlevels))),yst=rep(NA,(length(year)*length(ab)*length(sdmlevels))),se.yst=rep(NA,(length(year)*length(ab)*length(sdmlevels))),var.est=rep(NA,(length(year)*length(ab)*length(sdmlevels))),descrip=rep("simple",(length(year)*length(ab)*length(sdmlevels))))
m <- 0 #index
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  #temp.data[is.na(temp.data$uid),]
  for(j in 1:length(ab)) {
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    #data.obj.i[is.na(data.obj.i$uid),]
    for (k in 1:length(sdmlevels)){
      data.obj.k <- data.obj.i[data.obj.i$SDM==sdmlevels[k],]
      #data.obj.k[is.na(data.obj.k$uid),]  ## in 2024, NAs introduced in data.obj.k.. added line 412 to deal with this and added na.rm = TRUE to line 415 
      data.obj.k <- data.obj.k[!is.na(data.obj.k$uid),]
      m=m+1
      if(dim(data.obj.k)[1]!=0){
        out[m,"yst"] <- mean(data.obj.k$STDTOTALCAUGHT, na.rm = TRUE)
        out[m,"se.yst"] <- sqrt(var(data.obj.k$STDTOTALCAUGHT))/sqrt(dim(data.obj.k)[1])
        out[m,"var.est"]<- var(data.obj.k$STDTOTALCAUGHT)/dim(data.obj.k)[1] #var.est is calculated as variance of the estimator
      }
      out$Strata[m] <- sdmlevels[k]
      out$YEAR[m] <- year[i]
      out$SUBAREA[m] <- as.character(ab[j])
    }
  }
}
out
scall.levels.2014toCRNT <- out

### ... Merge data for all years into 1 dataframe ... ###
# Data by Strata: High, Medium, Low; by subare by year
names(scall.levels.2001to2004)[2] <- "Mean"
names(scall.levels.2001to2004)[7] <- "Std. Err."
names(scall.levels.2001to2004)[1] <- "Strata"

names(scall.levels.2005to2007)[1] <- "Mean"
names(scall.levels.2005to2007)[4] <- "Std. Err."
names(scall.levels.2005to2007)[5] <- "Strata"
names(scall.levels.2005to2007)[2] <- "var.est"

names(scall.levels.2008to2013)[1] <- "Mean"
names(scall.levels.2008to2013)[4] <- "Std. Err."
names(scall.levels.2008to2013)[5] <- "Strata"
names(scall.levels.2008to2013)[2] <- "var.est"

names(scall.levels.2014toCRNT)[4] <- "Mean"
names(scall.levels.2014toCRNT)[5] <- "Std. Err."

sdm.levels.est.all <- rbind(scall.levels.2001to2004[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2005to2007[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")],
                            scall.levels.2008to2013[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2014toCRNT[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")])
sdm.levels.est.all <- sdm.levels.est.all[order(sdm.levels.est.all$YEAR, sdm.levels.est.all$SUBAREA, sdm.levels.est.all$Strata), ]
sdm.levels.est.all$Strata <- as.factor(sdm.levels.est.all$Strata)
## AT: Include CV for model input
sdm.levels.est.all$CV <- log((sdm.levels.est.all$"Std. Err."/sdm.levels.est.all$Mean)^2 + 1)  #JS Dec 2021 - need to check this not sure this right 
sdm.levels.est.all$size <- size
names(sdm.levels.est.all)[grep("Std. Err.",names(sdm.levels.est.all))] <- "Std.Err"

#Add 2020 when had no survey
sdm.levels.est.all <- rbind(sdm.levels.est.all, data.frame(YEAR = rep(2020, 12), SUBAREA = c(rep("SFA29A",3),rep("SFA29B",3),rep("SFA29C",3),rep("SFA29D",3)), Strata = rep(c("low","med","high"),4), Mean=rep(NA,12), Std.Err=rep(NA,12),  var.est=rep(NA,12), CV=rep(NA,12), size = rep(size,12)) )

sdm.levels.est.all <- sdm.levels.est.all %>% arrange(SUBAREA,YEAR, Strata)


#Subarea A - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29A" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea A - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29A" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea B - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea B - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea B - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea C - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea C - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea C - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea D - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019


#Subarea D - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea D - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

sdm.levels.prerec <- sdm.levels.est.all
sdm.levels.prerec


#writeout data 
#write.csv(sdm.levels.prerec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.",size,".csv"))



# Stratified estimates (one estimate for each subarea by year)
out.domain.surf$se.yst <- sqrt(out.domain.surf$var.yst)
out.domain.geophys$se.yst <- sqrt(out.domain.geophys$var.yst)
sdm.strat.est.all <- rbind(out.strat[,c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.surf[, c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.geophys[, c("YEAR","SUBAREA","yst","se.yst","descrip")],out.strat.2014toCRNT[,c("YEAR","SUBAREA","yst","se.yst","descrip")] )
sdm.strat.est.all$rel.err <- sdm.strat.est.all$se.yst/sdm.strat.est.all$yst # relative error = standard error divided by the mean
sdm.strat.est.all <- sdm.strat.est.all[with(sdm.strat.est.all,order(sdm.strat.est.all$YEAR, sdm.strat.est.all$SUBAREA)),]
sdm.strat.est.all$size <- size

#Add 2020 when had no survey 
sdm.strat.est.all <- rbind(sdm.strat.est.all, data.frame(YEAR = rep(2020, 4), SUBAREA = c("SFA29A","SFA29B","SFA29C","SFA29D"),  yst =rep(NA,4), se.yst=rep(NA,4), descrip=rep(NA,4), rel.err=rep(NA,4), size = rep(size,4)) )

sdm.strat.est.all <- sdm.strat.est.all %>% arrange(SUBAREA,YEAR)

#subarea A 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29A" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29A"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29A"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29A"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29A"& sdm.strat.est.all$YEAR==2019])  

#subarea B 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29B" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29B"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29B"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29B"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29B"& sdm.strat.est.all$YEAR==2019])  

#subarea C 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29C" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29C"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29C"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29C"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29C"& sdm.strat.est.all$YEAR==2019])  

#subarea D 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29D" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29D"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29D"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29D"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29D"& sdm.strat.est.all$YEAR==2019])  

sdm.strat.est.all.prerec <- sdm.strat.est.all
sdm.strat.est.all.prerec

#writeout data 
#write.csv(sdm.strat.est.all.prerec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Numbers.",size,".csv"))




# ---- RECRUITS ---- 
# NOTE this section of code is run once for each size (comm, rec, prerec). Define below
strata.group <- SDMareas
size <- "rec" # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column prerec, rec, or comm)
data.obj$STDTOTALCAUGHT <- data.obj$rec # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column prerec, rec, or comm)

# Only use regular survey tows for estimation (TOW_TYPE_ID = 1)
data.obj <- data.obj[data.obj$TOW_TYPE_ID==1,]
dim(data.obj)
head(data.obj)

###... For Years:  2001-2004 Calculate Stratified Random Survey Estimate - SDM strata ...###
# NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
### DATA: scall.levels.2001to2004 (data by low, med, high), out.strat (stratified est over all subarea)
ab <- unique(strata.group$Subarea)
year <- c(2001,2002,2003,2004) # update year list to add a new year and update "rep(NA,#) below
out <- data.frame(YEAR=rep(NA,16),SUBAREA=rep(NA,16),yst=rep(NA,16),se.yst=rep(NA,16),descrip=rep("post.stratified",16)) # update number in rep(NA,#), #+5 when add new year of data
m <- 0 #index
scall.levels.2001to2004 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      #stratified estimates
      scall.sum <- post.stratify.all(data.obj.i$STDTOTALCAUGHT, data.obj.i$STRATA, data.obj.i$SDM, Strata.info=strata.group,  oldstrata.name="Subarea",newstrata.name="Strata")
      out[m,(3:4)] <- as.data.frame(scall.sum[1])[,2:3]
      
      #estimates by strata
      scall.str <- as.data.frame(t(as.data.frame(scall.sum[2])))
      names(scall.str) <- "ybd"
      scall.str$LEVEL <-  row.names(scall.str)
      scall.str$YEAR <- rep(year[i],dim(scall.str)[1])
      scall.str$SUBAREA <- rep(ab[j],dim(scall.str)[1])
      var.obs <- tapply(data.obj.i$STDTOTALCAUGHT,data.obj.i$SDM, var) #Calculate variance by strata
      var.ybd <- as.data.frame(var.obs)
      var.ybd$LEVEL <- row.names(var.ybd)
      #var.ybd$var.ybd <- as.vector(var.ybd$var.ybd)
      scall.str <- merge(scall.str, var.ybd, by.x=c("LEVEL"),by.y=("LEVEL"), all.x=TRUE)
      scall.n <- as.data.frame(t(as.data.frame(scall.sum[3])))
      names(scall.n) <-"n"
      scall.n$LEVEL <- row.names(scall.n)
      scall.str <- merge(scall.str, scall.n, by.x=c("LEVEL"),by.y=("LEVEL"), all.x=TRUE)
      scall.levels.2001to2004[[m]] <- scall.str
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}
out
out.strat <- out
scall.levels.2001to2004 <- as.data.frame(do.call(rbind,scall.levels.2001to2004))
scall.levels.2001to2004$se.yst <- sqrt(scall.levels.2001to2004$var.obs)/sqrt(scall.levels.2001to2004$n)
scall.levels.2001to2004$var.est <- scall.levels.2001to2004$var.obs/scall.levels.2001to2004$n
scall.levels.2001to2004 <- scall.levels.2001to2004[scall.levels.2001to2004$n!=0,]# Remove records where n = 0; i.e. remove records for which there were no tows

###... For Years: 2005-2007 Calculate Domain Estimate by subarea for SURFICIAL to SDM strata ...###
###... SUBAREA A ...### DATA ITEMS: scall.est.A (all data), scall.levels.A.2005to2007 (data by low, med, high), out.domain.surf.a  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29A'
scall.est.A <- list()
scall.levels.A.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.A, strata.group.i)
    scall.est.A[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.A <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.A$LEVEL <- row.names(scall.levels.A)
    scall.levels.A$YEAR <- rep(year[i],3)
    scall.levels.A$SUBAREA <- rep(subarea,3)
    scall.levels.A.2005to2007[[m]] <- scall.levels.A
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.a <- out
scall.levels.A.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.A.2005to2007 ))


###... SUBAREA B ...### DATA ITEMS: scall.est.B (all data), scall.levels.B.2005to2007 (data by low, med, high), out.domain.surf.b  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29B'
scall.est.B <- list()
scall.levels.B.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.B, strata.group.i)
    scall.est.B[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.B <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.B$LEVEL <- row.names(scall.levels.B)
    scall.levels.B$YEAR <- rep(year[i],3)
    scall.levels.B$SUBAREA <- rep(subarea,3)
    scall.levels.B.2005to2007[[m]] <- scall.levels.B
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.b <- out
scall.levels.B.2005to2007  <- as.data.frame(do.call(rbind,scall.levels.B.2005to2007 ))


###... SUBAREA C ...### DATA ITEMS: scall.est.C (all data), scall.levels.C.2005to2007 (data by low, med, high), out.domain.surf.c  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29C'
scall.est.C <- list()
scall.levels.C.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.C, strata.group.i)
    scall.est.C[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.C <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.C$LEVEL <- row.names(scall.levels.C)
    scall.levels.C$YEAR <- rep(year[i],3)
    scall.levels.C$SUBAREA <- rep(subarea,3)
    scall.levels.C.2005to2007[[m]] <- scall.levels.C
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.c <- out
scall.levels.C.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.C.2005to2007))


###... SUBAREA D ...### DATA ITEMS: scall.est.D (all data), scall.levels.D.2005to2007 (data by low, med, high), out.domain.surf.d  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29D'
scall.est.D <- list()
scall.levels.D.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.D, strata.group.i)
    scall.est.D[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.D <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.D$LEVEL <- row.names(scall.levels.D)
    scall.levels.D$YEAR <- rep(year[i],3)
    scall.levels.D$SUBAREA <- rep(subarea,3)
    scall.levels.D.2005to2007[[m]] <- scall.levels.D
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.d <- out
scall.levels.D.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.D.2005to2007))

# Merge all 2005 to 2007 data together
scall.levels.2005to2007 <- rbind(scall.levels.A.2005to2007, scall.levels.B.2005to2007, scall.levels.C.2005to2007, scall.levels.D.2005to2007)
out.domain.surf <- rbind(out.domain.surf.a, out.domain.surf.b, out.domain.surf.c, 	out.domain.surf.d)

###... For Years: 2008-2013 Calculate Domain Estimate by subarea for GEOPHYSICAL (GEOPHYS_ID) to SDM strata ...###
### DATA: scall.dom.2008to2013(data all), scall.levels.2008to2013 (data by low, med, high), out.domain.geophys (stratified est over all subarea)

ab <- unique(strata.group$Subarea)
year <- c(2008, 2009, 2010, 2011, 2012, 2013)
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),var.yst=rep(NA,(length(year)*length(ab))),descrip=rep('domain',(length(year)*length(ab))))
m <- 0 #index
scall.dom.2008to2013 <- list()
scall.levels.2008to2013 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      
      scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$GEOPHYS_ID, data.obj.i$SDM, RevisedstrataareasSFA29.revAugust2[RevisedstrataareasSFA29.revAugust2$Subarea==ab[j],], strata.group.i)
      scall.dom.2008to2013[[m]] <- c(YR=year[i], SUBAREA=ab[j], scall.dom)
      scall.sum <- summary.domain.est(scall.dom)
      out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
      
      scall.levels <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
      scall.levels$LEVEL <- row.names(scall.levels)
      scall.levels$YEAR <- rep(year[i],3)
      scall.levels$SUBAREA <- rep(ab[j],3)
      scall.levels.2008to2013[[m]] <- scall.levels
      
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}

out
out.domain.geophys <- out
scall.levels.2008to2013 <- as.data.frame(do.call(rbind,scall.levels.2008to2013))


###... For Years: 2014-YYYY Calculate Stratified Random Survey Estimate - SDM strata ...###
### NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
### DATA: scall.strat.2014toYYYY(data all - stratified SDM estimate), scall.levels.2014toYYYY (data by low, med, high)

ab <- unique(strata.group$Subarea)
year <- c(2014:2019,2021:surveyyear)
#out <- #data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(#length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),effic.alloc=rep(NA,(length(year)*length(ab))),effic.str=rep(NA,(length(year)*length(ab))),var.ran=rep(NA,(length(year)*length(ab))),max.eff=rep(NA,(length(yea#r)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))
#Paired down output bc must set effic = FALSE and nopt = FALSE if zero catch in all tows. Get a reduced output from summary.PEDstrata when set these 2 options to FALSE
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))

descrip=rep(NA,(length(year)*length(ab)))
m <- 0 #index
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      scall.sum <- summary(PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT),effic = FALSE, nopt = FALSE) #set effic and nopt to TRUE to get Vrandom
      #out[m,-(1:2)] <- scall.sum[c(-6,-11)] #use if set effic=TRUE and nopt=TRUE
      out[m,-(1:2)] <- scall.sum[c(-6)]
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}
out
out.strat.2014toCRNT <- out

#Calculate mean and variance within each SDM strata (High, Medium, Low)
#NOTE: could also obtain this if added scall.levels <- PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT) in the above loop and then assigned all outputs within each loop to a list
sdmlevels <- na.omit(unique(data.obj$SDM))
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab)*length(sdmlevels))),SUBAREA=rep(NA,(length(year)*length(ab)*length(sdmlevels))),Strata=rep(NA,(length(year)*length(ab)*length(sdmlevels))),yst=rep(NA,(length(year)*length(ab)*length(sdmlevels))),se.yst=rep(NA,(length(year)*length(ab)*length(sdmlevels))),var.est=rep(NA,(length(year)*length(ab)*length(sdmlevels))),descrip=rep("simple",(length(year)*length(ab)*length(sdmlevels))))
m <- 0 #index
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    for (k in 1:length(sdmlevels)){
      data.obj.k <- data.obj.i[data.obj.i$SDM==sdmlevels[k],]
      #data.obj.k[is.na(data.obj.k$uid),]  ## in 2024, NAs introduced in data.obj.k.. added line 412 to deal with this and added na.rm = TRUE to line 415 
      data.obj.k <- data.obj.k[!is.na(data.obj.k$uid),]
      m=m+1
      if(dim(data.obj.k)[1]!=0){
        out[m,"yst"] <- mean(data.obj.k$STDTOTALCAUGHT, na.rm = TRUE)
        out[m,"se.yst"] <- sqrt(var(data.obj.k$STDTOTALCAUGHT))/sqrt(dim(data.obj.k)[1])
        out[m,"var.est"]<- var(data.obj.k$STDTOTALCAUGHT)/dim(data.obj.k)[1] #var.est is calculated as variance of the estimator
      }
      out$Strata[m] <- sdmlevels[k]
      out$YEAR[m] <- year[i]
      out$SUBAREA[m] <- as.character(ab[j])
    }
  }
}
out
scall.levels.2014toCRNT <- out

### ... Merge data for all years into 1 dataframe ... ###
# Data by Strata: High, Medium, Low; by subare by year
names(scall.levels.2001to2004)[2] <- "Mean"
names(scall.levels.2001to2004)[7] <- "Std. Err."
names(scall.levels.2001to2004)[1] <- "Strata"

names(scall.levels.2005to2007)[1] <- "Mean"
names(scall.levels.2005to2007)[4] <- "Std. Err."
names(scall.levels.2005to2007)[5] <- "Strata"
names(scall.levels.2005to2007)[2] <- "var.est"

names(scall.levels.2008to2013)[1] <- "Mean"
names(scall.levels.2008to2013)[4] <- "Std. Err."
names(scall.levels.2008to2013)[5] <- "Strata"
names(scall.levels.2008to2013)[2] <- "var.est"

names(scall.levels.2014toCRNT)[4] <- "Mean"
names(scall.levels.2014toCRNT)[5] <- "Std. Err."

sdm.levels.est.all <- rbind(scall.levels.2001to2004[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2005to2007[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")],
                            scall.levels.2008to2013[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2014toCRNT[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")])
sdm.levels.est.all <- sdm.levels.est.all[order(sdm.levels.est.all$YEAR, sdm.levels.est.all$SUBAREA, sdm.levels.est.all$Strata), ]
sdm.levels.est.all$Strata <- as.factor(sdm.levels.est.all$Strata)
## AT: Include CV for model input
sdm.levels.est.all$CV <- log((sdm.levels.est.all$"Std. Err."/sdm.levels.est.all$Mean)^2 + 1)  #JS Dec 2021 - need to check this not sure this right 
sdm.levels.est.all$size <- size
names(sdm.levels.est.all)[grep("Std. Err.",names(sdm.levels.est.all))] <- "Std.Err"

#Add 2020 when had no survey
sdm.levels.est.all <- rbind(sdm.levels.est.all, data.frame(YEAR = rep(2020, 12), SUBAREA = c(rep("SFA29A",3),rep("SFA29B",3),rep("SFA29C",3),rep("SFA29D",3)), Strata = rep(c("low","med","high"),4), Mean=rep(NA,12), Std.Err=rep(NA,12),  var.est=rep(NA,12), CV=rep(NA,12), size = rep(size,12)) )

sdm.levels.est.all <- sdm.levels.est.all %>% arrange(SUBAREA,YEAR, Strata)


#Subarea A - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29A" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea A - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29A" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea B - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea B - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea B - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea C - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea C - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea C - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea D - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019


#Subarea D - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea D - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

sdm.levels.rec <- sdm.levels.est.all
sdm.levels.rec


#In 2015: Recruit (numbers) Dlow: removed highest tow, used mean (cahnged from 68/TOW to 49/TOW)
#                          D high: used median     (changed from CHANGED from 281/TOW to 22/TOW)
#for recruit
sdm.levels.rec$Mean[which(sdm.levels.rec$SUBAREA=='SFA29D' & sdm.levels.rec$YEAR=='2015' &sdm.levels.rec$Strata=="low")] <- 49
sdm.levels.rec$Mean[which(sdm.levels.rec$SUBAREA=='SFA29D' & sdm.levels.rec$YEAR=='2015' &sdm.levels.rec$Strata=="high")] <- 22

#writeout data 
#write.csv(sdm.levels.rec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.",size,".csv"))



### Stratified estimates (one estimate for each subarea by year)
out.domain.surf$se.yst <- sqrt(out.domain.surf$var.yst)
out.domain.geophys$se.yst <- sqrt(out.domain.geophys$var.yst)
sdm.strat.est.all <- rbind(out.strat[,c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.surf[, c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.geophys[, c("YEAR","SUBAREA","yst","se.yst","descrip")],out.strat.2014toCRNT[,c("YEAR","SUBAREA","yst","se.yst","descrip")] )
sdm.strat.est.all$rel.err <- sdm.strat.est.all$se.yst/sdm.strat.est.all$yst # relative error = standard error divided by the mean
sdm.strat.est.all <- sdm.strat.est.all[with(sdm.strat.est.all,order(sdm.strat.est.all$YEAR, sdm.strat.est.all$SUBAREA)),]
sdm.strat.est.all$size <- size

#Add 2020 when had no survey 
sdm.strat.est.all <- rbind(sdm.strat.est.all, data.frame(YEAR = rep(2020, 4), SUBAREA = c("SFA29A","SFA29B","SFA29C","SFA29D"),  yst =rep(NA,4), se.yst=rep(NA,4), descrip=rep(NA,4), rel.err=rep(NA,4), size = rep(size,4)) )

sdm.strat.est.all <- sdm.strat.est.all %>% arrange(SUBAREA,YEAR)

#subarea A 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29A" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29A"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29A"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29A"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29A"& sdm.strat.est.all$YEAR==2019])  

#subarea B 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29B" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29B"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29B"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29B"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29B"& sdm.strat.est.all$YEAR==2019])  

#subarea C 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29C" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29C"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29C"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29C"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29C"& sdm.strat.est.all$YEAR==2019])  

#subarea D 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29D" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29D"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29D"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29D"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29D"& sdm.strat.est.all$YEAR==2019])  

sdm.strat.est.all.rec <- sdm.strat.est.all
sdm.strat.est.all.rec

#writeout data 
#write.csv(sdm.strat.est.all.rec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Numbers.",size,".csv"))



# ---- COMMERCIAL ---- 
# NOTE this section of code is run once for each size (comm, rec, prerec). Define below
strata.group <- SDMareas
size <- "comm" # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column prerec, rec, or comm)
data.obj$STDTOTALCAUGHT <- data.obj$comm # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column prerec, rec, or comm)

# Only use regular survey tows for estimation (TOW_TYPE_ID = 1)
data.obj <- data.obj[data.obj$TOW_TYPE_ID==1,]
dim(data.obj)
head(data.obj)

###... For Years:  2001-2004 Calculate Stratified Random Survey Estimate - SDM strata ...###
# NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
### DATA: scall.levels.2001to2004 (data by low, med, high), out.strat (stratified est over all subarea)
ab <- unique(strata.group$Subarea)
year <- c(2001,2002,2003,2004) # update year list to add a new year and update "rep(NA,#) below
out <- data.frame(YEAR=rep(NA,16),SUBAREA=rep(NA,16),yst=rep(NA,16),se.yst=rep(NA,16),descrip=rep("post.stratified",16)) # update number in rep(NA,#), #+5 when add new year of data
m <- 0 #index
scall.levels.2001to2004 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      #stratified estimates
      scall.sum <- post.stratify.all(data.obj.i$STDTOTALCAUGHT, data.obj.i$STRATA, data.obj.i$SDM, Strata.info=strata.group,  oldstrata.name="Subarea",newstrata.name="Strata")
      out[m,(3:4)] <- as.data.frame(scall.sum[1])[,2:3]
      
      #estimates by strata
      scall.str <- as.data.frame(t(as.data.frame(scall.sum[2])))
      names(scall.str) <- "ybd"
      scall.str$LEVEL <-  row.names(scall.str)
      scall.str$YEAR <- rep(year[i],dim(scall.str)[1])
      scall.str$SUBAREA <- rep(ab[j],dim(scall.str)[1])
      var.obs <- tapply(data.obj.i$STDTOTALCAUGHT,data.obj.i$SDM, var) #Calculate variance by strata
      var.ybd <- as.data.frame(var.obs)
      var.ybd$LEVEL <- row.names(var.ybd)
      #var.ybd$var.ybd <- as.vector(var.ybd$var.ybd)
      scall.str <- merge(scall.str, var.ybd, by.x=c("LEVEL"),by.y=("LEVEL"), all.x=TRUE)
      scall.n <- as.data.frame(t(as.data.frame(scall.sum[3])))
      names(scall.n) <-"n"
      scall.n$LEVEL <- row.names(scall.n)
      scall.str <- merge(scall.str, scall.n, by.x=c("LEVEL"),by.y=("LEVEL"), all.x=TRUE)
      scall.levels.2001to2004[[m]] <- scall.str
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}
out
out.strat <- out
scall.levels.2001to2004 <- as.data.frame(do.call(rbind,scall.levels.2001to2004))
scall.levels.2001to2004$se.yst <- sqrt(scall.levels.2001to2004$var.obs)/sqrt(scall.levels.2001to2004$n)
scall.levels.2001to2004$var.est <- scall.levels.2001to2004$var.obs/scall.levels.2001to2004$n
scall.levels.2001to2004 <- scall.levels.2001to2004[scall.levels.2001to2004$n!=0,]# Remove records where n = 0; i.e. remove records for which there were no tows

###... For Years: 2005-2007 Calculate Domain Estimate by subarea for SURFICIAL to SDM strata ...###
###... SUBAREA A ...### DATA ITEMS: scall.est.A (all data), scall.levels.A.2005to2007 (data by low, med, high), out.domain.surf.a  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29A'
scall.est.A <- list()
scall.levels.A.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.A, strata.group.i)
    scall.est.A[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.A <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.A$LEVEL <- row.names(scall.levels.A)
    scall.levels.A$YEAR <- rep(year[i],3)
    scall.levels.A$SUBAREA <- rep(subarea,3)
    scall.levels.A.2005to2007[[m]] <- scall.levels.A
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.a <- out
scall.levels.A.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.A.2005to2007 ))


###... SUBAREA B ...### DATA ITEMS: scall.est.B (all data), scall.levels.B.2005to2007 (data by low, med, high), out.domain.surf.b  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29B'
scall.est.B <- list()
scall.levels.B.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.B, strata.group.i)
    scall.est.B[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.B <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.B$LEVEL <- row.names(scall.levels.B)
    scall.levels.B$YEAR <- rep(year[i],3)
    scall.levels.B$SUBAREA <- rep(subarea,3)
    scall.levels.B.2005to2007[[m]] <- scall.levels.B
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.b <- out
scall.levels.B.2005to2007  <- as.data.frame(do.call(rbind,scall.levels.B.2005to2007 ))


###... SUBAREA C ...### DATA ITEMS: scall.est.C (all data), scall.levels.C.2005to2007 (data by low, med, high), out.domain.surf.c  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29C'
scall.est.C <- list()
scall.levels.C.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.C, strata.group.i)
    scall.est.C[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.C <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.C$LEVEL <- row.names(scall.levels.C)
    scall.levels.C$YEAR <- rep(year[i],3)
    scall.levels.C$SUBAREA <- rep(subarea,3)
    scall.levels.C.2005to2007[[m]] <- scall.levels.C
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.c <- out
scall.levels.C.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.C.2005to2007))


###... SUBAREA D ...### DATA ITEMS: scall.est.D (all data), scall.levels.D.2005to2007 (data by low, med, high), out.domain.surf.d  (stratified est over all subarea)
year <- c(2005, 2006, 2007)
out <- data.frame(YEAR=rep(NA,3),SUBAREA=rep(NA,3),yst=rep(NA,3),var.yst=rep(NA,3),descrip=rep('domain',3))
m <- 0 #index
subarea <- 'SFA29D'
scall.est.D <- list()
scall.levels.D.2005to2007 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  m=m+1
  strata.group.i <- strata.group[strata.group$Subarea==subarea,]
  data.obj.i <- temp.data[temp.data$STRATA==subarea,]
  if(dim(data.obj.i)[1]!=0){
    scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$Start.Bottom, data.obj.i$SDM, strata.bottomtypeandzone.D, strata.group.i)
    scall.est.D[[m]] <- c(YR=year[i], SUBAREA=subarea, scall.dom)
    scall.sum <- summary.domain.est(scall.dom)
    out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
    
    scall.levels.D <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
    scall.levels.D$LEVEL <- row.names(scall.levels.D)
    scall.levels.D$YEAR <- rep(year[i],3)
    scall.levels.D$SUBAREA <- rep(subarea,3)
    scall.levels.D.2005to2007[[m]] <- scall.levels.D
    
  }
  out$YEAR[m] <- year[i]
  out$SUBAREA <- subarea
}

out
out.domain.surf.d <- out
scall.levels.D.2005to2007 <- as.data.frame(do.call(rbind,scall.levels.D.2005to2007))

# Merge all 2005 to 2007 data together
scall.levels.2005to2007 <- rbind(scall.levels.A.2005to2007, scall.levels.B.2005to2007, scall.levels.C.2005to2007, scall.levels.D.2005to2007)
out.domain.surf <- rbind(out.domain.surf.a, out.domain.surf.b, out.domain.surf.c, 	out.domain.surf.d)

###... For Years: 2008-2013 Calculate Domain Estimate by subarea for GEOPHYSICAL (GEOPHYS_ID) to SDM strata ...###
### DATA: scall.dom.2008to2013(data all), scall.levels.2008to2013 (data by low, med, high), out.domain.geophys (stratified est over all subarea)

ab <- unique(strata.group$Subarea)
year <- c(2008, 2009, 2010, 2011, 2012, 2013)
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),var.yst=rep(NA,(length(year)*length(ab))),descrip=rep('domain',(length(year)*length(ab))))
m <- 0 #index
scall.dom.2008to2013 <- list()
scall.levels.2008to2013 <- list()
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      
      scall.dom <- Domain.estimates(data.obj.i$STDTOTALCAUGHT, data.obj.i$GEOPHYS_ID, data.obj.i$SDM, RevisedstrataareasSFA29.revAugust2[RevisedstrataareasSFA29.revAugust2$Subarea==ab[j],], strata.group.i)
      scall.dom.2008to2013[[m]] <- c(YR=year[i], SUBAREA=ab[j], scall.dom)
      scall.sum <- summary.domain.est(scall.dom)
      out[m,(3:4)] <- as.numeric(c(scall.sum[[2]][2],scall.sum[[2]][3]))
      
      scall.levels <- with(scall.dom,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
      scall.levels$LEVEL <- row.names(scall.levels)
      scall.levels$YEAR <- rep(year[i],3)
      scall.levels$SUBAREA <- rep(ab[j],3)
      scall.levels.2008to2013[[m]] <- scall.levels
      
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}

out
out.domain.geophys <- out
scall.levels.2008to2013 <- as.data.frame(do.call(rbind,scall.levels.2008to2013))


###... For Years: 2014-YYYY Calculate Stratified Random Survey Estimate - SDM strata ...###
### NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
### DATA: scall.strat.2014toYYYY(data all - stratified SDM estimate), scall.levels.2014toYYYY (data by low, med, high)

ab <- unique(strata.group$Subarea)
year <- c(2014:2019,2021:surveyyear)
#out <- #data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(#length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),effic.alloc=rep(NA,(length(year)*length(ab))),effic.str=rep(NA,(length(year)*length(ab))),var.ran=rep(NA,(length(year)*length(ab))),max.eff=rep(NA,(length(yea#r)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))
#Paired down output bc must set effic = FALSE and nopt = FALSE if zero catch in all tows. Get a reduced output from summary.PEDstrata when set these 2 options to FALSE
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))

descrip=rep(NA,(length(year)*length(ab)))
m <- 0 #index
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    if(dim(data.obj.i)[1]!=0){
      scall.sum <- summary(PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT),effic = FALSE, nopt = FALSE) #set effic and nopt to TRUE to get Vrandom
      #out[m,-(1:2)] <- scall.sum[c(-6,-11)] #use if set effic=TRUE and nopt=TRUE
      out[m,-(1:2)] <- scall.sum[c(-6)]
    }
    out$YEAR[m] <- year[i]
    out$SUBAREA[m] <- as.character(ab[j])
  }
}
out
out.strat.2014toCRNT <- out

#Calculate mean and variance within each SDM strata (High, Medium, Low)
#NOTE: could also obtain this if added scall.levels <- PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT) in the above loop and then assigned all outputs within each loop to a list


sdmlevels <- na.omit(unique(data.obj$SDM))
out <- data.frame(YEAR=rep(NA,(length(year)*length(ab)*length(sdmlevels))),SUBAREA=rep(NA,(length(year)*length(ab)*length(sdmlevels))),Strata=rep(NA,(length(year)*length(ab)*length(sdmlevels))),yst=rep(NA,(length(year)*length(ab)*length(sdmlevels))),se.yst=rep(NA,(length(year)*length(ab)*length(sdmlevels))),var.est=rep(NA,(length(year)*length(ab)*length(sdmlevels))),descrip=rep("simple",(length(year)*length(ab)*length(sdmlevels))))
m <- 0 #index
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    for (k in 1:length(sdmlevels)){
      data.obj.k <- data.obj.i[data.obj.i$SDM==sdmlevels[k],]
      #data.obj.k[is.na(data.obj.k$uid),]  ## in 2024, NAs introduced in data.obj.k.. added line 412 to deal with this and added na.rm = TRUE to line 415 
      data.obj.k <- data.obj.k[!is.na(data.obj.k$uid),]
      m=m+1
      if(dim(data.obj.k)[1]!=0){
        out[m,"yst"] <- mean(data.obj.k$STDTOTALCAUGHT, na.rm = TRUE)
        out[m,"se.yst"] <- sqrt(var(data.obj.k$STDTOTALCAUGHT))/sqrt(dim(data.obj.k)[1])
        out[m,"var.est"]<- var(data.obj.k$STDTOTALCAUGHT)/dim(data.obj.k)[1] #var.est is calculated as variance of the estimator
      }
      out$Strata[m] <- sdmlevels[k]
      out$YEAR[m] <- year[i]
      out$SUBAREA[m] <- as.character(ab[j])
    }
  }
}
out
scall.levels.2014toCRNT <- out

### ... Merge data for all years into 1 dataframe ... ###
# Data by Strata: High, Medium, Low; by subare by year
names(scall.levels.2001to2004)[2] <- "Mean"
names(scall.levels.2001to2004)[7] <- "Std. Err."
names(scall.levels.2001to2004)[1] <- "Strata"

names(scall.levels.2005to2007)[1] <- "Mean"
names(scall.levels.2005to2007)[4] <- "Std. Err."
names(scall.levels.2005to2007)[5] <- "Strata"
names(scall.levels.2005to2007)[2] <- "var.est"

names(scall.levels.2008to2013)[1] <- "Mean"
names(scall.levels.2008to2013)[4] <- "Std. Err."
names(scall.levels.2008to2013)[5] <- "Strata"
names(scall.levels.2008to2013)[2] <- "var.est"

names(scall.levels.2014toCRNT)[4] <- "Mean"
names(scall.levels.2014toCRNT)[5] <- "Std. Err."

sdm.levels.est.all <- rbind(scall.levels.2001to2004[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2005to2007[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")],
                            scall.levels.2008to2013[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2014toCRNT[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")])
sdm.levels.est.all <- sdm.levels.est.all[order(sdm.levels.est.all$YEAR, sdm.levels.est.all$SUBAREA, sdm.levels.est.all$Strata), ]
sdm.levels.est.all$Strata <- as.factor(sdm.levels.est.all$Strata)
## AT: Include CV for model input
sdm.levels.est.all$CV <- log((sdm.levels.est.all$"Std. Err."/sdm.levels.est.all$Mean)^2 + 1)  #JS Dec 2021 - need to check this not sure this right 
sdm.levels.est.all$size <- size
names(sdm.levels.est.all)[grep("Std. Err.",names(sdm.levels.est.all))] <- "Std.Err"

#Add 2020 when had no survey
sdm.levels.est.all <- rbind(sdm.levels.est.all, data.frame(YEAR = rep(2020, 12), SUBAREA = c(rep("SFA29A",3),rep("SFA29B",3),rep("SFA29C",3),rep("SFA29D",3)), Strata = rep(c("low","med","high"),4), Mean=rep(NA,12), Std.Err=rep(NA,12),  var.est=rep(NA,12), CV=rep(NA,12), size = rep(size,12)) )

sdm.levels.est.all <- sdm.levels.est.all %>% arrange(SUBAREA,YEAR, Strata)


#Subarea A - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29A" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea A - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29A" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29A"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea B - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea B - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea B - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29B" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29B"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea C - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea C - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea C - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29C" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29C"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019



#Subarea D - low 
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="low", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="low"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019


#Subarea D - medium  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="med", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="med"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

#Subarea D - high  
sdm.levels.est.all[sdm.levels.est.all$YEAR==2020 & sdm.levels.est.all$SUBAREA=="SFA29D" & sdm.levels.est.all$Strata=="high", c("Mean","Std.Err","var.est", "CV")] <- c(approx(sdm.levels.est.all$YEAR[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"], sdm.levels.est.all$Mean[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"], xout=2020)$y, sdm.levels.est.all$Std.Err[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$var.est[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019], sdm.levels.est.all$CV[sdm.levels.est.all$SUBAREA=="SFA29D"&sdm.levels.est.all$Strata=="high"& sdm.levels.est.all$YEAR==2019]) #assume var & se from 2019

sdm.levels.comm <- sdm.levels.est.all
sdm.levels.comm


#In 2015: 
#     Commerical (numbers) D low: used median (changed from 133/TOW to 61/TOW))
#for commercial
sdm.levels.comm$Mean[which(sdm.levels.comm$SUBAREA=='SFA29D' & sdm.levels.comm$YEAR=='2015' &sdm.levels.comm$Strata=="low")] <- 61

#writeout data 
#write.csv(sdm.levels.comm, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.",size,".csv"))



## Stratified estimates (one estimate for each subarea by year)
out.domain.surf$se.yst <- sqrt(out.domain.surf$var.yst)
out.domain.geophys$se.yst <- sqrt(out.domain.geophys$var.yst)
sdm.strat.est.all <- rbind(out.strat[,c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.surf[, c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.geophys[, c("YEAR","SUBAREA","yst","se.yst","descrip")],out.strat.2014toCRNT[,c("YEAR","SUBAREA","yst","se.yst","descrip")] )
sdm.strat.est.all$rel.err <- sdm.strat.est.all$se.yst/sdm.strat.est.all$yst # relative error = standard error divided by the mean
sdm.strat.est.all <- sdm.strat.est.all[with(sdm.strat.est.all,order(sdm.strat.est.all$YEAR, sdm.strat.est.all$SUBAREA)),]
sdm.strat.est.all$size <- size

#Add 2020 when had no survey 
sdm.strat.est.all <- rbind(sdm.strat.est.all, data.frame(YEAR = rep(2020, 4), SUBAREA = c("SFA29A","SFA29B","SFA29C","SFA29D"),  yst =rep(NA,4), se.yst=rep(NA,4), descrip=rep(NA,4), rel.err=rep(NA,4), size = rep(size,4)) )

sdm.strat.est.all <- sdm.strat.est.all %>% arrange(SUBAREA,YEAR)

#subarea A 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29A" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29A"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29A"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29A"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29A"& sdm.strat.est.all$YEAR==2019])  

#subarea B 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29B" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29B"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29B"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29B"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29B"& sdm.strat.est.all$YEAR==2019])  

#subarea C 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29C" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29C"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29C"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29C"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29C"& sdm.strat.est.all$YEAR==2019])  

#subarea D 2020 stratifed interpolation 	
sdm.strat.est.all[sdm.strat.est.all$YEAR==2020 & sdm.strat.est.all$SUBAREA=="SFA29D" , c("yst","se.yst", "rel.err")] <- 	c(approx(sdm.strat.est.all$YEAR[sdm.strat.est.all$SUBAREA=="SFA29D"], sdm.strat.est.all$yst[sdm.strat.est.all$SUBAREA=="SFA29D"], xout=2020)$y, sdm.strat.est.all$se.yst[sdm.strat.est.all$SUBAREA=="SFA29D"& sdm.strat.est.all$YEAR==2019], sdm.strat.est.all$rel.err[sdm.strat.est.all$SUBAREA=="SFA29D"& sdm.strat.est.all$YEAR==2019])  

sdm.strat.est.all.comm <- sdm.strat.est.all
sdm.strat.est.all.comm

#writeout data 
#write.csv(sdm.strat.est.all.comm, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Numbers.",size,".csv"))


# ---- Merge dataframes A-D & calculate inputs for model ---- 

#estimates by SDM strata 
sdm.levels <- rbind(sdm.levels.prerec, sdm.levels.rec, sdm.levels.comm) 

#writeout data 
write.csv(sdm.levels, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.csv"))

#Stratified estimates 
sdm.strat.est <- rbind(sdm.strat.est.all.prerec, sdm.strat.est.all.rec, sdm.strat.est.all.comm)

#writeout data 
write.csv(sdm.strat.est, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Numbers.csv"))

## data for model
# "rh" - recruit numbers scaled to respective SDM strata in millions 
# "obs.nu" recruit numbers per tow CV
# "L" commercial number scaled to respective SDM strata in millions 

# rh # 
rh.obs.nu <- sdm.levels %>% filter(size == "rec" & !(SUBAREA == "SFA29A" & Strata == "high")) %>% dplyr::select(YEAR, SUBAREA, Strata, Mean, CV, size) 
rh.obs.nu <-  merge(rh.obs.nu, towable.units, by = c("SUBAREA", "Strata")) 
rh.obs.nu <- rh.obs.nu %>% mutate(rh = round(((Mean*TowableUnits)/1000000),digits =4)) 
rh.obs.nu <- rh.obs.nu %>% arrange(SUBAREA, Strata, YEAR)
head(rh.obs.nu)
write.csv(rh.obs.nu, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/rh.obs.nu.2001to",surveyyear,".csv"))

# L # 
L.model <- sdm.levels %>% filter(size == "comm" & !(SUBAREA == "SFA29A" & Strata == "high")) %>% dplyr::select(YEAR, SUBAREA, Strata, Mean, CV, size) 
L.model <-  merge(L.model, towable.units, by = c("SUBAREA", "Strata")) 
L.model <- L.model %>% mutate(L = round(((Mean*TowableUnits)/1000000),digits =4)) 
L.model <- L.model %>% arrange(SUBAREA, Strata, YEAR)
head(L.model)
write.csv(L.model, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/L.2001to",surveyyear,".csv"))



### --- Plots Numbers number per tow A to D ---- 
sdm.levels$group <- factor(sdm.levels$size,      # Reordering group factor levels
                           levels = c("prerec", "rec", "comm"))
size_names <- as_labeller(
  c(`prerec` = "Prerecruits (<90 mm)", `rec` = "Recruits (90-99 mm)",`comm` = "Commercial (>= 100 mm)"))

#For plots - removing 2020 values
sdm.levels <- sdm.levels %>% 
  mutate(Mean = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ Mean)) %>%
  mutate(Std.Err = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ Std.Err)) %>%
  mutate(var.est = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ var.est)) %>%
  mutate(CV = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ CV)) %>% 
  mutate(SUBAREA_FR = case_when(SUBAREA == "SFA29A" ~ "Sous-zone A",
                                SUBAREA == "SFA29B" ~ "Sous-zone B",
                                SUBAREA == "SFA29C" ~ "Sous-zone C",
                                SUBAREA == "SFA29D" ~ "Sous-zone D")) |> 
  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
                                SUBAREA == "SFA29B" ~ "Subarea B",
                                SUBAREA == "SFA29C" ~ "Subarea C",
                                SUBAREA == "SFA29D" ~ "Subarea D"))
  

## Subarea A 
A.number.per.tow <- ggplot(data = sdm.levels %>% filter(SUBAREA == "Subarea A" & Strata != "high"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~group, ncol=1, labeller = size_names) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"), labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"), labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"), labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4)) #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
A.number.per.tow

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29A.Numberspertow.",surveyyear,".png"), plot = A.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29A.Numberspertow.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
#A.number.per.tow
#dev.off()



## Subarea B 
B.number.per.tow <- ggplot(data = sdm.levels %>% filter(SUBAREA == "Subarea B"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~group, ncol=1, labeller = size_names) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.89),panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))#+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
B.number.per.tow

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29B.Numberspertow.",surveyyear,".png"), plot = B.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29B.Numberspertow.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
#B.number.per.tow
#dev.off()


## Subarea C 
C.number.per.tow <- ggplot(data = sdm.levels %>% filter(SUBAREA == "Subarea C"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~group, ncol=1, labeller = size_names) + 
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.89),legend.background = element_rect(fill=alpha('white', 0.8)),panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))#Legend bkg colour and transparency)
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
C.number.per.tow

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29C.Numberspertow.",surveyyear,".png"), plot = C.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29C.Numberspertow.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
#C.number.per.tow
#dev.off()

## Subarea D 
D.number.per.tow <- ggplot(data = sdm.levels %>% filter(SUBAREA == "Subarea D"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~group, ncol=1, labeller = size_names) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.89),panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4)) #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
D.number.per.tow

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29D.Numberspertow.",surveyyear,".png"), plot = D.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29D.Numberspertow.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
#D.number.per.tow
#dev.off()

## All Subareas A-D Pre-recruits 
AtoD.number.per.tow.prerec <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "prerec"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~SUBAREA, ncol=2) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor.x = element_blank(),legend.title = element_blank(),text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))

AtoD.number.per.tow.prerec

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Prerecruit.",surveyyear,".png"), plot = AtoD.number.per.tow.prerec, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

## All Subareas A-D Pre-recruits - FR
AtoD.number.per.tow.prerec.fr <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "prerec"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~SUBAREA_FR, ncol=2) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  theme_bw() + ylab("Nombre moyen par trait") + xlab("Année") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor.x = element_blank(),legend.title = element_blank(),text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))

AtoD.number.per.tow.prerec.fr

#Export plot - FR
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Prerecruit.",surveyyear,"_FR.png"), plot = AtoD.number.per.tow.prerec.fr, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)


## All Subareas A-D Recruits 
AtoD.number.per.tow.rec <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "rec"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~SUBAREA, ncol=2) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor.x = element_blank(),legend.title = element_blank(),text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))#+ 

AtoD.number.per.tow.rec

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Recruit.",surveyyear,".png"), plot = AtoD.number.per.tow.rec, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

## All Subareas A-D Recruits - FR
AtoD.number.per.tow.rec.fr <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "rec"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~SUBAREA_FR, ncol=2) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  theme_bw() + ylab("Nombre moyen par trait") + xlab("Année") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor.x = element_blank(),legend.title = element_blank(),text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))

AtoD.number.per.tow.rec.fr

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Recruit.",surveyyear,"_FR.png"), plot = AtoD.number.per.tow.rec.fr, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)


## All Subareas A-D Commercial  
AtoD.number.per.tow.comm <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "comm"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~SUBAREA, ncol=2) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor.x = element_blank(),legend.title = element_blank(),text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))#+ 

AtoD.number.per.tow.comm

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Commercial.",surveyyear,".png"), plot = AtoD.number.per.tow.comm, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

## All Subareas A-D Commercial-FR
AtoD.number.per.tow.comm.fr <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "comm"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~SUBAREA_FR, ncol=2) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="Élevée", "med"="Moyenne", "low"="Faible"))+
  theme_bw() + ylab("Nombre moyen par trait") + xlab("Année") +
  theme(legend.position = c(0.1, 0.85),panel.grid.minor.x = element_blank(),legend.title = element_blank(),text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))

AtoD.number.per.tow.comm.fr

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Commercial.",surveyyear,"_FR.png"), plot = AtoD.number.per.tow.comm.fr, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)



### ---- SUBAREA E ----  
# NO DOMAIN DATAFRAME FOR E - for 2005 take simple mean #
#some year were all exploratory tows - would need to get data from data.obj.all

#prerecruits
E.area <- data.obj.all[data.obj.all$STRATA_ID==45,]
sizeE.prerec <- "prerec"	# Define - ensure matches entry on next line where assign E.area$STDTOTALCAUGHT
E.area$STDTOTALCAUGHT <- E.area$prerec #DEFINE SIZE; comm, rec, prerec
E.years <- unique(E.area$YEAR)

out.e.prerec <- data.frame(YEAR=E.years,SUBAREA='SFA29E',yst=rep(NA,length(E.years)),var.yst=rep(NA,length(E.years)), se.yst=rep(NA,length(E.years)),
                           descrip=rep('simple mean',length(E.years)), size=rep(sizeE.prerec,length(E.years)))

for (i in 1:length(E.years)) {
  temp.E <- E.area[E.area$YEAR==E.years[i],]
  out.e.prerec[i,3] <- mean(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])
  out.e.prerec[i,4] <- var(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])/dim(temp.E)[1] #variance of the estimator
  out.e.prerec[i,5] <- sd(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])/sqrt(dim(temp.E)[1]) #SEM
}
out.e.prerec

#Recruits
E.area <- data.obj.all[data.obj.all$STRATA_ID==45,]
sizeE.rec <- "rec"	# Define - ensure matches entry on next line where assign E.area$STDTOTALCAUGHT
E.area$STDTOTALCAUGHT <- E.area$rec #DEFINE SIZE; comm, rec, prerec
E.years <- unique(E.area$YEAR)

out.e.rec <- data.frame(YEAR=E.years,SUBAREA='SFA29E',yst=rep(NA,length(E.years)),var.yst=rep(NA,length(E.years)), se.yst=rep(NA,length(E.years)),
                        descrip=rep('simple mean',length(E.years)), size=rep(sizeE.rec,length(E.years)))

for (i in 1:length(E.years)) {
  temp.E <- E.area[E.area$YEAR==E.years[i],]
  out.e.rec[i,3] <- mean(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])
  out.e.rec[i,4] <- var(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])/dim(temp.E)[1] #variance of the estimator
  out.e.rec[i,5] <- sd(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])/sqrt(dim(temp.E)[1]) #SEM
}
out.e.rec

#commercial 
E.area <- data.obj.all[data.obj.all$STRATA_ID==45,]
sizeE.comm <- "comm"	# Define - ensure matches entry on next line where assign E.area$STDTOTALCAUGHT
E.area$STDTOTALCAUGHT <- E.area$comm #DEFINE SIZE; comm, rec, prerec
E.years <- unique(E.area$YEAR)

out.e.comm <- data.frame(YEAR=E.years,SUBAREA='SFA29E',yst=rep(NA,length(E.years)),var.yst=rep(NA,length(E.years)), se.yst=rep(NA,length(E.years)),
                         descrip=rep('simple mean',length(E.years)), size=rep(sizeE.comm,length(E.years)))

for (i in 1:length(E.years)) {
  temp.E <- E.area[E.area$YEAR==E.years[i],]
  out.e.comm[i,3] <- mean(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])
  out.e.comm[i,4] <- var(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])/dim(temp.E)[1] #variance of the estimator
  out.e.comm[i,5] <- sd(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])/sqrt(dim(temp.E)[1]) #SEM
}
out.e.comm

# fill in for 2020 when no survey (note - added code to fill in 2020 value consistent with other areas, but bc E not modelled not needed - just left commented out incase needed in future)
out.e.prerec <- out.e.prerec %>% add_row(YEAR = 2020, size = "prerec", SUBAREA = "SFA29E")
#	out.e.prerec[out.e.prerec$YEAR==2020,c("yst","var.yst","se.yst")] <- c(approx(out.e.prerec$YEAR, out.e.prerec$yst, xout=2020)$y, out.e.prerec$var.yst[out.e.prerec$YEAR==2019], out.e.prerec$se.yst[out.e.prerec$YEAR==2019]) #assume var & se from 2019
out.e.prerec <- out.e.prerec %>% add_row(YEAR = c(2006:2011), size = "prerec", SUBAREA = "SFA29E")
out.e.prerec <- out.e.prerec %>% arrange(YEAR)
out.e.prerec

out.e.rec <- out.e.rec %>% add_row(YEAR = 2020, size = "rec", SUBAREA = "SFA29E")
#	out.e.rec[out.e.rec$YEAR==2020,c("yst","var.yst","se.yst")] <- c(approx(out.e.rec$YEAR, out.e.rec$yst, xout=2020)$y, out.e.rec$var.yst[out.e.rec$YEAR==2019], out.e.rec$se.yst[out.e.rec$YEAR==2019]) #assume var & se from 2019
out.e.rec <- out.e.rec %>% add_row(YEAR = c(2006:2011), size = "rec", SUBAREA = "SFA29E")
out.e.rec <- out.e.rec %>% arrange(YEAR)
out.e.rec

out.e.comm <- out.e.comm %>% add_row(YEAR = 2020, size = "comm", SUBAREA = "SFA29E")
#	out.e.comm[out.e.comm$YEAR==2020,c("yst","var.yst","se.yst")] <- c(approx(out.e.comm$YEAR, out.e.comm$yst, xout=2020)$y, out.e.comm$var.yst[out.e.comm$YEAR==2019], out.e.comm$se.yst[out.e.comm$YEAR==2019]) #assume var & se from 2019
out.e.comm <- out.e.comm %>% add_row(YEAR = c(2006:2011), size = "comm", SUBAREA = "SFA29E")
out.e.comm <- out.e.comm %>% arrange(YEAR)
out.e.comm


#merge together 
out.e <- 	rbind(out.e.prerec, out.e.rec, out.e.comm)

#writeout 
write.csv(out.e, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SubareaE.ExploratoryMeanbyTow.Numbers.",surveyyear,".csv",sep=""))


# ---- Plot Subarea E Number per tow ----- 

out.e$group <- factor(out.e$size,      # Reordering group factor levels
                      levels = c("prerec", "rec", "comm"))
size_names <- as_labeller(
  c(`prerec` = "Prerecruits (<90 mm)", `rec` = "Recruits (90-99 mm)",`comm` = "Commercial (>= 100 mm)"))

#2001-surveyyear
E.number.per.tow <- ggplot(data = out.e, aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~group, ncol=1, labeller = size_names) + 
  scale_x_continuous(limits = c(2001, (surveyyear+1)), breaks = seq(2001, (surveyyear+1), by = 4)) +
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank()) + 
 # geom_pointrange(data = out.e, aes(ymin=(yst-se.yst), ymax=(yst - se.yst))) 
  geom_pointrange(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst)) + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))
  #geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
  #           alpha=0.1,       #transparency
  #           linetype=1,      #solid, dashed or other line types
  #           colour="grey70", #border line color
  #           size=1,          #border line size
  #           fill="grey70") 
E.number.per.tow

#2012-surveyyear
E.number.per.tow <- ggplot(data = out.e , aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~group, ncol=1, labeller = size_names) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank(),text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
  # geom_pointrange(data = out.e, aes(ymin=(yst-se.yst), ymax=(yst - se.yst))) 
  geom_pointrange(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst)) + 
  scale_x_continuous(limits = c(2012, (surveyyear+1)), breaks = seq(2012, (surveyyear+1), by = 2))
#geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#           alpha=0.1,       #transparency
#           linetype=1,      #solid, dashed or other line types
#           colour="grey70", #border line color
#           size=1,          #border line size
#           fill="grey70") 
E.number.per.tow


ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29E.Numberspertow.",surveyyear,".png"), plot = E.number.per.tow, scale = 2.5, width = 6, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29E.Numberspertow.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
#E.number.per.tow
#dev.off()

##French version -- NOT COMPLETE - overwrites ENG version still 
out.e <- out.e %>% filter(YEAR %in% c(2012:surveyyear))
out.e$YEAR <- as.factor(out.e$YEAR)

#2012-surveyyear
E.number.per.tow <- ggplot(data = out.e %>% filter(YEAR %in% c(2012:surveyyear)), aes(x=YEAR, y=yst, group = 1)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~group, ncol=1, labeller = size_names) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) 
  #scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026))
#geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#           alpha=0.1,       #transparency
#           linetype=1,      #solid, dashed or other line types
#           colour="grey70", #border line color
#           size=1,          #border line size
#           fill="grey70") 
E.number.per.tow

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29E.Numberspertow.",surveyyear,".png"), plot = E.number.per.tow, scale = 2.5, width = 6, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


##
# --- Sample size n by SDM strata by subarea by year  ----
##
year <- unique(data.obj$YEAR)
ab <- c("SFA29A","SFA29B","SFA29C","SFA29D") # don't include E since no SDM in E, plus tapply errors out if data.obj.i is empty
sdm.n <- list()
m <- 0 #index
for (i in 1:length(year)) {
  temp.data <- data.obj[data.obj$YEAR==year[i],]
  for(j in 1:length(ab)) {
    m=m+1
    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
    out <- tapply(data.obj.i$uid,data.obj.i$SDM,length)
    out <- as.data.frame(out)
    names(out) <- "N"
    out$sdm.strata <- row.names(out)
    out$YEAR <- year[i]
    out$SUBAREA <- as.character(ab[j])
    sdm.n[[m]]<- out[,c(1,2,3,4)]
  }
}
sdm.n.all <- as.data.frame(do.call(rbind,sdm.n))
#no survey in 2020 

#writeout 
write.csv(sdm.n.all, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM_SampleSize.",surveyyear,".csv",sep=""))

# Plot 
tows.by.level <- ggplot(data=sdm.n.all %>% filter(!(SUBAREA == "SFA29A" & sdm.strata == "high")), aes(x=YEAR, y=N, col=sdm.strata, pch=sdm.strata)) + 
  geom_point() + 
  geom_line(aes(linetype=sdm.strata)) + facet_wrap(~SUBAREA) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,4),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85)) + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026))
tows.by.level

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.SDM.SampleSize.",surveyyear,".png"),width=11,height=11,units = "in",res=300)
tows.by.level
dev.off()


##
# --- Stratified estimates - NOT REQUIRED/USED by assessement but interesting ---- 
##

#rename subareas 
sdm.strat.est$subarea[sdm.strat.est$SUBAREA == "SFA29A"] <- "Subarea A"
sdm.strat.est$subarea[sdm.strat.est$SUBAREA == "SFA29B"] <- "Subarea B"
sdm.strat.est$subarea[sdm.strat.est$SUBAREA == "SFA29C"] <- "Subarea C"
sdm.strat.est$subarea[sdm.strat.est$SUBAREA == "SFA29D"] <- "Subarea D"

## remove 2020 since this is interpolated 
sdm.strat.est$yst[sdm.strat.est$YEAR == 2020] <- NA 


AtoD.stratified.plot <- ggplot(data = sdm.strat.est, aes(x=YEAR, y=yst,  col=size, pch=size)) + 
  geom_point() + 
  geom_line(aes(linetype = size)) + 
  facet_wrap(~subarea, ncol=2) +
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1))
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
AtoD.stratified.plot

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.",surveyyear,".png"),width=11,height=11,units = "in",res=300)
AtoD.stratified.plot
dev.off()



## Just pre-recruits ####
LTM.prerec <-  sdm.strat.est %>% filter(size == "prerec" & YEAR < surveyyear) %>% group_by(subarea) %>% summarise(LTM = median(yst, na.rm = TRUE))
LTM.prerec
#subarea     LTM
#<chr>     <dbl>
#  1 Subarea A  20.1
#2 Subarea B  35.0
#3 Subarea C  42.6
#4 Subarea D  69.4

AtoD.stratified.plot.prerec <- ggplot(data = sdm.strat.est %>% filter(size == "prerec"), aes(x=YEAR, y=yst)) + 
  geom_point(size = 0.25) + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2, scales = "free") +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_hline(data = LTM.prerec, aes(yintercept=LTM), col = "blue", linetype = "dashed")+
  theme(axis.title = element_text(size = 16), strip.text = element_text(size = 12), axis.text = element_text(size = 12))
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
AtoD.stratified.plot.prerec

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.prerecuit.numbers.",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)

#ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.prerecuit.numbers._wotow92",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)

## Just recruits ####
LTM.rec <-  sdm.strat.est %>% filter(size == "rec" & YEAR < surveyyear) %>% group_by(subarea) %>% summarise(LTM = median(yst, na.rm = TRUE   ))
LTM.rec
#subarea      LTM
#<chr>      <dbl>
#  1 Subarea A  0.998
#2 Subarea B  9.59 
#3 Subarea C  7.91 
#4 Subarea D 11.0  

AtoD.stratified.plot.rec <- ggplot(data = sdm.strat.est %>% filter(size == "rec"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2, scales = "free") +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_hline(data = LTM.rec, aes(yintercept=LTM), col = "blue", linetype = "dashed")+
  theme(axis.title = element_text(size = 16), strip.text = element_text(size = 12), axis.text = element_text(size = 12))
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
AtoD.stratified.plot.rec

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.recuit.numbers.",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)

#ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.recuit.numbers.wotow92",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)


## Just commercial ####
LTM.comm <-  sdm.strat.est %>% filter(size == "comm" & YEAR < surveyyear) %>% group_by(subarea) %>% summarise(LTM = median(yst , na.rm = TRUE  ))
LTM.comm
#subarea     LTM
#<chr>     <dbl>
#  1 Subarea A  95.7
#2 Subarea B 130. 
#3 Subarea C  75.3
#4 Subarea D 119. 

AtoD.stratified.plot.comm <- ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2, scales = "free") +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_hline(data = LTM.comm, aes(yintercept=LTM), col = "blue", linetype = "dashed")+
  theme(axis.title = element_text(size = 16), strip.text = element_text(size = 12), axis.text = element_text(size = 12))
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
AtoD.stratified.plot.comm

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.commercial.numbers.",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)

#ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.commercial.numbers.wotow92.",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)


#-- REFERENCE POINTS -proposal based off survey index of commercial numbers per tow ---- 

# LRP Plots ---- 

##... LRP Plot - 40% median value  -- where median is proxy for MSY - median doesn't include current year (2024) ####
# 40% of 50% Bmax where Bmax proxy for Bo

#Commerical biomass time series median of the yearly median values 
LTM.comm <-  sdm.strat.est %>% filter(size == "comm" & YEAR < surveyyear) %>% group_by(subarea) %>% summarise(LTM = median(yst   , na.rm = TRUE))
LTM.comm
#subarea     LTM
#<chr>     <dbl>
#1 Subarea A  95.7
#2 Subarea B 130. 
#3 Subarea C  75.3
#4 Subarea D 131. 

LTM.comm$LRP <- LTM.comm$LTM*0.4
LTM.comm

ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2, scales = "free") +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_hline(data = LTM.comm, aes(yintercept=LRP), col = "red", linetype = "dashed",  linewidth=0.5)

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/Com_timeseries_LRP.40.Bmedian.",surveyyear,".png"),width=15,height=10,units = "in",dpi=300)



##... LRP Plot - 40% of 50% Bmax where Bmax proxy for Bo ####
max.val <- sdm.strat.est %>% filter(size == "comm") %>% group_by(subarea) %>% summarize(Bo = max(yst, na.rm = TRUE))

max.val$Bmsy <- max.val$Bo*0.5
max.val

max.val$LRP <- 0.4*max.val$Bmsy
max.val

ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2) +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_hline(data = max.val, aes(yintercept=LRP), col = "red", linetype = "dashed",  linewidth=0.5)

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/Com_timeseries_LRP.40.of.50.Bmax.",surveyyear,".png"),width=15,height=10,units = "in",dpi=300)


##... LRP Plot - 20% of Bo (i.e. Bmax ) ####
max.val <- sdm.strat.est %>% filter(size == "comm") %>% group_by(subarea) %>% summarize(Bo = max(yst, na.rm = TRUE))
max.val$LRP <- max.val$Bo*0.2
max.val
#subarea      Bo   LRP
#<chr>     <dbl> <dbl>
#1 Subarea A  258.  51.6
#2 Subarea B  575. 115. 
#3 Subarea C  490.  98.1
#4 Subarea D  386.  77.2


ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2) +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_hline(data = max.val, aes(yintercept=LRP), col = "red", linetype = "dashed",  linewidth=0.5)

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/Com_timeseries_LRP.20.of.Bmax.ie.Bo.",surveyyear,".png"),width=15,height=10,units = "in",dpi=300)

## min biomass "B recover" in timeseries ####
Brec.comm <-  sdm.strat.est %>% filter(size == "comm" & YEAR < surveyyear) %>% group_by(subarea) %>% summarise(min = min(yst , na.rm = TRUE))
Brec.comm
#subarea     min
#<chr>     <dbl>
#1 Subarea A  15.2
#2 Subarea B  57.7
#3 Subarea C  39.4
#4 Subarea D  50.5


ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2) +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_hline(data = Brec.comm, aes(yintercept=min), col = "red", linetype = "dashed",  linewidth=0.5)

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/Com_timeseries_LRP.Brecover.",surveyyear,".png"),width=15,height=10,units = "in",dpi=300)


### USE Brecover ### 
Brec.comm
#rename min as LRP 
Brec.comm$LRP <- Brec.comm$min
Brec.comm
  
#### USR Options #### 
##... USR Plot - 80% of Bavg (Bavg ~ Bmsy)
#Commerical biomass time series median of the yearly median values 
LTM.comm <-  sdm.strat.est %>% filter(size == "comm" & YEAR < surveyyear) %>% group_by(subarea) %>% summarise(LTM = median(yst   , na.rm = TRUE))
LTM.comm

LTM.comm$USR <- LTM.comm$LTM*0.8
LTM.comm
#subarea     LTM   USR
#<chr>     <dbl> <dbl>
#1 Subarea A  95.7  76.6
#2 Subarea B 130.  104. 
#3 Subarea C  75.3  60.2
#4 Subarea D 131.  105. 

PA.limits <- merge(LTM.comm, Brec.comm, by = "subarea")
PA.limits


ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2) +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_rect(data = PA.limits, aes(ymin = -Inf, ymax = LRP, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="firebrick1", inherit.aes = FALSE) + 
  geom_rect(data = PA.limits, aes(ymin = LRP, ymax = USR, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="goldenrod1", inherit.aes = FALSE) + 
  geom_rect(data = PA.limits, aes(ymin = USR, ymax = Inf, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="chartreuse2", inherit.aes = FALSE)

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/Com_timeseries_LRP.Brecover.USR.80.of.Bmedian.",surveyyear,".png"),width=15,height=10,units = "in",dpi=300)




##... USR Plot - 80% of 50% Bmax where Bmax proxy for Bo
#Commerical biomass time series median of the yearly median values 
max.val <- sdm.strat.est %>% filter(size == "comm") %>% group_by(subarea) %>% summarize(Bo = max(yst, na.rm = TRUE))
max.val$Bmsy <- max.val$Bo*0.5
max.val

max.val$USR <- max.val$Bmsy*0.8
max.val

PA.limits <- merge(max.val, Brec.comm, by = "subarea")
PA.limits

ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2, scales = "free") +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_rect(data = PA.limits, aes(ymin = -Inf, ymax = LRP, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="firebrick1", inherit.aes = FALSE) + 
  geom_rect(data = PA.limits, aes(ymin = LRP, ymax = USR, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="goldenrod1", inherit.aes = FALSE) + 
  geom_rect(data = PA.limits, aes(ymin = USR, ymax = Inf, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="chartreuse2", inherit.aes = FALSE)

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/Com_timeseries_LRP.Brecover.USR.80.of.50.Bmax.",surveyyear,".png"),width=15,height=10,units = "in",dpi=300)




##... USR Plot - Median stock size 
LTM.comm <-  sdm.strat.est %>% filter(size == "comm" & YEAR < surveyyear) %>% group_by(subarea) %>% summarise(USR = median(yst, na.rm = TRUE))
LTM.comm

PA.limits <- merge(LTM.comm, Brec.comm, by = "subarea")
PA.limits


ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=yst)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~subarea, ncol=2, scales = "free") +
  theme_bw() + 
  ylab("Survey mean no./tow") +
  xlab("Year") + 
  #theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank()) + 
  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst)) + 
  scale_x_continuous(breaks = seq(2001,surveyyear+1,by=4), limits = c(2001,surveyyear+1)) + 
  geom_rect(data = PA.limits, aes(ymin = -Inf, ymax = LRP, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="firebrick1", inherit.aes = FALSE) + 
  geom_rect(data = PA.limits, aes(ymin = LRP, ymax = USR, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="goldenrod1", inherit.aes = FALSE) + 
  geom_rect(data = PA.limits, aes(ymin = USR, ymax = Inf, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="chartreuse2", inherit.aes = FALSE)

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/Com_timeseries_LRP.Brecover.USR.MedianBiomass.",surveyyear,".png"),width=15,height=10,units = "in",dpi=300)









ggplot(dat, aes(x=YearSurvey, y=B.median)) +
  geom_point( size = 2) +
  geom_line( lwd = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab ("Year") + ylab("Commercial Biomass (mt)") + 
  #geom_hline(data = dat.combio.median, aes(yintercept=median_val), linetype="dashed",   color = "blue", size=0.5) +
  geom_hline(yintercept=LRP, linetype="dashed",   color = "red", size=0.5) +
  #ylim(0,2000) + 
  scale_x_continuous(breaks = seq(1980,2020,by=2)) +
  scale_y_continuous(breaks = seq(0,1500,by=250), limits = c(0,1500), expand = c(0,0)) +
  geom_rect(data = PA.limits, aes(ymin = -Inf, ymax = LRP, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="firebrick1", inherit.aes = FALSE)# + 
#geom_rect(data = PA.limits, aes(ymin = LRP, ymax = USR, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="goldenrod1", inherit.aes = FALSE) + 
#geom_rect(data = PA.limits, aes(ymin = USR, ymax = Inf, xmin = -Inf, xmax = Inf), alpha = 0.3, fill="chartreuse2", inherit.aes = FALSE)





### END ###

## Use pieces of below code for creating plots with french labels 

	### EN FRANCAIS!!!
	library (plyr)
  FR <- c ("ZPP29A","ZPP29B","ZPP9C","ZPP29D")
  SUBAREA <- c("SFA29A","SFA29B","SFA29C","SFA29D")
  SUB_FR <- data.frame (SUBAREA, FR)
  
  XX_FR <- join (XX, SUB_FR, type = 'left')
  XX_FR$FR <- ordered(XX_FR$FR,c("ZPP9C","ZPP29D","ZPP29A","ZPP29B"))
  XX_FR$Strata <- ordered(XX_FR$Strata,c("high","med","low"))
  
	scall.plot <- xyplot(Mean ~ YEAR|FR, groups = Strata,  scales=list(x=list(tick.number=8), tick.number=7, alternating=FALSE, cex=1.1,cex.lab=2),
	                     data=XX_FR, pch = c(3,2,1), col = c("green","red","black"), type="b", lty=c(3,2,1), # colors = 1 (black), 2 (red), 3 (green)
	                     ylim=ylimits, 
	                     as.table = FALSE,
	                     xlab=list("Ann?e",cex=1.2),ylab=list ("Nombre moyen par trait",cex=1.2),  main="",
	                     key= list(x=.30,y=.95,corner=c(1,1), transparent=TRUE, lines =list(lty=c(3,2,1),col=c("green","red","black"),
	                       pch=c(3,2,1),type="b"),divide=1,cex=1.1, text=list(c("Elev?","Moyen","Faible"))))
	
	windows()
	scall.plot
	
	png(paste0 ("figures/NumPerTow_", size, "_2015corr","_FR", ".png"),11,9,res=400,units='in')
	scall.plot
	dev.off()
	
	





