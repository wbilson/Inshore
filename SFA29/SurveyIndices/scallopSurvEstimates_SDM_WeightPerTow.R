###..........................###
### Survey Scallop estimates ###
###         SDM              ###
###     J. Sameoto           ###
###..........................###
# Updated Feb 2016 . J.Sameoto
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

#Bring in survey tow data with SDM value (note - SFA29_SDM_LWM.R script must be run to get updated survey tows with SDM values prior to runnint this script)
sdmtows <- read.csv("Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv")
table(sdmtows$CRUISE)
sdmtows$uid <- paste(sdmtows$CRUISE, sdmtows$TOW_NO, sep=".")
sdmtows <- sdmtows[,c("uid","SDM")]

#towable units by subarea and strata 
towable.units <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Scripts/Model/SFA29W_model_areas_towable_units.csv")) 

# Get meat weight data - cannot calculate weight per tow prior to 2014 since cannot reproduce the mw-sh models. Stephen did the models for pre 2014 and don't have the weight-SHF data - using his values of the summarized data 
weight.per.tow.previous <- read.csv(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear-1,"/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear-1,".Commercial.Weight.csv"))
#only want up to 2013 since this script will calculate 2014  on 
weight.per.tow.previous <- weight.per.tow.previous %>% filter(YEAR < 2014)

#Summary check of previous year data 
weight.per.tow.previous %>% group_by(YEAR, SUBAREA, Strata) %>% summarize(count = n()) %>% print(n=Inf)


# sfa29shw.dat <- read.csv("dataoutput/SFA29liveweight2014_JS.csv") #note 2014 data from Jessica's MTWT-SH model - will be slightly different than values from Stephen
sfa29shw.dat <- read.csv(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear,"/Assessment/Data/SurveyIndices/SFA29liveweight2014to",surveyyear,".csv"))
# CHECK TO MAKE SURE DIMS ARE CORRECT (Cutting off "X" columns + check that length freq bins are correct)
sfa29shw.dat <- sfa29shw.dat[,3:dim(sfa29shw.dat)[2]]
#sfa29shw.dat[,12:51] <-  sfa29shw.dat[,12:51]/1000 #convert tow size bins from grams to kg
sfa29shw.dat[,c(grep("BIN_ID_0",names(sfa29shw.dat)):grep("BIN_ID_195",names(sfa29shw.dat)))] <-  sfa29shw.dat[,c(grep("BIN_ID_0",names(sfa29shw.dat)):grep("BIN_ID_195",names(sfa29shw.dat)))]/1000 #convert tow size bins from grams to kg

data.obj <- sfa29shw.dat
dim(data.obj)

# Check to see if any NA's in data & remove records with NAs in data - should not be any 
	dim(data.obj)
	data.obj[is.na(rowSums(data.obj[,c(grep("BIN_ID_0",names(sfa29shw.dat)):grep("BIN_ID_195",names(sfa29shw.dat)))])),]
	#data.obj <- data.obj[!is.na(rowSums(data.obj[,c(grep("BIN_ID_0",names(sfa29shw.dat)):grep("BIN_ID_195",names(sfa29shw.dat)))])),]
	dim(data.obj)

#Bring in survey tow data with SDM value (note - SFA29_SDM_LWM.R script must be run to get updated survey tows with SDM values prior to runnint this script)
	sdmtows <- read.csv("Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv")
	table(sdmtows$CRUISE)
	sdmtows$uid <- paste(sdmtows$CRUISE, sdmtows$TOW_NO, sep=".")
	sdmtows <- sdmtows[,c("uid","SDM")]
#make sure unique tow
	length(unique(sdmtows$uid))
	dim(sdmtows)
	
# Assign subarea names
	strata.names <- paste0("SFA29",LETTERS[1:5])
	strata.ids <- sort(unique(data.obj$STRATA_ID))
	data.obj$STRATA <- NA
	for(i in 1:length(strata.ids)) data.obj$STRATA[data.obj$STRATA_ID==strata.ids[i]] <- strata.names[i]
	
# Assign year
	data.obj$YEAR <- as.numeric(substr(data.obj$CRUISE,6,9))
	
# Calc unique id
	data.obj$uid <- paste(data.obj$CRUISE, data.obj$TOW_NO, sep=".")
	length(unique(	data.obj$uid ))
	table(	data.obj$YEAR)
	
	
#Pre-recruits per tow 0 to 89 mm SH;   note for Prerecruits for 2013 assessment - just used 20-60mm#
	data.obj$pre.bm <-  data.obj %>% dplyr::select(grep("BIN_ID_0",names(data.obj)):grep("BIN_ID_85",names(data.obj))) %>%  rowSums(na.rm=TRUE)
	
#recruits per tow 90 to 99 mm SH 
	data.obj$rec.bm <- data.obj %>% dplyr::select(grep("BIN_ID_90",names(data.obj)):grep("BIN_ID_95",names(data.obj))) %>%  rowSums(na.rm=TRUE)
	
#commercial	per tow >= 100 mm SH 
	data.obj$com.bm <- data.obj %>% dplyr::select(grep("BIN_ID_100",names(data.obj)):grep("BIN_ID_195",names(data.obj))) %>%  rowSums(na.rm=TRUE)

	dim(data.obj)
# Left join survey tows to surficial substrate tows on uid
	data.obj <- merge(data.obj, surf.all, by.x='uid', by.y='uid', all.x=TRUE)
	dim(data.obj)
	
# Left join survey tows to SDM level on uid
	data.obj <- merge(data.obj, sdmtows, by.x='uid', by.y='uid', all.x=TRUE)
	dim(data.obj)
	
#In 2024 did tows outside 29W and also in non-MBES covered part of B; remove these so apples and apples with previous estimates 
	data.obj %>% filter(CRUISE == "SFA292024" & is.na(SDM) == TRUE)
	dim(data.obj)
	
	data.obj <- data.obj[!(data.obj$CRUISE == "SFA292024" & is.na(data.obj$SDM) == TRUE),]
	dim(data.obj)
	
	data.obj.all <- data.obj
	
#check for NAs 
	summary(data.obj)

###
###  ----    Calculate Stratified Random Survey Estimates  ---- 
###   PEDstrata(data.obj, strata.group, strata.name, catch, Subset)                            ###
###   Domain.estimates(data, Strata, Domain, strata.obj, domain.obj, Nd = NULL)                ###
###   post.stratify.all(data, OldStrata, pStrata, Strata.info, oldstrata.name, newstrata.name) ###
###

# ---- PRE-RECRUITS ----		
# NOTE this section of code is run once for each size (comm, rec, prerec). Define below
	strata.group <- SDMareas
	size <- "prerec" # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column  pre.bm, rec.bm, or com.bm)
	data.obj$STDTOTALCAUGHT <- data.obj$pre.bm # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column pre.bm, rec.bm, or com.bm)

	# Only use regular survey tows for estimation (TOW_TYPE_ID = 1)
	data.obj.all <- data.obj
	data.obj <- data.obj[data.obj$TOW_TYPE_ID==1,]
	dim(data.obj)
	head(data.obj)

## NOTE: only updating for current year (below) and saving data from 2014
###... For Years: 2014-YYYY Calculate Stratified Random Survey Estimate - SDM strata ...###
### NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
### DATA: scall.strat.2014toYYYY(data all - stratified SDM estimate), scall.levels.2014toYYYY (data by low, med, high)

	ab <- unique(strata.group$Subarea)
	year <- c(2014:2019,2021:surveyyear) 
	out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),effic.alloc=rep(NA,(length(year)*length(ab))),effic.str=rep(NA,(length(year)*length(ab))),var.ran=rep(NA,(length(year)*length(ab))),max.eff=rep(NA,(length(year)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))


	m <- 0 #index
	for (i in 1:length(year)) {
		temp.data <- data.obj[data.obj$YEAR==year[i],]
		for(j in 1:length(ab)) {
			m=m+1
			strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
			data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
				if(dim(data.obj.i)[1]!=0){
				scall.sum <- summary(PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT),effic = TRUE, nopt = TRUE) #set effic and nopt to TRUE to get Vrandom
				out[m,-(1:2)] <- scall.sum[c(-6,-11)]
				}
			out$YEAR[m] <- year[i]
			out$SUBAREA[m] <- as.character(ab[j])
		}
	}
	out
	out.strat.2014toYYYY <- out
#check unique row for areas
	out.strat.2014toYYYY %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	
	
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
				m=m+1
				if(dim(data.obj.k)[1]!=0){
				out[m,"yst"] <- mean(data.obj.k$STDTOTALCAUGHT)
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
	scall.levels.2014toYYYY <- out
	#check unique row for areas
	scall.levels.2014toYYYY %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	

### ... Merge data for all years into 1 dataframe ... ###
	# Data by Strata: High, Medium, Low; by subare by year
	names(scall.levels.2014toYYYY)[4] <- "Mean"
	names(scall.levels.2014toYYYY)[5] <- "Std.Err"


## AT: Include CV for model input
scall.levels.2014toYYYY$CV <- log((scall.levels.2014toYYYY$"Std.Err"/scall.levels.2014toYYYY$Mean)^2 + 1)  
scall.levels.2014toYYYY$size <- size

#Add 2020 when had no survey
sdm.levels.est.all <- rbind(scall.levels.2014toYYYY, data.frame(YEAR = rep(2020, 12), SUBAREA = c(rep("SFA29A",3),rep("SFA29B",3),rep("SFA29C",3),rep("SFA29D",3)), Strata = rep(c("low","med","high"),4), Mean=rep(NA,12), Std.Err=rep(NA,12),  var.est=rep(NA,12), descrip=rep(NA,12), CV=rep(NA,12), size = rep(size,12)) )
		
sdm.levels.est.all <- sdm.levels.est.all %>% arrange(SUBAREA,YEAR, Strata)
sdm.levels.est.all
		
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
		#check unique row for areas
		sdm.levels.prerec %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
		
		
### Stratified estimates (one estimate for each subarea by year)
		sdm.strat.est.all <- out.strat.2014toYYYY[,c("YEAR","SUBAREA","yst","se.yst","descrip")] 
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
		#check unique row for areas
		sdm.strat.est.all.prerec %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
		
		
		
		
# ---- RECRUITS ----		
	# NOTE this section of code is run once for each size (comm, rec, prerec). Define below
	strata.group <- SDMareas
	size <- "rec" # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column  pre.bm, rec.bm, or com.bm)
	data.obj$STDTOTALCAUGHT <- data.obj$rec.bm # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column pre.bm, rec.bm, or com.bm)
	
	# Only use regular survey tows for estimation (TOW_TYPE_ID = 1)
	data.obj.all <- data.obj
	data.obj <- data.obj[data.obj$TOW_TYPE_ID==1,]
	dim(data.obj)
	head(data.obj)
	
	## NOTE: only updating for current year (below) and saving data from 2014
	###... For Years: 2014-YYYY Calculate Stratified Random Survey Estimate - SDM strata ...###
	### NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
	### DATA: scall.strat.2014toYYYY(data all - stratified SDM estimate), scall.levels.2014toYYYY (data by low, med, high)
	
	ab <- unique(strata.group$Subarea)
	year <- c(2014:2019,2021:surveyyear) 
	out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),effic.alloc=rep(NA,(length(year)*length(ab))),effic.str=rep(NA,(length(year)*length(ab))),var.ran=rep(NA,(length(year)*length(ab))),max.eff=rep(NA,(length(year)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))
	

	m <- 0 #index
	for (i in 1:length(year)) {
	  temp.data <- data.obj[data.obj$YEAR==year[i],]
	  for(j in 1:length(ab)) {
	    m=m+1
	    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
	    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
	    if(dim(data.obj.i)[1]!=0){
	      scall.sum <- summary(PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT),effic = TRUE, nopt = TRUE) #set effic and nopt to TRUE to get Vrandom
	      out[m,-(1:2)] <- scall.sum[c(-6,-11)]
	    }
	    out$YEAR[m] <- year[i]
	    out$SUBAREA[m] <- as.character(ab[j])
	  }
	}
	out
	out.strat.2014toYYYY <- out
	#check unique row for areas
	out.strat.2014toYYYY %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	
	
	
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
	scall.levels.2014toYYYY <- out
	#check unique row for areas
	scall.levels.2014toYYYY %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	
	
	
	### ... Merge data for all years into 1 dataframe ... ###
	# Data by Strata: High, Medium, Low; by subare by year
	names(scall.levels.2014toYYYY)[4] <- "Mean"
	names(scall.levels.2014toYYYY)[5] <- "Std.Err"
	
	#sdm.levels.est.all <- rbind(scall.levels.2001to2004[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2005to2007[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")],
	#			scall.levels.2008to2013[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2014toYYYY[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")])
	#sdm.levels.est.all <- sdm.levels.est.all[order(sdm.levels.est.all$YEAR, sdm.levels.est.all$SUBAREA, sdm.levels.est.all$Strata), ]
	#sdm.levels.est.all$Strata <- as.factor(sdm.levels.est.all$Strata)
	## AT: Include CV for model input
	#sdm.levels.est.all$CV <- log ((sdm.levels.est.all$"Std. Err."/sdm.levels.est.all$Mean)^2 + 1)
	#write.csv(sdm.levels.est.all,paste("dataoutput/SDM_HighMedLow_2001to2014_WGT_",size,".csv",sep=""))
	
	## AT: Include CV for model input
	scall.levels.2014toYYYY$CV <- log((scall.levels.2014toYYYY$"Std.Err"/scall.levels.2014toYYYY$Mean)^2 + 1)  #NEED TO CHECK ON THIS CALCULATION JS 
	scall.levels.2014toYYYY$size <- size
	
	#Add 2020 when had no survey
	sdm.levels.est.all <- rbind(scall.levels.2014toYYYY, data.frame(YEAR = rep(2020, 12), SUBAREA = c(rep("SFA29A",3),rep("SFA29B",3),rep("SFA29C",3),rep("SFA29D",3)), Strata = rep(c("low","med","high"),4), Mean=rep(NA,12), Std.Err=rep(NA,12),  var.est=rep(NA,12), descrip=rep(NA,12), CV=rep(NA,12), size = rep(size,12)) )
	
	sdm.levels.est.all <- sdm.levels.est.all %>% arrange(SUBAREA,YEAR, Strata)
	sdm.levels.est.all
	
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
	#check unique row for areas
	sdm.levels.rec %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	
	
	
	
### Stratified estimates (one estimate for each subarea by year)
	sdm.strat.est.all <- out.strat.2014toYYYY[,c("YEAR","SUBAREA","yst","se.yst","descrip")] 
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
	#check unique row for areas
	sdm.strat.est.all.rec %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	

# ---- COMMERCIAL ----		
	# NOTE this section of code is run once for each size (comm, rec, prerec). Define below
	strata.group <- SDMareas
	size <- "comm" # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column  pre.bm, rec.bm, or com.bm)
	data.obj$STDTOTALCAUGHT <- data.obj$com.bm # MUST DEFINE if prerecruits, recruits or commercial size (i.e. column pre.bm, rec.bm, or com.bm)
	
	# Only use regular survey tows for estimation (TOW_TYPE_ID = 1)
	data.obj.all <- data.obj
	data.obj <- data.obj[data.obj$TOW_TYPE_ID==1,]
	dim(data.obj)
	head(data.obj)
	
	## NOTE: only updating for current year (below) and saving data from 2014
	###... For Years: 2014-YYYY Calculate Stratified Random Survey Estimate - SDM strata ...###
	### NOTE: entries for Subarea in strata.group and STRATA in data.obj must be equal #
	### DATA: scall.strat.2014toYYYY(data all - stratified SDM estimate), scall.levels.2014toYYYY (data by low, med, high)
	
	ab <- unique(strata.group$Subarea)
	year <- c(2014:2019,2021:surveyyear) 
	out <- data.frame(YEAR=rep(NA,(length(year)*length(ab))),SUBAREA=rep(NA,(length(year)*length(ab))),yst=rep(NA,(length(year)*length(ab))),se.yst=rep(NA,(length(year)*length(ab))),Yst=rep(NA,(length(year)*length(ab))),df.yst=rep(NA,(length(year)*length(ab))),alpha=rep(NA,(length(year)*length(ab))),effic.alloc=rep(NA,(length(year)*length(ab))),effic.str=rep(NA,(length(year)*length(ab))),var.ran=rep(NA,(length(year)*length(ab))),max.eff=rep(NA,(length(year)*length(ab))),descrip=rep(NA,(length(year)*length(ab))))
	
	
	m <- 0 #index
	for (i in 1:length(year)) {
	  temp.data <- data.obj[data.obj$YEAR==year[i],]
	  for(j in 1:length(ab)) {
	    m=m+1
	    strata.group.i <- strata.group[strata.group$Subarea==ab[j],]
	    data.obj.i <- temp.data[temp.data$STRATA==ab[j],]
	    if(dim(data.obj.i)[1]!=0){
	      scall.sum <- summary(PEDstrata(data.obj.i, strata.group.i,'SDM',catch=data.obj.i$STDTOTALCAUGHT),effic = TRUE, nopt = TRUE) #set effic and nopt to TRUE to get Vrandom
	      out[m,-(1:2)] <- scall.sum[c(-6,-11)]
	    }
	    out$YEAR[m] <- year[i]
	    out$SUBAREA[m] <- as.character(ab[j])
	  }
	}
	out
	out.strat.2014toYYYY <- out
	#check unique row for areas
	out.strat.2014toYYYY %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	


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
	scall.levels.2014toYYYY <- out
	#check unique row for areas
	scall.levels.2014toYYYY %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	scall.levels.2014toYYYY


	### ... Merge data for all years into 1 dataframe ... ###
	# Data by Strata: High, Medium, Low; by subare by year
	names(scall.levels.2014toYYYY)[4] <- "Mean"
	names(scall.levels.2014toYYYY)[5] <- "Std.Err"
	
	#sdm.levels.est.all <- rbind(scall.levels.2001to2004[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2005to2007[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")],
	#			scall.levels.2008to2013[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2014toYYYY[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")])
	#sdm.levels.est.all <- sdm.levels.est.all[order(sdm.levels.est.all$YEAR, sdm.levels.est.all$SUBAREA, sdm.levels.est.all$Strata), ]
	#sdm.levels.est.all$Strata <- as.factor(sdm.levels.est.all$Strata)
	## AT: Include CV for model input
	#sdm.levels.est.all$CV <- log ((sdm.levels.est.all$"Std. Err."/sdm.levels.est.all$Mean)^2 + 1)
	#write.csv(sdm.levels.est.all,paste("dataoutput/SDM_HighMedLow_2001to2014_WGT_",size,".csv",sep=""))
	
	## AT: Include CV for model input
	scall.levels.2014toYYYY$CV <- log((scall.levels.2014toYYYY$"Std.Err"/scall.levels.2014toYYYY$Mean)^2 + 1)  #NEED TO CHECK ON THIS CALCULATION JS 
	scall.levels.2014toYYYY$size <- size
	
	#Add 2020 when had no survey
	sdm.levels.est.all <- rbind(scall.levels.2014toYYYY, data.frame(YEAR = rep(2020, 12), SUBAREA = c(rep("SFA29A",3),rep("SFA29B",3),rep("SFA29C",3),rep("SFA29D",3)), Strata = rep(c("low","med","high"),4), Mean=rep(NA,12), Std.Err=rep(NA,12),  var.est=rep(NA,12), descrip=rep(NA,12), CV=rep(NA,12), size = rep(size,12)) )
	
	sdm.levels.est.all <- sdm.levels.est.all %>% arrange(SUBAREA,YEAR, Strata)
	sdm.levels.est.all
	
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
	#check unique row for areas
	sdm.levels.comm %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	

### Stratified estimates (one estimate for each subarea by year)
	sdm.strat.est.all <- out.strat.2014toYYYY[,c("YEAR","SUBAREA","yst","se.yst","descrip")] 
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
	#check unique row for areas
	sdm.strat.est.all.comm %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	
	
# ---- Merge dataframes A-D & calculate inputs for model ---- 
	sdm.levels.prerec %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	sdm.levels.rec %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	sdm.levels.comm %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	
	
	#estimates by SDM strata 
	sdm.levels <- rbind(sdm.levels.prerec, sdm.levels.rec, sdm.levels.comm) 
	#check unique row for areas
	sdm.levels %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	sdm.levels %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	
	#at this point everything is OK.. for 2014 on for rows per subarea per strata 
	
	#merge to pre-2014 weight per tow for commerical size  
	pre.2014 <- weight.per.tow.previous %>% dplyr::select(YEAR, Mean, SUBAREA, Strata) %>% filter(YEAR < 2014)
	#check unique row for areas
	pre.2014 %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	
	#just commercial size since 2014 
	since.2014 <- sdm.levels %>% filter(size == "comm") %>% dplyr::select(YEAR, Mean, SUBAREA, Strata)
	#check unique row for areas
	since.2014 %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)

	#combined old pre 2014 data with estimates since 2014 calculated in this script 
	sdm.levels.2001toYYYY <- rbind(pre.2014, since.2014)
	
	#check unique row for areas
	sdm.levels.2001toYYYY %>% group_by(YEAR, SUBAREA, Strata) %>% summarise(count = n()) %>% print(n = Inf)
	
	#writeout data 
	write.csv(sdm.levels.2001toYYYY, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Commercial.Weight.csv"), row.names = F)
	
	
	#Stratified estimates 
	sdm.strat.est <- rbind(sdm.strat.est.all.prerec, sdm.strat.est.all.rec, sdm.strat.est.all.comm)
	#check unique row for areas
	sdm.strat.est %>% group_by(YEAR, SUBAREA) %>% summarise(count = n()) %>% print(n = Inf)
	
	#writeout data 
	write.csv(sdm.strat.est, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2014to",surveyyear,".Weight.csv"))
	
	
	## data for model
	# "Ih" - commercial biomass scaled to respective SDM strata in millions 
	# "obs.tau" commercial biomass  per tow CV - note only from 2014 on since using Stephen's cv values from the 2014 framework, sorry I named the output file since 2001, not trying to confuse on purpose but can we just leave as is? If change need to update Create.model.files.R.... 
	Ih.obs.tau <- sdm.levels %>% filter(size == "comm" & !(SUBAREA == "SFA29A" & Strata == "high")) %>% dplyr::select(YEAR, SUBAREA, Strata, Mean, CV, size) 
	Ih.obs.tau <-  merge(Ih.obs.tau, towable.units, by = c("SUBAREA", "Strata")) 
	
	#Scale to area 
	Ih.obs.tau <- Ih.obs.tau %>% mutate(Ih = round(((Mean*TowableUnits)/1000),digits =4)) #units mt 
	Ih.obs.tau <- Ih.obs.tau %>% arrange(SUBAREA, Strata, YEAR)
	head(Ih.obs.tau)
	Ih.obs.tau %>% arrange(YEAR, SUBAREA, Strata)
	
	write.csv(Ih.obs.tau, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/Ih.obs.tau.2001to",surveyyear,".csv"))
	
	
### --- Plots weight per tow A to D Commercial size ---- 
#can't plot full time series for recruits for weight bc don't have that pre-2014 time series from Stephen -- but recruit weights not used in the model; numbers with avg weight is 
	unique(sdm.levels.2001toYYYY$Strata)
	
	#For plots - removing 2020 values
	sdm.levels.2001toYYYY <- sdm.levels.2001toYYYY %>% 
	  mutate(Mean = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ Mean)) |> 
	  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
	                             SUBAREA == "SFA29B" ~ "Subarea B",
	                             SUBAREA == "SFA29C" ~ "Subarea C",
	                             SUBAREA == "SFA29D" ~ "Subarea D"))
	
	#sdm.levels.2001toYYYY$Strata.f <- factor(sdm.levels.2001toYYYY$Strata,  levels = c("high", "med",  "low"))
	#levels(sdm.levels.2001toYYYY$Strata.f)
	
	#colors.sdm <- c("forestgreen",  "darkblue", "firebrick2") #(or 'forestgreen') (med, high, low)
	colors.sdm <- c('firebrick2', 'darkgrey', 'darkblue')
	
	## All Subareas A-D Commercial  
	AtoD.per.tow.comm <- ggplot(data = sdm.levels.2001toYYYY %>% filter(!(SUBAREA == "Subarea A" & Strata == "high")), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
	  geom_point() + 
	  geom_line(aes(linetype = Strata)) + 
	  facet_wrap(~SUBAREA, ncol=2) + 
	  theme_bw() + ylab("Mean weight/tow (kg)") + xlab("Year") + 
	  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank(), text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
	  scale_color_manual(values=colors.sdm, breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) + 
	  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))
	AtoD.per.tow.comm
	
#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Weightpertow.Commercial.",surveyyear,".png"), plot = AtoD.per.tow.comm, scale = 2.5, width =8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
	
### --- Extra - Plots weight per tow A to D Recruit size ---- 
#can't plot full time series for recruits for weight bc don't have that pre-2014 time series from Stephen -- but recruit weights not used in the model; numbers with avg weight is 

#rec.sdm.levels.2014.YYYY <- sdm.levels %>% filter(size == "rec") %>% dplyr::select(YEAR, Mean, SUBAREA, Strata)

#colors.sdm <- c("forestgreen",  "darkblue", "firebrick2") #(or 'forestgreen') (med, high, low)
#colors.sdm <- c('firebrick2', 'darkgrey', 'darkblue')

## All Subareas A-D Recruit  
#AtoD.per.tow.rec <- ggplot(data = rec.sdm.levels.2014.YYYY %>% filter(!(SUBAREA == "SFA29A" & Strata == "high")), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
#  geom_point() + 
#  geom_line(aes(linetype = Strata)) + 
#  facet_wrap(~SUBAREA, ncol=2) + 
#  theme_bw() + ylab("Mean weight/tow (kg)") + xlab("Year") + 
#  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank()) + 
#  scale_color_manual(values=colors.sdm, breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
#  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
#  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))
#AtoD.per.tow.rec

#save
#ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Weightpertow.Recruit.2014-.",surveyyear,".png"), plot = AtoD.per.tow.rec, scale = 2.5, width =6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)


### --- stratified estimate since 2014 ----- 

LTM.comm <-  sdm.strat.est %>% filter(size == "comm" & YEAR < surveyyear) %>% group_by(SUBAREA) %>% summarise(LTM = median(yst , na.rm = TRUE  )) %>% 
  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
                             SUBAREA == "SFA29B" ~ "Subarea B",
                             SUBAREA == "SFA29C" ~ "Subarea C",
                             SUBAREA == "SFA29D" ~ "Subarea D"))

LTM.comm

unique(sdm.levels.2001toYYYY$Strata)

#For plots - removing 2020 values
sdm.strat.est <- sdm.strat.est %>% 
  mutate(Mean = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ yst), se = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ se.yst)) |> 
  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
                             SUBAREA == "SFA29B" ~ "Subarea B",
                             SUBAREA == "SFA29C" ~ "Subarea C",
                             SUBAREA == "SFA29D" ~ "Subarea D"))

## All Subareas A-D Commercial  stratified estimates 
strat.comm.plot <- ggplot(data = sdm.strat.est %>% filter(size == "comm"), aes(x=YEAR, y=Mean)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar( aes(x=YEAR, ymin=yst-se, ymax=yst+se), width=0.2, colour="orange", alpha=0.9, size=0.5) + 
  geom_hline(data = LTM.comm, aes(yintercept=LTM), col = "blue", linetype = "dashed")+
  facet_wrap(~SUBAREA, ncol=2) + 
  theme_bw() + ylab("Mean weight/tow (kg)") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank()) + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026))
strat.comm.plot

#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Weightpertow.Commercial.stratifiedestimate.since2014.",surveyyear,".png"), plot = strat.comm.plot, scale = 2.5, width =6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)



	
### ---- SUBAREA E ----  
## NO DOMAIN DATAFRAME FOR E - for 2005 take simple mean #
	
	#prerecruits
	E.area <- data.obj.all[data.obj.all$STRATA_ID==45,]
	sizeE.prerec <- "prerec"	# Define - ensure matches entry on next line where assign E.area$STDTOTALCAUGHT
	E.area$STDTOTALCAUGHT <- E.area$pre.bm #DEFINE SIZE; comm, rec, prerec
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
	E.area$STDTOTALCAUGHT <- E.area$rec.bm #DEFINE SIZE; comm, rec, prerec
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
	E.area$STDTOTALCAUGHT <- E.area$com.bm #DEFINE SIZE; comm, rec, prerec
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
#	out.e.prerec <- out.e.prerec %>% add_row(YEAR = c(2006:2011), size = "prerec", SUBAREA = "SFA29E")
	out.e.prerec <- out.e.prerec %>% arrange(YEAR)
	out.e.prerec
	
	out.e.rec <- out.e.rec %>% add_row(YEAR = 2020, size = "rec", SUBAREA = "SFA29E")
	#	out.e.rec[out.e.rec$YEAR==2020,c("yst","var.yst","se.yst")] <- c(approx(out.e.rec$YEAR, out.e.rec$yst, xout=2020)$y, out.e.rec$var.yst[out.e.rec$YEAR==2019], out.e.rec$se.yst[out.e.rec$YEAR==2019]) #assume var & se from 2019
#	out.e.rec <- out.e.rec %>% add_row(YEAR = c(2006:2011), size = "rec", SUBAREA = "SFA29E")
	out.e.rec <- out.e.rec %>% arrange(YEAR)
	out.e.rec
	
	out.e.comm <- out.e.comm %>% add_row(YEAR = 2020, size = "comm", SUBAREA = "SFA29E")
	#	out.e.comm[out.e.comm$YEAR==2020,c("yst","var.yst","se.yst")] <- c(approx(out.e.comm$YEAR, out.e.comm$yst, xout=2020)$y, out.e.comm$var.yst[out.e.comm$YEAR==2019], out.e.comm$se.yst[out.e.comm$YEAR==2019]) #assume var & se from 2019
#	out.e.comm <- out.e.comm %>% add_row(YEAR = c(2006:2011), size = "comm", SUBAREA = "SFA29E")
	out.e.comm <- out.e.comm %>% arrange(YEAR)
	out.e.comm
	
	
	#merge together 
	out.e <- 	rbind(out.e.prerec, out.e.rec, out.e.comm)
	
	#writeout 
	write.csv(out.e, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SubareaE.ExploratoryWeightbyTow.",surveyyear,".csv",sep=""))
	
	
# ---- Plot Subarea E Weight per tow ----- 
	out.e$group <- factor(out.e$size,      # Reordering group factor levels
	                      levels = c("prerec", "rec", "comm"))

	size_names <- as_labeller(
	  c(`prerec` = "Prerecruits (<90 mm)", `rec` = "Recruits (90-99 mm)",`comm` = "Commercial (>= 100 mm)"))
	
	E.weight.per.tow <- ggplot(data = out.e, aes(x=YEAR, y=yst)) + 
	  geom_point() + 
	  geom_line() + 
	  scale_x_continuous(limits = c(2013, (surveyyear+1)), breaks = seq(2013, (surveyyear+1), by = 4)) +
	  facet_wrap(~group, ncol=1, labeller = size_names, scales = "free") + 
	  theme_bw() + ylab("Mean weight/tow (kg)") + xlab("Year") + 
	  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank()) + 
	  geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst))  + 
	  scale_x_continuous(breaks = seq(2013,2026,by=4), limits = c(2013,2026)) #+ 
	E.weight.per.tow
	
	#save
	ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29E.Weightpertow.",surveyyear,".png"), plot = E.weight.per.tow, scale = 2.5, width =6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
	
	#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29E.Weightpertow.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
	#E.weight.per.tow
	#dev.off()
	
	
## END ## 
	
	

