###..........................###
### Survey Scallop estimates ###
###         SDM              ###
###     J. Sameoto           ###
###..........................###
# Overhauled in Dec 2021 J.Sameoto 

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
#setwd('Y:/Inshore/BoF/Assessment_fns/SFA29W')
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
#path.directory <- "Y:/Inshore/SFA29/"
years <- c(2001:surveyyear) #when have 2021 data ready with SDM value then can use line of code below 
#yr.crnt <- surveyyear-1

#Bring in survey tow data with SDM value (note - SFA29_SDM_LWM.R script must be run to get updated survey tows with SDM values prior to runnint this script)
sdmtows <- read.csv("Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv")
table(sdmtows$CRUISE)
sdmtows$uid <- paste(sdmtows$CRUISE, sdmtows$TOW_NO, sep=".")
sdmtows <- sdmtows[,c("uid","SDM")]

#towable units by subarea and strata 
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
# To select CLAPPERS - select from SCDEADRES 

quer2 <- paste(
		"SELECT *                                     ",
		"FROM scallsur.SCDEADRES                      ",
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
	
#In 2024 did tows outside 29W and also in non-MBES covered part of B; remove these so apples and apples with previous estimates 
	data.obj %>% filter(CRUISE == "SFA292024" & is.na(SDM) == TRUE)
	dim(data.obj)
	
	data.obj <- data.obj[!(data.obj$CRUISE == "SFA292024" & is.na(data.obj$SDM) == TRUE),]
	dim(data.obj)
	
	data.obj.all <- data.obj

# check for NA records 
	summary(data.obj)
	
####
###  ----   Calculate Stratified Random Survey Estimates ----             
###   PEDstrata(data.obj, strata.group, strata.name, catch, Subset)                            ###
###   Domain.estimates(data, Strata, Domain, strata.obj, domain.obj, Nd = NULL)                ###
###   post.stratify.all(data, OldStrata, pStrata, Strata.info, oldstrata.name, newstrata.name) ###
####
	
# ---- PRE-RECRUITS ----	
  # NOTE this section of code is run once for each size (comm, rec, precec). Define below
	strata.group <- SDMareas
	size <- "prerec" # MUST DEFINE if precruits, recruits or commercial size (i.e. column prerec, rec, or comm)
	data.obj$STDTOTALCAUGHT <- data.obj$prerec # MUST DEFINE if precruits, recruits or commercial size (i.e. column prerec, rec, or comm)

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

sdm.levels.prerec <- sdm.levels.est.all
sdm.levels.prerec

#writeout data 
#write.csv(sdm.levels.prerec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Clappers.",size,".csv"))
	
	
	
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
#write.csv(sdm.strat.est.all.prerec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Clappers.",size,".csv"))




# ---- RECRUITS ---- 
# NOTE this section of code is run once for each size (comm, rec, precec). Define below
strata.group <- SDMareas
size <- "rec" # MUST DEFINE if precruits, recruits or commercial size (i.e. column prerec, rec, or comm)
data.obj$STDTOTALCAUGHT <- data.obj$rec # MUST DEFINE if precruits, recruits or commercial size (i.e. column prerec, rec, or comm)

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

#writeout data 
#write.csv(sdm.levels.rec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Clappers.",size,".csv"))



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

sdm.strat.est.all.rec <- sdm.strat.est.all
sdm.strat.est.all.rec

#writeout data 
#write.csv(sdm.strat.est.all.rec, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Clappers.",size,".csv"))



# ---- COMMERCIAL ---- 
# NOTE this section of code is run once for each size (comm, rec, precec). Define below
strata.group <- SDMareas
size <- "comm" # MUST DEFINE if precruits, recruits or commercial size (i.e. column prerec, rec, or comm)
data.obj$STDTOTALCAUGHT <- data.obj$comm # MUST DEFINE if precruits, recruits or commercial size (i.e. column prerec, rec, or comm)

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

#writeout data 
#write.csv(sdm.levels.comm, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Clappers.",size,".csv"))



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

sdm.strat.est.all.comm <- sdm.strat.est.all
sdm.strat.est.all.comm

#writeout data 
#write.csv(sdm.strat.est.all.comm, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Clappers.",size,".csv"))


# ---- Merge dataframes A-D & calculate inputs for model ---- 

#estimates by SDM strata 
sdm.levels <- rbind(sdm.levels.prerec, sdm.levels.rec, sdm.levels.comm) 

#writeout data 
write.csv(sdm.levels, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Clappers.csv"))

#Stratified estimates 
sdm.strat.est <- rbind(sdm.strat.est.all.prerec, sdm.strat.est.all.rec, sdm.strat.est.all.comm)

#writeout data 
write.csv(sdm.strat.est, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.StratifiedEstimates.2001to",surveyyear,".Clappers.csv"))


## data for model
# "clappers" - commercial clapper numbers scaled to respective SDM strata in millions 
# "obs.phi" commercial clapper numbers per tow CV
clappers.obs.phi <- sdm.levels %>% filter(size == "comm" & !(SUBAREA == "SFA29A" & Strata == "high")) %>% dplyr::select(YEAR, SUBAREA, Strata, Mean, CV, size) 
clappers.obs.phi <-  merge(clappers.obs.phi, towable.units, by = c("SUBAREA", "Strata")) 
clappers.obs.phi <- clappers.obs.phi %>% mutate(clappers = round(((Mean*TowableUnits)/1000000),digits =4)) 
clappers.obs.phi <- clappers.obs.phi %>% arrange(SUBAREA, Strata, YEAR)
head(clappers.obs.phi)
write.csv(clappers.obs.phi, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/clappers.obs.phi.2001to",surveyyear,".csv"))


### --- Plots Clapper number per tow A to D ---- 
sdm.levels$group <- factor(sdm.levels$size,      # Reordering group factor levels
                      levels = c("prerec", "rec", "comm"))
size_names <- as_labeller(
  c(`prerec` = "Precrecruits (<90 mm)", `rec` = "Recruits (90-99 mm)",`comm` = "Commercial (>= 100 mm)"))

#For plots - removing 2020 values
sdm.levels <- sdm.levels %>% 
  mutate(Mean = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ Mean)) %>%
  mutate(Std.Err = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ Std.Err)) %>%
  mutate(var.est = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ var.est)) %>%
  mutate(CV = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ CV)) %>% 
  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
                             SUBAREA == "SFA29B" ~ "Subarea B",
                             SUBAREA == "SFA29C" ~ "Subarea C",
                             SUBAREA == "SFA29D" ~ "Subarea D"))


## Subarea A 
A.number.per.tow <- ggplot(data = sdm.levels %>% filter(SUBAREA == "Subarea A" & Strata != "high"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~group, ncol=1, labeller = size_names) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank())  + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026)) #+ #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
A.number.per.tow

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29A.Numberspertow.Clappers.",surveyyear,".png"), plot = A.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29A.Numberspertow.Clappers.",surveyyear,".png"),width=8,height=11,units = "in"#,res=300)
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
  theme(legend.position = c(0.1, 0.89),panel.grid.minor = element_blank())  + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026)) #+ #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
B.number.per.tow

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29B.Numberspertow.Clappers.",surveyyear,".png"), plot = B.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29B.Numberspertow.Clappers.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
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
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026)) #+  #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
C.number.per.tow

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29C.Numberspertow.Clappers.",surveyyear,".png"), plot = C.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29C.Numberspertow.Clappers.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
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
  theme(legend.position = c(0.1, 0.89),legend.background = element_rect(fill=alpha('white', 0.8)),panel.grid.minor = element_blank())  + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026)) #+ #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
D.number.per.tow

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29D.Numberspertow.Clappers.",surveyyear,".png"), plot = D.number.per.tow, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29D.Numberspertow.Clappers.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
#D.number.per.tow
#dev.off()

## All Subareas A-D Pre-recruits 
AtoD.number.per.tow.prerec <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "prerec"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  facet_wrap(~SUBAREA, ncol=2) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank(), text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12))  + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4)) #+ #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
AtoD.number.per.tow.prerec

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Clappers.Prerecruit.",surveyyear,".png"), plot = AtoD.number.per.tow.prerec, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Numberspertow.Clappers.Prerecruit.",surveyyear,".png"),width=11,height=11#,units = "in",res=300)
#AtoD.number.per.tow.prerec
#dev.off()

## All Subareas A-D Recruits 
AtoD.number.per.tow.rec <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "rec"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) +
  facet_wrap(~SUBAREA, ncol=2) + 
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank(), text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12))  + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4)) #+ #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
AtoD.number.per.tow.rec

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Clappers.Recruit.",surveyyear,".png"), plot = AtoD.number.per.tow.rec, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Numberspertow.Clappers.Recruit.",surveyyear,".png"),width=11,height=11,units = "in",res=300)
#AtoD.number.per.tow.rec
#dev.off()



## All Subareas A-D Commercial  
AtoD.number.per.tow.comm <- ggplot(data = sdm.levels %>% filter(!(SUBAREA == "Subarea A" & Strata == "high") & sdm.levels$size == "comm"), aes(x=YEAR, y=Mean,  col=Strata, pch=Strata)) + 
  geom_point() + 
  geom_line(aes(linetype = Strata)) + 
  facet_wrap(~SUBAREA, ncol=2) +
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank(), text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12))  + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4)) #+ #+ 
# geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
#             alpha=0.1,       #transparency
#             linetype=1,      #solid, dashed or other line types
#             colour="grey70", #border line color
#             size=1,          #border line size
#             fill="grey70") 
AtoD.number.per.tow.comm

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29AtoD.Numberspertow.Clappers.Commercial.",surveyyear,".png"), plot = AtoD.number.per.tow.comm, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Numberspertow.Clappers.Commercial.",surveyyear,".png"),width=11,height=11,units = "in",res=300)
#AtoD.number.per.tow.comm
#dev.off()

### ---- SUBAREA E ----  
# NO DOMAIN DATAFRAME FOR E - for 2005 take simple mean #
#some year were all exploratory tows - would need to get data from data.obj.all

#precruits
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
	write.csv(out.e, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SubareaE.ExploratoryMeanbyTow.Clappers.",surveyyear,".csv",sep=""))


# ---- Plot Subarea E Clappers per tow ----- 
out.e$group <- factor(out.e$size,      # Reordering group factor levels
	                        levels = c("prerec", "rec", "comm"))
size_names <- as_labeller(
	  c(`prerec` = "Precrecruits (<90 mm)", `rec` = "Recruits (90-99 mm)",`comm` = "Commercial (>= 100 mm)"))
	
E.number.per.tow <- ggplot(data = out.e, aes(x=YEAR, y=yst)) + 
	  geom_point() + 
	  geom_line() + 
	  facet_wrap(~group, ncol=1, labeller = size_names) + 
	  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
	  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank()) + 
   geom_pointrange(aes(ymin=yst-se.yst, ymax=yst+se.yst))  + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026)) #+ 
	 # geom_ribbon(aes(ymin=out.e$yst-out.e$se.yst, ymax=out.e$yst+out.e$se.yst), 
	 #             alpha=0.1,       #transparency
	 #             linetype=1,      #solid, dashed or other line types
	 #             colour="grey70", #border line color
	 #             size=1,          #border line size
	 #             fill="grey70") 
E.number.per.tow

ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29E.Numberspertow.Clappers.",surveyyear,".png"), plot = E.number.per.tow, scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29E.Numberspertow.Clappers.",surveyyear,".png"),width=8,height=11,units = "in",res=300)
#E.number.per.tow
#dev.off()
	
# ---- Proportion of Clappers ---- 
# NOTE - since this is calculated relative to LIVE + DEAD need to have LIVE numbers script run first before you can do this part: 
#dead <- read.csv(paste0("dataoutput/SDM_HighMedLow_2001to", surveyyear,"_DEAD_",size,".csv"))	 #update file name for current year
#live <- read.csv(paste0("dataoutput/SDM_HighMedLow_2001to", surveyyear,"_",size,".csv"))	 #update file name for current year

#read in data 
dead <- read.csv(paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Clappers.csv"))
live <- read.csv(paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Numbers.csv"))
dead <- dead %>% dplyr::select(YEAR, SUBAREA, Strata, Dead.no = Mean , size) 
live <- live %>% dplyr::select(YEAR, SUBAREA, Strata, Live.no = Mean , size) 
prop.dead <- merge(live, dead, by=c("YEAR","SUBAREA","Strata","size"))
prop.dead$prop.dead <- 	 prop.dead$Dead.no /(prop.dead$Dead.no+prop.dead$Live.no)
prop.dead$prop.dead.no.NAs <- prop.dead$prop.dead
# turn all NaN to 0 
prop.dead$prop.dead.no.NAs[is.na(prop.dead$prop.dead.no.NAs)] <- 0
	
write.csv(prop.dead, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SDM.HighMedLow.2001to",surveyyear,".Clapper.Proportion.csv"), row.names = FALSE)

#Remove 2020 values

#For plots - removing 2020 values
prop.dead <- prop.dead %>% 
  mutate(Live.no = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ Live.no)) %>%
  mutate(Dead.no = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ Dead.no)) %>%
  mutate(prop.dead = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ prop.dead)) %>%
  mutate(prop.dead.no.NAs = case_when(YEAR == 2020 ~ NA_real_, TRUE ~ prop.dead.no.NAs)) %>% 
  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
                             SUBAREA == "SFA29B" ~ "Subarea B",
                             SUBAREA == "SFA29C" ~ "Subarea C",
                             SUBAREA == "SFA29D" ~ "Subarea D"))

#Commercial Size 	
	XX <- prop.dead %>% filter(size=="comm") %>% 
	  filter(SUBAREA != "Subarea A" | Strata != "high") #Removing the "high" habitat category from subarea A.

clap.prop.comm <- ggplot(data=XX, aes(x=YEAR, y=prop.dead.no.NAs, col= Strata, shape = Strata)) + geom_point()  + geom_line(aes(linetype = Strata)) + 
	  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  facet_wrap(~SUBAREA) + theme_bw() + 
  theme(legend.position = c(0.85, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank(), text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12)) + 
	  ylab("Commercial (>=100mm) Clappers (proportion)") + 
	  xlab("Year")  + 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))
clap.prop.comm
	
#save
	ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Clappers.Prop.Commercial.",surveyyear,".png"), plot = clap.prop.comm, scale = 2.5, width =8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
	
#dev.off()

		
#Recruit Size 	
	XX <- prop.dead %>% filter(size=="rec")
	
#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Clappers.Prop.Recruit.",surveyyear,".png"),width=8,height=8,units = "in",res=300)
	
clap.prop.rec <- ggplot(data=XX, aes(x=YEAR, y=prop.dead.no.NAs, col= Strata, shape = Strata)) + geom_point()  + geom_line(aes(linetype = Strata)) + 
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  facet_wrap(~SUBAREA) + theme_bw() + 
  theme(legend.position = c(0.85, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank()) +
	  ylab("Recruit (90-99mm) Clappers (proportion)") + 
	  xlab("Year") + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026)) #+ 
clap.prop.rec

#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Clappers.Prop.Recruit.",surveyyear,".png"), plot = clap.prop.rec, scale = 2.5, width =6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
#dev.off()
	
#Pre-recruit Size 	
XX <- prop.dead %>% filter(size=="prerec")
	
	#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Clappers.Prop.Prerecruit.",surveyyear,".png"),width=8,height=8,units = "in",res=300)
	
clap.prop.prerec <- ggplot(data=XX, aes(x=YEAR, y=prop.dead.no.NAs, col= Strata, shape = Strata)) + geom_point()  + geom_line(aes(linetype = Strata)) + 
	  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  facet_wrap(~SUBAREA) + theme_bw() +
  theme(legend.position = c(0.85, 0.85),panel.grid.minor = element_blank(),legend.title = element_blank()) + 
	  ylab("Pre-recruit (<90mm) Clappers (proportion)") + 
	  xlab("Year") + 
  scale_x_continuous(breaks = seq(2001,2026,by=4), limits = c(2001,2026)) #+ 
clap.prop.prerec
	#save
	ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29AtoD.Clappers.Prop.Prerecruit.",surveyyear,".png"), plot = clap.prop.prerec, scale = 2.5, width =6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
	
#dev.off()
	
# ---- Proportion Dead by Tow ----- 
#remember, at start saved dead scallop worked up  by tow in data.obj.all -- this is all tows, not just tow type 1 but that's fine since this file of prop dead by tow is for spatial plots and want all information we can use 
data.obj.all

#Get live numbers per tow 
quer3 <- paste(
  "SELECT *                                     ",
  "FROM scallsur.SCLIVERES                      ",
  "WHERE strata_id IN (41, 42, 43, 44, 45)      ",
  "AND (cruise in ('",cruise.list,"'))          ",
  sep=""
)

# Run for ROracle
data.live <- dbGetQuery(chan, quer3)

# Calc unique id
data.live$uid <- paste(data.live$CRUISE, data.live$TOW_NO, sep=".")

#Pre-recruits per tow 0 to 89 mm SH;   note for Prerecruits for 2013 assessment - just used 20-60mm#
data.live$prerec <-  data.live %>% dplyr::select(grep("BIN_ID_0",names(data.live)):grep("BIN_ID_85",names(data.live))) %>%  rowSums(na.rm=TRUE)

#recruits per tow 90 to 99 mm SH 
data.live$rec <- data.live %>% dplyr::select(grep("BIN_ID_90",names(data.live)):grep("BIN_ID_95",names(data.live))) %>%  rowSums(na.rm=TRUE)

#commercial	per tow >= 100 mm SH 
data.live$comm <- data.live %>% dplyr::select(grep("BIN_ID_100",names(data.live)):grep("BIN_ID_195",names(data.live))) %>%  rowSums(na.rm=TRUE)

dim(data.obj.all)
dim(data.live)

# Left join survey tows to surficial substrate tows on uid
data.join <- merge(data.obj.all %>% dplyr::select(uid, CRUISE,TOW_NO,START_LAT, START_LONG,STRATA,YEAR, dead.prerec=prerec, dead.rec=rec, dead.comm= comm), data.live %>% dplyr::select(uid,live.prerec=prerec, live.rec=rec, live.comm= comm ), by.x='uid', by.y='uid', all.x=TRUE)
dim(data.join)
head(data.join)

#proportion dead per size where prop dead is dead/(dead+live)
data.join$prop.dead.comm <- data.join$dead.comm/(data.join$dead.comm + data.join$live.comm)
data.join$prop.dead.rec <- data.join$dead.rec/(data.join$dead.rec + data.join$live.rec)
data.join$prop.dead.prerec <- data.join$dead.prerec/(data.join$dead.prerec + data.join$live.prerec)

#if NA make 0 
data.join$prop.dead.comm[is.na(data.join$prop.dead.comm)] <- 0
data.join$prop.dead.rec[is.na(data.join$prop.dead.rec)] <- 0
data.join$prop.dead.prerec[is.na(data.join$prop.dead.prerec)] <- 0




#write out data and use this file to spatially plot proportion dead in the spatial plot script 
write.csv(data.join, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SFA29.all.tows.2001to",surveyyear,".Proportion.Dead.csv"), row.names = FALSE)

### END ###

##
# --- Stratified estimates fo CLAPPERS - NOT REQUIRED/USED by assessement but interesting ---- 
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

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.clappers.",surveyyear,".png"),width=11,height=11,units = "in",res=300)
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

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.prerecuit.clapper.numbers.",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)

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

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.recuit.clapper.numbers.",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)

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

ggsave(paste0(path.directory,assessmentyear,"/Assessment/Figures/SFA29.AtoD.stratified.commercial.clapper.numbers.",surveyyear,".png"),width=9,height=8,units = "in",dpi=300)