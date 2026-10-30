###..........................###  
### Survey Bycatch estimates ###
###..........................###  
# updated Feb, 2015
# update Jan 2021 - rehauled J.Sameoto 
#Note time series figures not finished yet in ggplot - BW to do 

options(stringsAsFactors=FALSE)
library(PEDstrata) 
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ROracle)

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
# surficial data use for 2005, 2006, 2007:
SFA292005to2007sediment <- surf.all[surf.all$CRUISE%in%c('SFA292005','SFA292006','SFA292007'),]
SFA292005to2007sediment  <- SFA292005to2007sediment [,c("uid","surf")]
names(SFA292005to2007sediment ) <- c("uid","Start.Bottom")

#DEFINE:
path.directory <- "Y:/Inshore/Assessment/SFA29/"
assessmentyear <- 2027 #year in which you are conducting the assessment 
surveyyear <- 2026  #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#Bring in survey tow data with SDM value (note - SFA29_SDM_LWM.R script must be run to get updated survey tows with SDM values prior to runnint this script)
sdmtows <- read.csv("Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv")
table(sdmtows$CRUISE)
sdmtows$uid <- paste(sdmtows$CRUISE, sdmtows$TOW_NO, sep=".")
sdmtows <- sdmtows[,c("uid","SDM")]


# Define survey data list (Year/Cruises) UPDATE TO INCLUDE LATEST YEAR
cruise.list <- paste0("SFA29",(2001:surveyyear))
cruise.list <- paste(cruise.list,collapse="','")


###.............###
### Import data ###
###.............###
# Note: data standardized to 800m tow length and 17.5 feet width (17.5 feet width consistent with scallop proration) #
# ROracle; note this can take ~ 10 sec or so, don't panic
 chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')
#SQL 
	quer2 <- paste(
		"SELECT *                                                                                                                               ",
		"FROM (                                                                                                                                 ",
		"SELECT t.CRUISE, t.TOW_NO, to_char(tow_date,'yyyy') AS YEAR, t.strata_id,  a.common,  a.TotalCaught, a.StdTotalCaught, GEOPHYS_ID      ",
		"FROM (                                                                                                                                 ",
		"SELECT *                                                                                                                               ",
		"FROM (                                                                                                                                 ",
		"SELECT *                                                                                                                               ",
		"FROM scallsur.SCTOWS                                                                                                                   ",
		"WHERE (cruise in ('",cruise.list,"'))                 																			        ",
		")                                                                                                                                      ",
		"WHERE strata_id <> 46) t                                                                                                               ",
		"LEFT JOIN (                                                                                                                            ",
		"SELECT to_char(tow_date,'yyyy'), cruise, strata_id, tow_no, common, tow_len, count(*) AS TotalCaught, count(*)*(800/tow_len)*(17.5/18) ", 
		"AS StdTotalCaught   																													",
		"FROM (                                                                                                                                 ",
		"SELECT *                                                                                                                               ",
		"FROM (                                                                                                                                 ",
		"SELECT *                                                                                                                               ",
		"FROM scallsur.scbycatch_by_tow_raw                                                                                                     ",
		"WHERE (cruise in ('",cruise.list,"'))                      																		    ",
		")                                                                                                                                      ",
		"WHERE (speccd_id = 2550                                                                                                                ",
		"and strata_id <> 46)                                                                                                                   ",
		") GROUP BY to_char(tow_date,'yyyy'), cruise, strata_id, tow_no, common, tow_len                                                        ",
		") a                                                                                                                                    ",
		"ON (t.cruise = a.cruise                                                                                                                ",
		"AND t.tow_no = a.tow_no)                                                                                                               ",
		"order by cruise, tow_no                                                                                                                ",
		")                                                                                                                                      ",
		sep=""
	  )   
	
# Run for ROracle
	data.obj <- dbGetQuery(chan, quer2)
	

###
### ---- Lobster from Survey ANALYSIS ---- 
###

# Fill in NULL/NA in STDTOTALCAUGHT with 0 #
	data.obj$STDTOTALCAUGHT[is.na(data.obj$STDTOTALCAUGHT)] <- 0

# Assign subarea names 
	data.obj$STRATA <- NA
	data.obj$STRATA[data.obj$STRATA_ID==41] <- "SFA29A"
	data.obj$STRATA[data.obj$STRATA_ID==42] <- "SFA29B"
	data.obj$STRATA[data.obj$STRATA_ID==43] <- "SFA29C"
	data.obj$STRATA[data.obj$STRATA_ID==44] <- "SFA29D"
	data.obj$STRATA[data.obj$STRATA_ID==45] <- "SFA29E"
				
# Calc unique id 
	data.obj$uid <- paste(data.obj$CRUISE, data.obj$TOW_NO, sep=".")
	
# Left join survey tows to surficial substrate tows on uid 
	data.obj <- merge(data.obj, SFA292005to2007sediment, by.x='uid', by.y='uid', all.x=TRUE)
	
# Left join survey tows to SDM tows on uid 
	data.obj <- merge(data.obj, sdmtows, by.x='uid', by.y='uid', all.x=TRUE)
	dim(data.obj)
	
	
###
###  ----  Calculate Stratified Random Survey Estimates  ----                                   
###   PEDstrata(data.obj, strata.group, strata.name, catch, Subset)                            ###
###   Domain.estimates(data, Strata, Domain, strata.obj, domain.obj, Nd = NULL)                ###
###   post.stratify.all(data, OldStrata, pStrata, Strata.info, oldstrata.name, newstrata.name) ###
###

	strata.group <- SDMareas #SDM stratified estimates 
	
	#In 2024 did tows outside 29W and also in non-MBES covered part of B; remove these so apples and apples with previous estimates 
	data.obj %>% filter(CRUISE == "SFA292024" & is.na(SDM) == TRUE)
	dim(data.obj)
	
	data.obj <- data.obj[!(data.obj$CRUISE == "SFA292024" & is.na(data.obj$SDM) == TRUE),]
	dim(data.obj)
	
	data.obj.all <- data.obj
	
	#data.obj <- data.obj[data.obj$TOW_TYPE_ID==1,] #Need to add tow_type_id in bycatch view if want to use just tow type 1 ; for now are using all tows assuming they're all type 1 - good enough for what this analysis is it's used for 
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
	year <- c(2014:2019, 2021:surveyyear)
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
	
	#Add 2020 when had no survey
	out.strat.2014toYYYY <- rbind(out.strat.2014toYYYY, data.frame(YEAR = rep(2020, 4), SUBAREA = c("SFA29A","SFA29B","SFA29C","SFA29D"), yst=rep(NA,4), se.yst=rep(NA,4),  Yst=rep(NA,4), df.yst=rep(NA,4), alpha=rep(NA,4), effic.alloc=rep(NA,4), effic.str=rep(NA,4), var.ran=rep(NA,4), max.eff=rep(NA,4), descrip=rep(NA,4)))
	
	out.strat.2014toYYYY <- out.strat.2014toYYYY %>% arrange(SUBAREA,YEAR)
	out.strat.2014toYYYY
	
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

	#Add 2020 when had no survey
	scall.levels.2014toYYYY <- rbind(scall.levels.2014toYYYY, data.frame(YEAR = rep(2020, 12), SUBAREA = c(rep("SFA29A",3),rep("SFA29B",3),rep("SFA29C",3),rep("SFA29D",3)), Strata = rep(c("low","med","high"),4), yst=rep(NA,12), se.yst=rep(NA,12),  var.est=rep(NA,12), descrip=rep(NA,12)))
	
	scall.levels.2014toYYYY <- scall.levels.2014toYYYY %>% arrange(SUBAREA,YEAR, Strata)
	scall.levels.2014toYYYY
	
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
	
	names(scall.levels.2014toYYYY)[4] <- "Mean"	
	names(scall.levels.2014toYYYY)[5] <- "Std. Err."
	
	sdm.levels.est.all <- rbind(scall.levels.2001to2004[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2005to2007[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")],
				scall.levels.2008to2013[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")], scall.levels.2014toYYYY[,c("YEAR","SUBAREA","Strata","Mean","Std. Err.","var.est")])
	sdm.levels.est.all <- sdm.levels.est.all[order(sdm.levels.est.all$YEAR, sdm.levels.est.all$SUBAREA, sdm.levels.est.all$Strata), ]  
	sdm.levels.est.all$Strata <- as.factor(sdm.levels.est.all$Strata)

	#write out index 
	write.csv(sdm.levels.est.all, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/LOBSTER.SDM.HighMedLow.2001to",surveyyear,".csv"), row.names = FALSE)
	
	# Stratified estimates (one estimate for each subarea by year) 
	out.domain.surf$se.yst <- sqrt(out.domain.surf$var.yst)	
	out.domain.geophys$se.yst <- sqrt(out.domain.geophys$var.yst)	
	sdm.strat.est.all <- rbind(out.strat[,c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.surf[, c("YEAR","SUBAREA","yst","se.yst","descrip")], out.domain.geophys[, c("YEAR","SUBAREA","yst","se.yst","descrip")],out.strat.2014toYYYY[,c("YEAR","SUBAREA","yst","se.yst","descrip")] )	
	sdm.strat.est.all$rel.err <- sdm.strat.est.all$se.yst/sdm.strat.est.all$yst # relative error = standard error divided by the mean (is relative standard error - multiply by 10 to express as a percentage) 
	sdm.strat.est.all <- sdm.strat.est.all[with(sdm.strat.est.all,order(sdm.strat.est.all$YEAR, sdm.strat.est.all$SUBAREA)),]

	#write out index 
	write.csv(sdm.strat.est.all, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/LOBSTER.SDM.Stratified.2001to",surveyyear,".csv"), row.names = FALSE)
		

	
	
###... SUBAREA E ...###	 - NO DOMAIN DATAFRAME FOR E - for 2005 take simple mean #	
#some year were all exploratory tows - would need to get data from data.obj.all 
	E.area <- data.obj.all[data.obj.all$STRATA_ID==45,]
	E.years <- unique(E.area$YEAR)
	out.e <- data.frame(YEAR=E.years,SUBAREA='SFA29E',yst=rep(NA,length(E.years)),var.yst=rep(NA,length(E.years)),descrip=rep('simple mean',length(E.years))) 
	for (i in 1:length(E.years)) {
		temp.E <- E.area[E.area$YEAR==E.years[i],] 
		out.e[i,3] <- mean(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])
		out.e[i,4] <- var(temp.E$STDTOTALCAUGHT[temp.E$YEAR==E.years[i]])
	}
	out.e
	#add data for no survey in 2020 & not data in 2006 to 2011 
	out.e <- rbind(out.e, data.frame(YEAR = 2020, SUBAREA = "SFA29E", yst=NA, var.yst=NA, descrip=NA))  
	out.e <- rbind(out.e, data.frame(YEAR = (2006:2011), SUBAREA = rep("SFA29E",6), yst=rep(NA,6), var.yst=rep(NA,6), descrip=rep(NA,6)))  
	out.e <- out.e %>% arrange(SUBAREA,YEAR)
	out.e
	#write out 
	write.csv(out.e, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/LOBSTER.SubareaE.2001to",surveyyear,".csv"), row.names = FALSE)
	
	

	

###
### ---- Plot lobster survey bycatch per standard tow by subarea by year ----
###
# NOTE panel E estimates are for simple mean only!!! #
	
#	library(lattice)

#... plot ...#
	# Data by Strata #
	
	sdm.levels.est.all <- sdm.levels.est.all |> 
	  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
	                             SUBAREA == "SFA29B" ~ "Subarea B",
	                             SUBAREA == "SFA29C" ~ "Subarea C",
	                             SUBAREA == "SFA29D" ~ "Subarea D"))
	
	
	ggplot(data = sdm.levels.est.all, aes(x=YEAR, y=Mean, color=Strata, shape = Strata,group=Strata)) + 
	  geom_point() + geom_line(aes(linetype = Strata)) +
	  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_linetype_manual(values = c(1,2,3),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  scale_shape_manual(values = c(15:17),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
	  xlab("Year")+ylab("Mean no./tow")+
	  theme_bw() + 
	  theme(axis.title.x = element_text(vjust = -2))+
	  facet_wrap(~SUBAREA) + 
	  theme(legend.position = c(0.1, 0.85),panel.grid.minor = element_blank())
	
	#save
	ggsave(filename = paste0(path.directory,assessmentyear,'/Assessment/Figures/LobsterSurvey_NumPerTow_SDMlevels',surveyyear, '.png'), plot = last_plot(), scale = 2.5, width =11, height = 9, dpi = 300, units = "cm", limitsize = TRUE)	
	
	##### OLD PLOT CODE 
#		XX <- sdm.levels.est.all #DEFINE DATA TO BE PLOTTED 
	#	XX$SUBAREA <- ordered(XX$SUBAREA,c("SFA29C","SFA29D","SFA29A","SFA29B"))
	#	XX$Strata <- ordered(XX$Strata,c("high","med","low"))
	
	#	scall.plot <- xyplot(Mean ~ YEAR|SUBAREA, groups = Strata,  scales=list(tick.number=10),		
	#		data=XX, pch = c(3,2,1), col = c("green","red","black"), type="b", lty=c(3,2,1), # colors = 1 (black), 2 (red), 3 (green)
	#	ylim=c(-1,20),  
	#	as.table = FALSE,
	#	xlab="Year", ylab="Mean no./tow", main="",
	#	key= list(x=.30,y=.95,corner=c(1,1), transparent=TRUE, lines =list(lty=c(3,2,1),col=c("green","red","black"), 
	#		pch=c(3,2,1),type="b"),divide=1,cex=0.8, text=list(c("High","Medium","Low")))) 
	#scall.plot

	#png("figures/LobsterSurvey_NumPerTow_SDMlevels.png",11,9,res=400,units='in')
	#scall.plot
	#dev.off()
	
	#rename Subarea for figure.
	out.e <- out.e |>
	  mutate(SUBAREA = case_when(SUBAREA == "SFA29E" ~ "Subarea E"))
	
	sdm.strat.est.all <- sdm.strat.est.all |> 
	  mutate(SUBAREA = case_when(SUBAREA == "SFA29A" ~ "Subarea A",
	                             SUBAREA == "SFA29B" ~ "Subarea B",
	                             SUBAREA == "SFA29C" ~ "Subarea C",
	                             SUBAREA == "SFA29D" ~ "Subarea D"))
	
	# Stratified estimates plot #
	#this is plot used in survey summary presentation 
	lobster.str <-  rbind(sdm.strat.est.all %>% dplyr::select(YEAR, SUBAREA,yst,descrip), out.e %>% dplyr::select(YEAR, SUBAREA, yst,descrip))  
	lobster.str <- lobster.str %>% arrange(SUBAREA,YEAR)
	str(lobster.str)
	lobster.str$year <- as.numeric(lobster.str$YEAR)	
	
	
	ggplot(data = lobster.str, aes(x=year, y=yst, group = 1)) + #use group = 1 for lines to work.
		  geom_point() + 
		  geom_line() +
		  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2004, (surveyyear+1), by = 4)) +
		  theme_bw() +
	    theme(text = element_text(size=15), axis.title = element_text(size =15),axis.text = element_text(size = 12))+
		  facet_wrap(~SUBAREA) + 
		  ylab("Mean number of Lobster per standarized tow ") + 
		  xlab("Year") +
	    scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 6))
#save
ggsave(filename = paste0(path.directory,assessmentyear,'/Assessment/Figures/LobsterSurvey_NumPerTow_SDM',surveyyear, '.png'), plot = last_plot(), scale = 2.5, width =9, height = 6, dpi = 300, units = "cm", limitsize = TRUE)	
		
####	OLD PLOT CODE 
		
#	YY <- sdm.strat.est.all #DEFINE DATA TO BE PLOTTED 
#	YY <- rbind(YY[,c(1:3,5)],out.e[,c(1:3,5)])
#	dd <- data.frame(YEAR=seq(2006,2011,1),SUBAREA=rep("SFA29E",6),yst=rep(NA,6),descrip=rep(NA,6))
#	YY <- rbind(YY,dd)
#	YY <- YY[order(YY$YEAR, YY$SUBAREA),]
#	YY$SUBAREA <- ordered(YY$SUBAREA,c("SFA29C","SFA29D","SFA29E","SFA29A","SFA29B"))
	
	
#	scall.plot <- xyplot(yst ~ YEAR|SUBAREA, scales=list(tick.number=10),	
#		data=YY, pch = c(1), col = c(1), type="b", lty=c(1),
#		ylim=c(-1,16), 
#		as.table = FALSE,
#		xlab="Year", ylab="Mean no./tow" #,main="Lobster SDM Stratified Estimate",
		#key= list(x=.30,y=.95,corner=c(1,1), transparent=TRUE, lines =list(lty=c(3,2,1),col=c(3,2,1), 
			#pch=c(3,2,1),type="b"),divide=1,cex=0.8, text=list(c("","","")))) 
#	scall.plot

#	png("figures/LobsterSurvey_NumPerTow_SDM.png",11,9,res=400,units='in')
#	scall.plot
#	dev.off()
	
	