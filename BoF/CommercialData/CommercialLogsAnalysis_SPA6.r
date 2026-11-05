###.....................................###
###  Commercial Catch Rates and Plots   ###
###             SPA6                    ###
###       J. Raper June 2020            ###
###.....................................###
 
#Summarizes commercial log data from SPA6 for a given year and updates the following files:
#CPUE_spa6_combined_yyyy.csv
#CPUE_spa6_fleet_yyyy.csv
#CPUE_spa6_subarea_yyyy.csv

#Assigns VMS strata to logs and calculates proportion of catch in each strata

#Creates the following plots:
# SPA6_CPUEyyyy.png
# SPA6_CPUEandEffort_byfleetyyyy.png
# SPA6_CPUEandEffort_combinedyyyy.png
# SPA6_TACandLandingsyyyy.png
# SPA6_RefPtsyyyy.png
# SPA6_CatchandEffortyyyy.png
# SPA6_CPUEgridplotyyyy.png
# SPA6_Effortgridplotyyyy.png
# SPA6_Catchgridplotyyyy.png
# SPA6_CPEUbyMonthyyyy.png
# SPA6_LandingsbyMonthyyyy.png


#BEFORE RUNNING THIS SCRIPT:

# compare SCALLOP db landings with area cap monitoring report and address any issues (differences > 2 mt for each fleet/area should be investigated)
# perform spatial checks of SPA6 logs: plot and check logs where coordinates fall on land or areas not normally fished, correct positions in SCALLOP db if necessary
# update SPA6_TACandLandings_YYYY.xlsx with TAC and landings from area cap monitoring report and save in the current assessment folder: Y:\Inshore\BoF\YYYY\Assessment\Data\CommercialData
# ensure date format in your SQL Developer profile is set to YYYY-MM-DD
	
library(ROracle)
library (PBSmapping)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(lubridate)


#### DEFINE ####
	
	direct <- "Y:/Inshore/Assessment/BoF"
	fishingyear <- 2026 #most recent year of commercial fishing data to be used (e.g. if fishing season is 2019/2020, use 2020)
	assessmentyear <- 2026 #year in which you are conducting the assessment
	un.ID=Sys.getenv("un.raperj") #ptran username
	pwd.ID=Sys.getenv("pw.raperj") #ptran password
#	un.ID=un.sameotoj #ptran username
#	pwd.ID=pw.sameotoj#ptran password
	
#Date range for logs to be selected 
	start.date.logs <- "2025-10-01"  #YYYY-MM-DD use Oct 1 
	ends.date.logs <- "2026-10-01"  #YYYY-MM-DD use Oct 1 
	
#### Read files ####
	
	# TAC and Landings time series flat files
	tacq<-read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA6_TACandLandings_", 
	                       fishingyear, ".xlsx"), sheet = "TACandLandings")
	
	#CPUE_spa6 files from previous year
	CPUE_spa6_subarea <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_spa6_subarea_", (fishingyear-1), ".csv")) 	
	CPUE_spa6_fleet <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_spa6_fleet_", (fishingyear-1), ".csv")) 	
	CPUE_spa6_combined <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_spa6_combined_", (fishingyear-1), ".csv")) 	

	#polygons for assigning VMS strata to data
	spa6IN<-read.csv("Y:/Inshore/Databases/Scallsur/SPA6_SurveyStrata/2015/SPA6_VMS_IN_R_final.csv")
	spa6OUT<-read.csv("Y:/Inshore/Databases/Scallsur/SPA6_SurveyStrata/2015/SPA6_VMS_OUT_R_final.csv")
	attr(spa6IN,"projection") <- "LL"
	attr(spa6OUT,"projection") <- "LL"
	
	 #polygons for spatial plots
	 poly.sf <- st_read("Y:/Inshore/Databases/Scallsur/SPA6_SurveyStrata/SPA6_all", layer = "SPA6_wgs84")
	 poly.VMSIN <- st_read("Y:/Inshore/Databases/Scallsur/SPA6_SurveyStrata/2015", layer = "SPA6_VMSStrata_IN_2015")
	 poly.6A <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA6A_polygon_NAD83")
	 poly.6B <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA6B_polygon_NAD83")
	 poly.6C <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA6C_polygon_NAD83")
	 poly.6D <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA6D_polygon_NAD83")
	 Land <- st_read("Y:/GISdata/Private/AtlanticUSCdnCoast", layer = "MAR_Maine")


	 #### Import Mar-scal functions 
	
	 funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",
	            "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
	            "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
	            "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R")
	dir <- getwd()
	for(fun in funcs)
	{
	  temp <- dir
	  download.file(fun,destfile = basename(fun))
	  source(paste0(dir,"/",basename(fun)))
	  file.remove(paste0(dir,"/",basename(fun)))
	}
	
	
#### Select data ####

	quer <- paste(
		"SELECT * 			                             ",
		"FROM scallop.scallop_log_marfis s		         ",
		"WHERE s.assigned_area in ('6A','6B','6C','6D')       ",
		" 	AND  s.date_fished >= to_date('",start.date.logs,"','YYYY-MM-DD') and s.date_fished < to_date('",ends.date.logs,"','YYYY-MM-DD') ",
		"AND s.licence_id not in ('356089', '356090', '356091', '356092', '368730', '369132', '369133') ", #remove FSC
		"	AND (s.data_class = 1                        ",
		"OR (s.data_class = 2 AND s.quality_flag =',4' ) ",
		"OR (s.data_class = 2 AND s.quality_flag =',1,4') ",
		"OR (s.data_class = 2 AND s.quality_flag =',2,4')) ",
		sep=""
		)
	
	chan <- dbConnect(drv = dbDriver("Oracle"), username=un.ID,  password = pwd.ID,  dbname = "ptran", believeNRows=FALSE)
	
	logs <- dbGetQuery(chan, quer)
	dim(logs)
	
	#Convert coordinates:
	logs$lat <- convert.dd.dddd(logs$LATITUDE/100)
	logs$lon <- (-1)*convert.dd.dddd(logs$LONGITUDE/100)
	
	#Add year and month columns:
	logs$DATE_FISHED <- as.Date(logs$DATE_FISHED, format="%Y-%m-%d")  
	logs$YEAR <- year(logs$DATE_FISHED)
	logs$month <- month(logs$DATE_FISHED)
	head(logs)
	unique(logs$month)
	str(logs)

	#Check data
	table(logs$YEAR) #check years, fishery for SPA 6 should be just for a SINGLE year since starts in Jan
	table(logs$FLEET) #check fleets, SPA6 is fished by Full Bay and Mid-Bay
	table(logs$ASSIGNED_AREA, logs$FLEET) #check number of records for rule of <5
	
	#Check cpue by month
	logs %>%
	  group_by(month) %>%
	  summarise(mean(CPUE_KG))
	
	#Check landings by month
	logs %>%
	  group_by(month) %>%
	  summarise(sum(DAY_CATCH_KG/1000))
	
	#If any issues here, STOP and address with log scan and/or VMS checks. Make any corrections in the SCALLOP db (and send to CDD), then restart this script
	
	
	#In the 2021 season after verifying with log scans, there were still CPUE outliers up to 317 kg/h. Removing records with CPUE > 200
	logs <- (logs[which(logs$CPUE <= 200),]) 
	dim(logs)
	
	#Change year in logs to reflect fishing year instead of calendar year (in case there was fishing outside designated season dates)
	logs$YEAR <- as.numeric(fishingyear) 

	#Add ID row
  logs$ID<-1:nrow(logs)

  
#### Calculate CPUE by fleet and subarea ####
  
  logs$effort_h <- (logs$AVG_TOW_TIME*logs$NUM_OF_TOWS)/60
  
  #In the 2025 season after verifying with log scans, there were still effort outliers >24hrs/day. Removing records with effort > 24
  logs <- (logs[which(logs$effort_h <= 24),])
  dim(logs)
  
  effort.dat.subarea <- aggregate(logs$effort_h, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=sum)
  names(effort.dat.subarea) <- c("fleet","area","effort.hr")
  
  catch.dat.subarea <-  aggregate(logs$DAY_CATCH_KG, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=sum)
  names(catch.dat.subarea) <- c("fleet", "area", "catch.kg")
  
  nlogs.subarea <- aggregate(logs$LOG_SEQ, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=length)
  names(nlogs.subarea) <- c("fleet", "area", "n")
  
  cpue.subarea <- merge(merge(effort.dat.subarea, catch.dat.subarea, by.x=c("fleet","area"), by.y=c("fleet","area")), 
                        nlogs.subarea, by.x=c("fleet","area"), by.y=c("fleet","area"))
  
  #CPUE is reported as long as rule of 5 is met for Privacy Act considerations, otherwise returns NA.
  cpue.subarea$cpue.kgh <- ifelse(cpue.subarea$n > 5, cpue.subarea$catch.kg/cpue.subarea$effort.hr, NA)
  
  #cpue.subarea$cpue.kgh <- cpue.subarea$catch.kg/cpue.subarea$effort.hr #to look at records without Privacy removals
  
#### Update CPUE_spa6_subarea csv file ####
  
  #Convert catch and effort units
  cpue.subarea$catch.dat.mt <- cpue.subarea$catch.kg/1000
  cpue.subarea$effort.dat.1000h <- cpue.subarea$effort.hr/1000
  
  #Add year
  cpue.subarea$year <- fishingyear
  print(cpue.subarea)
  
  #Append current year data to CPUE_spa6
  comm.dat.subarea <- rbind(CPUE_spa6_subarea, cpue.subarea[,c("year", "area", "fleet", "effort.dat.1000h", "catch.dat.mt", "cpue.kgh")])
  
  #Save to current assessment folder
  write.csv(comm.dat.subarea, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_spa6_subarea_", fishingyear, ".csv"), row.names = FALSE)
  
  
#### Calculate fleet effort ####

  #CPUE by fleet:
  
  effort.dat.fleet <- aggregate(logs$effort_h, by=list(logs$FLEET), FUN=sum)
  names(effort.dat.fleet) <- c("fleet","effort.dat.hr")
  
  catch.dat.fleet <-  aggregate(logs$DAY_CATCH_KG, by=list(logs$FLEET), FUN=sum)
  names(catch.dat.fleet) <- c("fleet", "catch.dat.kg")
  
  nlogs.fleet <- aggregate(logs$LOG_SEQ, by=list(logs$FLEET), FUN=length)
  names(nlogs.fleet) <- c("fleet", "n")
  
  cpue.fleet <- merge(merge(effort.dat.fleet, catch.dat.fleet, by.x="fleet", by.y="fleet"),
                       nlogs.fleet, by.x="fleet", by.y="fleet")
  
  #CPUE is reported as long as rule of 5 is met for Privacy Act considerations, otherwise returns NA:
  cpue.fleet$cpue.kgh <- ifelse(cpue.fleet$n > 5, cpue.fleet$catch.dat.kg/cpue.fleet$effort.dat.hr, NA)
  
  #Landings by fleet:
  landings <- as.data.frame(t(tacq[c(1,2,3,5),-1]))
  names(landings) <- c("FB","MB","FSC", "TAC")
  landings$year <- as.numeric(rownames(landings))
  
  #Convert landings data to long format
  landings <- reshape2::melt(landings, id.vars = "year", value.name = "catch.fleet.mt")
  
  #Combine landings with cpue
  catch.fishery.fleet <- landings[which(landings$year == fishingyear),]
  catch.fishery.fleet <- catch.fishery.fleet[-c(3,4),]
  cpue.fleet <- cbind(cpue.fleet, catch.fishery.fleet)
  print(cpue.fleet)
  
  #Calculate fishery-level (fleet) effort:
  cpue.fleet$effort.fleet.1000h <- cpue.fleet$catch.fleet.mt/cpue.fleet$cpue.kgh
  
    
#### Update CPUE_spa6_fleet csv file  ####

  #Append current year data to CPUE_spa6_fleet
  comm.dat.fleet <- rbind(CPUE_spa6_fleet, cpue.fleet[,c("fleet", "year", "effort.dat.hr", "catch.dat.kg", "cpue.kgh", "catch.fleet.mt", "effort.fleet.1000h")])
  
  #Save to current assessment folder
  write.csv(comm.dat.fleet, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_spa6_fleet_", fishingyear, ".csv"), row.names = FALSE)    

  
#### Calculate combined fleet effort  ####
  
  effort.dat.combined <- sum(logs$effort_h)
  catch.dat.combined <-  sum(logs$DAY_CATCH_KG)
  
  cpue.combined <- data.frame(area = "SPA6", year = fishingyear, effort.dat.hr = effort.dat.combined, 
                         catch.dat.kg = catch.dat.combined, cpue.kgh = catch.dat.combined/effort.dat.combined)
  
  #Combine landings with cpue
  catch.fishery.combined <- landings %>%
    filter(year == fishingyear & variable != 'TAC') %>%
    summarize(catch.fleet.mt = sum(catch.fleet.mt))
  
  cpue.combined <- cbind(cpue.combined, catch.fishery.combined)
    
  #Calculate fishery-level (fleet) effort:
  cpue.combined$effort.fleet.1000h <- cpue.combined$catch.fleet.mt/cpue.combined$cpue.kgh 
  print(cpue.combined)
  
  
#### Update CPUE_spa6_combined csv file ####
  
  #Append current year data to CPUE_spa6_combined
  comm.dat.combined <- rbind(CPUE_spa6_combined, cpue.combined)
  
  #Save to current assessment folder
  write.csv(comm.dat.combined, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_spa6_combined_", fishingyear, ".csv"), row.names = FALSE)    
  
  
#### Assign VMS strata and determine proportion of catch in each ####

  events<- subset(logs,select=c("ID","lon","lat"))
  names(events)<-c("EID","X","Y")
  attr(events,"projection") <- "LL"
  logs$VMSSTRATA[logs$ID%in%findPolys(events,spa6IN)$EID]<-'IN'
  logs$VMSSTRATA[logs$ID%in%findPolys(events,spa6OUT)$EID]<-'OUT'
  unique(logs$VMSSTRATA)
  table(logs$VMSSTRATA, useNA = "always")
  
  #Plot points and check strata allocations   
  eventsIN <- logs %>%
    dplyr::select(ID, lon, lat, VMSSTRATA) %>%
    filter(VMSSTRATA == 'IN') %>%
    rename(EID = ID, X = lon, Y = lat)
  attr(eventsIN,"projection") <- "LL"
  
  eventsOUT <- logs %>%
    dplyr::select(ID, lon, lat, VMSSTRATA) %>%
    filter(VMSSTRATA == 'OUT') %>%
    rename(EID = ID, X = lon, Y = lat)
  attr(eventsOUT,"projection") <- "LL"
  
  eventsNA <- logs %>%
    dplyr::select(ID, lon, lat, VMSSTRATA) %>%
    filter(is.na(VMSSTRATA)) %>%
    rename(EID = ID, X = lon, Y = lat)
  attr(eventsNA,"projection") <- "LL"                  
  
  plotPolys(spa6IN, col = "pink")
  addPolys(spa6OUT, col = "lightblue")
  addPoints(eventsIN, col = "red")
  addPoints(eventsOUT, col = "blue")
  addPoints(eventsNA, col = "black")
  
  table(logs$VMSSTRATA, useNA = "always")
  
  #Calculate catch from usable logs in each strata:  
  VMSstrat <- logs %>%
    group_by(VMSSTRATA) %>%
    summarize(Catch = sum(DAY_CATCH_KG)/1000)
  
  #Proportion of catch from each strata:
  VMSstrat$CatchProp <- VMSstrat$Catch/sum(logs$DAY_CATCH_KG/1000)
  
  #Total combined landings
  landings.tot <- catch.fishery.combined[1,1]
  
  #Adjusted catch (fishery-level) from each strata:
   VMSstrat<- VMSstrat %>%
    mutate(CatchAdjusted = CatchProp*landings.tot)
  print(VMSstrat)
  
  #Use these values to update Catch_IN, Catch_OUT, and Catch_NA fields of SPA6TACandLandings file#


#### Time Series Plots ####
  
#CPUE by fleet and subarea:
  
  #remove cpue prior to 2004 for Full Bay with too few records to show catch rate within subarea:
  #6B: 2002, 2003	(all others already NA in file)
  comm.dat.subarea$cpue.kgh[which(comm.dat.subarea$fleet=='Full Bay'&comm.dat.subarea$area=='6B'&comm.dat.subarea$year%in%c(2002,2003))] <- NA
  
  ggplot(filter(comm.dat.subarea), aes(x = year, y = cpue.kgh)) +
    theme_bw(base_size = 16)  +  
    geom_point(aes(colour = fleet)) +
    geom_line(aes(linetype = fleet, colour = fleet)) +
    scale_x_continuous("Year", breaks = seq(1993,fishingyear,5)) +
    scale_y_continuous("Catch Rate (kg/h)", limits = c(0,60), breaks = seq(0,60,10)) +
    scale_colour_manual(values = c("black", "red")) +
    facet_wrap(~area) +
    theme(legend.title = element_blank()) + theme(legend.position.inside=c(0.6, 0.92)) # play with the location if you want it inside the plotting panel
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_CPUE",fishingyear, ".png"), width = 24, height = 24, dpi = 400,units='cm')
     
#CPUE by month
  CPUE_month <- logs %>% 
    group_by(month) %>%
    summarise(sum(DAY_CATCH_KG)/sum(effort_h)) %>%
    slice(1:7) %>% #reorder months, this may change depending on months fished in a given year
    mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order
  
  ggplot(CPUE_month, aes(x = month, y = `sum(DAY_CATCH_KG)/sum(effort_h)`, group =1)) +
    theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    geom_point() +
    geom_line() +
    scale_x_discrete("Month") +
    scale_y_continuous("Catch Rate (kg/h)", limits = c(0,40), breaks = seq(0,60,10))
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_CPUEbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')
  
#Landings by month (from clean logs)
  Landings_month <- logs %>% 
    group_by(month) %>%
    summarise(sum(DAY_CATCH_KG/1000)) %>%
    slice(1:7) %>% #reorder months, this may change depending on months fished in a given year
    mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order
  
  ggplot(Landings_month, aes(x = month, y = `sum(DAY_CATCH_KG/1000)`, group =1)) +
    theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    geom_point() +
    geom_line() +
    scale_x_discrete("Month") +
    scale_y_continuous("Landings (t)")
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_LandingsbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')
  

  #CPUE and Effort by fleet (fishery-level)
  ggplot(comm.dat.fleet) +
    theme_bw(base_size = 16) + theme(panel.grid=element_blank()) +  
    theme(axis.line.y.right = element_line(color = "red"), axis.ticks.y.right = element_line(color = "red"), axis.text.y.right = element_text(color = "red")) +
    geom_point(aes(x = year, y = cpue.kgh)) + 
    geom_line(aes(x = year, y = cpue.kgh)) + 
    geom_point(aes(x = year, y = effort.fleet.1000h*2), color = "red") + 
    geom_line(aes(x = year, y = effort.fleet.1000h*2), color = "red", linetype = "dashed") +
    scale_y_continuous("CPUE (kg/h)", sec.axis = sec_axis(~./2, name = "Effort (1000h)", breaks = seq(0,20,5))) + # 
    scale_x_continuous("Year", breaks=seq(2002,fishingyear,2)) +
    facet_wrap(~fleet, ncol = 1)
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_CPUEandEffort_byfleet",fishingyear, ".png"), width = 24, height = 20, dpi = 400,units='cm')

  #CPUE and Effort combined (fishery-level)
  ggplot(comm.dat.combined) +
    theme_bw(base_size = 16) + theme(panel.grid=element_blank()) +  
    theme(axis.line.y.right = element_line(color = "red"), axis.ticks.y.right = element_line(color = "red"), axis.text.y.right = element_text(color = "red")) +
    geom_point(aes(x = year, y = cpue.kgh)) + 
    geom_line(aes(x = year, y = cpue.kgh)) + 
    geom_point(aes(x = year, y = effort.fleet.1000h), color = "red") + 
    geom_line(aes(x = year, y = effort.fleet.1000h), color = "red", linetype = "dashed") +
    scale_y_continuous("CPUE (kg/h)", sec.axis = sec_axis(~./1, name = "Effort (1000h)")) +  
    scale_x_continuous("Year", breaks=seq(2002,fishingyear,2))
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_CPUEandEffort_combined",fishingyear, ".png"), width = 24, height = 20, dpi = 400,units='cm')

  #TAC and Landings
  ggplot() +
    theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + # white background, no gridlines
    geom_bar(data=landings[landings$variable%in%c('FSC','FB','MB'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c('FSC', 'FB','MB'))), colour="black", stat="identity") + 
    geom_line(data=landings[landings$variable == 'TAC',], aes(x = year, y = catch.fleet.mt), lwd = 1) +
    scale_y_continuous("Landings (meats, t)", breaks=seq(0,1200,200)) + # 
    scale_x_continuous("Year", breaks=seq(1976,fishingyear,4)) +
    scale_fill_manual(values=c("black","white", "grey"), labels=c("Food, Social, and Ceremonial","Full Bay", "Mid-Bay"), name=NULL) +
    annotate(geom="text",label="TAC", x=2003.4, y= 210) +
    theme(legend.position=c(0.75, 0.85)) # play with the location if you want it inside the plotting panel
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_TACandLandings",fishingyear, ".png"), width = 24, height = 20, dpi = 400,units='cm')
  
  #TAC and Landings (FR)
  ggplot() +
    theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + # white background, no gridlines
    geom_bar(data=landings[landings$variable%in%c('FSC','FB','MB'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c('FSC', 'FB','MB'))), colour="black", stat="identity") + 
    geom_line(data=landings[landings$variable == 'TAC',], aes(x = year, y = catch.fleet.mt), lwd = 1) +
    scale_y_continuous("Débarquements (tonnes de chairs)", breaks=seq(0,1200,200)) + # 
    scale_x_continuous("Année", breaks=seq(1976,fishingyear,4)) +
    scale_fill_manual(values=c("black","white", "grey"), labels=c("À des fins alimentaires, sociales et rituelles","Totalité de la baie", "Milieu de la baie"), name=NULL) +
    annotate(geom="text",label="TAC", x=2003.4, y= 210) +
    theme(legend.position=c(0.65, 0.85)) # play with the location if you want it inside the plotting panel
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_TACandLandings_FR",fishingyear, ".png"), width = 24, height = 20, dpi = 400,units='cm')
  
#CPUE Fleets and Subareas combined
  ggplot(comm.dat.combined) +
    theme_bw(base_size = 20) + theme(panel.grid=element_blank()) + 
    geom_point(aes(x = year, y = cpue.kgh)) +
    geom_line(aes(x = year, y = cpue.kgh)) +
    scale_y_continuous("Catch rate (kg/h)", limits=c(0,40), breaks=seq(0,40,5)) +
    scale_x_continuous("Year", breaks=seq(2002,fishingyear,2)) +
    geom_hline(aes(yintercept=median(cpue.kgh[1:(dim(comm.dat.combined)[1]-1)])), linetype = "dashed") + # median line is all years except current year
    geom_text(x = 2008, y = 16, label = "median", size = 4)
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_CPUE_combined",fishingyear, ".png"), width = 24, height = 20, dpi = 400,units='cm')
  
  
  
#### Catch and effort with linear model ####
  ggplot(logs, aes(x = effort_h, y = DAY_CATCH_KG)) +
    theme_bw() + theme(panel.grid=element_blank()) +
    geom_point() +
    geom_smooth(method='lm') +
    scale_y_continuous("Catch (meats, kg)") + 
    scale_x_continuous("Effort (h)")
    # + facet_wrap(~FLEET, scales = "free") #use if you want separate plots by fleet
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_CatchandEffort",fishingyear, ".png"), width = 24, height = 20, dpi = 400,units='cm')


#### SPATIAL PLOTS ####
  
  #Pecjector basemap with SPA6 boundaries
  p <- pecjector(area =list(x=c(-67.4,-65.8), y=c(44.2,45.2), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(bathy = c(50,'c'), scale.bar = c('bl',0.5)))
  
  
#CPUE Grid Plot
  
  #Create raster of mean cpue
  for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','CPUE_KG'))
  coordinates(for.raster) <- c("lon", "lat")
  
  r <- raster(ext = extent(poly.sf), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
  
  raster.data <- rasterize(for.raster, r, "CPUE_KG", fun = mean)
  
  ##convert raster to sf object
  df <- (as.data.frame(raster::rasterToPoints(raster.data)))
  names(df) <- c("lon", "lat", "mean.cpue")
  
  ##Fix to allow coord_sf(expand = F) to work correctly
  sf::sf_use_s2(FALSE)
  
  ##add sf objects to basemap outside of pecjector
    p +
    geom_tile(df, mapping = aes(lon, lat, fill = mean.cpue), color = "grey55") +
    geom_sf(data = poly.6A, fill=NA, colour="grey55") +
    geom_sf(data = poly.6B, fill=NA, colour="grey55") +
    geom_sf(data = poly.6C, fill=NA, colour="grey55") +
    geom_sf(data = poly.6D, fill=NA, colour="grey55") +
    geom_sf(data = poly.VMSIN, fill=NA, colour="red") +
    scale_fill_binned(type = "viridis", direction = -1, name="CPUE (kg/h)", breaks = c(10, 20, 30, 40)) +
    geom_sf(data = Land, fill="grey55", colour = "grey55") +
    theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"), 
          legend.text = element_text(size = 10),
          legend.position = c(.8,.6), 
          legend.box.background = element_rect(colour = "grey55", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
          legend.box.margin = margin(6, 8, 6, 8)) +
      coord_sf(xlim = c(-67.4,-65.8), ylim = c(44.2,45.2), expand = F)
 #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_CPUEgridplot_new",fishingyear, ".png"), width = 9, height = 9, dpi = 200,units='in')
  
  
  # # Make histogram by area for distribution of cpue
  # histolog<-subset(logs, YEAR == fishingyear, c('ASSIGNED_AREA','CPUE_KG'))
  # 
  # cpuehisto <- ggplot (histolog, aes (x = CPUE_KG)) + 
  #   geom_histogram (breaks = seq (0, 200, by = 5), col = 'grey60') + labs (x = 'CPUE (kg/h)', y = 'Count') +
  #   facet_wrap (~ASSIGNED_AREA, ncol = 1) +
  #   scale_x_continuous(breaks = seq (0, 200, by = 50)) +
  #   theme_bw() +
  #   theme(panel.grid=element_blank()) 
  # cpuehisto
  # 
  # tapply(histolog$CPUE_KG, histolog$ASSIGNED_AREA, summary)
  
  
  #Catch Grid Plot
  
  #Create raster of cumulative catch
  for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','DAY_CATCH_KG'))
  coordinates(for.raster) <- c("lon", "lat")
  
  r <- raster(ext = extent(poly.sf), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
  
  raster.data <- rasterize(for.raster, r, "DAY_CATCH_KG", fun = sum)
  
  ##convert raster to sf object
  df <- (as.data.frame(raster::rasterToPoints(raster.data)))
  names(df) <- c("lon", "lat", "tot.catch")
  
  ##add sf objects to basemap outside of pecjector
    p +
    geom_tile(df, mapping = aes(lon, lat, fill = tot.catch), color = "grey55") +
    geom_sf(data = poly.6A, fill=NA, colour="grey55") +
    geom_sf(data = poly.6B, fill=NA, colour="grey55") +
    geom_sf(data = poly.6C, fill=NA, colour="grey55") +
    geom_sf(data = poly.6D, fill=NA, colour="grey55") +
    geom_sf(data = poly.VMSIN, fill=NA, colour="red") +
    scale_fill_binned(type = "viridis", direction = -1, name="Catch (kg)") +
    geom_sf(data = Land, fill="grey55", colour = "grey55") +
    theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"), 
          legend.text = element_text(size = 10),
          legend.position = c(.8,.6), 
          legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
          legend.box.margin = margin(6, 8, 6, 8)) +
      coord_sf(xlim = c(-67.4,-65.8), ylim = c(44.2,45.2), expand = FALSE)
  #save
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_Catchgridplot",fishingyear, ".png"), width = 9, height = 9, dpi = 200,units='in')
 
  
  #Effort Grid Plot
  
  #Create raster of cumulative effort
  for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','effort_h'))
  coordinates(for.raster) <- c("lon", "lat")
  
  r <- raster(ext = extent(poly.sf), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
  
  raster.data <- rasterize(for.raster, r, "effort_h", fun = sum)
  
  ##convert raster to sf object
  df <- (as.data.frame(raster::rasterToPoints(raster.data)))
  names(df) <- c("lon", "lat", "tot.effort")
  
  ##add sf objects to basemap outside of pecjector
  p +
    geom_tile(df, mapping = aes(lon, lat, fill = tot.effort), color = "grey55") +
    geom_sf(data = poly.6A, fill=NA, colour="grey55") +
    geom_sf(data = poly.6B, fill=NA, colour="grey55") +
    geom_sf(data = poly.6C, fill=NA, colour="grey55") +
    geom_sf(data = poly.6D, fill=NA, colour="grey55") +
    geom_sf(data = poly.VMSIN, fill=NA, colour="red") +
    scale_fill_binned(type = "viridis", direction = -1, name="Effort (h)") +
    geom_sf(data = Land, fill="grey55", colour = "grey55") +
    theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 10, face = "bold"), 
          legend.text = element_text(size = 10),
          legend.position = c(.8,.6), 
          legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
          legend.box.margin = margin(6, 8, 6, 8)) +
    coord_sf(xlim = c(-67.4,-65.8), ylim = c(44.2,45.2), expand = FALSE)
  #save  
  ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_Effortgridplot",fishingyear, ".png"), width = 9, height = 9, dpi = 200,units='in')
    
  
  
  
# #### French Language Plots ####
#   
#   #PA Reference Point PLOT
#     ggplot(comm.dat.combined) +
#     theme_bw(base_size = 20) + theme(panel.grid=element_blank()) + 
#     geom_point(aes(x = year, y = cpue.kgh)) +
#     geom_line(aes(x = year, y = cpue.kgh)) +
#     scale_y_continuous("Taux de prises (kg/h)", limits=c(0,30), breaks=seq(0,30,5)) +
#     scale_x_continuous("Ann?e", breaks=seq(2002,fishingyear,2)) +
#     geom_hline(yintercept = 6.2, linetype = "dashed") +
#     geom_hline(yintercept = 9.1, linetype = "dashed") +
#     annotate("rect", xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = 6.2, fill = "red", alpha = 0.3) +
#     annotate("rect", xmin=-Inf, xmax = Inf, ymin = 6.2, ymax = 9.1, fill = "yellow", alpha = 0.3) + 
#     annotate("rect", xmin=-Inf, xmax = Inf, ymin = 9.1, ymax = Inf, fill = "green", alpha = 0.3) +
#     annotate("text", label="Bonne Sant?", x=2018, y=11, size=7) +
#     annotate("text", label="Prudence", x=2018, y=7.8, size=7) +
#     annotate("text", label="Critique", x=2018, y=5, size=7)
#   #save
#   ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA6_RefPts",fishingyear, "_FR",".png"), width = 24, height = 20, dpi = 400,units='cm')
    

