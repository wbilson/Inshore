###.....................................###
###  Commercial Catch Rates and Plots   ###
###              SPA3                   ###
###       J. Raper June 2020            ###
###.....................................###

#Summarizes commercial log data from SPA3 for a given year and updates CPUE_spa3, Effort_spa3_subarea, and EffortFleet_spa3 csv files. 
#Creates time series and spatial plots for catch, effort, and cpue


#Summarizes commercial log data from SPA1B for a given year and updates the following files:
# CPUE_1B_subarea_yyyy.csv
# CPUE_1B_combined_yyyy.csv

#Creates the following plots:
# SPA3_CPUEyyyy.png
# SPA3_CPUEandEffort_combinedyyyy.png
# SPA3_TACandLandingsyyyy.png
# SPA3_CatchandEffortyyyy.png
# SPA3_CPUEgridplotyyyy.png
# SPA3_Effortgridplotyyyy.png
# SPA3_Catchgridplotyyyy.png
# SPA1A_CPEUbyMonthyyyy.png
# SPA1A_LandingsbyMonthyyyy.png


#BEFORE RUNNING THIS SCRIPT:

# compare SCALLOP db landings with area cap monitoring report and address any issues (differences > 2 mt for each fleet/area should be investigated)
# update SPA3_TACandLandings_YYYY.xlsx with TAC and landings from monitoring report and save in the current assessment folder: Y:\INSHORE SCALLOP\BoF\YYYY\Assessment\Data\CommercialData
# ensure date format in your SQL Developer profile is set to YYYY-MM-DD

library(ROracle)
library(ggplot2)
library(RColorBrewer)
library(openxlsx)
library(PBSmapping)
library(data.table)
library(sf)
library(dplyr)
library(lubridate)


#### DEFINE ####

direct <- "Y:/Inshore/Assessment/BoF"
#direct <- "Y:/Inshore/BoF"
fishingyear <- 2026 #most recent year of commercial fishing data to be used (e.g. if fishing season is 2019/2020, use 2020)
assessmentyear <- 2026 #year in which you are conducting the assessment
un.ID=Sys.getenv("un.raperj") #ptran username
pwd.ID=Sys.getenv("pw.raperj") #ptran password
#un.ID=un.sameotoj #ptran username
#pwd.ID=pw.sameotoj#ptran password

#Date range for logs to be selected 
start.date.logs <- "2025-10-01"  #YYYY-MM-DD use Oct 1 
ends.date.logs <- "2026-10-01"  #YYYY-MM-DD use Oct 1 


#### Read files ####

# TAC and Landings time series flat files
tacq<-read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA3_TACandLandings_", 
                      fishingyear, ".xlsx"), sheet = "TACandLandings")

#CPUE and effort csv files from previous year
CPUE_spa3_subarea <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_spa3_subarea_", fishingyear-1, ".csv"))
CPUE_spa3_combined <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_spa3_combined_", fishingyear-1, ".csv"))

#Polygon to separate catch between BILU and St Mary's Bay
BILU.poly<-read.csv("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/csv_NAD83/BILUpoly.csv")

#Polygons for spatial plots
poly.sf <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA3_polygon_NAD83")
poly.VMS <- st_read("Y:/Inshore/Databases/Scallsur/SPA3", layer = "SPA3_VMS_StrataBrierLurcher")
poly.SMB <- st_read("Y:/Inshore/Databases/Scallsur/SPA3", layer = "SMB")




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

quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in ('SPA3')       ",
  " 	AND  s.date_fished >= to_date('",start.date.logs,"','YYYY-MM-DD') and s.date_fished < to_date('",ends.date.logs,"','YYYY-MM-DD') ",
  "AND s.licence_id not in ('356089', '356090', '356091', '356092', '368730', '369132', '369133') ", #remove FSC
  "	AND (s.data_class = 1                        ",
  "OR (s.data_class = 2 AND s.quality_flag =',4' ) ",
  "OR (s.data_class = 2 AND s.quality_flag =',1,4') ",
  "OR (s.data_class = 2 AND s.quality_flag =',2,4')) ",
  sep=""
)

chan <- dbConnect(drv = dbDriver("Oracle"), username=un.ID,  password = pwd.ID,  dbname = "ptran", believeNRows=FALSE)

logs <- dbGetQuery(chan, quer2)
dim(logs)

#Convert coordinates:
logs$lat <- convert.dd.dddd(logs$LATITUDE/100) #coordinates in SCALLOP db are stored without the decimal point, have to divide by 100 to get degrees decimal minutes for convert.dd.dddd
logs$lon <- (-1)*convert.dd.dddd(logs$LONGITUDE/100)

#Add year and month columns:
logs$DATE_FISHED <- as.Date(logs$DATE_FISHED, format="%Y-%m-%d")  
logs$YEAR <- year(logs$DATE_FISHED)
logs$month <- month(logs$DATE_FISHED)
head(logs)
unique(logs$month)
str(logs)

#Check data
table(logs$YEAR) #check years, fishing season in SPA3 spans 2 calendar years
table(logs$FLEET) #check fleets, SPA3 should only have records from Full Bay

#Check cpue by month
logs %>%
  group_by(month) %>%
  summarise(mean(CPUE_KG))

#Check landings by month
logs %>%
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG/1000))

#If any issues here, STOP and address with log scan and/or VMS checks. Make any corrections in the SCALLOP db (and send to CDD), then restart this script


#Remove CPUE outliers
logs <- (logs[which(logs$CPUE_KG <= 200),]) 
dim(logs)

#Change year in logs to reflect fishing year instead of calendar year
logs$YEAR <- as.numeric(fishingyear) 

#### Separate Catch by area (BILU and St.Mary's Bay) and season (Oct/June) ####

#Assign area
logs$area<-"SMB"
logs$ID<-1:nrow(logs)
events=subset(logs,area=="SMB",c("ID","lon","lat"))
names(events)<-c("EID","X","Y")
logs$area[logs$ID%in%findPolys(events,BILU.poly)$EID]<-"BILU"

#Assign fishing season (June/Oct)
logs$season <- "June" 
logs$season[logs$DATE_FISHED %between% c("2025-10-01", "2025-11-15")] <- "Oct" #Update date range to current season
#check date ranges to make sure none fall outside fishing season

table(logs$season, logs$area) #check no of records for rule of <5

#### Calculate CPUE by subarea and season ####

logs$effort_h <- (logs$AVG_TOW_TIME*logs$NUM_OF_TOWS)/60

effort.dat.subarea <- aggregate(logs$effort_h, by=list(logs$area, logs$season), FUN=sum)
names(effort.dat.subarea) <- c("area","season","effort.hr")

catch.dat.subarea <-  aggregate(logs$DAY_CATCH_KG, by=list(logs$area, logs$season), FUN=sum)
names(catch.dat.subarea) <- c("area", "season", "catch.kg")

nlogs.subarea <- aggregate(logs$LOG_SEQ, by=list(logs$area, logs$season), FUN=length)
names(nlogs.subarea) <- c("area", "season", "n")

cpue.subarea <- merge(merge(effort.dat.subarea, catch.dat.subarea, by.x=c("area", "season"), by.y=c("area", "season"), all=TRUE), 
                      nlogs.subarea, by.x=c("area", "season"), by.y=c("area", "season"), all=TRUE)

#CPUE is reported as long as rule of 5 is met for Privacy Act considerations, otherwise returns NA.
cpue.subarea$cpue.kgh <- ifelse(cpue.subarea$n > 5, cpue.subarea$catch.kg/cpue.subarea$effort.hr, NA)


#### Update CPUE_spa3_subarea csv file ####

#Convert catch and effort units
cpue.subarea$catch.dat.mt <- cpue.subarea$catch.kg/1000
cpue.subarea$effort.dat.1000h <- cpue.subarea$effort.hr/1000

#Add fishing year and update season
cpue.subarea$year <- fishingyear
cpue.subarea$date <- ifelse(cpue.subarea$season == "June", paste0("Jun-", fishingyear), paste("Oct-", fishingyear-1))

#Append current year data to CPUE_spa3_subarea
comm.dat.subarea <- rbind(CPUE_spa3_subarea, cpue.subarea[,c("area", "year", "season", "date", "cpue.kgh", "effort.dat.1000h", "catch.dat.mt")])

#Save to current assessment folder
write.csv(comm.dat.subarea, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_spa3_subarea_", fishingyear, ".csv"), row.names = FALSE)


#### Calculate Fleet Effort ####

#CPUE all areas combined:
effort.dat.combined <- sum(logs$effort_h)
catch.dat.combined <-  sum(logs$DAY_CATCH_KG)

cpue.combined <- data.frame(area = "SPA3", year = fishingyear, effort.dat.1000h = effort.dat.combined/1000, 
                            catch.dat.mt = catch.dat.combined/1000, cpue.kgh = catch.dat.combined/effort.dat.combined)

#Get landings data:
landings <- tacq[,-1]
landings <- as.data.frame(t(landings))
names(landings) <- c("landings.fleet.mt", "TAC")
landings$year <- as.numeric(substr(rownames(landings),6,9))

#Calculate fleet effort
cpue.combined$catch.fleet.mt <- landings$landings.fleet.mt[which(landings$year == fishingyear)]
cpue.combined$effort.fleet.1000h <- cpue.combined$catch.fleet.mt/cpue.combined$cpue.kgh


#### Update CPUE_spa3_combined csv file ####

#Append current year data to CPUE_spa6_combined
comm.dat.combined <- rbind(CPUE_spa3_combined, cpue.combined)

#Save to current assessment folder
write.csv(comm.dat.combined, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_spa3_combined_", fishingyear, ".csv"), row.names = FALSE)


#### Time Series Plots ####

#CPUE by season and subarea:

#median lines
BLSummer <- filter(comm.dat.subarea, area == "BILU", season == "June")
med.BLSummer <- median(BLSummer$cpue.kgh, na.rm = TRUE)
SMB <- filter(comm.dat.subarea, area == "SMB", season == "June")
med.SMB <- median(SMB$cpue.kgh, na.rm = TRUE)
BLFall <- filter(comm.dat.subarea, area == "BILU", season == "Oct")
med.BLFall <- median(BLFall$cpue.kgh, na.rm = TRUE)
med = c(med.BLSummer, med.SMB, med.BLFall)

#plot
ggplot(filter(comm.dat.subarea, area != 'SMB'| season != 'Oct'), aes(x = year, y = cpue.kgh, colour = interaction(area, season))) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) +  
  geom_line() +
  geom_point() +
  scale_x_continuous("Year", breaks = seq(2002,fishingyear,2)) +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,50), breaks = seq(0,50,10)) +
  scale_colour_manual(values = c("black", "blue", "green"), labels = c("Brier/Lurcher Summer", "St. Mary's Bay", "Brier/Lurcher Fall")) +
  geom_hline(yintercept = med, linetype = "dashed", color = c("black", "blue", "green")) +
  theme(legend.title = element_blank())
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_CPUE",fishingyear, ".png"), width = 24,height = 16,dpi = 400,units='cm')



#CPUE by month
CPUE_month <- logs %>% 
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG)/sum(effort_h)) %>%
  slice(5,1:4) %>% #reorder months, this may change depending on months fished in a given year
  mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order

ggplot(CPUE_month, aes(x = month, y = `sum(DAY_CATCH_KG)/sum(effort_h)`, group =1)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_point() +
  geom_line() +
  scale_x_discrete("Month") +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,80), breaks = seq(0,80,10))
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_CPUEbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')

#Landings by month (from clean logs)
Landings_month <- logs %>% 
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG/1000)) %>%
  slice(5,1:4) %>% #reorder months, this may change depending on months fished in a given year
  mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order

ggplot(Landings_month, aes(x = month, y = `sum(DAY_CATCH_KG/1000)`, group =1)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_point() +
  geom_line() +
  scale_x_discrete("Month") +
  scale_y_continuous("Landings (t)")
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_LandingsbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')


#CPUE and Effort (fishery-level)
ggplot(comm.dat.combined) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) +  
  theme(axis.line.y.right = element_line(color = "red"), axis.ticks.y.right = element_line(color = "red"), axis.text.y.right = element_text(color = "red")) +
  geom_point(aes(x = year, y = cpue.kgh)) + 
  geom_line(aes(x = year, y = cpue.kgh)) + 
  geom_point(aes(x = year, y = effort.fleet.1000h), color = "red") + 
  geom_line(aes(x = year, y = effort.fleet.1000h), color = "red", linetype = "dashed") +
  scale_y_continuous("CPUE (kg/h)", sec.axis = sec_axis(~./1, name = "Effort (1000h)")) +  
  scale_x_continuous("Year", breaks=seq(2003,fishingyear,2))
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_CPUEandEffort_combined",fishingyear, ".png"), width = 24,height = 20,dpi = 400,units='cm')


#TAC and Landings
ggplot(landings) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_bar(aes(x = year, y = landings.fleet.mt), stat = "identity", color = "black", fill = "white") +
  geom_line(aes(x = year, y = TAC)) +
  scale_y_continuous("Landings (meats, t)", breaks=seq(0,400,50), limits = c(0, 350)) + 
  scale_x_continuous("Year", breaks=seq(1997,fishingyear,2)) +
  geom_text(x = 2003.5, y = 315, label = "TAC", size = 5)
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_TACandLandings",fishingyear, ".png"), width = 24,height = 20, dpi = 400,units='cm')


#### Catch and effort with linear model ####
ggplot(logs, aes(x = effort_h, y = DAY_CATCH_KG)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_y_continuous("Catch (meats, kg)") + 
  scale_x_continuous("Effort (h)") 
# + facet_wrap(~FLEET, scales = "free") #use if you want separate plots by fleet
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_CatchandEffort",fishingyear, ".png"), width = 24, height = 20, dpi = 400,units='cm')



#### SPATIAL PLOTS ####

#Pecjector basemap with SPA3 boundaries
p <- pecjector(area =list(x=c(-66.8,-65.8), y=c(43.6,44.6), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = c(50,'c'), scale.bar = c('tl')))


#CPUE Grid Plot

#Create raster of mean cpue
for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','CPUE_KG'))
coordinates(for.raster) <- c("lon", "lat")

r <- raster(ext = extent(poly.sf), resolution = 1/60, crs = CRS('+init=EPSG:4326'))

raster.data <- rasterize(for.raster, r, "CPUE_KG", fun = mean)

##convert raster to sf object
df <- (as.data.frame(raster::rasterToPoints(raster.data)))
names(df) <- c("lon", "lat", "mean.cpue")

##add sf objects to basemap outside of pecjector
p +
  geom_tile(df, mapping = aes(lon, lat, fill = mean.cpue), color = "grey55") +
  geom_sf(data = poly.sf, fill=NA, colour="grey55") +
  geom_sf(data = poly.VMS, fill=NA, colour="red") +
  geom_sf(data = poly.SMB, fill=NA, colour="red") +
  coord_sf(xlim = c(-66.8,-65.8), ylim = c(43.6,44.6), expand = FALSE) +
  scale_fill_binned(type = "viridis", direction = -1, name="CPUE (kg/h)") +
  #geom_polygon(data = shp, aes(x = long, y = lat, group = group), fill="grey55") +
  #annotation_scale() +
  #annotation_north_arrow(location = "tl") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"), 
      legend.text = element_text(size = 8),
      legend.position = c(.85,.5), #legend position
      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
      legend.box.margin = margin(6, 8, 6, 8))
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_CPUEgridplot_new",fishingyear, ".png"), width = 9, height = 9, dpi = 200,units='in')


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
  geom_sf(data = poly.sf, fill=NA, colour="grey55") +
  geom_sf(data = poly.VMS, fill=NA, colour="red") +
  geom_sf(data = poly.SMB, fill=NA, colour="red") +
  coord_sf(xlim = c(-66.8,-65.8), ylim = c(43.6,44.6), expand = FALSE) +
  scale_fill_binned(type = "viridis", direction = -1, name="Catch (kg)", breaks = c(500, 1000, 1500, 2000, 2500, 3000)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.85,.5), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8))
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_Catchgridplot",fishingyear, ".png"), width = 9, height = 9, dpi = 200,units='in')


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
  geom_sf(data = poly.sf, fill=NA, colour="grey55") +
  geom_sf(data = poly.VMS, fill=NA, colour="red") +
  geom_sf(data = poly.SMB, fill=NA, colour="red") +
  coord_sf(xlim = c(-66.8,-65.8), ylim = c(43.6,44.6), expand = FALSE) +
  scale_fill_binned(type = "viridis", direction = -1, name="Effort (h)", breaks = c(25, 50, 75, 100, 125, 150, 200)) +
  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        legend.position = c(.85,.5), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 1)), #Legend bkg colour and transparency
        legend.box.margin = margin(6, 8, 6, 8))
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA3_Effortgridplot",fishingyear, ".png"), width = 9, height = 9, dpi = 200,units='in')

