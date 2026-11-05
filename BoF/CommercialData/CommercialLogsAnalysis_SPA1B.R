###.....................................###
###  Commercial Catch Rates and Plots   ###
###              SPA 1B                 ###
###       J. Raper June 2020            ###
###.....................................###

#Summarizes commercial log data from SPA1B for a given year and updates the following files:
# CPUE_1B_subarea_yyyy.csv
# CPUE_1B_fleet_yyyy.csv
# CPUE_1B_combined_yyyy.csv

#Creates the following plots:
# SPA1B_CPUEyyyy.png
# SPA1B_CPUEandEffortyyyy.png
# SPA1B_CPUEandEffort_combinedyyyy.png
# SPA1B_TACandLandingsyyyy.png
# SPA1B_CatchandEffortyyyy.png
# SPA1B_CPUEgridplotyyyy.png
# SPA1B_Effortgridplotyyyy.png
# SPA1B_Catchgridplotyyyy.png
# SPA1B_CPEUbyMonthyyyy.png
# SPA1B_LandingsbyMonthyyyy.png


#BEFORE RUNNING THIS SCRIPT:

# compare SCALLOP db landings with area cap monitoring report and address any issues (differences > 2 mt for each fleet/area should be investigated)
# update SPA1B_TACandLandings_YYYY.xlsx with TAC and landings from monitoring report and save in the current assessment folder: Y:\INSHORE SCALLOP\BoF\YYYY\Assessment\Data\CommercialData
# ensure date format in your SQL Developer profile is set to YYYY-MM-DD

library(ROracle)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(openxlsx)
library(sf)
library(lubridate)


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

# TAC and Landings time series flat file
tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1B_TACandLandings_", 
                       fishingyear, ".xlsx"), sheet = "TACandLandings")

#CPUE_1B_subarea.csv and CPUE_1B_fleet.csv files from previous year
CPUE_1B_subarea <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_1B_subarea_", (fishingyear-1), ".csv")) 	
CPUE_1B_fleet <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_1B_fleet_", (fishingyear-1), ".csv")) 	
CPUE_1B_combined <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_1B_combined_", (fishingyear-1), ".csv")) 	

#Polygons for spatial plots
poly.sf <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA1B_polygon_NAD83")
poly.strata <-  st_read("Y:/People/Amy/2012 survey prep/AmyArc", layer = "SCSTRATADEFS_Polygons")


#### Select data #### 

quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in ('1B', '28C', '28D')       ",
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
table(logs$YEAR) #check years, fishing season in 1B spans 2 calendar years
table(logs$FLEET) #check fleets, 1B is fished by Full Bay, Mid-Bay, and Upper-Bay
table(logs$FLEET, logs$ASSIGNED_AREA) #check no of records for rule of <5

#Check cpue by month
logs %>%
  group_by(month) %>%
  summarise(mean(CPUE_KG))

#Check landings by month
logs %>%
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG/1000))

#If any issues here, STOP and address with log scan and/or VMS checks. Make any corrections in the SCALLOP db (and send to CDD), then restart this script

#2024 - remove CPUE outliers
logs <- (logs[which(logs$CPUE <= 200),]) 
dim(logs)

#Change year in logs to reflect fishing year instead of calendar year
logs$YEAR <- as.numeric(fishingyear) 

#Replace '1B' with '28B'
logs$ASSIGNED_AREA <- sub("1B", "28B", logs$ASSIGNED_AREA)


#### Calculate CPUE by fleet and subarea ####

logs$effort_h <- (logs$AVG_TOW_TIME*logs$NUM_OF_TOWS)/60

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
print(cpue.subarea)


#### Update CPUE_1B_subarea csv file ####

#Convert catch and effort units
cpue.subarea$catch.dat.mt <- cpue.subarea$catch.kg/1000
cpue.subarea$effort.dat.1000h <- cpue.subarea$effort.hr/1000

#Add fishing season and year
cpue.subarea$season <- ifelse(cpue.subarea$fleet == "Full Bay", paste0(fishingyear-1, "/", fishingyear), fishingyear)
cpue.subarea$year <- fishingyear

#Append current year data to CPUE_1B
comm.dat.subarea <- rbind(CPUE_1B_subarea, cpue.subarea[,c("fleet", "area", "season", "year", "cpue.kgh", "catch.dat.mt", "effort.dat.1000h")])

#Save to current assessment folder
write.csv(comm.dat.subarea, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_1B_subarea_", fishingyear, ".csv"), row.names = FALSE)


#### Calculate Fleet Effort ####

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
landings <- tacq[c(1:4,9),-1]
landings <- as.data.frame(t(landings))
names(landings) <- c("FB", "MB", "UB","FSC", "TAC")
landings$year <- as.numeric(substr(rownames(landings),6,9))

#Convert landings data to long format
landings <- reshape2::melt(landings, id.vars = "year", value.name = "catch.fleet.mt")

#Combine landings with cpue
catch.fishery.fleet <- landings[which(landings$year == fishingyear),]
catch.fishery.fleet <- catch.fishery.fleet[-(4:5),]
cpue.fleet <- cbind(cpue.fleet, catch.fishery.fleet)
print(cpue.fleet)

#Calculate fishery-level (fleet) effort:
cpue.fleet$effort.fleet.1000h <- cpue.fleet$catch.fleet.mt/cpue.fleet$cpue.kgh


#### Update CPUE_1B_fleet csv file ####

#Append current year data to CPUE_1B
comm.dat.fleet <- rbind(CPUE_1B_fleet, cpue.fleet[,c("fleet", "year", "effort.dat.hr", "catch.dat.kg", "cpue.kgh", "catch.fleet.mt", "effort.fleet.1000h")])

#Save to current asssessment folder
write.csv(comm.dat.fleet, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_1B_fleet_", fishingyear, ".csv"), row.names = FALSE)


#### Calculate combined fleet effort  ####

effort.dat.combined <- sum(logs$effort_h)
catch.dat.combined <-  sum(logs$DAY_CATCH_KG)

cpue.combined <- data.frame(area = "SPA1B", year = fishingyear, effort.dat.hr = effort.dat.combined, 
                            catch.dat.kg = catch.dat.combined, cpue.kgh = catch.dat.combined/effort.dat.combined)

#Combine landings with cpue
catch.fishery.combined <- landings %>%
  filter(year == fishingyear & variable != 'TAC') %>%
  summarize(catch.fleet.mt = sum(catch.fleet.mt, na.rm=TRUE))

cpue.combined <- cbind(cpue.combined, catch.fishery.combined)

#Calculate fishery-level (fleet) effort:
cpue.combined$effort.fleet.1000h <- cpue.combined$catch.fleet.mt/cpue.combined$cpue.kgh 
print(cpue.combined)


#### Update CPUE_1B_combined csv file ####

#Append current year data to CPUE_1B_combined
comm.dat.combined <- rbind(CPUE_1B_combined, cpue.combined)

#Save to current asssessment folder
write.csv(comm.dat.combined, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_1B_combined_", fishingyear, ".csv"), row.names = FALSE) 


#### Time Series Plots ####

#CPUE by fleet and subarea:
ggplot(filter(comm.dat.subarea, area != '1B'), aes(x = year, y = cpue.kgh)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) +  
  geom_point(aes(colour = fleet)) +
  geom_line(aes(linetype = fleet, colour = fleet)) +
  scale_x_continuous("Year", breaks = seq(2003,fishingyear,5)) +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,50), breaks = seq(0,50,10)) +
  scale_colour_manual(values = c("black", "blue", "green")) +
  facet_wrap(~area) +
  theme(legend.title = element_blank())
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_CPUE",fishingyear, ".png"), width = 24,height = 16,dpi=400,units='cm')

#CPUE combined
ggplot(comm.dat.combined, aes(x = year, y = cpue.kgh)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) +  
  geom_point() +
  geom_line() +
  scale_x_continuous("Year", breaks = seq(2003,fishingyear,5)) +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,50), breaks = seq(0,50,10)) +
  geom_hline(aes(yintercept=median(cpue.kgh[1:(dim(comm.dat.combined)[1]-1)])), linetype = "dashed") + # median line is all years except current year
  geom_text(x = 2009, y = 2, label = "median", size = 4)
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_CPUE_combined",fishingyear, ".png"), width = 24,height = 16,dpi=400,units='cm')

#CPUE by month
CPUE_month <- logs %>% 
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG)/sum(effort_h)) %>%
  slice(8:10,1:7) %>% #reorder months, this may change depending on months fished in a given year
  mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order

ggplot(CPUE_month, aes(x = month, y = `sum(DAY_CATCH_KG)/sum(effort_h)`, group =1)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_point() +
  geom_line() +
  scale_x_discrete("Month") +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,60), breaks = seq(0,60,10))
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_CPUEbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')

#Landings by month (from clean logs)
Landings_month <- logs %>% 
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG/1000)) %>%
  slice(8:10,1:7) %>% #reorder months, this may change depending on months fished in a given year
  mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order

ggplot(Landings_month, aes(x = month, y = `sum(DAY_CATCH_KG/1000)`, group =1)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_point() +
  geom_line() +
  scale_x_discrete("Month") +
  scale_y_continuous("Landings (t)")
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_LandingsbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')


#CPUE and Effort by fleet (fishery-level)
ggplot(comm.dat.fleet) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) +  
  theme(axis.line.y.right = element_line(color = "red"), axis.ticks.y.right = element_line(color = "red"), axis.text.y.right = element_text(color = "red")) +
  geom_point(aes(x = year, y = cpue.kgh)) + 
  geom_line(aes(x = year, y = cpue.kgh)) + 
  geom_point(aes(x = year, y = effort.fleet.1000h*2), color = "red") + 
  geom_line(aes(x = year, y = effort.fleet.1000h*2), color = "red", linetype = "dashed") +
  scale_y_continuous("CPUE (kg/h)", sec.axis = sec_axis(~./2, name = "Effort (1000h)", breaks = seq(0,20,5))) + # 
  scale_x_continuous("Year", breaks=seq(2003,fishingyear,3)) +
  facet_wrap(~fleet, ncol = 1)
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_CPUEandEffort",fishingyear, ".png"), width = 24,height = 20,dpi=400,units='cm')

#CPUE and Effort combined (fishery-level)
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
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_CPUEandEffort_combined",fishingyear, ".png"), width = 24,height = 20,dpi=400,units='cm')

#TAC and Landings
landings %>%filter(year >= 2003) %>%
ggplot() +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + # white background, no gridlines
  #geom_bar(data=landings[landings$variable%in%c('FB','MB', 'UB', 'FSC'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c('FSC','UB', 'MB', 'FB'))), colour="black", stat="identity") + 
  geom_bar(data=landings %>% filter (year >=2003, variable != 'TAC'), aes(year, catch.fleet.mt, fill=factor(variable, levels = c('FSC','UB', 'MB', 'FB'))), colour="black", stat="identity") + 
  geom_line(data=landings[landings$variable == 'TAC',], aes(x = year, y = catch.fleet.mt), lwd = 1) +
  scale_y_continuous("Landings (meats, t)", breaks=seq(0,1200,200)) + # 
  scale_x_continuous("Year", breaks=seq(2003,fishingyear,2), limits =c(2002.5, fishingyear+0.5)) +
  scale_fill_manual(values=c("black", "white", "royalblue2", "grey"), labels=c("FSC","Upper Bay", "Mid-Bay", "Full Bay"), name=NULL) +
  annotate(geom="text",label="TAC", x=2010.4, y= 425) +
  theme(legend.position=c(0.15, 0.85)) # play with the location if you want it inside the plotting panel
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_TACandLandings",fishingyear, ".png"),width =24,height =20,dpi=400,units='cm')


#### Catch and effort with linear model ####
 ggplot(logs, aes(x = effort_h, y = DAY_CATCH_KG)) +
   theme_bw() + theme(panel.grid=element_blank()) +
   geom_point() +
   geom_smooth(method='lm') +
   scale_y_continuous("Catch (meats, kg)") + 
   scale_x_continuous("Effort (h)") 
   # + facet_wrap(~FLEET, scales = "free") #use if you want separate plots by fleet
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_CatchandEffort",fishingyear, ".png"), width = 24, height = 20,dpi=400,units='cm')



#### SPATIAL PLOTS ####
 
 
#Pecjector basemap with SPA 1B boundaries
p <- pecjector(area =list(x=c(-66.5,-64.4), y=c(44.8,45.8), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = c(50,'c')))

 
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
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.5,-64.4), ylim = c(44.8,45.8), expand = FALSE) +
   scale_fill_binned(type = "viridis", direction = -1, name="CPUE (kg/h)", limits = c(0, 150), breaks = c(25, 50, 75, 100, 125)) +
   annotation_scale(location = "tl") +
   annotation_north_arrow(location = "br") +
   theme(legend.key.size = unit(5,"mm"),
         plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 10, face = "bold"), 
         legend.text = element_text(size = 10),
         legend.position = c(.85, .15),
         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
         legend.box.margin = margin(1, 1, 1, 1))
 #save
 ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_CPUEgridplot",fishingyear, ".png"), width=9,height=9,dpi=200,units='in')
 
 
 #Make histogram by area for distribution of cpue
 histolog<-subset(logs, YEAR == fishingyear, c('ASSIGNED_AREA','CPUE_KG'))

 cpuehisto <- ggplot (histolog, aes (x = CPUE_KG)) +
   geom_histogram (breaks = seq (0, 200, by = 5), col = 'grey60') + labs (x = 'CPUE (kg/h)', y = 'Count') +
   #facet_wrap (~ASSIGNED_AREA, ncol = 1) +
   scale_x_continuous(breaks = seq (0, 200, by = 50)) +
   theme_bw() +
   theme(panel.grid=element_blank())
 cpuehisto

 tapply(histolog$CPUE_KG, histolog$ASSIGNED_AREA, summary)
 
 
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
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.5,-64.4), ylim = c(44.8,45.8), expand = FALSE) +
   scale_fill_binned(type = "viridis", direction = -1, name="Catch (kg)", breaks = c(2000, 4000, 6000, 8000)) +
   theme(legend.key.size = unit(5,"mm"),
         plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 10, face = "bold"), 
         legend.text = element_text(size = 10),
         legend.position = c(.8, .18),
         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
         legend.box.margin = margin(1, 1, 1, 1))
 #save
 ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_Catchgridplot",fishingyear, ".png"), width=9,height=9,dpi = 200,units='in')
 
 
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
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.5,-64.4), ylim = c(44.8,45.8), expand = FALSE) +
   scale_fill_binned(type = "viridis", direction = -1, name="Effort (h)") +
   theme(legend.key.size = unit(5,"mm"),
           plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
           axis.title = element_text(size = 12),
           axis.text = element_text(size = 10),
           legend.title = element_text(size = 10, face = "bold"), 
           legend.text = element_text(size = 10),
           legend.position = c(.8, .18),
           legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
           legend.box.margin = margin(1, 1, 1, 1))
 #save
 ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA1B_Effortgridplot",fishingyear, ".png"), width = 9,height = 9,dpi = 200,units='in')
