###.....................................###
###  Commercial Catch Rates and Plots   ###
###             SPA 4 & 5               ###
###        J. Raper June 2020           ###
###.....................................###

#Summarizes commercial log data from SPA4 and SPA5 for a given year and updates CPUE_spa4.csv and CPUE_spa5.csv files. 
#Creates the following plots:
# SPA4_CPUEyyyy.png
# SPA5_CPUEyyyy.png
# SPA4_CPUEandEffortyyyy.png
# SPA5_CPUEandEffortyyyy.png
# SPA4and5_TACandLandingsyyyy.png
# SPA4and5_CatchandEffortyyyy.png
# SPA4and5_CPUEgridplotyyyy.png
# SPA4and5_CPUEgridplotyyyy.png
# SPA4and5_CPUEgridplotyyyy.png
# SPA1A_CPEUbyMonthyyyy.png
# SPA1A_LandingsbyMonthyyyy.png

#BEFORE RUNNING THIS SCRIPT:

# compare SCALLOP db landings with area cap monitoring report and address any issues (differences > 2 mt for each fleet/area should be investigated)
# update SPA4and5_TACandLandings_YYYY.xlsx with TAC and landings from monitoring report and save in the current assessment folder: Y:\INSHORE SCALLOP\BoF\YYYY\Assessment\Data\CommercialData
# ensure date format in your SQL Developer profile is set to YYYY-MM-DD

library(ROracle)
library(ggplot2)
library(RColorBrewer)
library(openxlsx)
library(sf) 
library(lubridate)



#### Import Mar-scal functions 

funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R")
dir <- getwd()
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

#Date range for logs to be selected 
start.date.logs <- "2025-10-01"  #YYYY-MM-DD use Oct 1 
ends.date.logs <- "2026-10-01"  #YYYY-MM-DD use Oct 1 

#### Read files ####

# TAC and Landings time series flat files
tacq<-read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA4and5_TACandLandings_", 
                       fishingyear, ".xlsx"), sheet = "TACandLandings")
tacq4<-read.csv("Y:/Inshore/Assessment/BoF/CommercialData/Archive/2014/SPA4_TACandLandings_2014.csv") # DO NOT UPDATE - historial data 

#CPUE csv files from previous year
CPUE_spa4 <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_spa4_", fishingyear-1, ".csv")) 
CPUE_spa5 <- read.csv(paste0(direct,"/",(assessmentyear-1),"/Assessment/Data/CommercialData/CPUE_spa5_", fishingyear-1, ".csv")) 

#Polygons for spatial plots

poly.sf4 <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA4_polygon_NAD83")
poly.sf5 <- st_read("Y:/GISdata/Private/BoFBoundaries/SPABoundaries_Redrawn2014/SPA New Polys/shp polygons", layer = "SPA5_polygon_NAD83")
poly.strata <- st_read("Y:/People/Amy/2012 survey prep/AmyArc", layer = "SCSTRATADEFS_Polygons")



#### Select data #### 

#Update date range
quer2 <- paste(
  "SELECT * 			                             ",
  "FROM scallop.scallop_log_marfis s		         ",
  "WHERE s.assigned_area in ('SPA4', 'SPA5')       ",
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
table(logs$YEAR, logs$ASSIGNED_AREA) #check years, fishing season spans 2 calendar years in SPA4 and 1 in SPA 5
table(logs$FLEET) #check fleets, SPA4 & 5 should only have records from Full Bay

#Check cpue by month
logs %>%
  group_by(month) %>%
  summarise(mean(CPUE_KG))

#Check landings by month
logs %>%
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG/1000))
#If any issues here, STOP and address with log scan and/or VMS checks. Make any corrections in the SCALLOP db (and send to CDD), then restart this script

#Remove Mid-Bay records
logs <- logs[logs$FLEET != 'Mid-Bay',]

#Remove CPUE outliers
dim(logs)
logs <- logs[logs$CPUE_KG <200,]
dim(logs)

#Change year in logs to reflect fishing year instead of calendar year
logs$YEAR <- as.numeric(fishingyear)


#### Calculate CPUE by Area ####

logs$effort_h <- (logs$AVG_TOW_TIME*logs$NUM_OF_TOWS)/60

effort.dat <- aggregate(logs$effort_h, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=sum)
names(effort.dat) <- c("fleet","area","effort.hr")

catch.dat <-  aggregate(logs$DAY_CATCH_KG, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=sum)
names(catch.dat) <- c("fleet", "area", "catch.kg")

nlogs <- aggregate(logs$LOG_SEQ, by=list(logs$FLEET, logs$ASSIGNED_AREA), FUN=length)
names(nlogs) <- c("fleet", "area", "n")

cpue <- merge(merge(effort.dat, catch.dat, by.x=c("fleet","area"), by.y=c("fleet","area")), 
              nlogs, by.x=c("fleet","area"), by.y=c("fleet","area"))

#CPUE is reported as long as rule of 5 is met for Privacy Act considerations, otherwise returns NA.
cpue$cpue.kgh <- ifelse(cpue$n > 5, cpue$catch.kg/cpue$effort.hr, NA)


#### Calculate Fleet Effort ####

#Landings by fleet:
landings <- tacq[,-1]
landings <- as.data.frame(t(landings))
names(landings) <- c("SPA4", "SPA5", "TAC")
landings$year <- as.numeric(substr(rownames(landings),6,9))

#Convert landings data to long format
landings <- reshape2::melt(landings, id.vars = "year", value.name = "catch.fleet.mt")

#Combine landings with cpue
catch.fishery.area <- landings[which(landings$year == fishingyear),]
catch.fishery.area <- catch.fishery.area[-3,]
cpue.fleet <- cbind(cpue, catch.fishery.area)

#Calculate fishery-level (fleet) effort:
cpue.fleet$effort.fleet.1000h <- cpue.fleet$catch.fleet.mt/cpue.fleet$cpue.kgh
print(cpue.fleet)


#### Update CPUE_spa4 and CPUE_spa5 csv files ####

#Convert catch and effort units
cpue.fleet$catch.dat.mt <- cpue.fleet$catch.kg/1000
cpue.fleet$effort.dat.1000h <- cpue$effort.hr/1000

#Combine and rearrange columns:
comm.dat <- cbind(cpue.fleet, year = fishingyear, season = paste0(fishingyear-1, "/", fishingyear)) 
comm.dat <- comm.dat[,c("fleet", "area", "season", "year", "cpue.kgh", "catch.dat.mt", "effort.dat.1000h", "catch.fleet.mt", "effort.fleet.1000h")]

#Append current year data to CPUE_spa4
comm.dat.spa4 <- rbind(CPUE_spa4, comm.dat[which(cpue$area == 'SPA4'),])

#Append current year data to CPUE_spa5
comm.dat.spa5 <- rbind(CPUE_spa5, comm.dat[which(cpue$area == 'SPA5'),])

#Save to current assessment folder
write.csv(comm.dat.spa4, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_spa4_", fishingyear, ".csv"), row.names = FALSE)
write.csv(comm.dat.spa5, paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/CPUE_spa5_", fishingyear, ".csv"), row.names = FALSE)


#### Time Series Plots ####

#SPA4 CPUE time series:
ggplot(comm.dat.spa4, aes(x = year, y = cpue.kgh)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + 
  geom_point() +
  geom_line() +
  scale_x_continuous("Year", breaks = seq(1983,fishingyear,3)) +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,110), breaks = seq(0,110,10)) +
  geom_hline(aes(yintercept=median(cpue.kgh[1:(dim(comm.dat.spa4)[1]-1)])), linetype = "dashed") + # median line is all years except current year
  geom_text(x = 1998, y = 22, label = "median")
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4_CPUE",fishingyear, ".png"), width = 9.5, height = 8, dpi = 300,units='in')

#SPA5 CPUE time series:
ggplot(comm.dat.spa5, aes(x = year, y = cpue.kgh)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + 
  geom_point() +
  geom_line() +
  scale_x_continuous("Year", breaks = seq(1977,fishingyear,3)) +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,60), breaks = seq(0,60,10)) +
  geom_hline(aes(yintercept=median(cpue.kgh[1:(dim(comm.dat.spa5)[1]-1)])), linetype = "dashed") + # median line is all years except current year
  geom_text(x = 1998, y = 22, label = "median")
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4_CPUE",fishingyear, ".png"), width = 9.5, height = 8, dpi = 300,units='in')

#CPUE by month
CPUE_month <- logs %>% 
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG)/sum(effort_h)) %>%
  slice(5:7,1:4) %>% #reorder months, this may change depending on months fished in a given year
  mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order

ggplot(CPUE_month, aes(x = month, y = `sum(DAY_CATCH_KG)/sum(effort_h)`, group =1)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_point() +
  geom_line() +
  scale_x_discrete("Month") +
  scale_y_continuous("Catch Rate (kg/h)", limits = c(0,60), breaks = seq(0,60,10))
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_CPUEbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')

#Landings by month (from clean logs)
Landings_month <- logs %>% 
  group_by(month) %>%
  summarise(sum(DAY_CATCH_KG/1000)) %>%
  slice(5:7,1:4) %>% #reorder months, this may change depending on months fished in a given year
  mutate(month = factor(month, levels=unique(month))) #so months will plot in correct order

ggplot(Landings_month, aes(x = month, y = `sum(DAY_CATCH_KG/1000)`, group =1)) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_point() +
  geom_line() +
  scale_x_discrete("Month") +
  scale_y_continuous("Landings (t)")
#save
ggsave(filename = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_LandingsbyMonth",fishingyear, ".png"), width=24,height=20,dpi=400,units='cm')



#SPA4 CPUE and Effort (fishery-level)
ggplot(comm.dat.spa4) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + 
  theme(axis.line.y.right = element_line(color = "red"), axis.ticks.y.right = element_line(color = "red"), axis.text.y.right = element_text(color = "red")) +
  geom_point(aes(x = year, y = cpue.kgh)) + 
  geom_line(aes(x = year, y = cpue.kgh)) + 
  geom_point(aes(x = year, y = effort.fleet.1000h*2), color = "red") + 
  geom_line(aes(x = year, y = effort.fleet.1000h*2), color = "red", linetype = "dashed") +
  scale_y_continuous("CPUE (kg/h)", sec.axis = sec_axis(~./2, name = "Effort (1000h)", breaks = seq(0,50,10))) + # 
  scale_x_continuous("Year", breaks=seq(1984,fishingyear,2)) 
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4_CPUEandEffort",fishingyear, ".png"), width = 9.5, height = 8, dpi = 300,units='in')

#SPA5 CPUE and Effort (fishery-level)
ggplot(comm.dat.spa5) +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + 
  theme(axis.line.y.right = element_line(color = "red"), axis.ticks.y.right = element_line(color = "red"), axis.text.y.right = element_text(color = "red")) +
  geom_point(aes(x = year, y = cpue.kgh)) + 
  geom_line(aes(x = year, y = cpue.kgh)) + 
  geom_point(aes(x = year, y = effort.fleet.1000h*10), color = "red") + 
  geom_line(aes(x = year, y = effort.fleet.1000h*10), color = "red", linetype = "dashed") +
  scale_y_continuous("CPUE (kg/h)", sec.axis = sec_axis(~./10, name = "Effort (1000h)", breaks = seq(0,5,1))) + # 
  scale_x_continuous("Year", limits = c(1984, fishingyear), breaks=seq(1984,fishingyear,2)) 
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA5_CPUEandEffort",fishingyear, ".png"), width = 9.5, height = 8, dpi = 300,units='in')


#TAC and Landings

#Prepare historical SPA4 TAC data:
tacq4 <- as.data.frame(t(tacq4[2,-1]))
names(tacq4) <- "TAC"
tacq4$year <- as.numeric(substr(rownames(tacq4), 2,5))+1

#Plot SPA 4 & 5 landings with TAC lines
ggplot() +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + # white background, no gridlines
  geom_bar(data=landings[landings$variable%in%c('SPA4','SPA5'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c("SPA5", "SPA4"))), colour="black", stat="identity") + 
  geom_line(data=landings[landings$variable == 'TAC' & landings$year >= 2014,], aes(x = year, y = catch.fleet.mt), lwd = 1) + #adds combined TAC line
  geom_line(data=tacq4, aes(year, TAC),linetype="dashed", lwd=1) + #adds historical SPA4 TAC line
  scale_y_continuous("Landings (meats, t)", breaks=seq(0,2800,200)) + 
  scale_x_continuous("Year", breaks=seq(1984,fishingyear,2)) +
  scale_fill_manual(values=c("skyblue1", "grey"), labels=c("SPA 5", "SPA 4"), name=NULL) +
  annotate(geom="text",label="SPA 4 and 5 TAC", x=2014, y= 300) + 
  annotate(geom="text",label="SPA 4 TAC", x=2004, y= 1350) +
  theme(legend.position=c(0.85, 0.85)) # play with the location if you want it inside the plotting panel
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_TACandLandings",fishingyear, ".png"), width = 9.5, height = 8, dpi = 300,units='in')

#SPA 4 & 5 TAC and Landings with truncated x-axis for presentations
ggplot() +
  theme_bw(base_size = 16) + theme(panel.grid=element_blank()) + # white background, no gridlines
  geom_bar(data=landings[landings$variable%in%c('SPA4','SPA5'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c("SPA5", "SPA4"))), colour="black", stat="identity") + 
  geom_line(data=landings[landings$variable == 'TAC' & landings$year >= 2014,], aes(x = year, y = catch.fleet.mt), lwd = 1) + #adds combined TAC line
  geom_line(data=tacq4[tacq4$year >= 2008,], aes(year, TAC),linetype="dashed", lwd=1) + #adds historical SPA4 TAC line
  scale_y_continuous("Landings (meats, t)", limits = c(0, 275), breaks=seq(0,250,50)) + 
  scale_x_continuous("Year", breaks=seq(2008,fishingyear,2), limits = c(2007, fishingyear+1)) +
  scale_fill_manual(values=c("skyblue1", "grey"), labels=c("SPA 5", "SPA 4"), name=NULL) +
  annotate(geom="text",label="SPA 4 and 5 TAC", x=2014, y= 240) + 
  annotate(geom="text",label="SPA 4 TAC", x=2009, y= 140) +
  theme(legend.position=c(0.7, 0.85)) # play with the location if you want it inside the plotting panel
ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_TACandLandings_pres",fishingyear, ".png"), width = 9.5, height = 8, dpi = 300,units='in')


#### Catch and effort with linear model ####
 ggplot(logs, aes(x = effort_h, y = DAY_CATCH_KG)) +
   theme_bw() + theme(panel.grid=element_blank()) +
   geom_point() +
   geom_smooth(method='lm') +
   scale_y_continuous("Catch (meats, kg)") + #
   scale_x_continuous("Effort (h)") +
   facet_wrap(~ASSIGNED_AREA, scales = "free")
 ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_CatchandEffort",fishingyear, ".png"), width = 9.5, height = 8, dpi = 300,units='in')


#### Spatial Plots ####
 
#Pecjector basemap with SPA 4&5 boundaries
p <- pecjector(area =list(x=c(-66.2,-65.5), y=c(44.46,45), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey", bathy = c(50,'c'), scale.bar = c('tl',0.5)))
 
 
#CPUE Grid Plot
 
 #Create raster of mean cpue
 for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','CPUE_KG'))
 coordinates(for.raster) <- c("lon", "lat")
 
 r <- raster(ext = extent(poly.strata), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
 
 raster.data <- rasterize(for.raster, r, "CPUE_KG", fun = mean)
 
 ##convert raster to sf object
 df <- (as.data.frame(raster::rasterToPoints(raster.data)))
 names(df) <- c("lon", "lat", "mean.cpue")
 
 ##Fix to allow coord_sf(expand = F) to work correctly
 sf::sf_use_s2(FALSE)
 
 ##add sf objects to basemap outside of pecjector
 p +
   geom_tile(df, mapping = aes(lon, lat, fill = mean.cpue), color = "grey55") +
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.2,-65.5), ylim = c(44.46,45), expand = F) +
   scale_fill_binned(type = "viridis", direction = -1, name="CPUE (kg/h)", breaks = c(25, 50, 75)) +
   #geom_polygon(data = shp, aes(x = long, y = lat, group = group), fill="grey55") +
   theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
           axis.title = element_text(size = 12),
           axis.text = element_text(size = 10),
           legend.title = element_text(size = 10, face = "bold"), 
           legend.text = element_text(size = 10),
           legend.position = c(.88,.15), 
           legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
           legend.box.margin = margin(1, 1, 1, 1)) #Legend bkg margin (top, right, bottom, left)
 ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_CPUEgridplot",fishingyear, ".png"), width = 9, height = 9, dpi = 300,units='in')
 

 # Make histogram by area for distribution of cpue
 # histolog<-subset(logs, YEAR == fishingyear, c('ASSIGNED_AREA','CPUE_KG'))
 # 
 # cpuehisto <- ggplot (histolog, aes (x = CPUE_KG)) + 
 # geom_histogram (breaks = seq (0, 1300, by = 5), col = 'grey60') + labs (x = 'CPUE (kg/h)', y = 'Count') +
 # facet_wrap (~ASSIGNED_AREA, ncol = 1) +
 # scale_x_continuous(breaks = seq (0, 1300, by = 50)) +
 # theme_bw() +
 # theme(panel.grid=element_blank()) 
 # cpuehisto
 # 
 # tapply(histolog$CPUE_KG, histolog$ASSIGNED_AREA, summary)
 

#Catch Grid Plot
 
 #Create raster of cumulative catch
 for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','DAY_CATCH_KG'))
 coordinates(for.raster) <- c("lon", "lat")
 
 r <- raster(ext = extent(poly.strata), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
 
 raster.data <- rasterize(for.raster, r, "DAY_CATCH_KG", fun = sum)
 
 ##convert raster to sf object
 df <- (as.data.frame(raster::rasterToPoints(raster.data)))
 names(df) <- c("lon", "lat", "tot.catch")


 ##add sf objects to basemap outside of pecjector
 p +
   geom_tile(df, mapping = aes(lon, lat, fill = tot.catch), color = "grey55") +
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.2,-65.5), ylim = c(44.46,45), expand = F) +
   scale_fill_binned(type = "viridis", direction = -1, name="Catch (kg)", breaks = c(1000, 2000, 3000, 4000)) +
   theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 10, face = "bold"), 
         legend.text = element_text(size = 10),
         legend.position = c(.88,.15), 
         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
         legend.box.margin = margin(1, 1, 1, 1)) #Legend bkg margin (top, right, bottom, left)
 ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_Catchgridplot",fishingyear, ".png"), width = 9, height = 9, dpi = 300,units='in')
 
 
#Effort Grid Plot

 #Create raster of mean effort
 for.raster <- subset(logs, YEAR==fishingyear, c('lon','lat','effort_h'))
 coordinates(for.raster) <- c("lon", "lat")
 
 r <- raster(ext = extent(poly.strata), resolution = 1/60, crs = CRS('+init=EPSG:4326'))
 
 raster.data <- rasterize(for.raster, r, "effort_h", fun = sum)
 
 ##convert raster to sf object
 df <- (as.data.frame(raster::rasterToPoints(raster.data)))
 names(df) <- c("lon", "lat", "tot.effort")
 
 ##add sf objects to basemap outside of pecjector
 p +
   geom_tile(df, mapping = aes(lon, lat, fill = tot.effort), color = "grey55") +
   geom_sf(data = poly.strata, fill=NA, colour="grey55") +
   coord_sf(xlim = c(-66.2,-65.5), ylim = c(44.46,45), expand = F) +
   scale_fill_binned(type = "viridis", direction = -1, name="Effort (h)") +
   theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10),
         legend.title = element_text(size = 10, face = "bold"), 
         legend.text = element_text(size = 10),
         legend.position = c(.88,.15), 
         legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
         legend.box.margin = margin(1, 1, 1, 1)) #Legend bkg margin (top, right, bottom, left)
 ggsave(file = paste0(direct, "/",assessmentyear,"/Assessment/Figures/CommercialData/SPA4and5_Effortgridplot",fishingyear, ".png"), width = 9, height = 9, dpi = 300,units='in')
 