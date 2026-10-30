

#This script is used for intersecting survey data with scallop species distribution model (SDM) layer (ESRI grid - sdm_utm_50) by:
# 1 - creating polyline between start and end points (assumes straight line tow)
# 2 - segments the geometry of the line by 50m and then forms points at 50 m intervals for each tow (matching the resolution of the raster).
# 3 - SDM values are extracted for each point
# 4 - SDM point values are averaged across each tow
# 5 - Data is stitched to the file containing all previous years sdm data (2001-present).
# 6 - Data is assigned a high, medium and low classification based on the sdm mean value. 

#Note: method for obtaining a length weighted mean has changed in the past due to loss of ArcGIS tools (see document Methods_SurvTows_SDM.doc for more details). The following method was adopted for the 2022 assessment. 

library(ROracle)
require(sf)
require(raster)
require(tidyverse)

options(stringsAsFactors = FALSE)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)


# ---Import Source functions ----------------------------------------------------------------------


funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}
#source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/convert.dd.dddd.r")


# ---Read in SDM and data query ----------------------------------------------------------------------

sdm <- raster("Y:/Inshore/Databases/Scallsur/SFA29BottomTypes/SDM/sdm_sfa29/w001001.adf") #UTM zone 19
#projectRaster(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#Pull survey data from database - ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set survey.year and cruise - *Note: requires single quotations within double quotations*
survey.year <- "'2026'"
cruise <- "'SFA292026'"

#Db Query:
quer2 <- paste(
  "SELECT CRUISE, TOW_NO, CRUISE||'.'||TOW_NO AS ID, start_lat, start_long, end_lat, end_long 			                ",
  "FROM SCALLSUR.SCTOWS			",
  "WHERE to_char(TOW_DATE,'yyyy')=",
  survey.year,
  "AND CRUISE =",cruise,
  sep=""
)

#Pull data 
ScallopSurv.dat <- dbGetQuery(chan, quer2)

# ---Foramtting data  ----------------------------------------------------------------------

#Convert lat/lon to DD:
ScallopSurv.dat  <- ScallopSurv.dat  %>% 
  mutate(DDSlat = convert.dd.dddd(ScallopSurv.dat $START_LAT)) %>% 
  mutate(DDSlon = convert.dd.dddd(ScallopSurv.dat $START_LONG)) %>% 
  mutate(DDElat = convert.dd.dddd(ScallopSurv.dat $END_LAT)) %>% 
  mutate(DDElon = convert.dd.dddd(ScallopSurv.dat $END_LONG))
#mutate(mid_lon = with(ScallopSurv.dat ,apply(cbind(DDElon,DDSlon),1,mean))) %>% 
#mutate(mid_lat = with(ScallopSurv.dat ,apply(cbind(DDElat,DDSlat),1,mean)))


#Move start and end coords into same column
ScallopSurv.start <- ScallopSurv.dat  %>% 
  dplyr::select(-DDElon,-DDElat) %>% #remove end coords
  dplyr::rename(LAT = DDSlat) %>% 
  dplyr::rename(LONG = DDSlon) %>% 
  mutate(POSITION = "START")

ScallopSurv.end <- ScallopSurv.dat  %>% 
  dplyr::select(-DDSlon,-DDSlat) %>% #remove Start coords
  dplyr::rename(LAT = DDElat) %>% 
  dplyr::rename(LONG = DDElon) %>% 
  mutate(POSITION = "END")

ScallopSurv <- rbind(ScallopSurv.start, ScallopSurv.end) %>% 
  arrange(TOW_NO)

#duplicate lat lon columns: (To keep coord columns after converting to sf)
ScallopSurv$latitude <- ScallopSurv$LAT
ScallopSurv$longitude <- ScallopSurv$LONG

#Convert dataframe to sf points to lines:
ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 32619) %>% #Convert to utm zone 19
  group_by(TOW_NO) %>%
  dplyr::summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>%
  st_segmentize(units::set_units(50,m)) %>% #Segment line geometry by 50m (i.e. the resolution of the sdm layer)
  st_cast("POINT") #convert each geometry to point (~800m/50m = ~16 points per tow)

mapview::mapview(ScallopSurv.sf)+
  mapview::mapview(sdm)

#####################################################################################################################
#Alternative methods:
#Leave as line and extract with weight = TRUE argument to produce mean value for the line (presumably weighting it by length over each cell).
#ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("longitude", "latitude"), crs = 4326) %>% 
#  st_transform(crs = 32619) %>% #Convert to utm zone 19
#  group_by(TOW_NO) %>%
#  summarize(do_union=FALSE) %>% 
#  st_cast("LINESTRING")

#sdm.val <- raster::extract(sdm, ScallopSurv.sf, weight = TRUE, normalizeWeights = TRUE, fun = "mean") #remove fun="mean" to see segemented values for each tow. This method does not give a weighted value.

#OR 

#Convert to polygon by adding small buffer to line (1m) which produces 
#ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("longitude", "latitude"), crs = 4326) %>% 
#  st_transform(crs = 32619) %>% #Convert to utm zone 19
#  group_by(TOW_NO) %>%
#  summarize(do_union=FALSE) %>% 
#  st_cast("LINESTRING") %>% 
#  st_buffer(0.5, endCapStyle = "FLAT") #Now a polygon with width of 

#sdm.val <- raster::extract(sdm, ScallopSurv.sf, weight = TRUE, normalizeWeights = TRUE)
#sdm.val[[1]]

#End of alternative methods for extracting (with weighted values). If used, script below will need to be adjusted
#####################################################################################################################

# ---Extract raster data at point locations  ----------------------------------------------------------------------

sdm.val <- raster::extract(sdm, ScallopSurv.sf, sp = TRUE)


# --- Boxplot to check sdm ranges  ----------------------------------------------------------------------

#Plot to check ranges for each tow
p <- ggplot(sdm.val@data, aes(as.factor(TOW_NO), sdm_sfa29))
p + geom_boxplot()+
  xlab("Tow number")+
  ylab("SDM probability")+
  geom_hline(yintercept=0.3, linetype="dashed", color = "red")+
  geom_hline(yintercept=0.6, linetype="dashed", color = "blue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot to check any tows if needed
check <- ScallopSurv.sf %>% filter(TOW_NO == 40)
mapview::mapview(check)+ #Change TOW_NO
  mapview::mapview(sdm)


# ---Mean of sdm values for each tow  ----------------------------------------------------------------------

#Take the average of the sdm values for each tow
sdm.mean <- sdm.val@data %>% 
  group_by(TOW_NO) %>%
  summarize(sdmval_LWM = mean(sdm_sfa29, na.rm = TRUE))

# ---formatting data to match previous years ----------------------------------------------------------------------

sdmtows <- merge(sdm.mean, ScallopSurv.dat, by = "TOW_NO")

#Calculate tow length (assumes straight line tow)
towlength <- st_as_sf(ScallopSurv, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 32619) %>% #Convert to utm zone 19
  group_by(TOW_NO) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  mutate(length = sf::st_length(geometry)) %>% 
  mutate(length = unclass(length))

#merge to sdmtows dataframe
sdmtows <- merge(sdmtows, towlength, by = "TOW_NO")

sdmtows <- sdmtows %>% 
  add_column(sdmval_CNT = NA) %>% 
  dplyr::select(CRUISE, TOW_NO, ET_ID = ID, length, sdmval_LWM, sdmval_CNT)

# Assign SDM level (low,med,high) based on SDM length weighted mean values (mval_LWM)
sdmtows$SDM <- NA
sdmtows$SDM[sdmtows$sdmval_LWM < 0.3] <- "low"
sdmtows$SDM[sdmtows$sdmval_LWM >= 0.3 & sdmtows$sdmval_LWM < 0.6] <- "med"
sdmtows$SDM[sdmtows$sdmval_LWM >= 0.6] <- "high"

# ---Bring in the previous survey tow details and stitch them all together ----------------------------------------------------------------------

sdmtows.old <- read.csv("Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv")
sdmtows.updt <- rbind(sdmtows.old,sdmtows)
sdmtows.updt <-sdmtows.updt %>% arrange(CRUISE, TOW_NO)
summary(sdmtows.updt)

# ---Save updated dataframe ----------------------------------------------------------------------

write.csv(sdmtows.updt, "Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv", row.names = F)

#save single year to archive folder for records
#write.csv(sdmtows, paste0("Y:/Inshore/SFA29/ScalSurv_SDM/Archived/SFA29",survey.year,"Tows_SDM.csv")) #save out file with year


#########################################################################################################
#Previous method where each year had its own individual file and would be stitched together within the SHF script.

# Bring in the survey tow details and stitch them all together
#sdmtows.old <- read.csv("Y:/INSHORE SCALLOP/SFA29/2022/Assessment/Data/SurveyIndices/ScalSurv_SDM/SFA292001to2010_SDM.csv")
#sdmtows <- NULL
#for(i in c(2011:2019)) sdmtows[[as.character(i)]] <- read.csv(paste0("Y:/INSHORE SCALLOP/SFA29/2022/Assessment/Data/SurveyIndices/ScalSurv_SDM/SFA29",i,"Tows_SDM.csv"))
#c(2011:2019, 2021:surveyyear)  - replace above when have 2021 data to read in 
#sdmtows <- do.call("rbind",sdmtows.old)
#sdmtows <- rbind(sdmtows.old,sdmtows)

#sdmtows <- sdmtows %>% arrange(CRUISE, TOW_NO)

#########################################################################################################

  










