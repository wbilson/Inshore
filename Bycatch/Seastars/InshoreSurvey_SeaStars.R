
library(ROracle)
library(tidyverse)
library(sf)


uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)
#uid <- un.englishg
#pwd <- pw.englishg

#set year 
survey.year <- 2026 #survey year

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# ----Import Source functions and polygons---------------------------------------------------------------------

#### Import Mar-scal functions 
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  #"https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
) 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#### Import Mar-scal shapefiles

# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
mgmt.zones.detailed <- st_read(paste0(temp2, "/Scallop_Strata.shp")) %>% 
  filter(Scal_Area != "SFA29W")

mgmt.zones <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/scallop_management_zones/ScallopFishingAreas_2024.shp") %>% 
  filter(str_starts(Area_Name, "SPA")) %>%  
  st_transform(crs = 32620)

bathy_sf <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/bathymetry/bathymetry_15m.shp") 

Land <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/Atl_region_land.shp") %>% 
  st_transform(crs = 32620) 

# -----------------------------Import SHF data (live and dead)--------------------------------------------

##.. LIVE ..##
## NOTE: For BoF plots keep strata_id call included; for document remove strata_id limits
#         *Query reads in ALL strata and ALL tow types - this is not equivalent to what is used in population models*

#Db Query:
quer2 <- paste(
  "SELECT * 			                ",
  "	 FROM SCALLSUR.sctows             ",
  "WHERE strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)


#If ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)

ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% 
  filter(year == 2026)

# 1. Filter rows containing "sea stars"  #Comments for sea stars were entered consitently.
ScallopSurv.SStars  <- ScallopSurv %>% 
  filter(str_detect(COMMENTS, "sea stars"))

str(ScallopSurv.SStars)
#'data.frame':	3 obs. of  33 variables:
#$ CRUISE          : chr  "BF2026" "BF2026" "BF2026"
#$ TOW_NO          : int  43 44 56


ScallopSurv.SStars.sf <- st_as_sf(ScallopSurv.SStars, coords = c("lon", "lat"), crs = 4326)

mapview::mapview(ScallopSurv.SStars.sf)

