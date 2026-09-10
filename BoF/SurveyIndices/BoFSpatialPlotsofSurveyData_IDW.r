###................................................................................###
###                         Spatial Figures (using IDW)                            ###
###                           Full BoF and Approaches                              ###
###                           G. English & J. Sameoto                              ###
###                                  March 2025                                    ###
###       (on https://github.com/Mar-scal/Inshore/tree/main/SurveyIndices)         ###
###................................................................................###

# Spatial figures of commercial, recruit and pre-recruit scallop sizes of Survey Density, Survey Biomass, Condition, Meat count and Clappers for BoF: 
#Full Bay
#SPA 1A
#SPA1B
#SPA 3
#SPA4 and 5
#and SPA 6

#disable scientific notation or else it will show on legend for figures 
options(scipen = 999)
options(stringsAsFactors = FALSE)

# required packages
require (CircStats)
require (TeachingDemos)
require (PBSmapping)
require (akima)
require (gstat)
require (fields)
require (splancs)
require (RColorBrewer)
require (spatstat)
require (RPMG)
require (rmapshaper)
require (lubridate)
require (tidyverse)
require (sf)
require (maptools)
require (forcats)
library (ROracle)
require(ggspatial)
require(concaveman)
library(scales)
# install.packages("devtools")
# devtools::install_github("ropensci/rnaturalearthhires")



# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)
#uid <- un.englishg
#pwd <- pw.englishg

#set year 
survey.year <- 2026 #survey year
assessmentyear <- 2026 #year in which you are providing advice for- determines where to save files to
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

#set up directory to save plot
saveplot.dir <- paste0(path.directory,assessmentyear,"/Assessment/Figures/")

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
  "FROM scallsur.scliveres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)


#If ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)
ScallopSurv  <- ScallopSurv[,1:51]

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commercial, recruit and prerecruit data columns
ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>% #standardize number per tow to numbers per m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60
  
table(ScallopSurv$year)
summary(ScallopSurv) #check data


##.. DEAD ..##
#NOTE:  *Query reads in ALL strata and ALL tow types - this is not equivalent to what is used in population models*

#Db Query:
quer3 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scdeadres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)

ScallopSurv.dead <- dbGetQuery(chan, quer3)
ScallopSurv.dead   <- ScallopSurv.dead[,1:51]

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commercial, recruit and prerecruit data columns
ScallopSurv.dead <- ScallopSurv.dead %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60

# -------------------------------Calculating proportion of clappers------------------------------------------

live <- ScallopSurv %>% 
  dplyr::select(CRUISE, STRATA_ID, tow, year, lat, lon, tot, ID, com, rec, pre)

dead <- ScallopSurv.dead %>% 
  dplyr::select(CRUISE, STRATA_ID, tow, year, lat, lon, tot, ID, com, rec, pre)

prop.clappers <- merge(live,dead,by="ID") %>% 
  mutate(prop.dead.com = com.y/(com.x + com.y)) %>% #number of commercial sized clappers relative to total number of commercial size live and dead combined
  mutate(prop.dead.rec = rec.y/(rec.x + rec.y)) %>% #number of recruit sized clappers relative to total number of recruit size live and dead combined
  mutate_at(vars(prop.dead.com:prop.dead.rec), ~replace(., is.na(.), 0)) %>% 
  dplyr::select(ID, CRUISE = CRUISE.x, STRATA_ID = STRATA_ID.x, tow = tow.x, year = year.x, lat = lat.x, lon = lon.x, prop.dead.com, prop.dead.rec)

#	write.csv(prop.clappers, file="Y:/Inshore/BoF/dataoutput/PropClappers.csv")  #Export if needed 

# --------------------------------Import Biomass per tow data-----------------------------------------

# this is output from the meat weight/shell height modelling !NB: not all years needed depending on what you want to show
#code for reading in multiple csvs at once and combining into one dataframe from D.Keith (2015)

max.yr <- assessmentyear#max(na.omit(ScallopSurv$year))
Year <- seq((max.yr-4),max.yr)
Year <- Year[! Year %in% 2020] #No 2020 data - remove from data query.
num.years <- length(Year)

#SPA1A1B4and5
BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",Year[i],".csv",sep=""), header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

#SPA3
BIliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/BIliveweight",Year[i],".csv",sep=""), header=T)
  BIliveweight <- rbind(BIliveweight,temp)
}

#SPA6
GMliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/GMliveweight",Year[i],".csv",sep=""), header=T)
  GMliveweight <- rbind(GMliveweight,temp)
}

liveweight <- rbind(if(exists("BFliveweight")) BFliveweight, if(exists("BIliveweight")) BIliveweight, if(exists("GMliveweight")) GMliveweight) #Combine SPA condition data together if data is available

#check data
head(liveweight)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and biomass for commercial, recruit and prerecruit data columns
ScallopSurv.kg <- liveweight %>%
  dplyr::rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com.bm = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 /1000
  mutate(rec.bm = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75 /1000
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60 /1000
  
# For Meat Count plot:
ScallopSurv.mtcnt <- ScallopSurv.kg %>% 
  dplyr::select(ID, STRATA_ID, year, lat, lon, com.bm) %>% 
  merge(ScallopSurv %>% dplyr::select(ID, com), by = "ID") %>% 
  mutate(meat.count = (0.5/(com.bm/com))) %>% 
  filter(!is.na(meat.count))

# --------------------------------Load Condition data for survey year -----------------------------------------

#Condition data is read by looking for files with "ConditionforMap" in the file name from the respective SFAs located within the current assessment year folder 
#(e.g. Y:\INSHORE SCALLOP\BoF\2020\Assessment\Data\SurveyIndices\SPA1A1B4and5\BFConditionforMap2013.csv). This will pull in the data for each year that a condition file was previously generated and then can be used for assessing multiple years.

#SPA1A1B4and5
con.dat.list <- list.files(path = paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5"), pattern = "^BFConditionforMap")
BF.con.dat <- list()
for(i in 1:length(con.dat.list)){
  BF.con.dat[[i]] <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/",con.dat.list[[i]]))
}

BF.con.dat <- bind_rows(BF.con.dat) 
BF.con.dat <- BF.con.dat %>% #Combine the condition data from files that are found
  mutate(CRUISE = paste0("BF", BF.con.dat$YEAR))#Add Cruise information

#SPA3
con.dat.list <- list.files(path = paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3"), pattern = "^BIConditionforMap")
BI.con.dat <- list()
for(i in 1:length(con.dat.list)){
  BI.con.dat[[i]] <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/",con.dat.list[[i]]))
}

BI.con.dat <- bind_rows(BI.con.dat)
BI.con.dat <- BI.con.dat %>% #Combine the condition data from files that are found
  mutate(CRUISE = paste0("BI", BI.con.dat$YEAR)) #Add Cruise information


#SPA6
con.dat.list <- list.files(path = paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6"), pattern = "^GMConditionforMap")
GM.con.dat <- list()
for(i in 1:length(con.dat.list)){
  GM.con.dat[[i]] <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/",con.dat.list[[i]]))
}

GM.con.dat <- bind_rows(GM.con.dat) 
GM.con.dat <- GM.con.dat %>% #Combine the condition data from files that are found
  mutate(CRUISE = paste0("GM", GM.con.dat$YEAR)) #Add Cruise information

#Now combine the Cruise dataframes together
con.dat <- rbind(if(exists("BF.con.dat")) BF.con.dat, if(exists("BI.con.dat")) BI.con.dat, if(exists("GM.con.dat")) GM.con.dat) #Combine SPA condition data together if data is available

#check data structure
head(con.dat)
table(con.dat$CRUISE)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), and ID (cruise.tow#) columns
con.dat <- con.dat %>%
  dplyr::rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>%  #Convert to DD
  rename(tow = TOW_NO) %>% 
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) #No CRUISE column in con.dat - ID column is required for contour.gen function


# ----------- Making a custom boundary for IDW analysis based off survey tows ---------------

#create shapefile out of survey tow locations. It will get re-written during IDW, but just a placeholder for function below.
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

# Define groups so that BoF, SPA3, and GM can be isolated
group_list <- list(
  BoF = c("1", "4", "5"),
  SPA3 = c("3"),
  SPA6 = c("6A", "6B", "6C")
)

# Function to process each group
create_polygon <- function(group_ids, group_name) {
  group_points <- Surv.sf[Surv.sf$MGT_AREA_ID %in% group_ids, ]
  
  if (nrow(group_points) > 2) {  # At least 3 points needed for a polygon
    hull <- concaveman(group_points)  # Create concave hull
    
    if (!is.null(hull) && nrow(hull) > 0) {  # Check if hull was created
      buffered <- st_buffer(hull, dist = 5000)  # Apply 5 km buffer
      
      if (!is.null(buffered) && nrow(buffered) > 0) {  # Ensure valid geometry
        return(st_sf(group = group_name, geometry = st_geometry(buffered)))  # Ensure geometry column
      }
    }
  }
  return(NULL)  # Return NULL if no valid polygon was created
}

# Apply function to each group and remove NULLs
polygons <- lapply(names(group_list), function(name) create_polygon(group_list[[name]], name))
idw_sf<- do.call(rbind, polygons[!sapply(polygons, is.null)])  # Remove NULLs and combine

# Ensure correct CRS
st_crs(idw_sf) <- 32620

# -------------- Set consistent plot objects/themes ------------------------------

#Set standard layers: bathymetry on bottom layer, and land, survey tows, etc above IDW layer
bathy <-   ggplot() + #goes before IDW layer
            geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
            coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)
  
# Function to generate map layers to go after IDW layer
p <- function(mgmt_zone, surv_sf, land, scale_location = "tl", arrow_location = "tl", legend_position) {
  list(
    # Management Zones 
    mgmt_zone,
    # # Unlist to allow multiple styles (flattens lists)
    # unlist(mgmt_zone_styles, recursive = FALSE), 
    
    # Survey Points (white outline, black fill)
    geom_sf(data = surv_sf, color = "white", fill = "black", shape = 21, size = 1.5),
    
    # Land (grey fill)
    geom_sf(data = land, fill = "grey60"),
    
    # Custom X and Y axis formatting (easier for french translation)
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)),
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)),
    
    # Set theme for the plot
    theme(plot.title = element_text(size = 14, hjust = 0.5), # Plot title size and position
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold"), 
          legend.text = element_text(size = 12),
          legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), # Legend background color and transparency
          legend.box.margin = margin(6, 8, 6, 8),
          legend.position = legend_position, # Dynamic legend position
          legend.justification = c(0.5, 0.5)), # Keep legend within bounds, 
    
    # Add scale bar with selectable location
    annotation_scale(
      location = scale_location, width_hint = 0.5, 
      pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")
    ),
    
    # Add north arrow with selectable location
    annotation_north_arrow(
      location = arrow_location, which_north = "true", 
      height = unit(1.25, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),
      style = north_arrow_fancy_orienteering
    )
  )
}

#creating list with legend positions so that you can call for "spa" legend position. Any edits to legend positions are done here
legend_positions <- list(
  "spa1"  = c(0.85, 0.35),
  "spa1b" = c(0.85, 0.25),
  "spa3"  = c(0.82, 0.44),
  "spa4"  = c(0.88, 0.23),
  "spa6"  = c(0.15, 0.62)
)

#management zone list
mgmt_zone_styles <- list(
  "BoF"  = geom_sf(data = mgmt.zones.detailed %>% filter(!STRATA_ID %in% c(26,57,90,91,92,93)), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  "spa3" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(92,93)), color = "red", linewidth = 0.3, fill = NA, linetype = "dashed"),
  "spa6" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(90,91)) %>% st_union() %>% st_as_sf(), color = "red", fill = NA, linewidth = 0.3, linetype = "dashed"),
  # "spa6in" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 90), color = "red", fill = NA, linewidth = 0.2, linetype = "solid"),
  # "spa6out" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 91), color = "black", fill = NA, linewidth = 0.2, linetype = "solid"),
  "Gen"  = geom_sf(data = mgmt.zones, color = "black", fill = NA, linewidth = 0.3, linetype = "solid")
)

#coordinates for each SPA
coord_ranges = list(
  "BoF" = list(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80)),
  "spa1a" = list(xlim = c(-66.40,-64.70), ylim = c(44.37,45.30)),
  "spa1b" = list(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70)),
  "spa3"  = list(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60)),
  "spa4"  = list(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96)),
  "spa6"  = list(xlim = c(-67.5, -66.35), ylim = c(44.4, 45.2)),
  "BoFall" = list(xlim = c(-67.28,-64.4), ylim = c(43.60, 45.62))
  ) 
  
# ------------------------------COMMERCIAL SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com) #commercial size
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nabundance \n(N/Tow)", limits = c(0,1900), breaks = c(0,50,1000,1500)) +  #oob = scales::squish, max(preds_idw2$prediction))) + #default is max value, change if needed
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")
  
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nabundance \n(N/Tow)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + #default is max value, change if needed
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nabundance \n(N/Tow)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + #default is max value, change if needed
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")
  
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nabundance \n(N/Tow)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + #default is max value, change if needed
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Density (>= 80mm)"), 
       x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nabundance \n(N/Tow)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + #default is max value, change if needed 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nabundance \n(N/Tow)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + #default is max value, change if needed
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")
  
#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com) #commercial size
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)

#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,35), breaks = c(0,5,10,20,30)) +# oob = scales::squish max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,35), breaks = c(0,5,10,20,30)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,35), breaks = c(0,5,10,20,30)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,35), breaks = c(0,5,10,20,30)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,35), breaks = c(0,5,10,20,30)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,35), breaks = c(0,5,10,20,30)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nbiomass \n(kg/Tow)", limits = c(0,35), breaks = c(0,5,10,20,30)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CONDITION PLOTS-----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(con.dat,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_condition <- log(Surv.sf$Condition)
Surv.sf$log_condition[which(Surv.sf$log_condition==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_condition~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Condition)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", begin = 0.5, end = 1, trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", begin = 0.5, end = 1, trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", begin = 0.5, end = 1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", begin = 0.5, end = 1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", begin = 0.5, end = 1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Condition"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", begin = 0.5, end = 1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H", begin = 0.5, end = 1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Condition',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----MEAT COUNT -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.mtcnt,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_mtct <- log(Surv.sf$meat.count)
Surv.sf$log_mtct[which(Surv.sf$log_mtct==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_mtct~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$meat.count)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,110), breaks = c(10,25,50,75,100) ) +#max(preds_idw2$prediction))) oob = scales::squish + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,110), breaks = c(10,25,50,75,100)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,110), breaks = c(10,25,50,75,100)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,110), breaks = c(10,25,50,75,100)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,110), breaks = c(10,25,50,75,100)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Meat Count"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,110), breaks = c(10,25,50,75,100)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "Meat count \n(per 500g)", limits = c(10,110), breaks = c(10,25,50,75,100)) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_MeatCount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com)
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(prop.clappers,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop.dead.com <- log(Surv.sf$prop.dead.com)
Surv.sf$log_prop.dead.com[which(Surv.sf$log_prop.dead.com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop.dead.com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop.dead.com)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Commercial \nclappers \n(Proportion)", limits = c(0,1)) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec) #recruit size
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nabundance \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec) #recruit size
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

# ----BoF ALL (BF, SPA 3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) + #max(preds_idw2$prediction)
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nbiomass \n(kg/Tow)", limits = c(0,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec)
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(prop.clappers,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop.dead.rec <- log(Surv.sf$prop.dead.rec)
Surv.sf$log_prop.dead.rec[which(Surv.sf$log_prop.dead.rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop.dead.rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop.dead.rec)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Recruit \nclappers \n(Proportion)", limits = c(0,1)) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------PRE-RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre) #prerecruit size
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nabundance \n(N/Tow)", limits = c(0,125), breaks = c(0,25,50,100),oob = scales::squish) + #max(preds_idw2$prediction)
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nabundance \n(N/Tow)", limits = c(0,125), breaks = c(0,25,50,100),oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nabundance \n(N/Tow)", limits = c(0,125), breaks = c(0,25,50,100),oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nabundance \n(N/Tow)", limits = c(0,125), breaks = c(0,25,50,100),oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nabundance \n(N/Tow)", limits = c(0,125), breaks = c(0,25,50,100),oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nabundance \n(N/Tow)", limits = c(0,125), breaks = c(0,25,50,100),oob = scales::squish) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nabundance \n(N/Tow)", limits = c(0,125), breaks = c(0,25,50,100),oob = scales::squish) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre) #prerecruit size
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nbiomass \n(kg/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre)
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Pre-Recruit \nclappers \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


#### |---------------------------- end of English script -----------------------------------| ####


#---------------------------- SURVEY DISTRIBUTION PLOTS - FRENCH TRANSLATION ----------------------------------

### All English figures created above will be re-created as French figures.
### Assuming data/shapefile setup has been run, these can be run independently of English figures
### The only translations needed were as follows (update if translation is not correct):
# Commercial Abundance = Abondance commerciale
# Commercial Biomass = Biomasse commerciale
# Meat Count (per 500g) = La quantité de chair (par 500g)
# Commercial Clappers = Claquettes commerciale
# Recruit Abundance = Abondance des recrues
# Recruit Biomass = Biomasse des recrues
# Recruit Clappers = Claquettes des recrues
# Pre-Recruit Abundance = Abondance des prérecrues
# Pre-Recruit Biomass = Biomasse des prérecrues
# Pre-Recruit Clappers = Claquettes des prérecrues
# Tow = traits de chalut

## words that stay the same: Latitude, Longitude, Condition, Proportion

#set up a directory for french figures
saveplot.dir.fr <- paste0(saveplot.dir,"FrenchFigures_indicies/")

# ------------------------------COMMERCIAL SCALLOP - SURVEY DISTRIBUTION PLOTS ---------------------------------

# ----DENSITY PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com) #commercial size
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ncommerciale \n(N/traits de chalut)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ncommerciale \n(N/traits de chalut)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ncommerciale \n(N/traits de chalut)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ncommerciale \n(N/traits de chalut)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ncommerciale \n(N/traits de chalut)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ncommerciale \n(N/traits de chalut)", limits = c(0,975), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Density (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_ComDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com) #commercial size
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(kg/traits de chalut)", limits = c(0,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(kg/traits de chalut)", limits = c(0,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(kg/traits de chalut)", limits = c(0,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(kg/traits de chalut)", limits = c(0,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(kg/traits de chalut)", limits = c(0,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(kg/traits de chalut)", limits = c(0,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ncommerciale \n(kg/traits de chalut)", limits = c(0,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Biomass (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_ComBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CONDITION PLOTS-----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(con.dat,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_condition <- log(Surv.sf$Condition)
Surv.sf$log_condition[which(Surv.sf$log_condition==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_condition~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Condition)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Condition"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "G", direction = -1,  trans = "sqrt", name = "Condition (g)", limits = c(4,18), oob = scales::squish) +  #max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Condition"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_Condition',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----MEAT COUNT -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.mtcnt,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_mtct <- log(Surv.sf$meat.count)
Surv.sf$log_mtct[which(Surv.sf$log_mtct==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_mtct~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$meat.count)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,45), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,45), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,45), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,45), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,45), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Meat Count"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,45), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "B",  trans = "sqrt", name = "La quantité \nde chair \n(par 500g)", limits = c(10,45), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Meat Count"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_MeatCount',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_com <- log(Surv.sf$com)
Surv.sf$log_com[which(Surv.sf$log_com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$com)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_ComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_ComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_ComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_ComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_ComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_ComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(prop.clappers,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop.dead.com <- log(Surv.sf$prop.dead.com)
Surv.sf$log_prop.dead.com[which(Surv.sf$log_prop.dead.com==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop.dead.com~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop.dead.com)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_PropComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_PropComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_PropComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_PropComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_PropComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ncommerciale \n(Proportion)", limits = c(0,1)) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers Proportion (>= 80mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_PropComClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec) #recruit size
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Density (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_RecDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec) #recruit size
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

# ----BoF ALL (BF, SPA 3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes recrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Biomass (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_RecBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_rec <- log(Surv.sf$rec)
Surv.sf$log_rec[which(Surv.sf$log_rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$rec)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_RecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_RecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_RecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_RecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_RecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_RecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(prop.clappers,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop.dead.rec <- log(Surv.sf$prop.dead.rec)
Surv.sf$log_prop.dead.rec[which(Surv.sf$log_prop.dead.rec==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop.dead.rec~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop.dead.rec)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_PropRecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_PropRecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_PropRecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(proportion)", limits = c(0,1)) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_PropRecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(proportion)", limits = c(0,1)) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_PropRecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes recrues \n(Proportion)", limits = c(0,1)) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers Proportion (65-79mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_PropRecClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------PRE-RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre) #prerecruit size
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Abondance \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Density (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_PreDensity',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----BIOMASS PLOTS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.kg,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre) #prerecruit size
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes prérecrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_PreBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes prérecrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_PreBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes prérecrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_PreBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes prérecrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_PreBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes prérecrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_PreBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Biomasse \ndes prérecrues \n(kg/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Biomass (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_PreBiomass',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(ScallopSurv.dead,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_pre <- log(Surv.sf$pre)
Surv.sf$log_pre[which(Surv.sf$log_pre==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_pre~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$pre)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_PreClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_PreClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_PreClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_PreClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_PreClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Claquettes \ndes prérecrues \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Clappers (< 65mm)"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_PreClappers',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



##### |------------------------------------- END OF SCRIPT -----------------------------------------------| #####
