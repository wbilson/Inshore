
## OLEX LAT LONG DATA CONVERSION ##
## AND SPA AND STRATA MATCHING ##

###  VERIFY THE COORDINATES MATCH THE RECORDS IN FIELD NOTEBOOK  ##

#CONSIDERATIONS 
# - Data points for large pelagics - how are these formatted in olex output? - may be entered as "Gr?nnramme" in between tracklines
# - Tracklines that are ended during a tow and started again will need to be identified and start and end coords will need to be checked.
# - if anything is entered during the survey that are not tracklines - will need to check (i.e. notes of whales etc.)
# - check number of tows and number of track logs match
# - Can use lat long to match strata ID and SPAs to auto fill these columns.

#Data entry considerations:
#SPA6 management boundaries might not match with database - check these against field notes. These will be flagged in checks
#Other SPAs may also have this issue.

# Load libraries and functions and Strata/management shapefiles

library(data.table)
library(splitstackshape)
library(tidyverse)
library(sf)
library(rmapshaper)
library(mapview)

funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#Read in inshore boundaries
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
SPA1A <- st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% mutate(ET_ID = "1A")
SPA1B <- st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% mutate(ET_ID = "1B")
SPA2 <- st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp")) %>% mutate(ET_ID = "2")
SPA3 <- st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp")) %>% mutate(ET_ID = "3")
SPA4 <- st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp")) %>% mutate(ET_ID = "4")
SPA5 <- st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp")) %>% mutate(ET_ID = "5")
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A")
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B")
SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C")
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")

VMS_out <- st_read(paste0(temp2, "/SPA6_VMSstrata_OUT_2015.shp")) %>% mutate(ET_ID = "OUT")
VMS_in <- st_read(paste0(temp2, "/SPA6_VMSstrata_IN_2015.shp")) %>% mutate(ET_ID = "IN")

SFA29 <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% mutate(ID = seq(1,5,1)) %>%  #TO FIX IN COPY ON GITHUB (ET_ID missing so adding it here)
  mutate(ET_ID = case_when(ID == 1 ~ 41, 
                           ID == 2 ~ 42,
                           ID == 3 ~ 43,
                           ID == 4 ~ 44,
                           ID == 5 ~ 45)) %>% 
  dplyr::select(Id = ID, ET_ID) %>% st_transform(crs = 4326)

#Because SPA6A, 6B, 6D overlap - need to cut out (#TO FIX IN COPY ON GITHUB)
SPA6A <- rmapshaper::ms_erase(SPA6A,SPA6B)%>% 
  dplyr::select(Id, ET_ID) %>% 
  st_make_valid()
#plot(SPA6A)

SPA6B <- rmapshaper::ms_erase(SPA6B,SPA6D)%>% 
  dplyr::select(Id, ET_ID) %>% 
  st_make_valid()
#plot(SPA6B)

SPA_BoF <- rbind(SPA1A, SPA1B, SPA2, SPA3, SPA4, SPA5, SPA6A, SPA6B, SPA6C, SPA6D) %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

rm(SPA1A, SPA1B, SPA2, SPA3, SPA4, SPA5, SPA6A, SPA6B, SPA6C, SPA6D) #declutter environment

#STRATA FILE FROM GITHUB
# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the strata shapefile
strata <- st_read(paste0(temp2, "/PolygonSCSTRATAINFO_rm46-26-57.shp"))
###########################################################################################################

#Import olex data:

#Norwegian translation according to Google:
#Gr?nnramme - basic framework
#Navn - name
#Rute uten navn- Route without name
#Garnstart - start
#Garnstopp - stop
#Brunsirkel - brown circle (points along trackline?)

zz <- read_csv("Y:/Inshore/Survey/Olex/OLEX tow tracks/2026/20260627.gz", locale = locale(encoding = "latin1"))
names(zz) <- "Ferdig.forenklet"
#zz <- read.csv(gzfile('Y:/Inshore/Survey/OLEX tow tracks/2023/20230711.gz'))

str(zz)
zz$Ferdig.forenklet <- as.character(zz$Ferdig.forenklet)

#Split characters separated by spaces into columns.
zz <- cSplit(zz, "Ferdig.forenklet", sep = " ", type.convert = FALSE) 

#zz <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp")) %>% 
#  select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
#  mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
#  mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)

#Occasionally a "Gr?nnramme" occurs where a "Garnstopp" should be or between tow tracks. 
#RUN but Check if number of "Garnstart" == "Garnstopp"!! 
zz <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp", "Gr?nnramme")) %>% 
  dplyr::select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
  mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
  mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)

table(zz$Ferdig.forenklet_4) #missing one stop record in BI2026
View(zz)

#Select the row where the track data starts (i.e. the first "Garnstart"). Check for "Gr?nnramme".
#zz <- zz[1:nrow(zz)] #[Row# where "Garnstart" first occurs: to end of data]  #Most likely its however many stations there are, but could be more if observations were added.

#Convert decimal degrees to degrees minutes.
zz$Latitude.deg <- convert.dd.dddd(zz$Latitude, format = 'deg.min')
zz$Longitude.deg <- convert.dd.dddd(zz$Longitude, format = 'deg.min')*-1

zz.start <- zz %>% filter(Ferdig.forenklet_4 == "Garnstart") %>% 
  dplyr::rename(Start_lat = Latitude.deg) %>% 
  dplyr::rename(Start_long = Longitude.deg) %>% 
  dplyr::select(Start_lat, Start_long, Start_lat_dec = Latitude, Start_long_dec = Longitude)

#If required to have 3 decimal places - values copied down in fieldbook are not rounded so run to get 3 decimal places not rounded:
zz.start$Start_lat <- trunc(zz.start$Start_lat*10^3)/10^3
zz.start$Start_long <- trunc(zz.start$Start_long*10^3)/10^3

zz.end <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstopp")) %>% #"Gr?nnramme"
  dplyr::rename(End_lat = Latitude.deg) %>% 
  dplyr::rename(End_long = Longitude.deg) %>% 
  dplyr::select(End_lat, End_long, End_lat_dec = Latitude, End_long_dec = Longitude)

zz.end$End_lat <- trunc(zz.end$End_lat*10^3)/10^3
zz.end$End_long <- trunc(zz.end$End_long*10^3)/10^3

######################################
#EDITS TO BI2026:
#rm first row
zz.start <- zz.start[-1, ]
zz.end <- zz.end[-1, ]

#Tow 7 was split into two.. remove second start and first end associated with tow 7 (checked logbook to confirm)
zz.start <- zz.start[-8, ]
zz.end <- zz.end[-7, ]

#Find the garnstart where there is no stop record
which(zz.start$Start_lat == 4411.055)# row/tow 60
#need to add end coords for tow 60 - from logbook
#End_long == 6631.244, End_long_dec == -66.5207333333
#End_lat == 4410.705, End_lat_dec == 44.1784166667
zz.end <- zz.end %>% 
  add_row(End_lat = 4410.705, End_long = 6631.244, End_lat_dec = 44.17842, End_long_dec = -66.52073, .before = 60)

#End of Edits to BI2026
#######################################

coords <- cbind(zz.start, zz.end) %>% 
  mutate(ID = seq(1,nrow(zz.start),1))  #NOTE - ID IS NOT TOW NUMBER (although it could lineup). it is only used to compare records when matching strata #s and SPAs.

# Match Strata ID and SPA # to lat and long data (use start lat long)
coords.sf <- st_as_sf(coords, coords = c("Start_long_dec", "Start_lat_dec"), crs = 4326) %>%
  mutate(ID = as.factor(ID))
plot(coords.sf)

coords.sf.end <- st_as_sf(coords, coords = c("End_long_dec", "End_lat_dec"), crs = 4326)
#plot(coords.sf.end)

#BOF - Dont run for SFA29W
strata.match <- st_intersection(strata, coords.sf)
strata.match <- strata.match %>% 
  dplyr::select(STRATA_ID, ID) %>%
  mutate(ID = as.factor(ID))
#BOF SPA - Dont run for SFA29W
spa.match <- st_intersection(SPA_BoF , coords.sf)
spa.match <- spa.match %>% dplyr::select(ET_ID, ID) %>%
  mutate(ID = as.factor(ID))

#BOF - All points should have strata, and spa matches. If there are discrepancies - check 
#may have to run sf::sf_use_s2(FALSE)
coords.sf <- coords.sf %>% 
  st_join(spa.match, by = "ID", suffix = c("", ".y")) %>% 
  st_join(strata.match, by = "ID", suffix = c("", ".y")) %>% 
  dplyr::select(ID, Start_lat, Start_long, End_lat, End_long, ET_ID, STRATA_ID)

#SFA29W
#strata.match <- st_intersection(SFA29 , coords.sf)
#strata.match <- strata.match %>% dplyr::select(ET_ID, ID)

#SFA29W - All points should have strata, and spa matches. If there are discrepancies - check 
#coords.sf <- coords.sf %>% 
#  st_join(strata.match,by = "ID", suffix = c("", ".y")) %>% 
#  dplyr::select(ID, Start_lat, Start_long, End_lat, End_long, ET_ID)


##########################################################
#plot to check

mapview::mapview(coords.sf) + #%>% filter(ID %in% c(96,98)) #option to filter out specific points
  #mapview::mapview(SFA29)
  #mapview::mapview(coords.sf.end) +
  #mapview::mapview(strata) +
  mapview::mapview(SPA_BoF)#+
  #mapview::mapview(VMS_out, col.regions=list("red"))+
  #mapview::mapview(VMS_in, col.regions=list("red"))

#Save for copying into tow.csv

coords.sf <- coords.sf%>% 
  st_drop_geometry()

write.csv(coords.sf, "Y:/Inshore/Survey/Olex/OLEX tow tracks/Olex-latlong_conversion/BI2026_coords_check.csv")

###########################################################################################################

#check file
tow <- read.csv("Y:/Inshore/Survey/2023/DataEntry/GM2023/GM2023tow_CONVERTED.csv")

#Convert decimal degrees to decimal minutes seconds.
tow$Start_Latitude <- convert.dd.dddd(tow$Start_lat, format = 'dec.deg')
tow$Start_Longitude <- convert.dd.dddd(tow$Start_long, format = 'dec.deg')*-1

tow$End_Latitude <- convert.dd.dddd(tow$End_lat, format = 'dec.deg')
tow$End_Longitude <- convert.dd.dddd(tow$End_long, format = 'dec.deg')*-1

tow.start.sf <- st_as_sf(tow, coords = c("Start_Longitude", "Start_Latitude"), crs = 4326)

tow.end.sf <- st_as_sf(tow, coords = c("End_Longitude", "End_Latitude"), crs = 4326)

mapview::mapview(tow.start.sf %>% filter(Oracle.tow..%in% c(2,15,24,32,37,53,61,71,73,78,82,90,93,107,110,112))) + 
  mapview::mapview(VMS_in) +
  mapview::mapview(VMS_out)

####################################################################################


 