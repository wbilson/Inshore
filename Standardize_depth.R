
#R standardize depth workflow (used from 2021 on wards). Prior to 2021, ArcGIS workflow was used.
#modified in 2023 to us Terra package instead of Raster package (depreciated).

library(ROracle)
require(sf)
require(terra)
require(dplyr)
library(ggplot2)

options(stringsAsFactors = FALSE)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#### Import Source functions####

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


#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#set survey.year and cruise - *Note: requires single quotations within double quotations*
survey.year <- "'2026'"
cruise <- "'BF2026'"
#appendingfile_year <- "2021" # for importing the current spreadsheet to append to.
#updatefile_year <- "2021" #For saving file

#Db Query:
quer2 <- paste(
  "SELECT CRUISE, TOW_NO, CRUISE||'.'||TOW_NO AS ID, start_lat, start_long, end_lat, end_long 			                ",
  "FROM SCALLSUR.SCTOWS			",
  "WHERE to_char(TOW_DATE,'yyyy')=",
  survey.year,
  "AND CRUISE =",cruise,
  sep=""
)


#Pull data using ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)


#Convert lat/lon to DD:
ScallopSurv$lat <- convert.dd.dddd(ScallopSurv$START_LAT)
ScallopSurv$lon <- convert.dd.dddd(ScallopSurv$START_LONG)
#duplicate lat lon columns: (To keep consitent with previous records)
ScallopSurv$DDSlat <- ScallopSurv$lat
ScallopSurv$DDSlon <- ScallopSurv$lon 

table(ScallopSurv$CRUISE)

#added Jan 2020 
#calculate mid point assuming staright line distance 
# This makes sure that ALL the data have the lat/long calculated in the same way
#ScallopSurv$mid.lon <- with(ScallopSurv,apply(cbind(DDElon,DDSlon),1,mean))
#ScallopSurv$mid.lat <- with(ScallopSurv,apply(cbind(DDElat,DDSlat),1,mean))
#ScallopSurv$mid.lat <- convert.dd.dddd(ScallopSurv$mid.lat)
#ScallopSurv$mid.lon <- convert.dd.dddd(ScallopSurv$mid.lon)


#Read in Bathy (with Terra):  #Former process involved importing raster (UTM zone 20 into Arcmap, and extracting imported points from database. ArcMap converts data points to UTM zone 20 in order to extract).

## As of 2024; using DEM for all inshore areas including SFA 29W - bc in 2024 did some sampling outside MBES domain; use moving forward. 

#---- For BoF and approaches areas ----
bathy <- rast("Y:/Inshore/Assessment/StandardDepth/ScotianShelfDEM_Olex/mdem_olex/w001001.adf")
crs(bathy)<- "epsg:32620"
ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon", "lat"), crs = 4326) |> 
      st_transform(crs = 32620) #Convert to utm zone 20 to match bathy.
#extract olex depth values
olex.depth <- terra::extract(bathy, vect(ScallopSurv.sf))
#Append depth to survey data
ScallopSurv.dpth <- cbind(ScallopSurv.sf, olex.depth) %>% 
      mutate(FID = row_number()) |>  dplyr::select(FID, CRUISE, TOW_NO, ID, START_LAT, START_LONG, DDSlat, DDSlon, RASTERVALU = mdem_olex) %>%  #sort to match towsdd_Std column formatting
      st_set_geometry(NULL) #removes geometry

#Load previous towsdd_stdDepth.csv file to append to.
towsdd <- read.csv(paste0("Y:/Inshore/Assessment/StandardDepth/towsdd_StdDepth.csv"))
table(towsdd$CRUISE)
dim(towsdd)

#Appending to towsdd
towsdd.updt <- rbind(towsdd, ScallopSurv.dpth)
dim(towsdd.updt)
table(towsdd.updt$CRUISE)

#check if increase in tows in what expect 
dim(towsdd.updt)[1] - dim(towsdd)[1]  

#Check values and plot if necessary
summary(towsdd.updt)
#mapview::mapview(ScallopSurv.sf)+ # %>% filter(TOW_NO %in% c(269,270,272)))
#  mapview::mapview(bathy)

#Save
write.csv(towsdd.updt, "Y:/Inshore/Assessment/StandardDepth/towsdd_StdDepth.csv", row.names = FALSE)



#---- For SFA 29 ; note in 2024 surveyed outside MBES area; thus use regular DEM to get depths; compare to past years, use this DEM moving forward -----
bathy <- rast("Y:/Inshore/Assessment/StandardDepth/ScotianShelfDEM_Olex/mdem_olex/w001001.adf")
crs(bathy)<- "epsg:32620"
ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon", "lat"), crs = 4326) |> 
  st_transform(crs = 32620) #Convert to utm zone 20 to match bathy.
#extract olex depth values
olex.depth <- terra::extract(bathy, vect(ScallopSurv.sf))
#Append depth to survey data
ScallopSurv.dpth <- cbind(ScallopSurv.sf, olex.depth) %>% 
  mutate(FID = row_number()) |>  dplyr::select(FID, CRUISE, TOW_NO, ID, START_LAT, START_LONG, DDSlat, DDSlon, RASTERVALU = mdem_olex) %>%  #sort to match towsdd_Std column formatting
  st_set_geometry(NULL) #removes geometry


#sfa292024.depth.MBES <- ScallopSurv.dpth
MBES <- sfa292024.depth.MBES %>% select(ID, mbes.value = RASTERVALU) ##using 29W MBES data 

DEM <- ScallopSurv.dpth %>% select(ID, dem.value = RASTERVALU) ## Using DEM data 

compare <- merge(MBES, DEM, by = c("ID"))

compare$diff <- compare$mbes.value - compare$dem.value ### dem values ~ 2 m deeper than MBES data for SFA 29 2024; change script to use DEM moving forward. Update all of values to new DEM when run next full assessment for 29W 

ggplot(data = compare, aes(x = diff)) + 
  geom_histogram()



#*make copy manually and add year to name - move file to Archived folder under Y:/Inshore/StandardDepth **
# do at end of survey season when year's surveys are complete # 

###################################################################################################################
### Checks - used to run older tows to see if R script workflow generates the same values as ArcGIS workflow     ##
###################################################################################################################
towsdd.test <- towsdd %>% 
  filter(CRUISE == "BF2006") %>% ## Will need to change CRUISE if checking other years/cruises.
  arrange(TOW_NO)

#check depth values
format(round(towsdd.test$RASTERVALU,5), nsmall = 5) == format(round(ScallopSurv.dpth$RASTERVALU,5), nsmall = 5) #Will return false without reducing sigfigs... see minor difference without rounding in calculation below.
towsdd.test$diff <- towsdd.test$RASTERVALU - ScallopSurv.dpth$RASTERVALU
plot(abs(towsdd.test$diff))

#check latitude
format(round(towsdd.test$DDSlat,7), nsmall = 7) == format(round(ScallopSurv.dpth$DDSlat,7), nsmall = 7)
towsdd.test$diff <- towsdd.test$DDSlat - ScallopSurv.dpth$DDSlat
plot(abs(towsdd.test$diff))
#################################################################################################