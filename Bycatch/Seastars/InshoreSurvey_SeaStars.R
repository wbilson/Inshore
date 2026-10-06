
library(ROracle)
library(tidyverse)
library(sf)
library(ggspatial)
library(ggstar)


uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

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
  filter(Scal_Area != "SFA29W") %>%  
  st_transform(crs = 4326)

mgmt.zones <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/scallop_management_zones/ScallopFishingAreas_2024.shp") %>% 
  filter(str_starts(Area_Name, "SPA")) %>%  
  st_transform(crs = 4326)

bathy_sf <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/bathymetry/bathymetry_15m.shp") 

Land <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/Atl_region_land.shp") %>% 
  st_transform(crs = 4326) 

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
  mutate(slat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(slon = convert.dd.dddd(START_LONG)) %>%
  filter(year == 2026)

# 1. Filter rows containing "sea stars"  #Comments for sea stars were entered consitently.
ScallopSurv.SStars  <- ScallopSurv %>% 
  filter(str_detect(COMMENTS, "sea stars"))

str(ScallopSurv.SStars)
#'data.frame':	3 obs. of  33 variables:
#$ CRUISE          : chr  "BF2026" "BF2026" "BF2026"
#$ TOW_NO          : int  43 44 56


ScallopSurv.SStars.sf <- st_as_sf(ScallopSurv.SStars, coords = c("lon", "lat"), crs = 4326) %>% 
  add_column(Seastar_Tows = c("", "", ""))

sstar.spatial <- ggplot() + #Plot survey data and format figure.
  #geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
  geom_sf(data = mgmt.zones.detailed, color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid") +
  geom_sf(data = Land, fill = "grey60") +
  geom_star(data = ScallopSurv.SStars.sf, aes(x = slon, y = slat, starshape = Seastar_Tows, fill = Seastar_Tows), size = 4) +
 #scale_starshape_manual(values = c(1)) + # 1 is the default 5-point star
  coord_sf(xlim = c(-67.5, -64.5), ylim = c(43.5, 46), expand = TRUE)+
  labs(x = "Longitude",y = "Latitude") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) + # Custom X and Y axis formatting (easier for french translation)
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + # Add scale bar with selectable location
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.25, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + # Add north arrow with selectable location+
  theme_bw()+
  theme(legend.key.size = unit(6,"mm"),
        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 10),
        #legend.position = c(.86,.22), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)),
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

ggsave(plot = sstar.spatial, "Y:/Inshore/Assessment/BoF/2026/Assessment/Figures/Asterias_sp_presence.png", scale = 3.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)
