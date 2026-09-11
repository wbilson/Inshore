###...................................................................................................###
###                  Scallop Health - Spatial Figures (using IDW) and Disease Metrics                 ###
###                                   Full BoF and Approaches                                         ###
###                             G. English & J. Sameoto & B. Wilson                                   ###
###                                       March 2025                                                  ###
###              (on https://github.com/Mar-scal/Inshore/tree/main/SurveyIndices)                     ###
###...................................................................................................###

#Plots for Proportions of myco and discoloured meats in samples by tow and by SPA

#Spatial figures of commercial, recruit and pre-recruit scallop sizes of Survey Density, Survey Biomass, Condition, Meat count and Clappers for BoF: 
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
require(maptools)
require(forcats)
library(ROracle)
require(ggspatial)
require(concaveman)
#library(RCurl)

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
survey.year <- 2026  #survey year
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

# -------------------------------Import WGTHGT DATA------------------------------------------

#List of tows that have detailed samples
#NOTE:  *Query reads in ALL strata and ALL tow types - this is not equivalent to what is used in population models*

quer4 <- paste(
  "SELECT * 			                ",
  "FROM SCALLSUR.scwgthgt s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)

sampled.dat <- dbGetQuery(chan, quer4)
sampled.dat <- sampled.dat[,1:17]

sampled.dat <- sampled.dat %>%
  mutate(year = year(TOW_DATE)) %>%
  filter(year %in% c(2018:survey.year)) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>% 
  mutate(MYCO_INFECTED = as.factor(MYCO_INFECTED)) %>% 
  mutate(MEAT_COLOUR = as.factor(MEAT_COLOUR)) %>% 
  mutate(MEAT_COLOUR = case_when(MEAT_COLOUR == "Normal white colour" ~ "Normal", 
                                 MEAT_COLOUR == "moderate (light brown/gray)" ~ "Moderate",
                                 MEAT_COLOUR == "severe (dark brown/gray)" ~ "Severe")) %>%
  mutate(STRATA_ID = as.numeric(STRATA_ID)) %>% 
  mutate(SPA = case_when(STRATA_ID %in% c(39,6,7,11,12,13,14,15,16,17,18,19,20,48,56) ~ "SPA1A", 
                         STRATA_ID %in% c(54,37,38,50,51,52,53,55,35,49) ~ "SPA1B",
                         STRATA_ID %in% c(57,26) ~ "SPA2",
                         STRATA_ID %in% c(22,23,24) ~ "SPA3",
                         STRATA_ID %in% c(1,2,3,4,5,8,9,10,47) ~ "SPA4",
                         STRATA_ID %in% c(21) ~ "SPA5",
                         STRATA_ID %in% c(30,31,32) ~ "SPA6"))

# ------------------------------- MYCO PROPORTION PLOTS -----------------------------------------

table(sampled.dat$MYCO_INFECTED)

myco.dat <- sampled.dat %>% 
  group_by(CRUISE, tow) %>% 
  count(MYCO_INFECTED)

#convert to wide table format
myco.datw <- pivot_wider(myco.dat, 
                         names_from = MYCO_INFECTED,
                         values_from = n,
                         values_fill = 0)

myco.datw <- myco.datw %>%
  mutate(tot = N+Y) %>% 
  mutate(prop = Y/(tot)) %>% #calculates proportion of infected meats
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)

tow.dat <- sampled.dat %>% group_by(ID, tow, SPA, lat, lon, year) %>% 
  summarise()

myco.datw <- merge(myco.datw, tow.dat, by = "ID", all.x = TRUE) %>% 
  dplyr::select(-tow.y) %>% 
  rename(tow = tow.x)
 
# #Saves files by cruise
# for(i in unique(myco.datw$CRUISE)){
#   write.csv(myco.datw %>% filter(year == survey.year & CRUISE == i), paste0(path.directory,survey.year,"/Assessment/Data/SurveyIndices/",i,"towsdd_MYCOprop.csv"))
# }
# 
# write.csv(myco.datw, "Y:/Inshore/BoF/",survey.year,"/Assessment/Data/SurveyIndices/BF2023towsdd_MYCOprop.csv")

# Calculate total proportions

proportions_data <- myco.datw %>%
  group_by(CRUISE, SPA, year) %>%
  summarise(tot.N = sum(N),
            tot.Y = sum(Y),
            tot.all = sum(tot)) %>% 
  mutate(N.prop = tot.N/tot.all) %>%
  mutate(Y.prop = tot.Y/tot.all) %>% 
  mutate(prop.check = N.prop + Y.prop)

table(proportions_data$prop.check)

prop_data_4plot <- proportions_data %>% 
  select(CRUISE, SPA, year, Y.prop, N.prop)%>%
  group_by(CRUISE, SPA) 


prop_data_4plot <- reshape2::melt(prop_data_4plot, id.vars = c("year", "SPA", "CRUISE"), value.name = "value")

prop_data_4plot <- prop_data_4plot %>%
  add_row(year = 2020, SPA = "SPA1A", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA1B", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA3", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA4", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA5", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA6", CRUISE = NA, variable = NA, value = NA)


# plot Proportions
plot <- ggplot(prop_data_4plot) +
  geom_bar(data=prop_data_4plot[prop_data_4plot$variable%in%c('N.prop','Y.prop'),],
           aes(year, value, fill=factor(variable, levels = c('N.prop','Y.prop'))), colour="black", stat="identity") +
  ylab("Proportion of samples with myco") +
  scale_x_continuous(breaks = seq(2018, survey.year, 2))+
  scale_fill_manual(values=c("skyblue3","salmon"), labels=c("No Myco","Myco"), name=NULL) +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        legend.position = "bottom",legend.direction = "horizontal",
        legend.key.width = unit(0.7, "cm"), legend.text=element_text(size=8),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 5, 5, 1, "points"))+
  facet_wrap(~ SPA)
plot

#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Disease_Metrics/Proportion_of_Myco_by_SPA.png"), plot = plot, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#ggsave(filename = paste0("Y:/Inshore/BoF/",assessmentyear,"/Assessment/Figures/Disease_Metrics/Proportion_of_Myco_by_SPA.png"), plot = plot, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)


#line plot:

line_plot <- ggplot(proportions_data)+
  geom_line(aes(x = year, y = Y.prop, group = SPA))+
  ylim(0, 1.0)+
  ylab("Proportion of Myco identified in samples")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ SPA)
line_plot

#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Disease_Metrics/Proportion_of_Myco_in_samples_by_SPA_lineplot.png"), plot = line_plot, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#Save as .CSV-----------------------------------------
#By Tow
write.csv(myco.datw, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/Myco_in_Meats_By_Tow.csv"))
#By SPA
proportions_data <- proportions_data %>% 
  select(-N.prop, -prop.check)
write.csv(proportions_data, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/Myco_in_Meats_By_SPA.csv"))

# -------------------------------DISCOLOURED PROPORTION PLOTS-----------------------------------------

############# Proportion of B's and C's ##################

greymeat.dat <- sampled.dat %>% 
  group_by(CRUISE, tow) %>% 
  count(MEAT_COLOUR)

#convert to wide table format
greymeat.datw <- pivot_wider(greymeat.dat, 
                             names_from = MEAT_COLOUR,
                             values_from = n,
                             values_fill = 0)

greymeat.datw <- greymeat.datw %>%
  mutate(prop.Severe = (Severe)/(Normal + Moderate + Severe)) %>% 
  mutate(prop.Moderate = (Moderate)/(Normal + Moderate + Severe)) %>% 
  mutate(Severe = as.numeric(Severe)) %>% 
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE)

greymeat.datw <- merge(greymeat.datw, tow.dat, by = "ID", all.x = TRUE) %>% 
  dplyr::select(-tow.y) %>% 
  rename(tow = tow.x)

# Saves files by cruise
#  for(i in unique(greymeat.datw$CRUISE)){
#    write.csv(greymeat.datw %>% filter(year == survey.year & CRUISE == i), paste0(path.directory,survey.year,"/Assessment/Data/SurveyIndices/",i,"towsdd_QUALITYprop.csv"))
#  }
# 
# write.csv(greymeat.datw, "Y:/Inshore/BoF/",survey.year,"/Assessment/Data/SurveyIndices/BI2021towsdd_QUALITY.csv")

# Calculate proportions
proportions_data <- greymeat.datw %>%
  group_by(CRUISE, SPA, year) %>%
  summarise(tot.Normal = sum(Normal),
            tot.Moderate = sum(Moderate),
            tot.Severe = sum(Severe),
            tot.all = sum(Normal + Moderate + Severe)) %>% 
  mutate(Normal.prop = tot.Normal/tot.all) %>%
  mutate(Moderate.prop = tot.Moderate/tot.all) %>%
  mutate(Severe.prop = tot.Severe/tot.all) %>% 
  mutate(prop.check = Normal.prop + Moderate.prop + Severe.prop)

table(proportions_data$prop.check)

prop_data_4plot <- proportions_data %>% 
  select(CRUISE, SPA, year, Normal.prop , Moderate.prop, Severe.prop)


prop_data_4plot <- reshape2::melt(prop_data_4plot, id.vars = c("year", "SPA", "CRUISE"), value.name = "value")

prop_data_4plot <- prop_data_4plot %>%
  add_row(year = 2020, SPA = "SPA1A", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA1B", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA3", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA4", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA5", CRUISE = NA, variable = NA, value = NA) %>%
  add_row(year = 2020, SPA = "SPA6", CRUISE = NA, variable = NA, value = NA)

# plot without pattern
plot <- ggplot(prop_data_4plot) +
  geom_bar(data=prop_data_4plot[prop_data_4plot$variable%in%c('Normal.prop' , 'Moderate.prop', 'Severe.prop'),],
           aes(year, value, fill=factor(variable, levels = c('Normal.prop' , 'Moderate.prop', 'Severe.prop'))), colour="black", stat="identity") +
  ylab("Proportion of samples with discoloured meats") +
  scale_x_continuous(breaks = seq(2018, survey.year, 2))+
  scale_fill_manual(values=c("skyblue3", "grey", "grey18"), labels=c("Normal - A", "Discoloured - B", "Discoloured - C"), name=NULL) +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        legend.position = "bottom",legend.direction = "horizontal",
        legend.key.width = unit(0.7, "cm"), legend.text=element_text(size=8),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 5, 5, 1, "points"))+
  facet_wrap(~ SPA)
plot

#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Disease_Metrics/Proportion_of_Discolouredmeats_A_B_C_by_SPA.png"), plot = plot, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)


#line plot - Bs:
line_plot <- ggplot(proportions_data)+
  geom_line(aes(x = year, y = Moderate.prop, group = SPA))+
  ylim(0, 1.0)+
  ylab("Proportion of discoloured meats - B")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ SPA)
line_plot

#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Disease_Metrics/Proportion_of_Discolouredmeats_Bs_by_SPA_lineplot.png"), plot = line_plot, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#line plot - Cs:
line_plot <- ggplot(proportions_data)+
  geom_line(aes(x = year, y = Severe.prop, group = SPA))+
  ylim(0, 1.0)+
  ylab("Proportion of discoloured meats - C")+
  theme_bw()+
  facet_wrap(~ SPA)
line_plot

#save
ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Disease_Metrics/Proportion_of_Discolouredmeats_Cs_by_SPA_lineplot.png"), plot = line_plot, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)


#Save as .CSV-----------------------------------------
#By Tow
write.csv(greymeat.datw, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/Discoloured_Meats_By_Tow.csv"))
#By SPA
proportions_data <- proportions_data %>% 
  select(-Normal.prop, -prop.check)
write.csv(proportions_data, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/Discoloured_Meats_By_SPA.csv"))
          

# SPATIAL PLOTS -----------------------------------------------------------


# ----------- Making a custom boundary for IDW analysis based off survey tows ---------------
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

#creating list with legend positions so that you can call for "spa1a" legend position. Any edits to legend positions are done here
legend_positions <- list(
  "spa1"  = c(0.85, 0.35),
  "spa1b" = c(0.85, 0.25),
  "spa3"  = c(0.82, 0.44),
  "spa4"  = c(0.88, 0.23),
  "spa6"  = c(0.15, 0.62)
)

mgmt_zone_styles <- list(
  "BoF"  = geom_sf(data = mgmt.zones.detailed %>% filter(!STRATA_ID %in% c(26,57,90,91,92,93)), color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid"),
  "spa3" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(92,93)), color = "red", linewidth = 0.3, fill = NA, linetype = "dashed"),
  "spa6" = geom_sf(data = mgmt.zones.detailed %>% filter(STRATA_ID %in% c(90,91)) %>% st_union() %>% st_as_sf(), color = "red", fill = NA, linewidth = 0.3, linetype = "dashed"),
  # "spa6in" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 90), color = "red", fill = NA, linewidth = 0.2, linetype = "solid"),
  # "spa6out" = geom_sf(data = mgmt.zones.detailed %>%  filter(STRATA_ID %in% 91), color = "black", fill = NA, linewidth = 0.2, linetype = "solid"),
  "Gen"  = geom_sf(data = mgmt.zones, color = "black", fill = NA, linewidth = 0.3, linetype = "solid")
)

coord_ranges = list(
  "BoF" = list(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80)),
  "spa1a" = list(xlim = c(-66.40,-64.70), ylim = c(44.37,45.30)),
  "spa1b" = list(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70)),
  "spa3"  = list(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60)),
  "spa4"  = list(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96)),
  "spa6"  = list(xlim = c(-67.5, -66.35), ylim = c(44.4, 45.2)),
  "BoFall" = list(xlim = c(-67.28,-64.4), ylim = c(43.60, 45.62))
) 

# -------------- MYCOBACTERIUM ------------------------------------------------------------

# Proportion of Myco ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(myco.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,1), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BoFAll_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,1), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BF_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,1), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1A_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,1), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1B_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,1), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA4_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,1), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA3_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,1), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA6_MycoProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# #Myco per Tow  ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(myco.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_Y <- log(Surv.sf$Y) 
Surv.sf$log_Y[which(Surv.sf$log_Y==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_Y~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Y)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BoFAll_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BF_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1A_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1B_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA4_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA3_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA6_Myco_per_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# -------------- DISCOLOURED SCALLOPS ------------------------------------------------------------

# Proportion of Discoloured (Moderate) scallops ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop.Moderate) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BoFAll_GreyMeat_B_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BF_GreyMeat_B_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1A_GreyMeat_B_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1B_GreyMeat_B_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA4_GreyMeat_B_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA3_GreyMeat_B_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA6_GreyMeat_B_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# Proportion of Discoloured (Severe) scallops ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop.Severe) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +  #oob = scales::squish
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BoFAll_GreyMeat_C_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BF_GreyMeatProportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1A_GreyMeat_C_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1B_GreyMeat_C_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(proportion)",limits = c(0,1), oob = scales::squish) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA4_GreyMeat_C_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA3_GreyMeat_C_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(proportion)", limits = c(0,1), oob = scales::squish) +
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA6_GreyMeat_C_Proportion',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Number of Discoloured scallops (moderate) per Tow  ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_grey <- log(Surv.sf$Moderate) 
Surv.sf$log_grey[which(Surv.sf$log_grey==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_grey~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Moderate)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BoFAll_GreyMeats_B_per_Tow',survey.year,'_new.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BF_GreyMeats_per_B_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1A_GreyMeats_per_B_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1B_GreyMeats_per_B_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA4_GreyMeats_per_B_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA3_GreyMeats_per_B_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (B) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA6_GreyMeats_per_B_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# Number of Discoloured scallops (Severe) per Tow  ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_grey <- log(Surv.sf$Severe) 
Surv.sf$log_grey[which(Surv.sf$log_grey==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_grey~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Severe)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BoFAll_GreyMeats_C_per_Tow',survey.year,'_new.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_BF_GreyMeats_per_C_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1A_GreyMeats_per_C_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA1B_GreyMeats_per_C_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA4_GreyMeats_per_C_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA3_GreyMeats_per_C_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Discoloured (C) \nscallop \n(N/Tow)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir,'Disease_Metrics/ContPlot_SPA6_GreyMeats_per_C_Tow',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

#### |------------------------- end of English script -----------------------------|####

#### SCALLOP HEALTH PLOTS - FRENCH TRANSLATION ####

### All English figures created above will be re-created as French figures.
### Assuming data/shapefile setup has been run (everything before "MYCOBACTERIUM"), these can be run independently of English figures
### The only translations needed were as follows (update if translation is not correct):
# Discoloured scallop (meat) = Chair décoloré
# Tow = traits de chalut

## words that stay the same: Latitude, Longitude, Proportion, Myco

#set up a directory for french figures
saveplot.dir.fr <- paste0(saveplot.dir,"FrenchFigures_indicies/")
# -------------- MYCOBACTERIUM ------------------------------------------------------------

# Proportion of Myco ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(myco.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.115), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.115), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.115), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.115), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.115), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.115), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(proportion)", limits = c(0,0.115), oob = scales::squish) +#max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Myco Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_MycoProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# #Myco per Tow  ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(myco.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_Y <- log(Surv.sf$Y) 
Surv.sf$log_Y[which(Surv.sf$log_Y==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_Y~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$Y)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Myco \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Myco per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_Myco_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# -------------- DISCOLOURED SCALLOPS ------------------------------------------------------------

# Proportion of Discoloured scallops ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_prop <- log(Surv.sf$prop) 
Surv.sf$log_prop[which(Surv.sf$log_prop==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_prop~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$prop)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75), oob = scales::squish) +
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75), oob = scales::squish) +
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(proportion)", limits = c(0,0.75), oob = scales::squish) +
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Discoloured scallop Proportion"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_GreyMeatProportion',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# Number of Discoloured scallops (moderate + severe) per Tow  ------------------------------------------------------

#IDW for whole BoF. Specific changes for each area will be done during plotting
Surv.sf<-st_as_sf(subset(greymeat.datw,year==survey.year),coords=c("lon","lat"))
st_crs(Surv.sf) <- 4326
Surv.sf<-st_transform(Surv.sf,crs = 32620)

#log transforming for better idw() display. Transformed back later for reader comprehension
Surv.sf$log_grey <- log(Surv.sf$NUM_GREYMEAT) 
Surv.sf$log_grey[which(Surv.sf$log_grey==-Inf)] <- log(0.0001)


#Overlay a grid over the survey area, as it is required to interpolate (smaller cellsize will take more time to run than larger. both here and during interpolation)
grid <- st_make_grid(idw_sf, cellsize=1000) %>% st_intersection(idw_sf)
#Doing the inverse distance weighted interpolation
preds_idw2 <- gstat::idw(log_grey~1,Surv.sf,grid,idp=3)

preds_idw2$prediction <- exp(preds_idw2$var1.pred)
summary(preds_idw2$prediction)
summary(Surv.sf$NUM_GREYMEAT)

# ----BoF ALL (BF, SPA3, & SPA6) -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["Gen"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoFall"]]$xlim, ylim = coord_ranges[["BoFall"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BoFAll_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----FULL BAY -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["BoF"]]$xlim, ylim = coord_ranges[["BoF"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "BoF Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_BF_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa1"]]) +
  coord_sf(xlim = coord_ranges[["spa1a"]]$xlim, ylim = coord_ranges[["spa1a"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1A Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1A_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa1b"]]) +
  coord_sf(xlim = coord_ranges[["spa1b"]]$xlim, ylim = coord_ranges[["spa1b"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA1B Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA1B_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = mgmt_zone_styles[["BoF"]], Surv.sf, Land, scale_location = "br", arrow_location = "br", legend_position = legend_positions[["spa4"]]) +
  coord_sf(xlim = coord_ranges[["spa4"]]$xlim, ylim = coord_ranges[["spa4"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA4 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save  
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA4_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["spa3"]], mgmt_zone_styles[["Gen"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa3"]]) +
  coord_sf(xlim = coord_ranges[["spa3"]]$xlim, ylim = coord_ranges[["spa3"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA3 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA3_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----
bathy + #Plot survey data and format figure.
  geom_sf(data = preds_idw2, aes(fill = prediction),  colour = NA) + 
  scale_fill_viridis_c(option = "H",  trans = "sqrt", name = "Chair décoloré \n(N/traits de chalut)", limits = c(0,max(preds_idw2$prediction))) + 
  p(mgmt_zone = list(mgmt_zone_styles[["Gen"]], mgmt_zone_styles[["spa6"]]), Surv.sf, Land, scale_location = "tl", arrow_location = "tl", legend_position = legend_positions[["spa6"]]) +
  coord_sf(xlim = coord_ranges[["spa6"]]$xlim, ylim = coord_ranges[["spa6"]]$ylim, expand = FALSE) +
  labs(#title = paste(survey.year, "", "SPA6 Discoloured scallop per Tow"), 
    x = "Longitude", y = "Latitude")

#save
ggsave(filename = paste0(saveplot.dir.fr,'ContPlot_SPA6_GreyMeats_per_Tow',survey.year,'_FR.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


##### |----------------------------------- END OF SCRIPT ------------------------------------| #####
