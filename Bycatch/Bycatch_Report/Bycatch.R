#### This script should pull bycatch data and create files of standardized abundance per tow for each species  in part 1,
#  part 2 pulls the individual bycatch data from the scallsur data base - normalized on individuals 
## J.Sameoto nov 2024 

library(dplyr)
library(lubridate)
library(sf)
library(tidyr)
library(flextable)
library(data.table)
library(ggplot2)
library(gt)
library(ftExtra)
library(janitor)
library(rgdal)
library(terra)
library(patchwork)
library(viridis)
#library(RStoolbox)
library(ROracle)
library(knitr)
library(tinytex)
library(kableExtra)
library(haven)
library(gridExtra)
library(collapse)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)


## SETUP 
setwd("Y:/Inshore/Bycatch/SurveyBycatch/bycatch_report")

#### Import Mar-scal functions
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
  "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

##Data pull
#con= dbConnect(DBI::dbDriver("Oracle"), un.sameotoj, pw.sameotoj, 'ptran') 

#un.ID <- keyring::key_list("Oracle")[1,2]
#pw.ID <- keyring::key_get("Oracle", un.ID)
un.ID <- un.sameotoj
pw.ID <- pw.sameotoj

#### PUll data for standardize abundance by tow 
con <- dbConnect(DBI::dbDriver("Oracle"), un.ID, pw.ID, 'ptran')
bycatch <- dbGetQuery(con, (" SELECT * from SCALLSUR.SCBYCATCH_STD"))

tows <- dbGetQuery(con, ("SELECT TOW_SEQ, CRUISE, TOW_NO, TOW_DATE, TOW_TYPE_ID, STRATA_ID, START_LAT, START_LONG, TOW_DIR, TOW_LEN, TOW_LEN_ID, DEPTH, BOTTOM_TEMP, BOTTOM_ID, MGT_AREA_ID from SCALLSUR.SCTOWS"))



#formatting of bycatch data
bycatch$COMMON[bycatch$COMMON == "BATHYPOLYPUS ARCTICUS"] <- "NORTH ATLANTIC OCTOPUS"
bycatch$COMMON[bycatch$COMMON == "SEMIROSSIA TENERA"] <- "LESSER BOBTAIL SQUID"
bycatch$COMMON[bycatch$COMMON == "BRILL/WINDOWPANE"] <- "WINDOWPANE FLOUNDER"
bycatch$COMMON[bycatch$COMMON == "AHLIA EGMONTIS"] <- "KEY WORM EEL"
#combining white, red and Hake (NS) together
#This will also produce a combined data file for Red, White and HAKE (NS). These can be distinguished in the file by speccd_id and scientific name
bycatch$COMMON[bycatch$COMMON == "WHITE HAKE"] <- "HAKE (NS)"
bycatch$COMMON[bycatch$COMMON == "SQUIRREL OR RED HAKE"] <- "HAKE (NS)"
bycatch$COMMON[bycatch$COMMON == "LEUCORAJA <35cm"] <- "LITTLE OR WINTER SKATE under 35cm"

bycatch$SLAT<-convert.dd.dddd(bycatch$START_LAT,format='dec.deg')
bycatch$SLONG<-convert.dd.dddd(bycatch$START_LON,format='dec.deg')
bycatch$ELAT<-convert.dd.dddd(bycatch$END_LAT,format='dec.deg')
bycatch$ELONG<-convert.dd.dddd(bycatch$END_LON,format='dec.deg')

#format date to allow to select data by year
bycatch$TOW_DATE<- as.Date(bycatch$TOW_DATE)
bycatch <- bycatch %>%
  dplyr::mutate(year = lubridate::year(TOW_DATE), 
                month = lubridate::month(TOW_DATE), 
                day = lubridate::day(TOW_DATE))

#to get species that has more than one record
s<- bycatch %>% 
  group_by(COMMON) %>% 
  #removing uncommon species with only one or few records
  #update this filter accordingly
  filter(!COMMON %in% c("CEPHALOPODA C.", "ROSEFISH(BLACK BELLY)", "SMELTS,CAPELIN (NS)", "EELPOUT (NS)", "SUMMER FLOUNDER", "GULF STREAM FLOUNDER", "ALEWIFE")) %>% 
  dplyr::filter(year == 2023) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

species_all = unique(bycatch$COMMON)  #to get a list of all species encountered 
species = unique(s$COMMON) # to get a list of species for the report
omit = setdiff(species_all, species) #species that were omitted from the plots and maps in report, referenced in introduction
omit = omit[order(omit)]
#format tow data
tows$TOW_DATE<- as.Date(tows$TOW_DATE)
tows <- tows %>%
  dplyr::mutate(year = lubridate::year(TOW_DATE), 
                month = lubridate::month(TOW_DATE), 
                day = lubridate::day(TOW_DATE))

tows$SLAT<-convert.dd.dddd(tows$START_LAT,format='dec.deg')
tows$SLONG<-convert.dd.dddd(tows$START_LON,format='dec.deg')
tows <- tows %>% 
  dplyr::select(TOW_SEQ, CRUISE, TOW_NO, MGT_AREA_ID, year, month, day, SLAT, SLONG, DEPTH, TOW_DIR, TOW_LEN, BOTTOM_TEMP, STRATA_ID)
head(tows)

#to set up total tows for survey area to be used in the footer of the tables
n <- tows %>% 
  dplyr::filter(year == max(year)) %>% 
  dplyr::select(CRUISE, TOW_NO) %>% 
  group_by(CRUISE) %>% 
  summarise(total_records = n_distinct(TOW_NO)) %>% 
  rename(areas = CRUISE)

#to set up loop for each species
species = species[order(species)]
species_data = list()


bycatch_by_tow <- bycatch %>% 
  dplyr::select(TOW_SEQ, CRUISE, MGT_AREA_ID, TOW_NO, SPECCD_ID, COMMON, SCIENTIFIC, MEAS_ID, TOTAL_SAMPLED_GEAR, ABUNDANCE_RAW, ABUNDANCE_STD, SLAT, SLONG, year, month, day) %>% 
  group_by(TOW_SEQ, year, CRUISE, TOW_NO, SPECCD_ID, COMMON, SCIENTIFIC, MEAS_ID, TOTAL_SAMPLED_GEAR) %>% 
  dplyr::summarise(abun_raw = sum(ABUNDANCE_RAW), abun_std = sum(ABUNDANCE_STD)) 



for(i in 1:length(species_all)) { 
  species_data[[species_all[i]]] <- bycatch_by_tow %>% 
    dplyr::filter(COMMON == species_all[i]) %>% 
    right_join(y= tows, by = c("TOW_SEQ", "year", "CRUISE", "TOW_NO")) %>% 
    replace_NA( value = 0, cols = c("abun_raw", "abun_std"), set = FALSE)
  # mutate(across(where(is.numeric), ~ replace_na(., 0))) #%>%  #Assumes all NAs are 0
}

new_row_2020 <- data.frame(
  COMMON = NA,
  year = 2020,
  TOW_SEQ = 0,
  CRUISE = "XX2020",
  TOW_NO = 0,
  abun_raw = NA,
  abun_std = NA
  #Add other columns as needed with NA values
)

for(i in 1:length(species_all)) { 
  species_data[[species_all[i]]]<-
    rbind(species_data[[species_all[i]]],new_row_2020)
}
#This writes a data file for all species ever recorded on the inshore scallop survey  

for (i in 1:length(species_all)) { 
write.csv(species_data[[species_all[i]]],file=paste0("species_data/std_data_",species_all[i],".csv"), row.names = FALSE)
}


#### Pull data for individual data - individual level data (data normalized on individuals within tows)
#specify species code in sql query 
con <- dbConnect(DBI::dbDriver("Oracle"), un.ID, pw.ID, 'ptran')
bycatch.ind <- dbGetQuery(con, (" SELECT * from SCALLSUR.SCBYCATCH_BY_TOW_RAW WHERE SPECCD_ID = 43"))

dim(bycatch.ind)
head(bycatch.ind)

table(bycatch.ind$MEAS_ID)
table(bycatch.ind$SEX_ID)


bycatch.ind <- bycatch.ind %>% select(CRUISE, TOW_NO, TOW_DATE, STRATA_ID, START_LAT, START_LONG, END_LAT, END_LONG, BYCATCH_SEQ, SPECCD_ID, COMMON, SCIENTIFIC, MEAS_VAL)


write.csv(bycatch.ind, "Y:/Inshore/Bycatch/SurveyBycatch/WinterFlounder_GF_PED_2024/winterfounderindividuals.csv")





