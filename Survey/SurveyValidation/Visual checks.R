
#-------------------------------------------------------------------------------------------------------
##CHANGES MADE IN 2024 (by Brittany Wilson, Feb 2024)
#- added a check for NAs in Tow length in the tow_CONVERTED.csv
#- added a check for duplicate row entries within tows in dhf.csv
#- integrated alternative to the check.tows.spatial function (This function is pulled from the Github version now!)
#- added a check for Strata entered matches where the tow is located
#-------------------------------------------------------------------------------------------------------


######  DATA CHECKS ########

#libraries
require(ggplot2)
require(tidyverse)
require(sf)
require(units)
library(ROracle)

#Check tows spatial function and coord convert function:

funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Inshore/master/Survey/check.tows.spatial.r",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


#define
direct <- "Y:/Inshore/Survey/"
year <- 2026 #For years prior to 2023, the directory name is different! Will need to adjust if running for previous years - year/data entry templates and examples/
CRUISE <- "BI" # "BI", BF", "GM", "SFA29"
#uid = Sys.getenv("un.raperj") #ptran username
#pwd = Sys.getenv("pw.raperj") #ptran password
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)


###Read in shapefiles if needed
temp <- tempfile()
### Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
### Figure out what this file was saved as
temp2 <- tempfile()
### Unzip it
unzip(zipfile=temp, exdir=temp2)

### Now read in the shapefiles
#BF.strata <- st_read(paste0(temp2, "/inshore_survey_strata/PolygonSCSTRATAINFO_rm46-26-57.shp")) %>% st_transform(crs = 4326)

#---- polygons with strata for SPA 2 ----
BF.strata <- st_read(paste0(temp2, "/inshore_survey_strata/archive/PolygonSCSTRATAINFO_AccessedSept102017.shp")) %>% st_transform(crs = 4326)

SPA1A <- st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% mutate(ET_ID = "1A") %>% st_transform(crs = 4326)
SPA1B <- st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% mutate(ET_ID = "1B") %>% st_transform(crs = 4326)
SPA2 <- st_read(paste0(temp2, "/SPA2_polygon_NAD83_revised2023.shp")) %>% mutate(ET_ID = "2") %>% st_transform(crs = 4326)
SPA3 <- st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp")) %>% mutate(ET_ID = "3") %>% st_transform(crs = 4326)
SPA4 <- st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp")) %>% mutate(ET_ID = "4") %>% st_transform(crs = 4326)
SPA5 <- st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp")) %>% mutate(ET_ID = "5") %>% st_transform(crs = 4326)
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A") %>% st_transform(crs = 4326)
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B") %>% st_transform(crs = 4326)
SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C") %>% st_transform(crs = 4326)
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D") %>% st_transform(crs = 4326)

VMS_out <- st_read(paste0(temp2, "/SPA6_VMSstrata_OUT_2015.shp")) %>% mutate(ET_ID = "OUT") %>% st_transform(crs = 4326)
VMS_in <- st_read(paste0(temp2, "/SPA6_VMSstrata_IN_2015.shp")) %>% mutate(ET_ID = "IN") %>% st_transform(crs = 4326)

SFA29 <- st_read(paste0(temp2, "/SFA29_subareas_utm19N.shp")) %>% mutate(ID = seq(1,5,1)) %>%  #TO FIX IN COPY ON GITHUB (ET_ID missing so adding it here)
  mutate(ET_ID = case_when(ID == 1 ~ 41, 
                           ID == 2 ~ 42,
                           ID == 3 ~ 43,
                           ID == 4 ~ 44,
                           ID == 5 ~ 45)) %>% 
  dplyr::select(Id = ID, ET_ID) %>% st_transform(crs = 4326)

# tow_CONVERTED.csv ----------------------------------------------------------------

num.tows <- read.csv(paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"tow_CONVERTED.csv"))

#Check for missing tow lengths:
table(is.na(num.tows$Tow_len))

#check for NAs in other columns.
#check that ranges of values make sense (min, maxs etc)
summary(num.tows)

### Check number of gangs, line and unlined - num_lined is number of lined gangs on gear, num_unlined is number of unlined gangs on gear; since switch to Miracle gear in 2012 this should be always be num_lined = 2, and num_unlined = 7 
table(num.tows$num_lined)
num.tows[num.tows$num_lined != 2,]
#IF ANY TOWS RETURNED, MUST CHECK - ERROR

table(num.tows$num_unlined)
num.tows[num.tows$num_unlined != 7,]
#IF ANY TOWS RETURNED, MUST CHECK - ERROR

### Check number of gangs, num_lined_freq - number of lined gangs fished -- can be < 2 but NEVER zero -- DO NOT LOAD IF ZERO - if true zero tow must be dropped. 
#CHECK ANY TOWS < 2 to confirm; and confirm NOT ZERO 
num.tows[num.tows$num_lined_freq < 2,]

### Check number of gangs, num_unlined_freq - number of unlined gangs fished -- can be < 7 but NEVER zero -- DO NOT LOAD IF ZERO - if true zero tow must be dropped. 
#CHECK ANY TOWS < 7 to confirm; and confirm NOT ZERO 
num.tows[num.tows$num_unlined_freq < 7,]



# HGTWGT.csv ----------------------------------------------------------------

mwsh <- read.csv(paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_HGTWGT.csv"))
str(mwsh)

mwsh$Weight <- as.numeric(as.character(mwsh$Weight))
mwsh$Mycobacteria <- as.factor(mwsh$Mycobacteria)
mwsh$Meat_Colour <- as.factor(mwsh$Meat_Colour)

summary(mwsh)

#Plots height and weight

#all data points
ggplot() + geom_text(data=mwsh, aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow)))

# adjust tow numbers to test it out 
# if an entire tow is away from the rest, make sure that the height and weight columns weren't flipped!!
#(labels are Tow numbers)

#Plot fewer tows to visualize better - Tows 1-50
ggplot() + geom_text(data=mwsh[mwsh$Tow>1 & mwsh$Tow<50,]
                     , aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow)))

#Plot individual tows (labels are sample numbers)

ggplot() + geom_text(data=mwsh[mwsh$Tow==25,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==35,], aes(Height, Weight, colour=as.factor(Tow), label=Num))

ggplot() + geom_text(data=mwsh[mwsh$Tow==27,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==21,], aes(Height, Weight, colour=as.factor(Tow), label=Num))


#Plot fewer tows to visualize better - Tows 50-100
ggplot() + geom_text(data=mwsh[mwsh$Tow>50 & mwsh$Tow<100,]
                     , aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow))) 

#Plot individual tows (labels are sample numbers)

ggplot() + geom_text(data=mwsh[mwsh$Tow==90,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==69,], aes(Height, Weight, colour=as.factor(Tow), label=Num))

#Plot fewer tows to visualize better - Tows 100 max tow number for cruise
ggplot() + geom_text(data=mwsh[mwsh$Tow>100 & max(mwsh$Tow),]
                     , aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow))) 

#Plot individual tows (labels are sample numbers)
ggplot() + geom_text(data=mwsh[mwsh$Tow==273,], aes(Height, Weight, colour=as.factor(Tow), label=Num))


ggplot() + geom_text(data=mwsh[mwsh$Tow==72,], aes(Height, Weight, colour=as.factor(Tow), label=Num))
ggplot() + geom_text(data=mwsh[mwsh$Tow==76,], aes(Height, Weight, colour=as.factor(Tow), label=Num))

#Plot fewer tows to visualize better - Tows 100 max tow number for cruise
ggplot() + geom_text(data=mwsh[mwsh$Tow>100 & max(mwsh$Tow),]
                     , aes(Height, Weight, colour=as.factor(Tow), label=as.factor(Tow))) 

#Plot individual tows (labels are sample numbers)
ggplot() + geom_text(data=mwsh[mwsh$Tow==122,], aes(Height, Weight, colour=as.factor(Tow), label=Num))


# bycatch.csv ----------------------------------------------------------------

bycatch <- read.csv(paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_bycatch.csv"))

## NO NA's ALLOWED! REPLACE THESE WITH APPROPRIATE CODES (unknown sex=0!)
print(bycatch[is.na(bycatch$Species_code) & !is.na(bycatch$Tow_num) |
                is.na(bycatch$Measure_code) & !is.na(bycatch$Tow_num) |
                is.na(bycatch$Sex) & !is.na(bycatch$Tow_num),])

#Merge bycatch codes onto common name 
query.bycatch <-("SELECT * FROM scallsur.scspeciescodes")

chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')
bycatch.codes <- dbGetQuery(chan, query.bycatch)
bycatch.codes <- bycatch.codes %>% select(SPECCD_ID, COMMON)

#left join bycatch on codes to get common name. all.x = TRUE means extra rows will be added to the output, one for each row in x that has no matching row in y
bycatch.names <- merge(bycatch, bycatch.codes, by.x = c("Species_code"), by.y=c("SPECCD_ID"), all.x = TRUE)

# number of rows should match 
dim(bycatch.names)[1] == dim(bycatch)[1]

#find if any codes that did not have a match for code in scallsur 
bycatch.names[is.na(bycatch.names$COMMON),]


#Check codes and sex codes make sense 
ggplot() + geom_point(data=bycatch.names, aes(Sex, as.factor(Species_code)))

#CHECK why octopus not sexed
bycatch[bycatch$Species_code == 4524 & bycatch$Sex == 0,]

#CHECK why lobster not sexed
bycatch[bycatch$Species_code == 2550 & bycatch$Sex == 0,]

#check by little/winter small skate not sexed 
bycatch[bycatch$Species_code == 1191 & bycatch$Sex == 0,]

#check why redfish sexed 
bycatch[bycatch$Species_code == 20 & bycatch$Sex != 0,]

#check why windowpane sexed 
bycatch[bycatch$Species_code == 143 & bycatch$Sex != 0,]

#check why winter flounder sexed 
bycatch[bycatch$Species_code == 43 & bycatch$Sex != 0,]

#check why winter skate not sexed 
bycatch[bycatch$Species_code == 204 & bycatch$Sex == 0,]

#check why little skate not sexed 
bycatch[bycatch$Species_code == 203 & bycatch$Sex == 0,]

#check why smooth skate not sexed 
bycatch[bycatch$Species_code == 202 & bycatch$Sex == 0,]

#check why spiny dogfish sexed 
bycatch[bycatch$Species_code == 220,]

##confirm different species of sea cucumber 6721
bycatch[bycatch$Species_code == 6721,]



# We only record ocean pout, so the species code should be 640. Change records of 845 or other (642, 598) to 640. Note that in 2012, they were miscoded as 845. 
# bycatch[which(bycatch$Species_code%in% c(845, 642, 598)),]

#Note - horse mussels are not entered in the bycatch.csv. These are entered in a separate file (so there should *not* be species code 4332 in this file)

ggplot() + geom_point(data=bycatch, aes(as.factor(Sex), Measurement)) + facet_wrap(~Species_code, scales="free")
bycatch[which(bycatch$Species_code%in% c(12)),]
bycatch[which(bycatch$Species_code%in% c(41)),]
bycatch[which(bycatch$Species_code%in% c(122)),]

#bycatch[which(bycatch$Species_code==1191 & bycatch$Tow_num==53 & bycatch$Measurement>100),]$Measurement <-24
#bycatch[which(bycatch$Species_code==1191 & bycatch$Measurement==30.5),]$Measurement <- 30
#bycatch[which(bycatch$Species_code==1191 & bycatch$Measurement>30 & bycatch$Tow_num==244),]$Measurement <- 26



# dhf.csv ----------------------------------------------------------------

dhf <-  read.csv(paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_dhf.csv"))

#Check for duplicated rows from columns X0 and X95, grouped by tow
dhf.sh.bins <- dhf |> dplyr::select(TOW,X0:X95)
#Remove rows that contain all NAs for the shell height bins (excluding TOW column).
dhf.sh.bins <- filter(dhf.sh.bins, rowSums(is.na(dhf.sh.bins[,-1])) != ncol(dhf.sh.bins[,-1]))

#Checks for duplicate rows within a Tow.
dhf.dup.check <- data.frame()
for(i in unique(dhf.sh.bins$TOW)){
output <- dhf.sh.bins |> filter(TOW == i)
output$wx <- duplicated(output, fromLast = TRUE)
dhf.dup.check <- rbind(dhf.dup.check, output)
}
###Check these tows for duplicate entries### - Manually inspect all TRUE TOWs (cross reference datasheets) - These may or may not be errors (e.g. could just have 1s in the same column and NAs in the rest).
dhf.dup.check |> filter(wx == TRUE) |> dplyr::select(TOW)

#Look at specific Tow number and cross reference with data sheet.
View(dhf.dup.check |> filter(TOW == 39)) #enter tow

#Re-arrange data for plotting:

### REPEAT THIS SECTION FOR LIVE AND THEN FOR DEAD #######

dhf <-  read.csv(paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_dhf.csv"))

#Re-arrange data for plotting:
dhf <- reshape2::melt(dhf, id.vars=c("CRUISE", "TOW", "GEAR", "DEPTH", "c"))
dhf <- dplyr::arrange(dhf, TOW)
dhf$variable <- gsub('X', '', dhf$variable)
dhf$bin <- ifelse(dhf$c %in% c(0,2), dhf$variable, 
                  ifelse(dhf$c %in% c(1,3), as.numeric(dhf$variable)+100, NA))
dhf <- dplyr::arrange(dhf, TOW, GEAR, c)

#FILTER for live/dead - **!! CHANGE THIS LINE FOR LIVE OR DEAD !!**
dhf <- dhf |> filter(c %in% c(2,3)) # c(0,1) for Live. and c(2,3)) for Dead


#Plot to look for outliers - will need to adjust for survey tow numbers. Plots frequency (y axis), bin (x axis), by Tow and Gear (unlined/lined)
dhf1 <- dhf[dhf$TOW>0 & dhf$TOW < 15,]
ggplot(dhf1, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")


dhf2 <- dhf[dhf$TOW >= 16 & dhf$TOW <= 30,]
#ggplot() + geom_point(data=dhf2, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf2, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf3 <- dhf[dhf$TOW>=31 & dhf$TOW <= 45,]
#ggplot() + geom_point(data=dhf3, aes(as.numeric(bin), value)) + facet_grid(GEAR~TOW, scales="free")
ggplot(dhf3, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf4 <- dhf[dhf$TOW>=46 & dhf$TOW <= 60,]
ggplot(dhf4, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf5 <- dhf[dhf$TOW>=61 & dhf$TOW <= 75,]
ggplot(dhf5, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf6 <- dhf[dhf$TOW>=76 & dhf$TOW <= 90,]
ggplot(dhf6, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf6 <- dhf[dhf$TOW>=91 & dhf$TOW <= 105,]
ggplot(dhf6, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf7 <- dhf[dhf$TOW>=106 & dhf$TOW <= 120,]
ggplot(dhf7, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf8 <- dhf[dhf$TOW>=121 & dhf$TOW <= 135,]
ggplot(dhf8, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf9 <- dhf[dhf$TOW>=136 & dhf$TOW <= 150,]
ggplot(dhf9, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf10 <- dhf[dhf$TOW>=151 & dhf$TOW <= 165,]
ggplot(dhf10, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf11 <- dhf[dhf$TOW>=166 & dhf$TOW <= 180,]
ggplot(dhf11, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf12 <- dhf[dhf$TOW>=181 & dhf$TOW <= 195,]
ggplot(dhf12, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf13 <- dhf[dhf$TOW>=196 & dhf$TOW <= 210,]
ggplot(dhf13, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf14 <- dhf[dhf$TOW>=211 & dhf$TOW <= 225,]
ggplot(dhf14, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf15 <- dhf[dhf$TOW>=226 & dhf$TOW <= 240,]
ggplot(dhf15, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf16 <- dhf[dhf$TOW>=241 & dhf$TOW <= 255,]
ggplot(dhf16, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf17 <- dhf[dhf$TOW>256 & dhf$TOW <= 270,]
ggplot(dhf17, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf18 <- dhf[dhf$TOW>271 & dhf$TOW <= 285,]
ggplot(dhf18, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf19 <- dhf[dhf$TOW>286 & dhf$TOW <= 300,]
ggplot(dhf19, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf20 <- dhf[dhf$TOW>301 & dhf$TOW <= 315,]
ggplot(dhf20, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf21 <- dhf[dhf$TOW>316 & dhf$TOW <= 330,]
ggplot(dhf21, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")

dhf22 <- dhf[dhf$TOW>331 & dhf$TOW <= 345,]
ggplot(dhf22, aes(as.numeric(bin), value)) + geom_bar(fill = "aquamarine3", stat = "identity") + facet_grid(GEAR~TOW, scales="free")


######## REPEAT ABOVE FOR DEAD ######


# horsemussellive.csv---------------------

hm.live <- read.csv(paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_horsemussellivefreq.csv"))

#Species Code must be "4332"
table(hm.live$SPECIES.CODE)

hm.live <- hm.live %>% 
    mutate(flag.speciescode = case_when(SPECIES.CODE != 4332 ~ "check",
                            TRUE ~ "ok"))
hm.live %>% filter(flag.speciescode == "check")

#Prorate factor can only be 1 or 2. 
# - 1 if both lined drags fished AND sampled the catch from both these drags (i.e. no prorating) 
# - 2 if only had 1 lined drag fish (i.e. correspond to you having to make NUM_LINED_FREQ = 1 when normally it's 2) OR
   #you had both lined drags fish but subsampled the catch by only counting/measuring the length frequencies from one of the 2 lined drags. 

table(hm.live$PRORATE.FACTOR)
hm.live$PRORATE.FACTOR <- as.numeric(hm.live$PRORATE.FACTOR)

hm.live <- hm.live %>% 
  mutate(flag.proratefac = case_when((PRORATE.FACTOR != 1) & (PRORATE.FACTOR != 2) ~ "check",
                          TRUE ~ "ok"))
hm.live %>% filter(flag.proratefac == "check")

#For any tows where PRORATE.FACTOR == 2, check
hm.live <- hm.live %>% 
  mutate(flag.factor2 = case_when(PRORATE.FACTOR == as.numeric(2) ~ "check",
                          TRUE ~ "ok"))
hm.live %>% filter(flag.factor2 == "check") #Ensure this correctly marked as prorated.

#Number of tows should match number of tows from survey
length(hm.live$TOW) == length(num.tows$Oracle.tow..) # same length
table(hm.live$TOW == num.tows$Oracle.tow..) #Same numbers? Should all be TRUE

#There should be x unique tows where x is the number of tows from the survey:
length(unique(hm.live$TOW))
table(duplicated(hm.live$TOW)) #Should all be false


#LIVE.DEAD column should only contain "L"s. D's will be in the horsemusseldead.csv
hm.live <- hm.live %>% 
  mutate(flag.live = case_when(LIVE.DEAD != "L" ~ "check",
                          TRUE ~ "ok"))
hm.live %>% filter(flag.live == "check")

#Check Cruise - should only be one cruise!
unique(hm.live$CRUISE)
table(hm.live$CRUISE)

#Does the cruise in the data sheet match the cruise you are data checking?
unique(hm.live$CRUISE) == paste0(CRUISE,year)

#All numbers under Bin ID columns should be > 0. Also flag any numbers per bin > 100 (not necessarily incorrect, but will flag these tows to check)
#Less than 0?
hm.live %>% 
  filter_at(vars(5:44), any_vars(. <0))

#Greater than 100?
hm.live %>% 
  filter_at(vars(5:44), any_vars(. >100))

# Horsemusseldead.csv ---------------------

hm.dead <- read.csv(paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_horsemusseldeadfreq.csv"))

#Species Code must be "4332"
table(hm.dead$SPECIES.CODE)

hm.dead <- hm.dead %>% 
  mutate(flag.speciescode = case_when(SPECIES.CODE != 4332 ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.speciescode == "check")

#Prorate factor can only be 1 or 2
table(hm.dead$PRORATE.FACTOR)

hm.dead <- hm.dead %>% 
  mutate(flag.proratefac = case_when((PRORATE.FACTOR != 1) & (PRORATE.FACTOR != 2) ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.proratefac == "check")

#For any tows where PRORATE.FACTOR == 2, check
hm.dead <- hm.dead %>% 
  mutate(flag.factor2 = case_when(PRORATE.FACTOR == as.numeric(2) ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.factor2 == "check") #Ensure this correctly marked as prorated.

#Tow numbers should match number of tows from survey
length(hm.dead$TOW) == length(num.tows$Oracle.tow..)# same length
table(hm.dead$TOW == num.tows$Oracle.tow..) #Should all be TRUE

#Number of unique tows should match number of tows from survey:
length(unique(hm.dead$TOW))
table(duplicated(hm.dead$TOW)) #Should all be false


#LIVE.DEAD column should only contain "D"s. 
hm.dead <- hm.dead %>% 
  mutate(flag.dead = case_when(LIVE.DEAD != "D" ~ "check",
                          TRUE ~ "ok"))
hm.dead %>% filter(flag.dead == "check")

#Check Cruise - should only be one cruise!
unique(hm.dead$CRUISE)
table(hm.dead$CRUISE)

#Does the cruise in the data sheet match the cruise you are data checking?
unique(hm.dead$CRUISE) == paste0(CRUISE,year)

#All numbers under Bin ID columns should be > 0. Also flag any values > 50 (not necessarily incorrect, but will flag these tows to check)
#Less than 0?
hm.dead %>% 
  filter_at(vars(5:44), any_vars(. <0))

#Greater than 50?
hm.dead %>% 
  filter_at(vars(5:44), any_vars(. >50))

#Extra horse mussel checks ------------------------------------------------

#Tows where horse mussels were present - live or dead
hm.check.l <- hm.live %>% 
  mutate(totlive = dplyr::select(., X0:X195) %>% rowSums(na.rm = TRUE) %>% round(0))
hm.check.d <- hm.dead %>% 
  mutate(totdead = dplyr::select(., X0:X195) %>% rowSums(na.rm = TRUE) %>% round(0))

hm.check <- data.frame(cbind("TOW" = hm.check.l$TOW, "Total.Live" = hm.check.l$totlive , "TOW.D" = hm.check.d$TOW, "Total.Dead" = hm.check.d$totdead))

hm.check %>% filter(Total.Live > 0 | Total.Dead > 0) 

#Moving bycatch length frequency data into SCBYCATCHLENGTHFREQUENCIES table#
nrow(hm.check %>% filter(Total.Live > 0 | Total.Dead > 0)) # This number should match the number of records that will be entered into SCBYATCHES 
#associated with code 4332 for that given cruise, and can be used later on as a check to ensure the HM records are being inserted properly.

#How many records were there in total (i.e. cells that are not NAs)
hm.l.long <- pivot_longer(hm.check.l, 
             cols = starts_with("X"),
             names_to = "Bin",
             names_prefix = "X",
             values_to = "Number",
             values_drop_na = TRUE)

hm.d.long <- pivot_longer(hm.check.d, 
                          cols = starts_with("X"),
                          names_to = "Bin",
                          names_prefix = "X",
                          values_to = "Number",
                          values_drop_na = TRUE)


nrow(hm.l.long)+nrow(hm.d.long)

# SPATIAL CHECKS ----------------------------------------------------------
### tow data spatial checks
### use SCSTRATAINFO to make sure that they don't cross strata
### calculate distances between tow start/end to make sure they are within 1km-ish
# cruise <- "BI2019"
# direct <- "Y:/INSHORE SCALLOP/Survey/"
# desktop="C:/Users/keyserf/Desktop/"

#Check for strange Lat Long outliers (Can check these spatially in the plots below)

start_long_less6500 <- num.tows %>% filter(Start_long < abs(6500))
min(start_long_less6500$Start_long)
which(start_long_less6500$Start_long == min(start_long_less6500$Start_long)) #Which row is this?

end_long_less6500 <- num.tows %>% filter(End_long < abs(6500))
which(end_long_less6500$End_long == min(end_long_less6500$End_long)) #Which row is this?

end_lat_less4300 <- num.tows %>% filter(End_lat < abs(4300))
min(end_lat_less4300$End_lat) #warning if none.
which(end_lat_less4300$End_lat == min(end_lat_less4300$End_lat)) #Which row is this?

end_lat_more4600 <- num.tows %>% filter(End_lat > abs(4600))
min(end_lat_more4600$End_lat)#warning if none.
which(end_lat_more4600$End_lat == min(end_lat_more4600$End_lat)) #Which row is this?

end_long_less6500 <- num.tows %>% filter(End_long < abs(6500))
min(end_long_less6500$End_long)
which(end_long_less6500$End_long == min(end_long_less6500$End_long)) #Which row is this?


 ######## NEW (Added in 2023) ########
 #Can run in place of check.tows.spatial function (check.tows.spatial function kept below)
 
 # ----- Checking Tow length --------------------------------------------------------------------
 
 check.tows <- num.tows |> dplyr::select(Oracle.tow.., Strata_id, Start_lat, Start_long, End_lat, End_long, Tow_type_id)
 check.tows$Start_lat <- convert.dd.dddd(format="dec.deg", x=check.tows$Start_lat)
 check.tows$Start_long <- convert.dd.dddd(format="dec.deg", x=check.tows$Start_long)
 check.tows$End_lat <- convert.dd.dddd(format="dec.deg", x=check.tows$End_lat)
 check.tows$End_long <- convert.dd.dddd(format="dec.deg", x=check.tows$End_long)
 check.tows$Start_long <- -check.tows$Start_long
 check.tows$End_long <- -check.tows$End_long
 
 #First make sf object - convert to linestring with start and end coords:
 #Move start and end coords into same column
 check.tows.start <- check.tows  %>% 
   dplyr::select(-End_long,-End_lat) %>% #remove end coords
   dplyr::rename(LAT = Start_lat) %>% 
   dplyr::rename(LONG = Start_long) %>% 
   mutate(POSITION = "START")
 
 check.tows.end <- check.tows  %>% 
   dplyr::select(-Start_long,-Start_lat) %>% #remove Start coords
   dplyr::rename(LAT = End_lat) %>% 
   dplyr::rename(LONG = End_long) %>% 
   mutate(POSITION = "END")
 
 check.tows <- rbind(check.tows.start, check.tows.end) %>% 
   arrange(Oracle.tow..)
 
 #Convert dataframe to sf points to lines and calculate tow length:
 check.tows$Oracle.tow.. <- as.factor(check.tows $Oracle.tow..)
 check.tows.sf <- st_as_sf(check.tows , coords = c("LONG", "LAT"), crs = 4326) %>% 
   st_transform(crs = 32620) %>% 
   group_by(Oracle.tow..) %>%
   dplyr::summarize(do_union=FALSE) %>% 
   st_cast("LINESTRING") 
 
 check.tows.sf <- check.tows.sf |> 
   mutate(tow.length_calculated = st_length(check.tows.sf)) |> 
   mutate(tow.length_calculated = drop_units(tow.length_calculated)) |> 
   mutate(tow.length_survey = num.tows$Tow_len)
 
 # flag the tow if the distance is greater than 2 km
 check.tows.sf$diff <- check.tows.sf$tow.length_calculated - check.tows.sf$tow.length_survey
 check.tows.sf$flag <- ifelse(check.tows.sf$diff > 100, "check", 
                              ifelse(check.tows.sf$diff < -100, "check", "ok"))
 #Save for record:
 #st_write(check.tows.sf |> st_drop_geometry(), paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_flagged_tows_new.csv"))
 
 #Check these tows!
 check.tows.sf |> filter(flag == "check")
 
 # ----- Spatial plots of tows to check --------------------------------------------------------------------
 
 #What strata shapefile to plot?
 if (CRUISE == "SFA29") {
   Strata.sf <- SFA29
 } else if (CRUISE == "BF") {
   Strata.sf <- rbind(SPA1A, SPA1B, SPA4, SPA5)
 } else if (CRUISE == "BI") {
   Strata.sf <- SPA3
 } else {
   Strata.sf <- rbind(SPA6A, SPA6B, SPA6C, SPA6D)
 }
 
 #First Visually inspect all tows to ensure they are within the boundaries of the SPAs/SFAs.
 
 #For interactive map use Mapview to inspect:
 mapview::mapview(check.tows.sf, zcol = "flag")+
   mapview::mapview(Strata.sf, col.regions = "transparent")
 
 
 #GGPLOT - Plot just the tows to check and save plot

 #Plot and save

 ggplot()+
   geom_sf(data = Strata.sf) +
   geom_sf(data = check.tows.sf |> filter(flag == "check"), aes(colour = Oracle.tow..))+
   geom_sf_text(data = check.tows.sf |> filter(flag == "check"), aes(label = Oracle.tow..),
                nudge_x=0.02, nudge_y = 0) #adjust nudge if needed to view tow line properly
 #save
ggsave(filename = paste0("Y:/Inshore/Survey/", year,"/DataEntry/",CRUISE, year,"/",CRUISE,year,"_strata_check.png"), plot = last_plot(), scale = 2.5, width =8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

 #For interactive map use Mapview to inspect tows to check:
 mapview::mapview(check.tows.sf |> filter(Oracle.tow.. == 36), zcol = "Oracle.tow..")+
   mapview::mapview(Strata.sf)
 
 plot.tows <- check.tows.sf |> filter(Oracle.tow..%in% c(81:91))
 mapview::mapview(plot.tows, zcol = "Oracle.tow..")+
   mapview::mapview(BF.strata)
 
 #For interactive map use Mapview to inspect:
 mapview::mapview(check.tows.sf |> filter(flag == "check"), zcol = "Oracle.tow..")+
   mapview::mapview(Strata.sf)
 
 # ----- REPEAT TOWS - CHECK OVERLAP --------------------------------------------------------------------

 if(CRUISE != "SFA29"){
   
repeatslastyear <- read.csv(paste0(direct, as.numeric(year)-1, "/DataEntry/",CRUISE,year-1,"/",CRUISE,year-1,"tow_CONVERTED.csv"))

#repeatslastyear <- read.csv(paste0(direct, as.numeric(year)-1, "/data entry templates and examples/",CRUISE,year-1,"/",CRUISE,year-1,"tow_CONVERTED.csv"))

rep.comp.tow <- read.csv(paste0(direct, year, "/DataEntry/",CRUISE,year,"/",CRUISE,year,"_REPCOMPTOW.csv"))
 
tows.for.comparison <- rep.comp.tow$REF.TOW_NO

repeatslastyear<- repeatslastyear |> dplyr::select(Oracle.tow.., Strata_id, Start_lat, Start_long, End_lat, End_long, Tow_type_id) |> filter(Oracle.tow.. %in% tows.for.comparison)
   repeatslastyear$Start_lat <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$Start_lat)
   repeatslastyear$Start_long <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$Start_long)
   repeatslastyear$End_lat <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$End_lat)
   repeatslastyear$End_long <- convert.dd.dddd(format="dec.deg", x=repeatslastyear$End_long)
   repeatslastyear$Start_long <- -repeatslastyear$Start_long
   repeatslastyear$End_long <- -repeatslastyear$End_long
   
   #First make sf object - convert to linestring with start and end coords:
   #Move start and end coords into same column
   repeatslastyear.start <- repeatslastyear  %>% 
     dplyr::select(-End_long,-End_lat) %>% #remove end coords
     dplyr::rename(LAT = Start_lat) %>% 
     dplyr::rename(LONG = Start_long) %>% 
     mutate(POSITION = "START")
   
   repeatslastyear.end <- repeatslastyear  %>% 
     dplyr::select(-Start_long,-Start_lat) %>% #remove Start coords
     dplyr::rename(LAT = End_lat) %>% 
     dplyr::rename(LONG = End_long) %>% 
     mutate(POSITION = "END")
   
   repeatslastyear <- rbind(repeatslastyear.start, repeatslastyear.end) %>% 
     arrange(Oracle.tow..)
   
   #Convert dataframe to sf points to lines and calculate tow length:
   repeatslastyear$Oracle.tow.. <- as.factor(repeatslastyear$Oracle.tow..)
   repeatslastyear.sf <- st_as_sf(repeatslastyear , coords = c("LONG", "LAT"), crs = 4326) %>% 
     st_transform(crs = 32620) %>% 
     group_by(Oracle.tow..) %>%
     dplyr::summarize(do_union=FALSE) %>% 
     st_cast("LINESTRING") 
   
  current.year.repeat.sf <- st_as_sf(check.tows |> filter(Tow_type_id == 5) , coords = c("LONG", "LAT"), crs = 4326) |> 
    st_transform(crs = 32620) %>% 
    group_by(Oracle.tow..) %>%
    dplyr::summarize(do_union=FALSE) %>% 
    st_cast("LINESTRING") 

  #PLot - shouldn't see any red tows...
   ggplot()+
     geom_sf(data = repeatslastyear.sf, colour = "red") +
     geom_sf(data = current.year.repeat.sf, colour = "blue")
     
   #For interactive map use Mapview to inspect:
   mapview::mapview(repeatslastyear.sf, color = "red")+
     mapview::mapview(current.year.repeat.sf, color = "blue")    
 }
 

 # ----- Check that Strata entered matches where the tow is --------------------------------------------------------------------
 
 if (CRUISE == "SFA29") { #Uses SFA29 Shapefile
   check.strata <- num.tows |>
     mutate(lat = convert.dd.dddd(Start_lat)) %>% #Convert to DD
     mutate(lon = convert.dd.dddd(-Start_long)) |>  #Convert to DD
     st_as_sf(coords = c("lon","lat"), crs= 4326)
   
   
check.strata <- st_intersection(Strata.sf,check.strata)

check.strata$flag <- ifelse(check.strata$ET_ID != check.strata$Strata_id, "check", 
                             ifelse(check.strata$ET_ID == check.strata$Strata_id, "ok"))
 } else { #Uses strata shapefile for all of BoF.
   check.strata <- num.tows |>
     mutate(lat = convert.dd.dddd(Start_lat)) %>% #Convert to DD
     mutate(lon = convert.dd.dddd(-Start_long)) |>  #Convert to DD
     st_as_sf(coords = c("lon","lat"), crs= 4326)
   
   
   check.strata <- st_intersection(BF.strata,check.strata)
   

   check.strata$flag <- ifelse(check.strata$ET_ID != check.strata$Strata_id, "check","ok")

   check.strata$flag <- ifelse(check.strata$ET_ID != check.strata$Strata_id, "check", 
                               ifelse(check.strata$ET_ID == check.strata$Strata_id, "ok"))

}
  

check.strata |> filter(flag == "check") #Check these strata entries!

 # -----Run check.tows.spatial function --------------------------------------------------------------------
#**WONT NEED TO RUN IF THE SECTION ABOVE HAS BEEN DONE**# 


 # Produces the following files for review:
 # - CRUISE_repeat_check.pdf
 # - CRUISE_SPA_check.pdf
 # - CRUISE_strata_check
 # - CRUISE_flagged_tows.csv
 # - Will also return the area and strata objects added within the function for further investigating any flagged tows.
 
 check.tows.spatial(cruise= paste0(CRUISE,year), year=year, direct="Y:/Inshore/Survey/", desktop="NULL", 
                    previouscruisefolder = paste0("data entry templates and examples/",CRUISE,year-1), previouscruisename = paste0(CRUISE,year-1), plot=TRUE, df=TRUE)
 
 
 #enter flag.tows_CRUISE produced by the check.tows.spatial function. 
 #find flagged.tows object in global enviro:
 for(obj in ls(pattern = "flagged.tows_")) #name changes every year and every cruise so search re-assign the object name with this:
   flagged.tows <- print(get(obj))
 
 #Enter the tow number indicated in the flagged_tows file 
 flagged.tows[flagged.tows$Oracle.tow..==4,]
 
 #Plot (add more tows if needed)
 ggplot() + 
   geom_polygon(data=area, aes(Longitude, Latitude, group=AREA_ID, fill=Area), colour="black") +
   geom_segment(data=flagged.tows[flagged.tows$Oracle.tow..==4,], aes(x=Start_long, y=Start_lat, xend=End_long, yend=End_lat), lwd=2)+
   geom_segment(data=flagged.tows[flagged.tows$Oracle.tow..==11,], aes(x=Start_long, y=Start_lat, xend=End_long, yend=End_lat), lwd=2)+
   geom_segment(data=flagged.tows[flagged.tows$Oracle.tow..==13,], aes(x=Start_long, y=Start_lat, xend=End_long, yend=End_lat), lwd=2)+
   geom_segment(data=flagged.tows[flagged.tows$Oracle.tow..==83,], aes(x=Start_long, y=Start_lat, xend=End_long, yend=End_lat), lwd=2)
 
## Need a way to handle overlapping strata! E.g. 31 and 32. THIS WAS ADDRESSED KIND OF...
# ggplot() + geom_polygon(data=strata[strata$STRATA_ID %in% c(31,32),], aes(LONGITUDE, LATITUDE, group=STRATA_ID), fill=NA, colour="black")

test <- subset(double.strata, strata==32)
length(unique(test$Oracle.tow..))

test2<- subset(unique(double.strata[,1:30]), Strata_id==32)
length(unique(test2$Oracle.tow..))

#FOR GM CRUISES ONLY
# VMS strata check
VMS.test[VMS.test$VMS=="check",]
ggplot() + 
  geom_polygon(data=otherVMS, aes(X, Y, group=PID), fill="yellow", alpha=0.5)+
  geom_polygon(data=inVMS, aes(X, Y, group=SID), fill="red",alpha=0.5) +
  geom_polygon(data=outVMS, aes(X, Y, group=SID), fill="blue", alpha=0.5) + 
  geom_path(data=towpath[towpath$Oracle.tow..%in% c(VMS.test[VMS.test$VMS=="check",]$Oracle.tow..),], 
            aes(LONGITUDE, LATITUDE, group=Oracle.tow..)) +
  geom_text(data=bftows[bftows$Oracle.tow..%in% c(VMS.test[VMS.test$VMS=="check",]$Oracle.tow..),],
            aes(Start_long, Start_lat, label=Oracle.tow..))+
  theme_bw() + theme(panel.grid=element_blank()) +
  coord_map() +
  xlim(-67.2, -66.4) +
  ylim(44.4, 45.1)


# ----temperature data matching to tows------------------------------------------------------------

source("Y:/Inshore/Survey/TemperatureDataScripts/Extract_survey_temperatures_function.r")
survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2019/", cruise = "BI2019", num.temps=4,
                    survey_time = "start", tow_duration=19, fig="pdf", export=T)
survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2019/", cruise = "BF2019", num.temps=4,
                    survey_time = "start", tow_duration=16, fig="pdf", export=T)
# survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2018/", cruise = "GM2018", num.temps=4,
#                     survey_time = "start", tow_duration=14, fig="pdf", export=T)
# survey.bottom.temps(direct = "Y:/Inshore scallop/Survey/2018/", cruise = "SFA292018", num.temps=4,
#                     survey_time = "start", tow_duration=14, fig="pdf", export=T)

# In BF2019, Tows 273, 274 and 275 had a reversed temperature profile. The seafloor was WARMER than the surface. For these 3 tows, we have to calculate temperature manually.
# To do this, insert a browser() on line 128 of the function. When it stops at the browser, do the following:
tow273 <- arrange(tempstows[tempstows$Tow==273 & !is.na(tempstows$Tow),], Temperature)
mean(tow273[(length(tow273$Temperature)-3) : length(tow273$Temperature),]$Temperature)
#14.04

tow274 <- arrange(tempstows[tempstows$Tow==274 & !is.na(tempstows$Tow),], Temperature)
mean(tow274[(length(tow274$Temperature)-3) : length(tow274$Temperature),]$Temperature)
#14.21

tow275 <- arrange(tempstows[tempstows$Tow==275 & !is.na(tempstows$Tow),], Temperature)
mean(tow275[(length(tow275$Temperature)-3) : length(tow275$Temperature),]$Temperature)
# 13.86

# Tows in SFA292019 with reversed temp profile:

tow9 <- arrange(tempstows[tempstows$Tow==9 & !is.na(tempstows$Tow),], Temperature)
mean(tow9[(length(tow9$Temperature)-3) : length(tow9$Temperature),]$Temperature)
#10.605
tow32 <- arrange(tempstows[tempstows$Tow==32 & !is.na(tempstows$Tow),], Temperature)
mean(tow32[(length(tow32$Temperature)-3) : length(tow32$Temperature),]$Temperature)
#12.325
tow35 <- arrange(tempstows[tempstows$Tow==35 & !is.na(tempstows$Tow),], Temperature)
mean(tow35[(length(tow35$Temperature)-3) : length(tow35$Temperature),]$Temperature)
#12
tow36 <- arrange(tempstows[tempstows$Tow==36 & !is.na(tempstows$Tow),], Temperature)
mean(tow36[(length(tow36$Temperature)-3) : length(tow36$Temperature),]$Temperature)
#12.2475
tow68 <- arrange(tempstows[tempstows$Tow==68 & !is.na(tempstows$Tow),], Temperature)
mean(tow68[(length(tow68$Temperature)-3) : length(tow68$Temperature),]$Temperature)
#10.29
tow69 <- arrange(tempstows[tempstows$Tow==69 & !is.na(tempstows$Tow),], Temperature)
mean(tow69[(length(tow69$Temperature)-3) : length(tow69$Temperature),]$Temperature)
#11.0375
tow70 <- arrange(tempstows[tempstows$Tow==70 & !is.na(tempstows$Tow),], Temperature)
mean(tow70[(length(tow70$Temperature)-3) : length(tow70$Temperature),]$Temperature)
#10.8825
tow71 <- arrange(tempstows[tempstows$Tow==71 & !is.na(tempstows$Tow),], Temperature)
mean(tow71[(length(tow71$Temperature)-3) : length(tow71$Temperature),]$Temperature)
#11.325
tow72 <- arrange(tempstows[tempstows$Tow==72 & !is.na(tempstows$Tow),], Temperature)
mean(tow72[(length(tow72$Temperature)-3) : length(tow72$Temperature),]$Temperature)
#11.51
tow73 <- arrange(tempstows[tempstows$Tow==73 & !is.na(tempstows$Tow),], Temperature)
mean(tow73[(length(tow73$Temperature)-3) : length(tow73$Temperature),]$Temperature)
#11.6525
tow74 <- arrange(tempstows[tempstows$Tow==74 & !is.na(tempstows$Tow),], Temperature)
mean(tow74[(length(tow74$Temperature)-3) : length(tow74$Temperature),]$Temperature)
#11.4
tow75 <- arrange(tempstows[tempstows$Tow==75 & !is.na(tempstows$Tow),], Temperature)
mean(tow75[(length(tow75$Temperature)-3) : length(tow75$Temperature),]$Temperature)
#11.6
tow89 <- arrange(tempstows[tempstows$Tow==89 & !is.na(tempstows$Tow),], Temperature)
mean(tow89[(length(tow89$Temperature)-3) : length(tow89$Temperature),]$Temperature)
#12.7675
tow90 <- arrange(tempstows[tempstows$Tow==90 & !is.na(tempstows$Tow),], Temperature)
mean(tow90[(length(tow90$Temperature)-3) : length(tow90$Temperature),]$Temperature)
#12.67
tow91 <- arrange(tempstows[tempstows$Tow==91 & !is.na(tempstows$Tow),], Temperature)
mean(tow91[(length(tow91$Temperature)-3) : length(tow91$Temperature),]$Temperature)
#12.5
tow92 <- arrange(tempstows[tempstows$Tow==92 & !is.na(tempstows$Tow),], Temperature)
mean(tow92[(length(tow92$Temperature)-3) : length(tow92$Temperature),]$Temperature)
#12.7375
tow93 <- arrange(tempstows[tempstows$Tow==93 & !is.na(tempstows$Tow),], Temperature)
mean(tow93[(length(tow93$Temperature)-3) : length(tow93$Temperature),]$Temperature)
#12.57
tow94 <- arrange(tempstows[tempstows$Tow==94 & !is.na(tempstows$Tow),], Temperature)
mean(tow94[(length(tow94$Temperature)-3) : length(tow94$Temperature),]$Temperature)
#12.5175
tow95 <- arrange(tempstows[tempstows$Tow==95 & !is.na(tempstows$Tow),], Temperature)
mean(tow95[(length(tow95$Temperature)-3) : length(tow95$Temperature),]$Temperature)
#12.47
tow96 <- arrange(tempstows[tempstows$Tow==96 & !is.na(tempstows$Tow),], Temperature)
mean(tow96[(length(tow96$Temperature)-3) : length(tow96$Temperature),]$Temperature)
#12.56
tow97 <- arrange(tempstows[tempstows$Tow==97 & !is.na(tempstows$Tow),], Temperature)
mean(tow97[(length(tow97$Temperature)-3) : length(tow97$Temperature),]$Temperature)
#12.52
tow98 <- arrange(tempstows[tempstows$Tow==98 & !is.na(tempstows$Tow),], Temperature)
mean(tow98[(length(tow98$Temperature)-3) : length(tow98$Temperature),]$Temperature)
#12.49
tow100 <- arrange(tempstows[tempstows$Tow==100 & !is.na(tempstows$Tow),], Temperature)
mean(tow100[(length(tow100$Temperature)-3) : length(tow100$Temperature),]$Temperature)
#12.95
tow103 <- arrange(tempstows[tempstows$Tow==103 & !is.na(tempstows$Tow),], Temperature)
mean(tow103[(length(tow103$Temperature)-3) : length(tow103$Temperature),]$Temperature)
#12.54
tow104 <- arrange(tempstows[tempstows$Tow==104 & !is.na(tempstows$Tow),], Temperature)
mean(tow104[(length(tow104$Temperature)-3) : length(tow104$Temperature),]$Temperature)
#12.47
tow105 <- arrange(tempstows[tempstows$Tow==105 & !is.na(tempstows$Tow),], Temperature)
mean(tow105[(length(tow105$Temperature)-3) : length(tow105$Temperature),]$Temperature)
#12.5
tow106 <- arrange(tempstows[tempstows$Tow==106 & !is.na(tempstows$Tow),], Temperature)
mean(tow106[(length(tow106$Temperature)-3) : length(tow106$Temperature),]$Temperature)
#12.52
tow107 <- arrange(tempstows[tempstows$Tow==107 & !is.na(tempstows$Tow),], Temperature)
mean(tow107[(length(tow107$Temperature)-3) : length(tow107$Temperature),]$Temperature)
#12.51
tow108 <- arrange(tempstows[tempstows$Tow==108 & !is.na(tempstows$Tow),], Temperature)
mean(tow108[(length(tow108$Temperature)-3) : length(tow108$Temperature),]$Temperature)
#12.39
tow109 <- arrange(tempstows[tempstows$Tow==109 & !is.na(tempstows$Tow),], Temperature)
mean(tow109[(length(tow109$Temperature)-3) : length(tow109$Temperature),]$Temperature)
#12.31
tow111 <- arrange(tempstows[tempstows$Tow==111 & !is.na(tempstows$Tow),], Temperature)
mean(tow111[(length(tow111$Temperature)-3) : length(tow111$Temperature),]$Temperature)
#12.45