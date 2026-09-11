###
###  SPA6 Shell Height Frequencies
###    
###    J.Sameoto July 2021

# ---- Prep work ---- 
options(stringsAsFactors = FALSE)

# required packages
library(ROracle)
library(PEDstrata) #v.1.0.2
library(tidyverse)
library(ggplot2)
library(PBSmapping)
library(lubridate)
library(sf)
library(mapview)

#Read in functions from Github
funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r" 
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# ///.... DEFINE THESE ENTRIES ....////

# Define:
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "6"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

#polygon to for assingning new strata to data #To bring in sf object from Github eventually - will need to identify in and out VMS data points.
inVMS <- read.csv("Y:/Inshore/Assessment/BoF/2015/SPA6/Survey/SPA6_VMS_IN_R_final_MOD.csv")
#inVMS <- read.csv("Y:/Inshore/BoF/2015/SPA6/Survey/SPA6_VMS_IN_R_final_MOD.csv")
outvms <- read.csv("Y:/Inshore/Assessment/BoF/2015/SPA6/Survey/SPA6_VMS_OUT_R_final_MOD.csv")
#outvms <- read.csv("Y:/Inshore/BoF/2015/SPA6/Survey/SPA6_VMS_OUT_R_final_MOD.csv")

#Read in inshore boundaries as sf objects (for checking)
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
outvms.sf <- st_read(paste0(temp2, "/SPA6_VMSstrata_OUT_2015.shp"))
inVMS.sf <- st_read(paste0(temp2, "/SPA6_VMSstrata_IN_2015.shp"))

#////... END OF DEFINE SECTION ...////


# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

### Livefreq data ###
#SQL query 2
quer2 <- "SELECT * 			                
FROM scallsur.scliveres s			
LEFT JOIN                         
(SELECT tow_date, cruise, tow_no     		
FROM SCALLSUR.sctows) t             
on (s.cruise = t.cruise and s.tow_no = t.tow_no)
where strata_id in (30, 31, 32)"

livefreq <- dbGetQuery(chan, quer2)

livefreq <- livefreq[,1:51]
livefreq$YEAR <- year(livefreq$TOW_DATE) 
livefreq$lat <- convert.dd.dddd(livefreq$START_LAT)
livefreq$lon <- convert.dd.dddd(livefreq$START_LONG)
livefreq.all <- livefreq

livefreq.all$lat <- convert.dd.dddd(livefreq.all$START_LAT)
livefreq.all$lon <- convert.dd.dddd(livefreq.all$START_LONG)
livefreq.all$ID <- 1:nrow(livefreq.all)

#identify tows "inside" the VMS strata and call them "IN" and tows "outside", "OUT".
livefreq.all$VMSSTRATA <- ""
events <- subset(livefreq.all,STRATA_ID%in%30:32,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
livefreq.all$VMSSTRATA[livefreq.all$ID%in%findPolys(events,inVMS)$EID] <- "IN"
livefreq.all$VMSSTRATA[livefreq.all$ID%in%findPolys(events,outvms)$EID] <- "OUT"

livefreq <- livefreq.all
livefreq$CruiseID <- paste0(livefreq$CRUISE,"." ,livefreq$TOW_NO)

###
# ---- Correct data-sets for errors ----
###

## LiveFreq errors 
#1. Tow 32 in 2011 (73 in 2010) not assinged to IN
#   Tow 70 in 2011 (90 in 2010) not assinged to IN
livefreq$VMSSTRATA[livefreq$TOW_NO==32 & livefreq$CRUISE == "GM2011"] <- "IN"
livefreq$VMSSTRATA[livefreq$TOW_NO==70 & livefreq$CRUISE == "GM2011"] <-"IN"

#2. In 2012 tow 9, 27, 111 not assinged to IN
livefreq$VMSSTRATA[livefreq$TOW_NO==9 & livefreq$CRUISE == "GM2012"] <- "IN"
livefreq$VMSSTRATA[livefreq$TOW_NO==27 & livefreq$CRUISE == "GM2012"] <-"IN"
livefreq$VMSSTRATA[livefreq$TOW_NO==111 & livefreq$CRUISE == "GM2012"] <-"IN"

#3. In 2013 tow 9, 27, 111 not assinged to IN
livefreq$VMSSTRATA[livefreq$TOW_NO==55 & livefreq$CRUISE == "GM2013"] <- "IN"
livefreq$VMSSTRATA[livefreq$TOW_NO==109 & livefreq$CRUISE == "GM2013"] <-"IN"
livefreq$VMSSTRATA[livefreq$TOW_NO==116 & livefreq$CRUISE == "GM2013"] <-"IN"

#4. In 2014 tow 9, 27, 111 not assinged to IN
livefreq$VMSSTRATA[livefreq$TOW_NO==31 & livefreq$CRUISE == "GM2014"] <- "IN"
livefreq$VMSSTRATA[livefreq$TOW_NO==42 & livefreq$CRUISE == "GM2014"] <-"OUT"

#5. In 2015 tow 20 not assigned to IN ( 2015 tow 20 links to 2014 tow 31 which links to a 2013 tow )
livefreq$VMSSTRATA[livefreq$TOW_NO==20 & livefreq$CRUISE == "GM2015"] <-"IN"
livefreq$VMSSTRATA[livefreq$TOW_NO==25 & livefreq$CRUISE == "GM2015"] <-"OUT"

#6. In 2016 tow 15,75,92 not assinged to OUT
livefreq$VMSSTRATA[livefreq$TOW_NO==15 & livefreq$CRUISE == "GM2016"] <-"OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==75 & livefreq$CRUISE == "GM2016"] <-"OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==92 & livefreq$CRUISE == "GM2016"] <-"OUT"

#7. In 2017 tow 15 (is a repeat of GM2016 tow 15) assigned to OUT
livefreq$VMSSTRATA[livefreq$TOW_NO==56 & livefreq$CRUISE == "GM2017"] <-"OUT"
#8 In 2017, tow 95 needs to be IN
livefreq$VMSSTRATA[livefreq$TOW_NO==95 & livefreq$CRUISE == "GM2017"] <- "IN"

#Clean up VMSSTRATA for GM2017
livefreq$VMSSTRATA[livefreq$TOW_NO==86 & livefreq$CRUISE == "GM2017"] <- "OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==23 & livefreq$CRUISE == "GM2017"] <- "OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==104 & livefreq$CRUISE == "GM2017"] <- "OUT"
livefreq$VMSSTRATA[livefreq$TOW_NO==120 & livefreq$CRUISE == "GM2017"] <- "OUT"

#8. In 2018 tow 40 (is a repeat of GM2017 tow 56) assigned to IN but needs to be OUT - a repeated repeat
livefreq$VMSSTRATA[livefreq$TOW_NO==40 & livefreq$CRUISE == "GM2018"] <-"OUT"

#9. In 2019 tow 95 assigned to IN but needs to be OUT 
livefreq$VMSSTRATA[livefreq$TOW_NO==95 & livefreq$CRUISE == "GM2019"] <-"OUT"

table(livefreq$VMSSTRATA)

#Check current years VMSSTRATA Assignment.
check.out.YYYY <- livefreq %>% filter(CRUISE == paste0("GM", surveyyear))%>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  filter(VMSSTRATA == "OUT")

check.in.YYYY <- livefreq %>% filter(CRUISE == paste0("GM", surveyyear))%>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  filter(VMSSTRATA == "IN")

#require(mapview)
mapview::mapview(check.out.YYYY) +
  mapview(outvms.sf, col.regions = "red")

mapview::mapview(check.in.YYYY) +
  mapview(inVMS.sf, col.regions = "green")
#  mapview::mapview(check.2019)


# ---- Shell height Frequency means by year ----

GMlivefreq.6.IN <- subset(livefreq, c(VMSSTRATA=="IN" & TOW_TYPE_ID==1)) #include tow type IDs 1 only since tow type 5 has different prob of selection!
SPA6.Inner.SHFmeans <- sapply(split(GMlivefreq.6.IN[c(11:50)], GMlivefreq.6.IN$YEAR), function(x){apply(x,2,mean)})
SPA6.Inner.SHFmeans <- as.data.frame(SPA6.Inner.SHFmeans)
#write.csv(SPA6.SHFmeans.IN, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA6.SHF.",surveyyear,".IN.csv")) 

GMlivefreq.6.OUT <- subset(livefreq, c(VMSSTRATA=="OUT"  & TOW_TYPE_ID==1)) #include tow type IDs 1 only since tow type 5 has different prob of selection!
SPA6.Outer.SHFmeans <- sapply(split(GMlivefreq.6.OUT[c(11:50)], GMlivefreq.6.OUT$YEAR), function(x){apply(x,2,mean)})
SPA6.Outer.SHFmeans <- as.data.frame(SPA6.Outer.SHFmeans)
#write.csv(SPA6.SHFmeans.OUT, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA6.SHF.",surveyyear,".OUT.csv")) 

# read in shell height data (created from file: SPA6_2015_VMS_Indicies_IN_2019.csv)
#	SPA6.Inner.SHFmeans  <- read.csv(paste0('Y:/INSHORE SCALLOP/BoF/dataoutput/SPA6.SHF.',maxyear,'.IN.csv'), row.names = "X") #update output file name#
#	SPA6.Outer.SHFmeans <- read.csv(paste0('Y:/INSHORE SCALLOP/BoF/dataoutput/SPA6.SHF.',maxyear,'.OUT.csv'), row.names = "X") #update output file name
	
# ---- PLOT SHF FOR EACH YEAR BY STRATA ----
##.. 1) IN VMS AREA 
#SPA3.Inner.for.plot <- data.frame(bin.label = row.names(SPA3.Inner.SHFmeans), SPA3.Inner.SHFmeans)
SPA6.Inner.SHFmeans$"2020" <- NA
SPA6.Inner.SHFmeans$"2020" <- as.numeric(SPA6.Inner.SHFmeans$"2020")
head(SPA6.Inner.SHFmeans)
SPA6.Inner.SHFmeans$bin.mid.pt <- seq(2.5,200,by=5)

SPA6.InVMS.SHFmeans.for.plot <- pivot_longer(SPA6.Inner.SHFmeans, 
                                    cols = -c("bin.mid.pt"),
                                    names_to = "year",
                                    names_prefix = "X",
                                    values_to = "SH",
                                    values_drop_na = FALSE)
SPA6.InVMS.SHFmeans.for.plot$year <- as.numeric(SPA6.InVMS.SHFmeans.for.plot$year)

SPA6.InVMS.SHFmeans.for.plot <- SPA6.InVMS.SHFmeans.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA6.InVMS.SHFmeans.for.plot$SH <- round(SPA6.InVMS.SHFmeans.for.plot$SH,3)

ylimits <- c(0,max(SPA6.InVMS.SHFmeans.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA6.IN.SHF <- ggplot() + geom_col(data = SPA6.InVMS.SHFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA6.IN.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA6_SHF_InVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA6.IN.SHF)
dev.off()

##.. 1) OUT VMS AREA 
#SPA3.Inner.for.plot <- data.frame(bin.label = row.names(SPA3.Inner.SHFmeans), SPA3.Inner.SHFmeans)
SPA6.Outer.SHFmeans$"2020" <- NA
SPA6.Outer.SHFmeans$"2020" <- as.numeric(SPA6.Inner.SHFmeans$"2020")
head(SPA6.Outer.SHFmeans)
SPA6.Outer.SHFmeans$bin.mid.pt <- seq(2.5,200,by=5)

SPA6.OutVMS.SHFmeans.for.plot <- pivot_longer(SPA6.Outer.SHFmeans, 
                                             cols = -c("bin.mid.pt"),
                                             names_to = "year",
                                             names_prefix = "X",
                                             values_to = "SH",
                                             values_drop_na = FALSE)
SPA6.OutVMS.SHFmeans.for.plot$year <- as.numeric(SPA6.OutVMS.SHFmeans.for.plot$year)

SPA6.OutVMS.SHFmeans.for.plot <- SPA6.OutVMS.SHFmeans.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA6.OutVMS.SHFmeans.for.plot$SH <- round(SPA6.OutVMS.SHFmeans.for.plot$SH,3)

ylimits <- c(0,max(SPA6.OutVMS.SHFmeans.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA6.OUT.SHF <- ggplot() + geom_col(data = SPA6.OutVMS.SHFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA6.OUT.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA6_SHF_OutVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA6.OUT.SHF)
dev.off()



# ---- CALCULATE lbar FOR SPA 6 IN and OUT VMSAREA TO BE USED FOR CALCULATING GROWTH ----

# ---- Lbar for INNER VMS STRATA ----
#commercial mean shell height in year t
SPA6.Inner.SHFmeans <- SPA6.Inner.SHFmeans %>% select(-c("bin.mid.pt"))
# Note for SPA 6 Do NOT have data in 2004 and very limited data in 2005 !!!

#for Inner VMS Strata - have data in 2005 (but limited)
#years <- c(1997:2003, 2005:2019)
years <-c(1997:2003, 2005:2019, 2021:surveyyear)

SPA6.SHactual.Com.IN<-rep(length(years))
for(i in 1:length(years)){
  temp.data <- SPA6.Inner.SHFmeans[c(17:40),0+i]  #commercial size  #Originally: SPA6.SHF[c(17:40),1+i]; WHy isn't Column set to 0+1 like for recruits???
  SPA6.SHactual.Com.IN[i]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data) }

SPA6.SHactual.Com.IN <- data.frame(years, SPA6.SHactual.Com.IN)
SPA6.SHactual.Com.IN <- rbind(SPA6.SHactual.Com.IN, c(2004,NA,NA))
SPA6.SHactual.Com.IN <- rbind(SPA6.SHactual.Com.IN, c(2020,NA,NA))
SPA6.SHactual.Com.IN <- SPA6.SHactual.Com.IN[order(SPA6.SHactual.Com.IN$years),]
SPA6.SHactual.Com.IN

SPA6.SHactual.Rec.IN <- rep(length(years))
for(i in 1:length(years)){
  temp.data <- SPA6.Inner.SHFmeans[c(14:16),0+i]  #recruit size
  SPA6.SHactual.Rec.IN[i]<-sum(temp.data*seq(67.5,77.5,by=5))/sum(temp.data) }

SPA6.SHactual.Rec.IN <- data.frame(years, SPA6.SHactual.Rec.IN)
SPA6.SHactual.Rec.IN <- rbind(SPA6.SHactual.Rec.IN, c(2004,NA,NA))
SPA6.SHactual.Rec.IN <- rbind(SPA6.SHactual.Rec.IN, c(2020,NA,NA))
SPA6.SHactual.Rec.IN <- SPA6.SHactual.Rec.IN[order(SPA6.SHactual.Rec.IN$years),]
SPA6.SHactual.Rec.IN

#---- Lbar for OUTER VMS STRATA ---- 

SPA6.Outer.SHFmeans <- SPA6.Outer.SHFmeans %>% select(-c("bin.mid.pt"))
#NOte for SPA 6 Do NOT have data in 2004 OR 2005 for OUTER strata

 
years <-c(1997:2003, 2006:2019, 2021:surveyyear)#For Outer VMS strata - no tows/data in 2005

SPA6.SHactual.Com.OUT <- rep(length(years))
for(i in 1:length(years)){
  temp.data <- SPA6.Outer.SHFmeans[c(17:40),0+i]  #commercial size  #Originally: SPA6.SHF[c(17:40),1+i]; WHy isn't Column set to 0+1 like for recruits???
  SPA6.SHactual.Com.OUT[i]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data) }

SPA6.SHactual.Com.OUT <- data.frame(years, SPA6.SHactual.Com.OUT)
SPA6.SHactual.Com.OUT <- rbind(SPA6.SHactual.Com.OUT, c(2004,NA,NA))
SPA6.SHactual.Com.OUT <- rbind(SPA6.SHactual.Com.OUT, c(2005,NA,NA)) 
SPA6.SHactual.Com.OUT <- rbind(SPA6.SHactual.Com.OUT, c(2020,NA,NA)) 
SPA6.SHactual.Com.OUT <- SPA6.SHactual.Com.OUT[order(SPA6.SHactual.Com.OUT$years),]
SPA6.SHactual.Com.OUT

SPA6.SHactual.Rec.OUT <- rep(length(years))
for(i in 1:length(years)){
  temp.data <- SPA6.Outer.SHFmeans[c(14:16),0+i]  #recruit size
  SPA6.SHactual.Rec.OUT[i]<-sum(temp.data*seq(67.5,77.5,by=5))/sum(temp.data) }

SPA6.SHactual.Rec.OUT <- data.frame(years, SPA6.SHactual.Rec.OUT)
SPA6.SHactual.Rec.OUT <- rbind(SPA6.SHactual.Rec.OUT, c(2004,NA,NA))
SPA6.SHactual.Rec.OUT <- rbind(SPA6.SHactual.Rec.OUT, c(2005,NA,NA))
SPA6.SHactual.Rec.OUT <- rbind(SPA6.SHactual.Rec.OUT, c(2020,NA,NA)) 
SPA6.SHactual.Rec.OUT <- SPA6.SHactual.Rec.OUT[order(SPA6.SHactual.Rec.OUT$years),]
SPA6.SHactual.Rec.OUT

sh.actual.in <- data.frame(SPA6.SHactual.Com.IN, SPA6.SHactual.Rec.IN %>% dplyr::select(SPA6.SHactual.Rec.IN))

sh.actual.out <- data.frame(SPA6.SHactual.Com.OUT, SPA6.SHactual.Rec.OUT %>% dplyr::select(SPA6.SHactual.Rec.OUT))

#Inner
lbar.dat.IN <- merge(SPA6.SHactual.Com.IN, SPA6.SHactual.Rec.IN, by="years" )
lbar.dat.IN$VMSSTRATA <- "InsideVMS"
names(lbar.dat.IN)[c(2,3)]  <- c("SPA6.SHactual.Com", "SPA6.SHactual.Rec")
#Outer
lbar.dat.OUT <- merge(SPA6.SHactual.Com.OUT, SPA6.SHactual.Rec.OUT, by="years")
lbar.dat.OUT$VMSSTRATA <- "OutsideVMS"
names(lbar.dat.OUT)[c(2,3)] <- c("SPA6.SHactual.Com", "SPA6.SHactual.Rec")
lbar <- rbind(lbar.dat.IN, lbar.dat.OUT) 
lbar

#Also saved below:
#write.csv(lbar.dat.IN, paste0('Y:/INSHORE SCALLOP/BoF/dataoutput/SPA6lbarComRecINVMS.',maxyear,'.csv'))
#write.csv(lbar.dat.OUT, paste0('Y:/INSHORE SCALLOP/BoF/dataoutput/SPA6lbarComRecOUTVMS.',maxyear,'.csv'))

#
# ---- Expected mean shell height for year t+1 (eg., for year 1997 output is expected SH in 1998) ----
#
# use equation: Linfty*(1-exp(-exp(logK)))+exp(-exp(logK))*mean.shell.height
# inputs from Von B files; All SPA6 was modelled over all years (Y:\INSHORE SCALLOP\BoF\2016\Growth\SPA6_VonB_2016.R)

#>VONBF2016.nlme
#Nonlinear mixed-effects model fit by REML
#Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0)
#Data: test.data

#  Fixed effects: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#  Value Std.Error    DF t-value p-value
#  Linf 148.197    3.7847 77980  39.156  0.0000
#  logK  -1.576    0.0762 77980 -20.698  0.0000
#  T0    -0.366    0.1665 77980  -2.195  0.0281


# ---- Grow up lbar for each year (t) to find expected mean SH for year t+1 ----

# Set if using INNER VMSAREA: 
#the value in 1997 (first value) is the predicted height for 1998
SPA6.SHpredict.Com.IN <- data.frame(years=lbar.dat.IN$years, SPA6.SHpredict.Com.IN=rep(NA,length(lbar.dat.IN$years)))

for(i in 1:length(SPA6.SHpredict.Com.IN$years)){
  temp.data <- lbar$SPA6.SHactual.Com[lbar$VMSSTRATA=="InsideVMS"][i] #commercial size
  SPA6.SHpredict.Com.IN$SPA6.SHpredict.Com.IN[i] <- 148.197*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data }

#the value in 1997 (first value) is the predicted height for 1998
SPA6.SHpredict.Rec.IN <- data.frame(years=lbar.dat.IN$years, SPA6.SHpredict.Rec.IN=rep(NA,length(lbar.dat.IN$years)))

for(i in 1:length(SPA6.SHpredict.Rec.IN$years)){
  temp.data <- lbar$SPA6.SHactual.Rec[lbar$VMSSTRATA=="InsideVMS"][i]   #recruit size
  SPA6.SHpredict.Rec.IN$SPA6.SHpredict.Rec.IN[i] <- 148.197*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data}

SPA6.SHpredict.IN <- merge(SPA6.SHpredict.Com.IN, SPA6.SHpredict.Rec.IN, by="years")
SPA6.SHpredict.IN$years.predict <- SPA6.SHpredict.IN$years+1

# OUTTER VMS not currently used for modelling, but done here anyways.. 
SPA6.SHpredict.Com.OUT <- data.frame(years=lbar.dat.OUT$years, SPA6.SHpredict.Com.OUT=rep(NA,length(lbar.dat.OUT$years)))

for(i in 1:length(SPA6.SHpredict.Com.OUT$years)){
  temp.data <- lbar$SPA6.SHactual.Com[lbar$VMSSTRATA=="OutsideVMS"][i] #commercial size
  SPA6.SHpredict.Com.OUT$SPA6.SHpredict.Com.OUT[i]<-148.197*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data }


#the value in 1997 (first value) is the predicted height for 1998
SPA6.SHpredict.Rec.OUT <- data.frame(years=lbar.dat.OUT$years, SPA6.SHpredict.Rec.OUT=rep(NA,length(lbar.dat.OUT$years)))

for(i in 1:length(SPA6.SHpredict.Rec.OUT$years)){
  temp.data <- lbar$SPA6.SHactual.Rec[lbar$VMSSTRATA=="OutsideVMS"][i]  #recruit size
  SPA6.SHpredict.Rec.OUT$SPA6.SHpredict.Rec.OUT[i]<-148.197*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data}

SPA6.SHpredict.OUT <- merge(SPA6.SHpredict.Com.OUT, SPA6.SHpredict.Rec.OUT, by="years")
SPA6.SHpredict.OUT$years.predict <- SPA6.SHpredict.OUT$years+1
SPA6.SHpredict.OUT

#create predictions dataframe
sh.predict.in <- data.frame(SPA6.SHpredict.IN %>% dplyr::select(years, years.predict, SPA6.SHpredict.Com.IN, SPA6.SHpredict.Rec.IN))

sh.predict.out <- data.frame(SPA6.SHpredict.OUT %>% dplyr::select(years, years.predict, SPA6.SHpredict.Com.OUT, SPA6.SHpredict.Rec.OUT))

#export the objects to use in predicting mean weight
#IN VMSAREA
dump(c('sh.actual.in','sh.predict.in'), paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA6/SPA6.SHobj.IN.",surveyyear,".R"))

#export the objects to use in predicting mean weight
#OUT VMSAREA
dump(c('sh.actual.out','sh.predict.out'),paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA6/SPA6.SHobj.OUT.",surveyyear,".R"))

write.csv(cbind(sh.actual.in, sh.actual.out %>% dplyr::select(!years), sh.predict.in %>% dplyr::select(!years), sh.predict.out %>% dplyr::select(!c(years,years.predict))), paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA6/SPA6.lbar.by.strata.",surveyyear,".csv"))


# ---- Plot of lbar for SPA 6 ----

# Prep for plot: 
# Reshape into long format
SPA6.lbar.VMS <- lbar %>% dplyr::select(years, lbar = SPA6.SHactual.Com, Strata = VMSSTRATA)
  

# Plot of Mean Commercial SHell Height by Strata Group 
plot.SPA6.lbar.VMS <- ggplot(SPA6.lbar.VMS) + geom_line(aes(x = years, y = lbar, color = Strata)) + 
  geom_point(aes(x = years, y = lbar, color = Strata)) +
  theme_bw() + ylab("Mean Commercial Shell Height (mm)") + 
  scale_color_discrete(name="Strata", labels = c("VMSIN", "VMSOUT")) + 
  theme(legend.position = c(0.8, 0.2),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)) 
plot.SPA6.lbar.VMS 

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA6_lbar.png"), type="cairo", width=15, height=15, units = "cm", res=400)
print(plot.SPA6.lbar.VMS)
dev.off()



# Plot of Mean Commercial Shell Height (lbar)
#plot.SPA6.lbar <- ggplot(lbar, aes(x = years, y = SPA6.SHactual.Com)) + facet_wrap(~VMSSTRATA) +
#  geom_line() + 
#  geom_point() +
#  theme_bw() + ylab("Shell Height (mm)") + xlab("Year") + 
#  xlim(c(1997,surveyyear+1))
#plot.SPA6.lbar

# Save out plot
#png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA6_lbar.png"), type="cairo", width=15, height=10, units = "cm", res=400)
#print(plot.SPA6.lbar)
#dev.off()

SPA6.InVMS.SHFmeans.for.plot.1997to2008 <- SPA6.InVMS.SHFmeans.for.plot %>% filter(year < 2009)
SPA6.InVMS.SHFmeans.for.plot.2009to2021 <- SPA6.InVMS.SHFmeans.for.plot %>% filter(year >= 2009)

#shorten SH data for plot or else get warning when run ggplot 
SPA6.InVMS.SHFmeans.for.plot$SH <- round(SPA6.InVMS.SHFmeans.for.plot$SH,3)

ylimits <- c(0,max(SPA6.InVMS.SHFmeans.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA6.IN.SHF <- ggplot() + geom_col(data = SPA6.InVMS.SHFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA6.IN.SHF

