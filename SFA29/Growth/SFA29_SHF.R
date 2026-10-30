###........................................###
###
###    SFA 29West
###    Shell Height Frequencies
###
###    J. Sameoto Feb 2016
###.... Revised by DK Feb 2018 to reduce redunancy and minimize potential for error, script has been highly revised and outputs now differ, please don't kill
###     me world, but this should be easier in the end!
###   Revised further in Nov 2021 J.SAmeoto 
###
###........................................###

#NOTE - SHF for Subarea E newly added to this code  at end 

options(stringsAsFactors = FALSE)

#required packages
library(ROracle)
library(lubridate)
library(ggplot2)
library(reshape2)
library(PEDstrata)
library(tidyverse)

#setwd temporarily to grab these data...
setwd('Y:/Inshore/Assessment/BoF/Assessment_fns/SFA29W')
source('Geophysicalareas.R')
source('SedimentareasSFA29.R')
source('Domainestimates.R')
source('post.stratify.all.R')
source('SDMareas.R')
# source('SFA29sediment2005-2007.R') #tow numbers with assigned surficial substrata strata - done pre-Jessica (likely by J.Black) #keep this note as a reminder 
# Tow with surficial assignments From 2001 to 2013 done by J.Sameoto:
# Use new file: SFA2920012013_Surficial_FinalJuly162014.csv
surf.all <- read.csv('SFA2920012013_Surficial_FinalJuly162014.csv')
# note but will only use surficial data for 2005, 2006, 2007:
surf.all <- surf.all[,c("uid","surf")]
names(surf.all) <- c("uid","Start.Bottom")


# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)
#uid <- un.englishg
#pwd <- pw.englishg

surveyyear <- 2026  #This is the last survey year for which you want to include  - note should match year of cruise below 
cruise <- "SFA292026"  #note should match year for surveyyear set above 
assessmentyear <- 2027 #year in which you are conducting the survey 
path.directory <- "Y:/Inshore/Assessment/SFA29/"
years <- c(2001:2026) 

#Bring in survey tow data with SDM value (note - SFA29_SDM_LWM.R script must be run to get updated survey tows with SDM values prior to running this script)
sdmtows <- read.csv("Y:/Inshore/Assessment/SFA29/ScalSurv_SDM/SFA29Tows_SDM.csv")
table(sdmtows$CRUISE)
# Get an unique ID for the tows and keep only the columns we need for later...
sdmtows$uid <- paste(sdmtows$CRUISE, sdmtows$TOW_NO, sep=".")
sdmtows <- sdmtows %>% dplyr::select(uid, SDM)

# Assign SDM level (low,med,high) based on SDM length weighted mean values (mval_LWM)
#sdmtows$SDM <- NA
#sdmtows$SDM[sdmtows$sdmval_LWM < 0.3] <- "low"
#sdmtows$SDM[sdmtows$sdmval_LWM >= 0.3 & sdmtows$sdmval_LWM < 0.6] <- "med"
#sdmtows$SDM[sdmtows$sdmval_LWM >= 0.6] <- "high"


###
# read in shell height data from database
###

# Get the cruises
cruise.list <- paste0("SFA29",(2001:(surveyyear)))
cruise.list <- paste(cruise.list,collapse="','")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#numbers by shell height bin
quer2 <- paste(
  "SELECT *                                     ",
  "FROM scallsur.SCLIVERES                      ",
  "WHERE strata_id IN (41, 42, 43, 44, 45)      ",
  "AND (cruise in ('",cruise.list,"'))          ",
  sep=""
)
	
# Run for ROracle
  SFA29livefreq.dat <- dbGetQuery(chan, quer2)


# Assign subarea names
	strata.names <- paste0("SFA29",LETTERS[1:5])
	strata.ids <- sort(unique(SFA29livefreq.dat$STRATA_ID))
	SFA29livefreq.dat$STRATA <- NA
	for(i in 1:length(strata.ids)) SFA29livefreq.dat$STRATA[SFA29livefreq.dat$STRATA_ID==strata.ids[i]] <- strata.names[i]
	

#add YEAR and CRUISEID column to data
	SFA29livefreq.dat$YEAR <- as.numeric(substr(SFA29livefreq.dat$CRUISE,6,9))

# Calc unique id
	SFA29livefreq.dat$uid <- paste(SFA29livefreq.dat$CRUISE, SFA29livefreq.dat$TOW_NO, sep=".")

# Left join survey tows to SDM level on uid
	SFA29livefreq.dat <- merge(SFA29livefreq.dat, sdmtows, by.x='uid', by.y='uid', all.x=TRUE)
	dim(SFA29livefreq.dat)
	# In 2004 1 tow landed in the exceedlingly small part of SFA29A which is high, we remove this tow for subsequent analyses below...
	SFA29livefreq.dat[which(SFA29livefreq.dat$STRATA == "SFA29A" & SFA29livefreq.dat$SDM == "high"),]
	SFA29livefreq.dat <- SFA29livefreq.dat[-which(SFA29livefreq.dat$STRATA == "SFA29A" & SFA29livefreq.dat$SDM == "high"),]

# in 2024 - survey poirtub if B that was not covered by MBES - do not use for estimates these were exploratory; these were tows 3,4,5,6,7 
	SFA29livefreq.dat %>% filter(YEAR == 2024 & is.na(SDM) == TRUE)
	dim(SFA29livefreq.dat)
	SFA29livefreq.dat <- SFA29livefreq.dat[!(SFA29livefreq.dat$CRUISE == "SFA292024" & SFA29livefreq.dat$TOW_NO %in% c(3,4,5,6,7)), ]
	dim(SFA29livefreq.dat)
	SFA29livefreq.dat.all <- SFA29livefreq.dat

	
#---- SHF Calculations ----
# CALCULATE SHF FOR EACH YEAR BY SUBAREA AND SDM (High, Med, Low); Note: No high for Subarea A
# This is just a simple mean for each stratum 
setwd(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/"))  # this could be embedded in the loop code below.. but for now works here.. 
num.areas <- length(unique(SFA29livefreq.dat$STRATA))
areas <- unique(SFA29livefreq.dat$STRATA)
num.sdm <- length(unique(SFA29livefreq.dat$SDM[!is.na(SFA29livefreq.dat$SDM)]))
sdms <- unique(SFA29livefreq.dat$SDM[!is.na(SFA29livefreq.dat$SDM)])
shf.bins <- seq(0,195,by=5) # I'd like to automate this...
year.names <- c(paste0("X",2001:2019),paste0("X",2021:surveyyear)) # If the data frame names are numbers it just causes grief...
SHF_sdm_means <- NULL
# Select the colums with the SHF data in them, the strata name, and the SDM
cols <- c(grep("BIN_ID",names(SFA29livefreq.dat)))
# Run a loop to grab all the SHF data....
for(i in 1:num.areas)
{
  for(j in 1:num.sdm)
  {
    templivefreq <- subset(SFA29livefreq.dat, STRATA==areas[i] & TOW_TYPE_ID==1 & SDM==sdms[j])
    if(nrow(templivefreq) > 0 && areas[i] != "SFA29E")
    {
    SHF_sdm_means[[paste0(areas[i],"_",sdms[j])]] <- as.data.frame(sapply(split(templivefreq[cols], templivefreq$YEAR), function(x){apply(x,2,mean)}))
    colnames(SHF_sdm_means[[paste0(areas[i],"_",sdms[j])]]) <- year.names
    SHF_sdm_means[[paste0(areas[i],"_",sdms[j])]]$SDM <- sdms[j]
    SHF_sdm_means[[paste0(areas[i],"_",sdms[j])]]$STRATA <- areas[i]
    SHF_sdm_means[[paste0(areas[i],"_",sdms[j])]]$SHF.bin <- shf.bins
    # Output the results to a text file output...
   # write.csv(SHF_sdm_means[[paste0(areas[i],"_",sdms[j])]],paste0(areas[i],"_",sdms[j],".csv"))
    } # End the if loop to remove the cases for which we don't have information
  } # endfor(j in 1:num.sdm)
} # end for(i in 1:num.areas)

SHF_sdm_means <- do.call("rbind",SHF_sdm_means)
#missing SHF for 2020 - here likely the easiest spot to fill in -- add column for field X2020, but need to do so for all subareas and SDMs and bin e.g. 440 records - for subarea A only 2 SDMs (medium and low) x 40 bins + other 3 subareas x 3 SDMs x 40 bins = 440
SHF_sdm_means <- cbind(SHF_sdm_means, data.frame(X2020 = rep(NA,440)))

# Reshape these so we can make a pretty ggplot...
SHF_sdm_means_4_plot <- melt(SHF_sdm_means,id.vars = c("STRATA","SDM","SHF.bin"),variable.name = "year",value.name = "SHF")

# Get rid of the X's in the name...
SHF_sdm_means_4_plot$year <- as.numeric(substr(SHF_sdm_means_4_plot$year,2,5))

# Order the factor levels for SDM (for plotting)
SHF_sdm_means_4_plot$SDM <- factor (SHF_sdm_means_4_plot$SDM, levels = c("low", "med", "high"))
print(levels(SHF_sdm_means_4_plot$SDM))

#write out SHF data 
#write.csv(SHF_sdm_means_4_plot, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SHF.SDM.means.",surveyyear,".csv"))

SHF_sdm_means_4_plot <- SHF_sdm_means_4_plot |> 
  mutate(SUBAREA = case_when(STRATA == "SFA29A" ~ "Subarea A",
                             STRATA == "SFA29B" ~ "Subarea B",
                             STRATA == "SFA29C" ~ "Subarea C",
                             STRATA == "SFA29D" ~ "Subarea D"))#Add column SUBAREA for plot.

# Make the line plots of the SHF
strata.ids <- unique(SHF_sdm_means_4_plot$STRATA)
subarea.ids <- unique(SHF_sdm_means_4_plot$SUBAREA)
yr.max <- surveyyear
yr.min <- surveyyear-6

## SHF plots for last 6 years by subarea 
for(i in 1:length(strata.ids))
{
p <- ggplot(SHF_sdm_means_4_plot[SHF_sdm_means_4_plot$STRATA == strata.ids[i] & SHF_sdm_means_4_plot$year %in% yr.min:yr.max,],aes(SHF.bin,SHF,group=SDM)) + 
  geom_line(aes(group=SDM, linetype = SDM, color = SDM), size = 0.75) + facet_wrap(~year,ncol=1) + # geom_point(aes(shape=SDM)) + 
  scale_linetype_manual(values = c(1,2,4),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +  
  scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +  #scale_shape_manual(values = 1:3) +
  theme_bw() + theme(panel.grid=element_blank()) + scale_x_continuous(breaks = seq(0,170,20), limits = c (0,170)) +  #xlim(0, 170) + 
  geom_vline(xintercept=90, colour = "grey40") + geom_vline(xintercept=100, colour = "grey40") +
  xlab("Shell height") + ylab("Survey mean no./tow") + ggtitle(paste(subarea.ids[i],"Shell Height Frequency")) +
  theme(#plot.title = element_text(size=10,hjust=0.5), 
    legend.title = element_blank(),
    legend.margin = margin(0,0,0,0),
    legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)),
    legend.text = element_text(size=10), 
    legend.key.size = unit(0.53, "cm"), 
    legend.position = c(0.90, 0.95)) 
 windows(11,11); plot(p)
ggsave(paste0("SHF_",strata.ids[i],".png"), path=paste0(path.directory,"/",assessmentyear,"/Assessment/Figures/Growth/"), type="cairo", width=18, height=24, units = "cm", dpi=400)   
} # end for(i in 1:length(strata.ids))



## SHF plots for last 10 years by subarea  
yr.min <- surveyyear-10

for(i in 1:length(strata.ids))
{
  p <- ggplot(SHF_sdm_means_4_plot[SHF_sdm_means_4_plot$STRATA == strata.ids[i] & SHF_sdm_means_4_plot$year %in% yr.min:yr.max,],aes(SHF.bin,SHF,group=SDM)) + 
    geom_line(aes(group=SDM, linetype = SDM, color = SDM), size = 0.75) + facet_wrap(~year,ncol=1) + # geom_point(aes(shape=SDM)) + 
    scale_linetype_manual(values = c(1,2,4),breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +  
    scale_color_manual(values=c('firebrick2', 'darkgrey', 'darkblue'), breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +  #scale_shape_manual(values = 1:3) +
    theme_bw() + theme(panel.grid=element_blank()) + scale_x_continuous(breaks = seq(0,170,20), limits = c (0,170)) +  #xlim(0, 170) + 
    geom_vline(xintercept=90, colour = "grey40") + geom_vline(xintercept=100, colour = "grey40") +
    xlab("Shell height") + ylab("Survey mean no./tow") + ggtitle(paste(subarea.ids[i],"Shell Height Frequency")) +
    theme(#plot.title = element_text(size=10,hjust=0.5), 
      legend.title = element_blank(),
      legend.margin = margin(0,0,0,0),
      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)),
      legend.text = element_text(size=10), 
      legend.key.size = unit(0.53, "cm"), 
      legend.position = c(0.07, 0.99)) 
  windows(11,11); plot(p)
  ggsave(paste0("SHF_10yr",strata.ids[i],".png"), path=paste0(path.directory,"/",assessmentyear,"/Assessment/Figures/Growth/"), type="cairo", width=18, height=24, units = "cm", dpi=400)   
} # end for(i in 1:length(strata.ids))

###
### --- CALCULATE lbar FOR Comm and Rec; TO BE USED FOR CALCULATING GROWTH ---- 
###

#pivot wide so in right order (since 2020 was out of order) and drop 2020 
SHF_sdm_means <- SHF_sdm_means_4_plot %>%
  pivot_wider(names_from = year, values_from = SHF, names_prefix = "X") %>% dplyr::select(!X2020)

# Now we can get the Commercial and Recruit mean shell heights..
# Get the correct bins for the Commerical and Recruit sizes.
recs <- which(SHF_sdm_means$SHF.bin %in% c(90,95))
comm <- which(SHF_sdm_means$SHF.bin > 95)
# the average size of each of the recruit bins and commerical size bins
rec.mean.bins <- seq(92.5,97.5,by=5)
comm.mean.bins <- seq(102.5,197.5,by=5)
comm.size <- NULL
rec.size <- NULL
year.names.2 <- c(2001:2019, 2021:surveyyear)

# Now do the calculation to get the average SHF for each year and SHF bins....
for(i in 1:length(year.names.2))
{
tmp <- SHF_sdm_means[,c(year.names[i],"SDM","STRATA")]
names(tmp) <- c("SHF","SDM","STRATA")

# Do the calculation to get the average size of the commerical and recruits 
rec.size[[as.character(years[i])]] <- aggregate(SHF ~  SDM+STRATA,tmp[recs,],FUN = function(x) sum((x*rec.mean.bins)/sum(x)))
rec.size[[as.character(years[i])]]$year <- year.names.2[i]
comm.size[[as.character(years[i])]] <- aggregate(SHF ~  SDM+STRATA,tmp[comm,],FUN = function(x) sum((x*comm.mean.bins)/sum(x)))
comm.size[[as.character(years[i])]]$year <- year.names.2[i]
} 

# Unwrap list to get Recruit and commercial sizes for all areas and years
rec.size <- do.call("rbind",rec.size)
comm.size <- do.call("rbind",comm.size)

# Order the levels so they plot nicely..
rec.size$SDM  <- factor(rec.size$SDM, levels = c("low","med","high"))
comm.size$SDM  <- factor(comm.size$SDM, levels = c("low","med","high"))

#add 2020 as NA 
rec.size.2020 <- rec.size %>% distinct(SDM, STRATA) %>% mutate(SHF = NA, year = 2020)
rec.size <- rbind(rec.size, rec.size.2020) 
rec.size <- rec.size %>% dplyr::arrange(STRATA,SDM,year) |> 
  mutate(SUBAREA = case_when(STRATA == "SFA29A" ~ "Subarea A",
                             STRATA == "SFA29B" ~ "Subarea B",
                             STRATA == "SFA29C" ~ "Subarea C",
                             STRATA == "SFA29D" ~ "Subarea D")) 

comm.size.2020 <- comm.size %>% distinct(SDM, STRATA) %>% mutate(SHF = NA, year = 2020)
comm.size <- rbind(comm.size, comm.size.2020) 
comm.size <- comm.size %>% dplyr::arrange(STRATA,SDM,year) |> 
  mutate(SUBAREA = case_when(STRATA == "SFA29A" ~ "Subarea A",
                             STRATA == "SFA29B" ~ "Subarea B",
                             STRATA == "SFA29C" ~ "Subarea C",
                             STRATA == "SFA29D" ~ "Subarea D"))

#Remove row headers - they get out mis-matched when adding in 2020.
row.names(rec.size) <- NULL
row.names(comm.size) <- NULL

#---- Plot Avg Commercial size (lbar) and avg recruit size by year ---- 
# Plotting the average SHF for each year (i.e. Lbar) by SFA and bottom.  

# I think we should overhaul the colours and symbols, but these are what we use...
# in 2022 assessment: 
#  --- we should overhaul this to blind color pallet friendly and better pch 
#  --- automate saving out of figure with standardize names 

colr <- c('firebrick2', 'darkgrey', 'darkblue') #c("black","red","green")
line.type <- 1:3
symbs <- 15:17

# commerical average size 
y <- c(min(comm.size$SHF,na.rm = TRUE)-10,max(comm.size$SHF,na.rm = TRUE)+10)

lbar.comm <- ggplot(comm.size, aes(year,SHF,colour=SDM)) + geom_point(aes(shape = SDM)) + facet_wrap(~SUBAREA,scales="fixed") + geom_line(aes(linetype = SDM), size = 0.5) +
  scale_color_manual(values=colr, breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+ 
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 6)) +
  ylab("Shell Height (mm)") + xlab("Year") + 
  coord_cartesian(ylim=y) +
  theme_bw()+
  theme(axis.title = element_text(size =15),
        axis.text = element_text(size = 12),
        legend.position = c(.25, .75),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=15),
        panel.spacing.x = unit(5, "mm"),
        panel.grid.minor.x = element_blank(),legend.background = element_rect(fill=alpha('white', 0.8)))
  #guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1.5))
lbar.comm

ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29.lbar.comm.",surveyyear,".png"), plot = lbar.comm, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29.lbar.comm.",surveyyear,".png"),width=11,height=11,units = "in",res=300)
#lbar.comm
#dev.off()


# recruit average size 
y <- c(min(rec.size$SHF,na.rm = TRUE)-5,max(rec.size$SHF,na.rm = TRUE)+5)

lbar.rec <- ggplot(rec.size, aes(year,SHF,colour=SDM)) + geom_point(aes(shape = SDM)) + facet_wrap(~SUBAREA,scales="fixed") + geom_line(aes(linetype = SDM), size = 0.5) +
  scale_color_manual(values=colr,breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +
  scale_linetype_manual(values = line.type, breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low")) +
  scale_shape_manual(values = symbs, breaks = c("high", "med", "low"),labels = c("high"="High", "med"="Medium", "low"="Low"))+
  scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 6)) +
  ylab("Shell Height (mm)") + xlab("Year") + 
  coord_cartesian(ylim=y) +
  theme_bw()+
  theme(axis.title = element_text(size =15),
        axis.text = element_text(size = 12),
        legend.position = c(.25, .75),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=15),
        panel.spacing.x = unit(5, "mm"),
        panel.grid.minor.x = element_blank(),legend.background = element_rect(fill=alpha('white', 0.8)))
lbar.rec

ggsave(filename = paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29.lbar.rec.",surveyyear,".png"), plot = lbar.rec, scale = 2.5, width = 6, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory,assessmentyear,"/Assessment/Figures/Growth/SFA29.lbar.rec.",surveyyear,".png"),width=11,height=11,units = "in",res=300)
#lbar.rec
#dev.off()



# ---- Predicted Lbar in t+1 ----
### Next we grow the scallop from the current size to the size for next year...
# use equation: Linfty*(1-exp(-exp(logK)))+exp(-exp(logK))*mean.shell.height
#From Stephen (which should be from VonB modelling of aging from 29W)
Linf <-  152.03260
logK <-  -1.56787
#T0 don't have so use T0 from above VonB run to fill in just to get a plot:
#... NOTE - Stephen's VonB model run gives different parameter estimates from using full dataset above
# For now, use Stephen's given VonB parameter estimates to conduct growth calculations so full time series is equivalent. Will recalculate and redo in the future at next full RAP.
#View(comm.size %>% filter(year == 2024))
#View(rec.size %>% filter(year == 2024))
##recruits have NA for low 29B and low 29D in 2024; this is bc there were no recruits (0) in low in 29B and low in 29D in 2024 

#Grow up lbar for each year (t) to find expected mean SH for year t+1
comm.size$SHF.pred <- Linf*(1-exp(-exp(logK)))+exp(-exp(logK))*comm.size$SHF 
rec.size$SHF.pred <- Linf*(1-exp(-exp(logK)))+exp(-exp(logK))*rec.size$SHF 

comm.size$size <- "commercial"
rec.size$size <- "recruit"

lbar <- rbind(comm.size, rec.size)

#remove SUBAREA Column to keep consistant with previous years files:
lbar <- lbar |> dplyr::select(!SUBAREA)

lbar %>% filter(year %in% c(2024,2025) & size == "recruit")


#note lbar is column  "SHF" - current year average height; column name "SHF.pred" is the predicted average SH in the following year 
write.csv(lbar, paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SFA29.SHobj.",surveyyear,".csv"))

# Save the results....
#save(rec.size,comm.size,file = "SHF_SFA29.RData")

#bof example.. 
#export the objects to use in predicting mean weight
#dump(c('sh.actual','sh.predict'),paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA3/SPA3.SHobj.",surveyyear,".R"))

#write.csv(cbind(sh.actual, sh.predict %>% dplyr::select(!years)), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA3.lbar.to",surveyyear,".csv"))


# ---- Subarea E ---- 
# single domain within subarea E - take simple mean for all years 
#some year were all exploratory tows - would need to get data from data.obj.all ; Note ALL Tow types used
E.area <- SFA29livefreq.dat[SFA29livefreq.dat$STRATA_ID==45,]

#### Shell height Frequency means by year #NOTE: Only need to run once  (i.e. for comm, but not when running rec or prerec as it will always give same answer since its for all bins.
#### MUST ADD IN NEW YEAR FOR PLOTTING 
E.SHFmeans <- sapply(split(E.area[c(grep("BIN_ID_0",names(E.area)):grep("BIN_ID_195",names(E.area)))], E.area$YEAR), function(x){apply(x,2,mean)})
E.SHFmeans <- as.data.frame(E.SHFmeans)
#add column for 2020 - no survey year 
# SHF plot for 2 to 8 mile:  SPA1A.2to8.SHFmeans
E.SHFmeans.for.plot <- data.frame(bin.label = row.names(E.SHFmeans), E.SHFmeans)
E.SHFmeans.for.plot$X2020 <- NA # add 2020 column.
head(E.SHFmeans.for.plot)
E.SHFmeans.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

E.SHFmeans.for.plot <- pivot_longer(E.SHFmeans.for.plot, 
                                         cols = starts_with("X"),
                                         names_to = "year",
                                         names_prefix = "X",
                                         values_to = "SH",
                                         values_drop_na = FALSE)
E.SHFmeans.for.plot$year <- as.numeric(E.SHFmeans.for.plot$year)
table(E.SHFmeans.for.plot$year )

#select how many years back you want 
E.SHFmeans.for.plot <- E.SHFmeans.for.plot %>% filter(year > surveyyear-7)

#E.SHFmeans.for.plot <- E.SHFmeans.for.plot %>% filter(year > surveyyear-11)
#table(E.SHFmeans.for.plot$year )

#shorten SH data for plot or else get warning when run ggplot 
E.SHFmeans.for.plot$SH <- round(E.SHFmeans.for.plot$SH,3)

#Add autosave out of E.shf MEANS FOR FOR AL YEARS NOT JUST MOST RECENT 
#write.csv(E.SHFmeans , "dataoutput/SubareaE.SHF.csv")


# Plot SHF #
ylimits <- c(0,(max(E.SHFmeans.for.plot$SH,na.rm = TRUE)+10))
xlimits <- c(0,200)
recruitlimits <- c(90,100)

# plot SHF for 2to8 mile strata
plot.E.SHF <- ggplot() + geom_col(data = E.SHFmeans.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + ggtitle("Subarea E Shell Height Frequency")+
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.E.SHF

# Save out plot
ggsave(paste0("SHF_SFA29E.png"), path=paste0(path.directory,"/",assessmentyear,"/Assessment/Figures/Growth/"), plot = plot.E.SHF, type="cairo", width=18, height=24, units = "cm", dpi=400)  
#ggsave(paste0("SHF_SFA29E_10year.png"), path=paste0(path.directory,"/",assessmentyear,"/Assessment/Figures/Growth/"), plot = plot.E.SHF, type="cairo", width=18, height=24, units = "cm", dpi=400)  


#png(paste0(path.directory,assessmentyear, "/Assessment/Figures/Growth/SHF_SFA29E.png"), type="cairo", width=18, height=24, units = "cm", res=400)
#print(plot.E.SHF)
#dev.off()


### END ### 

