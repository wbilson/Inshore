###
###  ----  SPA 1B Shell Height Frequencies----
###    
###    J.Sameoto June 2020

# NOTE: SPA 1B Strata IDs: strata.spa1b<-c(35,37,38,49,51:53)


# ---- Prep work ---- 
options(stringsAsFactors = FALSE)

# required packages
library(ROracle)
library(PEDstrata) #v.1.0.2
library(tidyverse)
library(ggplot2)
library(PBSmapping)

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


# ///.... DEFINE THESE ENTRIES ....////

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"


#////... END OF DEFINE SECTION ...////


# ROracle; note this can take ~ 10 sec or so, don't panic
chan <-dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# read in shell height and meat weight data from database
# set sql question for numbers by shell height bin
query1 <- ("SELECT * FROM scallsur.scliveres WHERE strata_id IN (35, 37, 38, 49, 51, 52, 53)")

#Select data from database; execute query with ROracle; numbers by shell height bin
livefreq <- dbGetQuery(chan, query1)

#add YEAR column to data
# add YEAR column to data
livefreq$YEAR <- as.numeric(substr(livefreq$CRUISE,3,6))

# creates range of years
years <-c(1997:2019, 2021:surveyyear) #Skipping 2020 (add as NAs later)

# for Midbay North, need to assign strata to East/West
SPA1B.MBN.E <- data.frame(PID = 58,POS = 1:4,X=c(-65.710, -65.586, -65.197, -65.264),Y=c(45.280, 45.076, 45.237, 45.459)) #New strata
MBNlivefreq <- subset(livefreq, STRATA_ID == 38) #subset data
MBNlivefreq$lat <- convert.dd.dddd(MBNlivefreq$START_LAT) #format lat and lon
MBNlivefreq$lon <- convert.dd.dddd(MBNlivefreq$START_LONG)
MBNlivefreq$ID <- 1:nrow(MBNlivefreq)

# assign strata id "58" to Midbay North East
events <-  subset(MBNlivefreq,STRATA_ID == 38,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
MBNlivefreq$STRATA_ID[MBNlivefreq$ID %in% findPolys(events,SPA1B.MBN.E)$EID] <- 58
unique(MBNlivefreq$STRATA_ID)


# ---- CALCULATE SHF FOR EACH YEAR BY STRATA ----

#1. Cape Spencer
CSlivefreq <- subset(livefreq, STRATA_ID == 37 & TOW_TYPE_ID == 1)
SPA1B.CS.SHFmeans <- sapply(split(CSlivefreq[c(11:50)], CSlivefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.CS.SHFmeans,2)
colnames(SPA1B.CS.SHFmeans) #data from 1995 on 

#2a. MBN - East
MBNElivefreq <- subset(MBNlivefreq, STRATA_ID == 58 & TOW_TYPE_ID == 1)
SPA1B.MBNE.SHFmeans <- sapply(split(MBNElivefreq[c(11:50)],MBNElivefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.MBNE.SHFmeans,2)
colnames(SPA1B.MBNE.SHFmeans) #data from 1995 on 

#2b. MBN - West
#NOTE: MBN WEST has missing years
MBNWlivefreq <- subset(MBNlivefreq, STRATA_ID == 38 & TOW_TYPE_ID == 1)
SPA1B.MBNW.SHFmeans <- sapply(split(MBNWlivefreq[c(11:50)],MBNWlivefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.MBNW.SHFmeans,2)
colnames(SPA1B.MBNW.SHFmeans) #data from 1998 on but missing data from 2001 and 2004  

#add rows to SPA1B.MBNW.SHFmeans matrix (to fill in missing years) so it can be combined with MBNE
#this adds a new column for 2001 & 2004 and makes is zeros;
SPA1B.MBNW.SHFmeans <- data.frame(SPA1B.MBNW.SHFmeans)
SPA1B.MBNW.SHFmeans <- SPA1B.MBNW.SHFmeans %>% add_column("X2001" = 0, .after="X2000")
SPA1B.MBNW.SHFmeans <- SPA1B.MBNW.SHFmeans %>% add_column("X2004" = 0, .after="X2003")
#SPA1B.MBNW.SHFmeans <- cbind(SPA1B.MBNW.SHFmeans[, 1:3], matrix(0, nrow = 40), SPA1B.MBNW.SHFmeans[,4:5], matrix(0, nrow = 40), SPA1B.MBNW.SHFmeans[,6:dim(SPA1B.MBNW.SHFmeans)[2]])
#colnames(SPA1B.MBNW.SHFmeans) <- c(colnames(SPA1B.MBNW.SHFmeans)[1:3], c("2001"), colnames(SPA1B.MBNW.SHFmeans)[5:6],c("2004"), colnames(SPA1B.MBNW.SHFmeans)[8:dim(SPA1B.MBNW.SHFmeans)[2]])


#combine matrices proportionally (SPA1B.MBNE.SHFmeans from 1998 on and SPA1B.MBNW.SHFmeans is just from 1998 on 
SPA1B.MBN.SHFmeans <- (SPA1B.MBNE.SHFmeans[,10:dim(SPA1B.MBNE.SHFmeans)[2]]*0.732 ) + (SPA1B.MBNW.SHFmeans*0.268) 


#3.Upper Bay 28C
livefreq28C <- subset(livefreq, STRATA_ID == 53 & TOW_TYPE_ID == 1)
SPA1B.28C.SHFmeans <- sapply(split(livefreq28C[c(11:50)], livefreq28C$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.28C.SHFmeans, 2)
colnames(SPA1B.28C.SHFmeans) #data from 1998 on but missing data 2004

#this adds a new column for 2004 and makes is zeros;
SPA1B.28C.SHFmeans <- data.frame(SPA1B.28C.SHFmeans)
SPA1B.28C.SHFmeans <- SPA1B.28C.SHFmeans %>% add_column("X2004" = 0, .after="X2003")
#SPA1B.28C.SHFmeans <- cbind(SPA1B.28C.SHFmeans[,1:6], matrix(0, nrow = 40), SPA1B.28C.SHFmeans[,7:dim(SPA1B.28C.SHFmeans)[2]])
#colnames(SPA1B.28C.SHFmeans) <- c(colnames(SPA1B.28C.SHFmeans)[1:6],c("2004"), colnames(SPA1B.28C.SHFmeans)[8:dim(SPA1B.28C.SHFmeans)[2]])


#4. Advocate Harbour
AHlivefreq <- subset(livefreq, STRATA_ID == 35 & TOW_TYPE_ID == 1)
SPA1B.AH.SHFmeans <- sapply(split(AHlivefreq[c(11:50)], AHlivefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.AH.SHFmeans, 2)
colnames(SPA1B.AH.SHFmeans) #data from 1998 on but missing data 2004

#this adds a new column for 2004 and makes is zeros;
SPA1B.AH.SHFmeans <- data.frame(SPA1B.AH.SHFmeans)
SPA1B.AH.SHFmeans <- SPA1B.AH.SHFmeans %>% add_column("X2004" = 0, .after="X2003")
#SPA1B.AH.SHFmeans <- cbind(SPA1B.AH.SHFmeans[,1:6], matrix(0, nrow = 40), SPA1B.AH.SHFmeans[,7:dim(SPA1B.AH.SHFmeans)[2]])
#colnames(SPA1B.AH.SHFmeans) <- c(colnames(SPA1B.AH.SHFmeans)[1:6],c("2004"), colnames(SPA1B.AH.SHFmeans)[8:dim(SPA1B.AH.SHFmeans)[2]])

#5. 28D Outer D
Outlivefreq <- subset(livefreq, STRATA_ID == 49 & TOW_TYPE_ID == 1)
SPA1B.Out.SHFmeans <- sapply(split(Outlivefreq[c(11:50)], Outlivefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.Out.SHFmeans, 2)
colnames(SPA1B.Out.SHFmeans) #data from 1998 on but missing data 2004

#this adds a new column for 2004 and makes is zeros;
SPA1B.Out.SHFmeans <- data.frame(SPA1B.Out.SHFmeans)
SPA1B.Out.SHFmeans <- SPA1B.Out.SHFmeans %>% add_column("X2004" = 0, .after="X2003")
#SPA1B.Out.SHFmeans <- cbind(SPA1B.Out.SHFmeans[,1:6], matrix(0, nrow = 40), SPA1B.Out.SHFmeans[,7:dim(SPA1B.Out.SHFmeans)[2]])
#colnames(SPA1B.Out.SHFmeans) <- c(colnames(SPA1B.Out.SHFmeans)[1:6],c("2004"), colnames(SPA1B.Out.SHFmeans)[8:dim(SPA1B.Out.SHFmeans)[2]])


#6. Spencers Island
SIlivefreq <- subset(livefreq, STRATA_ID == 52 & TOW_TYPE_ID == 1)
SPA1B.SI.SHFmeans <- sapply(split(SIlivefreq[c(11:50)], SIlivefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.SI.SHFmeans, 2)
colnames(SPA1B.SI.SHFmeans) #data from 2005 on

#7. Scots Bay
SBlivefreq <- subset(livefreq, STRATA_ID == 51 & TOW_TYPE_ID == 1)
SPA1B.SB.SHFmeans <- sapply(split(SBlivefreq[c(11:50)], SBlivefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.SB.SHFmeans, 2)
colnames(SPA1B.SB.SHFmeans) #data from 2005 on



#---- PLOT SHF FOR EACH YEAR BY STRATA ----
# CS, MBN, 28C, Out, SI, SB, AH

#..1. SHF: Cape Spencer Strata 
SPA1B.CS.for.plot <- data.frame(bin.label = row.names(SPA1B.CS.SHFmeans), SPA1B.CS.SHFmeans)
SPA1B.CS.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.CS.for.plot)
SPA1B.CS.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.CS.for.plot <- pivot_longer(SPA1B.CS.for.plot, 
                                         cols = starts_with("X"),
                                         names_to = "year",
                                         names_prefix = "X",
                                         values_to = "SH",
                                         values_drop_na = FALSE)
SPA1B.CS.for.plot$year <- as.numeric(SPA1B.CS.for.plot$year)

SPA1B.CS.for.plot <- SPA1B.CS.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.CS.for.plot$SH <- round(SPA1B.CS.for.plot$SH,3)

ylimits <- c(0,max(SPA1B.CS.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.CS.SHF <- ggplot() + geom_col(data = SPA1B.CS.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.CS.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_CS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.CS.SHF)
dev.off()


#..2. SHF: Mid Bay North Strata 
SPA1B.MBN.for.plot <- data.frame(bin.label = row.names(SPA1B.MBN.SHFmeans), SPA1B.MBN.SHFmeans)
SPA1B.MBN.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.MBN.for.plot)
SPA1B.MBN.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.MBN.for.plot <- pivot_longer(SPA1B.MBN.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA1B.MBN.for.plot$year <- as.numeric(SPA1B.MBN.for.plot$year)

SPA1B.MBN.for.plot <- SPA1B.MBN.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.MBN.for.plot$SH <- round(SPA1B.MBN.for.plot$SH,3)

ylimits <- c(0,max(SPA1B.MBN.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF
plot.SPA1B.MBN.SHF <- ggplot() + geom_col(data = SPA1B.MBN.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.MBN.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_MBN.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.MBN.SHF)
dev.off()


#..3. SHF: Upper Bay 28C Strata 
SPA1B.28C.for.plot <- data.frame(bin.label = row.names(SPA1B.28C.SHFmeans), SPA1B.28C.SHFmeans)
SPA1B.28C.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.28C.for.plot)
SPA1B.28C.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.28C.for.plot <- pivot_longer(SPA1B.28C.for.plot, 
                                   cols = starts_with("X"),
                                   names_to = "year",
                                   names_prefix = "X",
                                   values_to = "SH",
                                   values_drop_na = FALSE)
SPA1B.28C.for.plot$year <- as.numeric(SPA1B.28C.for.plot$year)

SPA1B.28C.for.plot <- SPA1B.28C.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.28C.for.plot$SH <- round(SPA1B.28C.for.plot$SH,3)

ylimits <- c(0,max(SPA1B.28C.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.28C.SHF <- ggplot() + geom_col(data = SPA1B.28C.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.28C.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_28C.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.28C.SHF)
dev.off()


#4. SHF: Advocate Harbour (AH) Strata 
SPA1B.AH.for.plot <- data.frame(bin.label = row.names(SPA1B.AH.SHFmeans), SPA1B.AH.SHFmeans)
SPA1B.AH.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.AH.for.plot)
SPA1B.AH.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.AH.for.plot <- pivot_longer(SPA1B.AH.for.plot, 
                                   cols = starts_with("X"),
                                   names_to = "year",
                                   names_prefix = "X",
                                   values_to = "SH",
                                   values_drop_na = FALSE)
SPA1B.AH.for.plot$year <- as.numeric(SPA1B.AH.for.plot$year)

SPA1B.AH.for.plot <- SPA1B.AH.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.AH.for.plot$SH <- round(SPA1B.AH.for.plot$SH,3)

ylimits <- c(0,max(SPA1B.AH.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.AH.SHF <- ggplot() + geom_col(data = SPA1B.AH.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.AH.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_AH.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.AH.SHF)
dev.off()

#..5. SHF: 28D Outer D Strata 
SPA1B.Outer.for.plot <- data.frame(bin.label = row.names(SPA1B.Out.SHFmeans), SPA1B.Out.SHFmeans)
SPA1B.Outer.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.Outer.for.plot)
SPA1B.Outer.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.Outer.for.plot <- pivot_longer(SPA1B.Outer.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA1B.Outer.for.plot$year <- as.numeric(SPA1B.Outer.for.plot$year)

SPA1B.Outer.for.plot <- SPA1B.Outer.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.Outer.for.plot$SH <- round(SPA1B.Outer.for.plot$SH,3)

ylimits <- c(0,max(SPA1B.Outer.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.Out.SHF <- ggplot() + geom_col(data = SPA1B.Outer.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.Out.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_Out.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.Out.SHF)
dev.off()

#6. SHF: Spencers Island Strata 
SPA1B.SI.for.plot <- data.frame(bin.label = row.names(SPA1B.SI.SHFmeans), SPA1B.SI.SHFmeans)
SPA1B.SI.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.SI.for.plot)
SPA1B.SI.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.SI.for.plot <- pivot_longer(SPA1B.SI.for.plot, 
                                     cols = starts_with("X"),
                                     names_to = "year",
                                     names_prefix = "X",
                                     values_to = "SH",
                                     values_drop_na = FALSE)
SPA1B.SI.for.plot$year <- as.numeric(SPA1B.SI.for.plot$year)

SPA1B.SI.for.plot <- SPA1B.SI.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.SI.for.plot$SH <- round(SPA1B.SI.for.plot$SH,3)

ylimits <- c(0,max(SPA1B.SI.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1B.SI.SHF <- ggplot() + geom_col(data = SPA1B.SI.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.SI.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_SI.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.SI.SHF)
dev.off()


#7. SHF: Scots Bay Strata 
SPA1B.SB.for.plot <- data.frame(bin.label = row.names(SPA1B.SB.SHFmeans), SPA1B.SB.SHFmeans)
SPA1B.SB.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.SB.for.plot)
SPA1B.SB.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.SB.for.plot <- pivot_longer(SPA1B.SB.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA1B.SB.for.plot$year <- as.numeric(SPA1B.SB.for.plot$year)

SPA1B.SB.for.plot <- SPA1B.SB.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.SB.for.plot$SH <- round(SPA1B.SB.for.plot$SH,3)


ylimits <- c(0,max(SPA1B.SB.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF for 2to8 mile strata
plot.SPA1B.SB.SHF <- ggplot() + geom_col(data = SPA1B.SB.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.SB.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_SB.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.SB.SHF)
dev.off()



#---- CALCULATE STRATIFIED SHF FOR ALL OF SPA 1B FOR EACH YEAR ----

#convert SHF objects to dataframes: 
SPA1B.CS.SHFmeans.df <- data.frame(SPA1B.CS.SHFmeans)
SPA1B.MBNE.SHFmeans.df <- data.frame(SPA1B.MBNE.SHFmeans)
SPA1B.MBN.SHFmeans.df <- data.frame(SPA1B.MBN.SHFmeans)
SPA1B.28C.SHFmeans.df <- data.frame(SPA1B.28C.SHFmeans)
SPA1B.AH.SHFmeans.df <- data.frame(SPA1B.AH.SHFmeans)
SPA1B.Out.SHFmeans.df <- data.frame(SPA1B.Out.SHFmeans)
SPA1B.SI.SHFmeans.df <- data.frame(SPA1B.SI.SHFmeans)
SPA1B.SB.SHFmeans.df <- data.frame(SPA1B.SB.SHFmeans)

bin <- as.numeric(substr(names(livefreq[c(grep("BIN_ID_0", colnames(livefreq)):grep("BIN_ID_195", colnames(livefreq)))]),8,nchar(names(livefreq[c(11:50)]))))
SPA1B.SHF <- data.frame(Bin=bin, X1997 = rep(NA,40))

#1997-2000: just Cape Spencer and  MidBay North (total NH=485169.17, CS prop=0.3937, MBN prop=0.6063);use MBNE for 1997
SPA1B.SHF$X1997 <- (SPA1B.CS.SHFmeans.df$X1997*0.3937) + (SPA1B.MBNE.SHFmeans.df$X1997*0.6063) #yes using MBNE here is correct
SPA1B.SHF$X1998 <- (SPA1B.CS.SHFmeans.df$X1998*0.3937) + (SPA1B.MBN.SHFmeans.df$X1998*0.6063)
SPA1B.SHF$X1999 <- (SPA1B.CS.SHFmeans.df$X1999*0.3937) + (SPA1B.MBN.SHFmeans.df$X1999*0.6063)
SPA1B.SHF$X2000 <- (SPA1B.CS.SHFmeans.df$X2000*0.3937) + (SPA1B.MBN.SHFmeans.df$X2000*0.6063)

#2001-2003: cS, MBN, UB, AH, OuterBay (TOTAL NH=690986.55, cs prop=0.2765, MBN prop= 0.4257,UB prop=0.1274, AH prop=0.0125, oUT=0.1580)
SPA1B.SHF$X2001 <- (SPA1B.CS.SHFmeans.df$X2001*0.2765) + (SPA1B.MBN.SHFmeans.df$X2001*0.4257) + (SPA1B.28C.SHFmeans.df$X2001*0.1274) + (SPA1B.AH.SHFmeans.df$X2001*0.0125) + (SPA1B.Out.SHFmeans.df$X2001*0.1580)
SPA1B.SHF$X2002 <- (SPA1B.CS.SHFmeans.df$X2002*0.2765) + (SPA1B.MBN.SHFmeans.df$X2002*0.4257) + (SPA1B.28C.SHFmeans.df$X2002*0.1274) + (SPA1B.AH.SHFmeans.df$X2002*0.0125) + (SPA1B.Out.SHFmeans.df$X2002*0.1580)
SPA1B.SHF$X2003 <- (SPA1B.CS.SHFmeans.df$X2003*0.2765) + (SPA1B.MBN.SHFmeans.df$X2003*0.4257) + (SPA1B.28C.SHFmeans.df$X2003*0.1274) + (SPA1B.AH.SHFmeans.df$X2003*0.0125) + (SPA1B.Out.SHFmeans.df$X2003*0.1580)

#2004: Just Cape Spencer and  MidBay North (total NH=485169.17, CS prop=0.3937, MBN prop=0.6063)
SPA1B.SHF$X2004 <- (SPA1B.CS.SHFmeans.df$X2004*0.3937) + (SPA1B.MBN.SHFmeans.df$X2004*0.6063)

#2005-Present: (total NH=730740.49, CS prop=0.2614, SI prop=0.0278, SB prop=0.0266,MBN prop= 0.4025, UB prop=0.1204, AH prop=0.0118, oUT=0.1494)
SPA1B.SHF.2005topresent <- (SPA1B.CS.SHFmeans.df[,c(grep("X2005", colnames(SPA1B.CS.SHFmeans.df)):dim(SPA1B.CS.SHFmeans.df)[2])]*0.2614) + 
  (SPA1B.MBN.SHFmeans.df[,c(grep("X2005", colnames(SPA1B.MBN.SHFmeans.df)):dim(SPA1B.MBN.SHFmeans.df)[2])]*0.4025) + 
  (SPA1B.28C.SHFmeans.df[,c(grep("X2005", colnames(SPA1B.28C.SHFmeans.df)):dim(SPA1B.28C.SHFmeans.df)[2])]*0.1204) + 
  (SPA1B.AH.SHFmeans.df[,c(grep("X2005", colnames(SPA1B.AH.SHFmeans.df)):dim(SPA1B.AH.SHFmeans.df)[2])]*0.0118) + 
  (SPA1B.Out.SHFmeans.df[,c(grep("X2005", colnames(SPA1B.Out.SHFmeans.df)):dim(SPA1B.Out.SHFmeans.df)[2])]*0.1494) + 
  (SPA1B.SI.SHFmeans.df[,c(grep("X2005", colnames(SPA1B.SI.SHFmeans.df)):dim(SPA1B.SI.SHFmeans.df)[2])]*0.0278) + 
  (SPA1B.SB.SHFmeans.df[,c(grep("X2005", colnames(SPA1B.SB.SHFmeans.df)):dim(SPA1B.SB.SHFmeans.df)[2])]*0.0266)

#All years combined: (FIX ABOVE SO ALL  X. column names )
SPA1B.SHF <- cbind(SPA1B.SHF , SPA1B.SHF.2005topresent)

# SHF plot for 1A ALL  SPA1A.ALL.SHFmeans
SPA1B.shf.data.for.plot <- data.frame(bin.label = row.names(SPA1B.SHF), SPA1B.SHF)
SPA1B.shf.data.for.plot$X2020 <- NA # add 2020 column.
head(SPA1B.shf.data.for.plot)
SPA1B.shf.data.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1B.shf.data.for.plot <- pivot_longer(SPA1B.shf.data.for.plot, 
                                        cols = starts_with("X"),
                                        names_to = "year",
                                        names_prefix = "X",
                                        values_to = "SH",
                                        values_drop_na = FALSE)
SPA1B.shf.data.for.plot$year <- as.numeric(SPA1B.shf.data.for.plot$year)

SPA1B.shf.data.for.plot <- SPA1B.shf.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1B.shf.data.for.plot$SH <- round(SPA1B.shf.data.for.plot$SH,3)

ylimits <- c(0,40)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF for All of SPA 1A
plot.SPA1B.SHF <- ggplot() + geom_col(data = SPA1B.shf.data.for.plot, aes(x = bin.mid.pt, y = SH)) + facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1B.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_all.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1B.SHF)
dev.off()


#---- CALCULATE lbar FOR ALL OF SPA 1A TO BE USED FOR CALCULATING GROWTH ----
# Calculate mean shell height in year t and t+1 across all SPA 1B for each year - based on stratified SHF above

# ACTUAL shell height by year

#Commercial size >= 80 mm Shell height 
SPA1B.SHactual.Com <- rep(length(years))
for (i in 1:length(years)) {
  temp.data <- SPA1B.SHF[c(17:40),1 + i]  #commercial size; rows that are 80 mm SH and above 
  SPA1B.SHactual.Com[i] <- sum(temp.data*seq(82.5,197.5,by = 5))/sum(temp.data) }

#add year and make as df 
SPA1B.SHactual.Com <- data.frame((t(rbind(years, SPA1B.SHactual.Com))))
SPA1B.SHactual.Com <-  rbind(SPA1B.SHactual.Com, c(2020, NA)) %>%  #ADD IN 2020 as NA
  arrange(years)#Sort by Year
SPA1B.SHactual.Com

#Recruit size >= 80 mm Shell height 
SPA1B.SHactual.Rec <- rep(length(years))
for (i in 1:length(years)) {
  temp.data <- SPA1B.SHF[c(14:16),1 + i]  #recruit size; rows that are 65, 70, 75 -- i.e. from 65 mm SH to 79.9 mm SH 
  SPA1B.SHactual.Rec[i] <- sum(temp.data*seq(67.5,77.55,by = 5))/sum(temp.data) }

#add year and make as df 
SPA1B.SHactual.Rec <- data.frame((t(rbind(years, SPA1B.SHactual.Rec))))
SPA1B.SHactual.Rec <-  rbind(SPA1B.SHactual.Rec, c(2020, NA)) %>%  #ADD IN 2020 as NA
  arrange(years)#Sort by Year
SPA1B.SHactual.Rec

sh.actual <- data.frame(SPA1B.SHactual.Com, SPA1B.SHactual.Rec %>% dplyr::select(SPA1B.SHactual.Rec))

# PREDICTED shell height per year; where the value in 1997 (first value) is the predicted height for 1998
# inputs from Von B files; All BoF was modelled over all years (Y:\INSHORE SCALLOP\BoF\2015\Growth\BoF_VonB.R)
# Note, no new aging done in 2017-2019, use 2016 VonB relationship 
# VONBF2016.nlme
# Linf      logK        T0
# 148.19668  -1.57621  -0.36559

#the value in 1997 (first value) is the predicted height for 1998
years.predict <- 1998:(surveyyear+1)

# Commercial Size 
SPA1B.SHpredict.Com <- rep(length(years.predict))
for (i in 1:length(years.predict)) {
  temp.data <- SPA1B.SHactual.Com$SPA1B.SHactual.Com[i] #commercial size
  SPA1B.SHpredict.Com[i] <- 144.75*(1 - exp(-exp(-1.576))) + exp(-exp(-1.576))*temp.data }

#add year and make as df 
SPA1B.SHpredict.Com <- data.frame((t(rbind(years.predict, SPA1B.SHpredict.Com))))


#the value in 1997 (first value) is the predicted height for 1998
SPA1B.SHpredict.Rec <- rep(length(years.predict))
for (i in 1:length(years.predict)) {
  temp.data <- SPA1B.SHactual.Rec$SPA1B.SHactual.Rec[i]  #recruit size
  SPA1B.SHpredict.Rec[i] <- 144.75*(1-exp(-exp(-1.576))) + exp(-exp(-1.576))*temp.data }

#add year and make as df 
SPA1B.SHpredict.Rec <- data.frame((t(rbind(years.predict, SPA1B.SHpredict.Rec))))

sh.predict <- data.frame(SPA1B.SHactual.Com %>% dplyr::select(years), SPA1B.SHpredict.Com, SPA1B.SHpredict.Rec %>% dplyr::select(SPA1B.SHpredict.Rec)) #years = current year, years.predict = prediction year. (i.e. at year = 1999 acutal sh = X, year.predict = 2000, predicted sh = Y)

# Export the objects to use in predicting mean weight/growth
dump (c('sh.actual','sh.predict'), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA1B",surveyyear,".SHobj.R"))

#Note that years 2001-2003 are those that with NEW SHF script for 1B are SLIGHTLY different than historical values (slightly is ~ 1 mm ) 
write.csv(cbind(sh.actual, sh.predict %>% dplyr::select(!years)), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA1B_SHactualpredict.",surveyyear,".csv")) 


#---- CALCULATE lbar BY SUBAREA TO PLOT ----
# areas: CS, MBN, 28c, 28D

# Cape Spence (CS)
X <- length(years)
CS.lbar.strata <- data.frame(Year = years, lbar.CS = rep(NA,X))
for (i in 1:length(years)) {
  temp.data <- SPA1B.CS.SHFmeans[c(17:40),7 + i] #17:40 is 80 mm SH and above for Commercial size 
CS.lbar.strata[i,2] <- sum(temp.data*seq(82.5,197.5,by = 5))/sum(temp.data)
 }
CS.lbar.strata
CS.lbar.strata <-  rbind(CS.lbar.strata, c(2020, NaN)) %>%  #ADD IN 2020 as NA
  arrange(Year)#Sort by Year
CS.lbar.strata

# Mid Bay North (MBN)
years.MBN <- c(1998:2019, 2021:surveyyear) #since MBN starts in 1998, Skipping 2020 (add as NAs later) 
X <- length(years.MBN) 
MBN.lbar.strata <- data.frame(Year = years.MBN, lbar.MBN = rep(NA,X))
for (i in 1:length(years.MBN)) {
  temp.data <- SPA1B.MBN.SHFmeans[c(17:40),0 + i] #17:40 is 80 mm SH and above for Commercial size 
MBN.lbar.strata[i,2] <- sum(temp.data*seq(82.5,197.5,by = 5))/sum(temp.data)
 }
MBN.lbar.strata
MBN.lbar.strata <-  rbind(MBN.lbar.strata, c(2020, NaN)) %>%  #ADD IN 2020 as NA
  arrange(Year)#Sort by Year
MBN.lbar.strata


# 28C 
years.28C <- c(1998:2019, 2021:surveyyear) #since 28C starts in 1998. Skipping 2020 (add as NAs later) 
X <- length(years.28C)
C28.lbar.strata <- data.frame(Year = years.28C, lbar.28C = rep(NA,X))
for (i in 1:length(years.28C)) {
  temp.data <- SPA1B.28C.SHFmeans[c(17:40),0 + i]
C28.lbar.strata[i,2] <- sum(temp.data*seq(82.5,197.5,by = 5))/sum(temp.data)
 }
C28.lbar.strata
C28.lbar.strata <-  rbind(C28.lbar.strata, c(2020, NaN)) %>%  #ADD IN 2020 as NA
  arrange(Year)#Sort by Year
C28.lbar.strata

#8. 28D (all)
strata.28D <- c(35,49,51,52)
D28livefreq <- subset(livefreq, STRATA_ID %in% strata.28D & TOW_TYPE_ID == 1)
SPA1B.28D.SHFmeans <- sapply(split(D28livefreq[c(grep("BIN_ID_0", colnames(livefreq)):grep("BIN_ID_195", colnames(livefreq)))], D28livefreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1B.28D.SHFmeans, 2)

#add dummy column for 2004
SPA1B.28D.SHFmeans <- data.frame(SPA1B.28D.SHFmeans)
SPA1B.28D.SHFmeans <- SPA1B.28D.SHFmeans %>% add_column("X2004" = 0, .after="X2003")
SPA1B.28D.SHFmeans
#SPA1B.28D.SHFmeans <- cbind(SPA1B.28D.SHFmeans[, c(grep("1998", colnames(SPA1B.28D.SHFmeans)):grep("2003", colnames(SPA1B.28D.SHFmeans)))], matrix(0, nrow = 40), SPA1B.28D.SHFmeans[,c(grep("2005", colnames(SPA1B.28D.SHFmeans)):grep(surveyyear, colnames(SPA1B.28D.SHFmeans)))]) 
#colnames(SPA1B.28D.SHFmeans) <- c(colnames(SPA1B.28D.SHFmeans)[c(grep("1998", colnames(SPA1B.28D.SHFmeans)):grep("2003", colnames(SPA1B.28D.SHFmeans)))],c("2004"), colnames(SPA1B.28D.SHFmeans)[c(grep("2005", colnames(SPA1B.28D.SHFmeans)):grep(surveyyear, colnames(SPA1B.28D.SHFmeans)))])


years.28D <- c(1998:2019, 2021:surveyyear) #Skipping 2020 (add as NAs later) 
X <- length(years.28D)
D28.lbar.strata <- data.frame(Year = years.28D, lbar.28D = rep(NA,X))
for (i in 1:length(years.28D)) {
  temp.data <- SPA1B.28D.SHFmeans[c(17:40),0  + i]
D28.lbar.strata[i,2] <- sum(temp.data*seq(82.5,197.5,by = 5))/sum(temp.data)
 }
D28.lbar.strata
D28.lbar.strata <-  rbind(D28.lbar.strata, c(2020, NaN)) %>%  #ADD IN 2020 as NA
  arrange(Year)#Sort by Year
D28.lbar.strata


#Add 1997 to "MBN.lbar.strata","C28.lbar.strata","D28.lbar.strata" object 
MBN.lbar.strata <- rbind(data.frame(Year=1997, lbar.MBN=NaN) , MBN.lbar.strata)
C28.lbar.strata <- rbind(data.frame(Year=1997, lbar.28C=NaN) , C28.lbar.strata)
D28.lbar.strata <- rbind(data.frame(Year=1997, lbar.28D=NaN) , D28.lbar.strata)

SPA1B.lbar.strata.groups <- cbind(CS.lbar.strata, dplyr::select(MBN.lbar.strata, "lbar.MBN") ,dplyr::select(C28.lbar.strata, "lbar.28C"), dplyr::select(D28.lbar.strata, "lbar.28D"))

# Save out data - mean Commercial SHell Height by Strata Group 
dump(c("SPA1B.lbar.strata.groups"), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA1B",surveyyear,".lbarbyStrataGroup.R"))



# Prep for plot: 
# Reshape into long format
SPA1B.lbar.strata <- pivot_longer(SPA1B.lbar.strata.groups, 
                                  cols = starts_with("lbar"),
                                  names_to = "Strata",
                                  names_prefix = "lbar.",
                                  values_to = "lbar",
                                  values_drop_na = FALSE)

write.csv(SPA1B.lbar.strata, paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA1B.lbar.by.Strata.",surveyyear,".csv"))

# Plot of Mean Commercial SHell Height by Strata Group 
plot.SPA1B.lbar.strata <- ggplot(SPA1B.lbar.strata) + geom_line(aes(x = Year, y = lbar, color = Strata)) + 
  geom_point(aes(x = Year, y = lbar, color = Strata)) +
  theme_bw() + ylab("Mean Commercial Shell Height (mm)") + 
  scale_color_discrete(name="Strata", labels = c("28C", "28D","Cape Spencer","Mid Bay North")) + 
  theme(legend.position = c(.05, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)) 
plot.SPA1B.lbar.strata

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_lbar.png"), type="cairo", width=15, height=15, units = "cm", res=400)
print(plot.SPA1B.lbar.strata)
dev.off()



#---- REPEAT TOW SHF PLOTS ----    

# read in cross reference files for spr
query2 <- "SELECT * FROM scallsur.sccomparisontows WHERE sccomparisontows.cruise LIKE 'BF%' AND COMP_TYPE= 'R'"
crossref.BoF <- dbGetQuery(chan, query2)

#create ID column  
crossref.BoF$CruiseID <- paste(crossref.BoF$CRUISE_REF,crossref.BoF$TOW_NO_REF,sep = '.')  #create CRUISE_ID on "parent/reference" tow
livefreq$CruiseID <- paste(livefreq$CRUISE,livefreq$TOW_NO,sep = '.')  #create CRUISE_ID on "parent/reference" tow

#merge STRATA_ID from livefreq to the crosssref files based on parent/reference tow
crossref.BoF <- merge(crossref.BoF, subset(livefreq, select = c("STRATA_ID", "CruiseID")), by.x = "CruiseID", all = FALSE)

#Subset to years you want to look at for repeated tows 
crossref.BoF.prioryear <- subset(crossref.BoF, CRUISE == paste0("BF",surveyyear-1)) #CHANGE BACK TO -1 AFTER 2021
crossref.BoF.currentyear <- subset(crossref.BoF, CRUISE == paste0("BF",surveyyear))

#subset for years; 1 regular and 5 repeats 
tows <- c(1,5)
livefreq.prioryear <- subset(livefreq, YEAR == surveyyear-1 & TOW_TYPE_ID %in% tows) #CHANGE BACK TO -1 AFTER 2021
livefreq.currentyear <- subset(livefreq, YEAR == surveyyear & TOW_TYPE_ID %in% tows)
data.before <- livefreq.prioryear
data.after <- livefreq.currentyear 

# 1. MBN Repeated Tows 
temp <- subset(crossref.BoF.currentyear, STRATA_ID %in% c(38,58)) 
before <- temp[,c(grep("TOW_NO_REF", colnames(temp)))] #previous year tow numbers 
after <- temp[,c(grep("TOW_NO$", colnames(temp)))] #current year tow numbers NOTE: for grep()^ Asserts that we are at the start. $ Asserts that we are at the end; avoids calling tow_no_ref column as well 

MBN.before <- subset(data.before, TOW_NO %in% c(before)) #tow numbers for first year associated with repeats
MBN.after <- subset(data.after, TOW_NO %in% c(after)) # tow numbers for 2nd year associated with repeats
before.tows.n <- length(before)
after.tows.n <- length(after)

MBN.beforemeans <- sapply(split(MBN.before[c(grep("BIN_ID_0", colnames(MBN.before)):grep("BIN_ID_195", colnames(MBN.before)))],MBN.before$YEAR), function(x){apply(x,2,mean)})

MBN.aftermeans <- sapply(split(MBN.after[c(grep("BIN_ID_0", colnames(MBN.after)):grep("BIN_ID_195", colnames(MBN.after)))
], MBN.after$YEAR), function(x){apply(x,2,mean)})

MBN.repeats.shf <- data.frame(bin.label = row.names(MBN.beforemeans), MBN.beforemeans, MBN.aftermeans)
MBN.repeats.shf$bin.mid.pt <- seq(2.5,200,by=5)
head(MBN.repeats.shf)

MBN.repeats.shf.for.plot <- pivot_longer(MBN.repeats.shf, 
                                        cols = starts_with("X"),
                                        names_to = "year",
                                        names_prefix = "X",
                                        values_to = "SH",
                                        values_drop_na = FALSE)
MBN.repeats.shf.for.plot$year <- as.numeric(MBN.repeats.shf.for.plot$year)

# MBN Plot Repeat Tow SHFs, shorten SH data for plot or else get warning when run ggplot 
MBN.repeats.shf.for.plot$SH <- round(MBN.repeats.shf.for.plot$SH,3)

ylimits <- c(0,max(MBN.repeats.shf.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)
anno <- data.frame(year = c(surveyyear-1, surveyyear), n.tows = c(before.tows.n, after.tows.n), lab = c(paste0("N=",before.tows.n), paste0("N=",after.tows.n)))

# plot SHF
plot.MBN.repeats.shf.for.plot <- ggplot() + geom_col(data = MBN.repeats.shf.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20)) + 
  geom_text(data = anno, aes(x = 180,  y = max(MBN.repeats.shf.for.plot$SH), label = lab))

plot.MBN.repeats.shf.for.plot

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_MBN_RepeatTows.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.MBN.repeats.shf.for.plot)
dev.off()


# 2. AH Repeated Tows 
temp <- subset(crossref.BoF.currentyear, STRATA_ID == 35) #update with year
before <- temp[,c(grep("TOW_NO_REF", colnames(temp)))] #previous year tow numbers 
after <- temp[,c(grep("TOW_NO$", colnames(temp)))] #current year tow numbers NOTE: for grep()^ Asserts that we are at the start. $ Asserts that we are at the end; avoids calling tow_no_ref column as well 
before.tows.n <- length(before)
after.tows.n <- length(after)

AH.before <- subset(data.before, TOW_NO %in% c(before))
AH.after <- subset(data.after, TOW_NO %in% c(after))

AH.beforemeans <- sapply(split(AH.before[c(grep("BIN_ID_0", colnames(AH.before)):grep("BIN_ID_195", colnames(AH.before)))],AH.before$YEAR), function(x){apply(x,2,mean)})

AH.aftermeans <- sapply(split(AH.after[c(grep("BIN_ID_0", colnames(AH.after)):grep("BIN_ID_195", colnames(AH.after)))], AH.after$YEAR), function(x){apply(x,2,mean)})

AH.repeats.shf <- data.frame(bin.label = row.names(AH.beforemeans), AH.beforemeans, AH.aftermeans)
AH.repeats.shf$bin.mid.pt <- seq(2.5,200,by=5)
head(AH.repeats.shf)

AH.repeats.shf.for.plot <- pivot_longer(AH.repeats.shf, 
                                         cols = starts_with("X"),
                                         names_to = "year",
                                         names_prefix = "X",
                                         values_to = "SH",
                                         values_drop_na = FALSE)
AH.repeats.shf.for.plot$year <- as.numeric(AH.repeats.shf.for.plot$year)

# AH Plot Repeat Tow SHFs, shorten SH data for plot or else get warning when run ggplot 
AH.repeats.shf.for.plot$SH <- round(AH.repeats.shf.for.plot$SH,3)

ylimits <- c(0,max(AH.repeats.shf.for.plot$SH)+20)
xlimits <- c(0,200)
recruitlimits <- c(65,80)
anno <- data.frame(year = c(surveyyear-1, surveyyear), n.tows = c(before.tows.n, after.tows.n), lab = c(paste0("N=",before.tows.n), paste0("N=",after.tows.n)))

# plot SHF
plot.AH.repeats.shf.for.plot <- ggplot() + geom_col(data = AH.repeats.shf.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20)) + 
  geom_text(data = anno, aes(x = 180,  y = max(AH.repeats.shf.for.plot$SH), label = lab))

plot.AH.repeats.shf.for.plot

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_AH_RepeatTows.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.AH.repeats.shf.for.plot)
dev.off()


# 3. 28C Repeated Tows 
temp <- subset(crossref.BoF.currentyear, STRATA_ID == 53)#update with year
before <- temp[,c(grep("TOW_NO_REF", colnames(temp)))] #previous year tow numbers 
after <- temp[,c(grep("TOW_NO$", colnames(temp)))] #current year tow numbers NOTE: for grep()^ Asserts that we are at the start. $ Asserts that we are at the end; avoids calling tow_no_ref column as well 
before.tows.n <- length(before)
after.tows.n <- length(after)

C28.before <- subset(data.before, TOW_NO %in% c(before))
C28.after <- subset(data.after, TOW_NO %in% c(after))

C28.beforemeans <- sapply(split(C28.before[c(grep("BIN_ID_0", colnames(C28.before)):grep("BIN_ID_195", colnames(C28.before)))],C28.before$YEAR), function(x){apply(x,2,mean)})

C28.aftermeans <- sapply(split(C28.after[c(grep("BIN_ID_0", colnames(C28.after)):grep("BIN_ID_195", colnames(C28.after)))], C28.after$YEAR), function(x){apply(x,2,mean)})

C28.repeats.shf <- data.frame(bin.label = row.names(C28.beforemeans), C28.beforemeans, C28.aftermeans)
C28.repeats.shf$bin.mid.pt <- seq(2.5,200,by=5)
head(C28.repeats.shf)

C28.repeats.shf.for.plot <- pivot_longer(C28.repeats.shf, 
                                         cols = starts_with("X"),
                                         names_to = "year",
                                         names_prefix = "X",
                                         values_to = "SH",
                                         values_drop_na = FALSE)
C28.repeats.shf.for.plot$year <- as.numeric(C28.repeats.shf.for.plot$year)

# 28C Plot Repeat Tow SHFs, shorten SH data for plot or else get warning when run ggplot 
C28.repeats.shf.for.plot$SH <- round(C28.repeats.shf.for.plot$SH,3)

ylimits <- c(0,max(C28.repeats.shf.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)
anno <- data.frame(year = c(surveyyear-1, surveyyear), n.tows = c(before.tows.n, after.tows.n), lab = c(paste0("N=",before.tows.n), paste0("N=",after.tows.n)))

# plot SHF
plot.C28.repeats.shf.for.plot <- ggplot() + geom_col(data = C28.repeats.shf.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20)) + 
  geom_text(data = anno, aes(x = 180,  y = max(C28.repeats.shf.for.plot$SH), label = lab))

plot.C28.repeats.shf.for.plot

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_28C_RepeatTows.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.C28.repeats.shf.for.plot)
dev.off()



# 4. Out
temp <- subset(crossref.BoF.currentyear, STRATA_ID == 49)#update with year

before <- temp[,c(grep("TOW_NO_REF", colnames(temp)))] #previous year tow numbers 
after <- temp[,c(grep("TOW_NO$", colnames(temp)))] #current year tow numbers NOTE: for grep()^ Asserts that we are at the start. $ Asserts that we are at the end; avoids calling tow_no_ref column as well 
before.tows.n <- length(before)
after.tows.n <- length(after)

Out.before <- subset(data.before, TOW_NO %in% c(before))
Out.after <- subset(data.after, TOW_NO %in% c(after))

Out.beforemeans <- sapply(split(Out.before[c(grep("BIN_ID_0", colnames(Out.before)):grep("BIN_ID_195", colnames(Out.before)))],Out.before$YEAR), function(x){apply(x,2,mean)})

Out.aftermeans <- sapply(split(Out.after[c(grep("BIN_ID_0", colnames(Out.after)):grep("BIN_ID_195", colnames(Out.after)))
], Out.after$YEAR), function(x){apply(x,2,mean)})


Out.repeats.shf <- data.frame(bin.label = row.names(Out.beforemeans), Out.beforemeans, Out.aftermeans)
Out.repeats.shf$bin.mid.pt <- seq(2.5,200,by=5)
head(Out.repeats.shf)

Out.repeats.shf.for.plot <- pivot_longer(Out.repeats.shf, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
Out.repeats.shf.for.plot$year <- as.numeric(Out.repeats.shf.for.plot$year)


# OUT Plot Repeat Tow SHFs, shorten SH data for plot or else get warning when run ggplot 
Out.repeats.shf.for.plot$SH <- round(Out.repeats.shf.for.plot$SH,3)

ylimits <- c(0,max(Out.repeats.shf.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)
anno <- data.frame(year = c(surveyyear-1, surveyyear), n.tows = c(before.tows.n, after.tows.n), lab = c(paste0("N=",before.tows.n), paste0("N=",after.tows.n)))

# plot SHF
plot.Out.repeats.shf.for.plot <- ggplot() + geom_col(data = Out.repeats.shf.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20)) + 
  geom_text(data = anno, aes(x = 180,  y = max(Out.repeats.shf.for.plot$SH), label = lab))

plot.Out.repeats.shf.for.plot

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1B_SHF_Out_RepeatTows.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.Out.repeats.shf.for.plot)
dev.off()

print("END OF SHELL HEIGHT FREQUENCY SCRIPT")


