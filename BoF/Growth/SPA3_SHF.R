###
###  ----  SPA3 Shell Height Frequencies----
###    
###    J.Sameoto June 2020

#NOTES: SPA3 strata from Inshore data base are Strata IDs 22:24

# ---- Prep work ---- 
options(stringsAsFactors = FALSE)

# required packages
library(ROracle)
library(PEDstrata) #v.1.0.2
library(tidyverse)
library(ggplot2)
library(PBSmapping)
#source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/convert.dd.dddd.r")

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
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)
#uid <- un.sameotoj
#pwd <- pw.sameotoj
surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "3"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
cruise <- "'BI2026'"
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

#polygon to for assigning new strata to data #To bring in sf object from Github eventually - will need to identify in and out VMS data points.
spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")
#spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")

#////... END OF DEFINE SECTION ...////


# ROracle; note this can take ~ 10 sec or so, don't panic
chan <-dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

#define SQL query of numbers by shell height bin
query1 <- ("SELECT * FROM scallsur.scliveres WHERE strata_id IN (22,23,24)")

#execute query with ROracle 
BIlivefreq.dat <- dbGetQuery(chan, query1)

#add YEAR and CRUISEID column to data
BIlivefreq.dat$YEAR <- as.numeric(substr(BIlivefreq.dat$CRUISE,3,6))


# ---- post-stratify SPA3 for VMS strata area ----

#adjust data files for subsequent analysis
BIlivefreq.dat$lat <- convert.dd.dddd(BIlivefreq.dat$START_LAT)
BIlivefreq.dat$lon <- convert.dd.dddd(BIlivefreq.dat$START_LONG)
BIlivefreq.dat$ID <- 1:nrow(BIlivefreq.dat)

#identify tows "inside" the VMS strata and assign new STRATA_ID to them (99)
events <- subset(BIlivefreq.dat,STRATA_ID%in%23:24,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
BIlivefreq.dat$STRATA_ID[BIlivefreq.dat$ID%in%findPolys(events,spa3area)$EID] <- 99


# ---- CALCULATE SHF FOR EACH YEAR BY STRATA ----

#modelled years
BIlivefreq.yrs <- subset(BIlivefreq.dat, YEAR > 1995) 

#1. St. Mary's Bay
SMBlivefreq <- subset(BIlivefreq.yrs, STRATA_ID==22 & TOW_TYPE_ID==1) 
SPA3.SMB.SHFmeans <- sapply(split(SMBlivefreq[c(11:50)], SMBlivefreq$YEAR), function(x){apply(x,2,mean)})
round (SPA3.SMB.SHFmeans,2)

SPA3.SMB.SHFmeans 

#1. Inside VMS
Innerlivefreq <- subset (BIlivefreq.yrs, STRATA_ID==99 & TOW_TYPE_ID==1)
SPA3.Inner.SHFmeans <- sapply(split(Innerlivefreq[c(11:50)], Innerlivefreq$YEAR), function(x){apply(x,2,mean)})
round (SPA3.Inner.SHFmeans,2)

#1.Outside VMS
Outerlivefreq <- subset (BIlivefreq.yrs, STRATA_ID%in%23:24 & TOW_TYPE_ID==1)
SPA3.Outer.SHFmeans <- sapply(split(Outerlivefreq[c(11:50)], Outerlivefreq$YEAR), function(x){apply(x,2,mean)})
round (SPA3.Outer.SHFmeans,2)

# ---- PLOT SHF FOR EACH YEAR BY STRATA ----
#THIS NEEDS TO BE UPDATED AND MODIFIED SO TIDY AND TO USE GGPLOT - MUST FIRST MAKE DATA TIDY FORMAT 

##.. 1) Inner VMS Area: SPA3.Inner.SHFmeans
SPA3.Inner.for.plot <- data.frame(bin.label = row.names(SPA3.Inner.SHFmeans), SPA3.Inner.SHFmeans)
SPA3.Inner.for.plot$X2020 <- NA
head(SPA3.Inner.for.plot)
SPA3.Inner.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA3.Inner.for.plot <- pivot_longer(SPA3.Inner.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA3.Inner.for.plot$year <- as.numeric(SPA3.Inner.for.plot$year)

SPA3.Inner.for.plot <- SPA3.Inner.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA3.Inner.for.plot$SH <- round(SPA3.Inner.for.plot$SH,3)

ylimits <- c(0,max(SPA3.Inner.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA3.Inner.SHF <- ggplot() + geom_col(data = SPA3.Inner.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA3.Inner.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_SHF_InVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA3.Inner.SHF)
dev.off()


##.. 2) SMB: SPA3.SMB.SHFmeans
SPA3.SMB.for.plot <- data.frame(bin.label = row.names(SPA3.SMB.SHFmeans), SPA3.SMB.SHFmeans)
SPA3.SMB.for.plot$X2020 <- NA
head(SPA3.SMB.for.plot)
SPA3.SMB.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA3.SMB.for.plot <- pivot_longer(SPA3.SMB.for.plot, 
                                    cols = starts_with("X"),
                                    names_to = "year",
                                    names_prefix = "X",
                                    values_to = "SH",
                                    values_drop_na = FALSE)
SPA3.SMB.for.plot$year <- as.numeric(SPA3.SMB.for.plot$year)

SPA3.SMB.for.plot <- SPA3.SMB.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA3.SMB.for.plot$SH <- round(SPA3.SMB.for.plot$SH,3)

ylimits <- c(0,max(SPA3.SMB.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA3.SMB.SHF <- ggplot() + geom_col(data = SPA3.SMB.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA3.SMB.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_SHF_SMB.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA3.SMB.SHF)
dev.off()



##.. 3) Outer VMS: SPA3.Outer.SHFmeans
SPA3.OutVMS.for.plot <- data.frame(bin.label = row.names(SPA3.Outer.SHFmeans), SPA3.Outer.SHFmeans)
SPA3.OutVMS.for.plot$X2020 <- NA
head(SPA3.OutVMS.for.plot)
SPA3.OutVMS.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA3.OutVMS.for.plot <- pivot_longer(SPA3.OutVMS.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA3.OutVMS.for.plot$year <- as.numeric(SPA3.OutVMS.for.plot$year)

SPA3.OutVMS.for.plot <- SPA3.OutVMS.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA3.OutVMS.for.plot$SH <- round(SPA3.OutVMS.for.plot$SH,3)

ylimits <- c(0,max(SPA3.OutVMS.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA3.OutVMS.SHF <- ggplot() + geom_col(data = SPA3.OutVMS.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA3.OutVMS.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_SHF_OutVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA3.OutVMS.SHF)
dev.off()


# ---- CALCULATE STRATIFIED SHF FOR MODELLED AREA OF SPA 3 FOR EACH YEAR (JUST SMB AND VMS INNER) ----

years <- 1996:surveyyear 

bin <- as.numeric(substr(names(BIlivefreq.dat[c(grep("BIN_ID_0", colnames(BIlivefreq.dat)):grep("BIN_ID_195", colnames(BIlivefreq.dat)))]),8,nchar(names(BIlivefreq.dat[c(grep("BIN_ID_0", colnames(BIlivefreq.dat)):grep("BIN_ID_195", colnames(BIlivefreq.dat)))]))))
SPA3.SHF <- data.frame(Bin=bin, y.1996=rep(NA,40))

# 1996:SMB and InVMS (total NH=315429.3213, SMB prop=0.3737, InVMS prop=0.6263)
SPA3.SHF$y.1996 <- (SPA3.SMB.SHFmeans[,c(grep("1996",colnames(SPA3.SMB.SHFmeans)))]*0.3737)+ (SPA3.Inner.SHFmeans[,c(grep("1996",colnames(SPA3.Inner.SHFmeans)))]*0.6263)

# 1997,1998: just InVMS
SPA3.SHF$y.1997 <- SPA3.Inner.SHFmeans[,c(grep("1997",colnames(SPA3.Inner.SHFmeans)))]
SPA3.SHF$y.1998 <- SPA3.Inner.SHFmeans[,c(grep("1998",colnames(SPA3.Inner.SHFmeans)))]

# 1999-2001: SMB and InVMS (total NH=315429.3213, SMB prop=0.3737, InVMS prop=0.6263)
SPA3.SHF[,c((dim(SPA3.SHF)[2]+1):(dim(SPA3.SHF)[2]+3))] <- (SPA3.SMB.SHFmeans[,c(grep("1999",colnames(SPA3.SMB.SHFmeans)):grep("2001",colnames(SPA3.SMB.SHFmeans)))]*0.3737) + (SPA3.Inner.SHFmeans[,c(grep("1999",colnames(SPA3.Inner.SHFmeans)):grep("2001",colnames(SPA3.Inner.SHFmeans)))]*0.6263)

# 2002, 2003: just InVMS
SPA3.SHF$y.2002 <- SPA3.Inner.SHFmeans[,c(grep("2002",colnames(SPA3.Inner.SHFmeans)))]
SPA3.SHF$y.2003 <- SPA3.Inner.SHFmeans[,c(grep("2003",colnames(SPA3.Inner.SHFmeans)))]

# 2004-2019: SMB and InVMS (total NH=315429.3213, SMB prop=0.3737, InVMS prop=0.6263)
col.temp <- dim((SPA3.SMB.SHFmeans[,c(grep("2004",colnames(SPA3.SMB.SHFmeans)):grep("2019",colnames(SPA3.SMB.SHFmeans)))]*0.3737) + (SPA3.Inner.SHFmeans[,c(grep("2004",colnames(SPA3.Inner.SHFmeans)):grep("2019",colnames(SPA3.Inner.SHFmeans)))]*0.6263))[2]
SPA3.SHF[,c((dim(SPA3.SHF)[2]+1):((dim(SPA3.SHF)[2])+col.temp))] <- (SPA3.SMB.SHFmeans[,c(grep("2004",colnames(SPA3.SMB.SHFmeans)):grep("2019",colnames(SPA3.SMB.SHFmeans)))]*0.3737) + (SPA3.Inner.SHFmeans[,c(grep("2004",colnames(SPA3.Inner.SHFmeans)):grep("2019",colnames(SPA3.Inner.SHFmeans)))]*0.6263) 

# 2021-surveyyear:SMB and InVMS (total NH=315429.3213, SMB prop=0.3737, InVMS prop=0.6263)
#SPA3.SHF$y.2021 <- (SPA3.SMB.SHFmeans[,c(grep("2021",colnames(SPA3.SMB.SHFmeans)))]*0.3737)+ (SPA3.Inner.SHFmeans[,c(grep("2021",colnames(SPA3.Inner.SHFmeans)))]*0.6263)
col.temp <- dim((SPA3.SMB.SHFmeans[,c(grep("2021",colnames(SPA3.SMB.SHFmeans)):grep(paste0(surveyyear),colnames(SPA3.SMB.SHFmeans)))]*0.3737) + (SPA3.Inner.SHFmeans[,c(grep("2021",colnames(SPA3.Inner.SHFmeans)):grep(paste0(surveyyear),colnames(SPA3.Inner.SHFmeans)))]*0.6263))[2]
SPA3.SHF[,c((dim(SPA3.SHF)[2]+1):((dim(SPA3.SHF)[2])+col.temp))] <- (SPA3.SMB.SHFmeans[,c(grep("2021",colnames(SPA3.SMB.SHFmeans)):grep(paste0(surveyyear),colnames(SPA3.SMB.SHFmeans)))]*0.3737) + (SPA3.Inner.SHFmeans[,c(grep("2021",colnames(SPA3.Inner.SHFmeans)):grep(paste0(surveyyear),colnames(SPA3.Inner.SHFmeans)))]*0.6263) 

colnames(SPA3.SHF)[2:dim(SPA3.SHF)[2]] <- colnames(SPA3.Inner.SHFmeans)

SPA3.SHF <- data.frame(bin.label = row.names(SPA3.SHF), SPA3.SHF)
SPA3.SHF <- SPA3.SHF %>% add_column("X2020" = NA, .after="X2019")
SPA3.SHF$X2020 <- as.numeric(SPA3.SHF$X2020)

SPA3.SHF$bin.mid.pt <- seq(2.5,200,by=5)
head(SPA3.SHF)

#export SHF (if needed)
write.csv(SPA3.SHF, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/SPA3.SHF.modelled.area.means.to",surveyyear,".csv"))

###
### ---- PLOT SHF MODELLED AREA (JUST SMB AND VMS INNER) FOR EACH YEAR ----
###

SPA3.data.for.plot <- pivot_longer(SPA3.SHF, 
                                   cols = starts_with("X"),
                                   names_to = "year",
                                   names_prefix = "X",
                                   values_to = "SH",
                                   values_drop_na = FALSE)
SPA3.data.for.plot$year <- as.numeric(SPA3.data.for.plot$year)

SPA3.data.for.plot <- SPA3.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA3.data.for.plot$SH <- round(SPA3.data.for.plot$SH,3)

ylimits <- c(0,max(SPA3.data.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA3.SHF <- ggplot() + geom_col(data = SPA3.data.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA3.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_SHF_ModelledArea.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA3.SHF)
dev.off()


# ---- CALCULATE lbar FOR MODELLED AREA OF SPA 3 TO BE USED FOR CALCULATING GROWTH ----
#commercial mean shell height in year t
#For Growth calculations would be easier to have year corresponding to mean shell length

#years <- 1996:surveyyear 

#Just take SHF columns of commercial size 
SPA3.SHactual.Com <- SPA3.SHF[ SPA3.SHF$bin.mid.pt>=80, grepl( "X" , names( SPA3.SHF ))]

#average commercial size by year 
SPA3.SHactual.Com.lbar <- NA
for(i in 1:length(years)){
  SPA3.SHactual.Com.lbar[i] <- sum(SPA3.SHactual.Com[i]*seq(82.5,197.5,by=5))/sum(SPA3.SHactual.Com[i])
}
SPA3.SHactual.Com.lbar

#Just take SHF columns of recruit size 
SPA3.SHactual.Rec <- SPA3.SHF[ SPA3.SHF$bin.mid.pt >= 65 & SPA3.SHF$bin.mid.pt < 80, grepl( "X" , names( SPA3.SHF ))]

#average recruit size by year 
SPA3.SHactual.Rec.lbar <- NA
for(i in 1:length(years)){
  SPA3.SHactual.Rec.lbar[i] <- sum(SPA3.SHactual.Rec[i]*seq(67.5,77.5,by=5))/sum(SPA3.SHactual.Rec[i])
}
SPA3.SHactual.Rec.lbar

SPA3.lbar <- data.frame(Year = years,l.bar = SPA3.SHactual.Com.lbar, lr.bar = SPA3.SHactual.Rec.lbar)

SPA3.SHactual.Com <- data.frame((t(rbind(years, SPA3.SHactual.Com))))

SPA3.SHactual.Rec <- data.frame((t(rbind(years, SPA3.SHactual.Rec))))

#SPA3.SHactual.Rec <- rep(length(years))
#for(i in 1:length(years)){
#  temp.data <- SPA3.SHF[c(14:16),1+i]  #recruit size
#  SPA3.SHactual.Rec[i] <- sum(temp.data*seq(67.5,77.55,by=5))/sum(temp.data) }


# Plot of Mean Commercial Shell Height (lbar)
plot.SPA3.lbar <- ggplot(SPA3.lbar, aes(x = Year, y = l.bar)) + 
  geom_line() + 
  geom_point() +
  theme_bw() + ylab("Shell Height (mm)") + 
  xlim(c(1995,surveyyear+1))
plot.SPA3.lbar

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_lbar.png"), type="cairo", width=15, height=15, units = "cm", res=400)
print(plot.SPA3.lbar)
dev.off()

# ---- Predicted Lbar in t+1 ----
# expected mean shell height for year t+1 (eg., for year 1997 output is expected SH in 1998)

# use equation: Linfty*(1-exp(-exp(logK)))+exp(-exp(logK))*mean.shell.height
# inputs from Von B files; All SPA3 was modelled over all years (Y:\INSHORE SCALLOP\BoF\2016\Growth\SPA3_VonB.R)

#UPDATE FOR 2017!!!! - NOTE No aging done in 2016-17 therefore use 2016 Von B model for 2017 assessment 
#NOTE No aging done in 2017-18 therefore use 2016 Von B model for 2018 assessment 
#NOTE No aging done in 2018-19 therefore use 2016 Von B model for 2019 assessment 
#> VONBI2016.nlme
#Nonlinear mixed-effects model fit by REML
#Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0)
#Data: test.data
#Log-restricted-likelihood: -77871.33
#Fixed: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
#Linf        logK          T0
#149.9520637  -1.6498391  -0.3815667

#only need to predict for current year forward to next year
years.predict <- 1997:(surveyyear+1) #use current year +1
#years.predict <- years.predict[! years.predict %in% 2020] #No 2020 data - remove from list of years.predict

#the value in 1997 (first value) is the predicted height for 1997 based on actual height in 1996
SPA3.SHpredict.Com <- SPA3.SHactual.Com.lbar
for(i in 1:length(years.predict)){
  temp.data <- SPA3.SHactual.Com.lbar[i] #commercial size
  SPA3.SHpredict.Com[i]<-149.95*(1-exp(-exp(-1.649)))+exp(-exp(-1.649))*temp.data }

SPA3.SHpredict.Com <- data.frame((t(rbind(years.predict, SPA3.SHpredict.Com))))

#the value in 1997 (first value) is the predicted height for 1997 based on actual height in 1997
SPA3.SHpredict.Rec <- SPA3.SHactual.Rec.lbar
for(i in 1:length(years.predict)){
  temp.data <- SPA3.SHactual.Rec.lbar[i]  #recruit size
  SPA3.SHpredict.Rec[i] <- 149.95*(1-exp(-exp(-1.649)))+exp(-exp(-1.649))*temp.data}

sh.actual <- data.frame(years, SPA3.SHactual.Com = SPA3.SHactual.Com.lbar, SPA3.SHactual.Rec = SPA3.SHactual.Rec.lbar)
sh.predict <- data.frame(years, SPA3.SHpredict.Com, SPA3.SHpredict.Rec)


#export the objects to use in predicting mean weight
dump(c('sh.actual','sh.predict'),paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA3/SPA3.SHobj.",surveyyear,".R"))

write.csv(cbind(sh.actual, sh.predict %>% dplyr::select(!years)), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA3.lbar.to",surveyyear,".csv"))

# ---- CALCULATE lbar BY SUBAREA TO PLOT ---- 

#Inside and Outside VMS
#years <- 1996:surveyyear
years <- years[! years %in% 2020] #No 2020 data - remove from list of years
X <- length(years)
SPA3.vms.lbar<- data.frame(Year=years, InVMS.lbar=rep(NA,X), OutVMS.lbar=rep(NA,X))
for(i in 1:length(SPA3.vms.lbar$Year)){
  temp.data <- SPA3.Inner.SHFmeans[c(17:40),0+i]  #InVMS
  SPA3.vms.lbar[i,2]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data)

for(i in 1:length(SPA3.vms.lbar$Year)){
  temp.data <- SPA3.Outer.SHFmeans[c(17:40),0+i]  #OuterVMS
  SPA3.vms.lbar[i,3]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data)
}}
SPA3.vms.lbar <- rbind(SPA3.vms.lbar, c(2020, NA, NA)) %>% #ADD IN 2020 as NA
  arrange(Year)#Sort by Year
SPA3.vms.lbar

#SMB has missing years
years <- 1996:2003
X <- length(years)
SPA3.SMBa.lbar <- data.frame(Year=years, SMB.lbar=rep(NA,X))
SPA3.SMBa.lbar[1,"SMB.lbar"] <- sum(SPA3.SMB.SHFmeans[c(17:40),5]*seq(82.5,197.5,by=5))/sum(SPA3.SMB.SHFmeans[c(17:40),5])
SPA3.SMBa.lbar[4,"SMB.lbar"] <- sum(SPA3.SMB.SHFmeans[c(17:40),6]*seq(82.5,197.5,by=5))/sum(SPA3.SMB.SHFmeans[c(17:40),6])
SPA3.SMBa.lbar[5,"SMB.lbar"] <- sum(SPA3.SMB.SHFmeans[c(17:40),7]*seq(82.5,197.5,by=5))/sum(SPA3.SMB.SHFmeans[c(17:40),7])
SPA3.SMBa.lbar[6,"SMB.lbar"] <- sum(SPA3.SMB.SHFmeans[c(17:40),8]*seq(82.5,197.5,by=5))/sum(SPA3.SMB.SHFmeans[c(17:40),8])
SPA3.SMBa.lbar

years <- 2004:2019
X <- length(years)
SPA3.SMBb.lbar <- data.frame(Year=years, SMB.lbar=rep(NA,X))
for(i in 1:length(SPA3.SMBb.lbar$Year)){
  temp.data <- SPA3.SMB.SHFmeans[c(17:40),4+i]  #SMB
  SPA3.SMBb.lbar[i,2]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data)}
SPA3.SMBb.lbar

years <- 2021:surveyyear
X <- length(years)
SPA3.SMBc.lbar <- data.frame(Year=years, SMB.lbar=rep(NA,X))
for(i in 1:length(SPA3.SMBc.lbar$Year)){
  temp.data <- SPA3.SMB.SHFmeans[c(17:40),4+i]  #SMB
  SPA3.SMBc.lbar[i,2]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data)}
SPA3.SMBc.lbar

SPA3.SMB.lbar <- rbind(SPA3.SMBa.lbar,SPA3.SMBb.lbar, SPA3.SMBc.lbar, c(2020, NA)) %>% #ADD IN 2020 as NA
  arrange(Year) #Sort by Year
SPA3.SMB.lbar

## plot  Lbar by area for SPA 3 to plot

SPA3.strata.lbar <- merge(SPA3.SMB.lbar, SPA3.vms.lbar, by=("Year"))


SPA3.strata.lbar.for.plot <- pivot_longer(SPA3.strata.lbar, 
                                   cols = -Year,
                                   names_to = "strata",
                                   values_to = "SH",
                                   values_drop_na = FALSE)
#SPA3.strata.lbar.for.plot$year <- as.numeric(SPA3.strata.lbar.for.plot$year) #already numeric

# plot SHF 
plot.SPA3.strata.lbar <- ggplot(data = SPA3.strata.lbar.for.plot, aes(x = Year, y = SH, group=strata, color=strata)) + 
  geom_line() + geom_point()+ 
  theme_bw() +  ylab("Shell Height (mm)") + xlab("Year") + 
  scale_color_discrete(name="Strata", labels = c("Inside VMS","Outside VMS","Saint Marys Bay")) + 
  theme(legend.position = c(.05, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6))  + 
  xlim(c(1995,surveyyear+1))

plot.SPA3.strata.lbar

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_strata_lbar.png"), type="cairo", width=15, height=15, units = "cm", res=400)
print(plot.SPA3.strata.lbar)
dev.off()


###
### ---- PLOT REPEATED TOWS ----
###

#############
# read in cross reference files for spr
############

#define SQL query
query2 <- paste("SELECT * FROM scallsur.sccomparisontows",
                         " WHERE sccomparisontows.cruise =",cruise,
                        "AND COMP_TYPE= 'R'", sep="")

#execute query with ROracle 
crossref.spa3 <- dbGetQuery(chan, query2)


#assign strata ID
crossref.spa3$CruiseID<-paste(crossref.spa3$CRUISE_REF,crossref.spa3$TOW_NO_REF,sep='.')  #create CRUISE_ID on "parent" tow
BIlivefreq.dat$CruiseID<-paste(BIlivefreq.dat$CRUISE, BIlivefreq.dat$TOW_NO, sep='.')

#merge STRATA_ID from BIlivefreq to the crosssref files based on parent/reference tow
crossref.spa3<-merge (crossref.spa3, subset (BIlivefreq.dat, select=c("STRATA_ID", "CruiseID")), by.x="CruiseID", all=FALSE)

#subset for years
tows<-c(1,5)
livefreq.before<-subset (BIlivefreq.dat, YEAR==2023 & TOW_TYPE_ID%in%tows)
livefreq.after<-subset (BIlivefreq.dat, YEAR==2024 & TOW_TYPE_ID%in%tows)

#....................
#. SMB TOWS (StrataID 22) crossref.spa3[crossref.spa3$STRATA_ID==22,]
#
before<-crossref.spa3$TOW_NO_REF[crossref.spa3$STRATA_ID==22] # update  # year t-1 
after<-crossref.spa3$TOW_NO[crossref.spa3$STRATA_ID==22] # update  # year t 

data.before<-livefreq.before #update year t-1
data.after<-livefreq.after  #update year t

SMB.before<-subset (data.before, TOW_NO%in%before)
SMB.after<-subset (data.after, TOW_NO%in%after)

SMB.beforemeans<-sapply(split(SMB.before[c(11:50)], SMB.before$YEAR), function(x){apply(x,2,mean)})
SMB.aftermeans<-sapply(split(SMB.after[c(11:50)], SMB.after$YEAR), function(x){apply(x,2,mean)})

# .. plot SMB repeats
y.lim <-c(0,50)
data.ref<-SMB.beforemeans #In.beforemeans  ,  SMB.beforemeans   , out.beforemeans
data.year<-SMB.aftermeans #In.aftermeans, SMB.aftermeans, out.aftermeans
year <- surveyyear-1 #Update year t-1
tows <- length(after) 


png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_smb_repeats_SHF.png"), type="cairo", width=20, height=15, units = "cm", res=400)

#windows()
par(mfrow=c(2, 1), mar = c(2,2,0,0), omi =  c(0.75, 0.75, 0.1, 0.1), plt=c(0.1,1,0.1,1))

barplot(data.ref[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3, paste (year))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

a<-barplot(data.year[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3,paste (year+1))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

axis(side=1,at=a[seq(1,35,by=2)],labels=seq(2.5,172.5,by=10),outer=F)
mtext(side=1,line=2.5,"Shell height (mm)",outer=TRUE)
mtext("Survey mean no./tow", 2, -1, outer = T)

dev.off()


#....................
#.  Inside VMS (StrataID 99) crossref.spa3[crossref.spa3$STRATA_ID==99,]
#
before<-crossref.spa3$TOW_NO_REF[crossref.spa3$STRATA_ID==99] # update  # year t-1 
after<-crossref.spa3$TOW_NO[crossref.spa3$STRATA_ID==99] # update  # year t 

data.before<-livefreq.before #update year t-1
data.after<-livefreq.after  #update year t

In.Before<-subset (data.before, TOW_NO%in%before)
In.After<-subset (data.after, TOW_NO%in%after)

In.beforemeans<-sapply(split(In.Before[c(11:50)], In.Before$YEAR), function(x){apply(x,2,mean)})
In.aftermeans<-sapply(split(In.After[c(11:50)], In.After$YEAR), function(x){apply(x,2,mean)})

#.. plot Inner VMS repeats

y.lim <-c(0,50)
data.ref<-In.beforemeans #In.beforemeans  ,  SMB.beforemeans   , out.beforemeans
data.year<-In.aftermeans #In.aftermeans, SMB.aftermeans, out.aftermeans
year <- surveyyear-1 #Update as year t-1
tows <- length(after) #Update length(after)


png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_InnerVMS_repeats_SHF.png"), type="cairo", width=20, height=15, units = "cm", res=400)

#windows()
par(mfrow=c(2, 1), mar = c(2,2,0,0), omi =  c(0.75, 0.75, 0.1, 0.1), plt=c(0.1,1,0.1,1))

barplot(data.ref[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3, paste (year))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

a<-barplot(data.year[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3,paste (year+1))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

axis(side=1,at=a[seq(1,35,by=2)],labels=seq(2.5,172.5,by=10),outer=F)
mtext(side=1,line=2.5,"Shell height (mm)",outer=TRUE)
mtext("Survey mean no./tow", 2, -1, outer = T)

dev.off()

#....................
#.  oUTVMS (StrataID 23 OR 24) crossref.spa3[crossref.spa3$STRATA_ID%in%c(23,24),]
#. 

before<-crossref.spa3$TOW_NO_REF[crossref.spa3$STRATA_ID%in%c(23,24)] # update  # year t-1 
after<-crossref.spa3$TOW_NO[crossref.spa3$STRATA_ID%in%c(23,24)] # update  # year t 

data.before<-livefreq.before #update year t-1
data.after<-livefreq.after  #update yeaer t

out.before<-subset (data.before, TOW_NO%in%before)
out.after<-subset (data.after, TOW_NO%in%after)

out.beforemeans<-sapply(split(out.before[c(11:50)], out.before$YEAR), function(x){apply(x,2,mean)})
out.aftermeans<-sapply(split(out.after[c(11:50)], out.after$YEAR), function(x){apply(x,2,mean)})


# ... plot OUT VMS Repeats

y.lim <-c(0,50)
data.ref<-out.beforemeans #In.beforemeans  ,  SMB.beforemeans   , out.beforemeans
data.year<-out.aftermeans #In.aftermeans, SMB.aftermeans, out.aftermeans
year <- surveyyear-1 #Update year t-1
tows <- length(after) 

png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_OuterVMS_repeats_SHF.png"), type="cairo", width=20, height=15, units = "cm", res=400)

#windows()
par(mfrow=c(2, 1), mar = c(2,2,0,0), omi =  c(0.75, 0.75, 0.1, 0.1), plt=c(0.1,1,0.1,1))

barplot(data.ref[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3, paste (year))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

a<-barplot(data.year[1:35],xaxt="n",ylab="",xlab=" ",ylim=y.lim)
text(1,y.lim[2]-3,paste (year+1))
abline(v=c(19.323,15.74), lty=3)
text (30, y.lim[2]-3, paste ("N tows", tows, sep=" :"))

axis(side=1,at=a[seq(1,35,by=2)],labels=seq(2.5,172.5,by=10),outer=F)
mtext(side=1,line=2.5,"Shell height (mm)",outer=TRUE)
mtext("Survey mean no./tow", 2, -1, outer = T)

dev.off()


#Save out required objects to be saved only 
#save.image(file="SPA3_SHF2019.RData") #Necessary data is already saved above.
