###........................................###
###
###    SPA3
###    Clapper Numbers per tow
###    Clapper Population numbers
###    Clapper SHF
### 
###    Revamped July 2021 J.Sameoto 
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library (PBSmapping)
library (spr) #version 1.04

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

# Define: 
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

#uid <- un.sameotoj
#pwd <- pw.sameotoj
surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "3"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6"
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"



# ---- read in shell height data from database ----

#Set SQL 
query.dead <-("SELECT * FROM scallsur.scdeadres WHERE strata_id IN (22,23,24)")

chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')
deadfreq <- dbGetQuery(chan, query.dead)

#add YEAR and CRUISEID column to data
deadfreq$YEAR <- as.numeric(substr(deadfreq$CRUISE,3,6))
deadfreq$CruiseID <- paste(deadfreq$CRUISE,deadfreq$TOW_NO,sep='.') #will be used to assign strata_id to cross ref files

#
# ---- post-stratify SPA3 for VMS strata ----
#polygon to for assignning new strata to data
#spa3area<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA3_VMSpoly.csv")
#spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")
spa3area <- read.csv("Y:/Inshore/Databases/Scallsur/SPA3/SPA3_VMSpoly.csv")

#adjust data files for subsequent analysis
deadfreq$lat <- convert.dd.dddd(deadfreq$START_LAT)
deadfreq$lon <- convert.dd.dddd(deadfreq$START_LONG)
deadfreq$ID <- 1:nrow(deadfreq)

#identify tows "inside" the VMS strata and assign new STRATA_ID to them (99)
events <- subset(deadfreq,STRATA_ID%in%23:24,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
deadfreq$STRATA_ID[deadfreq$ID%in%findPolys(events,spa3area)$EID] <- 99


###
# ---- COMMERCIAL and RECRUIT CLAPPERS NUMBERS PER TOW AND POPULATION NUMBERS ----
###

####
# ---- St. Mary's Bay ----
####

############ Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 1991:surveyyear
X <- length(years)

SPA3.SMB.DeadRec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), method=rep("simple",X), NH=rep (117875.8905,X), Area=rep("SMB", X), Age=rep("Recruit", X))
for(i in 1:length(SPA3.SMB.DeadRec.simple$Year)){
  temp.data <- deadfreq[deadfreq$YEAR==1990+i,]
	SPA3.SMB.DeadRec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}

SPA3.SMB.DeadRec.simple

#interpolate for missing data
approx(SPA3.SMB.DeadRec.simple$Year, SPA3.SMB.DeadRec.simple$Mean.nums, xout=1997) # 0.059649
approx(SPA3.SMB.DeadRec.simple$Year, SPA3.SMB.DeadRec.simple$Mean.nums, xout=1998) # 0.1193
approx(SPA3.SMB.DeadRec.simple$Year, SPA3.SMB.DeadRec.simple$Mean.nums, xout=2002) # 0
approx(SPA3.SMB.DeadRec.simple$Year, SPA3.SMB.DeadRec.simple$Mean.nums, xout=2003) # 0
approx(SPA3.SMB.DeadRec.simple$Year, SPA3.SMB.DeadRec.simple$Mean.nums, xout=2020) # 0

#add to dataframe
SPA3.SMB.DeadRec.simple[SPA3.SMB.DeadRec.simple$Year==1997,2] <- 4.62982
SPA3.SMB.DeadRec.simple[SPA3.SMB.DeadRec.simple$Year==1998,2] <- 0.1193
SPA3.SMB.DeadRec.simple[SPA3.SMB.DeadRec.simple$Year==2002,2] <- 0
SPA3.SMB.DeadRec.simple[SPA3.SMB.DeadRec.simple$Year==2003,2] <- 0
SPA3.SMB.DeadRec.simple[SPA3.SMB.DeadRec.simple$Year==2020,2] <- 0

SPA3.SMB.DeadRec.simple$Pop <- SPA3.SMB.DeadRec.simple$Mean.nums*SPA3.SMB.DeadRec.simple$NH

############ Commercial (80+ mm)
years <- 1991:surveyyear
X <- length(years)

SPA3.SMB.DeadComm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X),  method=rep("simple",X), NH=rep(117875.8905, X), Area=rep("SMB", X), Age=rep("Comm", X))
for(i in 1:length(SPA3.SMB.DeadComm.simple$Year))  {
  temp.data <- deadfreq[deadfreq$YEAR==1990+i,]
	SPA3.SMB.DeadComm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==22 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}

SPA3.SMB.DeadComm.simple

#interpolate for missing data
approx(SPA3.SMB.DeadComm.simple$Year, SPA3.SMB.DeadComm.simple$Mean.nums, xout=1997) #0.22193
approx(SPA3.SMB.DeadComm.simple$Year, SPA3.SMB.DeadComm.simple$Mean.nums, xout=1998) #0.44386
approx(SPA3.SMB.DeadComm.simple$Year, SPA3.SMB.DeadComm.simple$Mean.nums, xout=2002) # 0.57545
approx(SPA3.SMB.DeadComm.simple$Year, SPA3.SMB.DeadComm.simple$Mean.nums, xout=2003) # 0.89606
approx(SPA3.SMB.DeadComm.simple$Year, SPA3.SMB.DeadComm.simple$Mean.nums, xout=2020) # 3.857895

#add to dataframe
SPA3.SMB.DeadComm.simple[SPA3.SMB.DeadComm.simple$Year==1997,2] <- 0.22193
SPA3.SMB.DeadComm.simple[SPA3.SMB.DeadComm.simple$Year==1998,2] <- 0.44386
SPA3.SMB.DeadComm.simple[SPA3.SMB.DeadComm.simple$Year==2002,2] <- 0.57545
SPA3.SMB.DeadComm.simple[SPA3.SMB.DeadComm.simple$Year==2003,2] <- 0.89606
SPA3.SMB.DeadComm.simple[SPA3.SMB.DeadComm.simple$Year==2020,2] <- 3.857895

SPA3.SMB.DeadComm.simple$Pop <- SPA3.SMB.DeadComm.simple$Mean.nums*SPA3.SMB.DeadComm.simple$NH

###
# ---- Inside VMS Strata ----
###

############ Recruit (65-79 mm)

#use simple mean for recruit, only TOW_TYPE_ID==1
years <- 1991:surveyyear
X <- length(years)

#simple means
SPA3.Inner.DeadRec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), method=rep("simple",X), NH=rep(197553.4308, X), Area=rep("InVMS", X), Age=rep("Recruit",X))
for(i in 1:length(SPA3.Inner.DeadRec.simple$Year)){
  temp.data <- deadfreq[deadfreq$YEAR==1990+i,]
	SPA3.Inner.DeadRec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}

SPA3.Inner.DeadRec.simple

#no survey in 2020 so interpolate 
approx(SPA3.Inner.DeadRec.simple$Year, SPA3.Inner.DeadRec.simple$Mean.nums, xout=2020) # 0
SPA3.Inner.DeadRec.simple[SPA3.Inner.DeadRec.simple$Year==2020,2] <- 0

#make dataframe for all of Inner DeadRecruit
SPA3.Inner.DeadRec.simple$Pop <- SPA3.Inner.DeadRec.simple$Mean.nums*SPA3.Inner.DeadRec.simple$NH

############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

# Simple means for Non-spr years
years <- 1991:surveyyear
X <- length(years)

SPA3.Inner.DeadComm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), method=rep("simple",X), NH=rep(197553.4308, X), Area=rep("InVMS", X), Age=rep("Comm", X))
for(i in 1:length(SPA3.Inner.DeadComm.simple$Year)){
  temp.data <- deadfreq[deadfreq$YEAR==1990+i,]
	SPA3.Inner.DeadComm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==99 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}

SPA3.Inner.DeadComm.simple

#no survey in 2020 so interpolate 
approx(SPA3.Inner.DeadComm.simple$Year, SPA3.Inner.DeadComm.simple$Mean.nums, xout=2020) #  7.789773
SPA3.Inner.DeadComm.simple[SPA3.Inner.DeadComm.simple$Year== 2020,2] <- 7.789773

#make dataframe for all of Inner DeadRecruit
SPA3.Inner.DeadComm.simple$Pop <- SPA3.Inner.DeadComm.simple$Mean.nums*SPA3.Inner.DeadComm.simple$NH

####
# ---- Outside VMS Strata ----
####

############ Recruit (65-79 mm)
years <- 1991:surveyyear
X <- length(years)

SPA3.Outer.DeadRec.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), method=rep("simple",X), NH=rep(555399.33, X), Area=rep("OutVMS", X), Age=rep("Recruit", X))
for(i in 1:length(SPA3.Outer.DeadRec.simple$Year)){
  temp.data <- deadfreq[deadfreq$YEAR==1990+i,]
	SPA3.Outer.DeadRec.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
}

SPA3.Outer.DeadRec.simple

#no survey in 2020 so interpolate 
approx(SPA3.Outer.DeadRec.simple$Year, SPA3.Outer.DeadRec.simple$Mean.nums, xout=2020) #  0
SPA3.Outer.DeadRec.simple[SPA3.Outer.DeadRec.simple$Year== 2020,2] <- 0

SPA3.Outer.DeadRec.simple$Pop <- SPA3.Outer.DeadRec.simple$Mean.nums*SPA3.Outer.DeadRec.simple$NH

############ Commercial (80+ mm)
#use simple mean for 1991:2006 (only TOW_TYPE_ID==1); spr for 2007+

# Simple means for Non-spr years
years <- 1991:surveyyear
X <- length(years)

SPA3.Outer.DeadComm.simple <- data.frame(Year=years, Mean.nums=rep(NA,X), method=rep("simple",X), NH=rep(555399.33, X), Area=rep("OutVMS", X), Age=rep("Comm", X))
for(i in 1:length(SPA3.Outer.DeadComm.simple$Year)){
  temp.data <- deadfreq[deadfreq$YEAR==1990+i,]
	SPA3.Outer.DeadComm.simple[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID%in%23:24 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
}

SPA3.Outer.DeadComm.simple

#no survey in 2020 so interpolate 
approx(SPA3.Outer.DeadComm.simple$Year, SPA3.Outer.DeadComm.simple$Mean.nums, xout=2020) #  8.916541
SPA3.Outer.DeadComm.simple[SPA3.Outer.DeadComm.simple$Year== 2020,2] <- 8.916541

SPA3.Outer.DeadComm.simple$Pop <- SPA3.Outer.DeadComm.simple$Mean.nums*SPA3.Outer.DeadComm.simple$NH


####
#  Make Numbers dataframe for SPA3
###
SPA3.ClapNumbers <- rbind(SPA3.SMB.DeadComm.simple, SPA3.SMB.DeadRec.simple, SPA3.Inner.DeadRec.simple, 
      SPA3.Outer.DeadRec.simple, SPA3.Outer.DeadComm.simple, SPA3.Inner.DeadComm.simple)
write.csv(SPA3.ClapNumbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3.Index.Clappers.",surveyyear,".csv"))

###
### ---- Calculate  Population size (numbers) for all commercial size clappers ----
###
#model only needs 1996+, ONLY inside VMS and SMB

clappers.SPA3.SMB.DeadComm.simple <- SPA3.SMB.DeadComm.simple %>% filter(Year >= 1996) %>% dplyr::select(Year, Pop)
clappers.SPA3.Inner.DeadComm.simple <- SPA3.Inner.DeadComm.simple %>% filter(Year >= 1996) %>% dplyr::select(Year, Pop)
#Ensure year ranges are the same: 
clappers.SPA3.SMB.DeadComm.simple$Year == clappers.SPA3.Inner.DeadComm.simple$Year
#Create object for N (population size) for SPA 3 Model area: 
clappers.N <- data.frame(Year = clappers.SPA3.SMB.DeadComm.simple$Year, N = clappers.SPA3.SMB.DeadComm.simple$Pop + clappers.SPA3.Inner.DeadComm.simple$Pop)
clappers.N
clappers.N$N

write.csv(clappers.N, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3.Clappers.N.formodel.",surveyyear,".csv"))


###                      
### ---- Plot Clappers per tow ---- 
###

text <- c("Commercial size", "Recruits")
x <- c(1996,surveyyear) #update year
y <- c(0,45)
data <- SPA3.ClapNumbers[SPA3.ClapNumbers$Year>=1996&SPA3.ClapNumbers$Year!=2020,] #don't plot 2020 data point since not "real" - interpolated 

SPA3.ClapNumbers.per.tow.plot <- ggplot(data = data, aes(x=Year, y=Mean.nums, col=Age, pch = Age)) + facet_wrap(~Area, ncol = 1) + 
  geom_point() + geom_line(aes(linetype = Age)) + theme_bw() + 
  ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.06, 0.92)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c('black','red'))
SPA3.ClapNumbers.per.tow.plot

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA",area,"_clapper_Numberpertow",surveyyear,".png"),width=11,height=8,units = "in",res=920)
SPA3.ClapNumbers.per.tow.plot
dev.off() 


###
### ---- Clappers SHF ---- 
###

#1. St. Mary's Bay
SMBdeadfreq <- subset (deadfreq, STRATA_ID==22 & TOW_TYPE_ID==1)
SPA3.SMB.SHFdead <- sapply(split(SMBdeadfreq[c(11:50)], SMBdeadfreq$YEAR), function(x){apply(x,2,mean)})
round (SPA3.SMB.SHFdead,2)


#1. Inside VMS
Innerdeadfreq <- subset (deadfreq, STRATA_ID==99 & TOW_TYPE_ID==1)
SPA3.Inner.SHFdead <- sapply(split(Innerdeadfreq[c(11:50)], Innerdeadfreq$YEAR), function(x){apply(x,2,mean)})
round (SPA3.Inner.SHFdead,2)


#1.Outside VMS
Outerdeadfreq <- subset (deadfreq, STRATA_ID%in%23:24 & TOW_TYPE_ID==1)
SPA3.Outer.SHFdead <- sapply(split(Outerdeadfreq[c(11:50)], Outerdeadfreq$YEAR), function(x){apply(x,2,mean)})
round (SPA3.Outer.SHFdead,2)

###
### ---- PLOT CLAPPER SHF FOR EACH YEAR BY STRATA ---- 
###

#... Clapper: Inner VMS SHF
SPA3.Inner.SHFdead 
SPA3.Inner.SHFdead.for.plot <- data.frame(bin.label = row.names(SPA3.Inner.SHFdead), SPA3.Inner.SHFdead)
head(SPA3.Inner.SHFdead.for.plot)
SPA3.Inner.SHFdead.for.plot$X2020 <- NA 
SPA3.Inner.SHFdead.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA3.Inner.SHFdead.for.plot <- pivot_longer(SPA3.Inner.SHFdead.for.plot, 
                                     cols = starts_with("X"),
                                     names_to = "year",
                                     names_prefix = "X",
                                     values_to = "SH",
                                     values_drop_na = FALSE)
SPA3.Inner.SHFdead.for.plot$year <- as.numeric(SPA3.Inner.SHFdead.for.plot$year)

SPA3.Inner.SHFdead.for.plot <- SPA3.Inner.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA3.Inner.SHFdead.for.plot$SH <- round(SPA3.Inner.SHFdead.for.plot$SH,3)

ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA3.Inner.SHFdead.SHF <- ggplot() + geom_col(data = SPA3.Inner.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA3.Inner.SHFdead.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_clapper_SHF_InnerVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA3.Inner.SHFdead.SHF
dev.off()



#... Clapper: Outter VMS SHF
SPA3.Outer.SHFdead 
SPA3.Outer.SHFdead.for.plot <- data.frame(bin.label = row.names(SPA3.Outer.SHFdead), SPA3.Outer.SHFdead)
head(SPA3.Outer.SHFdead.for.plot)
SPA3.Outer.SHFdead.for.plot$X2020 <- NA 
SPA3.Outer.SHFdead.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA3.Outer.SHFdead.for.plot <- pivot_longer(SPA3.Outer.SHFdead.for.plot, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SH",
                                            values_drop_na = FALSE)
SPA3.Outer.SHFdead.for.plot$year <- as.numeric(SPA3.Outer.SHFdead.for.plot$year)

SPA3.Outer.SHFdead.for.plot <- SPA3.Outer.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA3.Outer.SHFdead.for.plot$SH <- round(SPA3.Outer.SHFdead.for.plot$SH,3)

ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA3.Outer.SHFdead.SHF <- ggplot() + geom_col(data = SPA3.Outer.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA3.Outer.SHFdead.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_clapper_SHF_OuterVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA3.Outer.SHFdead.SHF
dev.off()

#... Clapper: SMB VMS SHF
SPA3.SMB.SHFdead 
SPA3.SMB.SHFdead.for.plot <- data.frame(bin.label = row.names(SPA3.SMB.SHFdead), SPA3.SMB.SHFdead)
head(SPA3.SMB.SHFdead.for.plot)
SPA3.SMB.SHFdead.for.plot$X2020 <- NA 
SPA3.SMB.SHFdead.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA3.SMB.SHFdead.for.plot <- pivot_longer(SPA3.SMB.SHFdead.for.plot, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SH",
                                            values_drop_na = FALSE)
SPA3.SMB.SHFdead.for.plot$year <- as.numeric(SPA3.SMB.SHFdead.for.plot$year)

SPA3.SMB.SHFdead.for.plot <- SPA3.SMB.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA3.SMB.SHFdead.for.plot$SH <- round(SPA3.SMB.SHFdead.for.plot$SH,3)

ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA3.SMB.SHFdead.SHF <- ggplot() + geom_col(data = SPA3.SMB.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA3.SMB.SHFdead.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA3_clapper_SHF_SMB.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA3.SMB.SHFdead.SHF
dev.off()

