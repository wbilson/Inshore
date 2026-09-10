###........................................###
###
###    SPA 5
###    Numbers per tow, Weight per tow, SHFs, Clappers 
###
###
###    L.Nasmith September 2016
###    Rewritten July 2021 J.Sameoto 
###........................................###


options(stringsAsFactors = FALSE)

#required packages
library(ROracle)
library(PEDstrata) #v.1.0.3
library(Hmisc)
library(tidyverse)
library(ggplot2)
library(cowplot)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle",  uid)

surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"

####
# read in shell height and meat weight data from database
###
#strata.spa5<-c(21)

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# numbers by shell height bin;
query1 <- "SELECT *
FROM scallsur.scliveres
WHERE strata_id = 21
AND (cruise LIKE 'BA%'
OR cruise LIKE 'BF%'
OR cruise LIKE 'BI%'
OR cruise LIKE 'GM%'
OR cruise LIKE 'RF%')"

# Select data from database; execute query with ROracle; numbers by shell height bin
spa5freq <- dbGetQuery(chan, query1)

#add YEAR column to data
spa5freq$YEAR <- as.numeric(substr(spa5freq$CRUISE,3,6))

###
# read in meat weight data; this is output from the meat weight/shell height modelling
###

#code for reading in multiple csvs at once and combining into one dataframe from D.Keith (2015)
Year <- c(seq(1983,2019),seq(2021,surveyyear))  #accouting for no data in 2020 
num.years <- length(Year)

BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",Year[i],".csv",sep="") ,header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

#check data structure
summary(BFliveweight)
str(BFliveweight)
table(BFliveweight$YEAR)

####
###
### ---- RECRUIT AND COMMERCIAL NUMBERS PER TOW ----
###
####

#Recruit
years <- 1990:surveyyear

#Recruit no/tow and population
SPA5.Rec <- data.frame(Year=years, Mean.nums=rep(NA,length(years)),method=rep("simple",length(years)), Area=rep("SPA5", length(years)), Age=rep("Recruit", length(years)))
for(i in 1:length(years)){
temp.data <- spa5freq[spa5freq$YEAR == years[i],]
SPA5.Rec[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==21, 24:26],1,sum))
}

SPA5.Rec

SPA5.Comm <- data.frame(Year=years, Mean.nums=rep(NA,length(years)), method=rep("simple",length(years)), Area=rep("SPA5", length(years)), Age=rep("Commercial", length(years)))
for(i in 1:length(years)){
temp.data<-spa5freq[spa5freq$YEAR== years[i],]
SPA5.Comm[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==21, 27:50],1,sum))
}

SPA5.Comm

#NOTE no survey in 2020 but bc SPA 5 not modelled, don't need to interpolate data for 2020 

###
#  Make Numbers dataframe
###
SPA5.Numbers <- rbind (SPA5.Rec, SPA5.Comm)
write.csv(SPA5.Numbers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA5.Index.Numbers",surveyyear,".csv"))


####
###
### ---- RECRUIT AND COMMERCIAL WEIGHT PER TOW -----
###
####

#Recruit no/tow and population
SPA5.RecWt <- data.frame(Year=years, Mean.wt=rep(NA,length(years)),method=rep("simple",length(years)), Area=rep("SPA5", length(years)), Age=rep("Recruit", length(years)))
for(i in 1:length(years)){
  temp.data <- BFliveweight[BFliveweight$YEAR== years[i],]
  SPA5.RecWt[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==21, 26:28],1,sum))
}

SPA5.RecWt

SPA5.CommWt <- data.frame(Year=years, Mean.wt=rep(NA,length(years)),method=rep("simple",length(years)), Area=rep("SPA5", length(years)), Age=rep("Commercial", length(years)))
for(i in 1:length(years)){
temp.data <- BFliveweight[BFliveweight$YEAR==years[i],]
SPA5.CommWt[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==21, 29:52],1,sum))
}

SPA5.CommWt


##
#  Make weight dataframe for SPA5
##
SPA5.Weight <- rbind(SPA5.RecWt,SPA5.CommWt)
SPA5.Weight$kg <- SPA5.Weight$Mean.wt/1000

write.csv(SPA5.Weight, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA5.Index.Weight",surveyyear,".csv"))

median(SPA5.Weight$kg[SPA5.Weight$Year<=2008 & SPA5.Weight$Age == "Commercial"], na.rm = TRUE)
median(SPA5.Weight$kg[SPA5.Weight$Year<=2008 & SPA5.Weight$Age == "Recruit"], na.rm = TRUE)

####
#### No/tows per survey
####
#SPA5 isn't consistently sampled, number of  tows is important to interpret data
survtow <- aggregate(TOW_NO~YEAR, data=spa5freq, length)
survtow

#Save out to input number of tows into Update document:
write.csv(survtow, paste0(path.directory,surveyyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA5.Numtows.per.survey",assessmentyear,".csv"), row.names = FALSE)

# SPA5detail<-subset(BFliveweight, STRATA_ID==21)
# detailtow<-tapply(SPA5detail$TOW_NO, SPA5detail$CRUISE, FUN = function(x) length(unique(x)))

####
#### ---- PLOT SURVEY INDEX -----
####

#separate current (2014+) from "old" series since no sampling for a number of years 

#median of "old" time series
#Commercial numbers # 79.47917, com #
#recruit numbers #22.33  rec#
#commercial biomass #1.579 com wt 
#recruit biomass #0.1321 rec wt

SPA5.Numbers$Size <- SPA5.Numbers$Age
SPA5.Numbers$Size[SPA5.Numbers$Age=="Comm"] <- "Commercial"

SPA5.Weight$Size <- SPA5.Weight$Age
SPA5.Weight$Size[SPA5.Weight$Age=="Comm"] <- "Commercial"


spa5.numbers.plot <- ggplot(data = SPA5.Numbers, aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red')) + 
  geom_hline(yintercept = median(SPA5.Numbers$Mean.nums[SPA5.Numbers$Year>=1990&SPA5.Numbers$Year<=2008&SPA5.Numbers$Size=="Commercial"], na.rm=TRUE), col="black", linetype="longdash") +
  geom_hline(yintercept = median(SPA5.Numbers$Mean.nums[SPA5.Numbers$Year>=1990&SPA5.Numbers$Year<=2008&SPA5.Numbers$Size=="Recruit"], na.rm=TRUE), col="blue", linetype="dotdash")
spa5.numbers.plot

spa5.weight.plot <- ggplot(data = SPA5.Weight, aes(x=Year, y= kg, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Mean kg/tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red')) + 
  xlim(c(min(SPA5.Numbers$Year),surveyyear)) + 
  geom_hline(yintercept = median(SPA5.Weight$kg[SPA5.Weight$Year>=1990&SPA5.Weight$Year<=2008&SPA5.Weight$Size=="Commercial"], na.rm=TRUE), col="black", linetype="longdash") +
 geom_hline(yintercept = median(SPA5.Weight$kg[SPA5.Weight$Year>=1990&SPA5.Weight$Year<=2008&SPA5.Weight$Size=="Recruit"], na.rm=TRUE), col="blue", linetype="dotdash")
spa5.weight.plot

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA5_NumberWeightPerTow",surveyyear,".png"), type="cairo", width=35, height=25, units = "cm", res=300)
plot_grid(spa5.numbers.plot, spa5.weight.plot, 
          ncol = 1, nrow = 2)
dev.off()

####
#### ---- SHF Live ----
####

SPA5.SHFmeans <- sapply(split(spa5freq[c(11:50)],spa5freq$YEAR), function(x){apply(x,2,mean)})
round(SPA5.SHFmeans,2)

SPA5.for.plot <- data.frame(bin.label = row.names(SPA5.SHFmeans), SPA5.SHFmeans)
SPA5.for.plot$X2020 <- NA
head(SPA5.for.plot)
SPA5.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA5.for.plot <- pivot_longer(SPA5.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA5.for.plot$year <- as.numeric(SPA5.for.plot$year)

SPA5.for.plot <- SPA5.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA5.for.plot$SH <- round(SPA5.for.plot$SH,3)

ylimits <- c(0,max(SPA5.for.plot$SH)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA5.SHF <- ggplot() + geom_col(data = SPA5.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA5.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA5_SHF_",surveyyear,".png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA5.SHF)
dev.off()



####
#### ---- CLAPPERS -----
####

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#numbers by shell height bin
dead.sql <- "SELECT *
FROM scallsur.scdeadres
WHERE strata_id = 21
AND (cruise LIKE 'BA%'
OR cruise LIKE 'BF%'
OR cruise LIKE 'BI%'
OR cruise LIKE 'GM%'
OR cruise LIKE 'RF%')"

# Select data from database; execute query with ROracle; numbers by shell height bin
spa5dead <- dbGetQuery(chan, dead.sql)

#add YEAR column to data
spa5dead$YEAR <- as.numeric(substr(spa5dead$CRUISE,3,6))

#Recruit

#Recruit no/tow and population
SPA5.Rec.dead <- data.frame(Year=years, Mean.nums=rep(NA,length(years)),method=rep("simple",length(years)), Area=rep("SPA5", length(years)), Age=rep("Recruit", length(years)))
for(i in 1:length(years)){
temp.data<-spa5dead[spa5dead$YEAR== years[i],]
SPA5.Rec.dead[i,2] <- mean(apply(temp.data[temp.data$STRATA_ID==21, 24:26],1,sum))
}

SPA5.Rec.dead
#NOTE no survey in 2020 but bc SPA 5 not modelled, don't need to interpolate data for 2020 

SPA5.Comm.dead <- data.frame(Year=years, Mean.nums=rep(NA,length(years)), method=rep("simple",length(years)), Area=rep("SPA5", length(years)), Age=rep("Commercial", length(years)))
for(i in 1:length(years)){
temp.data<-spa5dead[spa5dead$YEAR== years[i],]
SPA5.Comm.dead[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==21, 27:50],1,sum))
}
SPA5.Comm.dead
#NOTE no survey in 2020 but bc SPA 5 not modelled, don't need to interpolate data for 2020 

###
#  Make Numbers dataframe
##
SPA5.Numbers.dead <- rbind (SPA5.Rec.dead, SPA5.Comm.dead)
write.csv(SPA5.Numbers.dead, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA5.Index.Clappers",surveyyear,".csv"))


####
###
### ---- Plot Clapper No/tow -----
###
####

SPA5.Numbers.dead$Size <- SPA5.Numbers.dead$Age
SPA5.Numbers.dead$Size[SPA5.Numbers.dead$Age=="Comm"] <- "Commercial"

spa5.dead.numbers.plot <- ggplot(data = SPA5.Numbers.dead, aes(x=Year, y=Mean.nums, col=Size, pch=Size)) + 
  geom_point() + 
  geom_line(aes(linetype = Size)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red')) + 
  geom_hline(yintercept = median(SPA5.Numbers.dead$Mean.nums[SPA5.Numbers.dead$Year>=1990&SPA5.Numbers.dead$Year<=2008&SPA5.Numbers.dead$Size=="Commercial"], na.rm=TRUE), col="black", linetype="longdash") +
  geom_hline(yintercept = median(SPA5.Numbers.dead$Mean.nums[SPA5.Numbers.dead$Year>=1990&SPA5.Numbers.dead$Year<=2008&SPA5.Numbers.dead$Size=="Recruit"], na.rm=TRUE), col="blue", linetype="dotdash")
spa5.dead.numbers.plot
#separate current (2014+) from "old" series

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA5_ClappersPerTow",surveyyear,".png"), type="cairo", width=35, height=25, units = "cm", res=300)
spa5.dead.numbers.plot
dev.off()


####
###  ---- CLAPPER SHF ----
###
####

SPA5.SHFdead <- sapply(split(spa5dead[c(11:50)], spa5dead$YEAR), function(x){apply(x,2,mean)})
round(SPA5.SHFdead, 2)

####
###  ---- PLOT CLAPPER SHF ----
###
####

SPA5.dead.for.plot <- data.frame(bin.label = row.names(SPA5.SHFdead), SPA5.SHFdead)
SPA5.dead.for.plot$X2020 <- NA
head(SPA5.dead.for.plot)
SPA5.dead.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA5.dead.for.plot <- pivot_longer(SPA5.dead.for.plot, 
                              cols = starts_with("X"),
                              names_to = "year",
                              names_prefix = "X",
                              values_to = "SH",
                              values_drop_na = FALSE)
SPA5.dead.for.plot$year <- as.numeric(SPA5.dead.for.plot$year)

SPA5.dead.for.plot <- SPA5.dead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA5.dead.for.plot$SH <- round(SPA5.dead.for.plot$SH,3)

ylimits <- c(0,max(SPA5.dead.for.plot$SH,na.rm = TRUE)+10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA5.dead.SHF <- ggplot() + geom_col(data = SPA5.dead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA5.dead.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA5_dead_SHF_",surveyyear,".png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA5.dead.SHF)
dev.off()

