###........................................###
###
###    SPA 1A
###    Clapper Numbers per tow and Population Numbers
###    Clapper SHF
###
###
###    Rehauled July 2020 J.Sameoto
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(ROracle)
library(PEDstrata) #v.1.0.3
library(tidyverse)
library(cowplot)

# source strata deffinitions
source("Y:/Inshore/Assessment/BoF/SurveyDesignTables/BoFstratadef.R")
#source("Y:/Inshore/BoF/SurveyDesignTables/BoFstratadef.R")

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

###
# read in shell height and meat weight data from database
###
#strata.spa1a<-c(6,7,12:20,39)

# SQL
quer1 <- ("SELECT * FROM scallsur.scdeadres WHERE strata_id IN (6,7,12,13,14,15,16,17,18,19,20,39)
 AND (cruise LIKE 'BA%'
 OR cruise LIKE 'BF%'
 OR cruise LIKE 'BI%'
 OR cruise LIKE 'GM%'
 OR cruise LIKE 'RF%')")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle; numbers by shell height bin
deadfreq <- dbGetQuery(chan, quer1)

#add YEAR column to data
deadfreq$YEAR <- as.numeric(substr(deadfreq$CRUISE,3,6))

####
###
### ---- CLAPPER RECRUIT AND COMMERCIAL NUMBERS PER TOW AND POPULATION NUMBERS ---- 
###
####

####
# ---- 2 to 8 mile ----
####
years <- 1984:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.2to8.DeadRec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("2to8", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.2to8.DeadRec$Year)){
  if (years[i] != 2020) { 
temp.data<-deadfreq[deadfreq$YEAR==1983+i,]
SPA1A.2to8.DeadRec[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.DeadRec[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
}}
SPA1A.2to8.DeadRec

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.DeadRec$Year, SPA1A.2to8.DeadRec$Mean.nums, xout=2020) #  0 Mean numbers 
SPA1A.2to8.DeadRec[SPA1A.2to8.DeadRec$Year==2020,"Mean.nums"] <- 0
SPA1A.2to8.DeadRec[SPA1A.2to8.DeadRec$Year==2020,"Pop"] <- SPA1A.2to8.DeadRec[SPA1A.2to8.DeadRec$Year==2020,"Mean.nums"]*51945 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA1A.2to8.new$NH); check by seeing if matchs: 
SPA1A.2to8.DeadRec$Pop/SPA1A.2to8.DeadRec$Mean.nums
#Yes is 51945 


#Commercial no/tow and population
SPA1A.2to8.DeadComm <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("2to8", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.2to8.DeadComm$Year)){
  if (years[i] != 2020) { 
temp.data<-deadfreq[deadfreq$YEAR==1983+i,]
SPA1A.2to8.DeadComm[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.2to8.DeadComm[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA1A.2to8.DeadComm

#in 2020 had no survey to linear interpolation 
approx(SPA1A.2to8.DeadComm$Year, SPA1A.2to8.DeadComm$Mean.nums, xout=2020) #  2.4914 Mean numbers 
SPA1A.2to8.DeadComm[SPA1A.2to8.DeadComm$Year==2020,"Mean.nums"] <- 2.4914
SPA1A.2to8.DeadComm[SPA1A.2to8.DeadComm$Year==2020,"Pop"] <- SPA1A.2to8.DeadComm[SPA1A.2to8.DeadComm$Year==2020,"Mean.nums"]*51945 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA1A.2to8.new$NH); check by seeing if matchs: 
SPA1A.2to8.DeadComm$Pop/SPA1A.2to8.DeadComm$Mean.nums


#combine Recruit and Commercial dataframes for 2 to 8
SPA1A.2to8.Clappers <- rbind(SPA1A.2to8.DeadRec, SPA1A.2to8.DeadComm)


####
# ---- 8 to 16 mile ---- 
####
years <- 1984:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.8to16.DeadRec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.8to16.DeadRec$Year)){
  if (years[i] != 2020) { 
temp.data<-deadfreq[deadfreq$YEAR==1983+i,]
SPA1A.8to16.DeadRec[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.DeadRec[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,24:26],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA1A.8to16.DeadRec

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.DeadRec$Year, SPA1A.8to16.DeadRec$Mean.nums, xout=2020) #  0.069922 Mean numbers 
SPA1A.8to16.DeadRec[SPA1A.8to16.DeadRec$Year==2020,"Mean.nums"] <- 0.069922
SPA1A.8to16.DeadRec[SPA1A.8to16.DeadRec$Year==2020,"Pop"] <- SPA1A.8to16.DeadRec[SPA1A.8to16.DeadRec$Year==2020,"Mean.nums"]*199441 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA1A.8to16.noctrville.new$NH); check by seeing if matchs: 
SPA1A.8to16.DeadRec$Pop/SPA1A.8to16.DeadRec$Mean.nums


#Commercial no/tow and population
SPA1A.8to16.DeadComm <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("8to16", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.8to16.DeadComm$Year)){
  if (years[i] != 2020) {
temp.data<-deadfreq[deadfreq$YEAR==1983+i,]
SPA1A.8to16.DeadComm[i,2]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst
SPA1A.8to16.DeadComm[i,3]<-summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new, "STRATA_ID",catch=apply(temp.data[,27:50],1,sum),                                              Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA1A.8to16.DeadComm

#in 2020 had no survey to linear interpolation 
approx(SPA1A.8to16.DeadComm$Year, SPA1A.8to16.DeadComm$Mean.nums, xout=2020) #  7.9015 Mean numbers 
SPA1A.8to16.DeadComm[SPA1A.8to16.DeadComm$Year==2020,"Mean.nums"] <- 7.9015
SPA1A.8to16.DeadComm[SPA1A.8to16.DeadComm$Year==2020,"Pop"] <- SPA1A.8to16.DeadComm[SPA1A.8to16.DeadComm$Year==2020,"Mean.nums"]*199441 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA1A.8to16.noctrville.new$NH); check by seeing if matchs: 
SPA1A.8to16.DeadComm$Pop/SPA1A.8to16.DeadComm$Mean.nums


#combine Recruit and Commercial dataframes for 2 to 8
SPA1A.8to16.Clappers <- rbind(SPA1A.8to16.DeadRec, SPA1A.8to16.DeadComm)

####
# MidBay South
####
years <- 1997:surveyyear
X <- length(years)

#Recruit no/tow and population
SPA1A.MBS.DeadRec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X),method=rep("PED",X), Area=rep("MBS", X), Age=rep("Recruit", X))
for(i in 1:length(SPA1A.MBS.DeadRec$Year)){
  if (years[i] != 2020) {
temp.data<-deadfreq[deadfreq$YEAR==1996+i,]
SPA1A.MBS.DeadRec[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))
SPA1A.MBS.DeadRec[i,3]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 24:26],1,sum))*201138.52
} }
SPA1A.MBS.DeadRec

#interpolate missing years
approx(SPA1A.MBS.DeadRec$Year, SPA1A.MBS.DeadRec$Mean.nums, xout=2003) #0.15694
approx(SPA1A.MBS.DeadRec$Year, SPA1A.MBS.DeadRec$Mean.nums, xout=2004) # 0.31389

SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2003,"Mean.nums"] <- 0.15694
SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2004,"Mean.nums"] <- 0.31389
SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2003,"Pop"] <- SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2003,"Mean.nums"]*201138.52#calculate population number from interpolated no/tow
SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2003,"Pop"] <- SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2004,"Mean.nums"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.DeadRec$Year, SPA1A.MBS.DeadRec$Mean.nums, xout=2020) #  0.47857 Mean numbers 
SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2020,"Mean.nums"] <- 0.47857
SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2020,"Pop"] <- SPA1A.MBS.DeadRec[SPA1A.MBS.DeadRec$Year==2020,"Mean.nums"]*201138.52 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT ; check by seeing if matchs: 
SPA1A.MBS.DeadRec$Pop/SPA1A.MBS.DeadRec$Mean.nums



#Commercial no/tow and population
SPA1A.MBS.DeadCom <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X), method=rep("PED",X), Area=rep("MBS", X), Age=rep("Commercial", X))
for(i in 1:length(SPA1A.MBS.DeadCom$Year)){
  if (years[i] != 2020) {
temp.data<-deadfreq[deadfreq$YEAR==1996+i,]
SPA1A.MBS.DeadCom[i,2]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))
SPA1A.MBS.DeadCom[i,3]<- mean(apply(temp.data[temp.data$STRATA_ID==39 & temp.data$TOW_TYPE_ID==1, 27:50],1,sum))*201138.52
} } 
SPA1A.MBS.DeadCom

#interpolate missing years
approx(SPA1A.MBS.DeadCom$Year, SPA1A.MBS.DeadCom$Mean.nums, xout=2003) #1.1747
approx(SPA1A.MBS.DeadCom$Year, SPA1A.MBS.DeadCom$Mean.nums, xout=2004) # 1.604

SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2003,"Mean.nums"] <- 1.1747
SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2004,"Mean.nums"] <- 1.604
SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2003,"Pop"]<-SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2003,"Mean.nums"]*201138.52#calculate population number from interpolated no/tow
SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2004,"Pop"]<-SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2004,"Mean.nums"]*201138.52

#in 2020 had no survey to linear interpolation 
approx(SPA1A.MBS.DeadCom$Year, SPA1A.MBS.DeadCom$Mean.nums, xout=2020) #  2.6476 Mean numbers 
SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2020,"Mean.nums"] <- 2.6476
SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2020,"Pop"] <- SPA1A.MBS.DeadCom[SPA1A.MBS.DeadCom$Year==2020,"Mean.nums"]*201138.52 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT ; check by seeing if matchs: 
SPA1A.MBS.DeadCom$Pop/SPA1A.MBS.DeadCom$Mean.nums



#combine Recruit and Commercial dataframes for 8to16
SPA1A.MBS.Clappers <- rbind(SPA1A.MBS.DeadRec, SPA1A.MBS.DeadCom)

####
#  --- Merge all strata areas to Make Numbers dataframe for SPA1A ---- 
####

SPA1A.Clappers <- rbind(SPA1A.2to8.Clappers, SPA1A.8to16.Clappers, SPA1A.MBS.Clappers)
 
write.csv(SPA1A.Clappers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A.Index.Clappers.",surveyyear,".csv"))

####
###
###   ---- Calculate Commercial Clapper (N) Population size ----
###
####
#model only needs 1997+
SPA1A.2to8.DeadComm.simple <- SPA1A.2to8.DeadComm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1A.8to16.DeadComm.simple <- SPA1A.8to16.DeadComm %>% filter(Year >= 1997) %>% select(Year, Pop)
SPA1A.MBS.DeadCom.simple <- SPA1A.MBS.DeadCom %>% filter(Year >= 1997) %>% select(Year, Pop)

#Ensure year ranges are the same: 
SPA1A.2to8.DeadComm.simple$Year == SPA1A.8to16.DeadComm.simple$Year 
SPA1A.8to16.DeadComm.simple$Year == SPA1A.MBS.DeadCom.simple$Year

clappers.N <- data.frame(Year = SPA1A.2to8.DeadComm.simple$Year,  
                         N = (SPA1A.2to8.DeadComm.simple$Pop + SPA1A.8to16.DeadComm.simple$Pop + SPA1A.MBS.DeadCom.simple$Pop))
clappers.N$N

write.csv(clappers.N, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A.Clappers.N.formodel.",surveyyear,".csv"))

#2407208 3307299 3124698 2241693 2553577 3301929 2141849 1772048 1266256 2599658 1807355 1701771  666155  776203 1064191 1074597 
# 1570622 1450843 1406880 1083066 1404332 1410999 1687232 2237833 2788432 2756864

####
###
### ----  Plot Clapper No/tow ----
###
####

data <- SPA1A.Clappers
data$Size <- data$Age 
data$Mean.nums[data$Year==2020] <- NA #since don't want 2020 to plot in figures 

SPA1A.ClapNumbers.per.tow.plot <- ggplot(data = data, aes(x=Year, y=Mean.nums, col=Size, pch = Size)) +
  geom_point() + geom_line(aes(linetype = Size)) + facet_wrap(~Area) + 
  theme_bw() + 
  ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.8)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c('black','red'))
SPA1A.ClapNumbers.per.tow.plot

SPA1A.ClapNumbers.per.tow.plot.recent <- ggplot(data = data[data$Year>=2005,], aes(x=Year, y=Mean.nums, col=Size, pch = Size)) +
  geom_point() + geom_line(aes(linetype = Size)) + facet_wrap(~Area) +
  theme_bw() + 
  ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.9, 0.8)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c('black','red'))
SPA1A.ClapNumbers.per.tow.plot.recent


png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_ClapperNumberPerTow",surveyyear,".png"), type="cairo", width=35, height=25, units = "cm", res=300)

plot_grid(SPA1A.ClapNumbers.per.tow.plot, SPA1A.ClapNumbers.per.tow.plot.recent, 
           nrow = 2, label_x = 0.15, label_y = 0.95)
dev.off() 


####
###
### ---- CALCULATE SHF FOR EACH YEAR BY STRATA & PLOT (use yrs:1998+ for all areas) ----
###
####

#1. 2to8
years <- 1997:surveyyear
X <- length(years)

SPA1A.2to8.SHFdead <- data.frame(bin.mid.pt = seq(2.5,200,by=5), YEAR = NA) #added in 2022 
#SPA1A.2to8.SHFdead <- data.frame(Year=years,Mean.nums=rep(NA,X)) Changed in 2022 
for(i in 1:X){
  if (years[i] != 2020) {
temp.data<-deadfreq[deadfreq$YEAR==1996+i,]
for(j in 1:40){
SPA1A.2to8.SHFdead[j,i]<-summary(PEDstrata(temp.data,strata.SPA1A.2to8.new,"STRATA_ID",catch=temp.data[,10+j], Subset=temp.data$TOW_TYPE_ID==1))$yst
}} 
  if (years[i] == 2020) {
    SPA1A.2to8.SHFdead[,i] <- NA }
}

SPA1A.2to8.SHFdead.1 <- SPA1A.2to8.SHFdead

#### PLOT ###
names(SPA1A.2to8.SHFdead)
names(SPA1A.2to8.SHFdead)[1:dim(SPA1A.2to8.SHFdead)[2]] <- c(paste0("X",c(seq(1997,surveyyear)))) 
SPA1A.2to8.SHFdead$bin.label <- row.names(SPA1A.2to8.SHFdead)

head(SPA1A.2to8.SHFdead)
SPA1A.2to8.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1A.2to8.SHFdead.for.plot <- pivot_longer(SPA1A.2to8.SHFdead, 
                                      cols = starts_with("X"),
                                      names_to = "year",
                                      names_prefix = "X",
                                      values_to = "SH",
                                      values_drop_na = FALSE)
SPA1A.2to8.SHFdead.for.plot$year <- as.numeric(SPA1A.2to8.SHFdead.for.plot$year)

SPA1A.2to8.SHFdead.for.plot <- SPA1A.2to8.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1A.2to8.SHFdead.for.plot$SH <- round(SPA1A.2to8.SHFdead.for.plot$SH,3)


ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1A.2to8.SHFdead <- ggplot() + geom_col(data = SPA1A.2to8.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1A.2to8.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A.2to8_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1A.2to8.SHFdead
dev.off()



#2. 8to16
SPA1A.8to16.SHFdead <- data.frame(bin.mid.pt = seq(2.5,200,by=5), YEAR = NA) #added in 2022 
#SPA1A.8to16.SHFdead <- data.frame(Year=years,Mean.nums=rep(NA,X)) #changed in 2022
for(i in 1:X){
  if (years[i] != 2020) {
temp.data<-deadfreq[deadfreq$YEAR==1996+i,]
for(j in 1:40){
SPA1A.8to16.SHFdead[j,i]<-summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new,"STRATA_ID",catch=temp.data[,10+j], Subset=temp.data$TOW_TYPE_ID==1))$yst
}}
  if (years[i] == 2020) {
    SPA1A.8to16.SHFdead[,i] <- NA }
}

SPA1A.8to16.SHFdead

#### PLOT ###
names(SPA1A.8to16.SHFdead)
names(SPA1A.8to16.SHFdead)[1:dim(SPA1A.8to16.SHFdead)[2]] <- c(paste0("X",c(seq(1997,surveyyear)))) 
SPA1A.8to16.SHFdead$bin.label <- row.names(SPA1A.8to16.SHFdead)

SPA1A.8to16.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

head(SPA1A.8to16.SHFdead)

SPA1A.8to16.SHFdead.for.plot <- pivot_longer(SPA1A.8to16.SHFdead, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SH",
                                            values_drop_na = FALSE)
SPA1A.8to16.SHFdead.for.plot$year <- as.numeric(SPA1A.8to16.SHFdead.for.plot$year)

SPA1A.8to16.SHFdead.for.plot <- SPA1A.8to16.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1A.8to16.SHFdead.for.plot$SH <- round(SPA1A.8to16.SHFdead.for.plot$SH,3)


ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1A.8to16.SHFdead <- ggplot() + geom_col(data = SPA1A.8to16.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1A.8to16.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A.8to16_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1A.8to16.SHFdead
dev.off()



#3. MBS
MBSdeadfreq <- subset(deadfreq, STRATA_ID==39 & TOW_TYPE_ID==1 & YEAR>=1997)
SPA1A.MBS.SHFdead <- sapply(split(MBSdeadfreq[c(11:50)], MBSdeadfreq$YEAR), function(x){apply(x,2,mean)})
round(SPA1A.MBS.SHFdead,2)
SPA1A.MBS.SHFdead <- as.data.frame(SPA1A.MBS.SHFdead)

#### PLOT ###
names(SPA1A.MBS.SHFdead)
names(SPA1A.MBS.SHFdead)[1:dim(SPA1A.MBS.SHFdead)[2]] <- c(paste0("X",c(seq(1997,2002),seq(2005,2019),seq(2021,surveyyear))))
SPA1A.MBS.SHFdead$X2020 <- NA
SPA1A.MBS.SHFdead <- SPA1A.MBS.SHFdead %>% relocate(X2020, .after = X2019)
SPA1A.MBS.SHFdead$bin.label <- row.names(SPA1A.MBS.SHFdead)

head(SPA1A.MBS.SHFdead)
SPA1A.MBS.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA1A.MBS.SHFdead.for.plot <- pivot_longer(SPA1A.MBS.SHFdead, 
                                             cols = starts_with("X"),
                                             names_to = "year",
                                             names_prefix = "X",
                                             values_to = "SH",
                                             values_drop_na = FALSE)
SPA1A.MBS.SHFdead.for.plot$year <- as.numeric(SPA1A.MBS.SHFdead.for.plot$year)

SPA1A.MBS.SHFdead.for.plot <- SPA1A.MBS.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1A.MBS.SHFdead.for.plot$SH <- round(SPA1A.MBS.SHFdead.for.plot$SH,3)


ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA1A.MBS.SHFdead <- ggplot() + geom_col(data = SPA1A.MBS.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1A.MBS.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A.MBS_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA1A.MBS.SHFdead
dev.off()

### END OF SCRIPT In normal year ###


####
###
### ---- CALCULATE STRATIFIED Clapper SHF FOR ALL OF SPA 1A FOR EACH YEAR ---- 
###
####
#for curiosity, not needed for model 
##*** Won't work bc SPA1A.MBS.SHFdead missing data for year 2003 and 2004 - would need to sub in filler columns for these years in MBS to join the full dataset together for further analysis ##THIS CODE NOT FINISHED - need to interpolate SHF bins for missing years for MBS before can do this - something for future.. 

#years <- 1997:surveyyear
#str(SPA1A.MBS.SHFdead)

#bin <- as.numeric(substr(names(deadfreq[c(11:50)]),8,nchar(names(deadfreq[c(11:50)]))))
#SPA1A.Dead.SHF <- data.frame(Bin=bin, y.1997=rep(NA,40))
#SPA1A.Dead.SHF[,c(2:X)] <- (SPA1A.2to8.SHFdead*0.1147) + (SPA1A.8to16.SHFdead*0.4409) + (SPA1A.MBS.SHFdead[,c(5:dim(SPA1A.MBS.SHFdead)[2])]*0.4444) ###*** Won't work bc SPA1A.MBS.SHFdead missing data for year 2003 and 2004 - would need to sub in filler columns for these years in MBS to join the full dataset together for further analysis 
#round(SPA1A.Dead.SHF,2)

## Plot ## 