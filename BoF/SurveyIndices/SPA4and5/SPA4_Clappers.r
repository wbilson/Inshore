###........................................###
###
###    SPA 4
###    Clapper Numbers per tow, Population numbers
###    Clapper SHF
###
###  Revamped July 2021 J.SAmeoto 
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(ROracle)
library(PEDstrata) #v.1.0.3
library(Hmisc)
library(tidyverse)
library(cowplot)

# source strata definitions
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


# read in shell height and meat weight data from database
#strata.spa4<-c(1:5, 8:10)  #also strata_id 47 (inside 0-2 miles), but that is not included in the general SPA 4 analysis (see end of script)

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# numbers by shell height bin;
query1 <- ("SELECT *
FROM scallsur.scdeadres
WHERE strata_id IN (1,2,3,4,5,8,9,10)
AND (cruise LIKE 'BA%'
OR cruise LIKE 'BF%'
OR cruise LIKE 'BI%'
OR cruise LIKE 'GM%'
OR cruise LIKE 'RF%')")

# Select data from database; execute query with ROracle; numbers by shell height bin of clappers 
deadfreq <- dbGetQuery(chan, query1)

#add YEAR column to data
deadfreq$YEAR <- as.numeric(substr(deadfreq$CRUISE,3,6))


###
### ---- CLAPPER RECRUIT AND COMMERCIAL NUMBERS PER TOW AND POPULATION NUMBERS ----
###

#Recruit
years <- 1981:surveyyear # Runs to the most recent year by default 
years
X <- length(years)
X

SPA4.DeadRec <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X) ,method=rep("PED",X), Area=rep("SPA4", X), Age=rep("Recruit", X))
for(i in 1:length(SPA4.DeadRec$Year)){
  if (years[i] != 2020) { 
  temp.data<-deadfreq[deadfreq$YEAR==1980+i,]
  SPA4.DeadRec[i,2]<-summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,24:26],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst
  SPA4.DeadRec[i,3]<-summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,24:26],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$Yst
}}
SPA4.DeadRec

#in 2000, recruits were definded as 40-79 mm due to fast growth
BF2000.dead<-subset(deadfreq, YEAR==2000)
Deadrec.mod<-PEDstrata(BF2000.dead,strata.SPA4.new,"STRATA_ID",catch=apply(BF2000.dead[,19:26],1,sum), Subset=BF2000.dead$TOW_TYPE_ID==1)
summary (Deadrec.mod)$yst   # 6.1469
summary (Deadrec.mod)$Yst #965854
#update
SPA4.DeadRec[SPA4.DeadRec$Year==2000,"Mean.nums"]<-6.1469
SPA4.DeadRec[SPA4.DeadRec$Year==2000,"Pop"]<-965854

#in 2020 had no survey to linear interpolation 
approx(SPA4.DeadRec$Year, SPA4.DeadRec$Mean.nums, xout=2020) #  XXXXX Mean numbers 
SPA4.DeadRec[SPA4.DeadRec$Year==2020,"Mean.nums"] <- 0.017755
SPA4.DeadRec[SPA4.DeadRec$Year==2020,"Pop"] <- SPA4.DeadRec[SPA4.DeadRec$Year==2020,"Mean.nums"]*157128 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA4.new$NH); check by seeing if matchs: 
SPA4.DeadRec$Pop/SPA4.DeadRec$Mean.nums
#Yes is 157128
SPA4.DeadRec

#Commercial
years <- 1981:surveyyear
years
X <- length(years)
X

SPA4.DeadComm <- data.frame(Year=years, Mean.nums=rep(NA,X),Pop=rep(NA,X) ,method=rep("PED",X), Area=rep("SPA4", X), Age=rep("Commercial", X))
for(i in 1:length(SPA4.DeadComm$Year)){
  if (years[i] != 2020) {
  temp.data<-deadfreq[deadfreq$YEAR==1980+i,]
  SPA4.DeadComm[i,2]<-summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,27:50],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst
  SPA4.DeadComm[i,3]<-summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,27:50],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$Yst
} }
SPA4.DeadComm

#in 2020 had no survey to linear interpolation 
approx(SPA4.DeadComm$Year, SPA4.DeadComm$Mean.nums, xout=2020) #  XXXXX Mean numbers 
SPA4.DeadComm[SPA4.DeadComm$Year==2020,"Mean.nums"] <-  6.8904
SPA4.DeadComm[SPA4.DeadComm$Year==2020,"Pop"] <- SPA4.DeadComm[SPA4.DeadComm$Year==2020,"Mean.nums"]*157128 #calculate population number from interpolated no/tow; MUST CONFIRM THIS NUMBER IS RIGHT - Bumping by sum(strata.SPA4.new$NH); check by seeing if matchs: 
SPA4.DeadComm$Pop/SPA4.DeadComm$Mean.nums
#Yes is 157128
SPA4.DeadComm

###
#  Make Numbers dataframe for Clappers
###
SPA4.Clappers <- rbind(SPA4.DeadRec, SPA4.DeadComm)
#write.csv(SPA4.Clappers, "Y:/INSHORE SCALLOP/BoF/dataoutput/SPA4.Index.Clappers.2019.csv")
write.csv(SPA4.Clappers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA4.Index.Clappers.",surveyyear,".csv"))

###
### ---- Calculate Commercial Clapper (N) Population size ----
###
#model only needs 1983+

clappers <- SPA4.DeadComm[SPA4.DeadComm$Year>1982,]$Pop
clappers
#[1]   892059   899411   477504   669072  1292933  1808124 48430384 81036938 13845252  2043756   813981   613533  1141230   866322   773407  1008580   723040
#[18]  1099560  2327338  2578780  3420790  2095318   748552  1471109   532701   573789   478229   286895   341813   547138   772835   927518  1132741  1063309
#[35]  1049360  1164372   969389  1082675  1195973  1361058  1691054

###
### ---- Plot Clapper No/tow ---- 
###
data <- SPA4.Clappers[SPA4.Clappers$Year>=1981,]
data$Size <- data$Age 
data$Mean.nums[data$Year==2020] <- NA #since don't want 2020 to plot in figures 

SPA4.ClapNumbers.per.tow.plot <- ggplot(data = data, aes(x=Year, y=Mean.nums, col=Size, pch = Size)) +
  geom_point() + geom_line(aes(linetype = Size)) + theme_bw() + 
  ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.8, 0.92)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c('black','red'))
SPA4.ClapNumbers.per.tow.plot

SPA4.ClapNumbers.per.tow.plot.recent <- ggplot(data = data[data$Year>=2005,], aes(x=Year, y=Mean.nums, col=Size, pch = Size)) +
  geom_point() + geom_line(aes(linetype = Size)) + theme_bw() + 
  ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.7, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c('black','red'))
SPA4.ClapNumbers.per.tow.plot.recent


png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA4_ClapperNumberPerTow",surveyyear,".png"), type="cairo", width=35, height=25, units = "cm", res=300)

plot_grid(SPA4.ClapNumbers.per.tow.plot, SPA4.ClapNumbers.per.tow.plot.recent, 
          ncol = 2, nrow = 1, label_x = 0.15, label_y = 0.95)
dev.off() 

###
### ---- CALCULATE SHF FOR EACH YEAR BY STRATA   (use yrs:1998+ for all areas) ----
###

years <- c(1983:2019, 2021:surveyyear) # Runs to the most recent year by default, skips 2020.
#X <- length(years)
SPA4.SHFdead <- data.frame(bin.mid.pt = seq(2.5,200,by=5), YEAR = NA) 
#SPA4.SHFdead <- data.frame(rep(NA,X)) ## Code previous like this but was wrong orientation and gave too many rows - fixed in 2023
for(i in 1:length(years)){
  temp.data <- deadfreq[deadfreq$YEAR==years[i],]
  for(j in 1:40){
    SPA4.SHFdead[j,i] <- summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=temp.data[,10+j], Subset=temp.data$TOW_TYPE_ID==1))$yst
  }}
SPA4.SHFdead
colnames(SPA4.SHFdead) <- c(paste0("X",years))
SPA4.SHFdead
dim(SPA4.SHFdead)[1] == 40 
#View(SPA4.SHFdead)  

SPA4.SHFdead <- data.frame(bin.label = row.names(SPA4.SHFdead), SPA4.SHFdead)
SPA4.SHFdead$X2020 <- NA # add 2020 column.
SPA4.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)
head(SPA4.SHFdead)

## Code previous like this but gave too many rows - fixed above in 2023
#years <- 1983:surveyyear 
#X <- length(years)

#SPA4.SHFdead <- data.frame(rep(NA,X))
#for(i in 1:X){
#  if (years[i] != 2020) {
#temp.data<-deadfreq[deadfreq$YEAR==1982+i,]
#for(j in 1:40){
#SPA4.SHFdead[j,i]<-summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=temp.data[,10+j], Subset=temp.data$TOW_TYPE_ID==1))$yst
#}}
#  if (years[i] == 2020) {
#    SPA4.SHFdead[,i] <- NA }
#}

#SPA4.SHFdead

#names(SPA4.SHFdead)
#names(SPA4.SHFdead)[1:dim(SPA4.SHFdead)[2]] <- c(paste0("X",c(seq(1983,surveyyear))))
#SPA4.SHFdead$bin.label <- row.names(SPA4.SHFdead)

#head(SPA4.SHFdead)
#SPA4.SHFdead$bin.mid.pt <- seq(2.5,200,by=5)

SPA4.SHFdead.for.plot <- pivot_longer(SPA4.SHFdead, 
                                    cols = starts_with("X"),
                                    names_to = "year",
                                    names_prefix = "X",
                                    values_to = "SH",
                                    values_drop_na = FALSE)
SPA4.SHFdead.for.plot$year <- as.numeric(SPA4.SHFdead.for.plot$year)

SPA4.SHFdead.for.plot <- SPA4.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA4.SHFdead.for.plot$SH <- round(SPA4.SHFdead.for.plot$SH,3)


ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA4.SHFdead <- ggplot() + geom_col(data = SPA4.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA4.SHFdead

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA4_clapper_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA4.SHFdead
dev.off()



###
### ---- CALUCLATE LBAR OF CLAPPERS ----
###

years <- 1983:surveyyear #WILL NEED TO BE UPDATED TO "surveyyear" object when have 2021 data (if run with surveyyear = 2021 without 2021 data won't work)
comm.SHF.dead <- SPA4.SHFdead %>%  filter(bin.mid.pt > 80)  %>% select(-c(bin.label, bin.mid.pt))
SPA4.DEAD.lbar <- rep(length(years))
for(i in 1:length(years)){
  temp.data <- comm.SHF.dead[,0+i] 
  #temp.data <- SPA4.SHFdead[c(17:40),0+i]  #commercial size
  SPA4.DEAD.lbar[i]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data)
}
SPA4.DEAD.lbar <- data.frame(SPA4.DEAD.lbar)
SPA4.DEAD.lbar$years <- years 

#plot clapper lbar
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA4_clapper_lbar.png"), type="cairo", width=18, height=24, units = "cm", res=400)
ggplot(data = SPA4.DEAD.lbar, aes(x=years,y=SPA4.DEAD.lbar)) + 
         geom_point() + geom_line() + 
         theme_bw() + xlab("Year") + ylab("Commercial Mean Clapper Shell Height (mm)")
dev.off()

