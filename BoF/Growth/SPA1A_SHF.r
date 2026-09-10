###
###  ----  SPA 1A Shell Height Frequencies----
###    
###    J.Sameoto June 2020

# NOTE: SPA 1A Strata IDs: strata.spa1a<-c(6,7,12:20,39)

# ---- Prep work ---- 
options(stringsAsFactors = FALSE)

# required packages
library(ROracle)
library(PEDstrata) #v.1.0.2
library(tidyverse)
library(ggplot2)

#source strata definitions from Github
funcs <- "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/BoFstratadef.R" 
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# source strata definitions
#source("Y:/INSHORE SCALLOP/BoF/SurveyDesignTables/BoFstratadef.R")

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
query1 <- ("SELECT * FROM scallsur.scliveres WHERE strata_id IN (6,7,12,13,14,15,16,17,18,19,20,39) AND (cruise LIKE 'BA%' OR cruise LIKE 'BF%' OR cruise LIKE 'BI%' OR cruise LIKE 'GM%' OR cruise LIKE 'RF%')")

#Select data from database; execute query with ROracle 
livefreq <- dbGetQuery(chan, query1)

# add YEAR column to data
livefreq$YEAR <- as.numeric(substr(livefreq$CRUISE,3,6))

# creates range of years
years <-c(1997:2019, 2021:surveyyear) #Skipping 2020 (add as NAs later)

# The Number of Tows by years by strata in 1A: 
options(tibble.print_max = Inf)
#in 2 to 8 mile: 
livefreq %>% group_by(YEAR) %>% filter(STRATA_ID %in% strata.SPA1A.2to8.new$Strata) %>% summarize(n())
#in 8 to 16 mile: 
livefreq %>% group_by(YEAR) %>% filter(STRATA_ID %in% strata.SPA1A.8to16.noctrville.new$Strata) %>% summarize(n())
#in MBS: 
livefreq %>% group_by(YEAR) %>% filter(STRATA_ID %in% c(39)) %>% summarize(n())
#NOTE that in MBS there is no data for 2003 and 2004; the code will deal with this but be aware!


# ---- CALCULATE SHF FOR EACH YEAR BY STRATA   (use yrs:1998+ for all areas) ----

livefreq <- subset(livefreq, YEAR >= 1997)

#1. 2to8

SPA1A.2to8.SHFmeans <- data.frame(bin.mid.pt = seq(2.5,200,by=5), YEAR = NA) 

#SPA1A.2to8.SHFmeans <- data.frame(Year=years,Mean.nums=rep(NA,length(years))) #Changed Sept 2022 
for(i in 1:length(years)){
  temp.data <- livefreq[livefreq$YEAR== years[i],] #uses only the values in years. i.e. skips 2020

for(j in 1:40){
SPA1A.2to8.SHFmeans[j,i] <- summary(PEDstrata(temp.data,strata.SPA1A.2to8.new,"STRATA_ID",catch=temp.data[,10+j], Subset=temp.data$TOW_TYPE_ID==1))$yst
}}
names(SPA1A.2to8.SHFmeans) <- c(as.character(years))
round(SPA1A.2to8.SHFmeans,2)



#2. 8to16
SPA1A.8to16.SHFmeans <- data.frame(bin.mid.pt = seq(2.5,200,by=5), YEAR = NA) 

#SPA1A.8to16.SHFmeans <- data.frame(Year=years, Mean.nums=rep(NA,length(years))) #Changed Sept 2022 
for(i in 1:length(years)){
temp.data <- livefreq[livefreq$YEAR== years[i],]
for(j in 1:40){
SPA1A.8to16.SHFmeans[j,i] <- summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new,"STRATA_ID",catch=temp.data[,10+j], Subset=temp.data$TOW_TYPE_ID==1))$yst
}}
names(SPA1A.8to16.SHFmeans) <- c(as.character(years))
round(SPA1A.8to16.SHFmeans,2)


#3. MBS
MBSlivefreq <- subset(livefreq, STRATA_ID==39 & TOW_TYPE_ID==1)
SPA1A.MBS.SHFmeans <- sapply(split(MBSlivefreq[c(11:50)], MBSlivefreq$YEAR), function(x){apply(x,2,mean)})
round (SPA1A.MBS.SHFmeans,2)

# MBS has early years and missing years (2003,2004), needs to match 2to8 and 8to16 to combine
SPA1A.MBS.SHFmeans <- data.frame(SPA1A.MBS.SHFmeans)
SPA1A.MBS.SHFmeans <- SPA1A.MBS.SHFmeans %>% add_column("X2003" = 0, .after="X2002")
SPA1A.MBS.SHFmeans <- SPA1A.MBS.SHFmeans %>% add_column("X2004" = 0, .after="X2003")

# MBS has early years and missing years (2003,2004), needs to match 2to8 and 8to16 to combine
#SPA1A.MBS.SHFmeans <- cbind(SPA1A.MBS.SHFmeans[,5:10], matrix(0, nrow=40),matrix(0, nrow=40), SPA1A.MBS.SHFmeans[,11:dim(SPA1A#.MBS.SHFmeans)[2]])
#colnames(SPA1A.MBS.SHFmeans) <- c(colnames(SPA1A.MBS.SHFmeans)[1:6], c("2003", "2004"), colnames(SPA1A.MBS.SHFmeans)[9:length3(colnames(SPA1A.MBS.SHFmeans))])


#---- PLOT SHF FOR EACH YEAR BY STRATA ----
#  2to8, 8to16, MBS

# SHF plot for 2 to 8 mile:  SPA1A.2to8.SHFmeans
SPA1A.2to8.data.for.plot <- data.frame(bin.label = row.names(SPA1A.2to8.SHFmeans), SPA1A.2to8.SHFmeans)
SPA1A.2to8.data.for.plot$X2020 <- NA # add 2020 column.
head(SPA1A.2to8.data.for.plot)
SPA1A.2to8.data.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1A.2to8.data.for.plot <- pivot_longer(SPA1A.2to8.data.for.plot, 
                                          cols = starts_with("X"),
                                          names_to = "year",
                                          names_prefix = "X",
                                          values_to = "SH",
                                          values_drop_na = FALSE)
SPA1A.2to8.data.for.plot$year <- as.numeric(SPA1A.2to8.data.for.plot$year)

SPA1A.2to8.data.for.plot <- SPA1A.2to8.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1A.2to8.data.for.plot$SH <- round(SPA1A.2to8.data.for.plot$SH,3)

ylimits <- c(0,30)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF for 2to8 mile strata
plot.SPA1A.2to8.SHF <- ggplot() + geom_col(data = SPA1A.2to8.data.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1A.2to8.SHF


# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A_SHF_2to8.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1A.2to8.SHF)
dev.off()


# SHF plot for 8 to 16 mile:  SPA1A.8to16.SHFmeans
SPA1A.8to16.data.for.plot <- data.frame(bin.label = row.names(SPA1A.8to16.SHFmeans), SPA1A.8to16.SHFmeans)
SPA1A.8to16.data.for.plot$X2020 <- NA # add 2020 column.
head(SPA1A.8to16.data.for.plot)
SPA1A.8to16.data.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1A.8to16.data.for.plot <- pivot_longer(SPA1A.8to16.data.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA1A.8to16.data.for.plot$year <- as.numeric(SPA1A.8to16.data.for.plot$year)

SPA1A.8to16.data.for.plot <- SPA1A.8to16.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1A.8to16.data.for.plot$SH <- round(SPA1A.8to16.data.for.plot$SH,3)

ylimits <- c(0,50)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF for 8to16 mile strata 
plot.SPA1A.8to16.SHF <- ggplot() + geom_col(data = SPA1A.8to16.data.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1A.8to16.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A_SHF_8to16.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1A.8to16.SHF)
dev.off()



# SHF plot for Mid Bay South (MBS):  SPA1A.MBS.SHFmeans
MBS.data.for.plot <- data.frame(bin.label = row.names(SPA1A.MBS.SHFmeans), SPA1A.MBS.SHFmeans)
MBS.data.for.plot$X2020 <- NA # add 2020 column.
head(MBS.data.for.plot)
MBS.data.for.plot$bin.mid.pt <- seq(2.5,200,by=5)
  
MBS.data.for.plot <- pivot_longer(MBS.data.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
MBS.data.for.plot$year <- as.numeric(MBS.data.for.plot$year)

MBS.data.for.plot <- MBS.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
MBS.data.for.plot$SH <- round(MBS.data.for.plot$SH,3)

ylimits <- c(0,30)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF for MBS
plot.SPA1A.MBS.SHF<- ggplot() + geom_col(data = MBS.data.for.plot, aes(x = bin.mid.pt, y = SH)) + facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1A.MBS.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A_SHF_MBS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1A.MBS.SHF)
dev.off()



#---- CALCULATE STRATIFIED SHF FOR ALL OF SPA 1A FOR EACH YEAR ----

# create bin names and merge to combined SHF 

bin <- as.numeric(substr(names(livefreq[c(grep("BIN_ID_0", colnames(livefreq)):grep("BIN_ID_195", colnames(livefreq)))]),8,nchar(names(livefreq[c(grep("BIN_ID_0", colnames(livefreq)):grep("BIN_ID_195", colnames(livefreq)))]))))
#bin <- as.numeric(substr(names(livefreq[c(11:50)]),8,nchar(names(livefreq[c(11:50)]))))
SPA1A.SHF.bin.names <- data.frame(Bin=bin)
SPA1A.SHF <- (SPA1A.2to8.SHFmeans*0.1147) + (SPA1A.8to16.SHFmeans*0.4409) + (SPA1A.MBS.SHFmeans*0.4444)
SPA1A.SHF <- cbind(SPA1A.SHF.bin.names, SPA1A.SHF)
round(SPA1A.SHF,2)
dim(SPA1A.SHF)

# SHF plot for 1A ALL  SPA1A.ALL.SHFmeans
SPA1A.shf.data.for.plot <- data.frame(bin.label = row.names(SPA1A.SHF), SPA1A.SHF)
SPA1A.shf.data.for.plot$X2020 <- NA # add 2020 column.
head(SPA1A.shf.data.for.plot)
SPA1A.shf.data.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

SPA1A.shf.data.for.plot <- pivot_longer(SPA1A.shf.data.for.plot, 
                                  cols = starts_with("X"),
                                  names_to = "year",
                                  names_prefix = "X",
                                  values_to = "SH",
                                  values_drop_na = FALSE)
SPA1A.shf.data.for.plot$year <- as.numeric(SPA1A.shf.data.for.plot$year)

SPA1A.shf.data.for.plot <- SPA1A.shf.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA1A.shf.data.for.plot$SH <- round(SPA1A.shf.data.for.plot$SH,3)

ylimits <- c(0,30)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF for All of SPA 1A
plot.SPA1A.SHF<- ggplot() + geom_col(data = SPA1A.shf.data.for.plot, aes(x = bin.mid.pt, y = SH)) + facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA1A.SHF

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A_SHF_all.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA1A.SHF)
dev.off()



#---- CALCULATE lbar FOR ALL OF SPA 1A TO BE USED FOR CALCULATING GROWTH ----

# ACTUAL shell height by year

# Commerical size
SPA1A.SHactual.Com <- rep(length(years))
for(i in 1:length(years)){
  temp.data <- SPA1A.SHF[c(17:40),1+i]  #commercial size
  SPA1A.SHactual.Com[i] <- sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data) }

SPA1A.SHactual.Com <- data.frame((t(rbind(years, SPA1A.SHactual.Com))))
SPA1A.SHactual.Com <-  rbind(SPA1A.SHactual.Com, c(2020, NA)) %>%  #ADD IN 2020 as NA
  arrange(years)#Sort by Year

#Recruit Size 
SPA1A.SHactual.Rec <- rep(length(years))
for(i in 1:length(years)){
  temp.data2 <- SPA1A.SHF[c(14:16),1+i]   #recruit size
  SPA1A.SHactual.Rec[i]<-sum(temp.data2*seq(67.5,77.5,by=5))/sum(temp.data2)
}

SPA1A.SHactual.Rec <- data.frame((t(rbind(years, SPA1A.SHactual.Rec))))
SPA1A.SHactual.Rec <-  rbind(SPA1A.SHactual.Rec, c(2020, NA)) %>%  #ADD IN 2020 as NA
  arrange(years)#Sort by Year
SPA1A.SHactual.Rec

sh.actual <- data.frame(SPA1A.SHactual.Com, SPA1A.SHactual.Rec %>% dplyr::select(SPA1A.SHactual.Rec))

# Predicted expected mean shell height for year t+1 (eg., for year 1997 output is expected SH in 1998)
#the value in 1997 (first value) is the predicted height for 1998
years.predict <- 1998:(surveyyear+1)

# Commercial Size 
SPA1A.SHpredict.Com <- rep(length(years.predict))
for(i in 1:length(years.predict)){
  temp.data <- SPA1A.SHactual.Com$SPA1A.SHactual.Com[i] #commercial size
  SPA1A.SHpredict.Com[i] <- 148.197*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data }

SPA1A.SHpredict.Com <- data.frame((t(rbind(years.predict, SPA1A.SHpredict.Com))))

#Recruit size
SPA1A.SHpredict.Rec <- rep(length(years.predict))
for(i in 1:length(years.predict)){
  temp.data <- SPA1A.SHactual.Rec$SPA1A.SHactual.Rec[i]  #recruit size
  SPA1A.SHpredict.Rec[i] <- 148.197*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data }

SPA1A.SHpredict.Rec <- data.frame((t(rbind(years.predict, SPA1A.SHpredict.Rec))))

sh.predict <- data.frame(SPA1A.SHactual.Com %>% dplyr::select(years), SPA1A.SHpredict.Com, SPA1A.SHpredict.Rec %>% dplyr::select(SPA1A.SHpredict.Rec)) #years = current year, years.predict = prediction year. (i.e. at year = 1999 acutal sh = X, year.predict = 2000, predicted sh = Y)

# Export the objects to use in predicting mean weight/growth
dump (c('sh.actual','sh.predict'), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA1A",surveyyear,".SHobj.R"))


write.csv(cbind(sh.actual, sh.predict %>% dplyr::select(!years)), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA1A.lbar.to",surveyyear,".csv")) 


#---- CALCULATE lbar BY SUBAREA TO PLOT ----

SPA1A.lbar.strata <- data.frame(Year=years, lbar.2to8=rep(NA,length(years)), lbar.8to16=rep(NA,length(years)),lbar.MBS=rep(NA,length(years)))
for(i in 1:length(SPA1A.lbar.strata$Year)){
  temp.data <- SPA1A.2to8.SHFmeans[c(17:40),i]   #2to8
  temp.data2<- SPA1A.8to16.SHFmeans[c(17:40),i]  #8to16
  temp.data3<- SPA1A.MBS.SHFmeans[c(17:40),i]    #MBS
SPA1A.lbar.strata[i,2]<-sum(temp.data*seq(82.5,197.5,by=5))/sum(temp.data)
 SPA1A.lbar.strata[i,3]<-sum(temp.data2*seq(82.5,197.5,by=5))/sum(temp.data2)
SPA1A.lbar.strata[i,4]<-sum(temp.data3*seq(82.5,197.5,by=5))/sum(temp.data3)
}

SPA1A.lbar.strata <- rbind(SPA1A.lbar.strata, c(2020, NaN, NaN, NaN)) %>% #ADD IN 2020 as NA
  arrange(Year)
SPA1A.lbar.strata

#Reshape into long format
SPA1A.lbar.strata <- pivot_longer(SPA1A.lbar.strata, 
  cols = starts_with("lbar"),
  names_to = "Strata",
  names_prefix = "lbar.",
  values_to = "lbar",
  values_drop_na = FALSE
)

#Save out data - mean Commercial SHell Height by Strata 
write.csv(SPA1A.lbar.strata, paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA1A.lbar.by.strata.",surveyyear,".csv"))

# Plot of Mean Commercial SHell Height by Strata 
plot.SPA1A.lbar.strata <- ggplot(SPA1A.lbar.strata) + geom_line(aes(x = Year, y = lbar, color = Strata)) + 
  geom_point(aes(x = Year, y = lbar, color = Strata)) +
  theme_bw() + ylab("Mean Commercial Shell Height (mm)") + 
  scale_color_discrete(name="Strata", labels = c("2 to 8 mile strata","8 to 16 mile strata","Mid Bay South")) + 
  theme(legend.position = c(.05, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)) 
plot.SPA1A.lbar.strata

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A_lbar.png"), type="cairo", width=15, height=15, units = "cm", res=400)
print(plot.SPA1A.lbar.strata)
dev.off()


# ---- SHF for Strata 56 ----- 
# NOTE Strata 56 not used in the model - just for auxillary information ; use all tows that have been in this small strata 

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#numbers by shell height bin;
sql.56 <- "SELECT * FROM scallsur.scliveres WHERE strata_id = 56"

# Select data from database; execute query with ROracle; numbers by shell height bin
spa1a.56 <- dbGetQuery(chan, sql.56)

#add YEAR column to data
spa1a.56$YEAR <- as.numeric(substr(spa1a.56$CRUISE,3,6))

# SHF
spa1a.56.SHFmeans <- sapply(split(spa1a.56[c(11:50)], spa1a.56$YEAR), function(x){apply(x,2,mean)})
round(spa1a.56.SHFmeans,2)

# PLOT OF SHF for strata 56  

spa1a.56.data.for.plot <- data.frame(bin.label = row.names(spa1a.56.SHFmeans), spa1a.56.SHFmeans)
spa1a.56.data.for.plot$X2020 <- NA # add 2020 column.
head(spa1a.56.data.for.plot)
spa1a.56.data.for.plot$bin.mid.pt <- seq(2.5,200,by=5)

spa1a.56.data.for.plot <- pivot_longer(spa1a.56.data.for.plot, 
                                         cols = starts_with("X"),
                                         names_to = "year",
                                         names_prefix = "X",
                                         values_to = "SH",
                                         values_drop_na = FALSE)
spa1a.56.data.for.plot$year <- as.numeric(spa1a.56.data.for.plot$year)

spa1a.56.data.for.plot <- spa1a.56.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
spa1a.56.data.for.plot$SH <- round(spa1a.56.data.for.plot$SH,3)

ylimits <- c(0,150)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF for 2to8 mile strata
plot.spa1a.56.SHF <- ggplot() + geom_col(data = spa1a.56.data.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.spa1a.56.SHF


# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA1A_SHF_56.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.spa1a.56.SHF)
dev.off()
