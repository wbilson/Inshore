###
###  ----  SPA 4 Shell Height Frequencies----
###    
###    J.Sameoto June 2020

#NOTE strata.spa4<-c(1:5, 8:10)

# ---- Prep work ---- 
options(stringsAsFactors = FALSE)

# required packages
library(ROracle)
library(PEDstrata) #v.1.0.2
library(tidyverse)
library(ggplot2)
library(egg)

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

## Get data: 

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# numbers by shell height bin;
query1 <- ("SELECT *
             FROM scallsur.scliveres
           WHERE strata_id IN (1,2,3,4,5,8,9,10)
           AND (cruise LIKE 'BA%'
                OR cruise LIKE 'BF%'
                OR cruise LIKE 'BI%'
                OR cruise LIKE 'GM%'
                OR cruise LIKE 'RF%')")

# Select data from database; execute query with ROracle; numbers by shell height bin
livefreq <- dbGetQuery(chan, query1)

#add YEAR column to data
livefreq$YEAR <- as.numeric(substr(livefreq$CRUISE,3,6))

# The Number of Tows by years by strata in 1A: 
options(tibble.print_max = Inf)
livefreq %>% group_by(YEAR) %>% filter(STRATA_ID %in% strata.SPA4.new$Strata) %>% summarize(n())
#note starts in 1981; recall object strata.SPA4.new  comes from BoFstratadef.R



# ---- CALCULATE SHF for SPA 4 Strata in Modelled area (strata 1:5, 8:10) ----

years <- c(min(livefreq$YEAR):2019, 2021:surveyyear) # Runs to the most recent year by default, skips 2020.

SPA4.SHFmeans <- data.frame(bin.mid.pt = seq(2.5,200,by=5), YEAR = NA) 
#SPA4.SHFmeans <-  data.frame(Year=years, Mean.nums=rep(NA,length(years))) ## Code previous like this but was wrong orientation and gave too many rows - fixed in 2022 
for(i in 1:length(years)){
temp.data <- livefreq[livefreq$YEAR==years[i],]
for(j in 1:40){
SPA4.SHFmeans[j,i] <- summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=temp.data[,10+j], Subset=temp.data$TOW_TYPE_ID==1))$yst
}}
SPA4.SHFmeans
colnames(SPA4.SHFmeans) <- c(paste0("X",years))
SPA4.SHFmeans
dim(SPA4.SHFmeans)[1] == 40 
#View(SPA4.SHFmeans)  
  
SPA4.SHFmeans <- data.frame(bin.label = row.names(SPA4.SHFmeans), SPA4.SHFmeans)
SPA4.SHFmeans$X2020 <- NA # add 2020 column.
SPA4.SHFmeans$bin.mid.pt <- seq(2.5,200,by=5)
head(SPA4.SHFmeans)

###
### ---- PLOT SHF FOR EACH YEAR ----
###

SPA4.data.for.plot <- pivot_longer(SPA4.SHFmeans, 
                                         cols = starts_with("X"),
                                         names_to = "year",
                                         names_prefix = "X",
                                         values_to = "SH",
                                         values_drop_na = FALSE)
SPA4.data.for.plot$year <- as.numeric(SPA4.data.for.plot$year)

SPA4.data.for.plot <- SPA4.data.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA4.data.for.plot$SH <- round(SPA4.data.for.plot$SH,3)

ylimits <- c(0,30)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA4.SHF <- ggplot() + geom_col(data = SPA4.data.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA4.SHF

#export SHF (if needed)
write.csv(SPA4.SHFmeans, paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA4.SHFmeans.to",surveyyear,".csv"))

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA4_SHF.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA4.SHF)
dev.off()


###
### ---- CALCULATE lbar (t and t+1) FOR ALL OF SPA 4 (Modelled area) TO BE USED FOR CALCULATING GROWTH ----
###

#1996+ onwards

#Lbar in year t
years <- c(min(livefreq$YEAR):2019, 2021:surveyyear) # Runs to the most recent year by default, skips 2020.

#Just take SHF columns of commercial size
SPA4.SHactual.Com <- SPA4.SHFmeans[ SPA4.SHFmeans$bin.mid.pt>=80, grepl( "X" , names( SPA4.SHFmeans ))]
SPA4.SHactual.Com

#average commercial size by year 
SPA4.SHactual.Com.lbar <- NA
for(i in 1:length(years)){
  SPA4.SHactual.Com.lbar[i] <- sum(SPA4.SHactual.Com[i]*seq(82.5,197.5,by=5))/sum(SPA4.SHactual.Com[i])
}

SPA4.SHactual.Com.lbar <- data.frame((t(rbind(years, SPA4.SHactual.Com.lbar))))
SPA4.SHactual.Com.lbar <-  rbind(SPA4.SHactual.Com.lbar, c(2020, NaN)) %>%  #ADD IN 2020 as NA
  arrange(years)#Sort by Year
SPA4.SHactual.Com.lbar

#Just take SHF columns of recruit size 
SPA4.SHactual.Rec <- SPA4.SHFmeans[ SPA4.SHFmeans$bin.mid.pt >= 65 & SPA4.SHFmeans$bin.mid.pt < 80, grepl( "X" , names( SPA4.SHFmeans ))]
#average recruit size by year 
SPA4.SHactual.Rec.lbar <- NA
for(i in 1:length(years)){
  SPA4.SHactual.Rec.lbar[i] <- sum(SPA4.SHactual.Rec[i]*seq(67.5,77.5,by=5))/sum(SPA4.SHactual.Rec[i])
  }

SPA4.SHactual.Rec.lbar <- data.frame((t(rbind(years, SPA4.SHactual.Rec.lbar))))
SPA4.SHactual.Rec.lbar <-  rbind(SPA4.SHactual.Rec.lbar, c(2020, NaN)) %>%  #ADD IN 2020 as NA
  arrange(years)#Sort by Year
SPA4.SHactual.Rec.lbar


sh.actual <- data.frame(SPA4.SHactual.Com.lbar, SPA4.SHactual.Rec.lbar %>% dplyr::select(SPA4.SHactual.Rec.lbar))

# Plot of Mean Commercial Shell Height (lbar)
plot.SPA4.lbar <- ggplot(sh.actual, aes(x = years, y = SPA4.SHactual.Com.lbar)) + 
  geom_line() + 
  geom_point() +
  theme_bw() + ylab("Mean Commercial Shell Height (mm)") 
plot.SPA4.lbar

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA4_lbar.png"), type="cairo", width=15, height=15, units = "cm", res=400)
print(plot.SPA4.lbar)
dev.off()


# ---- Predicted Lbar in t+1 ----
# expected mean shell height for year t+1 (eg., for year 1983 output is expected SH in 1984)

# use equation: Linfty*(1-exp(-exp(logK)))+exp(-exp(logK))*mean.shell.height
# inputs from Von B files; All BoF (SPA 1A, 1B, and 4) was modelled over all years (C:\Users\nasmithl\Documents\_INSHORE\_ASSESSMENTS\2016\BoF\2016 Assessment)

# VONBF2016.nlme
# Nonlinear mixed-effects model fit by REML
# Model: HEIGHT ~ SSasympOff(AGE, Linf, logK, T0)
# Data: test.data
# Log-restricted-likelihood: -230077
# Fixed: list(Linf ~ 1, logK ~ 1, T0 ~ 1)
# Linf      logK        T0
# 148.19668  -1.57621  -0.36559

years.predict <- c((min(livefreq$YEAR)+1):(surveyyear+1))

#Commercial size
#the value in 1996 (first value) is the predicted height for 1997
SPA4.SHpredict.Com <- rep(length(years.predict))
for(i in 1:length(years.predict)){
  temp.data <- SPA4.SHactual.Com.lbar$SPA4.SHactual.Com.lbar[i] #commercial size
  SPA4.SHpredict.Com[i] <- 148.19668*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data }

SPA4.SHpredict.Com <- data.frame((t(rbind(years.predict, SPA4.SHpredict.Com))))

#the value in 1996 (first value) is the predicted height for 1997
SPA4.SHpredict.Rec <- rep(length(years.predict))
for(i in 1:length(years.predict)){
  temp.data <- SPA4.SHactual.Rec.lbar$SPA4.SHactual.Rec.lbar[i]  #recruit size
  SPA4.SHpredict.Rec[i]<-148.19668*(1-exp(-exp(-1.576)))+exp(-exp(-1.576))*temp.data }

SPA4.SHpredict.Rec <- data.frame((t(rbind(years.predict, SPA4.SHpredict.Rec))))

sh.predict <- data.frame(SPA4.SHactual.Com.lbar %>% dplyr::select(years), SPA4.SHpredict.Com, SPA4.SHpredict.Rec %>% dplyr::select(SPA4.SHpredict.Rec)) #years = current year, years.predict = prediction year. (i.e. at year = 1999 acutal sh = X, year.predict = 2000, predicted sh = Y)


#export the objects to use in predicting mean weight
#just export 1996+ for growth rate calculation
#export all whole time series as of 2020 
dump(c('sh.actual','sh.predict'),paste0(path.directory,assessmentyear,"/Assessment/Data/Growth/SPA1A1B4and5/SPA4.SHobj.",surveyyear,".R"))

write.csv(cbind(sh.actual, sh.predict %>% dplyr::select(!years)), paste0(path.directory,assessmentyear, "/Assessment/Data/Growth/SPA",area,"/SPA4.lbar.to",surveyyear,".csv"))

# ---- SPA 4 strata_id 47 (inside 0-2 miles) SHF ----

#Define SQL query
query2 <- ("SELECT * FROM scallsur.scliveres WHERE strata_id = 47 AND (cruise LIKE 'BA%' OR cruise LIKE 'BF%' OR cruise LIKE 'BI%' OR cruise LIKE 'GM%' OR cruise LIKE 'RF%')")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle; data from tows in the 0-2 mile strata 
spa4inside <- dbGetQuery(chan, query2)

#add YEAR column to data
spa4inside$YEAR <- as.numeric(substr(spa4inside$CRUISE,3,6))

# n tows by year 
TowsbyYear <- aggregate (TOW_NO ~ STRATA_ID + YEAR, data=spa4inside, length)
TowsbyYear

#no/tow
#years <- 1981:surveyyear 
#X <- length(years)

# Shell Height Frequencies (SHF) of 0 to 2 mile 
SPA4.Inside.SHFmeans <- sapply(split(spa4inside[c(11:50)], spa4inside$YEAR), function(x){apply(x,2,mean)})
SPA4.Inside.SHFmeans <- round(SPA4.Inside.SHFmeans,3)
SPA4.Inside.SHFmeans <- data.frame(bin.label = row.names(SPA4.Inside.SHFmeans), SPA4.Inside.SHFmeans)
SPA4.Inside.SHFmeans$X2020 <- NA # add 2020 column.
SPA4.Inside.SHFmeans$bin.mid.pt <- seq(2.5,200,by=5)
head(SPA4.Inside.SHFmeans)


SPA4.Inside.for.plot <- pivot_longer(SPA4.Inside.SHFmeans, 
                                   cols = starts_with("X"),
                                   names_to = "year",
                                   names_prefix = "X",
                                   values_to = "SH",
                                   values_drop_na = FALSE)
SPA4.Inside.for.plot$year <- as.numeric(SPA4.Inside.for.plot$year)

SPA4.Inside.for.plot <- SPA4.Inside.for.plot %>% filter(year > surveyyear-6)

ylimits <- c(0,60)
xlimits <- c(0,200)
recruitlimits <- c(65,80)


# plot SHF 
plot.SPA4.Inside.SHF.temp <- ggplot() + geom_col(data = SPA4.Inside.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20)) 
plot.SPA4.Inside.SHF.temp

#my_tag <- paste0("N = ",TowsbyYear$TOW_NO[TowsbyYear$YEAR%in%unique(SPA4.Inside.for.plot$year)])
#plot.SPA4.Inside.SHF <- tag_facet(plot.SPA4.Inside.SHF.temp, 
 #         x = 190, y = 40, 
#          vjust = -1, hjust = -0.25,
#          open = "", close = "",
#          fontface = 3,
#          size = 4,
#          family = "arial",
#          tag_pool = my_tag)
#plot.SPA4.Inside.SHF
 

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA4_SHF_0to2mile.png"), type="cairo", width=18, height=24, units = "cm", res=400)
print(plot.SPA4.Inside.SHF.temp)
dev.off()

