###........................................###
###
###    SPA 4
###    Ratio Lined/Unlined
###
###   Rehauled July 2020 J.Sameoto
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(ROracle)
library(PEDstrata) #v.1.0.3
library(Hmisc)
library(tidyverse)
library(ggplot2)
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

#numbers by shell height bin in the lined drags;
lined.query <-   "SELECT *
                  FROM scallsur.sclinedlive_std
                  WHERE strata_id IN (1,2,3,4,5,8,9,10)
                  AND (cruise LIKE 'BA%'
                  OR cruise LIKE 'BF%'
                  OR cruise LIKE 'BI%'
                  OR cruise LIKE 'GM%'
                  OR cruise LIKE 'RF%')"

# Select data from database; execute query with ROracle; numbers by shell height bin
lined <- dbGetQuery(chan, lined.query)


#numbers by shell height bin in the UNlined drags;
unlined.query <-  "SELECT *
                   FROM scallsur.scunlinedlive_std
                   WHERE strata_id IN (1,2,3,4,5,8,9,10)
                   AND (cruise LIKE 'BA%'
                   OR cruise LIKE 'BF%'
                   OR cruise LIKE 'BI%'
                   OR cruise LIKE 'GM%'
                   OR cruise LIKE 'RF%')"


unlined <- dbGetQuery(chan,unlined.query)
# We should turn off the DB connection...
dbDisconnect(chan)

#add YEAR column to data
lined$YEAR <- as.numeric(substr(lined$CRUISE,3,6))
unlined$YEAR <- as.numeric(substr(unlined$CRUISE,3,6))
table(unlined$YEAR)

###
### ---- LINED AND UNLINED GEAR ----
###

#Use Commercial size only
#model runs from 1983+

years <- 1983:surveyyear# Runs to the most recent year by default
X <- length(years)

ratio <- data.frame(Year=years, lined=rep(NA,X), unlined=rep(NA,X))
for(i in 1:X){
  if (years[i] != 2020) { 
temp.data <- lined[lined$YEAR==years[i],]
ratio[i,2] <- summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,27:50],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst
  }}

for(i in 1:X){
  if (years[i] != 2020) { 
temp.data <- unlined[unlined$YEAR==years[i],]
ratio[i,3] <- summary(PEDstrata(temp.data,strata.SPA4.new,"STRATA_ID",catch=apply(temp.data[,27:50],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst
}}
ratio
#RATIO.  Note that in the early years of this the values differ from what is used in the model.  This output and the SPA4_ModelData.xlsx should match
# from 1992 until present, everything 1991 and earlier will differ.

#in 2020 had no survey to linear interpolation 
approx(ratio$Year, ratio$lined, xout=2020) #   98.455 Mean numbers lined gear
approx(ratio$Year, ratio$unlined, xout=2020) #   136.17 Mean numbers unlined gear  
ratio[ratio$Year==2020,c("lined", "unlined")]<-c(98.455, 136.17) 

ratio$ratiolined <- ratio$lined/ratio$unlined
ratio
names(ratio) <- c("Year","Lined","Unlined","ratiolined")


# Create a new Ratio file, the most recent year will be added to the model using the SPA4_ModelFile.R
write.csv(ratio, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA4_ratioLinedtoUnlined",surveyyear,".csv"))


###
### ---- Plots ----
###

#Survey Ratio per year 

#reset 2020 to NA since not survey in that year and dont' wnat to show 2020 in plots 
ratio$ratiolined[ratio$Year==2020] <- NA 
ratio$Lined[ratio$Year==2020] <- NA 
ratio$Unlined[ratio$Year==2020] <- NA 

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA4_ratio",surveyyear,".png"),width=11,height=8,units = "in",res=920)

ggplot(data = ratio, aes(x=Year, y=ratiolined)) + 
  geom_point() + 
  geom_line() +  
  theme_bw() + ylab("Ratio") + xlab("Year") 

dev.off()


#Survey numbers per year from lined vs unlined 
unlined.lined.for.plot <- pivot_longer(ratio[,c("Year","Lined","Unlined")], 
                                       cols = c("Lined","Unlined"),
                                       names_to = "Gear",
                                       values_to = "Value",
                                       values_drop_na = FALSE)
unlined.lined.for.plot$Year <- as.numeric(unlined.lined.for.plot$Year)

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA4_lined.vs.unlined",surveyyear,".png"),width=11,height=8,units = "in",res=920)

ggplot(data = unlined.lined.for.plot, aes(x=Year, y=Value, col=Gear, pch=Gear)) + 
  geom_point() + 
  geom_line(aes(linetype = Gear)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.08, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))

dev.off() 

### END OF SCRIPT ###