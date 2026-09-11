###........................................###
###
###    SPA 1A
###    Ratio Lined/Unlined
###
###
###  Revamped July 2021 J.SAmeoto 
###........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(PEDstrata) #v.1.0.2

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


###
# read in shell height and meat weight data from database
###
#strata.spa1a<-c(6,7,12:20,39)

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#SQL - numbers by shell height bin   #code will exclude cruises not to be used: UB% and JJ%
#numbers by shell height bin in the lined drags

lined.query <-  "SELECT *
                 FROM scallsur.sclinedlive_std
                 WHERE strata_id IN (6,7,12,13,14,15,16,17,18,19,20,39)
                 AND (cruise LIKE 'BA%'
                 OR cruise LIKE 'BF%'
                 OR cruise LIKE 'BI%'
                 OR cruise LIKE 'GM%'
                 OR cruise LIKE 'RF%')"

# Select data from database; execute query with ROracle; numbers by shell height bin LINED drags
lined <- dbGetQuery(chan, lined.query)


#numbers by shell height bin in the UNlined drags;
unlined.query <-"SELECT *
                 FROM scallsur.scunlinedlive_std
                 WHERE strata_id IN (6,7,12,13,14,15,16,17,18,19,20,39)
                 AND (cruise LIKE 'BA%'
                 OR cruise LIKE 'BF%'
                 OR cruise LIKE 'BI%'
                 OR cruise LIKE 'GM%'
                 OR cruise LIKE 'RF%')"

# Select data from database; execute query with ROracle; numbers by shell height bin UNLINED drags
unlined <- dbGetQuery(chan,unlined.query)

#Disconnect from db 
dbDisconnect(chan)              
        
#add YEAR column to data
lined$YEAR <- as.numeric(substr(lined$CRUISE,3,6))
unlined$YEAR <- as.numeric(substr(unlined$CRUISE,3,6))

###
### ---- LINED GEAR: Numbers per Tow ----
###
#Use Commercial size only
#model runs from 1997+

###
# --- LINED: 2 to 8 mile & 8 to 16 mile---- 
###

years <- 1997:surveyyear 
X <- length(years)
dat <- lined

lined.2to16.com <- data.frame(Year=years,Lined.28=rep(NA,X), Lined.816=rep(NA,X))
for(i in 1:length(lined.2to16.com$Year)){
  if (years[i] != 2020) { 
temp.data<-dat[dat$YEAR==1996+i,]
lined.2to16.com[i,2] <- summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,grep("BIN_ID_80",colnames(temp.data)):grep("BIN_ID_195",colnames(temp.data))
],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst*0.1148 #proportion of area for 2to8
lined.2to16.com[i,3] <- summary(PEDstrata(temp.data, strata.SPA1A.8to16.noctrville.new,"STRATA_ID",catch=apply(temp.data[,grep("BIN_ID_80",colnames(temp.data)):grep("BIN_ID_195",colnames(temp.data))
],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst*0.4407 #proportion of area for 8to16
} }
lined.2to16.com

#in 2020 had no survey to linear interpolation 
approx(lined.2to16.com$Year, lined.2to16.com$Lined.28, xout=2020) #  13.888 Mean numbers 
lined.2to16.com[lined.2to16.com$Year==2020,"Lined.28"] <- 13.888
approx(lined.2to16.com$Year, lined.2to16.com$Lined.816, xout=2020) #  85.596 Mean numbers 
lined.2to16.com[lined.2to16.com$Year==2020,"Lined.816"] <- 85.596


###
# --- LINED: MBS ---
###

lined.MBS <- subset (lined, STRATA_ID==39)
dat <- lined.MBS

MBS.lined.com <- data.frame(Year=years, Lined.MBS=rep(NA,X))
for(i in 1:length(MBS.lined.com$Year)){
  temp.data <- dat[dat$YEAR==1996+i,]
  MBS.lined.com[i,2] <- mean(apply(temp.data[,grep("BIN_ID_80",colnames(temp.data)):grep("BIN_ID_195",colnames(temp.data))
],1,sum))*0.4445 #proportion of area for MBS
  }
MBS.lined.com

# interpolate missing years
# 2003, 2004 missing. Estimate using simple interpolation
approx(MBS.lined.com$Year, MBS.lined.com$Lined.MBS, xout=2003) # 31.53*0.4445 =
approx(MBS.lined.com$Year, MBS.lined.com$Lined.MBS, xout=2004) #37.051*0.4445 =
MBS.lined.com[MBS.lined.com$Year==2003,"Lined.MBS"] <- 14.015
MBS.lined.com[MBS.lined.com$Year==2004,"Lined.MBS"] <- 16.469

#in 2020 had no survey to linear interpolation 
approx(MBS.lined.com$Year, MBS.lined.com$Lined.MBS, xout=2020) #  28.26 Mean numbers 
MBS.lined.com[MBS.lined.com$Year==2020,"Lined.MBS"] <- 28.26



# merge lined dataframes
spa1a.lined <- merge(lined.2to16.com,MBS.lined.com, by.x="Year")
spa1a.lined$Lined.1A <- apply(spa1a.lined[,c(grep("Lined.28",colnames(spa1a.lined)),grep("Lined.816",colnames(spa1a.lined)),grep("Lined.MBS",colnames(spa1a.lined)))
], 1, sum)


###
###   ---- UNLINED GEAR: Numbers per Tow ---- 
###
#Use Commercial size only
#model runs from 1997+

###
# --- UNLINED: 2 to 8 mile & 8 to 16 mile---- 
###

years <- 1997:surveyyear 
X <- length(years)
dat <- unlined

unlined.2to16.com <- data.frame(Year=years,Unlined.28=rep(NA,X), Unlined.816=rep(NA,X))
for(i in 1:length(unlined.2to16.com$Year)){
  if (years[i] != 2020) { 
temp.data<-dat[dat$YEAR==1996+i,]
unlined.2to16.com[i,2] <- summary(PEDstrata(temp.data, strata.SPA1A.2to8.new, "STRATA_ID",catch=apply(temp.data[,grep("BIN_ID_80",colnames(temp.data)):grep("BIN_ID_195",colnames(temp.data))],1,sum),
Subset=temp.data$TOW_TYPE_ID==1))$yst*0.1148 #proportion of area for 2to8
unlined.2to16.com[i,3] <- summary(PEDstrata(temp.data,strata.SPA1A.8to16.noctrville.new,"STRATA_ID",catch=apply(temp.data[,grep("BIN_ID_80",colnames(temp.data)):grep("BIN_ID_195",colnames(temp.data))],1,sum), Subset=temp.data$TOW_TYPE_ID==1))$yst*0.4407 #proportion of area for 8to16
} }
unlined.2to16.com

#in 2020 had no survey to linear interpolation 
approx(unlined.2to16.com$Year, unlined.2to16.com$Unlined.28, xout=2020) #  12.064 Mean numbers 
unlined.2to16.com[unlined.2to16.com$Year==2020,"Unlined.28"] <- 12.064
approx(unlined.2to16.com$Year, unlined.2to16.com$Unlined.816, xout=2020) #  83.459 Mean numbers 
unlined.2to16.com[unlined.2to16.com$Year==2020,"Unlined.816"] <- 83.459



###
# --- UNLINED: MBS ---
###

unlined.MBS <- subset (unlined, STRATA_ID==39)
dat <- unlined.MBS

MBS.unlined.com <- data.frame(Year=years, Unlined.MBS=rep(NA,X))
for(i in 1:length(MBS.unlined.com$Year)){
  temp.data <- dat[dat$YEAR==1996+i,]
  MBS.unlined.com[i,2] <- mean(apply(temp.data[,grep("BIN_ID_80",colnames(temp.data)):grep("BIN_ID_195",colnames(temp.data))],1,sum))*0.4445 #proportion of area for MBS
  }
MBS.unlined.com

# interpolate missing years
approx(MBS.unlined.com$Year, MBS.unlined.com$Unlined.MBS, xout=2003) 
approx(MBS.unlined.com$Year, MBS.unlined.com$Unlined.MBS, xout=2004) 
MBS.unlined.com[MBS.unlined.com$Year==2003, "Unlined.MBS"] <- 38.529
MBS.unlined.com[MBS.unlined.com$Year==2004, "Unlined.MBS"] <- 40.932

#in 2020 had no survey to linear interpolation 
approx(MBS.unlined.com$Year, MBS.unlined.com$Unlined.MBS, xout=2020) #  28.928 Mean numbers 
MBS.unlined.com[MBS.unlined.com$Year==2020,"Unlined.MBS"] <- 28.928



# merge unlined dataframes
spa1a.unlined <- merge(unlined.2to16.com,MBS.unlined.com, by.x="Year")
spa1a.unlined$Unlined.1A <- apply(spa1a.unlined[,c(grep("Unlined.28",colnames(spa1a.unlined)),grep("Unlined.816",colnames(spa1a.unlined)),grep("Unlined.MBS",colnames(spa1a.unlined)))], 1, sum)


###
### ---- CALCUALTE RATIO FOR MODEL ----
###

# merge for ratio
ratio <- merge(spa1a.lined[,c(grep("Year",colnames(spa1a.lined)),grep("Lined.1A",colnames(spa1a.lined)))],
               spa1a.unlined[,c(grep("Year",colnames(spa1a.unlined)),grep("Unlined.1A",colnames(spa1a.unlined)))],
               by.x="Year")
ratio$ratiolined <- round(ratio$Lined.1A/ratio$Unlined.1A,4)

names(ratio) <- c("Year","Lined","Unlined","ratiolined")

# Create a new Ratio file, the most recent year will be added to the model using the SPA4_ModelFile.R
write.csv(ratio, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA1A_ratioLinedtoUnlined",surveyyear,".csv"))


# ---- Plot ratio over time ----
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA1A_Ratio.png"), type="cairo", width=15, height=15, units = "cm", res=400)
ggplot(ratio, aes(x=Year, y=ratiolined)) + 
  geom_point() + 
    geom_line() + ylab("Ratio") + 
  scale_x_continuous(limits = c(min(ratio$Year), max(ratio$Year+1))) + 
    theme_bw()
dev.off()


#Survey numbers per year from lined vs unlined 
unlined.lined.for.plot <- pivot_longer(ratio[,c("Year","Lined","Unlined")], 
                                       cols = c("Lined","Unlined"),
                                       names_to = "Gear",
                                       values_to = "Value",
                                       values_drop_na = FALSE)
unlined.lined.for.plot$Year <- as.numeric(unlined.lined.for.plot$Year)


png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA1A_lined.vs.unlined",surveyyear,".png"),width=11,height=8,units = "in",res=920)

ggplot(data = unlined.lined.for.plot, aes(x=Year, y=Value, col=Gear, pch=Gear)) + 
  geom_point() + 
  geom_line(aes(linetype = Gear)) + 
  theme_bw() + ylab("Survey mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.05, 0.9)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual( values=c('black','red'))

dev.off() 


### END OF SCRIPT ### 
