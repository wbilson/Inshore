###............................###
###        Clappers/tow        ###
###    SPA 6  Assessment       ###
###    Rewritten July 2021     ###
###    J.Sameoto               ###
###                            ###
###............................###

#NOTES:
#2005 data in BF cruise data
#6A - SPR design started in 6A officially in 2007 (CSAS Res Doc: 2008/002)
# Tows from 2006 SPA 6B SPR design were reassigned to 'new' strata areas including 6A during the 2008 SCALLSUR restratification and cleanup. Some are now in 6A but are not used for the mean estimates.
#........................................###

options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(PBSmapping)
library(spr) #version 1.04
library(lubridate)

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
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year 
assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "6"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6"
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"


# polygons to for assigning new strata to data
inVMS <- read.csv("Y:/Inshore/Assessment/BoF/2015/SPA6/Survey/SPA6_VMS_IN_R_final_MOD.csv")
#inVMS <- read.csv("Y:/Inshore/BoF/2015/SPA6/Survey/SPA6_VMS_IN_R_final_MOD.csv")
outvms <- read.csv("Y:/Inshore/Assessment/BoF/2015/SPA6/Survey/SPA6_VMS_OUT_R_final_MOD.csv")
#outvms <- read.csv("Y:/Inshore/BoF/2015/SPA6/Survey/SPA6_VMS_OUT_R_final_MOD.csv")

#SHF data
#SPA 6C(30), 6B(31), 6A(32)
# NOTE: 2005 data in BF cruise data;

#Set SQL query: 
quer2 <- ("SELECT * FROM scallsur.scdeadres s	 LEFT JOIN	(SELECT tow_date, cruise, tow_no FROM SCALLSUR.sctows) t on (s.cruise = t.cruise and s.tow_no = t.tow_no) where strata_id in (30, 31, 32)" )
	
# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

# Select data from database; execute query with ROracle; numbers by shell height bin
deadfreq <- dbGetQuery(chan, quer2)

#deadfreq <- sqlQuery(RODBCconn, quer2, believeNRows=FALSE)
deadfreq <- deadfreq[,1:51]
deadfreq$YEAR <- year(deadfreq$TOW_DATE)

deadfreq$lat <- convert.dd.dddd(deadfreq$START_LAT)
deadfreq$lon <- convert.dd.dddd(deadfreq$START_LONG)
deadfreq$ID <- 1:nrow(deadfreq)

#identify tows "inside" the VMS strata and call them "IN"
deadfreq$VMSSTRATA <- ""
events <- subset(deadfreq,STRATA_ID%in%30:32,c("ID","lon","lat"))
names(events) <- c("EID","X","Y")
deadfreq$VMSSTRATA[deadfreq$ID%in%findPolys(events,inVMS)$EID]<-"IN"
deadfreq$VMSSTRATA[deadfreq$ID%in%findPolys(events,outvms)$EID]<-"OUT"

deadfreq <- deadfreq[deadfreq$TOW_TYPE_ID==1,] #limit tows to tow type 1

###
# ---- correct data-sets for errors ----
###
#some repearted tows will not be assigned to the same strata as the "parent" tow due to where the START LAT and LONG are
#repeated tows should be corrected to match parent tow
#some experimental tows were used as parent tows for repeats, these should be removed

## deadfreq errors 
#1. Tow 32 in 2011 (73 in 2010) not assinged to IN
#   Tow 70 in 2011 (90 in 2010) not assinged to IN
deadfreq$VMSSTRATA[deadfreq$TOW_NO==32 & deadfreq$CRUISE == "GM2011"] <- "IN"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==70 & deadfreq$CRUISE == "GM2011"] <-"IN"

#2. In 2012 tow 9, 27, 111 not assinged to IN
deadfreq$VMSSTRATA[deadfreq$TOW_NO==9 & deadfreq$CRUISE == "GM2012"] <- "IN"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==27 & deadfreq$CRUISE == "GM2012"] <-"IN"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==111 & deadfreq$CRUISE == "GM2012"] <-"IN"

#3. In 2013 tow 9, 27, 111 not assinged to IN
deadfreq$VMSSTRATA[deadfreq$TOW_NO==55 & deadfreq$CRUISE == "GM2013"] <- "IN"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==109 & deadfreq$CRUISE == "GM2013"] <-"IN"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==116 & deadfreq$CRUISE == "GM2013"] <-"IN"

#4. In 2014 tow 9, 27, 111 not assinged to IN
deadfreq$VMSSTRATA[deadfreq$TOW_NO==31 & deadfreq$CRUISE == "GM2014"] <- "IN"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==42 & deadfreq$CRUISE == "GM2014"] <-"OUT"

#5. In 2015 tow 20 not assigned to IN ( 2015 tow 20 links to 2014 tow 31 which links to a 2013 tow )
deadfreq$VMSSTRATA[deadfreq$TOW_NO==20 & deadfreq$CRUISE == "GM2015"] <-"IN"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==25 & deadfreq$CRUISE == "GM2015"] <-"OUT"

#6. In 2016 tow 15,75,92 not assinged to OUT
deadfreq$VMSSTRATA[deadfreq$TOW_NO==15 & deadfreq$CRUISE == "GM2016"] <-"OUT"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==75 & deadfreq$CRUISE == "GM2016"] <-"OUT"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==92 & deadfreq$CRUISE == "GM2016"] <-"OUT"

#7. In 2017 tow 15 (is a repeat of GM2016 tow 15) assigned to OUT
deadfreq$VMSSTRATA[deadfreq$TOW_NO==56 & deadfreq$CRUISE == "GM2017"] <-"OUT"
#8 In 2017, tow 95 needs to be IN
deadfreq$VMSSTRATA[deadfreq$TOW_NO==95 & deadfreq$CRUISE == "GM2017"] <- "IN"

#Clean up VMSSTRATA for GM2017
deadfreq$VMSSTRATA[deadfreq$TOW_NO==86 & deadfreq$CRUISE == "GM2017"] <- "OUT"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==23 & deadfreq$CRUISE == "GM2017"] <- "OUT"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==104 & deadfreq$CRUISE == "GM2017"] <- "OUT"
deadfreq$VMSSTRATA[deadfreq$TOW_NO==120 & deadfreq$CRUISE == "GM2017"] <- "OUT"

#8. In 2018 tow 40 (is a repeat of GM2017 tow 56) assigned to IN but needs to be OUT - a repeated repeat
deadfreq$VMSSTRATA[deadfreq$TOW_NO==40 & deadfreq$CRUISE == "GM2018"] <-"OUT"

#9. In 2019 tow 95 assogmed to IN but needs to be OUT 
deadfreq$VMSSTRATA[deadfreq$TOW_NO==95 & deadfreq$CRUISE == "GM2019"] <-"OUT"


#Check 
table(deadfreq$VMSSTRATA)


### ---- Survey index - number ----

###  IN  ###

STRATA.ID <- "IN"

###... commercial size (>=80mm) ...###
#simple means
years <- 1997:surveyyear
X <- length(years)
SPA6.Comm.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Comm.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Comm.simple.dead [i,2] <- mean(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
	SPA6.Comm.simple.dead [i,3] <- var(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
}
SPA6.Comm.simple.dead$Age <- "Commercial"
SPA6.Comm.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Comm.simple.dead.IN <- SPA6.Comm.simple.dead

#no survey in 2020 need to interpolate 
approx(SPA6.Comm.simple.dead.IN$Year, SPA6.Comm.simple.dead.IN$Mean.nums, xout=2020) # 11.34498
SPA6.Comm.simple.dead.IN[SPA6.Comm.simple.dead.IN$Year==2020,c("Mean.nums","var.y")] <- c(11.34498,  407.11188) #assume var from 2019
SPA6.Comm.simple.dead.IN

###... Recruit size (>=65mm&<80mm) ...###
# bins 65,70,75; Recruits (65-79mm)
#simple means
years <- 1997:surveyyear
X <- length(years)
SPA6.Recruit.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Recruit.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Recruit.simple.dead[i,2] <- mean(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
	SPA6.Recruit.simple.dead[i,3] <- var(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
}
SPA6.Recruit.simple.dead$Age <- "Recruit"
SPA6.Recruit.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Recruit.simple.dead.IN <- SPA6.Recruit.simple.dead

#no survey in 2020 need to interpolate 
approx(SPA6.Recruit.simple.dead.IN$Year, SPA6.Recruit.simple.dead.IN$Mean.nums, xout=2020) # 0.08301887
SPA6.Recruit.simple.dead.IN[SPA6.Recruit.simple.dead.IN$Year==2020,c("Mean.nums","var.y")] <- c(0.08301887, 0.7165167) #assume var from 2019 but it's zero and given approximated 2020 value is not zero we need a numbere here - so we'll go with var from 2021 
SPA6.Recruit.simple.dead.IN


### OUT  ###

STRATA.ID <- "OUT"

###... commercial size (>=80mm) ...###
#simple means
years <- 1997:surveyyear
X <- length(years)
SPA6.Comm.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Comm.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Comm.simple.dead [i,2] <- mean(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
	SPA6.Comm.simple.dead [i,3] <- var(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,27:50],1,sum))
}
SPA6.Comm.simple.dead$Age <- "Commercial"
SPA6.Comm.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Comm.simple.dead.OUT <- SPA6.Comm.simple.dead

#no survey in 2020 need to interpolate 
approx(SPA6.Comm.simple.dead.OUT$Year, SPA6.Comm.simple.dead.OUT$Mean.nums, xout=2020) # 15.50786
SPA6.Comm.simple.dead.OUT[SPA6.Comm.simple.dead.OUT$Year==2020,c("Mean.nums","var.y")] <- c(15.50786,  948.399194 ) #assume var from 2019
SPA6.Comm.simple.dead.OUT

###... Recruit size (>=65mm&<80mm) ...###
# bins 65,70,75; Recruits (65-79mm)
#simple means
years <- 1997:surveyyear
X <- length(years)
SPA6.Recruit.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Recruit.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Recruit.simple.dead[i,2] <- mean(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
	SPA6.Recruit.simple.dead[i,3] <- var(apply(temp.data[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1 ,24:26],1,sum))
}
SPA6.Recruit.simple.dead$Age <- "Recruit"
SPA6.Recruit.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Recruit.simple.dead.OUT <- SPA6.Recruit.simple.dead

#no survey in 2020 need to interpolate 
approx(SPA6.Recruit.simple.dead.OUT$Year, SPA6.Recruit.simple.dead.OUT$Mean.nums, xout=2020) # 0.06875
SPA6.Recruit.simple.dead.OUT[SPA6.Recruit.simple.dead.OUT$Year==2020,c("Mean.nums","var.y")] <- c(0.06875,  0.6050000 ) #assume var from 2019
SPA6.Recruit.simple.dead.OUT

#combine all df of clappers
#NOTE: in Inside VMS area of SPA 6, area is 623.94 sq km, standard tow is 0.0042672 sq km; therefore towable units = 146217.6603

SPA6Clappers <- rbind(SPA6.Comm.simple.dead.IN, SPA6.Recruit.simple.dead.IN, SPA6.Comm.simple.dead.OUT , SPA6.Recruit.simple.dead.OUT )
SPA6Clappers$Pop <-  SPA6Clappers$Mean.nums*146217.6603
  
write.csv(SPA6Clappers, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA6.Index.Clappers.",surveyyear,".csv"))



# ---- Plot Clapper per tow ----
#DON"T PLoT 2020 since it's not "REAL" data since no survey in 2020 

SPA6Clappers$Size <- SPA6Clappers$Age
data <- SPA6Clappers
data$Mean.nums[data$Year==2020] <- NA #since don't want 2020 to plot in figures 

SPA6.ClapNumbers.per.tow.plot <- ggplot(data = data, aes(x=Year, y=Mean.nums, col=Size, pch = Size)) + facet_wrap(~VMSSTRATA, ncol = 1) + 
  geom_point() + geom_line(aes(linetype = Size)) + theme_bw() + 
  ylab("Clapper mean no./tow") + xlab("Year") + 
  theme(legend.position = c(0.06, 0.92)) + 
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_color_manual(values=c('black','red'))
SPA6.ClapNumbers.per.tow.plot

png(paste0(path.directory,assessmentyear,"/Assessment/Figures/SPA",area,"_clapper_Numberpertow",surveyyear,".png"),width=11,height=8,units = "in",res=920)
SPA6.ClapNumbers.per.tow.plot
dev.off() 


###
# ---- Shell height Frequency means by year  ----
##

# IN VMS clapper SHF #
GMdeadfreq.IN <- subset(deadfreq, c(VMSSTRATA=="IN" & TOW_TYPE_ID==1)) #include tow type IDs 1 only since tow type 5 has different prob of selection!
SPA6.SHFmeans <- sapply(split(GMdeadfreq.IN[c(11:50)], GMdeadfreq.IN$YEAR), function(x){apply(x,2,mean)})
SPA6.SHFmeans <- as.data.frame(SPA6.SHFmeans)
SPA6.SHFmeans$"2020" <- NA 

SPA6.SHFdead.for.plot <- data.frame(bin.label = row.names(SPA6.SHFmeans), SPA6.SHFmeans)
SPA6.SHFdead.for.plot$bin.mid.pt <- seq(2.5,200,by=5)
head(SPA6.SHFdead.for.plot)

SPA6.SHFdead.for.plot <- pivot_longer(SPA6.SHFdead.for.plot, 
                                            cols = starts_with("X"),
                                            names_to = "year",
                                            names_prefix = "X",
                                            values_to = "SH",
                                            values_drop_na = FALSE)
SPA6.SHFdead.for.plot$year <- as.numeric(SPA6.SHFdead.for.plot$year)

SPA6.SHFdead.for.plot <- SPA6.SHFdead.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA6.SHFdead.for.plot$SH <- round(SPA6.SHFdead.for.plot$SH,3)

ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA6.SHFdead.in <- ggplot() + geom_col(data = SPA6.SHFdead.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA6.SHFdead.in

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA6_clapper_SHF_InnerVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA6.SHFdead.in
dev.off()


# OUT VMS clapper SHF #
GMdeadfreq.OUT <- subset(deadfreq, c(VMSSTRATA=="OUT" & TOW_TYPE_ID==1)) #include tow type IDs 1 only since tow type 5 has different prob of selection!
SPA6.SHFmeans.out <- sapply(split(GMdeadfreq.OUT[c(11:50)], GMdeadfreq.OUT$YEAR), function(x){apply(x,2,mean)})
SPA6.SHFmeans.out <- as.data.frame(SPA6.SHFmeans.out)
SPA6.SHFmeans.out$"2020" <- NA 

SPA6.SHFdead.out.for.plot <- data.frame(bin.label = row.names(SPA6.SHFmeans.out), SPA6.SHFmeans.out)
SPA6.SHFdead.out.for.plot$bin.mid.pt <- seq(2.5,200,by=5)
head(SPA6.SHFdead.out.for.plot)

SPA6.SHFdead.out.for.plot <- pivot_longer(SPA6.SHFdead.out.for.plot, 
                                      cols = starts_with("X"),
                                      names_to = "year",
                                      names_prefix = "X",
                                      values_to = "SH",
                                      values_drop_na = FALSE)
SPA6.SHFdead.out.for.plot$year <- as.numeric(SPA6.SHFdead.out.for.plot$year)

SPA6.SHFdead.out.for.plot <- SPA6.SHFdead.out.for.plot %>% filter(year > surveyyear-7)
#shorten SH data for plot or else get warning when run ggplot 
SPA6.SHFdead.out.for.plot$SH <- round(SPA6.SHFdead.out.for.plot$SH,3)

ylimits <- c(0,10)
xlimits <- c(0,200)
recruitlimits <- c(65,80)

# plot SHF 
plot.SPA6.SHFdead.out <- ggplot() + geom_col(data = SPA6.SHFdead.out.for.plot, aes(x = bin.mid.pt, y = SH)) + 
  facet_wrap(~year, ncol = 1) + 
  theme_bw() + ylim(ylimits) + xlim(xlimits) + ylab("Survey mean no./tow") + xlab("Shell Height (mm)") + 
  geom_vline(xintercept = recruitlimits, linetype = "dotted") + scale_x_continuous(breaks = seq(0,max(xlimits),20))
plot.SPA6.SHFdead.out

# Save out plot
png(paste0(path.directory,assessmentyear, "/Assessment/Figures/SPA6_clapper_SHF_OuterVMS.png"), type="cairo", width=18, height=24, units = "cm", res=400)
plot.SPA6.SHFdead.out
dev.off()


#### ---- End of script in a typical year ---- 
 

 
#### THE BELOW CODE HAS NOT BEEN UPDATED SINCE 2019 ###  

#If run below code -- be aware some of the above run objects will be overwritten which is why you should dave out your RData now -- and just use the below code to make extra plots if needed 

## Plot Proportion of Clappers
# Scale clapper to survey index of live animals
###.............................###
#### Survey index - Proportion  ###
###.............................###
#Note get GMPropClappersbyTowYYYY.csv from previouly running SpatialPlots_SPA6_YYYY.r script
#remember GMPropClappersbyTowYYYY.csv if for ALL tows where above - deadfreq was subset to just type 1 tows (~line 66)
	deadfreq.prop <- read.csv('Y:/INSHORE SCALLOP/BoF/dataoutput/GMPropClappersbyTow2019.csv')
	head(deadfreq.prop)
	deadfreq.prop <- deadfreq.prop[,-1]
	dim(deadfreq.prop)
	
	deadfreq.attr <- read.csv('Y:/INSHORE SCALLOP/BoF/dataoutput/GMdeadfreq.all.strataID.csv', stringsAsFactors=FALSE) #can use deadfreq bc just matching on cruise and tow for VMSAREA and SPA area
	dim(deadfreq.attr) #ONLY TOW TYPES 1 AND 5
	head(deadfreq.attr)
	deadfreq.attr <- deadfreq.attr[,c("CRUISE","TOW_NO","VMSAREA","TOW_TYPE_ID")]
	deadfreq.prop.strata <- merge(deadfreq.prop, deadfreq.attr, by.x=c("CRUISE","tow"),by.y=c("CRUISE","TOW_NO"))
	deadfreq.prop.strata <- deadfreq.prop.strata[deadfreq.prop.strata$TOW_TYPE_ID==1,] #limit tows to tow type 1
	names(deadfreq.prop.strata)[4] <- "YEAR"
	str(deadfreq.prop.strata)
	table(deadfreq.prop.strata$VMSAREA)
	table(deadfreq.prop.strata$TOW_TYPE_ID)
	
	
#NOTE the below code overwrites objects created above! Use with caution!	
	deadfreq	<- deadfreq.prop.strata	
	names(deadfreq)[9]  <- "VMSSTRATA"

	
###......###
###  IN  ###
###......###
STRATA.ID <- "IN"

###... commercial size (>=80mm) ...###
#simple means
years <- 1997:maxyear
X <- length(years)
SPA6.Comm.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Comm.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Comm.simple.dead [i,2] <- mean(temp.data$prop.dead.com[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
	SPA6.Comm.simple.dead [i,3] <- var(temp.data$prop.dead.com[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
}
SPA6.Comm.simple.dead$Age <- "Commercial"
SPA6.Comm.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Comm.simple.dead.IN <- SPA6.Comm.simple.dead


###... Recruit size (>=65mm&<80mm) ...###
# bins 65,70,75; Recruits (65-79mm)
#simple means
years <- 1997:maxyear
X <- length(years)
SPA6.Recruit.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Recruit.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Recruit.simple.dead[i,2] <- mean(temp.data$prop.dead.rec[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
	SPA6.Recruit.simple.dead[i,3] <- var(temp.data$prop.dead.rec[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
}
SPA6.Recruit.simple.dead$Age <- "Recruit"
SPA6.Recruit.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Recruit.simple.dead.IN <- SPA6.Recruit.simple.dead


###......###
### OUT  ###
###......###
STRATA.ID <- "OUT"

###... commercial size (>=80mm) ...###
#simple means
years <- 1997:maxyear
X <- length(years)
SPA6.Comm.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Comm.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Comm.simple.dead [i,2] <- mean(temp.data$prop.dead.com[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
	SPA6.Comm.simple.dead [i,3] <- var(temp.data$prop.dead.com[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
}
SPA6.Comm.simple.dead$Age <- "Commercial"
SPA6.Comm.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Comm.simple.dead.OUT <- SPA6.Comm.simple.dead


###... Recruit size (>=65mm&<80mm) ...###
# bins 65,70,75; Recruits (65-79mm)
#simple means
years <- 1997:maxyear
X <- length(years)
SPA6.Recruit.simple.dead <- data.frame(Year=years, Mean.nums=rep(NA,X), var.y=rep(NA,X), method=rep("simple",X))
for(i in 1:length(SPA6.Recruit.simple.dead$Year)){
	temp.data <- deadfreq[deadfreq$YEAR==1996+i,]
	SPA6.Recruit.simple.dead[i,2] <- mean(temp.data$prop.dead.rec[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
	SPA6.Recruit.simple.dead[i,3] <- var(temp.data$prop.dead.rec[temp.data$VMSSTRATA==STRATA.ID & temp.data$TOW_TYPE_ID==1])
}
SPA6.Recruit.simple.dead$Age <- "Recruit"
SPA6.Recruit.simple.dead$VMSSTRATA <- STRATA.ID
SPA6.Recruit.simple.dead.OUT <- SPA6.Recruit.simple.dead

#combine all df of clappers
	SPA6Clappers.Prop <- rbind(SPA6.Comm.simple.dead.IN, SPA6.Recruit.simple.dead.IN, SPA6.Comm.simple.dead.OUT , SPA6.Recruit.simple.dead.OUT )
	write.csv(SPA6Clappers.Prop, paste0('Y:/INSHORE SCALLOP/BoF/dataoutput/SPA6ClappersProportion1997to',maxyear,'.csv'))



# Plot Clapper per tow
#Data to plot:
clapper.comm.IN <- SPA6Clappers.Prop[SPA6Clappers.Prop$VMSSTRATA=='IN'&SPA6Clappers.Prop$Age=="Commercial",]
clapper.rec.IN <- SPA6Clappers.Prop[SPA6Clappers.Prop$VMSSTRATA=='IN'&SPA6Clappers.Prop$Age=="Recruit",]
clapper.comm.OUT <- SPA6Clappers.Prop[SPA6Clappers.Prop$VMSSTRATA=='OUT'&SPA6Clappers.Prop$Age=="Commercial",]
clapper.rec.OUT <- SPA6Clappers.Prop[SPA6Clappers.Prop$VMSSTRATA=='OUT'&SPA6Clappers.Prop$Age=="Recruit",]

#Survey number per tow and weight per tow for Commercial and Recuit sizes
	text <- c("Commercial size", "Recruits")
	x <- c(1997,maxyear) #update to most recent year
	y1 <- c(0,0.4)
	y2 <- c(0,0.4)
	#panel IN VMSAREA
	par(mfrow=c(2,1), mar = c(0,4,1,1), omi =  c(0.75, 0.75, 0.1, 0.1))
	plot (x,y1, type="n",xlab="",xaxt="n", ylab= "Clapper Index (prop.)")
	lines(clapper.comm.IN[,c("Year","Mean.nums")], type="b", pch=1, lty=1)
	lines(clapper.rec.IN[,c("Year","Mean.nums")], type="b", pch=3, lty=3)
	legend (1997,0.35, legend=text, pch=c(1,3), bty="n", lty=c(1,3))
	legend(1997,0.45, legend="Inside VMS Strata", bty="n")
	#panel OUT VMSAREA
	plot (x,y2, type="n",xlab="Year", ylab= "Clapper Index (prop.)")
	lines(clapper.comm.OUT[,c("Year","Mean.nums")], type="b", pch=1, lty=1)
	lines(clapper.rec.OUT[,c("Year","Mean.nums")], type="b", pch=3, lty=3)
	legend (1997,0.35, legend=text, pch=c(1,3), bty="n", lty=c(1,3))
	legend(1997,0.45, legend="Outside VMS Strata", bty="n")





