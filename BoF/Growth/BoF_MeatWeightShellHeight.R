###........................................###
###
###    Meat weight/shell height modelling
###    BoF: SPA1A,1B,4, and 5
###    L.Nasmith September 2016
###    Modified Sept 2017 by J.SAmeoto
###
###........................................###
#modified in July 2020 J.Sameoto 

#FOR FUTURE - Add STRATA ID on output file  in Condition for Spatial Map section-- to make selection of data easier and more robust for Spatial plot script 


options(stringsAsFactors = FALSE)

#required packages
library(tidyverse)
library(ROracle)
library (lme4)
library(lattice)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year for which you want to include  - not should match year of cruise below 
cruise <- "BF2026"  #note should match year for surveyyear set above 

assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "1A1B4and5"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/" #New directory on Sky
#path.directory <- "Y:/Inshore/BoF/"

###
# read in shell height and meat weight data from database
###
#strata.spa1a<-c(6,7,12:20,39)
#strata.spa1b<-c(35,37,38,49,51:53)
#strata.spa4<-c(1:5, 8:10)
#strata.spa5==21

#SQL query 1: detailed meat weight/shell height sampling data
quer1 <- paste0("SELECT * FROM scallsur.scwgthgt WHERE cruise = '", cruise, "'")


#numbers by shell height bin
quer2 <- paste0("SELECT * FROM scallsur.scliveres WHERE cruise = '", cruise, "'")
              

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')


# Select data from database; execute query with ROracle; numbers by shell height bin
BFdetail.dat <- dbGetQuery(chan, quer1)

BFlivefreq.dat <- dbGetQuery(chan, quer2)


#add YEAR column to data
BFdetail.dat$YEAR <- as.numeric(substr(BFdetail.dat$CRUISE,3,6))
BFlivefreq.dat$YEAR <- as.numeric(substr(BFlivefreq.dat$CRUISE,3,6))

###
# read in depth information and add it to the meat weight/shell height dataframe
###
#there is no strata_id in Olex file to select on, need a unique identifier for tows to link to depth ($ID)

BFdetail.dat$ID <- paste(BFdetail.dat$CRUISE,BFdetail.dat$TOW_NO,sep='.')
uniqueID <- unique(BFdetail.dat$ID)

OlexTows_all <- read.csv("Y:/Inshore/Assessment/StandardDepth/towsdd_StdDepth.csv") #New directory on Sky
#OlexTows_all <- read.csv("Y:/Inshore/StandardDepth/towsdd_StdDepth.csv")
names(OlexTows_all)[which(colnames(OlexTows_all)=="RASTERVALU")] <- "OLEXDEPTH_M"   #rename "RASTERVALU" column
OlexTows_all$OLEXDEPTH_M[OlexTows_all$OLEXDEPTH_M==-9999] <- NA
OlexTows_all$ID <- paste(OlexTows_all$CRUISE,OlexTows_all$TOW_NO,sep='.')
OlexTows_bof <- subset(OlexTows_all, ID%in%uniqueID)

BFdetail <- merge(BFdetail.dat,subset(OlexTows_bof,select=c("ID","OLEXDEPTH_M")), all=T)
BFdetail$ADJ_DEPTH <- BFdetail$OLEXDEPTH_M
BFdetail$ADJ_DEPTH[is.na(BFdetail$OLEXDEPTH_M)] <- -1*BFdetail$DEPTH[is.na(BFdetail$OLEXDEPTH_M)] #*-1 because DEPTH is positive

#Check if problem with merge: dim(BFdetail.dat)[1] and dim(BFdetail)[1] should be the same -- TRUE: 
dim(BFdetail.dat)[1] == dim(BFdetail)[1]


#add depth to the livefreq dataframe
BFlivefreq.dat$ID <- paste(BFlivefreq.dat$CRUISE,BFlivefreq.dat$TOW_NO,sep='.')
uniqueIDb <- unique(BFlivefreq.dat$ID)

OlexTows_freq <- subset(OlexTows_all,ID%in%uniqueIDb)
BFlivefreq <- merge(BFlivefreq.dat,subset(OlexTows_freq,select=c("ID","OLEXDEPTH_M")), all=T)
BFlivefreq$ADJ_DEPTH <- BFlivefreq$OLEXDEPTH_M
BFlivefreq$ADJ_DEPTH[is.na(BFlivefreq$OLEXDEPTH_M)] <- -1*BFlivefreq$DEPTH[is.na(BFlivefreq$OLEXDEPTH_M)] #*-1 because DEPTH is positive
summary(BFlivefreq)


###
# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
###
summary(BFdetail)
BFdetail <- BFdetail[complete.cases(BFdetail[,c(which(names(BFdetail)=="WET_MEAT_WGT"))]),]  #remove NAs from WET_MEAT_WEIGHT
BFdetail <- BFdetail[complete.cases(BFdetail[,c(which(names(BFdetail)=="HEIGHT"))]),]  #remove NAs from HEIGHT


#---- Meat weight shell height modelling ----

#Subset for year
BFdetail.foryear <- subset(BFdetail, YEAR==surveyyear) #!!!defined via objects in Define section

#create dataset for model
test.data <- subset(BFdetail.foryear, HEIGHT>40)
test.data$Log.HEIGHT <- log(test.data$HEIGHT)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT - mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)

#plot depths by tow
plot (ADJ_DEPTH~TOW_NO, data=test.data)
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/BFtowdepth",surveyyear,".png")) 
plot (ADJ_DEPTH~TOW_NO, data=test.data)
dev.off()

#run model
MWTSHBF.YYYY <- glmer(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|TOW_NO),data=test.data,     
                        family=Gamma(link=log), na.action = na.omit)

summary(MWTSHBF.YYYY)  

#Save summary to txt file
print(summary(MWTSHBF.YYYY))
sink(paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/MWTSHBF_ModelSummary.txt"))
print(summary(MWTSHBF.YYYY))
sink()

#diagnostics

latt <- data.frame(test.data, res=residuals(MWTSHBF.YYYY,"pearson"),fit=fitted(MWTSHBF.YYYY)) 

#Residuals vs fitted - full area 
plot(MWTSHBF.YYYY)

png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHBF",surveyyear,".png"))
plot(MWTSHBF.YYYY) 
dev.off()

#Plot of tow level residuals
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="residuals",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHBF",surveyyear,"_towresid.png"))
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="residuals",
  panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
dev.off()

#Plot of tow level fitted values 
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="Wet Meat weight")
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHBF",surveyyear,"_towfit.png"))
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="Wet Meat weight")
dev.off()

#Construct data.frame similar to BFlivenfreq for weight per tow
livefreqYYYY <- subset(BFlivefreq, YEAR==surveyyear)
liveweightYYYY <- livefreqYYYY

#create matrix of depths by tow to use in predict function
log.ctr.adj_depth <- log(abs(livefreqYYYY$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
Log.height.ctr <- log(seq(2.5, 197.5, by = 5)) - mean(test.data$Log.HEIGHT) #each shell height bin to predict on

temp <- matrix(NA,dim(liveweightYYYY)[1],40) 

#Use random effects for tows in detail sample and fixed effects otherwise
#Random effects for tows that were sampled for meat weight shell height; here ID tows that were sampled  
random.pred <- (1:dim(liveweightYYYY)[1])[is.element(liveweightYYYY$TOW_NO,unique(test.data$TOW_NO))]

#fixed effects for tows that weren't sampled for meat weight shell height; here ID tows that were NOT sampled   
fixed.pred <- (1:dim(liveweightYYYY)[1])[!is.element(liveweightYYYY$TOW_NO,unique(test.data$TOW_NO))]

#Predict using Random effects for tows that were sampled for meat weight shell height
for(i in random.pred) temp[i,] <- as.vector(predict(MWTSHBF.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i] ,40),TOW_NO=liveweightYYYY$TOW_NO[i]),re.form=NULL,type="response"))

#Predict using fixed effects for tows that weren't sampled for meat weight shell height
for(i in fixed.pred) temp[i,] <- as.vector(predict(MWTSHBF.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i] ,40)),re.form=~0,type="response"))

#multply temp matrix (weight) by live numbers to get weight/size bin
liveweightYYYY[,grep("BIN_ID_0", colnames(liveweightYYYY)):grep("BIN_ID_195", colnames(liveweightYYYY))] <- temp*livefreqYYYY[,grep("BIN_ID_0", colnames(livefreqYYYY)):grep("BIN_ID_195", colnames(livefreqYYYY))]



#export file for later analysis
write.csv(liveweightYYYY, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/BFliveweight",surveyyear,".csv")) 

#NEW - save only the objects we need later
save(MWTSHBF.YYYY, latt, liveweightYYYY, BFdetail.foryear, BFlivefreq, livefreqYYYY,
     file=paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/BFgrowth",surveyyear,".RData"))

# ----  Condition for Spatial Map (single map covers spatial area of SPA 1A, 1B, 4) ----
# Only calculates the point condition for the single year you modelled above (i.e. surveyyear that's set above) 

livefreq.condition.spatial <- livefreqYYYY
#adjust depth, must center on same mean as used in original model
livefreq.condition.spatial$log.ctr.adj_depth <- log(abs(livefreq.condition.spatial$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.condition.spatial <- livefreq.condition.spatial[is.element(livefreq.condition.spatial$TOW_NO, unique(test.data$TOW_NO)),c("TOW_NO","START_LAT", "START_LONG", "ADJ_DEPTH","log.ctr.adj_depth")]
livefreq.condition.spatial$YEAR <- surveyyear #add year to dataframe


#Predict a meat weight for 100mm for just sampled tows; #re.form=NULL (all random effects included since only predicting on those tows that were sampled)

livefreq.condition.spatial$Condition <- predict(MWTSHBF.YYYY,newdata=data.frame(Log.HEIGHT.CTR=rep(log(100), dim(livefreq.condition.spatial)[1])-mean(test.data$Log.HEIGHT),
                                                                                Log.DEPTH.CTR=livefreq.condition.spatial$log.ctr.adj_depth, 
                                                                                TOW_NO=livefreq.condition.spatial$TOW_NO),
                                                re.form=NULL, type="response")

head(livefreq.condition.spatial)

#export for spatial plot later 
write.csv(livefreq.condition.spatial, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/BFConditionforMap",surveyyear,".csv"))


# ----  Condition Time Series Figures (SPA 1A, 1B, 4) ----
#DEPTHS by area should be:
# CS = -76.64 #PLOT FOR 1B ASSESSMENT
# MBN = -45.42 #PLOT FOR 1B ASSESSMENT
# 28C = -33.56 #PLOT FOR 1B ASSESSMENT
# Outer = -42.18
# AH = -25.66
# SI = -23.49
# SB = -17.9
# 28Dcombined = -35.95 #PLOT FOR 1B ASSESSMENT
# SPA 4 = -82.57
# MBS = -59.11  #PLOT FOR 1A ASSESSMENT "MID BAY SOUTH"
# zone8to16 (Strata 12 to 20 in 1A)= -92.4 #PLOT FOR 1A ASSESSMENT label "8 to 16 mile"
# zone2to8 (Strata 6 and 7 in 1A) = -67.78 #PLOT FOR 1A ASSESSMENT label "2 to 8 mile" 
# SPA1A = -74.82
# SPA1B = -50.09

#Bring in file with depths by area, note some are by strata groups within area
mean.depth <- read.csv('Y:/Inshore/Assessment/StandardDepth/BoFMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area, New directory on Sky
#mean.depth <- read.csv('Y:/Inshore/StandardDepth/BoFMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area
unique(mean.depth$AREA)
length(mean.depth$AREA)

mean.depth.bof <- mean.depth[mean.depth$AREA %in% c("CapeSpencer",
                                     "MidBayNorth",
                                     "UpperBay28C",
                                     "Outer28D",
                                     "AdvocateHarbour",
                                     "SpencersIsland",
                                     "ScotsBay",
                                     "Combined28D",
                                     "SPA4",
                                     "MidBaySouth",
                                     "Zone8to16mile",
                                     "Zone2to8mile",
                                     "SPA1A",
                                     "SPA1B"),]
dim(mean.depth.bof)[1] == 14
unique(mean.depth.bof$AREA)

mean.depth.bof$YEAR <- surveyyear
mean.depth.bof$Condition <- NA

for (i in 1:length(mean.depth.bof$AREA)){
mean.depth.bof$Condition[i] <- predict(MWTSHBF.YYYY, newdata = data.frame(Log.HEIGHT.CTR=log(100) - mean(test.data$Log.HEIGHT),
                         Log.DEPTH.CTR = log(abs(mean.depth.bof$MeanDepth_m[i])) - mean(test.data$Log.DEPTH)),
                         re.form = NA, type = "response")  
}
mean.depth.bof

#Import previous year condition file: 
BF.con.ts <- read.csv(paste0(path.directory, assessmentyear-1,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BoF_ConditionTimeSeries.csv"))
unique(BF.con.ts$STRATA)
#Add NAs for 2020
#BF.con.ts <- BF.con.ts %>% add_row(YEAR = 2020, STRATA = "AdvocateHarbour", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "CapeSpencer", CONDITION = NA) %>%
#  add_row(YEAR = 2020, STRATA = "Combined28D", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "MidBayNorth", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "MidBaySouth", CONDITION = NA) %>%
#  add_row(YEAR = 2020, STRATA = "Outer28D", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "ScotsBay", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "SPA1A", CONDITION = NA) %>%
#  add_row(YEAR = 2020, STRATA = "SPA1B", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "SPA4", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "SpencersIsland", CONDITION = NA) %>%
#  add_row(YEAR = 2020, STRATA = "UpperBay28C", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "Zone2to8mile", CONDITION = NA) %>%
#  add_row(YEAR = 2020, STRATA = "Zone8to16mile", CONDITION = NA)
#BF.con.ts <- BF.con.ts[BF.con.ts$YEAR!=2019,]

#update timeseries and write out new file: 
BF.con.ts <- rbind(BF.con.ts %>% dplyr::select(YEAR, STRATA, CONDITION), mean.depth.bof %>% dplyr::select("YEAR",STRATA="AREA",CONDITION="Condition"))

BF.con.ts <- BF.con.ts[order(BF.con.ts$STRATA, BF.con.ts$YEAR),]
BF.con.ts

write.csv(BF.con.ts, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/BoF_ConditionTimeSeries.csv"), row.names=FALSE)



#... Plot Condition Time Series Figures:

#name strata for display - "CapeSpencer","MidBayNorth","UpperBay28C","Outer28D","AdvocateHarbour","SpencersIsland","ScotsBay","Combined28D","SPA4","MidBaySouth","Zone8to16mile","Zone2to8mile","SPA1A","SPA1B"
BF.con.ts$strata.name <- NA 
BF.con.ts$strata.name[BF.con.ts$STRATA=="CapeSpencer"] <- "Cape Spencer"
BF.con.ts$strata.name[BF.con.ts$STRATA=="MidBayNorth"] <- "Mid Bay North"
BF.con.ts$strata.name[BF.con.ts$STRATA=="UpperBay28C"] <- "Upper Bay (28C)"
BF.con.ts$strata.name[BF.con.ts$STRATA=="Combined28D"] <- "28D (combined)"
BF.con.ts$strata.name[BF.con.ts$STRATA=="Outer28D"] <- "28D Outer"
BF.con.ts$strata.name[BF.con.ts$STRATA=="AdvocateHarbour"] <- "Advocate Harbour"
BF.con.ts$strata.name[BF.con.ts$STRATA=="SpencersIsland"] <- "Spencer's Island"
BF.con.ts$strata.name[BF.con.ts$STRATA=="ScotsBay"] <- "Scots Bay"
BF.con.ts$strata.name[BF.con.ts$STRATA=="SPA4"] <- "SPA4"
BF.con.ts$strata.name[BF.con.ts$STRATA=="MidBaySouth"] <- "Mid Bay South"
BF.con.ts$strata.name[BF.con.ts$STRATA=="Zone8to16mile"] <- "8 to 16 mile"
BF.con.ts$strata.name[BF.con.ts$STRATA=="Zone2to8mile"] <- "2 to 8 mile"
BF.con.ts$strata.name[BF.con.ts$STRATA=="SPA1A"] <- "SPA1A"
BF.con.ts$strata.name[BF.con.ts$STRATA=="SPA1B"] <- "SPA1B"

#..SPA 1A
SPA1A.condition.ts.plot <- ggplot(BF.con.ts %>% filter(STRATA %in% c("MidBaySouth","Zone2to8mile","Zone8to16mile")),
       aes(x=YEAR, y=CONDITION,group_by(strata.name), color=strata.name)) +  
  geom_line(aes(linetype=strata.name)) + geom_point( size = 3, aes(shape=strata.name)) +
  xlab("Year") + ylab("Condition (meat weight, g)") + theme_bw() +
  coord_cartesian(ylim=c(8, 25)) +
  scale_y_continuous(breaks=seq(5, 25, 5))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = c(.01, .93),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1.5))

SPA1A.condition.ts.plot

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA1A_ConditionTimeSeries.png"), plot = SPA1A.condition.ts.plot, scale = 2.5, width = 9, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
#png(paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA1A_ConditionTimeSeries.png" ), res = 200, width = 900, height = 600 )
#SPA1A.condition.ts.plot
#dev.off()


#..SPA 1B
SPA1B.condition.ts.plot <- ggplot(BF.con.ts %>% filter(STRATA %in% c("CapeSpencer","MidBayNorth","UpperBay28C","Combined28D")),
                                  aes(x=YEAR, y=CONDITION,group_by(strata.name), color=strata.name)) +  
  geom_line(aes(linetype=strata.name)) + geom_point( size = 3, aes(shape=strata.name)) +
  xlab("Year") + ylab("Condition (meat weight, g)") + theme_bw() +
  coord_cartesian(ylim=c(8, 25)) +
  scale_y_continuous(breaks=seq(5, 25, 5))+
  theme(axis.title = element_text(size =15),
        axis.text = element_text(size = 12),
        legend.position = c(.01, .93),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1.5))

SPA1B.condition.ts.plot

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA1B_ConditionTimeSeries.png"), plot = SPA1B.condition.ts.plot, scale = 2.5, width = 9, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
#png(paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA1B_ConditionTimeSeries.png" ),res = 200, width = 900, height = 600 )
#SPA1B.condition.ts.plot
#dev.off()


#.. SPA 4
SPA4.condition.ts.plot <- ggplot(BF.con.ts %>% filter(STRATA =="SPA4"),
                                 aes(x=YEAR, y=CONDITION,group_by(strata.name), color=strata.name)) +  
  geom_line(aes(linetype=strata.name)) + geom_point( size = 3, aes(shape=strata.name)) +
  xlab("Year") + ylab("Condition (meat weight, g)") + theme_bw() +
  coord_cartesian(ylim=c(8, 25)) +
  scale_y_continuous(breaks=seq(5, 25, 5))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = c(.01, .9),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1.5))

SPA4.condition.ts.plot

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA4_ConditionTimeSeries.png"), plot = SPA4.condition.ts.plot, scale = 2.5, width = 9, height = 6, dpi = 300, units = "cm", limitsize = TRUE)
#png(paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA4_ConditionTimeSeries.png" ),res = 200, width = 900, height = 600 )
#SPA4.condition.ts.plot
#dev.off()


### END OF SCRIPT ###
