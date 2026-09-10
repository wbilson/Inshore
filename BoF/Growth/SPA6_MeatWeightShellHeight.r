###........................................###
###
###    Meat weight/shell height modelling
###    SPA6: 1997 to 2015
###    J.Sameoto, modified from
###    L.Nasmith
###    August 2015
###    Mod J.SAmeoto Oct 2017
###   Mod J.SAmeoto Sept 2018
###   Mod J.SAmeoto Sept 2019
###   Mod J.Sameoto June 2020 
###........................................###
#modified in July 2020 J.Sameoto 
# Note Starata IDs for SPA 6: strata.spa6<-c(30:32)

options(stringsAsFactors=FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(lme4)
library(lattice)
library(lubridate)

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year for which you want to include  - not should match year of cruise below 
cruise <- "GM2026"  #note should match year for surveyyear set above 

assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "6"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"


# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# Set SQL query 
quer1 <- paste0("SELECT * FROM scallsur.scwgthgt WHERE cruise = '", cruise, "'")


# read in shell height and meat weight data from database
# detailed meat weight/shell height sampling data
GMdetail.dat <- dbGetQuery(chan, quer1)

#numbers by shell height bin
quer2 <- paste0("SELECT * FROM scallsur.scliveres WHERE cruise = '", cruise, "'")
GMlivefreq.dat <- dbGetQuery(chan, quer2)

#add YEAR column to data
GMdetail.dat$YEAR <- year(GMdetail.dat$TOW_DATE)
GMlivefreq.dat$YEAR <- as.numeric(substr(GMlivefreq.dat$CRUISE,3,6))

### FOR 2024 - make sure no samples from SPA 2 are in analysis!!!! 
GMdetail.dat %>% group_by(STRATA_ID ) %>% summarise(n())
#Remove  SPA 2 tows which are those in STRATA_ID == 57
dim(GMdetail.dat)
GMdetail.dat <- GMdetail.dat[GMdetail.dat$STRATA_ID != 57 ,]
dim(GMdetail.dat)

dim(GMlivefreq.dat)
GMlivefreq.dat %>% group_by(MGT_AREA_ID) %>% summarise(n())
#Remove SPA 2 tows those as MGT_AREA_ID == 2
GMlivefreq.dat <- GMlivefreq.dat[GMlivefreq.dat$MGT_AREA_ID != 2,]
dim(GMlivefreq.dat)


#check
table(GMdetail.dat$YEAR)
table(GMlivefreq.dat$YEAR)


# read in depth information and add it to the meat weight/shell height dataframe
# there is no strata_id in Olex file to select on, need a unique identifier for tows to link to depth ($ID)
GMdetail.dat$ID <- paste(GMdetail.dat$CRUISE, GMdetail.dat$TOW_NO,sep='.')
uniqueID <- unique(GMdetail.dat$ID)

OlexTows_all <- read.csv("Y:/Inshore/Assessment/StandardDepth/towsdd_StdDepth.csv")
#OlexTows_all <- read.csv("Y:/Inshore/StandardDepth/towsdd_StdDepth.csv")
names(OlexTows_all)[which(colnames(OlexTows_all)=="RASTERVALU")] <- "OLEXDEPTH_M"   #rename "RASTERVALU" column
OlexTows_all$OLEXDEPTH_M[OlexTows_all$OLEXDEPTH_M==-9999] <- NA
OlexTows_all$ID <- paste(OlexTows_all$CRUISE,OlexTows_all$TOW_NO,sep='.')
OlexTows_bof <- subset (OlexTows_all, ID%in%uniqueID)

GMdetail <- merge(GMdetail.dat,subset(OlexTows_bof,select=c("ID","OLEXDEPTH_M")), all.x=T)
GMdetail$ADJ_DEPTH <- GMdetail$OLEXDEPTH_M
GMdetail$ADJ_DEPTH[is.na(GMdetail$OLEXDEPTH_M)] <- -1*GMdetail$DEPTH[is.na(GMdetail$OLEXDEPTH_M)] #*-1 because DEPTH is positive

#add depth to the livefreq dataframe
GMlivefreq.dat$ID <- paste(GMlivefreq.dat$CRUISE,GMlivefreq.dat$TOW_NO,sep='.')
uniqueIDb <- unique(GMlivefreq.dat$ID)

OlexTows_freq <- subset(OlexTows_all,ID%in%uniqueIDb)
GMlivefreq <- merge(GMlivefreq.dat,subset(OlexTows_freq,select=c("ID","OLEXDEPTH_M")), all=T)
GMlivefreq$ADJ_DEPTH <- GMlivefreq$OLEXDEPTH_M
GMlivefreq$ADJ_DEPTH[is.na(GMlivefreq$OLEXDEPTH_M)] <- -1*GMlivefreq$DEPTH[is.na(GMlivefreq$OLEXDEPTH_M)] #*-1 because DEPTH is positive
summary(GMlivefreq)


# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
summary(GMdetail)
GMdetail <- GMdetail[complete.cases(GMdetail[,c(which(names(GMdetail)=="WET_MEAT_WGT"))]),]  #remove NAs from WET_MEAT_WEIGHT
GMdetail <- GMdetail[complete.cases(GMdetail[,c(which(names(GMdetail)=="HEIGHT"))]),]  #remove NAs from HEIGHT
summary(GMdetail)


#---- Meat weight shell height modelling ----

#Subset for year
GMdetail.foryear <- subset(GMdetail, YEAR==surveyyear)

#create dataset for model
test.data <- subset(GMdetail.foryear, HEIGHT>40)
test.data$Log.HEIGHT <- log(test.data$HEIGHT)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT - mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)

#plot depths by tow.
plot(ADJ_DEPTH~TOW_NO, data=test.data)
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/GMtowdepth",surveyyear,".png")) #!!!DEFINE
plot(ADJ_DEPTH~TOW_NO, data=test.data)
dev.off()

#run model; update model name to correspond to year
MWTSHGM.YYYY <- glmer(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|TOW_NO),data=test.data,
                        family=Gamma(link=log), na.action = na.omit)

summary(MWTSHGM.YYYY)

#Save summary to txt file
print(summary(MWTSHGM.YYYY))
sink(paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/MWTSHGM",surveyyear,"_ModelSummary.txt"))
print(summary(MWTSHGM.YYYY))
sink()

#diagnostics
latt <- data.frame(test.data, res=residuals(MWTSHGM.YYYY,"pearson"),fit=fitted(MWTSHGM.YYYY)) #update model name

#Residuals vs fitted - full area
plot(MWTSHGM.YYYY) 
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHGM",surveyyear,".png"))
plot(MWTSHGM.YYYY) 
dev.off()

#Plot of tow level residuals
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="residuals",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHGM",surveyyear,"_towresid.png"))
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="residuals",
  panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
dev.off()

#Plot of tow level fitted values 
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="Wet Meat weight")
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHGM",surveyyear,"_towfit.png"))
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="Wet Meat weight")
dev.off()

#Construct data.frame similar to GMlivenfreq for weight per tow
livefreqYYYY <- subset(GMlivefreq, YEAR==surveyyear)
liveweightYYYY <- livefreqYYYY

#create matrix of depths by tow to use in predict function
Log.height.ctr <- log(seq(2.5, 197.5, by = 5)) - mean(test.data$Log.HEIGHT) #each shell height bin to predict on
log.ctr.adj_depth <- log(abs(livefreqYYYY$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) #depth by tow

temp <- matrix(NA,dim(liveweightYYYY)[1],40)

#Use random effects for tows in detail sample and fixed effects otherwise
#Random effects for tows that were sampled for meat weight shell height; here ID tows that were sampled  
random.pred <- (1:dim(liveweightYYYY)[1])[is.element(liveweightYYYY$TOW_NO,unique(test.data$TOW_NO))]

#fixed effects for tows that weren't sampled for meat weight shell height; here ID tows that were NOT sampled   
fixed.pred <- (1:dim(liveweightYYYY)[1])[!is.element(liveweightYYYY$TOW_NO,unique(test.data$TOW_NO))]

#Predict using Random effects for tows that were sampled for meat weight shell height
for(i in random.pred) temp[i,] <- as.vector(predict(MWTSHGM.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i], 40),TOW_NO=liveweightYYYY$TOW_NO[i]),re.form=NULL,type="response"))

#Predict using fixed effects for tows that weren't sampled for meat weight shell height
for(i in fixed.pred) temp[i,] <- as.vector(predict(MWTSHGM.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i], 40)), re.form=~0,type="response"))


#multply temp matrix (weight) by live numbers to get weight/size bin
liveweightYYYY[,grep("BIN_ID_0", colnames(liveweightYYYY)):grep("BIN_ID_195", colnames(liveweightYYYY))] <- temp*livefreqYYYY[,grep("BIN_ID_0", colnames(livefreqYYYY)):grep("BIN_ID_195", colnames(livefreqYYYY))]

#export file for later analysis
write.csv(liveweightYYYY, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/GMliveweight",surveyyear,".csv")) 


rm(uid)
rm(pwd)
rm(un.sameotoj)
rm(pw.sameotoj)

#!!!Now save workspace as .RData object: e.g. GMgrowth2019.RData
#save.image(file = paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/GMgrowth",surveyyear,".RData"))

#NEW - save only the objects we need later
save(MWTSHGM.YYYY, latt, liveweightYYYY, GMdetail.foryear, GMlivefreq, livefreqYYYY,
     file=paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/GMgrowth",surveyyear,".RData"))


# ----  Condition for Spatial Map ----
# Only calculates the point condition for the single year you modelled above (i.e. surveyyear that's set above) 

livefreq.condition.spatial <- livefreqYYYY
#adjust depth, must center on same mean as used in original model
livefreq.condition.spatial$log.ctr.adj_depth <- log(abs(livefreq.condition.spatial$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.condition.spatial <- livefreq.condition.spatial[is.element(livefreq.condition.spatial$TOW_NO, unique(test.data$TOW_NO)),c("TOW_NO","START_LAT", "START_LONG", "ADJ_DEPTH","log.ctr.adj_depth")]
livefreq.condition.spatial$YEAR <- surveyyear #add year to dataframe


#Predict a meat weight for 100mm for just sampled tows; #re.form=NULL (all random effects included since only predicting on those tows that were sampled)

livefreq.condition.spatial$Condition <- predict(MWTSHGM.YYYY,newdata=data.frame(Log.HEIGHT.CTR=rep(log(100), dim(livefreq.condition.spatial)[1])-mean(test.data$Log.HEIGHT),
                                    Log.DEPTH.CTR=livefreq.condition.spatial$log.ctr.adj_depth, 
                                    TOW_NO=livefreq.condition.spatial$TOW_NO),
                                    re.form=NULL, type="response")

head(livefreq.condition.spatial)

#export for spatial plot later 
write.csv(livefreq.condition.spatial, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/GMConditionforMap",surveyyear,".csv"))


# ---- For Condition Time Series Figure ----

#mean.depth <- read.csv('Y:/INSHORE SCALLOP/BoF/StandardDepth/BoFMeanDepths.csv') #File for the constant depth to predict on by area
#DEFINE DEPTH:
#-54.74 is for VMS INSIDE AREA
#-56.65 is for VMS OUTER AREA


#Bring in file with depths by area, note some are by strata groups within area
mean.depth <- read.csv('Y:/Inshore/Assessment/StandardDepth/BoFMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area
#mean.depth <- read.csv('Y:/Inshore/StandardDepth/BoFMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area
unique(mean.depth$AREA)
length(mean.depth$AREA)

mean.depth.GM <- mean.depth[mean.depth$AREA %in% c("SPA6 Modelled Area VMS IN",
                                                  "SPA6 OUT"),]
dim(mean.depth.GM)[1] == 2
unique(mean.depth.GM$AREA)


#create name column that matches condition file: 
mean.depth.GM$AREA_NAME <- NA
mean.depth.GM$AREA_NAME[mean.depth.GM$AREA == "SPA6 Modelled Area VMS IN"] <- "INVMS"
mean.depth.GM$AREA_NAME[mean.depth.GM$AREA == "SPA6 OUT"] <- "OUTVMS"

#add year, condition column 
mean.depth.GM$YEAR <- surveyyear
mean.depth.GM$Condition <- NA

for (i in 1:length(mean.depth.GM$AREA)){
  mean.depth.GM$Condition[i] <- predict(MWTSHGM.YYYY, newdata = data.frame(Log.HEIGHT.CTR=log(100) - mean(test.data$Log.HEIGHT),
                                                                           Log.DEPTH.CTR = log(abs(mean.depth.GM$MeanDepth_m[i])) - mean(test.data$Log.DEPTH)),
                                        re.form = NA, type = "response")  
}
mean.depth.GM
mean.depth.GM$Condition <- round(mean.depth.GM$Condition,3)


#Import previous year condition file: 
#note contains IN and OUT VMS strata condition; they don't vary much since the only predictor is depth and SH is constant at 100mm 
GM.con.ts <- read.csv(paste0(path.directory, assessmentyear-1,"/Assessment/Data/SurveyIndices/SPA6/SPA6_ConditionTimeSeries.csv"))

#GM.con.ts <- GM.con.ts[GM.con.ts$YEAR!=2019,] 
#GM.con.ts <- GM.con.ts %>% add_row(YEAR = 2020, STRATA = "INVMS", CONDITION = NA) %>% #Add in condition NA for 2020
#  add_row(YEAR = 2020, STRATA = "OUTVMS", CONDITION = NA)

#update timeseries and write out new file: 
GM.con.ts <- rbind(GM.con.ts %>% select(YEAR, STRATA, CONDITION), mean.depth.GM %>% select("YEAR",STRATA="AREA_NAME",CONDITION="Condition"))

GM.con.ts <- GM.con.ts[order(GM.con.ts$STRATA, GM.con.ts$YEAR),]
GM.con.ts
GM.con.ts$CONDITION <- round(GM.con.ts$CONDITION,3)

write.csv(GM.con.ts, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA6_ConditionTimeSeries.csv"))

GM.con.ts<- read.csv(paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA6_ConditionTimeSeries.csv"))


#... Plot Condition Time Series Figure:
GM.con.ts$strata.name <- NA 
GM.con.ts$strata.name[GM.con.ts$STRATA=="INVMS"] <- "Inside VMS"
GM.con.ts$strata.name[GM.con.ts$STRATA=="OUTVMS"] <- "Outside VMS"

condition.ts.plot <- ggplot(GM.con.ts %>% filter(STRATA %in% c("INVMS", "OUTVMS")),
                            aes(x=YEAR, y=CONDITION,group_by(strata.name), color=strata.name)) +  
  geom_line(aes(linetype=strata.name)) + geom_point( size = 3, aes(shape=strata.name)) +
  xlab("Year") + ylab("Condition (meat weight, g)") + theme_bw() +
  coord_cartesian(ylim=c(8, 20)) +
  scale_y_continuous(breaks=seq(5, 20, 5))+
  #scale_x_continuous(breaks=seq(1995,2023, 2))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = c(.08, .9),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.0, keyheight = 1.2))
condition.ts.plot


#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA6_ConditionTimeSeries.png"), plot = condition.ts.plot, scale = 2.5, width = 9, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA6_ConditionTimeSeries.png" ),res = 200, width = 900, height = 600 )
#condition.ts.plot
#dev.off()

