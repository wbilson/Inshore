###........................................###
###
###    Meat weight/shell height modelling
###    SPA3: 1996 to 2016
###    L.Nasmith
###    August 2015
###  Mod. J.Sameoto Aug 2018, July 2019
###........................................###
#modified in July 2020 J.Sameoto 

#required packages
library(tidyverse)
library(ROracle)
library(lme4)
library(lattice)
require(data.table)
library(lubridate)

##(1) running (e.g.) example(lmer) with a fresh install of Matrix 1.6.4 from source fails with the error above, (2) a fresh install.packages("lme4", type = "source") resolves the problem.
#install.packages("lme4", type = "source")

# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
uid <- keyring::key_list("Oracle")[1,2]
pwd <- keyring::key_get("Oracle", uid)

surveyyear <- 2026  #This is the last survey year for which you want to include  - not should match year of cruise below 
cruise <- "BI2026"  #note should match year for surveyyear set above 

assessmentyear <- 2026 #year in which you are conducting the survey 
area <- "3"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Y:/Inshore/Assessment/BoF/"
#path.directory <- "Y:/Inshore/BoF/"



###
# read in shell height and meat weight data from database
###
#strata.spa3<-c(22:24)

#SQL query 1: detailed meat weight/shell height sampling data
quer1 <- paste0("SELECT * FROM scallsur.scwgthgt WHERE strata_id IN (22,23,24) AND cruise = '", cruise, "'")


#numbers by shell height bin
quer2 <- paste0("SELECT * FROM scallsur.scliveres WHERE strata_id IN (22,23,24) AND cruise = '", cruise, "'")


# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')


# Select data from database; execute query with ROracle; numbers by shell height bin
#detailed meat weight/shell height sampling data
BIdetail.dat <- dbGetQuery(chan, quer1)

#numbers by shell height bin
BIlivefreq.dat <- dbGetQuery(chan, quer2)

#add YEAR column to data
BIdetail.dat$YEAR <- as.numeric(substr(BIdetail.dat$CRUISE,3,6))
BIlivefreq.dat$YEAR <- as.numeric(substr(BIlivefreq.dat$CRUISE,3,6))


###
# read in depth information and add it to the meat weight/shell height dataframe
###
#there is no strata_id in Olex file to select on, need a unique identifier for tows to link to depth ($ID)

BIdetail.dat$ID <- paste(BIdetail.dat$CRUISE,BIdetail.dat$TOW_NO,sep='.')
BIlivefreq.dat$ID <- paste(BIlivefreq.dat$CRUISE,BIlivefreq.dat$TOW_NO,sep='.')
detail.ID <- unique(BIdetail.dat$ID)
livefreq.ID <- unique(BIlivefreq.dat$ID)

OlexTows_all <- read.csv("Y:/Inshore/Assessment/StandardDepth/towsdd_StdDepth.csv")
#OlexTows_all <- read.csv("Y:/Inshore/StandardDepth/towsdd_StdDepth.csv")
names(OlexTows_all)[which(colnames(OlexTows_all)=="RASTERVALU")] <- "OLEXDEPTH_M"   #rename "RASTERVALU" column
OlexTows_all$OLEXDEPTH_M[OlexTows_all$OLEXDEPTH_M==-9999] <- NA
OlexTows_all$ID <- paste(OlexTows_all$CRUISE,OlexTows_all$TOW_NO,sep='.')
OlexTows_detail <- subset(OlexTows_all, ID%in%detail.ID)
OlexTows_livefreq <- subset(OlexTows_all, ID%in%livefreq.ID)

BIdetail <- merge(BIdetail.dat,subset(OlexTows_detail,select=c("ID","OLEXDEPTH_M")), all.x=T)
BIdetail$ADJ_DEPTH <- BIdetail$OLEXDEPTH_M
BIdetail$ADJ_DEPTH[is.na(BIdetail$OLEXDEPTH_M)] <- -1*BIdetail$DEPTH[is.na(BIdetail$OLEXDEPTH_M)] #*-1 because DEPTH is positive

BIlivefreq <- merge(BIlivefreq.dat,subset(OlexTows_livefreq,select=c("ID","OLEXDEPTH_M")), all.x=T)
BIlivefreq$ADJ_DEPTH <- BIlivefreq$OLEXDEPTH_M
BIlivefreq$ADJ_DEPTH[is.na(BIlivefreq$OLEXDEPTH_M)] <- -1*BIlivefreq$DEPTH[is.na(BIlivefreq$OLEXDEPTH_M)] #*-1 because DEPTH is positive


###
# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
###
summary(BIdetail)
BIdetail <- BIdetail[complete.cases(BIdetail[,c(which(names(BIdetail)=="WET_MEAT_WGT"))]),]  #remove NAs from WET_MEAT_WEIGHT
BIdetail <- BIdetail[complete.cases(BIdetail[,c(which(names(BIdetail)=="HEIGHT"))]),]  #remove NAs from HEIGHT
summary(BIdetail)

#---- Meat weight shell height modelling ----

#Subset for year
BIdetailYYYY <- subset(BIdetail, YEAR==surveyyear)

#create dataset for model
test.data <- subset(BIdetailYYYY, HEIGHT>40)
test.data$Log.HEIGHT <- log(test.data$HEIGHT)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT-mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH-mean(test.data$Log.DEPTH)
summary(test.data)

#plot depths by tow
plot(ADJ_DEPTH~TOW_NO, data=test.data)
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/BItowdepth",surveyyear,".png")) #!!!DEFINE
plot(ADJ_DEPTH~TOW_NO, data=test.data)
dev.off()

#run model
MWTSHBI.YYYY <- glmer(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|TOW_NO),data=test.data,
                    family=Gamma(link=log), na.action = na.omit)

summary(MWTSHBI.YYYY)

#Save summary to txt file
sink(paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/MWTSHBI",surveyyear,"_ModelSummary.txt"))
print(summary(MWTSHBI.YYYY))
sink()

#diagnostics
latt <- data.frame(test.data, res=residuals(MWTSHBI.YYYY,"pearson"),fit=fitted(MWTSHBI.YYYY)) 

#Residuals vs fitted - full area 
plot(MWTSHBI.YYYY) 
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHBI",surveyyear,".png"))
plot(MWTSHBI.YYYY) 
dev.off()

#Plot of tow level residuals
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="residuals",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHBI",surveyyear,"_towresid.png"))
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="residuals",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
dev.off()

#Plot of tow level fitted values 
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="Wet Meat weight")
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSHBI",surveyyear,"_towfit.png"))
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="Wet Meat weight")
dev.off()

#Construct data.frame similar to BIlivenfreq for weight per tow
livefreqYYYY <- subset(BIlivefreq, YEAR==surveyyear)
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
for(i in random.pred) temp[i,] <- as.vector(predict(MWTSHBI.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i] ,40),TOW_NO=liveweightYYYY$TOW_NO[i]),re.form=NULL,type="response"))

#Predict using fixed effects for tows that weren't sampled for meat weight shell height
for(i in fixed.pred) temp[i,] <- as.vector(predict(MWTSHBI.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i] ,40)),re.form=~0,type="response"))


#multply temp matrix (weight) by live numbers to get weight/size bin
liveweightYYYY[,grep("BIN_ID_0", colnames(liveweightYYYY)):grep("BIN_ID_195", colnames(liveweightYYYY))] <- temp*livefreqYYYY[,grep("BIN_ID_0", colnames(livefreqYYYY)):grep("BIN_ID_195", colnames(livefreqYYYY))]

#export file for later analysis
write.csv(liveweightYYYY, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/BIliveweight",surveyyear,".csv")) 

rm(uid)
rm(pwd)
rm(un.sameotoj)
rm(pw.sameotoj)

#!!!Now save workspace as .RData object: e.g. BIgrowth2019.RData
save.image(file = paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/BIgrowth",surveyyear,".RData"))

#NEW - save only the objects we need later
save(MWTSHBI.YYYY, latt, liveweightYYYY, BIdetailYYYY, BIlivefreq, livefreqYYYY,
     file=paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/BIgrowth",surveyyear,".RData"))

# ---- MWSH Figure April 2024 JS 
Log.height.ctr
#predict at each SH bin mid point 

pred.log.depth.ctr <- log(abs(-47.63)) - mean(test.data$Log.DEPTH)


pred.weight.fixed <- as.vector(predict(MWTSHBI.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(pred.log.depth.ctr,length(Log.height.ctr))),re.form=~0,type="response"))

pred.weight.fixed <- data.frame(height =seq(2.5, 197.5, by = 5) , weight = pred.weight.fixed)
pred.weight.fixed
pred.weight.fixed <- pred.weight.fixed[pred.weight.fixed$height < 175,]

library(ggplot2)
ggplot(data = test.data, aes(x = HEIGHT, y = WET_MEAT_WGT, alpha = 0.5)) + 
  geom_point() + 
  geom_line(data =pred.weight.fixed,aes(x = height, y = weight  ), size = 1.5, col='blue')  + 
  ylab("Meat weight (g)") + 
  xlab("Shell height (mm)") + 
  theme_bw()
## TO DO --- predict line for each tow - plot as different colour lines from main survey line




# ----  Condition for Spatial Map ----
# Only calculates the point condition for the single year you modelled above (i.e. surveyyear that's set above) 

livefreq.condition.spatial <- livefreqYYYY
#adjust depth, must center on same mean as used in original model
livefreq.condition.spatial$log.ctr.adj_depth <- log(abs(livefreq.condition.spatial$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.condition.spatial <- livefreq.condition.spatial[is.element(livefreq.condition.spatial$TOW_NO, unique(test.data$TOW_NO)),c("TOW_NO","START_LAT", "START_LONG", "ADJ_DEPTH","log.ctr.adj_depth")]
livefreq.condition.spatial$YEAR <- surveyyear #add year to dataframe


#Predict a meat weight for 100mm for just sampled tows; #re.form=NULL (all random effects included since only predicting on those tows that were sampled)

livefreq.condition.spatial$Condition <- predict(MWTSHBI.YYYY,newdata=data.frame(Log.HEIGHT.CTR=rep(log(100), dim(livefreq.condition.spatial)[1])-mean(test.data$Log.HEIGHT),
                                                                                Log.DEPTH.CTR=livefreq.condition.spatial$log.ctr.adj_depth, 
                                                                                TOW_NO=livefreq.condition.spatial$TOW_NO),
                                                re.form=NULL, type="response")

head(livefreq.condition.spatial)

#export for spatial plot later 
write.csv(livefreq.condition.spatial, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/BIConditionforMap",surveyyear,".csv"))


# ---- For Condition Time Series Figure ----

#mean.depth <- read.csv('Y:/Inshore/BoF/StandardDepth/BoFMeanDepths.csv') #File for the constant depth to predict on by area
#DEFINE DEPTH:
#SPA3 VMS IN Modelled Area (InVMSandSMB) = -47.63
#SMB = -24.59
#InsideVMS (BILU_InsideVMS) = -61.1
#OutsideVMS = -89.71


#Bring in file with depths by area, note some are by strata groups within area
mean.depth <- read.csv('Y:/Inshore/Assessment/StandardDepth/BoFMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by are
#mean.depth <- read.csv('Y:/Inshore/StandardDepth/BoFMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area
unique(mean.depth$AREA)
length(mean.depth$AREA)

mean.depth.BI<- mean.depth[mean.depth$AREA %in% c("SaintMarysBay",
                                                    "SPA3 OUT",
                                                    "BILU_InsideVMS",
                                                    "SPA3 VMS IN Modelled Area"),]
dim(mean.depth.BI)[1] == 4
unique(mean.depth.BI$AREA)

#create name column that matches condition file: 
mean.depth.BI$AREA_NAME <- NA
mean.depth.BI$AREA_NAME[mean.depth.BI$AREA == "SaintMarysBay"] <- "SMB"
mean.depth.BI$AREA_NAME[mean.depth.BI$AREA == "BILU_InsideVMS"] <- "InVMS"
mean.depth.BI$AREA_NAME[mean.depth.BI$AREA == "SPA3 OUT"] <- "OutVMS"
mean.depth.BI$AREA_NAME[mean.depth.BI$AREA == "SPA3 VMS IN Modelled Area"] <- "InVMS_SMB"

#add year, condition column 
mean.depth.BI$YEAR <- surveyyear
mean.depth.BI$Condition <- NA

for (i in 1:length(mean.depth.BI$AREA)){
  mean.depth.BI$Condition[i] <- predict(MWTSHBI.YYYY, newdata = data.frame(Log.HEIGHT.CTR=log(100) - mean(test.data$Log.HEIGHT),
                                                                            Log.DEPTH.CTR = log(abs(mean.depth.BI$MeanDepth_m[i])) - mean(test.data$Log.DEPTH)),
                                         re.form = NA, type = "response")  
}
mean.depth.BI
mean.depth.BI$Condition <- round(mean.depth.BI$Condition,3)


#Import previous year condition file: 
#note contains IN and OUT VMS strata condition; they don't vary much since the only predictor is depth and SH is constant at 100mm 
BI.con.ts <- read.csv(paste0(path.directory, assessmentyear-1,"/Assessment/Data/SurveyIndices/SPA3/SPA3_ConditionTimeSeries.csv"))
#BI.con.ts <- BI.con.ts[BI.con.ts$YEAR!=2019,]

#Below was run for 2021 assessment only.. won't need to run again.
#BI.con.ts <- BI.con.ts %>% add_row(YEAR = 2020, STRATA = "InVMS_SMB", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "OutVMS", CONDITION = NA) %>% 
#  add_row(YEAR = 2020, STRATA = "InVMS", CONDITION = NA) %>%
#  add_row(YEAR = 2020, STRATA = "SMB", CONDITION = NA)
#BI.con.ts <- BI.con.ts %>% arrange(STRATA, YEAR)

#update timeseries and write out new file: 
BI.con.ts <- rbind(BI.con.ts %>% dplyr::select(YEAR, STRATA, CONDITION), mean.depth.BI %>% dplyr::select(YEAR, STRATA = AREA_NAME, CONDITION= Condition))

#Compare previous Year
#left_join(BI.con.ts %>% dplyr::select(YEAR, STRATA, CONDITION) %>% filter(YEAR == 2019),mean.depth.BI %>% dplyr::select(YEAR, STRATA = AREA_NAME, CONDITION= Condition), by = "STRATA")

BI.con.ts <- BI.con.ts %>% group_by(STRATA) %>% 
  arrange(STRATA, YEAR) %>% 
  ungroup()

BI.con.ts %>% print(n=Inf)

write.csv(BI.con.ts, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA3_ConditionTimeSeries.csv"))


#... Plot Condition Time Series Figure:
BI.con.ts$strata.name <- NA 
BI.con.ts$strata.name[BI.con.ts$STRATA=="SMB"] <- "St. Mary's Bay"
BI.con.ts$strata.name[BI.con.ts$STRATA=="InVMS"] <- "Inside VMS"
BI.con.ts$strata.name[BI.con.ts$STRATA=="OutVMS"] <- "Outside VMS"
BI.con.ts$strata.name[BI.con.ts$STRATA=="InVMS_SMB"] <- "Inside VMS (St. Mary's Bay)"

#BI.con.ts <- BI.con.ts |> 
#  mutate(YEAR = lubridate::year(BI.con.ts$YEAR))

## TO DO -- add median line per plot?? or does this make it too busy since would need to add LTM (not including most current year) for each 'subarea'.. to think on.. (JS)

condition.ts.plot <- ggplot(BI.con.ts %>% filter(STRATA %in% c("SMB", "InVMS", "OutVMS")),
                            aes(x=YEAR, y=CONDITION,group_by(strata.name), color=strata.name)) +  
  geom_line(aes(linetype=strata.name)) + geom_point( size = 3, aes(shape=strata.name)) +
  xlab("Year") + ylab("Condition (meat weight, g)") + theme_bw() +
  coord_cartesian(ylim=c(5, 20)) +
  scale_y_continuous(breaks=seq(5, 20, 5))+
  #scale_x_continuous(breaks=seq(1995,2023, 2))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = c(.008, .20),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.0, keyheight = 1.2))
condition.ts.plot


#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA3_ConditionTimeSeries.png"), plot = condition.ts.plot, scale = 2.5, width = 9, height = 6, dpi = 300, units = "cm", limitsize = TRUE)








