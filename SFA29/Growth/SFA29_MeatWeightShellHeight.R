###........................................###
###
###    Meat weight/shell height modelling
###    SFA29: 2014 to 2015
###    J.Sameoto, modified from
###    L.Nasmith
###    January 2016
###
###........................................###

#2016 analysis done by L. Nasmith
#overhauled script Oct 2021 J. Sameoto - note is to model only 1 year at a time 

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
cruise <- "SFA292026"  #note should match year for surveyyear set above 

assessmentyear <- 2027 #year in which you are conducting the assessment (surveyyear+1)
path.directory <- "Y:/Inshore/Assessment/SFA29/"
#path.directory <- "Y:/Inshore/SFA29/"

weight.by.tow.previous.yrs <- read.csv(paste0("Y:/Inshore/Assessment/SFA29/",assessmentyear-1,"/Assessment/Data/SurveyIndices/SFA29liveweight2014to",assessmentyear-2,".csv"))
weight.by.tow.previous.yrs <- weight.by.tow.previous.yrs[,2:dim(weight.by.tow.previous.yrs)[2]]

####
# read in shell height and meat weight data from database
####

# Get the cruises
cruise.list <- paste0("SFA29",(2001:(surveyyear)))
cruise.list <- paste(cruise.list,collapse="','")

# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"), username = uid, password = pwd,'ptran')

#detailed meat weight/shell height sampling data
quer1 <- paste(
		"SELECT *                                     ",
		"FROM scallsur.scwgthgt                      ",
		"WHERE strata_id IN (41, 42, 43, 44, 45)      ",
		"AND (cruise in ('",cruise.list,"'))          ",
		sep=""
	    )

SFA29detail.dat <- dbGetQuery(chan, quer1)


#numbers by shell height bin
quer2 <- paste(
		"SELECT *                                     ",
		"FROM scallsur.SCLIVERES                      ",
		"WHERE strata_id IN (41, 42, 43, 44, 45)      ",
		"AND (cruise in ('",cruise.list,"'))          ",
		sep=""
	    )

#SFA29livefreq.dat<- sqlQuery(RODBCconn, quer2, believeNRows=FALSE)
SFA29livefreq.dat <- dbGetQuery(chan, quer2)


#add YEAR column to data
SFA29detail.dat$YEAR <- as.numeric(substr(SFA29detail.dat$CRUISE,6,9))
SFA29livefreq.dat$YEAR <- as.numeric(substr(SFA29livefreq.dat$CRUISE,6,9))

###
# read in depth information and add it to the meat weight/shell height dataframe
###
#there is no strata_id in Olex file to select on, need a unique identifier for tows to link to depth ($ID)

SFA29detail.dat$ID <- paste(SFA29detail.dat$CRUISE,SFA29detail.dat$TOW_NO,sep='.')
uniqueID <- unique(SFA29detail.dat$ID)

OlexTows_all <- read.csv("Y:/Inshore/Assessment/StandardDepth/towsdd_StdDepth.csv")
names(OlexTows_all)[which(colnames(OlexTows_all)=="RASTERVALU")] <- "OLEXDEPTH_M"   #rename "RASTERVALU" column
OlexTows_all$OLEXDEPTH_M[OlexTows_all$OLEXDEPTH_M==-9999] <- NA
OlexTows_all$ID <- paste(OlexTows_all$CRUISE,OlexTows_all$TOW_NO,sep='.')
OlexTows_bof <- subset(OlexTows_all, ID%in%uniqueID)

SFA29detail <- merge(SFA29detail.dat,subset(OlexTows_bof,select=c("ID","OLEXDEPTH_M")), all.x=T)
SFA29detail$ADJ_DEPTH <- SFA29detail$OLEXDEPTH_M
SFA29detail$ADJ_DEPTH[is.na(SFA29detail$OLEXDEPTH_M)] <- -1*SFA29detail$DEPTH[is.na(SFA29detail$OLEXDEPTH_M)] #*-1 because DEPTH is positive

#Check if problem with merge: dim(BFdetail.dat)[1] and dim(BFdetail)[1] should be the same -- TRUE: 
dim(SFA29detail.dat)[1] == dim(SFA29detail)[1]

#add depth to the livefreq dataframe
SFA29livefreq.dat$ID <- paste(SFA29livefreq.dat$CRUISE,SFA29livefreq.dat$TOW_NO,sep='.')
uniqueIDb <- unique(SFA29livefreq.dat$ID)

OlexTows_freq <- subset (OlexTows_all,ID%in%uniqueIDb)
SFA29livefreq <- merge(SFA29livefreq.dat,subset(OlexTows_freq,select=c("ID","OLEXDEPTH_M")), all.x=T)
SFA29livefreq$ADJ_DEPTH <- SFA29livefreq$OLEXDEPTH_M
SFA29livefreq$ADJ_DEPTH[is.na(SFA29livefreq$OLEXDEPTH_M)] <- -1*SFA29livefreq$DEPTH[is.na(SFA29livefreq$OLEXDEPTH_M)]
summary(SFA29livefreq)

####
# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
####
summary(SFA29detail)
dim(SFA29detail)
SFA29detail <- SFA29detail[complete.cases(SFA29detail[,c(which(names(SFA29detail)=="WET_MEAT_WGT"))]),]  #remove NAs from WET_MEAT_WEIGHT
SFA29detail <- SFA29detail[complete.cases(SFA29detail[,c(which(names(SFA29detail)=="HEIGHT"))]),]  #remove NAs from HEIGHT
summary(SFA29detail)
dim(SFA29detail)
table(SFA29detail$YEAR)

#---- Meat weight shell height modelling ----
# For all of SFA29 #

#Subset for year
SFA29detail.foryear <- subset(SFA29detail, YEAR==surveyyear) #!!!defined via objects in Define section

plot(SFA29detail.foryear$WET_MEAT_WGT ~ SFA29detail.foryear$HEIGHT)

#create dataset for model
test.data <- subset(SFA29detail.foryear, HEIGHT>40)
test.data$Log.HEIGHT <- log(test.data$HEIGHT)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT - mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)
#plot(x = test.data$Log.HEIGHT.CTR, y = test.data$WET_MEAT_WGT)
#test.data <- test.data |> filter(TOW_NO != 15)
#test.data |> filter(TOW_NO == 15)

#plot depths by tow
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/SFA29towdepth",surveyyear,".png")) 
plot(ADJ_DEPTH~TOW_NO, data=test.data)
dev.off()

#run model

#NOTE: Model did not converge in 2023, so the 2023 MWSH model uses a different optimizer - optimizer="bobyqa" to produce a model that converges.

#MWTSHSFA29.YYYY <- glmer(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|TOW_NO),data=test.data, family=Gamma(link=log), na.action = na.omit)

MWTSHSFA29.YYYY <- glmer(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|TOW_NO),data=test.data,     
              family=Gamma(link=log), na.action = na.omit, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

summary(MWTSHSFA29.YYYY) 

#lm.test <- lm(log(WET_MEAT_WGT)~log(HEIGHT), data = test.data) 
#summary(lm.test)
#plot(lm.test)

#Save summary to txt file
sink(paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/MWTSHSFA29_ModelSummary.txt"))
print(summary(MWTSHSFA29.YYYY))
sink()

#diagnostics
latt <- data.frame(test.data, res=residuals(MWTSHSFA29.YYYY,"pearson"),fit=fitted(MWTSHSFA29.YYYY)) 

#Residuals vs fitted - full area 
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSH.29.",surveyyear,".png"))
plot(MWTSHSFA29.YYYY) 
dev.off()

#Plot of tow level residuals
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSH.29.",surveyyear,"_towresid.png"))
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="residuals",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
dev.off()

#Plot of tow level fitted values 
png(paste0(path.directory, assessmentyear, "/Assessment/Figures/Growth/MWTSH.29.",surveyyear,"_towfit.png"))
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="fitted", ylab="Wet Meat weight")
dev.off()

#Construct data.frame similar to SFA29livefreq for weight per tow
livefreqYYYY <- subset(SFA29livefreq, YEAR==surveyyear)
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
for(i in random.pred) temp[i,] <- as.vector(predict(MWTSHSFA29.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i] ,40),TOW_NO=liveweightYYYY$TOW_NO[i]),re.form=NULL,type="response"))

#Predict using fixed effects for tows that weren't sampled for meat weight shell height
for(i in fixed.pred) temp[i,] <- as.vector(predict(MWTSHSFA29.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i] ,40)),re.form=~0,type="response"))

#multply temp matrix (weight) by live numbers to get weight/size bin
liveweightYYYY[,grep("BIN_ID_0", colnames(liveweightYYYY)):grep("BIN_ID_195", colnames(liveweightYYYY))] <- temp*livefreqYYYY[,grep("BIN_ID_0", colnames(livefreqYYYY)):grep("BIN_ID_195", colnames(livefreqYYYY))]


#export file for later analysis
write.csv(liveweightYYYY, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SFA29liveweight",surveyyear,".csv")) 

#append current year to previous years (since 2014) 
weight.by.tow.updated <- rbind(weight.by.tow.previous.yrs, liveweightYYYY)
#export file for later analysis
write.csv(weight.by.tow.updated, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SFA29liveweight2014to",surveyyear,".csv")) 

#need to save model as year defined object 
assign(paste0("MWTSHSFA29.", surveyyear), MWTSHSFA29.YYYY)   
#assign(paste0("MWTSHSFA29.", 2019), MWTSHSFA29.YYYY)   
assign(paste0("latt.", surveyyear), latt)   
assign(paste0("liveweight.", surveyyear), liveweightYYYY)   
assign(paste0("SFA29detail.", surveyyear), SFA29detail.foryear)   
assign(paste0("SFA29livefreq.", surveyyear), SFA29livefreq)   
assign(paste0("livefreq.", surveyyear), livefreqYYYY)   

#save only the objects we need later
save(list = c(paste0("MWTSHSFA29.", surveyyear), paste0("latt.", surveyyear), paste0("liveweight.", surveyyear), paste0("SFA29detail.",surveyyear), paste0("SFA29livefreq.", surveyyear), paste0("livefreq.",surveyyear)),
    file=paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SFA29growth",surveyyear,".RData"))


# ----  Condition for Spatial Map ----
# predicts meat weight of 100 mm SH scallop given depth at sampled locations 
# Only calculates the point condition for the single year you modelled above (i.e. surveyyear that's set above) 

livefreq.condition.spatial <- livefreqYYYY
#adjust depth, must center on same mean as used in original model
livefreq.condition.spatial$log.ctr.adj_depth <- log(abs(livefreq.condition.spatial$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.condition.spatial <- livefreq.condition.spatial[is.element(livefreq.condition.spatial$TOW_NO, unique(test.data$TOW_NO)),c("TOW_NO","START_LAT", "START_LONG", "ADJ_DEPTH","log.ctr.adj_depth")]
livefreq.condition.spatial$YEAR <- surveyyear #add year to dataframe


#Predict a meat weight for 100mm for just sampled tows; #re.form=NULL (all random effects included since only predicting on those tows that were sampled)

livefreq.condition.spatial$Condition <- predict(MWTSHSFA29.YYYY,newdata=data.frame(Log.HEIGHT.CTR=rep(log(100), dim(livefreq.condition.spatial)[1])-mean(test.data$Log.HEIGHT),
                                                                                Log.DEPTH.CTR=livefreq.condition.spatial$log.ctr.adj_depth, 
                                                                                TOW_NO=livefreq.condition.spatial$TOW_NO),
                                                re.form=NULL, type="response")

head(livefreq.condition.spatial)

#export for spatial plot later 
write.csv(livefreq.condition.spatial, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SFA29ConditionforMap",surveyyear,".csv"))



# ----  Condition Time Series Figures ----
# see'Y:/Inshore/SFA29/SFA29DepthProfile/AreaMeanDepths.xlsx' #File for the constant depth to predict on by area
#subarea A  -75.05039
#subarea B  -46.36262
#subarea C  -37.33779
#subarea D  -47.18775
#subarea E  -67.65107

#Bring in file with depths by area, note some are by strata groups within area
mean.depth <- read.csv('Y:/Inshore/Assessment/SFA29/SFA29DepthProfile/SFA29_AreaMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area
mean.depth
unique(mean.depth$AREA)
length(mean.depth$AREA)

mean.depth.29 <- mean.depth[mean.depth$AREA %in% c("SFA29A",
                                                    "SFA29B",
                                                    "SFA29C",
                                                    "SFA29D",
                                                    "SFA29E"),]
dim(mean.depth.29)[1] == 5
unique(mean.depth.29$AREA)

mean.depth.29$YEAR <- surveyyear
mean.depth.29$Condition <- NA

for (i in 1:length(mean.depth.29$AREA)){
  mean.depth.29$Condition[i] <- predict(MWTSHSFA29.YYYY, newdata = data.frame(Log.HEIGHT.CTR=log(100) - mean(test.data$Log.HEIGHT),
                                        Log.DEPTH.CTR = log(abs(mean.depth.29$MeanDepth_m[i])) - mean(test.data$Log.DEPTH)),
                                        re.form = NA, type = "response")  
}
mean.depth.29

#Import previous year condition file: 
SFA29.con.ts <- read.csv(paste0(path.directory, assessmentyear-1,"/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeseries2001to",surveyyear-1,".csv"))
SFA29.con.ts
unique(SFA29.con.ts$STRATA)
#note NA for 2020 since no survey in 2020 

#update timeseries and write out new file: 
SFA29.con.ts <- rbind(SFA29.con.ts %>% dplyr::select(YEAR, STRATA, CONDITION), mean.depth.29 %>% dplyr::select("YEAR",STRATA="AREA",CONDITION="Condition"))

SFA29.con.ts <- SFA29.con.ts[order(SFA29.con.ts$STRATA, SFA29.con.ts$YEAR),]
SFA29.con.ts

write.csv(SFA29.con.ts, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SFA29W_ConditionTimeSeries2001to",surveyyear,".csv"))

SFA29.con.ts <- SFA29.con.ts %>% 
  mutate(STRATA = case_when(STRATA == "SFA29A" ~ "Subarea A", 
                           STRATA == "SFA29B" ~ "Subarea B",
                           STRATA == "SFA29C" ~ "Subarea C",
                           STRATA == "SFA29D" ~ "Subarea D",
                           STRATA == "SFA29E" ~ "Subarea E"))

#... Plot Condition Time Series Figures:

SFA29.condition.ts.plot <- ggplot(SFA29.con.ts,
                                  aes(x=YEAR, y=CONDITION,group_by(STRATA), color=STRATA)) +  
  geom_line(aes(linetype=STRATA)) + geom_point( size = 3, aes(shape=STRATA)) +
  scale_linetype_manual(values = c(1:5)) +  
  scale_colour_manual(values = c("turquoise4", "goldenrod2", "mediumvioletred", "seagreen", "blue4")) +  
  scale_shape_manual(values = c(19,17,15,3,18))+
  xlab("Year") + ylab("Condition (meat weight, g)") + theme_bw() +
  coord_cartesian(ylim=c(5, 20)) +
  #scale_y_continuous(breaks=seq(5, 20, 5))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = c(.01, 0.99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1.5)) +
  scale_x_continuous(breaks = seq(1998,2026,by=4), limits = c(2000,2026)) 

SFA29.condition.ts.plot

#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SFA29W.ConditionTimeSeries.png"), plot = SFA29.condition.ts.plot, scale = 2.5, width = 8, height = 6, dpi = 300, units = "cm", limitsize = TRUE)



# --- Predict meat weight of 105 mm SH to get wK for model ---- 
# Here we are getting the condition to predict for the model.  NOTE: to predict wk for model:  predicted shell height to 105mm
# Also note that we are predicting on the average depth as calcuated for the entire subarea 
#mod.cond <- data.frame(cond = rep(NA,length(mean.depth.29$AREA)), area= rep(NA,length(mean.depth.29$AREA)), year = rep(NA,length(mean.depth.29$AREA)))


mean.depth.wk <- mean.depth[mean.depth$AREA %in% c("SFA29A",
                                                   "SFA29B",
                                                   "SFA29C",
                                                   "SFA29D",
                                                   "SFA29E"),]
dim(mean.depth.wk)[1] == 5
unique(mean.depth.wk$AREA)

mean.depth.wk$YEAR <- surveyyear
mean.depth.wk$Condition_105mmSH <- NA
mean.depth.wk
for (i in 1:length(mean.depth.wk$AREA)){
  mean.depth.wk$Condition_105mmSH[i] <- predict(MWTSHSFA29.YYYY, newdata = data.frame(Log.HEIGHT.CTR=log(105) - mean(test.data$Log.HEIGHT),
                                                                              Log.DEPTH.CTR = log(abs(mean.depth.wk$MeanDepth_m[i])) - mean(test.data$Log.DEPTH)),
                                        re.form = NA, type = "response")  
}
mean.depth.wk

write.csv(mean.depth.wk, paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/Condition.105mmSH.wk.",surveyyear,".csv"))

## END ## 
