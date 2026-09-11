###........................................###
###
###    SPA 6
###    Model
###    J. Sameoto Oct 2015,
###    Modified from:
###    L.Nasmith
###    September 2015
###   
###........................................###

### Modified Nov 2017 J.Sameoto and then DK Nov 2017
### DK modified in Aug/Sep 2018 to align with latest revisions to SPA 1A, 1B, 3, and 4.
###  J.Sameoto June 2020  Modified for running 2019 model run in Summer 2020 and for new folder structure of BOF 

# The November 2017 revision did the following to this file........................
# 1:  Removed the previous years model runs, the results of model runs from 2009-2015 are now stored in the folder ... \BoF\Model_results_2009_2015\.  This was
#     done because there was no consistent file structure before this time for how the model results were stored (if they were even retained).  Now all model results for these
#     years should be easily located.  From 2016 onwards the model results from the previous year are loaded from where they were saved last year
# 2:  Defined a set number of iterations for all the model MCMC runs stored above, niter was 100,000, nburnin was 50,000, nchains = 3, and nthin = 10.  For model year
#     2016 I left these values as whatever they were in 2016 as this model was run and I wanted to use those results for the prediction evaluation to be consistent.
# 3:  While niter and other MCMC parameters can be changed, unless the model structure changes I don't see any reason to use anything other than these values as the 
#     model appears to converge, but we do need to improve our convergence criteria for example we don't return Rhat values which is a good quick convergence criteria check.
# 4:  Made a SPA6.inits function below, this was more useful when running a bunch of different years, but works and there is no need to rename this each year.  If you
#     change the nchains you'll need to add/remove a row from this (you need 1 list(P=...) for each chain).
# 5:  Using variable NY rather than entering the number of years in all places required.
# 6:  Created/organized some very basic file structure for the results from this based on what we had in place.  
#     Each year we will need to create a folder structure .../'Year' Assement/'Area'....  This is where we should put everything related to at least the modelling for a given
#     area.  We can have a bigger discussion on what exactly should go where whenever we have time.  All model results are being place in a sub-folder called "ModelOutput"
#     So for 2017 the model script for SPA6 is found at Y:\INSHORE SCALLOP\BoF\2017\2017 Assessment\SPA6\SPA6_Model_2017_final.r
#     And the model results (figures, tables, Rdata files) are found inside Y:\INSHORE SCALLOP\BoF\2017\2017 Assessment\SPA6\ModelOutput
#     The larger discussion about file organization may alter this structure but until that happens please look at 2017 file structure to mimic this in subsequent years.
#  7: Options to save the figures and model results are embedded in the code, they are commented out by default so you don't overwrite something by mistake, uncomment these
#     to save.
#  8: The old model run files + an intermediary file I used to transition from old way to this way can be found in the "archive" folder found in each areas Assessment folder
#  9:  Thanks for Freya we now have a function (BoF.model.stats) which produces a nice summary of the model results

# In May 2018 while doing somethng completely different DK decided to:
# 1:  Remove the ooogly stript which had us inputing data manually 
# 2:  Note that you'll need to manually update the file SPA4_ModelData.xlsx each year.
# In September 2018 the prediction evaluation was overhauled and automated, manual entries are now minimized in this script to the
# as great an extent as possible, I believe you will only need to update catch.next.year in the script.

###  J.Sameoto June 2020  Modified for running 2019 model run in Summer 2020 and for new folder structure of BOF 
## g and gr were completely recalculated for the full timeseries in 2020 - these values used for this model run 
## Remember - model is only run for the Inside VMS area and therefore the Catch for the model is the catch associated with the inside VMS area - not the full SPA 6 catch that you'd see in the landings figures 
### J.Sameoto June 2021 modified to save out select result and diagnostics to be used by future Rmd CSAS Update file, make model file name generic so don't update every year,  

#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES
#in model formulation use: for BoF: have biomass remove catch then grow up and kill off; in 29: have survey, grow up animals, kill off animals due to natural mortality, THEN remove catch
#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES#####NOTES

rm(list=ls(all=T))
options(stringsAsFactors = FALSE)

#DEFINE:
direct <- "Y:/Inshore/Assessment/BoF"
#direct <- "Y:/Inshore/BoF"
assessmentyear <- 2025 #year in which you are conducting the assessment 
surveyyear <- 2025 #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019 
area <- 6  #this would be the SPA, for entries options are to use: 1A, 1B, 3, 4, or 6  


# Set the value for catch next year, assume same catch removals as current year, this is used in SSModel.plot.median() after the model runs; note no interim used for SPA 6 fishery 
catch.next.year <- 173 #0.55*314 = 172.7

#reference points - **NEW BIOMASS ref pts for SPA 6**
#Set reference points 
USR <- 471
LRP <- 236


#required packages
library(SSModel) #v 1.0-5
#library(R2WinBUGS)
library(openxlsx)
library(lubridate)
library(compareDF)
library(tidyverse)
require(openxlsx)
#remotes::install_github('jsameoto/rosettafish')
library(rosettafish)

#### Import Mar-scal functions 
#funcs <- c("https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/CreateExcelModelFile.R",
#           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_plot_median_new.r",
#           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_predict_summary_median.r",
#           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/BoFmodelstats.R")
#dir <- getwd()
#for(fun in funcs) 
#{
#  temp <- dir
#  download.file(fun,destfile = basename(fun))
#  source(paste0(dir,"/",basename(fun)))
#  file.remove(paste0(dir,"/",basename(fun)))
#}

funcs <- c("https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/CreateExcelModelFile.R",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_plot_median_new.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/SSModel_predict_summary_median.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/BoF/Model/BoFmodelstats.R")
dir <- tempdir()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


## need this for decision tables 
SPA6.landings <- read.xlsx(paste0(direct, "/", assessmentyear, "/Assessment/Data/CommercialData/SPA6_TACandLandings_",assessmentyear,".xlsx"))

## ---- Build and check the model input file ----

# Steps:
# 1) run the function below with savefile=F
# 2) review the messages in the R console
# 3) review CompareOutputTable_SPAxx.xlsx
# 4) Highlight any problematic values in yellow, and re-save as CompareOutputTable_SPAxx_highlighted.xlsx
# 5) Add manual edits to CreateExcelModelFile_2020.R as needed, in the EDITS section (~line 345). Keep it organized by SPA!
# 6) Review the diagnostics in the Plots and Viewer tabs of Rstudio
# 7) run the function below with savefile=T
# 8) colour code the notes as needed
# 9) when satisfied with the table, re-name it to remove the date. E.g. SPAxx_ModelData_R.xlsx 

CreateExcelModelFile(direct = direct, 
                     assessmentyear=2025, surveyyear = 2025, 
                     area = 6, LastYearsModelRData = "SPA6_Model_2024", 
                     savefile = T)

# for testing only (using FK private repo): 
# direct_test <- "C:/Users/keyserf/Documents/Github/BoF/"
# source(paste0(direct_test, "CreateExcelModelFile_2020.R"))

#################################################################

#recruit biomass and cv use spr for 2007+

# Let's be consistent with our MCMC!
niter = 400000
nchains = 3
nburnin = 200000
nthin = 10

#Set parameters you want to come back from the winbugs model 
parm = c("B","R","q","K","P","sigma","S","m","kappa.tau","r", "Fmort","mu","Irep","IRrep","Presid","sPresid","Iresid","IRresid","sIresid","sIRresid")
#parm = c("B","R","q","K","P","sigma","S","m","kappa.tau","r", "Fmort","mu","Irep","IRrep")


# Read in the data...  Note that in 2018 SPA6 is the one area which wasn't updated with the new _R Data file system as some work remains to get 
# all the data organized
raw.dat <- read.xlsx(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA",area,"/SPA6_ModelData_R_2025-10-16.xlsx"),sheet = "AlignedForModel",
                     cols=1:13)
str(raw.dat)
raw.dat$C <- as.numeric(raw.dat$C)

#Set the year range
yrs <- min(raw.dat$YearSurvey,na.rm=T):max(raw.dat$YearSurvey,na.rm=T)
#yrs <-2006:2019
NY <- length(yrs)
# Set the value for catch next year, this is used in SSModel.plot.median() after the model runs
#catch.next.year <- 162
raw.dat$C[raw.dat$YearSurvey == max(raw.dat$YearSurvey)] <- catch.next.year
raw.dat <- raw.dat[raw.dat$YearSurvey %in% yrs,]
raw.dat
SPA6.dat <- as.list(raw.dat[,c("C","N","I","I.cv","IR","IR.cv","ratiolined","g","gR","clappers")])
SPA6.dat$NY <- NY
# We use the same inits every year so why make something unique every year?  Note that the number of lists needs to make the number of chains
SPA6.inits <- function(NY)
{
  structure(list(list(P=round(runif(NY,0.01,3),2),K=100,r=round(runif(NY,0.01,3),2),S=0.15,q=0.01,sigma=1,kappa.tau=2), 
                 list(P=round(runif(NY,0.01,3),2),K=1000,r=round(runif(NY,0.01,3),2),S=0.85,q=0.6,sigma=0.1,kappa.tau=1),
                 list(P=round(runif(NY,0.01,3),2),K=500,r=round(runif(NY,0.01,3),2),S=0.5,q=0.4,sigma=0.5,kappa.tau=1.5)))
}

BoFSPA6.priors <- BoFSPA4.priors
# For the SPA6 priors we did this in the past, but in 2018 it put m into a parameter space that crashed the model
# reasons for this change were never documented so not sure why it was done, guessing to make the model work!
#BoFSPA6.priors$kappa.tau.b <- 2.5

# ---- Run the model ----
Spa6.model <- SSModel(SPA6.dat,BoFSPA6.priors,SPA6.inits(NY),model.file=BoFmodel,Years=yrs, parms = parm,
                     nchains=nchains,niter=niter,nburnin=nburnin,nthin=nthin,debug=T) #increased inter and used higher thin to combat autocorrelation (as evidenced by low n.eff)

#need to save model as year defined object for prediction evaluations 
assign(paste0("Spa6.", max(yrs)), Spa6.model)   

# Save this when you are happy with the results
save(list = paste0("Spa6.", max(yrs)), file=paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA",area,"/SPA6_Model_",max(yrs),".RData"))
#save(Spa6.model,file = paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA6_Model_",max(yrs),".RData"))

# If you are happy with the run you did most recently you can just load it rather than re-running the model.
#load(paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA6_Model_",max(yrs),".RData"))

# This just saves you wasting time copy/pasting an updated name everywhere below.
mod.res <- Spa6.model
#mod.res <- Spa6.2025


#This gives a print to screen of model results and allows you to save it
temp <- print(mod.res)
write.csv(temp, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/spa6ModelOutput.csv",sep=""))

# You want this to be a minimum of 400, if less than this you should increase your chain length
min.neff <- min(mod.res$summary[,9])
if(min.neff < 400) print(paste("Hold up sport!! Minimum n.eff is",min.neff," You should re-run the model and increase nchains until this is at least 400")) 
if(min.neff >= 400) print(paste("Good modelling friend, your minimum n.eff is",min.neff)) 
# same idea for Rhat, here we want to make sure Rhat is < 1.05 which suggests the chains are well mixed.
Rhat <- signif(max(mod.res$summary[,8]),digits=4)
if(Rhat > 1.05) print(paste("Hold up mes amis!! You have an Rhat of ",Rhat," ideally this would be below 1.05, check your model results and consider running a longer chain")) 
if(Rhat <= 1.05) print(paste("Good modelling friend, your max Rhat is",Rhat)) 


# This prints and saves some interesting summary statistics in a file structure that we don't really look at anymore
summ.Spa6 <- summary(mod.res)
dump('summ.Spa6', paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/spa6Modelsummary.R"))


# This plots the time series of survey biomass estimates.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Survey_est_figure_",area,".png"),width=8,height=11,units = "in",res=920)
SSModel.plot.median(mod.res, type="Survey.est")
dev.off()

# This is our biomass time series, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_",area,".png"),width=8,height=11,units = "in",res=920)
SSModel.plot.median(mod.res,Catch.next.year=catch.next.year,pred.lim=0.2,log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY])
dev.off()

# This is our FRENCH biomass time series, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_",area,"_FR.png"),width=8,height=11,units = "in",res=920)
SSModel.plot.median(mod.res,Catch.next.year=catch.next.year,pred.lim=0.2,log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY], french=TRUE)
dev.off()

##NEW APRIL 2022
# This is our biomass time series with EXAMPLE reference points, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_RefPts_",area,".png"),width=8,height=11,units = "in",res=920)

RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.4,1.4,1.4))

SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2, log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY],RP.labels = RP.labels) 

dev.off()

#Just commercial biomass time series (for presentation)
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_ComRefPts_",area,".png"),width=9,height=8,units = "in",res=920)

RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.4,1.4,1.4))

SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2, log.R=NULL,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY],RP.labels = RP.labels) 

dev.off()

# This is our FRENCH biomass time series with reference points, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_RefPts_",area,"_FR.png"),width=8,height=11,units = "in",res=920)

RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.4,1.4,1.4))

SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2, log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY],RP.labels = RP.labels, french=TRUE) 

dev.off()


# Plot of the posteriors, this is not all of our posteriors it is worth noting, just a selection of them.
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Posterior_figure_",area,".png"),width=8,height=11,units = "in",res=920)
#plot(SPA6.new.2017, type="Prior.Post")
SSModel.plot.median(mod.res, type="Prior.Post")
dev.off()

# Plot the exploitation and natural mortalities.
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Mortality_figure_",area,".png"),width=8,height=11,units = "in",res=920)
SSModel.plot.median(mod.res, type="Exploit")
dev.off()


# ---- RESIDUAL PLOTS ----
#Assuming in your parm call you included pulling the model residuals from winbugs, then you can run the following residual plots
#mod.res <- Spa6.2019
Presids <- data.frame(mod.res $summary[grepl('^Presid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Presids
Presids$year <- 2006:max(yrs)
# The names for inshore might be different so check those if this breaks
Presids <- Presids %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(Presids) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.Presids <- ggplot(Presids) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
plot.Presids
#Based on scale of unstandarized process residuals either in kilotonnes or scaled by K ? Need to talk to Dave 

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.Presids_",area,".png"),width=11,height=8,units = "in",res=920)
plot.Presids
dev.off()


sPresid <- data.frame(mod.res $summary[grepl('^sPresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Presids
sPresid$year <- 2006:max(yrs)
# The names for inshore might be different so check those if this breaks
sPresid <- sPresid %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(sPresid) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.sPresid <- ggplot(sPresid) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
plot.sPresid 

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.sPresid_",area,".png"),width=11,height=8,units = "in",res=920)
plot.sPresid
dev.off()


Iresid <- data.frame(mod.res $summary[grepl('^Iresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Iresid
Iresid$year <- 2006:max(yrs)
# The names for inshore might be different so check those if this breaks
Iresid <- Iresid %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(Iresid) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.Iresid <- ggplot(Iresid) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
plot.Iresid 

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.Iresid_",area,".png"),width=11,height=8,units = "in",res=920)
plot.Iresid
dev.off()


IRresid <- data.frame(mod.res $summary[grepl('^IRresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Iresid
IRresid$year <- 2006:max(yrs)
# The names for inshore might be different so check those if this breaks
IRresid <- IRresid %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(IRresid) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.IRresid <- ggplot(IRresid) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
plot.IRresid 

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.IRresid_",area,".png"),width=11,height=8,units = "in",res=920)
plot.IRresid
dev.off()



# ---- PREDICTION EVALUTATION - Condition Assumption ---- 
# Load in the old model results for the prediction evaluation, this will now automatically load all the
# necessary data up to the year you want (whatever you specified in (yrs)
pred.yr <- 2011:max(yrs) # Neeed to set this here as these loads will all overwrite the maximum year needed below...
num.pred.years <- length(pred.yr)
t.yrs <- yrs
for(i in 1:num.pred.years)
{
if(pred.yr[i] < 2016)   load(file = paste0(direct, "/Model_results_2009_2015/SPA6/SPA6_",pred.yr[i],".RData"))  
if(pred.yr[i] %in% c(2016, 2017, 2018))  load(file = paste0(direct, "/",pred.yr[i],"/",pred.yr[i]," Assessment/SPA6/ModelOutput/SPA6_Model_",pred.yr[i],".RData")) 
if(pred.yr[i] %in% c(2019, 2020, 2021))  load(file = paste0(direct,"/2021/Assessment/Data/Model/SPA",area,"/SPA6_Model_",pred.yr[i],".RData")) 
if(pred.yr[i] > 2021)  load(file = paste0(direct,"/",pred.yr[i],"/Assessment/Data/Model/SPA",area,"/SPA6_Model_",pred.yr[i],".RData"))
}
yrs <- t.yrs # Just so yrs doesn't get overwritten by what we load here
# Pull together the prediction data, note that this is now automated and will pull in the correct data for each year.
# SPA6 more straightforward than other areas
Combined.runs.predicted <- NULL
for(i in 1:num.pred.years) 
{
  #Grab the correct data...
  if(pred.yr[i] < 2016) dat <- get(paste0("Spa6.new.",pred.yr[i]))
  if(pred.yr[i] >= 2016) dat <- get(paste0("Spa6.",pred.yr[i]))
  Combined.runs.predicted[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch = raw.dat$C[raw.dat$YearSurvey==pred.yr[i]],
                                                                         g.parm= dat$data$g[dat$data$NY],
                                                                         gr.parm = dat$data$gR[dat$data$NY])
}

str.yr.pe <- min(as.numeric(names(Combined.runs.predicted))) # First year for the pe plot...

# Plot this PREDICTED prediction-evaluation plot.
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Prediction_evaluation_figure_predicted_",area,".png"),width=11,height=8,units = "in",res=920)
eval.predict(Combined.runs.predicted, Year=str.yr.pe, pred.lim=0.2)
dev.off()

# Pull together the prediction data, note that this is now automated and will pull in the correct data for each year.
num.pred.years <- length(pred.yr)
Combined.runs.actual <- NULL
for(i in 1:num.pred.years) 
{
  #Here we want to use the previous years results for everything except the growth parameters
  if(pred.yr[i] < 2016) dat <- get(paste0("Spa6.new.",pred.yr[i]))
  if(pred.yr[i] >= 2016) dat <- get(paste0("Spa6.",pred.yr[i]))
  # Run the prediction... Note I grab the estimate from the raw.dat object from above, saves a lot of grief!!
  Combined.runs.actual[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch = raw.dat$C[raw.dat$YearSurvey==pred.yr[i]],
                                                                      g.parm= raw.dat$g[raw.dat$YearSurvey==pred.yr[i]],
                                                                      gr.parm = raw.dat$gR[raw.dat$YearSurvey==pred.yr[i]])

} # end for(i in 1:num.pred.years) 

str.yr.pe <- min(as.numeric(names(Combined.runs.actual))) # first year for the pe plot

# Plot this ACTUAL prediction-evaluation plot.
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Prediction_evaluation_figure_actual_",area,".png"),width=11,height=8,units = "in",res=920)
eval.predict(Combined.runs.actual, Year=str.yr.pe, pred.lim=0.2)
dev.off()




# --- One Year Projection Boxplot for Plot ----
# Create data object (posterior distribution) associated with one year projection with interm TAC removals -- i.e. 1 year project boxplot data for commercial biomass timeseries figure in FSAR 
# Note B.next is next year predicted biomass having grown up scallop as per g.parm and gr.parm and using mortality - default in function is m.avg = 5 (last 5 years)
pred.1yr.boxplot <- predict(mod.res, Catch=catch.next.year, g.parm=mod.res$data$g[mod.res$data$NY],gr.parm=mod.res$data$gR[mod.res$data$NY])
pred.1yr.boxplot$B.next
median((pred.1yr.boxplot$B.next))
write.csv(pred.1yr.boxplot$B.next, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/boxplot.data.1y.predict.catch.of.",catch.next.year,".in.",max(yrs)+1,"_",area,".csv"),row.names = F)



# --- Decision Tables ----
#generally for documentation don't exceed e=0.18 TO 0.2 on table (NOTE: formal RR was decided for SPA6 in 2022 during AC meeting and first implemented in 2023, rest of SPA RR is 0.15)
#Finally here we have the decision table.  This plots the decision table for all catch rates between 0 and 500 increments of 10 tonnes of catch (seq(0,500,10)).

#decision <- predict(mod.res, Catch=c(seq(100, 250, 10)),g.parm=mod.res$data$g[mod.res$data$NY],gr.parm=mod.res$data$gR[mod.res$data$NY]) #g.parm and gr now updated automaticaly
#decision.table <- SSModel_predict_summary_median(decision, RRP=0.18)
#decision.table
#write.csv(decision.table, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/decisiontable",max(yrs),"_",area,".csv"),row.names = F)


#decision table with reference points 
decision  <- predict (mod.res, Catch=c(seq(0, 135, 15)), g.parm=mod.res$data$g[mod.res$data$NY],gr.parm=mod.res$data$gR[mod.res$data$NY]) 
decision.table <- SSModel_predict_summary_median(decision, LRP=LRP, USR=USR, RRP=0.18)
decision.table

#Add column for equivalent catch from total area
#first define proportion of catch from modeled area (Making the assumption that the proportion of catch from the modeled area stays the same from assessmentyear to assessmentyear+1)
prop.catch.in <- round(SPA6.landings %>% filter(Year == "Prop_IN") %>%
  dplyr::select(paste0(assessmentyear)) %>% pull(),2)
#Add Catch.all column and calculate equivalent catch for total area
decision.table$Next.year <- decision.table$Next.year %>% 
  mutate(Catch.all = round(Catch/prop.catch.in,0))
decision.table
#write.csv(decision.table, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/decisiontable.MOCK.Ref.Pts.",max(yrs),"_",area,".csv"),row.names = F)
write.csv(decision.table, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/decisiontable",max(yrs),"_",area,".csv"),row.names = F)


# Finally, use the BoF Model stats function to produce a nice summary of model info that we need for assessment docs text
#Produces files: summary stats temporal_6_2019.csv ; summary stats_6_2019.csv
# Be sure to set assessmentyear and surveyyear and RDatafile appropriately !!
#stats.output <- BoF.model.stats(area = "6", assessmentyear=2023, surveyyear=2023, direct = "Y:/Inshore/BoF/", RDatafile = "SPA6_Model_2023")
stats.output <- BoF.model.stats(area = "6", assessmentyear=assessmentyear, surveyyear=surveyyear, direct = "Y:/Inshore/Assessment/BoF/", RDatafile = paste0("SPA6_Model_",surveyyear))


# Probability that current year commercial biomass estimate is in the Healthy zone (i.e. above the USR), and in the cautious zone (ie. interpret as being above the LRP  and below the USR (i.e. 1- prob>USR): 
# Get the biomass posteriors for every year
B.dat <- mod.res$sims.matrix[, is.element(substr(dimnames(mod.res$sims.matrix)[[2]], 1, 2), "B[")]
#colnames(B.dat)
#dim(B.dat)[2]
#select last column of matrix since this is most recent year of biomasses 
B.dat.current.year <- B.dat[,dim(B.dat)[2]]

#probabilty that current year biomass is above the USR and above the LRP 
prob.above.USR <- sum(B.dat.current.year > USR)/length(B.dat.current.year)
prob.above.USR
prob.above.LRP <- sum(B.dat.current.year > LRP)/length(B.dat.current.year)
prob.above.LRP

#Save out key results and diagnostics -- use this workspace to source for .Rmd Update doc (Note can easily take 5 mins or so to save out)
#Objects saved out: LRP, USR, catch.next.year, min.neff, Rhat, temp, summ.Spa1A, decision, decision.table,  prob.above.USR, prob.above.LRP
tt <- Sys.time()
save(LRP, USR, catch.next.year, min.neff, Rhat, temp, stats.output, summ.Spa6, decision, decision.table, prob.above.USR, prob.above.LRP, 
     file=paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/Model_results_and_diagnostics_",max(yrs),"_",area,".RData"))
tt - Sys.time()

# ... END OF MODEL PLOTS...

############################################################################

## NEW INDICES PLOTS - BIOMASS (scaled to area) BY POPULATION NUMBER ###

raw.dat$wgt.num <- (raw.dat$I*1000000)/raw.dat$N #convert I in tonnes to grams
#coeff <- 10^7
options(scipen = 999)
raw.dat.forplot <- raw.dat #|> filter(YearCatch != 2024)

raw.dat.forplot <- raw.dat.forplot |> 
  rename("Biomass (tonnes)" = I) |> 
  rename("Numbers" = N) |> 
  rename("Average Weight per Scallop (g)" = wgt.num)

raw.dat.forplot.2 <- pivot_longer(raw.dat.forplot, 
                                  cols = c("Average Weight per Scallop (g)", "Numbers", "Biomass (tonnes)"),
                                  names_to = "Indices",
                                  #names_prefix = "X",
                                  values_to = "value",
                                  values_drop_na = FALSE)


I.N.plot.2 <- ggplot(data = raw.dat.forplot.2, aes (x = YearSurvey)) + 
  geom_line(data = raw.dat.forplot.2, aes(y = value), colour = "black") +
  scale_x_continuous(breaks=seq(min(raw.dat.forplot$YearSurvey),max(raw.dat.forplot$YearSurvey), 2))+
  theme_bw()+
  theme(axis.title.y.right = element_text(color = "grey"))+
  xlab("Year")+
  facet_wrap(Indices~., dir = "v", scales = "free")
I.N.plot.2

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA6/SPA6_population_number_panel_indices",surveyyear,".png"), type="cairo", width=20, height=15, units = "cm", res=300)
I.N.plot.2
dev.off() 


#Alternative plot:

raw.dat$wgt.num <- (raw.dat$I*1000000)/raw.dat$N #convert I in tonnes to grams
coeff <- 10^7
options(scipen = 999)
#raw.dat.forplot <- raw.dat #|> filter(YearSurvey != 2024)


I.N.plot <- ggplot(data = raw.dat, aes (x = YearSurvey)) + 
  geom_line(data = raw.dat, aes(y = wgt.num), colour = "black") +
  geom_line(data = raw.dat, aes(y = N/coeff), colour = "grey", linetype = "dashed") +
  scale_y_continuous(name = "Average weight per scallop (grams)",
                     sec.axis = sec_axis(~.*coeff, name = "Numbers of scallops"))+
  scale_x_continuous(breaks=seq(min(raw.dat.forplot$YearSurvey),max(raw.dat.forplot$YearSurvey), 2))+
  theme_bw()+
  theme(axis.title.y.right = element_text(color = "grey"))+
  xlab("Year")
I.N.plot

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA6/SPA4_population_number_index",surveyyear,".png"), type="cairo", width=20, height=15, units = "cm", res=300)
I.N.plot
dev.off() 
