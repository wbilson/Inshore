###........................................###
###
###    SPA 4
###    Model
###
###
###    L.Nasmith
###    September 2016
###    Updated Sept 2017 - J.SAmeoto
###    Updated Oct 2017  - DK
###    Updated May 2018 - DK
###    Updated June 2020 - JS
###........................................###

# The October 2017 revision did the following to this file........................
# 1:  Removed the previous years model runs, the results of model runs from 2009-2015 are now stored in the folder ... \BoF\Model_results_2009_2015\.  This was
#     done because there was no consistent file structure before this time for how the model results were stored (if they were even retained).  Now all model results for these
#     years should be easily located.  From 2016 onwards the model results from the previous year are loaded from where they were saved last year
# 2:  Defined a set number of iterations for all the model MCMC runs stored above, niter was 100,000, nburnin was 50,000, nchains = 3, and nthin = 10.  For model year
#     2016 I left these values as whatever they were in 2016 as this model was run and I wanted to use those results for the prediction evaluation to be consistent.
# 3:  While niter and other MCMC parameters can be changed, unless the model structure changes I don't see any reason to use anything other than these values as the 
#     model appears to converge, but we do need to improve our convergence criteria for example we don't return Rhat values which is a good quick convergence criteria check.
# 4:  Made a SPA4.inits function below, this was more useful when running a bunch of different years, but works and there is no need to rename this each year.  If you
#     change the nchains you'll need to add/remove a row from this (you need 1 list(P=...) for each chain).
# 5:  Using variable NY rather than entering the number of years in all places required.
# 6:  Created/organized some very basic file structure for the results from this based on what we had in place.  
#     Each year we will need to create a folder structure .../'Year' Assement/'Area'....  This is where we should put everything related to at least the modelling for a given
#     area.  We can have a bigger discussion on what exactly should go where whenever we have time.  All model results are being place in a sub-folder called "ModelOutput"
#     So for 2017 the model script for SPA3 is found at Y:\INSHORE SCALLOP\BoF\2017\2017 Assessment\SPA3\SPA3_Model_2017_final.r
#     And the model results (figures, tables, Rdata files) are found inside Y:\INSHORE SCALLOP\BoF\2017\2017 Assessment\SPA3\ModelOutput
#     The larger discussion about file organization may alter this structure but until that happens please look at 2017 file structure to mimic this in subsequent years.
#  7: Options to save the figures and model results are embedded in the code, they are commented out by default so you don't overwrite something by mistake, uncomment these
#     to save.
#  8: The old model run files + an intermediary file I used to transition from old way to this way can be found in the "archive" folder found in each areas Assessment folder
#  9:  Thanks for Freya we now have a function (BoF.model.stats) which produces a nice summary of the model results

# In May/Aug 2018 while doing somethng completely different DK decided to:
# 1:  Remove the ooogly stript which had us inputing data manually 
# 2:  Note that you'll need to manually update the file SPA3_ModelData.xlsx each year.
# In September 2018 the prediction evaluation was overhauled and automated, manual entries are now minimized in this script to the
# as great an extent as possible, I believe you will only need to update catch.next.year in the script.

###  J.Sameoto June 2020  Modified for running 2019 model run in Summer 2020 and for new folder structure of BOF 
### J.Sameoto June 2021 modified to define LRP, USR up front, to calculate probabiltiy > USR and > LRP for current year commercial biomass, and save out select result and diagnostics to be used by future Rmd CSAS Update file, make model file name generic so don't update every year,  

rm(list=ls(all=T))
options(stringsAsFactors = FALSE)

#DEFINE:
direct <- "Y:/Inshore/Assessment/BoF"
#direct <- "Y:/Inshore/BoF"
assessmentyear <- 2025 #year in which you are conducting the assessment 
surveyyear <- 2025  #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019 
area <- 4  #this would be the SPA, for entries options are to use: 1A, 1B, 3, 4, or 6  

#reference points 
LRP <- 530
USR <- 750

# Put in the catch for next year in the 2018 slot of the "C" data
# Set the value for catch next year, this is used in SSModel.plot.median()
# This should be based on the interim TAC for the area
catch.next.year <- 80


#required packages
library(SSModel) #v 1.0-3
library(openxlsx)
#library(plotly)
library(compareDF)
#library(ggplot2)
library(tidyverse)
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
                     area = 4, LastYearsModelRData = "SPA4_Model_2024", 
                     savefile = T)

# for testing only (using FK private repo): 
# direct_test <- "C:/Users/keyserf/Documents/Github/BoF/"
# source(paste0(direct_test, "CreateExcelModelFile_2020.R"))

#################################################################


# Let's be consistent with our MCMC year over year unless there is a need to change it...
# Our goal here is to make sure the n.eff is at least 400 for all the parameters and our Rhat is < 1.05
niter = 600000 # default = 100000
nchains = 3    # default =33
nburnin = 250000  # default = 50000
nthin = 20       # default = 10

#Set parameters you want to come back from the winbugs model 
parm = c("B","R","q","K","P","sigma","S","m","kappa.tau","r", "Fmort","mu","Irep","IRrep","Presid","sPresid","Iresid","IRresid","sIresid","sIRresid")
#parm = c("B","R","q","K","P","sigma","S","m","kappa.tau","r", "Fmort","mu","Irep","IRrep")


# Bring in the data, you will need to update this with the latest numbers! Note that we are using teh R file generated by
# the new fangled scripts here, should be checked to make sure they make sense!
raw.dat <- read.xlsx(paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA",area,"/SPA4_ModelData_R_2025-10-20.xlsx"),sheet = "AlignedForModel", cols=1:13)
                    
str(raw.dat)
raw.dat$C <- as.numeric(raw.dat$C)
 
# Set the value for catch next year, this is used in SSModel.plot.median() after the model runs and in the final year for the
# prediction evaluation figures DK changed this in 2018
# This is typically based on the interim TAC.
#catch.next.year <- 135
raw.dat$C[raw.dat$YearSurvey == max(raw.dat$YearSurvey,na.rm=T)] <- catch.next.year
# Get the years you want to run.
yrs <- min(raw.dat$YearSurvey,na.rm=T):max(raw.dat$YearSurvey,na.rm=T)
#yrs <- 1992:2019
# Just in case you subset the years to be anything other than all the years in the mod.dat file...
raw.dat <- raw.dat[raw.dat$YearSurvey %in% yrs,]
NY <- length(yrs) # Number of years.
# Create the list for the model.

SPA4.dat <-  as.list(raw.dat[,c("C","N","I","I.cv","IR","IR.cv","ratiolined","g","gR","clappers")])
SPA4.dat$NY <- NY

# This makes a function for the SPA4 inits, built this for 3 chains, if you change the number of chains you will need to change the number of lists (each list is inits 
# for 1 chain)
SPA4.inits <- function(NY)
{
  structure(list(list(P=round(runif(NY,0.01,3),2),K=100,r=round(runif(NY,0.01,3),2),S=0.15,q=0.01,sigma=1,kappa.tau=0.5), 
                 list(P=round(runif(NY,0.01,3),2),K=1000,r=round(runif(NY,0.01,3),2),S=0.85,q=0.6,sigma=0.1,kappa.tau=1),
                 list(P=round(runif(NY,0.01,3),2),K=500,r=round(runif(NY,0.01,3),2),S=0.5,q=0.4,sigma=0.5,kappa.tau=1.25)))
}

# ---- Run the model ----
# Note that I've been running into issues with BUGS struggling with high intial values of kappa.tau in the above, if BUGS trips saying
# cannot bracket slice for node m[] lower the largest value you have for kappa.tau in the inits function above
Spa4.model <- SSModel(SPA4.dat,BoFSPA4.priors,SPA4.inits(NY),model.file=BoFmodel,Years=yrs, parms = parm, nchains=nchains,niter=niter,nburnin=nburnin,nthin=nthin,debug=T)

#need to save model as year defined object for prediction evaluations 
assign(paste0("Spa4.", max(yrs)), Spa4.model)   

# Save this when you are happy with the results
save(list = paste0("Spa4.", max(yrs)), file=paste0(direct,"/",assessmentyear, "/Assessment/Data/Model/SPA",area,"/SPA4_Model_",max(yrs),".RData"))

# If you are happy with the run you did most recently you can just load it rather than re-running the model.
#load(file = paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA4_Model_",max(yrs),".RData"))
#This is just to save you from wasting time changing the names of a bunch of lines below...
#mod.res <- Spa4.2024
mod.res <- Spa4.model

#This gives a print to screen of model results and allows you to save it
temp <- print(mod.res)
# Save this output
write.csv(temp, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/spa4ModelOutput.csv"))

# You want this to be a minimum of 400, if less than this you should increase your chain length
min.neff <- min(mod.res$summary[,9])
if(min.neff < 400) print(paste("Hold up sport!! Minimum n.eff is",min.neff," You should re-run the model and increase nchains until this is at least 400")) 
if(min.neff >= 400) print(paste("Good modelling friend, your minimum n.eff is",min.neff)) 
# same idea for Rhat, here we want to make sure Rhat is < 1.05 which suggests the chains are well mixed.
Rhat <- signif(max(mod.res$summary[,8]),digits=4)
if(Rhat > 1.05) print(paste("Hold up mes amis!! You have an Rhat of ",Rhat," ideally this would be below 1.05, check your model results and consider running a longer chain")) 
if(Rhat <= 1.05) print(paste("Good modelling friend, your max Rhat is",Rhat)) 


# This prints and saves some interesting summary statistics in a file structure that we don't really look at anymore
summ.Spa4 <- summary(mod.res)
dump('summ.Spa4', paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/spa4ModelsummaryObj.R"))

# This plots the time series of survey biomass estimates.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Survey_est_figure_",area,".png"),width=8,height=11,units = "in",res=920)
#plot(Spa4.2017, type="Survey.est")
SSModel.plot.median(mod.res, type="Survey.est")
dev.off()

# This is our biomass time series with reference points, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_",area,".png"),width=8,height=11,units = "in",res=920)
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2018,2018,2018),size = c(1.4,1.4,1.4))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,RP.labels = RP.labels, log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY]) 

dev.off()

# This is our FRENCH biomass time series with reference points, box plot contains 80% of the data when pred.lim = 0.2 (i.e. 80% of the data is located between the whiskers)
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Model_biomass_figure_",area,"_FR.png"),width=8,height=11,units = "in",res=920)
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2018,2018,2018),size = c(1.4,1.4,1.4))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,RP.labels = RP.labels, log.R=F,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY], french = TRUE) 

dev.off()


#windows()
jpeg(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Biomass_figure_for_Document_",area,".jpg"),width=11,height=8,units = "in",res=920)
#RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2015,2017),size = c(1.4,1.4,1.4))
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.1,1.1,1.1))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY],log.R=NULL,RP.labels = RP.labels,cex=1.4)  
dev.off()

#windows()
jpeg(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Biomass_figure_for_Document_",area,"_FR.jpg"),width=11,height=8,units = "in",res=920)
#RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2015,2017),size = c(1.4,1.4,1.4))
RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2017,2017,2017),size = c(1.1,1.1,1.1))
SSModel.plot.median(mod.res, ref.pts=c(LRP,USR), Catch.next.year=catch.next.year, pred.lim=0.2,
                    g=mod.res$data$g[mod.res$data$NY],gR=mod.res$data$gR[mod.res$data$NY],log.R=NULL,RP.labels = RP.labels,cex=1.4, french = TRUE)  
dev.off()

# Plot of the posteriors, this is not all of our posteriors it is worth noting, just a selection of them.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Posterior_figure_",area,".png"),width=8,height=11,units = "in",res=920)
#plot(Spa4.2017, type="Prior.Post")
SSModel.plot.median(mod.res, type="Prior.Post")
dev.off()

# Plot the exploitation and natural mortalities.
#windows()
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Mortality_figure_",area,".png"),width=8,height=11,units = "in",res=920)
#plot(Spa4.2017, type="Exploit")
SSModel.plot.median(mod.res, type="Exploit")
dev.off()

# ---- RESIDUAL PLOTS ----
#Assuming in your parm call you included pulling the model residuals from winbugs, then you can run the following residual plots
#mod.res <- Spa4.2019
Presids <- data.frame(mod.res$summary[grepl('^Presid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Presids
Presids$year <- 1983:max(yrs)
# The names for inshore might be different so check those if this breaks
Presids <- Presids %>% dplyr::select(X50.,X2.5.,X97.5.,year)
names(Presids) <- c("median","LCI","UCI","year")
# And with almost no effort here's a nice plot.
plot.Presids <- ggplot(Presids) + geom_point(aes(x=year, y= median)) + 
  geom_errorbar(aes(x = year,ymin = LCI,ymax=UCI),width=0) + 
  theme_bw() + geom_hline(yintercept = 0)
#Based on scale of unstandarized process residuals either in kilotonnes or scaled by K ? Need to talk to Dave 
plot.Presids

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/plot.Presids_",area,".png"),width=11,height=8,units = "in",res=920)
plot.Presids
dev.off()


sPresid <- data.frame(mod.res$summary[grepl('^sPresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Presids
sPresid$year <- 1983:max(yrs)
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



Iresid <- data.frame(mod.res$summary[grepl('^Iresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Iresid
Iresid$year <- 1983:max(yrs)
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



IRresid <- data.frame(mod.res$summary[grepl('^IRresid',rownames(mod.res $summary)),])  #pull process residuals 
# Pick the appropriate years. note length of year must be same as length of Iresid
IRresid$year <- 1983:max(yrs)
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



# --- Prediction Evaluations - Condition Assumption ---- 
# Load in the old model results for the prediction evaluation, this will now automatically load all the
# necessary data up to the year you want (whatever you specified in (yrs)
pred.yr <- 2009:max(yrs) # Need to set this here as these loads will all overwrite the maximum year neeeded below...
num.pred.years <- length(pred.yr)
t.yrs <- yrs
for(j in 1:num.pred.years)
{
  if(pred.yr[j] < 2016)   load(file = paste0(direct,"/Model_results_2009_2015/SPA4/SPA4_",pred.yr[j],".RData"))  
  if(pred.yr[j] %in% c(2016, 2017, 2018))  load(file = paste0(direct,"/",pred.yr[j],"/",pred.yr[j]," Assessment/SPA4and5/ModelOutput/SPA4_Model_",pred.yr[j],".RData"))  
#  if(pred.yr[j] %in% c(2016))  load(file = paste0(direct,"/",pred.yr[j],"/",pred.yr[j]," Assessment/SPA4and5/ModelOutput/SPA4_Model_",pred.yr[j],".RData"))  
#  if(pred.yr[j] %in% c(2017))  load(file = paste0(direct,"/",pred.yr[j],"/",pred.yr[j]," Assessment/SPA4and5/ModelOutput/SPA4_Model_",pred.yr[j],".RData"))  
#  if(pred.yr[j] %in% c(2018))  load(file = paste0(direct,"/",pred.yr[j],"/",pred.yr[j]," Assessment/SPA4and5/ModelOutput/SPA4_Model_",pred.yr[j],".RData"))  
 # if(pred.yr[j]  %in% c(2019, 2020, 2021))   load(file = paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/SPA4_Model_",pred.yr[j],".RData"))
  if(pred.yr[j] == 2019)   load(file = paste0(direct,"/2020/Assessment/Data/Model/SPA",area,"/SPA4_Model_",pred.yr[j],".RData")) 
  #Note no model run for 2020 since no survey in 2020 but ran the model with imputed data since needed for prediction evals in 2021 
  if(pred.yr[j] == 2020)   load(file = paste0(direct,"/2021/Assessment/Data/Model/SPA",area,"/SPA4_Model_",pred.yr[j],".RData"))
  if(pred.yr[j] >= 2021)   load(file = paste0(direct,"/",pred.yr[j],"/Assessment/Data/Model/SPA",area,"/SPA4_Model_",pred.yr[j],".RData")) 
}

yrs <- t.yrs # Just so yrs doesn't get overwritten by what we load here

# Pull together the prediction data, note that this is now automated and will pull in the correct data for each year.
# Runs the prediction evaluation using the growth rates PREDICTED for g and gR parameters.  
Combined.runs.predicted <- NULL
for(i in 1:num.pred.years) 
{
  #Grab the correct data...
  dat <- get(paste0("Spa4.",pred.yr[i]))
  # The correct Catch data isn't available until the following year, so use this information for all but the most recent year...
  if(i < num.pred.years) 
  {
    dat.next <- get(paste0("Spa4.",(pred.yr[i]+1)))
    Combined.runs.predicted[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch = as.numeric(raw.dat$C[raw.dat$YearSurvey == (pred.yr[i])]),
                                                                           g.parm= dat$data$g[dat$data$NY],
                                                                           gr.parm = dat$data$gR[dat$data$NY])
  } # end if(i < num.pred.years) 
  # Now for the most recent year we need to supply what we expect for the catch next year, this value is supplied above in the catch.next.year obj.
  if(i == num.pred.years)    Combined.runs.predicted[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch = catch.next.year,
                                                                                                    g.parm= dat$data$g[dat$data$NY],
                                                                                                    gr.parm = dat$data$gR[dat$data$NY])
} # end for(i in 1:num.pred.years) 

str.yr.pe <- min(as.numeric(names(Combined.runs.predicted))) # Start year for the prediction eval plots

# Plot this PREDICTED prediction-evaluation plot
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Prediction_evaluation_figure_predicted_",area,".png"),width=11,height=8,units = "in",res=920)
eval.predict(Combined.runs.predicted, Year=str.yr.pe, pred.lim=0.2)
dev.off()

#Runs the prediction evaluation using the ACTUAL growth rates for g and gR parameters.  
Combined.runs.actual <- NULL
for(i in 1:num.pred.years) 
{
  dat <- get(paste0("Spa4.",(pred.yr[i])))
  # Run the prediction... Note I grab the estimate from the raw.dat object from above, saves a lot of grief!!
  Combined.runs.actual[[as.character(pred.yr[i] +1)]] <- predict(dat, Catch = as.numeric(raw.dat$C[raw.dat$YearSurvey == (pred.yr[i])]),
                                                                 g.parm= as.numeric(raw.dat$g[raw.dat$YearSurvey == (pred.yr[i])]),
                                                                 gr.parm = as.numeric(raw.dat$gR[raw.dat$YearSurvey == (pred.yr[i])]))
} # end for(i in 1:num.pred.years) 

str.yr.pe <- min(as.numeric(names(Combined.runs.actual)))

# Plot this ACTUAL prediction-evaluation plot
png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA",area,"/Prediction_evaluation_figure_actual_",area,".png"),width=11,height=8,units = "in",res=920)
eval.predict(Combined.runs.actual, Year=str.yr.pe,pred.lim=0.2)
dev.off()



# --- One Year Projection Boxplot for Plot ----
# Create data object (posterior distribution) associated with one year projection with interm TAC removals -- i.e. 1 year project boxplot data for commercial biomass timeseries figure in FSAR 
# Note B.next is next year predicted biomass having grown up scallop as per g.parm and gr.parm and using mortality - default in function is m.avg = 5 (last 5 years)
pred.1yr.boxplot <- predict(mod.res, Catch=catch.next.year, g.parm=mod.res$data$g[mod.res$data$NY],gr.parm=mod.res$data$gR[mod.res$data$NY])
pred.1yr.boxplot$B.next
median((pred.1yr.boxplot$B.next))
write.csv(pred.1yr.boxplot$B.next, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/boxplot.data.1y.predict.catch.of.",catch.next.year,".in.",max(yrs)+1,"_",area,".csv"),row.names = F)



# --- Decision Tables ----
#Finally here we have the decision table.  This plots the decision table for all catch rates between 0 and 500 increments of 10 tonnes of catch (seq(0,500,10)).
decision <- predict(mod.res, Catch=c(seq(50,160,20)), g.parm=mod.res$data$g[mod.res$data$NY],gr.parm=mod.res$data$gR[mod.res$data$NY])
decision.table <- SSModel_predict_summary_median(decision, LRP=LRP, USR=USR, RRP=0.15)
decision.table

write.csv(decision.table, paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/decisiontable",max(yrs),"_",area,".csv"),row.names = F)

# Finally we summarize the model results that are used in our CSAS text.
#Produces files: summary stats temporal_4_2019.csv ; summary stats_4_2019.csv
# UPDATE: Be sure to set assessmentyear and surveyyear and RDatafile appropriately !!
#stats.output <- BoF.model.stats(area = "4", assessmentyear=2023, surveyyear=2023, direct = "Y:/Inshore/BoF/", RDatafile = "SPA4_Model_2023")
stats.output <- BoF.model.stats(area = "4", assessmentyear=assessmentyear, surveyyear=surveyyear, direct = "Y:/Inshore/Assessment/BoF/", RDatafile = paste0("SPA4_Model_",surveyyear))


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
save(LRP, USR, catch.next.year, min.neff, Rhat, temp, stats.output, summ.Spa4, decision, decision.table,  prob.above.USR, prob.above.LRP,
     file=paste0(direct,"/",assessmentyear,"/Assessment/Data/Model/SPA",area,"/Model_results_and_diagnostics_",max(yrs),"_",area,".RData"))
tt - Sys.time()

# ... END OF MODEL PLOTS...

############################################################################

## NEW INDICES PLOTS - BIOMASS (scaled to area) BY POPULATION NUMBER ###

raw.dat$wgt.num <- (raw.dat$I*1000000)/raw.dat$N #convert I in tonnes to grams
coeff <- 10^7
options(scipen = 999)
raw.dat.forplot <- raw.dat# |> filter(YearSurvey != 2024)

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

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA4/SPA4_population_number_panel_indices",surveyyear,".png"), type="cairo", width=20, height=15, units = "cm", res=300)
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

png(paste0(direct,"/",assessmentyear,"/Assessment/Figures/Model/SPA4/SPA4_population_number_index",surveyyear,".png"), type="cairo", width=20, height=15, units = "cm", res=300)
I.N.plot
dev.off() 






 
  