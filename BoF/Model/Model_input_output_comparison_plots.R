#Create plots comparing the following:

# - clapper numbers (model input) to natural mortality
# - Biomass numbers (model input) to Biomass model output
# - Recruit numbers (model input) to Recruit Biomass model output

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(openxlsx)

direct <- "Y:/Inshore/Assessment/BoF/"
area <- "6" #"1A", "1B", "3", "4", "6"
year <- 2026 # year the script is being run (used in the file names)


#Will need to update the model data files:
if(area == "1A"){
  modfile <- read.xlsx(paste0(direct,year,"/Assessment/Data/Model/SPA1A/SPA1A_ModelData_R_",year,"-10-16.xlsx"),sheet = "AlignedForModel", cols=1:13)
  mod.sum <- read.csv(paste0(direct,year,"/Assessment/Data/Model/SPA1A/Spa1AModelOutput.csv"))
  title <- "SPA1A"
}

if(area == "1B"){
  modfile <- read.xlsx(paste0(direct,year,"/Assessment/Data/Model/SPA1B/SPA1B_ModelData_R_",year,"-10-16.xlsx"),sheet = "AlignedForModel", cols=1:13)
  mod.sum <- read.csv(paste0(direct,year,"/Assessment/Data/Model/SPA1B/Spa1BModelOutput.csv"))
  title <- "SPA1B"
}

if(area == "3"){
  modfile <- read.xlsx(paste0(direct,year,"/Assessment/Data/Model/SPA3/SPA3_ModelData_R_",year,"-10-16.xlsx"),sheet = "AlignedForModel", cols=1:13)
  mod.sum <- read.csv(paste0(direct,year,"/Assessment/Data/Model/SPA3/Spa3ModelOutput.csv"))
  title <- "SPA3"
}

if(area == "4"){
  modfile <- read.xlsx(paste0(direct,year,"/Assessment/Data/Model/SPA4/SPA4_ModelData_R_",year,"-10-19.xlsx"),sheet = "AlignedForModel", cols=1:13)
  mod.sum <- read.csv(paste0(direct,year,"/Assessment/Data/Model/SPA4/Spa4ModelOutput.csv"))
  title <- "SPA4"
}

if(area == "6"){
  modfile <- read.xlsx(paste0(direct,year,"/Assessment/Data/Model/SPA6/SPA6_ModelData_R_",year,"-10-19.xlsx"),sheet = "AlignedForModel", cols=1:13)
  mod.sum <- read.csv(paste0(direct,year,"/Assessment/Data/Model/SPA6/Spa6ModelOutput.csv"))
  title <- "SPA6"
}

#Run for individual areas --------

# clapper numbers (model input) to natural mortality ------

scx.dead <- modfile %>% 
  select(YearSurvey, clappers)

# Extract natural mort (inst)

colnames(mod.sum) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")

m.rows <- grep("^m\\[", mod.sum$vars)
m <- mod.sum[m.rows, ]
# convert inst mort to prop mort
m.prop <- m
m.prop[2:8] <- signif(1-exp(-m.prop[2:8]),2)

mort <- cbind(scx.dead, m.prop$median)
mort <- mort |> 
  dplyr::rename("Nat_mort" = "m.prop$median")

clap.mort.plot <- ggplot(mort)+
  geom_point(aes(x = clappers, y = Nat_mort))+
  geom_smooth(aes(x = clappers, y = Nat_mort), method = glm, se = FALSE)+
  geom_text_repel(aes(x = clappers, y = Nat_mort, label = YearSurvey), max.overlaps = 25)+
  #coord_cartesian(xlim = xlim, ylim = ylim)+
  xlab("Number of Clappers")+ ylab("Natural Mortality (proportional rate)")+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -15, hjust = 0.05))
clap.mort.plot

ggsave(filename=paste0(direct,year,"/Assessment/Figures/Exploratory_plots/NaturalMort_Clappers_SPA", area, "_",year,".png"), clap.mort.plot, dpi = 600, width = 6.5, height = 5.5)

#for SPA4 only
#clap.mort.plot.4 <- ggplot(mort %>% filter(YearSurvey != c(1989,1990,1991)))+
#  geom_point(aes(x = clappers, y = Nat_mort))+
#  geom_smooth(aes(x = clappers, y = Nat_mort), method = glm, se = FALSE)+
#  geom_text_repel(aes(x = clappers, y = Nat_mort, label = YearSurvey), max.overlaps = 25)+
  #coord_cartesian(xlim = xlim, ylim = ylim)+
#  xlab("Number of Clappers")+ ylab("Natural Mortality (proportional rate)")+
#  labs(title = title)+
#  theme_bw()+
#  theme(plot.title = element_text(vjust = -15, hjust = 0.05))
#clap.mort.plot.4

#ggsave(filename=paste0("Y:/Inshore/BoF/2025/Assessment/Figures/Exploratory_plots/NaturalMort_Clappers_SPA4_rmOutliers_2025.png"), clap.mort.plot.4, dpi = 600, width = 6.5, height = 5.5)


mort.4.plot <- pivot_longer(mort, 
                            cols = c("clappers", "Nat_mort"),
                            #names_prefix = "X",
                            values_to = "value",
                            values_drop_na = FALSE)

clap.mort.plot.2 <- ggplot(data = mort.4.plot, aes (x = YearSurvey)) + 
  geom_line(data = mort.4.plot, aes(y = value), colour = "black") +
  scale_x_continuous(breaks=seq(min(mort$YearSurvey),max(mort$YearSurvey), 2))+
  theme(axis.title.y.right = element_text(color = "grey"))+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -15, hjust = 0.05))+
  xlab("Year")+
  facet_wrap(name~., dir = "v", scales = "free")
clap.mort.plot.2

ggsave(filename=paste0(direct,year,"/Assessment/Figures/Exploratory_plots/NaturalMort_Clappers_byyear_SPA", area,"_",year,".png"), clap.mort.plot.2, dpi = 600, width = 10.5, height = 5.5)

# Commercial Biomass (input) vs Biomass (output) ------

scx.com.biomass <- modfile %>% 
  select(YearSurvey, I)

colnames(mod.sum) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
# Extract biomass (commercial)
b.rows <- grep("^B\\[", mod.sum$vars)
b <- mod.sum[b.rows, ]

com.biomass <- cbind(scx.com.biomass, b$median)
com.biomass <- com.biomass |> 
  dplyr::rename("Modelled_Com_Biomass" = "b$median")

com.biomass.plot <- ggplot(com.biomass)+
  geom_point(aes(x = I, y = Modelled_Com_Biomass))+
  geom_smooth(aes(x = I, y = Modelled_Com_Biomass), method = glm, se = FALSE)+
  geom_text_repel(aes(x = I, y = Modelled_Com_Biomass, label = YearSurvey), max.overlaps = 25)+
  xlab("Commercial Biomass Index")+ ylab("Modelled Commercial Biomass")+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -15, hjust = 0.05))
com.biomass.plot

ggsave(filename=paste0(direct,year,"/Assessment/Figures/Exploratory_plots/Com_ModelBM_BMIndex_SPA", area, "_",year,".png"), com.biomass.plot, dpi = 600, width = 10.5, height = 5.5)

# Recruit Biomass (input) vs Biomass (output) ------

scx.rec.biomass <- modfile %>% 
  select(YearSurvey, IR)

colnames(mod.sum) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")

# Extract biomass (recruits)
r.rows <- grep("^R\\[", mod.sum$vars)
r <- mod.sum[r.rows, ]

rec.biomass <- cbind(scx.rec.biomass, r$median)
rec.biomass <- rec.biomass |> 
  dplyr::rename("Modelled_Rec_Biomass" = "r$median")

#if(area == "1A"){ 
#  ylim <- c(0, 1700) 
#  xlim <- c(0, 550)}
#if(area == "1B"){ 
#  ylim <- c(0, 500) 
#  xlim <- c(0, 150)}
#if(area == "3"){ 
#  ylim <- c(0, 700) 
#  xlim <- c(0, 150)}
#if(area == "4"){ 
#  ylim <- c(0, 100) 
#  xlim <- c(0, 300)}
#if(area == "6"){ 
#  ylim <- c(0, 300) 
#  xlim <- c(0, 300)}

rec.biomass.plot <- ggplot(rec.biomass)+
  geom_point(aes(x = IR, y = Modelled_Rec_Biomass))+
  geom_smooth(aes(x = IR, y = Modelled_Rec_Biomass), method = glm, se = FALSE)+
  #coord_cartesian(xlim = xlim, ylim = ylim)+
  geom_text_repel(aes(x = IR, y = Modelled_Rec_Biomass, label = YearSurvey), max.overlaps = 50)+
  xlab("Recruit Biomass Index")+ ylab("Modelled Recruit Biomass")+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -10, hjust = 0.2))
rec.biomass.plot

ggsave(filename=paste0(direct,year,"/Assessment/Figures/Exploratory_plots/Rec_ModelBM_BMIndex_SPA", area, "_",year,".png"), rec.biomass.plot, dpi = 600, width = 10.5, height = 5.5)

#rec.biomass.plot.1A <- ggplot(rec.biomass %>% filter(YearSurvey != 2001))+
#  geom_point(aes(x = IR, y = Modelled_Rec_Biomass))+
#  geom_smooth(aes(x = IR, y = Modelled_Rec_Biomass), method = glm, se = FALSE)+
  #coord_cartesian(xlim = xlim, ylim = ylim)+
#  geom_text_repel(aes(x = IR, y = Modelled_Rec_Biomass, label = YearSurvey), max.overlaps = 50)+
#  xlab("Recruit Biomass Index")+ ylab("Modelled Recruit Biomass")+
#  labs(title = title)+
#  theme_bw()+
#  theme(plot.title = element_text(vjust = -15, hjust = 0.8))
#rec.biomass.plot.1A

#ggsave(filename=paste0("Y:/Inshore/BoF/2025/Assessment/Figures/Exploratory_plots/Rec_ModelBM_BMIndex_SPA", area, "_2025_rmOutlier.png"), rec.biomass.plot.1A, dpi = 600, width = 10.5, height = 5.5)

#rec.biomass.plot.4 <- ggplot(rec.biomass %>% filter(!YearSurvey %in% c(1988,1987,2001)))+
#  geom_point(aes(x = IR, y = Modelled_Rec_Biomass))+
#  geom_smooth(aes(x = IR, y = Modelled_Rec_Biomass), method = glm, se = FALSE)+
  #coord_cartesian(xlim = xlim, ylim = ylim)+
#  geom_text_repel(aes(x = IR, y = Modelled_Rec_Biomass, label = YearSurvey), max.overlaps = 50)+
#  xlab("Recruit Biomass Index")+ ylab("Modelled Recruit Biomass")+
#  labs(title = title)+
#  theme_bw()+
#  theme(plot.title = element_text(vjust = -10, hjust = 0.2))
#rec.biomass.plot.4

#ggsave(filename=paste0("Y:/Inshore/BoF/2025/Assessment/Figures/Exploratory_plots/Rec_ModelBM_BMIndex_SPA", area, "_2025_rmOutlier.png"), rec.biomass.plot.4, dpi = 600, width = 10.5, height = 5.5)

# Biomass (input) vs numbers (input) ------

scx.com.bm.num <- modfile %>% 
  select(YearSurvey, I, N)

bm.num.plot <- ggplot(scx.com.bm.num)+
  geom_point(aes(x = N, y = I))+
  geom_smooth(aes(x = N, y = I), method = glm, se = FALSE)+
  geom_text_repel(aes(x = N, y = I, label = YearSurvey), max.overlaps = 50)+
  xlab("Numbers Index")+ ylab("Biomass Index")+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -15, hjust = 0.05))
bm.num.plot

ggsave(filename=paste0(direct, year,"/Assessment/Figures/Exploratory_plots/Com_BM_Numbers_SPA", area, "_",year,".png"), bm.num.plot, dpi = 600, width = 10.5, height = 5.5)

#Modelled Commercial Biomass vs Modelled Recruit Biomass ----------

colnames(mod.sum) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")

#commercial Biomass
b.rows <- grep("^B\\[", mod.sum$vars)
b <- mod.sum[b.rows, ]

biomass <- as.data.frame(cbind(modfile$YearSurvey, b$median))
biomass <- biomass %>% 
  dplyr::rename("YearSurvey" = "V1") %>% 
  dplyr::rename("Modelled_Com_Biomass" = "V2")

#Recruit Biomass
r.rows <- grep("^R\\[", mod.sum$vars)
r <- mod.sum[r.rows, ]

biomass <- cbind(biomass, r$median)
biomass <- biomass |> 
  dplyr::rename("Modelled_Rec_Biomass" = "r$median")

modelled.com.rec.plot <- ggplot(biomass)+ 
  geom_point(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass))+
  geom_smooth(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass), method = glm, se = FALSE)+
  geom_text_repel(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass, label = YearSurvey), max.overlaps = 50)+
  xlab("Modelled Recruit Biomass")+ ylab("Modelled Commercial Biomass")+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -15, hjust = .8))
modelled.com.rec.plot

ggsave(filename=paste0(direct, year,"/Assessment/Figures/Exploratory_plots/ModBiomass_Com_Rec_SPA", area, "_",year,".png"), modelled.com.rec.plot, dpi = 600, width = 10.5, height = 5.5)

#modelled.com.rec.plot.1A <- ggplot(biomass %>% filter(YearSurvey != 2001))+ 
#  geom_point(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass))+
#  geom_smooth(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass), method = glm, se = FALSE)+
#  geom_text_repel(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass, label = YearSurvey), max.overlaps = 50)+
#  xlab("Modelled Recruit Biomass")+ ylab("Modelled Commercial Biomass")+
#  labs(title = title)+
#  theme_bw()+
#  theme(plot.title = element_text(vjust = -15, hjust = .8))
#modelled.com.rec.plot.1A

#ggsave(filename=paste0("Y:/Inshore/BoF/2025/Assessment/Figures/Exploratory_plots/ModBiomass_Com_Rec_SPA", area, "_2025_rmOutlier.png"), modelled.com.rec.plot.1A, dpi = 600, width = 10.5, height = 5.5)

#modelled.com.rec.plot.4 <- ggplot(biomass %>% filter(!YearSurvey %in% c(1987,1988,2001)))+ 
#  geom_point(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass))+
#  geom_smooth(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass), method = glm, se = FALSE)+
#  geom_text_repel(aes(x = Modelled_Rec_Biomass, y = Modelled_Com_Biomass, label = YearSurvey), max.overlaps = 50)+
#  xlab("Modelled Recruit Biomass")+ ylab("Modelled Commercial Biomass")+
#  labs(title = title)+
#  theme_bw()+
#  theme(plot.title = element_text(vjust = -15, hjust = .8))
#modelled.com.rec.plot.4

#ggsave(filename=paste0("Y:/Inshore/BoF/2025/Assessment/Figures/Exploratory_plots/ModBiomass_Com_Rec_SPA", area, "_2025_rmOutlier.png"), modelled.com.rec.plot.4, dpi = 600, width = 10.5, height = 5.5)

#Commercial Biomass Index vs Recruit Biomass Index ----------

biomass.idx <- modfile %>% 
  select(YearSurvey, I, IR)

biomass.idx.com.rec.plot <- ggplot(biomass.idx)+ # %>% filter(YearSurvey != 2001)
  geom_point(aes(x = IR, y = I))+
  geom_smooth(aes(x = IR, y = I), method = glm, se = FALSE)+
  geom_text_repel(aes(x = IR, y = I, label = YearSurvey), max.overlaps = 50)+
  xlab("Recruit Biomass Index")+ ylab("Commercial Biomass Index")+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -15, hjust = .8))
biomass.idx.com.rec.plot

ggsave(filename=paste0(direct, year,"/Assessment/Figures/Exploratory_plots/Biomass_Com_Rec_Index_SPA", area, "_",year,".png"), biomass.idx.com.rec.plot, dpi = 600, width = 10.5, height = 5.5)

#ggsave(filename=paste0("Y:/Inshore/BoF/2025/Assessment/Figures/Exploratory_plots/Biomass_Com_Rec_Index_SPA", area, "_2025_rmOutlier.png"), biomass.idx.com.rec.plot, dpi = 600, width = 10.5, height = 5.5)


#Modelled Recruitment Biomass vs Natural Mortality ----------

colnames(mod.sum) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")

#Recruit Biomass
r.rows <- grep("^R\\[", mod.sum$vars)
r <- mod.sum[r.rows, ]

biomass.rec <- as.data.frame(cbind(modfile$YearSurvey, r$median))
biomass.rec <- biomass.rec %>% 
  dplyr::rename("YearSurvey" = "V1") %>% 
  dplyr::rename("Modelled_Rec_Biomass" = "V2")

#nat mort
m.rows <- grep("^m\\[", mod.sum$vars)
m <- mod.sum[m.rows, ]
# convert inst mort to prop mort
m.prop <- m
m.prop[2:8] <- signif(1-exp(-m.prop[2:8]),2)

rec.bio.mort <- cbind(biomass.rec, m.prop$median)
rec.bio.mort <- rec.bio.mort |> 
  dplyr::rename("Nat_mort" = "m.prop$median")


mod.rec.mort.plot <- ggplot(rec.bio.mort)+ 
  geom_point(aes(x = Nat_mort, y = Modelled_Rec_Biomass))+
  geom_smooth(aes(x = Nat_mort, y = Modelled_Rec_Biomass), method = glm, se = FALSE)+
  geom_text_repel(aes(x = Nat_mort, y = Modelled_Rec_Biomass, label = YearSurvey), max.overlaps = 50)+
  xlab("Natural Mortality (proportional rate)")+ ylab("Modelled Recruit Biomass")+
  labs(title = title)+
  theme_bw()+
  theme(plot.title = element_text(vjust = -15, hjust = .8))
mod.rec.mort.plot

ggsave(filename=paste0(direct, year,"/Assessment/Figures/Exploratory_plots/Mod_rec_Biomass_NatMort_SPA", area, "_",year,".png"), mod.rec.mort.plot, dpi = 600, width = 10.5, height = 5.5)

#mod.rec.mort.plot.4 <- ggplot(rec.bio.mort %>% filter(YearSurvey != c(1989,1990,1991)))+ 
#  geom_point(aes(x = Nat_mort, y = Modelled_Rec_Biomass))+
#  geom_smooth(aes(x = Nat_mort, y = Modelled_Rec_Biomass), method = glm, se = FALSE)+
#  geom_text_repel(aes(x = Nat_mort, y = Modelled_Rec_Biomass, label = YearSurvey), max.overlaps = 50)+
#  xlab("Natural Mortality (proportional rate)")+ ylab("Modelled Recruit Biomass")+
#  labs(title = title)+
#  theme_bw()+
#  theme(plot.title = element_text(vjust = -15, hjust = .8))
#mod.rec.mort.plot.4

#ggsave(filename=paste0("Y:/Inshore/BoF/2025/Assessment/Figures/Exploratory_plots/Mod_rec_Biomass_NatMort_SPA", area, "_2025_rmOutlier.png"), mod.rec.mort.plot.4, dpi = 600, width = 10.5, height = 5.5)


