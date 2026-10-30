
## Relative trend of abundance, biomass and condition all scaled to 1 (max observed in time series)
## as first shown in 2024 BoF SR Update https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/4122484x.pdf
## J.Sameoto 

library(tidyverse)
library(dplyr)
library(reshape2)
library(cowplot)
library(gridExtra)
library(grid)
library(gridtext)
#install.packages("xfun")
library(xfun)

assessmentyear <- 2026

direct <- "Y:/Inshore/Assessment/BoF/"


# SPA 1A ---- 
numbers.1A <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA1A.population.model.input.",assessmentyear,".csv"))
numbers.1A <- numbers.1A %>% dplyr::select(Year,  N) 

weight.1A <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA1A.population.model.input.",assessmentyear,".csv"))
weight.1A <- weight.1A  %>%  dplyr::select(Year,  I) 

condition.1A <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BoF_ConditionTimeSeries.csv"))
                         
condition.1A <- condition.1A %>% filter(STRATA == "SPA1A")
condition.1A
#only since 1997 

#merge
all.dat.1A <- cbind(condition.1A, numbers.1A , weight.1A )

#clean up fields 
all.dat.1A <- all.dat.1A %>% dplyr::select(YEAR, CONDITION,  N, I)

all.dat.1A$Condition <- all.dat.1A$CONDITION/max(all.dat.1A$CONDITION, na.rm = TRUE)
all.dat.1A$Abundance <- all.dat.1A$N/max(all.dat.1A$N, na.rm = TRUE)
all.dat.1A$Biomass <- all.dat.1A$I/max(all.dat.1A$I, na.rm = TRUE)
all.dat.1A

#long format 
all.dat.1A.long <- melt(all.dat.1A,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("YEAR"),
                       # The source columns
                       measure.vars=c("Condition", "Abundance","Biomass" ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="index",
                       value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.1A.long$value[all.dat.1A.long$YEAR == 2020] <- NA

all.dat.1A.long

all.dat.1A.long$SPA <- "SPA1A"


#condition, biomass, number time series as proportion of the maximum observed 
plot.1A <- ggplot(all.dat.1A.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index) + 
  ylab("SPA 1A") + 
  xlab("") + 
  theme_bw()
plot.1A

# SPA1A PLOT FRENCH VERSION

all.dat.1A.long.FR <- all.dat.1A.long
all.dat.1A.long.FR$SPA <- "ZPrP1A"
all.dat.1A.long.FR <- all.dat.1A.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.1A.long.FR$index.FR <- factor(all.dat.1A.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.1A.FR <- ggplot(all.dat.1A.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index.FR) + 
  ylab("ZPrP 1A") + 
  xlab("") + 
  theme_bw()
plot.1A.FR


# SPA 1B ---- 
numbers.1B <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA1B.population.model.input.",assessmentyear,".csv"))
numbers.1B <- numbers.1B %>% dplyr::select(Year,  N) 

weight.1B <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA1B.population.model.input.",assessmentyear,".csv"))
weight.1B <- weight.1B  %>%  dplyr::select(Year,  I) 

condition.1B <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BoF_ConditionTimeSeries.csv"))
condition.1B <- condition.1B %>% filter(STRATA == "SPA1B")
condition.1B
#only since 1997 

#merge
all.dat.1B <- cbind(condition.1B, numbers.1B , weight.1B)

#clearn up fields 
all.dat.1B <- all.dat.1B %>% dplyr::select(YEAR, CONDITION,  N, I)

all.dat.1B$Condition <- all.dat.1B$CONDITION/max(all.dat.1B$CONDITION, na.rm = TRUE)
all.dat.1B$Abundance <- all.dat.1B$N/max(all.dat.1B$N, na.rm = TRUE)
all.dat.1B$Biomass <- all.dat.1B$I/max(all.dat.1B$I, na.rm = TRUE)
all.dat.1B

#long format 
all.dat.1B.long <- melt(all.dat.1B,
                        # ID variables - all the variables to keep but not split apart on
                        id.vars=c("YEAR"),
                        # The source columns
                        measure.vars=c("Condition", "Abundance","Biomass" ),
                        # Name of the destination column that will identify the original
                        # column that the measurement came from
                        variable.name="index",
                        value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.1B.long$value[all.dat.1B.long$YEAR == 2020] <- NA

all.dat.1B.long

all.dat.1B.long$SPA <- "SPA1B"



#condition, biomass, number time series as proportion of the mximum observed 
plot.1B <- ggplot(all.dat.1B.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index) + 
  ylab("SPA 1B") + #Proportion of maximum observed
  xlab("") + 
  theme_bw()
plot.1B


# SPA1B PLOT FRENCH VERSION
all.dat.1B.long.FR <- all.dat.1B.long
all.dat.1B.long.FR$SPA <- "ZPrP1B"
all.dat.1B.long.FR <- all.dat.1B.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.1B.long.FR$index.FR <- factor(all.dat.1B.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.1B.FR <- ggplot(all.dat.1B.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index.FR) + 
  ylab("ZPrP 1B") + 
  xlab("") + 
  theme_bw()
plot.1B.FR


# SPA 3 ---- 
dat.3 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/SPA3.population.model.input.",assessmentyear,".csv"))
condition.3 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/SPA3_ConditionTimeSeries.csv"))

lbar.3 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/Growth/SPA3/SPA3.lbar.to",assessmentyear,".csv"))

condition.3 <- condition.3 %>% filter(STRATA == "InVMS_SMB")
condition.3

lbar.3 <- lbar.3 %>% dplyr::select(years,  SPA3.SHactual.Com)
lbar.3

numbers.3 <- dat.3 %>% dplyr::select(Year,  N) 
numbers.3

weight.3 <- dat.3 %>% dplyr::select(Year,  I) 
weight.3


all.dat.3 <- cbind(condition.3,lbar.3, numbers.3, weight.3 )

all.dat.3 <- all.dat.3 %>% dplyr::select(YEAR, CONDITION,  N, I)

all.dat.3$Condition <- all.dat.3$CONDITION/max(all.dat.3$CONDITION, na.rm = TRUE)
all.dat.3$Abundance <- all.dat.3$N/max(all.dat.3$N, na.rm = TRUE)
all.dat.3$Biomass <- all.dat.3$I/max(all.dat.3$I, na.rm = TRUE)
all.dat.3



all.dat.3.long <- melt(all.dat.3,
     # ID variables - all the variables to keep but not split apart on
     id.vars=c("YEAR"),
     # The source columns
     measure.vars=c("Condition", "Abundance","Biomass" ),
     # Name of the destination column that will identify the original
     # column that the measurement came from
     variable.name="index",
     value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.3.long$value[all.dat.3.long$YEAR == 2020] <- NA

all.dat.3.long$SPA <- "SPA3"


#condition, biomass, number time series as proportion of the mximum observed 
plot.3 <- ggplot(all.dat.3.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) + 
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index) + 
  ylab("SPA 3") + 
  xlab("") + 
  theme_bw()
plot.3


# SPA3 PLOT FRENCH VERSION
all.dat.3.long.FR <- all.dat.3.long
all.dat.3.long.FR$SPA <- "ZPrP3"
all.dat.3.long.FR <- all.dat.3.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.3.long.FR$index.FR <- factor(all.dat.3.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.3.FR <- ggplot(all.dat.3.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) + 
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index.FR) + 
  ylab("ZPrP 3") + 
  xlab("") + 
  theme_bw()
plot.3.FR


# SPA 4 ---- 
numbers.4 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA4.Index.Numbers.",assessmentyear,".csv"))
numbers.4 <- numbers.4 %>% filter(Age == "Commercial") %>% dplyr::select(Year,  Mean.nums) 

weight.4 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/SPA4.Index.Weight.",assessmentyear,".csv"))
weight.4 <- weight.4  %>% filter(Age == "Commercial") %>% dplyr::select(Year,  Mean.wt) 
 
condition.4 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BoF_ConditionTimeSeries.csv"))

condition.4 <- condition.4 %>% filter(STRATA == "SPA4")
condition.4
#only since 1997 

#merge
all.dat.4 <- cbind(condition.4, numbers.4 %>% filter(Year >= 1997), weight.4 %>% filter(Year >= 1997 ))

#clearn up fields 
all.dat.4 <- all.dat.4 %>% dplyr::select(YEAR, CONDITION,  Mean.nums, Mean.wt)

all.dat.4$Condition <- all.dat.4$CONDITION/max(all.dat.4$CONDITION, na.rm = TRUE)
all.dat.4$Abundance <- all.dat.4$Mean.nums/max(all.dat.4$Mean.nums, na.rm = TRUE)
all.dat.4$Biomass <- all.dat.4$Mean.wt/max(all.dat.4$Mean.wt, na.rm = TRUE)
all.dat.4

#long format 
all.dat.4.long <- melt(all.dat.4,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("YEAR"),
                       # The source columns
                       measure.vars=c("Condition", "Abundance","Biomass" ),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="index",
                       value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.4.long$value[all.dat.4.long$YEAR == 2020] <- NA

all.dat.4.long

all.dat.4.long$SPA <- "SPA4"

#condition, biomass, number time series as proportion of the mximum observed 
plot.4 <- ggplot(all.dat.4.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index) + 
  ylab("SPA 4") + 
  xlab("") + 
  theme_bw()
plot.4


# SPA4 PLOT FRENCH VERSION
all.dat.4.long.FR <- all.dat.4.long
all.dat.4.long.FR$SPA <- "ZPrP4"
all.dat.4.long.FR <- all.dat.4.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.4.long.FR$index.FR <- factor(all.dat.4.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.4.FR <- ggplot(all.dat.4.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index.FR) + 
  ylab("ZPrP 4") + 
  xlab("") + 
  theme_bw()
plot.4.FR


# SPA 6 ---- 
#limit time series from 2006 on 

numbers.6 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/SPA6.Index.Numbers.IN.",assessmentyear,".csv"))
numbers.6 <- numbers.6 %>% filter(Age == "Commercial" & Year >= 2006) %>% dplyr::select(Year,   Mean.nums ) 

weight.6 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/SPA6.Index.Weight.IN.",assessmentyear,".csv"))
weight.6 <- weight.6  %>% filter(Age == "Commercial" & Year >= 2006) %>%  dplyr::select(Year,  Mean.wt)

condition.6 <- read.csv(paste0(direct,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/SPA6_ConditionTimeSeries.csv"))
condition.6 <- condition.6 %>% filter(STRATA == "INVMS" & YEAR >= 2006)
condition.6 




#merge
all.dat.6<- cbind(condition.6, numbers.6 , weight.6 )

#clean up fields 
all.dat.6 <- all.dat.6 %>% dplyr::select(YEAR, CONDITION,  Mean.nums , Mean.wt)

all.dat.6$Condition <- all.dat.6$CONDITION/max(all.dat.6$CONDITION, na.rm = TRUE)
all.dat.6$Abundance <- all.dat.6$Mean.nums/max(all.dat.6$Mean.nums, na.rm = TRUE)
all.dat.6$Biomass <- all.dat.6$Mean.wt/max(all.dat.6$Mean.wt, na.rm = TRUE)
all.dat.6

#long format 
all.dat.6.long <- melt(all.dat.6,
                        # ID variables - all the variables to keep but not split apart on
                        id.vars=c("YEAR"),
                        # The source columns
                        measure.vars=c("Condition", "Abundance","Biomass" ),
                        # Name of the destination column that will identify the original
                        # column that the measurement came from
                        variable.name="index",
                        value.name="value"
)

#remove 2020 - set as NA since no survey that year 
all.dat.6.long$value[all.dat.6.long$YEAR == 2020] <- NA

all.dat.6.long

all.dat.6.long$SPA <- "SPA6"

#condition, biomass, number time series as proportion of the mximum observed 
plot.6 <- ggplot(all.dat.6.long, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) +  
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index) + 
  ylab("SPA 6") + 
  xlab("") + 
  theme_bw()
plot.6


# SPA6 PLOT FRENCH VERSION
all.dat.6.long.FR <- all.dat.6.long
all.dat.6.long.FR$SPA <- "ZPrP6"
all.dat.6.long.FR <- all.dat.6.long.FR |> 
  mutate(index.FR = case_when(index == "Condition" ~ "Condition",
                              index == "Biomass" ~ "Biomasse",
                              index == "Abundance" ~ "Abondance")) 
all.dat.6.long.FR$index.FR <- factor(all.dat.6.long.FR$index.FR, levels=c("Condition", "Abondance", "Biomasse")) 

#condition, biomass, number time series as proportion of the maximum observed 
plot.6.FR <- ggplot(all.dat.6.long.FR, aes(x = YEAR, y = value)) + 
  geom_point(size = 1) + 
  geom_line() + 
  scale_y_continuous(breaks=seq(0, 1, 0.25), limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1995, assessmentyear, 5), limits = c(1995,assessmentyear))+
  facet_wrap(~index.FR) + 
  ylab("ZPrP 6") + 
  xlab("") + 
  theme_bw()
plot.6.FR

# PLOT ALL ON SAME FIGURE 
library(cowplot)
plot_grid(plot.1A, plot.1B, plot.3, plot.4, plot.6, ncol=1)

#want only single y axis label 
#need to label each row with SPA.. 

#ENGLISH VERSION  rot=90,
# gridtext
#yleft <- richtext_grob(text = c("Proportion of maximum observation"), rot=90, gp = gpar(fontsize = 12))
yleft <- grid.text("Proportion of maximum observation",  rot=90, gp=gpar(fontsize=12, col="black"))

bottom <-  richtext_grob(text = 'Year', gp = gpar(fontsize = 14))

p.eng <- list(plot.1A, plot.1B, plot.3, plot.4, plot.6)

# Lay out plots
plot.eng <- grid.arrange(grobs=p.eng, ncol = 1,
                     left = yleft, bottom = bottom)

grid.draw(plot.eng)

##Save out 
ggsave(file=paste0(direct,assessmentyear,"/Assessment/Figures/ConditionNumberWeight_BySPA_Proportion.png"), plot.eng, width = 20, height = 20, units = "cm", dpi = 300)


#FRENCH VERSION
plot_grid(plot.1A.FR, plot.1B.FR, plot.3.FR, plot.4.FR, plot.6.FR, ncol=1)

#yleft <- richtext_grob(text = "Proportion de l'observation maximale", rot=90, gp = gpar(fontsize = 14))
yleft <- grid.text("Proportion de l'observation maximale",  rot=90, gp=gpar(fontsize=12, col="black"))

bottom <-  richtext_grob(text = 'Année', gp = gpar(fontsize = 14))

p <- list(plot.1A.FR, plot.1B.FR, plot.3.FR, plot.4.FR, plot.6.FR)

# Lay out plots
plot.fr <- grid.arrange(grobs=p, ncol = 1,
                    left = yleft, bottom = bottom)
grid.draw(plot.fr)

ggsave(file=paste0(direct,assessmentyear,"/Assessment/Figures/ConditionNumberWeight_BySPA_Proportion_FR.png"), plot.fr, width = 20, height = 20, units = "cm", dpi = 300)


#Save using RStudio GUI 
#size 666 x 868 for document (ConditionNumberWeight_BySPA.jpeg and ConditionNumberWeight_BySPA_FR.jpeg)
#size 895 x 763 for presentation (ConditionNumberWeight_BySPA_forPres.jpeg)


#FOR FRENCH VERSION: From Raphael - Proportion du maximum observé? Proportion de l'observation maximale, the first one is the more direct translation, and the second is like saying Proportion of the maximum observation.


##### change in condition as percent #####

YR <- 2026

## 1A
con.yr.t <- condition.1A[condition.1A$YEAR == YR,]
con.yr.tminus1 <- condition.1A[condition.1A$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# -5.6974


## 1B
con.yr.t <- condition.1B[condition.1B$YEAR == YR,]
con.yr.tminus1 <- condition.1B[condition.1B$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
#  -12.9719

## 3 
con.yr.t <- condition.3[condition.3$YEAR == YR,]
con.yr.tminus1 <- condition.3[condition.3$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# -1.154918

## 4 
con.yr.t <- condition.4[condition.4$YEAR == YR,]
con.yr.tminus1 <- condition.4[condition.4$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# 13.06435

## 6 
con.yr.t <- condition.6[condition.6$YEAR == YR,]
con.yr.tminus1 <- condition.6[condition.6$YEAR == YR-1,]
((con.yr.tminus1$CONDITION - con.yr.t$CONDITION) / (con.yr.tminus1$CONDITION))*100
# 29.93796






##### change in biomass index as percent #####

YR <- 2025

## 1A
B.yr.t <- all.dat.1A[all.dat.1A$YEAR == YR,]
B.yr.tminus1 <- all.dat.1A[all.dat.1A$YEAR == YR-1,]
((B.yr.tminus1$Biomass - B.yr.t$Biomass) / (B.yr.tminus1$Biomass))*100
# 23.64881


## 1B
B.yr.t <- all.dat.1B[all.dat.1B$YEAR == YR,]
B.yr.tminus1 <- all.dat.1B[all.dat.1B$YEAR == YR-1,]
((B.yr.tminus1$Biomass - B.yr.t$Biomass) / (B.yr.tminus1$Biomass))*100
#  15.80261

## 3 
B.yr.t <- all.dat.3[all.dat.3$YEAR == YR,]
B.yr.tminus1 <- all.dat.3[all.dat.3$YEAR == YR-1,]
((B.yr.tminus1$Biomass - B.yr.t$Biomass) / (B.yr.tminus1$Biomass))*100
#  33.81979

## 4 
B.yr.t <- all.dat.4[all.dat.4$YEAR == YR,]
B.yr.tminus1 <- all.dat.4[all.dat.4$YEAR == YR-1,]
((B.yr.tminus1$Biomass - B.yr.t$Biomass) / (B.yr.tminus1$Biomass))*100
# 38.78101

## 6 
B.yr.t <- all.dat.6[all.dat.6$YEAR == YR,]
B.yr.tminus1 <- all.dat.6[all.dat.6$YEAR == YR-1,]
((B.yr.tminus1$Biomass - B.yr.t$Biomass) / (B.yr.tminus1$Biomass))*100
#45.93264

# range in biomass index: 15 - 46%



##### change in Modelled Biomass as percent #####

setwd(paste0("Y:/Inshore/Assessment/BoF/",assessment.year,"/Assessment/Data/Model/"))
#setwd("Y:/Inshore/BoF/2024/Assessment/Data/Model/")

#current/most recent year 
YR <- 2024

B.1A <- read.csv(paste0("SPA1A/summary stats_1A_",YR,".csv"))
B.1B <- read.csv(paste0("SPA1B/summary stats_1B_",YR,".csv"))
B.3 <- read.csv(paste0("SPA3/summary stats_3_",YR,".csv"))
B.4 <- read.csv(paste0("SPA4/summary stats_4_",YR,".csv"))
B.6 <- read.csv(paste0("SPA6/summary stats_6_",YR,".csv"))

                 
                 
## 1A
B.1A <- B.1A[B.1A$description  == "comm.biomass",]
yr.t <- B.1A[,6]
yr.t
yr.tminus1 <-  B.1A[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
# 44.45334

## 1B
B.1B <- B.1B[B.1B$description  == "comm.biomass",]
yr.t <- B.1B[,6]
yr.t
yr.tminus1 <-  B.1B[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#42.00945


## 3 
B.3 <- B.3[B.3$description  == "comm.biomass",]
yr.t <- B.3[,6]
yr.t
yr.tminus1 <-  B.3[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#32.62144


## 4 
B.4 <- B.4[B.4$description  == "comm.biomass",]
yr.t <- B.4[,6]
yr.t
yr.tminus1 <-  B.4[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#41.86935


## 6 
B.6 <- B.6[B.6$description  == "comm.biomass",]
yr.t <- B.6[,6]
yr.t
yr.tminus1 <-  B.6[,3]
yr.tminus1
((yr.tminus1 - yr.t) / (yr.tminus1))*100
#32.60546


# range in modelled biomass: 33 - 44%


