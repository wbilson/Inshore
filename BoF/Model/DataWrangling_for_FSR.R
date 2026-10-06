

#Write files for FSR -------------------------

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(openxlsx)

assessment.year <- 2026

direct <- "Y:/Inshore/Assessment/BoF/"


#PROPORTIONAL NATRUAL MORTALITY -------------------------

#SPA1A---------------------------

    modfile.1A <- read.xlsx(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA1A/SPA1A_ModelData_R_2026-09-11.xlsx"),sheet = "AlignedForModel", cols=1:13) #CHECK FILE NAME
    mod.sum.1A <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA1A/Spa1AModelOutput.csv"))
    
    colnames(mod.sum.1A) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
    m.rows.1A <- grep("^m\\[", mod.sum.1A$vars)
    m.1A <- mod.sum.1A[m.rows.1A, ]
    # convert inst mort to prop mort
    m.prop.1A <- m.1A
    m.prop.1A[2:8] <- signif(1-exp(-m.prop.1A[2:8]),2)
    
    years.1A <- modfile.1A %>%
      select(YearSurvey)
    
    mort.1A <- cbind(years.1A, m.prop.1A$median)
    mort.1A <- mort.1A |>
      dplyr::rename("m.prop" = "m.prop.1A$median") %>% 
      mutate(Area = "SPA1A")
    
#SPA1B---------------------------   
  
    modfile.1B <- read.xlsx(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA1B/SPA1B_ModelData_R_2026-09-11.xlsx"),sheet = "AlignedForModel", cols=1:13)#CHECK FILE NAME
    mod.sum.1B <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA1B/Spa1BModelOutput.csv"))
  
    colnames(mod.sum.1B) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
    m.rows.1B <- grep("^m\\[", mod.sum.1B$vars)
    m.1B <- mod.sum.1B[m.rows.1B, ]
    # convert inst mort to prop mort
    m.prop.1B <- m.1B
    m.prop.1B[2:8] <- signif(1-exp(-m.prop.1B[2:8]),2)
    
    years.1B <- modfile.1B %>%
      select(YearSurvey)
    
    mort.1B <- cbind(years.1B, m.prop.1B$median)
    mort.1B <- mort.1B |>
      dplyr::rename("m.prop" = "m.prop.1B$median") %>% 
      mutate(Area = "SPA1B")
    
  #SPA3---------------------------      
    
    modfile.3 <- read.xlsx(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA3/SPA3_ModelData_R_2026-09-15.xlsx"),sheet = "AlignedForModel", cols=1:13)#CHECK FILE NAME
    mod.sum.3 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA3/Spa3ModelOutput.csv"))
    
    colnames(mod.sum.3) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
    m.rows.3 <- grep("^m\\[", mod.sum.3$vars)
    m.3 <- mod.sum.3[m.rows.3, ]
    # convert inst mort to prop mort
    m.prop.3 <- m.3
    m.prop.3[2:8] <- signif(1-exp(-m.prop.3[2:8]),2)
    
    years.3 <- modfile.3 %>%
      select(YearSurvey)
    
    mort.3 <- cbind(years.3, m.prop.3$median)
    mort.3 <- mort.3 |>
      dplyr::rename("m.prop" = "m.prop.3$median") %>% 
      mutate(Area = "SPA3")
    
  #SPA4--------------------------- 
    
    modfile.4 <- read.xlsx(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA4/SPA4_ModelData_R_2026-09-15.xlsx"),sheet = "AlignedForModel", cols=1:13)#CHECK FILE NAME
    mod.sum.4 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA4/Spa4ModelOutput.csv"))
    
    colnames(mod.sum.4) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
    m.rows.4 <- grep("^m\\[", mod.sum.4$vars)
    m.4 <- mod.sum.4[m.rows.4, ]
    # convert inst mort to prop mort
    m.prop.4 <- m.4
    m.prop.4[2:8] <- signif(1-exp(-m.prop.4[2:8]),2)
    
    years.4 <- modfile.4 %>%
      select(YearSurvey)
    
    mort.4 <- cbind(years.4, m.prop.4$median)
    mort.4 <- mort.4 |>
      dplyr::rename("m.prop" = "m.prop.4$median") %>% 
      mutate(Area = "SPA4")
    
#SPA6---------------------------  

    modfile.6 <- read.xlsx(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA6/SPA6_ModelData_R_2026-09-16.xlsx"),sheet = "AlignedForModel", cols=1:13)#CHECK FILE NAME
    mod.sum.6 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA6/Spa6ModelOutput.csv"))
  
    colnames(mod.sum.6) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")
    m.rows.6 <- grep("^m\\[", mod.sum.6$vars)
    m.6 <- mod.sum.6[m.rows.6, ]
    # convert inst mort to prop mort
    m.prop.6 <- m.6
    m.prop.6[2:8] <- signif(1-exp(-m.prop.6[2:8]),2)
    
    years.6 <- modfile.6 %>%
      select(YearSurvey)
    
    mort.6 <- cbind(years.6, m.prop.6$median)
    mort.6 <- mort.6 |>
      dplyr::rename("m.prop" = "m.prop.6$median") %>% 
      mutate(Area = "SPA6")
  
#Combine--------------------------------------------
  
all.mort <- rbind(mort.1A, mort.1B, mort.3, mort.4, mort.6)
    
write.csv(all.mort, paste0(direct,assessment.year,"/Assessment/Data/Model/BoF_Proportional_Natural_Mortality_",assessment.year,".csv"), row.names = F)
    
###########################################################################################################################################################

#EXPLOITATION -------------------------

#SPA1A---------------------------

mu.1A <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA1A/spa1aModelOutput.csv"), header=T)
mu.1A <- mu.1A %>%
  filter(str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X) %>% 
  mutate(Area = "SPA1A")
mu.1A$X <- c((1997):(1996+nrow(mu.1A)))

#range(mu.1A %>% filter(X %in% 2015:2025) %>% select(X50.) %>% pull())

#SPA1B---------------------------   

mu.1B <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA1B/spa1BModelOutput.csv"), header=T)
mu.1B <- mu.1B %>%
  filter(str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)%>% 
  mutate(Area = "SPA1B")
mu.1B$X <- c((1997):(1996+nrow(mu.1B)))

#range(mu.1B %>% filter(X %in% 2015:2025) %>% select(X50.) %>% pull())

#SPA3---------------------------      

mu.3 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA3/spa3ModelOutput.csv"), header=T)
mu.3 <- mu.3 %>%
  filter(str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)%>% 
  mutate(Area = "SPA3")
mu.3$X <- c((1996):(1995+nrow(mu.3)))

#range(mu.3 %>% filter(X %in% 2015:2025) %>% select(X50.) %>% pull())

#SPA4--------------------------- 

mu.4 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA4/spa4ModelOutput.csv"), header=T)
mu.4 <- mu.4 %>%
  filter(str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)%>% 
  mutate(Area = "SPA4")
mu.4$X <- c((1983):(1982+nrow(mu.4)))

#range(mu.4 %>% filter(X %in% 2015:2025) %>% select(X50.) %>% pull())

#SPA6---------------------------  

mu.6 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/Model/SPA6/spa6ModelOutput.csv"), header=T)
mu.6 <- mu.6 %>%
  filter(str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)%>% 
  mutate(Area = "SPA6")
mu.6$X <- c((2006):(2005+nrow(mu.6)))

#range(mu.6 %>% filter(X %in% 2015:2025) %>% select(X50.) %>% pull())

#Combine--------------------------------------------

all.mu <- rbind(mu.1A, mu.1B, mu.3, mu.4, mu.6)

write.csv(all.mu, paste0(direct,assessment.year,"/Assessment/Data/Model/BoF_Exploitation_timeseries_",assessment.year,".csv"), row.names = F)

###############################################################################################################################################################

##### change in condition as percent #####

condition.1A <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BoF_ConditionTimeSeries.csv"))

condition.1A <- condition.1A %>% filter(STRATA == "SPA1A") %>% mutate(AREA = "SPA1A")
condition.1A
#median(condition.1A %>% filter(YEAR != 2020) %>% select(CONDITION) %>% pull())

condition.1B <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BoF_ConditionTimeSeries.csv"))
condition.1B <- condition.1B %>% filter(STRATA == "SPA1B") %>% mutate(AREA = "SPA1B")
condition.1B
#median(condition.1B %>% filter(YEAR != 2020) %>% select(CONDITION) %>% pull())

condition.3 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/SurveyIndices/SPA3/SPA3_ConditionTimeSeries.csv"))
condition.3 <- condition.3 %>% filter(STRATA == "InVMS_SMB") %>% select(!X) %>% mutate(AREA = "SPA3")
condition.3
#median(condition.3 %>% filter(YEAR != 2020) %>% select(CONDITION) %>% pull())

condition.4 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BoF_ConditionTimeSeries.csv"))
condition.4 <- condition.4 %>% filter(STRATA == "SPA4") %>% mutate(AREA = "SPA4")
condition.4
#median(condition.4 %>% filter(YEAR != 2020) %>% select(CONDITION) %>% pull())

condition.6 <- read.csv(paste0(direct,assessment.year,"/Assessment/Data/SurveyIndices/SPA6/SPA6_ConditionTimeSeries.csv"))
condition.6 <- condition.6 %>% filter(STRATA == "INVMS" & YEAR >= 2006) %>% select(!X) %>% mutate(AREA = "SPA6")
condition.6
#median(condition.6 %>% filter(YEAR != 2020) %>% select(CONDITION) %>% pull())

condition.all <- rbind(condition.1A, condition.1B, condition.3, condition.4, condition.6)

write.csv(condition.all, paste0(direct,assessment.year,"/Assessment/Data/Model/BoF_condition_timeseries_",assessment.year,".csv"), row.names = F)

