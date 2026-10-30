#Distribution plots - Model summary
library(tidyverse)
library(ggplot2)
library(ggridges)
library(readr)


#define:
dir <- "Y:/Inshore/Assessment/BoF/"
#dir <- "Y:/Inshore/BoF/"
dir.area <-  "SPA6"#"SPA3" #"SPA4", "SPA6", "SPA1A", "SPA1B"
file.area <- "spa6"#"spa3" #"spa4", "spa6", "spa1A", "spa1B"

#year ranges
if(dir.area == "SPA1A"){
  start.yr <- 1996
}
if(dir.area == "SPA1B") {
  start.yr <- 1996
}
if(dir.area == "SPA3") {
  start.yr <- 1995
}
if(dir.area == "SPA4") {
  start.yr <- 1982
}
if(dir.area == "SPA6") {
  start.yr <- 2005
}

# Median Biomass estimates -----------------------------------------------

#Load model summaries and format for plotting

#2026
sum.2026 <- read.csv(paste0(dir, "2026/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2026$Year <- 2026
sum.2026 <- sum.2026 %>% 
  filter(str_detect(X, "B")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2026$X <- c((start.yr+1):(start.yr+nrow(sum.2026)))

#2025
sum.2025 <- read.csv(paste0(dir, "2025/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2025$Year <- 2025
sum.2025 <- sum.2025 %>% 
  filter(str_detect(X, "B")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2025$X <- c((start.yr+1):(start.yr+nrow(sum.2025)))

#2024
sum.2024 <- read.csv(paste0(dir, "2024/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2024$Year <- 2024
sum.2024 <- sum.2024 %>% 
  filter(str_detect(X, "B")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2024$X <- c((start.yr+1):(start.yr+nrow(sum.2024)))

#2023
sum.2023 <- read.csv(paste0(dir, "2023/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2023$Year <- 2023
sum.2023 <- sum.2023 %>% 
  filter(str_detect(X, "B")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2023$X <- c((start.yr+1):(start.yr+nrow(sum.2023)))

#2022
sum.2022 <- read.csv(paste0(dir, "2022/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2022$Year <- 2022
sum.2022 <- sum.2022 %>% 
  filter(str_detect(X, "B")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2022$X <- c((start.yr+1):(start.yr+nrow(sum.2022)))
  
#2021
sum.2021 <- read.csv(paste0(dir, "2021/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2021$Year <- 2021
sum.2021 <- sum.2021 %>% 
  filter(str_detect(X, "B")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2021$X <- c((start.yr+1):(start.yr+nrow(sum.2021)))

#2020
#sum.2020 <- read.csv(paste0(dir, "2020/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
#sum.2020$Year <- 2020
#sum.2020 <- sum.2020 %>% 
#  filter(str_detect(X, "B")) %>% 
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2020$X <- c((start.yr+1):(start.yr+nrow(sum.2020)))

#2018
#sum.2018 <- read.csv(paste0(dir, "2018/2018 Assessment/",dir.area,"/ModelOutput/",file.area,"ModelOutput.csv"), header=T)
#sum.2018$Year <- 2018
#sum.2018 <- sum.2018 %>% 
#  filter(str_detect(X, "B")) %>% 
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2018$X <- c((start.yr+1):(start.yr+nrow(sum.2018)))

mod.sum <- rbind(sum.2021, sum.2022, sum.2023, sum.2024, sum.2025, sum.2026) #sum.2018, sum.2020, 
mod.sum$Year <- as.factor(mod.sum$Year)

#Plots

#Median Biomass estimates
ggplot(data = mod.sum, aes(x = X, y = X50., colour = Year, group = Year))+
  geom_ribbon(aes(ymin=X2.5., ymax=X97.5., fill = Year, color = Year, linetype = Year),alpha=0.2, linewidth = 0.1)+
  geom_point()+
  geom_line()+
  labs(x = "Year", y = "Median Commercial Biomass Estimates (mt)")

# Median Recruit Biomass estimates (R) -----------------------------------------------

#Load model summaries and format for plotting

#2026
sum.2026 <- read.csv(paste0(dir, "2026/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2026$Year <- 2026
sum.2026 <- sum.2026 %>% 
  filter(str_detect(X,"R")) %>% 
  filter(!str_detect(X,"IRrep")) %>% 
  filter(!str_detect(X,"IRresid")) %>%
  filter(!str_detect(X,"sIRresid")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2026$X <- c((start.yr+1):(start.yr+nrow(sum.2026)))

#2025
sum.2025 <- read.csv(paste0(dir, "2025/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2025$Year <- 2025
sum.2025 <- sum.2025 %>% 
  filter(str_detect(X,"R")) %>% 
  filter(!str_detect(X,"IRrep")) %>% 
  filter(!str_detect(X,"IRresid")) %>%
  filter(!str_detect(X,"sIRresid")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2025$X <- c((start.yr+1):(start.yr+nrow(sum.2025)))

#2024
sum.2024 <- read.csv(paste0(dir, "2024/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2024$Year <- 2024
sum.2024 <- sum.2024 %>% 
  filter(str_detect(X,"R")) %>% 
  filter(!str_detect(X,"IRrep")) %>% 
  filter(!str_detect(X,"IRresid")) %>%
  filter(!str_detect(X,"sIRresid")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2024$X <- c((start.yr+1):(start.yr+nrow(sum.2024)))

#2023
sum.2023 <- read.csv(paste0(dir, "2023/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2023$Year <- 2023
sum.2023 <- sum.2023 %>% 
  filter(str_detect(X,"R")) %>% 
  filter(!str_detect(X,"IRrep")) %>% 
  filter(!str_detect(X,"IRresid")) %>%
  filter(!str_detect(X,"sIRresid")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2023$X <- c((start.yr+1):(start.yr+nrow(sum.2023)))

#2022
sum.2022 <- read.csv(paste0(dir, "2022/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2022$Year <- 2022
sum.2022 <- sum.2022 %>% 
  filter(str_detect(X,"R")) %>% 
  filter(!str_detect(X,"IRrep")) %>% 
  filter(!str_detect(X,"IRresid")) %>%
  filter(!str_detect(X,"sIRresid")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2022$X <- c((start.yr+1):(start.yr+nrow(sum.2022)))

#2021
sum.2021 <- read.csv(paste0(dir, "2021/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2021$Year <- 2021
sum.2021 <- sum.2021 %>% 
  filter(str_detect(X,"R")) %>% 
  filter(!str_detect(X,"IRrep")) %>% 
  filter(!str_detect(X,"IRresid")) %>%
  filter(!str_detect(X,"sIRresid")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2021$X <- c((start.yr+1):(start.yr+nrow(sum.2021)))

#2020
#sum.2020 <- read.csv(paste0(dir, "2020/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
#sum.2020$Year <- 2020
#sum.2020 <- sum.2020 %>% 
#  filter(str_detect(X,"R")) %>% 
#  filter(!str_detect(X,"IRrep")) %>% 
#  filter(!str_detect(X,"IRresid")) %>%
#  filter(!str_detect(X,"sIRresid")) %>% 
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2020$X <- c((start.yr+1):(start.yr+nrow(sum.2020)))

#2018
#sum.2018 <- read.csv(paste0(dir, "2018/2018 Assessment/",dir.area,"/ModelOutput/",file.area,"ModelOutput.csv"), header=T)
#sum.2018$Year <- 2018
#sum.2018 <- sum.2018 %>% 
#  filter(str_detect(X,"R")) %>% 
#  filter(!str_detect(X,"IRrep")) %>% 
#  filter(!str_detect(X,"IRresid")) %>%
#  filter(!str_detect(X,"sIRresid")) %>% 
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2018$X <- c((start.yr+1):(start.yr+nrow(sum.2018)))

mod.sum <- rbind(sum.2021, sum.2022, sum.2023, sum.2024, sum.2025, sum.2026)#sum.2018, sum.2020, 
mod.sum$Year <- as.factor(mod.sum$Year)

#Plots

#Median Biomass estimates
ggplot(data = mod.sum, aes(x = X, y = X50., colour = Year, group = Year))+
  geom_ribbon(aes(ymin=X2.5., ymax=X97.5., fill = Year, color = Year, linetype = Year),alpha=0.2, linewidth = 0.1)+
  geom_point()+
  geom_line()+
  labs(x = "Year", y = "Median Recruit Biomass Estimates (mt)")


# Median exploitation -----------------------------------------------

#Load model summaries and format for plotting

#2026
sum.2026 <- read.csv(paste0(dir, "2026/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2026$Year <- 2026
sum.2026 <- sum.2026 %>% 
  filter(str_detect(X, "mu")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2026$X <- c((start.yr+1):(start.yr+nrow(sum.2026)))

#2025
sum.2025 <- read.csv(paste0(dir, "2025/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2025$Year <- 2025
sum.2025 <- sum.2025 %>% 
  filter(str_detect(X, "mu")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2025$X <- c((start.yr+1):(start.yr+nrow(sum.2025)))

#2024
sum.2024 <- read.csv(paste0(dir, "2024/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2024$Year <- 2024
sum.2024 <- sum.2024 %>% 
  filter(str_detect(X, "mu")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2024$X <- c((start.yr+1):(start.yr+nrow(sum.2024)))

#2023
sum.2023 <- read.csv(paste0(dir, "2023/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2023$Year <- 2023
sum.2023 <- sum.2023 %>% 
  filter(str_detect(X, "mu")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2023$X <- c((start.yr+1):(start.yr+nrow(sum.2023)))

#2022
sum.2022 <- read.csv(paste0(dir, "2022/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2022$Year <- 2022
sum.2022 <- sum.2022 %>% 
  filter(str_detect(X, "mu")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2022$X <- c((start.yr+1):(start.yr+nrow(sum.2022)))

#2021
sum.2021 <- read.csv(paste0(dir, "2021/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2021$Year <- 2021
sum.2021 <- sum.2021 %>% 
  filter(str_detect(X, "mu")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2021$X <- c((start.yr+1):(start.yr+nrow(sum.2021)))

#2020
#sum.2020 <- read.csv(paste0(dir, "2020/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
#sum.2020$Year <- 2020
#sum.2020 <- sum.2020 %>% 
#  filter(str_detect(X, "mu")) %>% 
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2020$X <- c((start.yr+1):(start.yr+nrow(sum.2020)))

#2018
#sum.2018 <- read.csv(paste0(dir, "2018/2018 Assessment/",dir.area,"/ModelOutput/",file.area,"ModelOutput.csv"), header=T)
#sum.2018$Year <- 2018
#sum.2018 <- sum.2018 %>% 
#  filter(str_detect(X, "mu")) %>% 
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2018$X <- c((start.yr+1):(start.yr+nrow(sum.2018)))

#Plots

mod.sum <- rbind(sum.2021, sum.2022, sum.2023, sum.2024, sum.2025, sum.2026)#sum.2018, sum.2020, 
mod.sum$Year <- as.factor(mod.sum$Year)
ggplot(data = mod.sum, aes(x = X, y = X50., colour = Year, group = Year))+
  geom_ribbon(aes(ymin=X2.5., ymax=X97.5., fill = Year, color = Year, linetype = Year),alpha=0.2, linewidth = 0.1)+
  geom_point()+
  geom_line()+
  labs(x = "Year", y = "Median Exploitation (mu)")

#ggplot(data = mod.sum %>% filter(Year %in% c(2024,2025)), aes(x = X, y = X50., colour = Year, group = Year))+
#  geom_ribbon(aes(ymin=X2.5., ymax=X97.5., fill = Year, color = Year, linetype = Year),alpha=0.2, linewidth = 0.1)+
#  geom_point()+
#  geom_line()+
#  labs(x = "Year", y = "Median Exploitation (mu)")



# Median mortality (m) -----------------------------------------------

#Load model summaries and format for plotting

#2026
sum.2026 <- read.csv(paste0(dir, "2026/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2026$Year <- 2026
sum.2026 <- sum.2026 %>% 
  filter(str_detect(X, "^m")) %>%
  filter(!str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2026$X <- c((start.yr+1):(start.yr+nrow(sum.2026)))

#2025
sum.2025 <- read.csv(paste0(dir, "2025/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2025$Year <- 2025
sum.2025 <- sum.2025 %>% 
  filter(str_detect(X, "^m")) %>%
  filter(!str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2025$X <- c((start.yr+1):(start.yr+nrow(sum.2025)))

#2024
sum.2024 <- read.csv(paste0(dir, "2024/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2024$Year <- 2024
sum.2024 <- sum.2024 %>% 
  filter(str_detect(X, "^m")) %>%
  filter(!str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2024$X <- c((start.yr+1):(start.yr+nrow(sum.2024)))

#2023
sum.2023 <- read.csv(paste0(dir, "2023/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2023$Year <- 2023
sum.2023 <- sum.2023 %>% 
  filter(str_detect(X, "^m")) %>%
  filter(!str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2023$X <- c((start.yr+1):(start.yr+nrow(sum.2023)))

#2022
sum.2022 <- read.csv(paste0(dir, "2022/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2022$Year <- 2022
sum.2022 <- sum.2022 %>% 
  filter(str_detect(X, "^m")) %>%
  filter(!str_detect(X, "mu")) %>%
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2022$X <- c((start.yr+1):(start.yr+nrow(sum.2022)))

#2021
sum.2021 <- read.csv(paste0(dir, "2021/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
sum.2021$Year <- 2021
sum.2021 <- sum.2021 %>% 
  filter(str_detect(X, "^m")) %>%
  filter(!str_detect(X, "mu")) %>% 
  mutate(X = parse_number(X)) %>%
  arrange(X)
sum.2021$X <- c((start.yr+1):(start.yr+nrow(sum.2021)))

#2020
#sum.2020 <- read.csv(paste0(dir, "2020/Assessment/Data/Model/",dir.area,"/",file.area,"ModelOutput.csv"), header=T)
#sum.2020$Year <- 2020
#sum.2020 <- sum.2020 %>% 
#  filter(str_detect(X, "^m")) %>%
#  filter(!str_detect(X, "mu")) %>%
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2020$X <- c((start.yr+1):(start.yr+nrow(sum.2020)))

#2018
#sum.2018 <- read.csv(paste0(dir, "2018/2018 Assessment/",dir.area,"/ModelOutput/",file.area,"ModelOutput.csv"), header=T)
#sum.2018$Year <- 2018
#sum.2018 <- sum.2018 %>% 
#  filter(str_detect(X, "^m")) %>%
#  filter(!str_detect(X, "mu")) %>%
#  mutate(X = parse_number(X)) %>%
#  arrange(X)
#sum.2018$X <- c((start.yr+1):(start.yr+nrow(sum.2018)))

#Plots

mod.sum <- rbind(sum.2021, sum.2022, sum.2023, sum.2024, sum.2025, sum.2026)#sum.2018, sum.2020, 
mod.sum$Year <- as.factor(mod.sum$Year)
ggplot(data = mod.sum, aes(x = X, y = X50., colour = Year, group = Year))+
  geom_ribbon(aes(ymin=X2.5., ymax=X97.5., fill = Year, color = Year, linetype = Year),alpha=0.2, linewidth = 0.1)+
  geom_point()+
  geom_line()+
  labs(x = "Year", y = "Median Mortality (m)")
