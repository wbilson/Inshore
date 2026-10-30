#___________________________________________________________________________#
#             BoF Panel Figures Script for FSAR Document
#                 (C.Haar & G. English - JUly 2025)
#                     Saved on GitHub/Mar-Scal
#___________________________________________________________________________#


options(stringsAsFactors=FALSE)
library(ROracle)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(openxlsx)
library(sf)
library(lubridate)
library(dplyr)
#library(SSModel) #v 1.0-3
library(openxlsx)
library(compareDF)
library(tidyverse)
library(showtext)
showtext_auto()
#library(rosettafish)
#require(rosettafish) || stop("Install the rosettafish package please: remotes::install_github('freyakeyser/rosettafish')")
#require(RCurl) || stop("Install the RCurl package please: install.packages('RCurl')")
#rosetta_terms_SABHU <- read.csv("https://raw.githubusercontent.com/mar-scal/rosetta_shell/master/terms.csv", fileEncoding = "UTF-8")

#### Import Mar-scal functions 
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  #"https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
) 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


# Everything you should review/may need to change or update is listed between this point and "Summary Data:"

# 95% CI for timeseries
cred.lim = 0.05
# there's a lot of 'year' objects... maintaining them for compatability with old codes
# (THEY SHOULD ALWAYS BE THE SAME YEAR)
year <- 2026 # similar to fishing/assessment year but used in setting up directories - could be swapped out at some point,
fishingyear <- 2026 # most recent year of commercial fishing data to be used (e.g. if fishing season is 2019/2020, use 2020)
assessmentyear <- 2026 # year of model data to use

# Sources
direct <- paste0("Y:/Inshore/Assessment/BoF/")
direct_out <- paste0("Y:/Inshore/Assessment/BoF/", year,"/Assessment/")
#direct <- paste0("Y:/Inshore/BoF/")
#direct_out <- paste0("Y:/Inshore/BoF/", year,"/Assessment/")

# Reference points
RR.tab <- data.frame(
  SPA = c("SPA 1A", "SPA 1B", "SPA 3", "SPA 4", "SPA 6"),
  RR  = c(0.15, 0.15, 0.15, 0.15, 0.18),
  stringsAsFactors = FALSE)
# To find RR for specific area: rp.tab$RR[rp.tab$SPA==paste0("SPA ", area)]

# MAY NEED TO ADD MORE YEAR LABELS BASED ON WHAT YEAR IT IS! CREATED IN 2025 BUT WILL NEED TO UPDATE MAX YEAR AS TIME GOES ON
# Blame the endless continuation of time or find a better work around :)
# since each SPA has data over varying year ranges, specify what the x-axis year starting/ending will be here:
# coord_cartesian in the plots can add a few years before or after this in the plots, but this controls the ticks/labels
x_years <- list(
  "1B" = list(
    breaks = seq(1995, year+3, 5),
    labels = c(1995, 2000, 2005, 2010, 2015, 2020, 2025)
    # EX: add ", 2030" to 'labels' in 2027, because max year will be 2027+3 (=2030) and this won't have a label
  ),
  "1A" = list(
    breaks = seq(1995, year+3, 5),
    labels = c(1995, 2000, 2005, 2010, 2015, 2020, 2025)
  ),
  "3" = list(
    breaks = seq(1995, year+3, 5),
    labels = c(1995, 2000, 2005, 2010, 2015, 2020, 2025)
  ),
  "4" = list(
    breaks = seq(1980, year+3, 5),
    labels = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)
  ),
  "6" = list(
    breaks = seq(2005, year+3, 5),
    labels = c(2005, 2010, 2015, 2020, 2025)
    #breaks = seq(1975, year+3, 5),
    #labels = c(1975, "", 1985, "", 1995, "", 2005, "", 2015, "", 2025) # blanks ("") used here because labeling all years would get squishy
  )
  # add more areas as needed
)

#Havent completely sorted this out yet
# if you want plots to be different colours depending on SPA (see perch FSAR example)
# could built this list, and then when you plot geom_line or geom_ribbon for example (do this outside of aes()) you could set color=plot.col
# because we specify area here, it should automatically change the plot to the desired colour.
#area_colors <- c("1A" = "green", "1B" = "blue", "3" = "orange",
#                 "4" = "purple", "6" = "red")
#plot_col <- area_colors[as.character(area)]

### ATTENTION: SELECT SPA HERE!!! ##################################################################################################################################

# Specify the SPA you want to create figures for:

# User input: either one of "1A", "1B", "3", "4", "6", or "all"
area_input <- "all"#"all"

# List of valid areas
area_list <- c("1A", "1B", "3", "4", "6")

# Vector of areas to run based on user input
areas_to_run <- if (area_input == "all") area_list else area_input
# will automatically define "area" variable used to search within directories/files below
# example: will set area <- "1A" first, if you select "all", then overwrite and repeat for other areas

#Need this to read in .RData for each SPA
loadEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env)
}

# Loop through each area
for (area in areas_to_run) {
  # area is now each individual string: "1A", then "1B", etc.
  message("Making figures for SPA: ", area)

# Everything below is dependent on "area" (defined above)
direct_mod <- paste0(direct_out,"Data/Model/SPA", area, "/SPA", area, "_Model_", year, ".RData") # can get the model summary data this way... but preference to pull from summary table created (csv below)
# 1A working fine

### Summary Data: ##################################################################################################################################
# Model (SPA1A_Model_2024.RData)
mod <- loadEnvironment(paste0(direct_mod))
    mod.name <- paste0("Spa", area, ".", year)
    #works for most areas, some have unique data structure:
    if (area == "3") {
      mod.name <- paste0("Spa", area, ".","new.", year)}
    # Recreate 'bounds' object to set x/y plot boundaries
    mod.sims <- mod[[mod.name]]$sims.matrix
    post.matrix <- mod.sims[, is.element(substr(dimnames(mod.sims)[[2]], 1, 2), "B[")]
    # And then grab the credible limits.
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 1 - cred.lim/2)))
    ylim <- c(0, max(bounds[2, ]))
# "Model_results_and_diagnostics_2023_1A.RData"
        # Source: Y:\Inshore\BoF\2023\Assessment\Data\Model\SPA##
dat <- loadEnvironment(paste0(direct_out,"Data/Model/SPA", area, "/Model_results_and_diagnostics_", year,"_", area, ".RData"))
      # define variables from dat for easier referencing
      catch.next.year <- dat$catch.next.year
      LRP <- dat$LRP
      USR <- dat$USR
      ref.pts=c(LRP,USR)
                  # not the same thing as biomass projection
                    #Biomass.next.year <- dat$decision$B.next
  # "summary stats_1A_2023.csv" --> Medians and CI for recent/last 2 years
  sum.dat <- read.csv(paste0(direct_out,"Data/Model/SPA", area, "/summary stats_", area, "_", year,".csv"))
  # "summary stats temporal_1A_2023.csv" --> Annual medians for full time series - NO CIs
  ann.dat <- read.csv(paste0(direct_out,"Data/Model/SPA", area, "/summary stats temporal_", area, "_", year,".csv"))
             # The "95_CI" column headings should actually be 97.5 - this is just a typo and is the correct upper CI
    # easiest way to pull year range
    Years <- ann.dat$year
    Years.ribbon <- ann.dat$year
    Years.ribbon[length(Years.ribbon)] <- Years.ribbon[length(Years.ribbon)]+0.15
  proj.dat <- read.csv(paste0(direct_out,"Data/Model/SPA", area, "/boxplot.data.1y.predict.catch.of.",dat$catch.next.year,".in.", year+1, "_", area,".csv"))
  # Set up boxplot
      proj.box <- as.data.frame(t(quantile(proj.dat$x, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))))
      names(proj.box) <- c("ymin", "lower", "middle", "upper", "ymax")
      proj.box$year <- year+1
  # "Spa1AModelOutput.csv" --> model ouput (summary) --> annual mean, median* (50%) ***and all CIs (2.5, 25, 50*, 75, 97.5) - no year #'s
  mod.sum <- read.csv(paste0(direct_out,"Data/Model/SPA", area, "/SPA", area, "ModelOutput.csv"))
        colnames(mod.sum) <- c("vars", "mean", "sd", "2.5%", "25%", "median", "75%", "97.5%", "Rhat", "n.eff")

    # Extract biomass (commercial)
    b.rows <- grep("^B\\[", mod.sum$vars)
    b <- mod.sum[b.rows, ]
    # Pull data just for the 1-yr projection
    b.proj <- b[nrow(b),]

    # Extract biomass (recruits)
    r.rows <- grep("^R\\[", mod.sum$vars)
    r <- mod.sum[r.rows, ]
    # Pull data just for the 1-yr projection
    #mod.sum.r.proj <- mod.sum.b[nrow(mod.sum.b),]

    # Extract exploitation (prop)
    mu.rows <- grep("^mu\\[", mod.sum$vars)
    mu <- mod.sum[mu.rows, ]

    # Extract fishing mort (inst?)
    #fm.rows <- grep("^Fmort\\[", mod.sum$vars)
    #fm <- mod.sum[fm.rows, ]

    # Extract natural mort (inst)
    m.rows <- grep("^m\\[", mod.sum$vars)
    m <- mod.sum[m.rows, ]
    # convert inst mort to prop mort
    m.prop <- m
    m.prop[2:8] <- signif(1-exp(-m.prop[2:8]),2)

# Set theme and base text size for all plots
theme_set(theme_few(base_size = 14))



## Panel B ##############################################################################################################################
######## FR Biomass plot ################################################################################################################
bm.ts.plot <- ggplot() +
  # coloured status zones
  geom_rect(aes(xmin=min(Years)-5,xmax=max(Years)+5,ymin=ref.pts[2],ymax=ylim[2]*1.05),fill=rgb(0,1,0,0.2),col=NA)+ # Green/Healthy
  geom_rect(aes(xmin=min(Years)-5,xmax=max(Years)+5,ymin=ref.pts[1],ymax=ref.pts[2]),fill=rgb(1,1,0,0.3),col=NA)+ # Yellow/Cautious
  geom_rect(aes(xmin=min(Years)-5,xmax=max(Years)+5,ymin=0,ymax=ref.pts[1]),fill=rgb(1,0,0,0.4),col=NA)+ # Red/Critical
  # ref pts
  geom_hline(aes(yintercept = ref.pts[1], color = "LRP", linetype = "LRP")) +
  geom_hline(aes(yintercept = ref.pts[2], color = "USR", linetype = "USR")) +
  scale_color_manual(name = "", values = c("LRP" = "firebrick", "USR" = "goldenrod1")) +
  scale_linetype_manual(name = "", values = c("LRP" = "dotdash", "USR" = "longdash")) +
  guides(color = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE)) +# rearranges legend order to put USR at top of list (more intuitive)
  # data
  geom_ribbon(aes(ymin=b$`2.5%`, ymax=b$`97.5%`, x=Years.ribbon),alpha=0.2,fill="grey20") + # plots 95% CI around time series
  geom_line(aes(x = Years, y = b$median)) + # plots biomass median
  geom_point(aes(x = Years, y = b$median), pch = 16, size = 1) + # plots biomass median
  geom_boxplot(data = proj.box, stat = "identity", # projection boxplot
               aes(x = year,ymin = ymin,lower = lower,middle = middle,upper = upper,ymax = ymax),
               outlier.shape=NA, fill = NA) +
  # styling
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,ylim[2]*1.05)) + # Sets plot boundaries
  scale_x_continuous(breaks=x_years[[area]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[area]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + # {...}
  labs(y = "Commercial biomass (meats, t)", x = "") +
  #annotate("text", x = year, y = Inf, label = "(B)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(B)",hjust = 1.5, vjust = 1.5, size = 5) +
  theme(legend.position = c(0.15,0.95), # places legend in by 0.15%, up by 0.95% (down by 0.05) of plot space
        legend.background = element_blank(),
        legend.key = element_blank(), # legend norm puts white boxes around symbols, removes this
        legend.key.width = unit(0.8, "cm"),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.text = element_text(margin = margin(r = 2)), # place lebsl father from ticks
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 4)), # place title farther from labels
        plot.margin = margin(5, 10, 5, 5, "points")) # need bigger margins around plot to account for axis labels
# Removing status labels from fig b/c causes translation issues, FSAR doesn't want words on plots, & not necessary
#RP.labels <- data.frame(region = c("Healthy","Cautious","Critical"),x.pos = c(2015,2015,2015),y.pos = c(0.95 * max(bounds[2, ]), ref.pts[1] * 1.5, ref.pts[1] * 0.5),size = c(1,1,1))
#geom_text(aes(x=RP.labels$x.pos[RP.labels$region == "Healthy"],  y=RP.labels$y.pos[RP.labels$region == "Healthy"],  label = toupper("Healthy")), col="chartreuse2",cex=5) +
#geom_text(aes(x=RP.labels$x.pos[RP.labels$region == "Cautious"], y=RP.labels$y.pos[RP.labels$region == "Cautious"], label = toupper("Cautious")),col="goldenrod1",cex=5) +
#geom_text(aes(x=RP.labels$x.pos[RP.labels$region == "Critical"], y=RP.labels$y.pos[RP.labels$region == "Critical"], label = toupper("Critical")),col="firebrick2",cex=5) +

showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_CommercialBiomass_SPA", area,"_",year,".png"), bm.ts.plot, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)

## Panel C  ###################################################################################################################
############ Exploitation / Mort Plot #########################################################################################
e.m.plot <- ggplot() +
  # exploitation (prop): mu
  geom_ribbon(aes(ymin=mu$`2.5%`,ymax=mu$`97.5%`,x=Years.ribbon[-1]-1), alpha=0.2,fill="grey10") +
  geom_hline(aes(yintercept=RR.tab$RR[RR.tab$SPA==paste0("SPA ", area)],col="RR"), lty="dotted", alpha=0.7, linewidth=0.8)+ # RR
  geom_line(aes(x=Years[-1]-1, y=mu$median), col="black", linewidth = 0.4) +
  geom_point(aes(x=Years[-1]-1, y=mu$median), col="black", size=1) +
  #styling
  scale_color_manual(name="",values=c("black"))+
  labs(y = "Exploitation (proportional rate)", x = "") +
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,0.6))+
  scale_x_continuous(breaks=x_years[[area]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[area]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  #annotate("text", x = year, y = Inf, label = "(C)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(C)",hjust = 1.5, vjust = 1.5, size = 5)+
  theme(legend.position = c(0.15,1),
        legend.background = element_blank(),
        legend.key.width = unit(0.8, "cm"),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.x = element_blank(),
        axis.text = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 0, 5, 0, "points"))
e.m.plot
showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_Exploitation_SPA", area,"_",year,".png"), e.m.plot, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)

## Panel D ####################################################################################################################
########## Recruit Biomass plot ###############################################################################################
rec.ts.plot <- ggplot() +
  # data
  geom_ribbon(aes(ymin=r$`2.5%`,ymax=r$`97.5%`,x=Years.ribbon), alpha=0.2,fill='grey20') +
  geom_hline(aes(yintercept=median(r$median[-length(r$median)]),col="LTM"), lty="dashed", alpha=0.7)+ # long-term median (1994:current yr-1)
  scale_color_manual(name="",values=c("grey20"))+
  geom_line(aes(Years,r$median),col="black", linewidth = 0.4) +
  geom_point(aes(Years,r$median), col="black", size=1) +
  # styling
  xlab("") + ylab("Recruit biomass (meats, t)")  +
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(-25,max(r$`97.5%`)*1.1))+
  scale_x_continuous(breaks=x_years[[area]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[area]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  #annotate("text", x = year, y = Inf, label = "(D)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(D)",hjust = 1.5, vjust = 1.5, size = 5)+
  theme(axis.title.x = element_blank(),
        legend.position = c(0.15,1),
        legend.background = element_blank(),
        legend.key.width = unit(0.7, "cm"),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 10, 5, 0, "points"))
rec.ts.plot
showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_RecruitBiomass_SPA", area,"_",year,".png"), rec.ts.plot, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)

## Panel 2 ####################################################################################################################
########## Natural Mortality ##################################################################################################
m.ltm <- median(m.prop$median[-length(m.prop$median)])
m.plot <- ggplot() +
  # nat mort (prop): m.prop
  geom_ribbon(aes(ymin=m.prop$`2.5%`,ymax=m.prop$`97.5%`,x=Years.ribbon), alpha=0.12,fill="black") +
  geom_hline(aes(yintercept=m.ltm,col="LTM"), lty="dashed", alpha=0.7)+ # RR
  geom_line(aes(x=Years, y=m.prop$median), col="black", linewidth = 0.4) +
  geom_point(aes(x=Years, y=m.prop$median), col="black", size=1) +
  #styling
  scale_color_manual(name="",values=c("grey20"))+
  labs(y = "Natural mortality (proportional rate)", x = "") +
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,1.0))+
  scale_x_continuous(breaks=x_years[[area]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[area]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  #annotate("text",x = Inf, y = Inf,label = "(C)",hjust = 1.5, vjust = 1.5, size = 5)+
  theme(legend.position = c(0.15,0.925),
        legend.background = element_blank(),
        legend.key.width = unit(0.7, "cm"),
        axis.title.x = element_blank(),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.text = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 5, 5, 5, "points"))
m.plot
showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel2_NatMort_SPA", area,"_",year,".png"), m.plot, dpi = 600, width = 4.5, height = 3.5)
showtext_auto(TRUE)

#Probably need an if loop for tacland plots
## Panel A ##############################################################################################################################
### tacland plot ########################################################################################################################
# CHANGE: tacq
# CHANGE: scale_x_continuous(breaks=seq(2000,fishingyear,5)) +
# ADD: annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
# ADD:
#legend.position = c(0.15, 1),
#panel.border = element_blank(),
#panel.background = element_blank(),
#axis.line = element_line(colour = "grey10", linewidth = 0.4),
#axis.title.x = element_blank(),
#axis.text.x = element_text(margin = margin(t = 2)),
#plot.margin = margin(5, 0, 5, 1, "points")
### SPA 1A #############################################################################################################################
if (area == "1A") {
# tacland plot modelled after code in --> Drive:[...]\GitHub\Inshore\BoF\CommercialData
#CommercialLogsAnalysis
SPA<-"1A"
# This "SPA" is differs from "area" because "area" can be specified at the top to specify which SPA the other figures are created for,
# but the TAC plots are all separate ggplot scripts because each is made quite diff so I created a new object to avoid potential problems
tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA",SPA,"_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
landings <- tacq[,-1]
landings <- as.data.frame(t(landings))
names(landings) <- c("landings.fleet.mt", "TAC")
landings$year <- as.numeric(substr(rownames(landings),6,9))
#plot
tacland <- ggplot(landings) +
  geom_bar(aes(x = year, y = landings.fleet.mt), stat = "identity", color = "black", fill = "white", linewidth=0.2) +
  geom_line(aes(x = year, y = TAC), lwd = 1) +
  ylab("Landings (meats, t)") +
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,max(landings$TAC, na.rm=T)+100))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_continuous(breaks=x_years[[SPA]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[SPA]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  #annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(A)",hjust = 1.5, vjust = 1.5, size = 5)+
  annotate(geom="text",label="TAC", x=2006.5, y= 600) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        legend.position = c(0.2, 0.85), legend.background = element_blank(),
        legend.key.width = unit(0.8, "cm"),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 5, 5, 1, "points"))
tacland

showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_TAClandings_SPA", area,"_",year,".png"), tacland, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)
  }
### SPA 1B #############################################################################################################################
if (area == "1B") {
# CommercialLogsAnalysis
#tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1B_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
  SPA<-"1B"
  tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA",SPA,"_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
  landings <- tacq[c(1:4,9),-1]
  landings <- as.data.frame(t(landings))
  names(landings) <- c("FB", "MB", "UB","FSC", "TAC")
  landings$year <- as.numeric(substr(rownames(landings),6,9))
  #Convert landings data to long format
  landings <- reshape2::melt(landings, id.vars = "year", value.name = "catch.fleet.mt")
  
  landings.tot <- landings %>% group_by(year) %>% 
    filter(variable != "TAC") %>% 
    summarize(tot = sum(catch.fleet.mt, na.rm = TRUE))
  
  # plot
  tacland <- ggplot(landings) +
    geom_bar(data = landings.tot, aes(x = year, y = tot), stat = "identity", color = "black", fill = "white", linewidth=0.2) +
    #geom_bar(data=landings[landings$variable%in%c('FB','MB', 'UB', 'FSC'),],
    #aes(year, catch.fleet.mt, fill=factor(variable, levels = c('FSC','UB', 'MB', 'FB'))), colour="black", stat="identity") +
    geom_line(data=landings[landings$variable == 'TAC',], aes(x = year, y = catch.fleet.mt), lwd = 1) +
    ylab("Landings (meats, t)") +
    coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,max(landings$catch.fleet.mt, na.rm=T)+100))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
    scale_x_continuous(breaks=x_years[[SPA]]$breaks,
                       # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                       labels = x_years[[SPA]]$labels, # , 2030), # add this back in 2030
                       expand = c(0, 0), limits = c(0, NA)) +
    #scale_fill_manual(values=c("black", "white", "royalblue2", "grey"), labels=c("FSC","Upper Bay", "Mid-Bay", "Full Bay"), name=NULL) +
    #annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
    annotate("text",x = Inf, y = Inf,label = "(A)",hjust = 1.5, vjust = 1.5, size = 5)+
    annotate(geom="text",label="TAC", x=2010.4, y= 425) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(margin = margin(t = 4)),
          axis.title.y = element_text(margin = margin(r = 4)),
          legend.position = c(0.2, 0.8), legend.background = element_blank(),
          legend.key.width = unit(0.8, "cm"),
          panel.border = element_rect(linewidth = 1, fill = NA),
          axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
          plot.margin = margin(5, 5, 5, 1, "points"))

showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_TAClandings_SPA", area,"_",year,".png"), tacland, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)
  }
### SPA 3 #############################################################################################################################
if (area == "3") {
# CommercialLogsAnalysis
#tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1B_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
SPA<-"3"
tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA",SPA,"_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
landings <- tacq[,-1]
landings <- as.data.frame(t(landings))
names(landings) <- c("landings.fleet.mt", "TAC")
landings$year <- as.numeric(substr(rownames(landings),6,9))
# plot
tacland <- ggplot(landings) +
  geom_bar(aes(x = year, y = landings.fleet.mt), stat = "identity", color = "black", fill = "white", linewidth=0.2) +
  geom_line(aes(x = year, y = TAC), lwd = 1) +
  ylab("Landings (meats, t)") +
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,max(landings$TAC, na.rm=T)+100))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_continuous(breaks=x_years[[SPA]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[SPA]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  #scale_fill_manual(values=c("black", "white", "royalblue2", "grey"), labels=c("FSC","Upper Bay", "Mid-Bay", "Full Bay"), name=NULL) +
  #annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(A)",hjust = 1.5, vjust = 1.5, size = 5)+
  annotate(geom="text",label="TAC", x=2003.5, y= 315) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        legend.position = c(0.2, 0.85), legend.background = element_blank(),
        legend.key.width = unit(0.8, "cm"),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 5, 5, 1, "points"))

showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_TAClandings_SPA", area,"_",year,".png"), tacland, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)
  }
### SPA 4 #############################################################################################################################
if (area == "4") {
SPA<-"4"
# CommercialLogsAnalysis
#tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1B_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA",SPA,"and5_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
# May need to change directory letter (Y: in this case, but could be Y:)
tacq4<-read.csv(paste0(direct,"CommercialData/Archive/2014/SPA4_TACandLandings_2014.csv")) # DO NOT UPDATE - historial data
landings <- tacq[,-1]
landings <- as.data.frame(t(landings))
names(landings) <- c("SPA4", "SPA5", "TAC")
landings$year <- as.numeric(substr(rownames(landings),6,9))
#Convert landings data to long format
landings <- reshape2::melt(landings, id.vars = "year", value.name = "catch.fleet.mt")
#Prepare historical SPA4 TAC data:
tacq4 <- as.data.frame(t(tacq4[2,-1]))
names(tacq4) <- "TAC"
tacq4$year <- as.numeric(substr(rownames(tacq4), 2,5))+1
# Plot
# Plot SPA 4 & 5 landings with TAC lines
# tacland <- ggplot(landings) +
#   #geom_bar(data=landings[landings$variable%in%c('SPA4','SPA5'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c("SPA5", "SPA4"))), colour="black", stat="identity") +
#   geom_bar(data=landings[landings$variable%in%c('SPA4','SPA5'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c("SPA5", "SPA4"))), colour="black", stat="identity") +
#   geom_line(data=landings[landings$variable == 'TAC' & landings$year >= 2014,], aes(x = year, y = catch.fleet.mt), lwd = 1) + #adds combined TAC line
#   geom_line(data=tacq4, aes(year, TAC),linetype="dashed", lwd=1) + #adds historical SPA4 TAC line
#   scale_fill_manual(values=c("skyblue1", "grey"), labels=c("SPA 5", "SPA 4"), name=NULL) +
#   ylab("Landings (meats, t)") +
#   coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,max(landings$catch.fleet.mt, na.rm=T)+100))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
#   scale_x_continuous(breaks=x_years[[SPA]]$breaks,
#                      # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
#                      labels = x_years[[SPA]]$labels, # , 2030), # add this back in 2030
#                      expand = c(0, 0), limits = c(0, NA)) +
#   #scale_fill_manual(values=c("black", "white", "royalblue2", "grey"), labels=c("FSC","Upper Bay", "Mid-Bay", "Full Bay"), name=NULL) +
#   #annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
#   annotate("text",x = Inf, y = Inf,label = "(A)",hjust = 1.5, vjust = 1.5, size = 5)+
#   annotate(geom="text",label="SPA 4 and 5 TAC", x=2015, y= 350) +
#   annotate(geom="text",label="SPA 4 TAC", x=2003, y= 1340) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 4)),
#         axis.title.y = element_text(margin = margin(r = 4)),
#         legend.position = c(0.35, 0.9), legend.background = element_blank(),
#         legend.key.width = unit(0.8, "cm"),
#         panel.border = element_rect(linewidth = 1, fill = NA),
#         axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
#         plot.margin = margin(5, 5, 5, 1, "points"))
# 
# showtext_auto(FALSE)
# ggsave(filename=paste0(direct_out, "/Figures/test/FSAR_panel1_TAClandings_SPA", area, ".png"), tacland, dpi = 600, width = 6.5, height = 5.5)
# showtext_auto(TRUE)

#BLACK AND WHITE LANDINGS FIGURE WITHOUT SEPARTE SPA 4 AND 5 LANDINGS
tacland <- ggplot(landings) +
  geom_bar(data=landings[landings$variable%in%c('SPA4','SPA5'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c("SPA5", "SPA4"))), colour="black", fill= "white", stat="identity", linewidth=0.2) +
  geom_line(data=landings[landings$variable == 'TAC' & landings$year >= 2014,], aes(x = year, y = catch.fleet.mt), lwd = 1) + #adds combined TAC line
  geom_line(data=tacq4, aes(year, TAC),linetype="dashed", lwd=1) + #adds historical SPA4 TAC line
  ylab("Landings (meats, t)") +
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,max(landings$catch.fleet.mt, na.rm=T)+100))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_continuous(breaks=x_years[[SPA]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[SPA]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  #scale_fill_manual(values=c("black", "white", "royalblue2", "grey"), labels=c("FSC","Upper Bay", "Mid-Bay", "Full Bay"), name=NULL) +
  #annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(A)",hjust = 1.5, vjust = 1.5, size = 5)+
  annotate(geom="text",label="SPA 4 and 5 TAC", x=2015, y= 350) +
  annotate(geom="text",label="SPA 4 TAC", x=2003, y= 1340) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        legend.position = c(0.35, 0.9), legend.background = element_blank(),
        legend.key.width = unit(0.8, "cm"),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 5, 5, 1, "points"))

showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_TAClandings_SPA", area,"_",year,".png"), tacland, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)
# SPA 4 & 5 TAC and Landings with truncated x-axis for presentations
tacland2 <- ggplot(landings) +
  geom_bar(data=landings[landings$variable%in%c('SPA4','SPA5') & landings$year > 2008,],
           aes(year, catch.fleet.mt, fill=factor(variable, levels = c("SPA5", "SPA4"))), colour="black", stat="identity",linewidth=0.2) +
  geom_line(data=landings[landings$variable == 'TAC' & landings$year >= 2014,], aes(x = year, y = catch.fleet.mt), lwd = 1) + #adds combined TAC line
  geom_line(data=tacq4[tacq4$year >= 2008,], aes(year, TAC),linetype="dashed", lwd=1) + #adds historical SPA4 TAC line
  scale_fill_manual(values=c("skyblue1", "grey"), labels=c("SPA 5", "SPA 4"), name=NULL) +
  ylab("Landings (meats, t)") +
  coord_cartesian(xlim=c(min(Years)-1, max(Years+2)), ylim=c(0,max(landings$catch.fleet.mt[landings$year > 2008], na.rm = TRUE)+100))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_continuous(breaks=x_years[[SPA]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[SPA]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  #scale_fill_manual(values=c("black", "white", "royalblue2", "grey"), labels=c("FSC","Upper Bay", "Mid-Bay", "Full Bay"), name=NULL) +
  #annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(A)",hjust = 1.5, vjust = 1.5, size = 5)+
  annotate(geom="text",label="SPA 4 and 5 TAC", x=2016, y= 260) +
  annotate(geom="text",label="SPA 4 TAC", x=2011, y= 160) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        legend.position = c(0.15, 0.9), legend.background = element_blank(),
        legend.key.width = unit(0.8, "cm"),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 5, 5, 1, "points"))

showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_TAClandings_SPA", area,"_",year,".png"), tacland, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)
  }
### SPA 6 #############################################################################################################################
if (area == "6") {
SPA<-"6"
# CommercialLogsAnalysis
#tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA1B_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
tacq <- read.xlsx(paste0(direct,"/",assessmentyear,"/Assessment/Data/CommercialData/SPA",SPA,"_TACandLandings_", fishingyear, ".xlsx"), sheet = "TACandLandings")
#Landings by fleet:
landings <- as.data.frame(t(tacq[c(1,2,3,5),-1]))
names(landings) <- c("FB","MB","FSC", "TAC")
landings$year <- as.numeric(rownames(landings))
#Convert landings data to long format
landings <- reshape2::melt(landings, id.vars = "year", value.name = "catch.fleet.mt")
landings <- landings %>% filter(year >= 2006)

landings.tot <- landings %>%
  group_by(year) %>% 
  filter(variable != "TAC") %>% 
  summarize(tot = sum(catch.fleet.mt, na.rm = TRUE))

# plot
tacland <- ggplot(landings) +
  geom_bar(data = landings.tot, aes(x = year, y = tot), stat = "identity", color = "black", fill = "white",linewidth=0.2) +
  #geom_bar(data=landings[landings$variable%in%c('FSC','FB','MB'),], aes(year, catch.fleet.mt, fill=factor(variable, levels = c('FSC', 'FB','MB'))), colour="black", stat="identity") +
  geom_line(data=landings[landings$variable == 'TAC',], aes(x = year, y = catch.fleet.mt), lwd = 1) +
  ylab("Landings (meats, t)") +
  coord_cartesian(xlim=c(min(x_years[[SPA]]$breaks), max(Years+2)), ylim=c(0,max(landings$catch.fleet.mt, na.rm=T)+100))+
  #scale_fill_manual(values=c("black","white", "grey"), labels=c("Food, Social, and Ceremonial","Full Bay", "Mid-Bay"), name=NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_continuous(breaks=x_years[[SPA]]$breaks,
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [,2030].
                     labels = x_years[[SPA]]$labels, # , 2030), # add this back in 2030
                     expand = c(0, 0), limits = c(0, NA)) +
  #annotate("text", x = year, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
  annotate("text",x = Inf, y = Inf,label = "(A)",hjust = 1.5, vjust = 1.5, size = 5)+
  annotate(geom="text",label="TAC", x=2009, y= 160) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        legend.position = c(0.5, 0.82), legend.background = element_blank(),
        legend.key.width = unit(0.8, "cm"),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 8, 5, 1, "points"))

showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_TAClandings_SPA", area,"_",year,".png"), tacland, dpi = 600, width = 6.5, height = 5.5)
showtext_auto(TRUE)
  }

## Combined ##################################################################################################################
#### FSAR 4 Panel plot #######################################################################################################
showtext_auto(FALSE)
panel <- cowplot::plot_grid(tacland, bm.ts.plot, e.m.plot, rec.ts.plot, align="v",ncol=2,axis="lr")
#ggsave(filename=paste0(direct_out, "/Figures/test/FSAR_panel1_SPA", area, ".png"), panel, dpi = 600, width = 9, height = 7)
ggsave(filename=paste0(direct_out, "Figures/FSR_Panel_plots/FSAR_panel1_SPA", area,"_",year,".png"), panel, dpi = 600, width = 9, height = 7) #use if altering a 4-panel figure (i.e SPA4 without SPA5 TAC) - Make sure to change "area_input" argument at line 86 to desired area!!
showtext_auto(TRUE)
# End of loop
}
