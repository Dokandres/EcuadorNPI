################################################################################
#                                                                              #
# Purpose:       Code to reproduce the figure 2 and supplement for epidemiolo  #
#                gical data in Ecuador                                         #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2022-01-12                                                    #
# Last updated:  2022-01-12                                                    #
# Source:                                                                      #
#                                                                              #
# Comment:                                                                     #
#                                                                              #
################################################################################

########################### Load required libraries ############################
{
library(tidyverse)
library(devtools)
library (usethis)
library(dplyr)
library(ggplot2)
library(readxl)
library(sf)
library (sp)
library(raster)
library(viridis)
library(writexl)
library(tidyverse)
library (RColorBrewer)
library (viridis)
library (ggridges)
library(summarytools)
library(lubridate)
library (gtsummary)
library (flextable)
library (stringr)
library(CI)
}


#Load data and select data-----------------------------------------------------------------------------
data <- read_excel("dataecuallepifrom20-21mod.xlsx")

#fuse year and epiweek with separator

data$epiyearr <- str_c(data$epiweek, "-", data$year)

data$percentpositives <- (data$`SARS-CoV-2Positivo`*100/data$`# Total muestras analizadas`)

#descriptive statistics 

summary (data$`SARS-CoV-2Positivo`)

summary (data$`# Total muestras analizadas`)

summary(data$percentpositives)

sd (data$`SARS-CoV-2Positivo`, na.rm = T)
sd (data$`# Total muestras analizadas`, na.rm = T)
sd(data$percentpositives, na.rm = T)

# package CI

CI_t (data$`# Total muestras analizadas`, ci = 0.95)

# separate per year for the table

data2020 <- subset (data, year== 2020)

data2021 <- subset (data, year==2021)

#plot total number of samples per epiweek versus positives for coronavirus

ggplot(data2020, aes(x = epiweek)) + 
  geom_line(aes ( y=`# Total muestras analizadas`), color ="black", size=0.2)+
  geom_line(aes ( y=`SARS-CoV-2Positivo` ), color ="red", size =0.2)+
  scale_x_continuous(breaks=seq(1,53,1))+
  scale_y_continuous(breaks=seq(0,17000,1000))

##################Figure 2. Circulation of SARS-CoV-2 and other respiratory viruses in Ecuador. (A) ############################ 
ggplot(data2020, aes(x = epiweek)) + 
  geom_line(aes(y= percentpositives), color= "orange", size=0.2) +
  scale_x_continuous(breaks=seq(1,53,1))+
  scale_y_continuous(breaks=seq(0,100,10))
