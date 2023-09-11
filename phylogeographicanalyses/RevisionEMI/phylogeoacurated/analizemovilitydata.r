################################################################################
#                                                                              #
# Purpose:       Code to reproduce Last part Fig 5 , mobility data Ecuador     #                                                                          #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2023-08-23                                                    #
# Last updated:  2023-08-23                                                    #
# Source:                                                                      #
# data gathered from:                                                          # 
# https://www.google.com/covid19/mobility/                                     #
# Comment: NA                                                                  #
#                                                                              #
################################################################################

#load libraries

{
  library(ggplot2)
  library(readxl)
  library(tidyverse)
  library(dplyr)
  library(ggpubr)
  library(cowplot)
  library(ggthemes)
  library(viridis)
  library(ggrepel)
  library(ggalluvial)
  library(lubridate)
  library (RColorBrewer)
  library(zoo)
  library(pastecs)
  library(colorspace)
  library (ggridges)
  library(ggeasy)
}


Sys.setlocale("LC_ALL", "English")
#load data 
movdata_df <- read_csv ("mobilitydata/Global_Mobility_Report.csv")             
movdata_dfecu <- movdata_df %>% filter(country_region == "Ecuador" & date < "2021-01-01")


create_plot <- function(column_name) {
  ggplot(data = movdata_dfecu, aes(x = date, y = .data[[column_name]])) +
    geom_area(alpha = 0.1) +
    stat_smooth(
      geom = 'area', method = 'loess', span = 1/3,
      alpha = 1/2, fill = "gray23"
    ) +
    geom_hline (yintercept=0)+
    scale_y_continuous(limits=c(-50,50), breaks=c(-50,0,50))+
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_classic()
}

# Call the function to create the plot and save it as a graphic variable
retailrecreation <- create_plot("retail_and_recreation_percent_change_from_baseline")

grocerypharmacy <- create_plot("grocery_and_pharmacy_percent_change_from_baseline")

parks <- create_plot("parks_percent_change_from_baseline")

transitstations <- create_plot("transit_stations_percent_change_from_baseline")

workplaces <- create_plot("workplaces_percent_change_from_baseline")

residential <- create_plot("residential_percent_change_from_baseline" )

allplotsmov <- ggarrange(retailrecreation, grocerypharmacy, parks, transitstations, workplaces, residential, ncol=2, nrow = 3)

allplotsmov
