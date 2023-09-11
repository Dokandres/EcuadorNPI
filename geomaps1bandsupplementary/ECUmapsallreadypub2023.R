################################################################################
#                                                                              #
# Purpose:       Code to reproduce the maps on figure 1 of ecuador             #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2023-01-10                                                    #
# Last updated:  2023-01-10                                                    #
# Source:                                                                      #
#                                                                              #
# Comment:                                                                     #
#                                                                              #
################################################################################


########################### Load required libraries ############################
{
library(exactextractr)
library(sp)
library(raster) 
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(rgdal)
library(maptools)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(tidyverse)
library(mltools)
library(readxl)
library (raster)
library(wpgpDownloadR)
library (rmapshaper)
  library(rOstluft.plot)
}

###############################LOAD DATA#################################################


#Load country boarders

States_ECUPRE <- st_as_sf (getData('GADM', country='ECU', level=1)) 
States_ECU <- ms_simplify (States_ECUPRE, keep = 0.01 ,keep_shapes = TRUE)
Boarders_ECUPRE <- st_as_sf(getData('GADM', country='ECU', level=0))
Boarders_ECU <- ms_simplify (Boarders_ECUPRE, keep = 0.001 ,keep_shapes = TRUE)
Municipalities_ECUPRE <- st_as_sf(getData('GADM', country='ECU', level=2))
Municipalities_ECU <- ms_simplify (Municipalities_ECUPRE, keep = 0.001 ,keep_shapes = TRUE)

#Load sampling sites serologic data


Sampling_Sites <- read_excel("ecuadorfinallistforpublic.xlsx")
#samples from 2019 out
Sampling_Sites <- Sampling_Sites %>% filter(YEAR !=2019)

#change positive and negative to 1 and 0 
Sampling_Sites$finalseroresult <- ifelse(Sampling_Sites$result == "positive", "1", 
                              ifelse(Sampling_Sites$result == "negative", "0",  "noresult"))
              
Sampling_Sites$finalseroresult <- as.numeric(Sampling_Sites$finalseroresult)  

#only select the positives to plot factor the moths 

Sampling_Sitespositives <- filter (Sampling_Sites, finalseroresult ==1)
Sampling_Sitespositives$mesmut= factor (Sampling_Sitespositives$mes,level= c("january2020", "february2020",  "march2020", "april2020","may2020", "juni2020", "july2020", "august2020", "september2020", "october2020", "november2020","december2020", "january2021", "february2021", "march2021", "april2021"))


# add number of samples per province 

Sampling_Sitescount <- Sampling_Sites %>% group_by (PROVINCIA, GID_1) %>% tally()
Sampling_Sitescount$n <- as.numeric (Sampling_Sitescount$n)

#'merge complete data                                                                                            

Samples_states <- merge(States_ECU, Sampling_Sites, by.x = "GID_1", by.y = "GID_1")     #Merge yout data with shapefile
Samples_states <- st_as_sf(Samples_states)     #Make the merged data a shapefile again to plot 

#merge count data
Samples_statescount <- merge(States_ECU, Sampling_Sitescount, by.x = "GID_1", by.y = "GID_1")     #Merge yout data with shapefile
Samples_statescount <- st_as_sf(Samples_statescount)     #Make the merged data a shapefile again to plot 

#merge positive data

#merge count data

Samples_statespositives <- merge(States_ECU, Sampling_Sitespositives, by.x = "GID_1", by.y = "GID_1")     #Merge yout data with shapefile
Samples_statespositives <- st_as_sf(Samples_statespositives)     #Make the merged data a shapefile again to plot 


#select positive data per month and merge 

Sampling_Sitespositivesmes <- Sampling_Sitespositives %>% group_by (PROVINCIA, GID_1, mesmut) %>% tally() %>% 
  ungroup() %>% tidyr::complete(nesting(PROVINCIA,GID_1),mesmut, fill = list(n = 0)) %>% group_by(PROVINCIA) %>% arrange(mesmut) %>% mutate (ncs= cumsum(n))

Sampling_Sitespositivesmes$n <- as.numeric (Sampling_Sitespositivesmes$n)

Samples_statespositivesmess <- right_join(States_ECU, Sampling_Sitespositivesmes, by=c("GID_1" = "GID_1"))     #Merge yout data with shapefile
Samples_statespositivesmess <- st_as_sf(Samples_statespositivesmess)     #Make the merged data a shapefile again to plot 

# population data 

#Estimated population density per grid-cell. The dataset is available to download in Geotiff and ASCII XYZ format at a resolution of 30 arc (approximately 1km at the equator). The projection is Geographic Coordinate System, WGS84. The units are number of people per square kilometre based on country totals adjusted to match the corresponding official United Nations population estimates that have been prepared by the Population Division of the Department of Economic and Social Affairs of the United Nations Secretariat (2019 Revision of World Population Prospects). The mapping approach is Random Forest-based dasymetric redistribution.
#WorldPop (www.worldpop.org - School of Geography and Environmental Science, University of Southampton; Department of Geography and Geosciences, University of Louisville; Departement de Geographie, Universite de Namur) and Center for International Earth Science Information Network (CIESIN), Columbia University (2018). Global High Resolution Population Denominators Project - Funded by The Bill and Melinda Gates Foundation (OPP1134076). https://dx.doi.org/10.5258/SOTON/WP00675

pop <- raster ("ecu_pd_2020_1km_UNadj.tif")
#plot(pop)
States_ECU$pop <- exact_extract(pop, States_ECU, 'max') 


###############################PLOT population DATA#################################################
#Plot popdata

ggplot() +
  geom_sf(data=States_ECU, aes(fill=States_ECU$pop), color="black", size=.01)+  #Continous data
  scale_fill_viridis_squished (limits= c(0,10000), alpha=.5, begin = 0, end = 1, option= "E", breaks=c(0,2000,4000,6000,8000,10000))+
  coord_sf(xlim=c(-75, -81.5))+
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("ECU_Mun_poptot.pdf")

###############-----samples total seroprevalence study

ggplot() +
  geom_sf(data=Samples_statescount,aes( fill=log2(n)), color="black", size=.005, show.legend = TRUE)+
  coord_sf(xlim=c(-75, -81.5))+
  scale_fill_viridis_c(alpha=.5, begin = 0, end = 1, option= "E")+
  theme(panel.grid.major = element_line(color = NA, 
                                       linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("Ecuador totalsamplemaps.pdf")

#### Figure S9. Temporal distribution of patients with SARS-CoV-2-specific antibodies during the study period in Ecuador (IgG). 

ggplot() +
  geom_sf(data=States_ECU, fill=NA, color="black", size=.005, show.legend = FALSE)+
  geom_sf(data=Samples_statespositivesmess,aes(fill= log2(ncs)), color="black", size=.005, show.legend = TRUE)+
  coord_sf(xlim=c(-75, -81.5))+
  facet_wrap(vars(mesmut))+
  scale_fill_viridis_b(alpha=.5, begin = 0.15, end = 1, option= "turbo")+

  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("Ecuadorpositivitypertimesturbo.pdf", )


########################### genomic samples map !!!!!!!!!!!!!!!!!!

#add PCR positives as data


datapcr <- read_excel("Code Ecuador/moleculardetectionsarsandotherviruses/ecuadorsamplesswaballdatarreadycurated2023.xlsx")

# add number of samples per province 

datapcrcount <- datapcr %>% group_by (PROVINCIA, GID_1) %>% tally()
datapcrcount$n <- as.numeric (datapcrcount$n)

#'merge complete data                                                                                            

Samples_statespcr <- merge(States_ECU, datapcrcount, by.x = "GID_1", by.y = "GID_1")     #Merge yout data with shapefile
Samples_statespcr <- st_as_sf(Samples_statespcr) 

# plot molecular data
ggplot() +
  geom_sf(data=Samples_statespcr,aes( fill=log2(n)), color="black", size=.005, show.legend = TRUE)+
  geom_sf (data=States_ECU, fill=NA, color="black", size=.005, show.legend = FALSE)+
  coord_sf(xlim=c(-75, -81.5))+
  scale_fill_viridis_c(alpha=.5, begin = 0, end = 1, option= "E")+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("Ecuador totalsamplemaps.pdf")
