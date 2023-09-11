################################################################################
#                                                                              #
# Purpose:       Code to reproduce the bivariate map of ecuador                #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2022-01-09                                                    #
# Last updated:  2022-01-09                                                    #
# Source:                                                                      #
#                                                                              #
# Comment:                                                                     #
#                                                                              #
################################################################################
#Load packages
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
#library (rOstluft.plot)
library(biscale)
library ( patchwork)
}


###############################LOAD DATA################################################


#Load country boarders
States_ECUPRE <- st_as_sf (getData('GADM', country='ECU', level=1)) 
States_ECU <- ms_simplify (States_ECUPRE, keep = 0.01 ,keep_shapes = TRUE)
Boarders_ECUPRE <- st_as_sf(getData('GADM', country='ECU', level=0))
Boarders_ECU <- ms_simplify (Boarders_ECUPRE, keep = 0.001 ,keep_shapes = TRUE)
Municipalities_ECUPRE <- st_as_sf(getData('GADM', country='ECU', level=2))
Municipalities_ECU <- ms_simplify (Municipalities_ECUPRE, keep = 0.001 ,keep_shapes = TRUE)

plot (States_ECU$GID_1)

#Load sampling sites

Sampling_Sites <- read_excel("/serologicalanalysesecuador/ecuadorfinallistforpublic.xlsx")

#let all the samples from 2019 out
Sampling_Sites <- Sampling_Sites %>% filter(YEAR !=2019)

#change positive and negative to 1 and 0 
Sampling_Sites$finalseroresult <- ifelse(Sampling_Sites$result == "positive", "1", 
                                         ifelse(Sampling_Sites$result == "negative", "0",  "noresult"))

Sampling_Sites$finalseroresult <- as.numeric(Sampling_Sites$finalseroresult)  


# add number of samples per province 

Sampling_Sitescount <- Sampling_Sites %>% group_by (PROVINCIA, GID_1) %>% tally()
Sampling_Sitescount$n <- as.numeric (Sampling_Sitescount$n)

#add PCR positives as data


datapcr <- read_excel("/moleculardetectionsarsandotherviruses/ecuadorsamplesswaballdatarreadycurated2023.xlsx")

# add number of samples per province 

datapcrcount <- datapcr %>% group_by (PROVINCIA, GID_1) %>% tally()
datapcrcount$n <- as.numeric (datapcrcount$n)

#'merge complete pcr data                                                                                            

Samples_statespcr <- merge(States_ECU, datapcrcount, by.x = "GID_1", by.y = "GID_1")     #Merge yout data with shapefile
Samples_statespcr <- st_as_sf(Samples_statespcr) 

#unite the variables of n in one data frame....

numbers <- Sampling_Sitescount %>% full_join(datapcrcount, by= "GID_1", suffix= c("sera", "pcr"))


# convert to a spatial object 

Samples_statesall <- merge(States_ECU, numbers, by.x = "GID_1", by.y = "GID_1")     #Merge yout data with shapefile
Samples_statesall <- st_as_sf(Samples_statesall)     #Make the merged data a shapefile again to plot 


mapbivar <- bi_class(Samples_statesall , nsera , npcr, style = "quantile", dim = 3) %>% 
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))

#construct a Legend

legend2 <- bi_legend(pal = "Bluegill",
                     dim = 3,
                     xlab = "Serology",
                     ylab = "PCR",
                     size = 9)


p1 <- ggplot(mapbivar) + 
  geom_sf(aes(fill = bi_class), 
          colour = NA, 
          size = .1, 
          show.legend = FALSE) +
  geom_sf(data = States_ECU ,  
          color = "black", 
          fill = NA, 
          size = 0.2) +
  coord_sf(xlim=c(-75, -81.5))+
  bi_scale_fill(pal = "Bluegill", 
                dim = 3, 
                na.value = "white") +
  bi_theme() 

p <- p1 | legend2
p


ggsave("Ecuadortotalsamplebivariate.pdf")

#number of samples per province 
ggplot (numbers, aes(fct_reorder(PROVINCIAsera, nsera), fill=npcr ))+
geom_bar(aes(y=nsera), stat= "identity")+
geom_bar (aes(y=npcr*(-1)), stat= "identity")+
coord_flip()+
scale_y_continuous(breaks=c(-1000,-800,-600,-400,-200,0,200,400,600,800,1000))+
theme_minimal()
