################################################################################
#                                                                              #
# Purpose:       Code to reproduce he geodata and maps of the molecular data,  #
#                Ecuador                                                       #
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

########################### Load required libraries ############################
{
library(raster)
library (ggmap)
library (ggspatial)
}


################### set working directory and load the data ####################

data <- read_excel("ecuadorsamplesswaballdatarreadycurated2023.xlsx")
data <- data %>% clean_names()

#Geodata maps

ECU_mun <- sf::st_as_sf(getData('GADM', country='ECU', level=1))

#plot only ECUmap 

ggplot() +
  geom_sf(data=ECU_mun, fill=NA, color="black", size=.005, show.legend = FALSE)+   #Plot country border
  annotation_scale(location = "bl", width_hint = 0.2) + #Define location of the scalebar to bottom left (bl) 
  annotation_north_arrow(location = "bl", which_north = "true",               #Define position and characteristics of arrow
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),        
                         style = north_arrow_fancy_orienteering)+                 
  theme(panel.grid.major = element_line(color = NA,                           #Define plot theme (background colour, lines,...)   
                                        linetype = "dashed", 
                                        size = 0),                                
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))                                 


#number of samples in the dataset 
ECU_count <- data %>% count(gid_1)

ECU_fused <- merge(ECU_mun, ECU_count , by.x ="GID_1", by.y="gid_1", all.x=TRUE) 


#Lfuse the data exel file with the raster data

Samples_states <- merge(ECU_mun, data, by.x = "GID_1", by.y = "gid_1")     #Merge yout data with shapefile
Samples_states <- st_as_sf(Samples_states)        



#Figure 1B

ggplot() + 
  geom_sf(data=ECU_fused, aes(fill = log(n)), alpha= 0.8)+ #Set colour of muns according to sample number
  scale_fill_viridis(option='E', begin= 0, end=1, na.value = "white")+ #Define colour scheme
  theme(panel.grid.major = element_line(color = NA,                         #Define plot theme (background colour, lines,...)      
                                        linetype = "dashed", 
                                        size = 0),                                
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("XXXXXX.pdf")
