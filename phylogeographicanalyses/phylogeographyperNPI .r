
################################################################################
#                                                                              #
# Purpose:       Code to reproduce the phylogeographic analyses in Ecuador     #
#                                                                              #
# Author:        Andres Moreira-Soto, Ben Wulf                                 #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2023-01-12                                                    #
# Last updated:  2023-01-12                                                    #
# Source:                                                                      #
#                                                                              #
# Comment:                                                                     #
#                                                                              #
################################################################################

########################### Load required libraries ############################

{
library(dplyr)
library (readr)
library (RColorBrewer)
library (sf)
library(rgdal)
library (ggmap)
library (ggspatial)
library (raster)
library (rmapshaper)
library(seraphim)
library(ggridges)
library (lubridate)
library(ggplot2)
library(rmapshaper)
}

##############1. Extracting the spatio-temporal information contained in posterior trees############################

setwd("")

treefile<- "filteredB11st2.trees.txt"

localTreesDirectory = ""
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 36
mostRecentSamplingDatum = 2020.9945355191257
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)


############ 2. Extracting the spatio-temporal information embedded in the MCC tree######################################

treefile<- "filteredB11st2.tree"

source("mccExtractions2.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccTreeExtraction(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "Ecuadorb11.csv", row.names=F, quote=F)  
#this file you get from the beast run ( phylogeography R) 
mcc_tabDF <- read.csv("Ecuadorb11.csv",header=T)

coordinates <- read_delim("statescoordinates.tsv.txt", col_names=c("name", "Lat", "Lon"))
originalPos <- as.data.frame(coordinates)
originalPos <- originalPos %>% select (Lat, Lon)


jitterRemovalhelper <- function (df,get="Lat"){
  
  distList <- apply(originalPos,1,function(x){abs(x["Lon"]-df['lon'])+abs(x["Lat"]-df["lat"]) })
  
  return(originalPos[which.min(distList),get])
  
}

jitterRemoval <- function (lat,lon,get='Lat')
{ 
  query = data.frame("lat"=lat,"lon"=lon)
  
  nearst <- apply(query,1,function(x){jitterRemovalhelper(x,get=get) })
  return(nearst)
  
  
}


##################### Classification and removal of Beast jitter##########################
mcc_tabDFperiod <- mcc_tabDF %>% mutate(period=as.factor(ifelse(endYear<2020.208,"noNPI",ifelse(2020.705<endYear,"lowNPI","highNPI"))),
                                       startLonR=jitterRemoval(lon=startLon,lat=startLat,'Lon'),
                                       startLatR=jitterRemoval(lon=startLon,lat=startLat,'Lat'),
                                       endLonR=jitterRemoval(lon=endLon,lat=endLat,'Lon'),
                                       endLatR=jitterRemoval(lon=endLon,lat=endLat,'Lat'))

mcc_tabDFperiod$period <- factor (mcc_tabDFperiod$period, levels =c("noNPI", "highNPI", "lowNPI"))

# convert percent dates 
mcc_tabDFperiod$datess <- as.Date (date_decimal (mcc_tabDFperiod$endYear))

#summary of transitions that are not normalized 
summary(mcc_tabDFperiod$period)

# Compress to one connection per state combination
mcc_tabDFperiod2 <- mcc_tabDFperiod %>% group_by(period,startLonR,startLatR,endLonR,endLatR) %>% summarise(counte=n())

#summary of transitions normalized by startlatlon and endlatlon (equal transitions from a to b)
summary(mcc_tabDFperiod$period)

# remove self loops 
mcc_tabDFperiod_noSelfLoop  = mcc_tabDFperiod2 %>% ungroup() %>% filter(startLonR != endLonR | startLatR != endLatR)
mcc_tabDFperiod_noSelfLoop$period <- factor (mcc_tabDFperiod_noSelfLoop$period, levels =c("noNPI", "highNPI", "lowNPI"))

#summary of transitions normalized by startlatlong !=  endlatlong

summary (mcc_tabDFperiod_noSelfLoop$period )


ECU_munPre <- sf::st_as_sf(raster::getData('GADM', country='ECU', level=1))
ECU_mun <- ms_simplify (ECU_munPre, keep = 0.001 ,keep_shapes = TRUE)

c1<- rev(brewer.pal(11,"RdYlBu"))

ggplot() +
  geom_sf(data=ECU_mun,fill="lightgray") +
  coord_sf (xlim= c(-82.0, -75.0))+
  geom_curve(aes(x = startLonR, 
                  y = startLatR, 
                  xend = endLonR, 
                  yend = endLatR,
                 size=counte,
             color = counte), lwd=0.8, 
                  arrow = arrow(type = "closed",length = unit(0.03, "npc"))
              ,data=mcc_tabDFperiod_noSelfLoop )+
  scale_color_gradient (low=c1[2],high=c1[11])+
  theme_classic()+
  facet_wrap(~period,ncol=NULL)
 
# count per period 

freq(mcc_tabDFperiod$period)

ggplot () + 
  geom_histogram (data=mcc_tabDFperiod, aes(x= datess, fill=period)) +
  scale_x_date(date_breaks = "7 days",
               date_labels = "%Y %m %d",)+
  scale_y_continuous(name ="count", breaks = seq(0, 300, 10))#+
  #theme_Publication()


# Figure 4. Evolution, spread and social determinants of SARS-CoV-2 in Ecuador. 4E ############################################################### 

ggplot () + 
  geom_density_ridges (data=mcc_tabDFperiod, aes(x= endYear, y=period, fill=period)) +
  scale_x_continuous(name ="Date", breaks = seq(2020.0, 2020.99, 0.10))#+
  #theme_Publication()+


