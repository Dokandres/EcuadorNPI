library(seraphim)
library(rmapshaper)
library(diagram)
library(raster)
library(tidyverse)
library(ggridges)

############ 2. Extracting the spatio-temporal information embedded in the MCC tree######################################

treefile<- "onlyb11filteredstrict.tree"

source("mccExtractions2.r")
mostRecentSamplingDatum = 2020.9945355191257
mcc_tree = readAnnotatedNexus(treefile)
mcc_tab = mccTreeExtraction(mcc_tree, mostRecentSamplingDatum)
write.csv(mcc_tab, "Ecuadorb11.csv", row.names=F, quote=F)  

#this file you get from the beast run ( phylogeography R) 
mcc_tabDF <- read.csv("Ecuadorb11.csv",header=T)

coordinates <- read_delim("statescoordinates.tsv.txt")
originalPos <- as.data.frame(coordinates)
originalPos <- originalPos %>% select (Lat=lat, Lon=long)


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

#classify as intra state or extra state transitions

mcc_tabDFperiod <- mcc_tabDFperiod %>% mutate (intrastate= ifelse (startLonR != endLonR | startLatR != endLatR, "extra", "intra"))
#intra and extra transitions

table (mcc_tabDFperiod$period, mcc_tabDFperiod$intrastate )

# convert percent dates 
mcc_tabDFperiod$datess <- as.Date (date_decimal (mcc_tabDFperiod$endYear))

#summary of transitions that are not normalized 
summary(mcc_tabDFperiod$period)

# Compress to one connection per state combination
mcc_tabDFperiod2 <- mcc_tabDFperiod %>% group_by(period,startLonR,startLatR,endLonR,endLatR, intrastate) %>% summarise(counte=n())

mcc_tabDFperiod2$counte <- as.numeric(mcc_tabDFperiod2$counte)

#summary of transitions normalized by startlatlon and endlatlon (equal transitions from a to b)
summary(mcc_tabDFperiod2$period)
# remove self loops 
mcc_tabDFperiod_noSelfLoop  = mcc_tabDFperiod2 %>% ungroup() %>% filter(startLonR != endLonR | startLatR != endLatR)
mcc_tabDFperiod_noSelfLoop$period <- factor (mcc_tabDFperiod_noSelfLoop$period, levels =c("noNPI", "highNPI", "lowNPI"))

#summary of transitions normalized by startlatlong !=  endlatlong

summary (mcc_tabDFperiod_noSelfLoop$period )


ECU_munPre <- sf::st_as_sf(raster::getData('GADM', country='ECU', level=1))
ECU_mun <- ms_simplify (ECU_munPre, keep = 0.001 ,keep_shapes = TRUE)

c1<- rev(brewer.pal(11,"RdYlBu"))

ggplot() +
  geom_sf(data=ECU_mun,fill="white") +
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
  geom_density_ridges (data=mcc_tabDFperiod, aes(x= endYear, y=counte, fill=intrastate)) +
  scale_x_continuous(name ="Date", breaks = seq(2020.0, 2020.99, 0.10))#+
#theme_Publication()+

ggplot () + 
  geom_histogram (data=mcc_tabDFperiod, aes(x= endYear, fill=intrastate) ,position = "dodge") +
  #scale_x_continuous(name ="Date", breaks = seq(2020.0, 2020.99, 0.10))+
  scale_fill_manual(values = c("darkred", "darkgray"))+
  facet_wrap(~period)+
  theme_minimal()
#theme_Publication()+

ggplot () + 
  geom_bar (data=mcc_tabDFperiod, aes(x= period, fill=intrastate), position="dodge") +
  #scale_x_continuous(name ="Date", breaks = seq(2020.0, 2020.99, 0.10))+
  scale_fill_manual(values = c("darkred", "darkgray"))+
  theme_classic()
