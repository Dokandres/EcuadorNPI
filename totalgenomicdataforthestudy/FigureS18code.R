################################################################################
#                                                                              #
# Purpose:       Code to reproduce Figure S18. Detail of SARS-CoV-2 lineages   #
#                in Ecuador.                                                   #
#                                                                              #
# Author:        Ben Wulf/Andres Moreira-Soto                                  #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2022-01-06                                                    #
# Last updated:  2022-01-06                                                    #
# Source:                                                                      #
#                                                                              #
# Comment:                                                                     #
#                                                                              #
################################################################################

########################### Load required libraries ############################

{
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(sf)
library(raster)
library(viridis)
library(scales)  
library (epitools)
library(ggrepel)
library(ggridges)
library(egg)
library(dplyr)
}


################### set working directory and load the data ####################

dataecu <- read.csv("nextcladeProofedSamplenamesWithDates.csv",sep=",")
dataecu$Lineage <- vapply(dataecu$name,function(x){strsplit(x,"/")[[1]][2]},"")
dataecu$date <- as.Date(dataecu$date,format = "%Y-%m-%d")


###################################################
#                                                 #
#          Heatmap & Frequency Plot               #
#                                                 #
###################################################

voi <- c("B.1.526","B.1.621","C.37","B.1.621.1")
voc <- c("B.1.1.7","P.1.3","P.1.2","P.1.10","P.1","P.1.12","P.1.9","B.1.617.2","AY.6","AY.4","AY.1","AY.20","AY.25","AY.12")

lineageColors <- c("#EBC7B2","#9C9CFF","#9CFFFF","#52CC52","#FF0303","#FF99FF","#000000")
names(lineageColors) <- c("Gamma","A.2.5","Iota","Mu","Alpha","Delta","other")

aliasList <- list(
  "Alpha"=c ("B.1.1.7"),
  "Gamma"=c("P.1.3","P.1.2","P.1.10","P.1","P.1.12","P.1.9"),
  "Delta"=c("B.1.617.2","AY.6","AY.4","AY.1","AY.20","AY.25","AY.12","AY.26","AY.33"),
  "Kappa"=c("B.1.617.1"),
  "Mu"=c("B.1.621","B.1.621.1"),
  "Iota"=c("B.1.526"),
  "A.2.5"=c("A.2.5","A.2.5.2","A.2.5.3","A.2.5.5")
)

labledLineages <- paste(gsub('\\.\\.','.2.5',gsub('[[:digit:]]+', '', names(unlist(aliasList)))),"-",unlist(aliasList)) 

qq <- dataecu %>% group_by(month= lubridate::floor_date(date, "month"),Lineage)%>% summarise(date,Lineage,"Occurences / Month"=n()) 
qq$VariantType <- "other"
qq$VariantType[qq$Lineage %in% unlist(aliasList["Alpha"])] <- "Alpha"
qq$VariantType[qq$Lineage %in% unlist(aliasList["Gamma"])] <- "Gamma"
qq$VariantType[qq$Lineage %in% unlist(aliasList["Delta"])] <- "Delta"
qq$VariantType[qq$Lineage %in% unlist(aliasList["Kappa"])] <- "Kappa"
qq$VariantType[qq$Lineage %in% unlist(aliasList["Mu"])] <- "Mu"
qq$VariantType[qq$Lineage %in% unlist(aliasList["A.2.5"])] <- "A.2.5"
qq$VariantType[qq$Lineage %in% unlist(aliasList["Iota"])] <- "Iota"


qq$VariantClass <- "other"
qq$VariantClass[qq$Lineage %in% voi] <- "WHO VOI"
qq$VariantClass[qq$Lineage %in% voc] <- "WHO VOC"
qq$VariantClass <- factor (qq$VariantClass, levels=c("WHO VOC","WHO VOI","other"))




qq$Lineage2 <- qq$Lineage

qq$Lineage2[qq$VariantType!="other"] <-  paste(qq$VariantType[qq$VariantType!="other"] ,"-", qq$Lineage[qq$VariantType!="other"])

# Sort Levels
qq$Lineage2 <- factor(qq$Lineage2 ,levels=rev(unique(c(labledLineages,sort(qq$Lineage2)))))


endOfObservation <- lubridate::floor_date(max(dataecu[!grepl("EPIISL",dataecu$name),]$date),"month") + 14


minmaxLines <- qq %>% group_by(Lineage2) %>% summarise(Lineage2 , minDate=min(month),maxDate=max(month)) %>% distinct()%>% pivot_longer(names_to="mima",cols= c("minDate","maxDate"))
testLines = data.frame(
  Lineage=c(qq$Lineage[1],qq$Lineage[1]),
  date =c(min(qq$month),max(qq$month)))


heatmapP <- ggplot()+
  geom_tile(data=qq, aes(x = month, y=Lineage2,alpha=`Occurences / Month`, fill= VariantType),color="lightgrey",height=.7)+
  xlab("Sample Date")+ 
  ylab("Lineage")+
  guides(color = 'legend')+
  scale_size_area()+
  scale_x_date(date_breaks = "1 month", expand=c(0,0),date_labels = "%m-%y",limits=as.Date(c('2020-01-14','2021-09-30')))+
  theme(panel.grid.major.x = element_line(
                                        linetype = "solid", 
                                        size = 0.5, color="lightgrey"),
        plot.title = element_text(hjust = 0.5,face="bold"),
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))+
  scale_fill_manual(values=lineageColors)+
  ggtitle("Total Diversity of SARS-CoV-2 in Ecuador")+
  labs( linetype = "Line Type", fill= "Variant Type")+
  geom_line(data=minmaxLines,aes(x=value,y=Lineage2),color="darkgrey") +
  geom_vline(xintercept=endOfObservation,color="red")

print(heatmapP)

