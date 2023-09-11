################################################################################
#                                                                              #
# Purpose:       Code to reproduce time-stamped phylogenetic tree Figure 4A 4B #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2022-01-05                                                    #
# Last updated:  2022-01-05                                                    #
# Source:                                                                      #
#                                                                              #
# Comment:                                                                     #
#                                                                              #
################################################################################


########################### Load required packages ############################
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
library(ggtree)
library(tidytree)
library(ape)
library(treeio)
library (RColorBrewer)
library(zoo)
library(pastecs)
}

################### load the data ####################

tree<-read.nexus('timetree.nexus')
metadata_df <- read_csv("nextcladeProofedSamplenamesWithDates.csv")
metadata_df$pango_lineage <- as.character(sapply(strsplit(metadata_df$name , "/"), "[[", 2))
metadata_df$namesep <- as.character(sapply(strsplit(metadata_df$name , "/"), "[[", 1))
metadata_df$moddates <- floor_date (metadata_df$date, unit= "month")
metadata_df$charsumm <- substr(metadata_df$pango_lineage , 1, 5)
metadata_df$newname <- ifelse(metadata_df$pango_lineage == "B.1.1.7", "Alpha", 
                      ifelse(metadata_df$pango_lineage == 'B.1.351', "Beta", 
                      ifelse(metadata_df$pango_lineage == 'P.1', "Gamma" ,
                      ifelse(metadata_df$pango_lineage == 'B.1.427', "Epsilon" ,
                      ifelse(metadata_df$pango_lineage == 'B.1.617.2', "Delta" ,
                      ifelse(metadata_df$pango_lineage == 'B.1.525', "Eta" ,
                      ifelse(metadata_df$pango_lineage == 'B.1.526', "Iota" ,
                      ifelse(metadata_df$pango_lineage == 'B.1.617.1', "Kappa" ,
                      ifelse(metadata_df$pango_lineage == 'B.1.621', "Mu" ,
                      ifelse(metadata_df$pango_lineage == 'P.2', "Zeta" ,
                             ifelse(metadata_df$pango_lineage == 'A.2.5', "A.2.5" ,
                             
                                    "Other Lineages")))))))))))


# classify if this study or GISAID retrieved sequences
metadata_df$source <- ifelse(grepl ("EPIISL", metadata_df$namesep), "GISAID", "thisstudy" )                                

#tree figure without colored tips

tree1<-ggtree(tree, mrsd="2019-11-01",as.Date=TRUE, color='grey80',size=0.2) + theme_tree2()+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  geom_rootedge(rootedge = 50, color="grey80")+
  theme(axis.text.x = element_text(size=10,angle=90))+
  vexpand(.02, direction= 1)+
  vexpand(.02, direction= -1)

tree1

Figure4panelA <- tree1 %<+% metadata_df + 
  
  #order as in variants alpha..beta...
  #alpha
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.1.7')),fill='grey',size=3, stroke=0.2,color='grey30',shape=21) +
  #beta
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.351')),fill='mediumorchid3',size=3,align=F, stroke=0.2,color='purple4',shape=21)+
  #gamma
  geom_tippoint(aes(
    subset=(pango_lineage=='P.1')),fill='peachpuff3',size=3, stroke=0.2,align=F, color='peachpuff4',shape=21)+
  #epsilon
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.427' | pango_lineage=='B.1.429' )),fill='darkgrey',size=3,align=F, stroke=0.2,color='grey30',shape=21)+
  #delta
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.617.2')),fill='tan4',size=3, stroke=0.2,align=F, color='salmon4',shape=21)+
  #eta 
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.525')),fill='grullo',size=3, stroke=0.2,align=F, color='salmon4',shape=21)+
  #Iota B.1.526
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.526')),fill='skyblue1',size=3, stroke=0.2,align=F, color='skyblue4',shape=21)+
  #kappa B.1.617.1
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.617.1')),fill='coral',size=3, stroke=0.2,align=F, color='coral',shape=21)+
  # a.2.5
  geom_tippoint(aes(
    subset=(pango_lineage=='A.2.5')),fill='cadetblue3',size=3, stroke=0.2,align=F, color='cadetblue4',shape=21)+
  #mu
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.621' | pango_lineage=='B.1.621.1')),fill='skyblue4',size=3, stroke=0.2,align=F, color='darkslategrey',shape=21)+
  #1.617.3
  geom_tippoint(aes(
    subset=(pango_lineage=='B.1.617l.3')),fill='darkgreen',size=3, stroke=0.2,align=F, color='grey',shape=21)+
  #Zeta
  geom_tippoint(aes(
    subset=(pango_lineage=='P.2')),fill='Taupe',size=3, stroke=0.2,align=F, color='Black',shape=21)+
  #select only the ones that dont have episl on them , which are the ones from our new dataset
  geom_tippoint(aes(
    subset=(!grepl("EPIISL", namesep))),fill='black',size=1.5,align=F,stroke=0.2,color='black',shape=21, alpha =0.7)+
  
  theme(axis.text.x = element_text(vjust=0.5, hjust=1))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.position = "top")

Figure4panelA

ggsave('Ecutreeforpub1.pdf')

################################## Figure 4B ###################################

#transform dates into as.factor to use them in ggplot
metadata_df$simpledates <- as.factor(as.yearmon(metadata_df$moddates))
metadata_df$dates2 <- format (lubridate::parse_date_time(x= metadata_df$moddates, orders = c("Y-m-d")), "%m-%Y")
#transform the data
metadatasummary <- metadata_df %>% 
  #select the dataset that i need
  select (simpledates,pango_lineage) %>% 
  #group by dates
  group_by (simpledates) %>% 
  #sum the number of unique pangolineages that are in the dataset
  dplyr::summarise (unique(pango_lineage)) %>% 
  #do acol with the number of lineages
  dplyr::summarise(countpangos= n())


#divide between low high NPIs 

selectlowNPI<- filter (metadatasummary, simpledates %in% c("Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020"))

selecthighNPI <- filter (metadatasummary, simpledates %in% c("Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020"))

stat.desc(selecthighNPI)
stat.desc(selectlowNPI)


Figure4B <- ggplot (metadatasummary, aes(simpledates, countpangos, group=1))+
  #geom_point (size=2)+
  #geom_line (size =1)+
  geom_smooth (span=0.4, color="darkred")


Figure4B

#to use for the pearson correlation of number of lineages
write.csv(metadatasummary,"/numberoflineages.csv", row.names = FALSE)  

################################## figuressupplementary 17a and b ##################################

#raw number of genomes biweekly Figure S17A

rawnumbergenomes <- ggplot(metadata_df, aes(x= date, Y=source, fill=source))+
  geom_histogram(alpha=5, size=1, color ="black" , position= "stack") +
  xlab("Sampling date")+
  ylab("Biweekly number of genomes")+
  geom_text(aes(y = ..count.., label = ..count..), size =5, stat = "bin", vjust = -0.5)+
  scale_x_date(date_breaks = "15 days", date_labels = "%d-%m-%y")+
  scale_fill_manual(values =c("grey", "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=-1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))#+
        #theme_Publication(22)

ggsave("plot number of samplesreadylinehistogram.pdf")

#percent number of genomes

percentnumbergenomes <- ggplot(metadata_df, aes(x= date, Y=source, fill=source))+
  geom_histogram(alpha=5, size=1, color ="black",  position= "fill") +
  xlab("Sampling date")+
  ylab("Biweekly percent of genomes")+
  #geom_text(aes(y = ..count.., label = ..count..), stat = "bin", vjust = -1)+
  scale_x_date(date_breaks = "15 days", date_labels = "%d-%m-%y")+
  scale_fill_manual(values =c("grey", "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))#+
      #theme_Publication(22)

ggsave("plot number of samplesreadylinehistogram.pdf")

ggarrange(rawnumbergenomes, percentnumbergenomes, ncol = 1, nrow = 2)


#Figure of Variant distributions lineages

figures17b<- 
  ggplot(data=metadata_df, mapping = aes(x = moddates, fill=charsumm ))+
  geom_bar(position= "fill", width=27,color='black')+
  #geom_bar(width=5)+
  #geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month", limits = as.Date(c('2020-02-01','2021-09-01')))+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=15))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  #scale_fill_manual(values = custom3, name='Lineage')+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.position = "top")+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  xlab('Date')+
  #ylab('Genome Count')
  ylab('Proportion of Genomes')


figures17b

ggsave('Eculineagesdistr1.pdf')





