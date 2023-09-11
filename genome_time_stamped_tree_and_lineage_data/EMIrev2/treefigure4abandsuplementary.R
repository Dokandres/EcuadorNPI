################################################################################
#                                                                              #
# Purpose:       Code to reproduce time-stamped phylogenetic tree Figure 4A 4B #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2022-01-05                                                    #
# Last updated:  2023-08-15                                                    #
# Source:                                                                      #
#                                                                              #
# Comment: this code was updated to add more sequences from GISAID             #
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
library(colorspace)
library (ggridges)
library(ggeasy)
library(phytools)
  }

################### load the data ####################
# set the system to english ?
Sys.setlocale("LC_ALL", "English")

custom3 <- c(
             'darkseagreen4', 'slategray1', 'thistle3', 'darkolivegreen', 'darkseagreen3', 'darkseagreen2', 'red3', 'grey30', 'plum2',
             'white', 'black', 'mediumpurple', 'blue3', 'gray40', 'cadetblue3','antiquewhite2', 'tan4', 'peachpuff3', 'palegreen3', 'dodgerblue1', 'hotpink2', 'skyblue1', 'purple4', 'mediumorchid3',
             'darkorange3', 'seagreen3', 'orchid3', 'olivedrab3', 'turquoise3', 'maroon3', 'sandybrown', 'darkcyan', 'salmon3',
             'mediumvioletred', 'green3', 'darkblue', 'darkslategray3', 'darkgoldenrod3', 'cornflowerblue', 'steelblue3', 'sienna3',
             'mediumspringgreen', 'darkmagenta', 'orangered3', 'royalblue3', 'firebrick3', 'mediumseagreen', 'darkslateblue', 'indianred3',
             'deepskyblue3', 'gold3', 'mediumaquamarine', 'darksalmon', 'mediumorchid', 'chartreuse3', 'lightsalmon3', 'deepskyblue4',
              'darkolivegreen2', 'mediumturquoise', 'rosybrown2', 'mediumslateblue', 'goldenrod2')



tree<-read.nexus('timetree.nexus')
treereroot <- midpoint.root(tree)

metadata_df <- read_delim("datesnew.tsv")
metadata_df$source <- ifelse(grepl("EPIISL", metadata_df$nameseq), "GISAID", "This study")
metadata_df$moddates <- floor_date (metadata_df$date, unit= "month")
metadata_df$charsumm <- substr(metadata_df$pangolineage , 1, 5)
metadata_df$newname <- ifelse(metadata_df$pangolineage == "B.1.1.7", "Alpha", 
                      ifelse(metadata_df$pangolineage == 'B.1.351', "Beta", 
                      ifelse(metadata_df$pangolineage == 'P.1', "Gamma" ,
                      ifelse(metadata_df$pangolineage == 'B.1.427', "Epsilon" ,
                      ifelse(metadata_df$pangolineage == 'B.1.617.2', "Delta" ,
                      ifelse(metadata_df$pangolineage == 'B.1.525', "Eta" ,
                      ifelse(metadata_df$pangolineage == 'B.1.526', "Iota" ,
                      ifelse(metadata_df$pangolineage == 'B.1.617.1', "Kappa" ,
                      ifelse(metadata_df$pangolineage == 'B.1.621', "Mu" ,
                      ifelse(metadata_df$pangolineage == 'P.2', "Zeta" ,
                             ifelse(metadata_df$pangolineage == 'A.2.5', "A.2.5" ,
                             
                                    "Other Lineages")))))))))))

#transform dates into as.factor to use them in ggplot
metadata_df$simpledates <- as.factor(as.yearmon(metadata_df$moddates))

metadata_df$dates2 <- format (lubridate::parse_date_time(x= metadata_df$moddates, orders = c("Y-m-d")), "%m-%Y")

#tree figure without colored tips

tree1<-ggtree(treereroot, mrsd="2021-09-01",as.Date=TRUE, color='grey80',size=0.2) + theme_tree2()+
  scale_x_date(date_labels = "%b",date_breaks = "1 month")+
  geom_rootedge(rootedge = 20, color="grey80")+
  theme(axis.text.x = element_text(size=10,angle=90))+
  vexpand(.02, direction= 1)+
  vexpand(.02, direction= -1)

tree1

Figure4panelA <- tree1 %<+% metadata_df + 
  
  #order as in variants alpha..beta...
  #alpha
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.1.7')),fill='grey',size=3, stroke=0.2,color='grey30',shape=21) +
  #beta
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.351')),fill='mediumorchid3',size=3,align=F, stroke=0.2,color='purple4',shape=21)+
  #gamma
  geom_tippoint(aes(
    subset=(pangolineage=='P.1')),fill='peachpuff3',size=3, stroke=0.2,align=F, color='peachpuff4',shape=21)+
  #epsilon
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.427' | pangolineage=='B.1.429' )),fill='darkgrey',size=3,align=F, stroke=0.2,color='grey30',shape=21)+
  #delta
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.617.2')),fill='tan3',size=3, stroke=0.2,align=F, color='tan4',shape=21)+
  #eta 
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.525')),fill='salmon2',size=3, stroke=0.2,align=F, color='salmon4',shape=21)+
  #Iota B.1.526
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.526')),fill='skyblue1',size=3, stroke=0.2,align=F, color='skyblue4',shape=21)+
  #kappa B.1.617.1
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.617.1')),fill='coral',size=3, stroke=0.2,align=F, color='coral',shape=21)+
  # a.2.5
  geom_tippoint(aes(
    subset=(pangolineage=='A.2.5')),fill='cadetblue3',size=3, stroke=0.2,align=F, color='cadetblue4',shape=21)+
  #mu
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.621' | pangolineage=='B.1.621.1')),fill='skyblue4',size=3, stroke=0.2,align=F, color='darkslategrey',shape=21)+
  #1.617.3
  geom_tippoint(aes(
    subset=(pangolineage=='B.1.617l.3')),fill='darkgreen',size=3, stroke=0.2,align=F, color='grey',shape=21)+
  #Zeta
  geom_tippoint(aes(
    subset=(pangolineage=='P.2')),fill='Taupe',size=3, stroke=0.2,align=F, color='Black',shape=21)+
  #select only the ones that dont have episl on them , which are the ones from our new dataset
  geom_tippoint(aes(
    subset=(grepl("This study", source))),fill='black',size=1.5,align=F,stroke=0.2,color='black',shape=21, alpha =0.7)+
  
  theme(axis.text.x = element_text(vjust=0.5, hjust=1))+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.position = "top")

Figure4panelA

ggsave('Ecutreeforpubrev.pdf', width = 6, height = 6, units = "in", dpi = 300)

################################## Figure 4B ###################################
# transform the data as how many sequences per source per month 

metadatasumsource <- metadata_df %>% 
  #select the dataset that i need
  select (simpledates,source) %>% 
  #group by dates
  group_by (simpledates,source) %>% dplyr::summarise(source) %>% dplyr::summarize(count= n())



#transform the data as how many lineages per time 
metadatasummary <- metadata_df %>% 
  #select the dataset that i need
  select (simpledates,pangolineage) %>% 
  #group by dates
  group_by (simpledates) %>% 
  #sum the number of unique pangolineages that are in the dataset
  dplyr::summarise (unique(pangolineage)) %>% 
  #do acol with the number of lineages
  dplyr::summarise(countpangos= n())


#divide between low high NPIs 

metadata_df$NPI <- ifelse (metadata_df$simpledates %in% c("Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020"), "LOWNPI",
                  ifelse (metadata_df$simpledates %in% c("Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020"), "HIGHNPI", "NAnalized"))



Figure4B <- ggplot (metadatasummary, aes(simpledates, countpangos, group=1))+
  #geom_point (size=2)+
  #geom_line (size =1)+
  ylab("Number of lineages")+
  geom_smooth (span=0.4, color="darkred")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

Figure4B

ggsave("plotnumberoflineages.pdf", Figure4B,width = 6, height = 4, units = "in", dpi = 300)

#to use for the pearson correlation of number of lineages
#write.csv(metadatasummary,"/numberoflineages.csv", row.names = FALSE)  

################################## figuressupplementary 17a and b ##################################

#percent number of genomes

percentnumbergenomes <- ggplot(metadata_df, aes(x= simpledates, fill=source))+
  geom_bar(size=1,  position= "fill") +
  labs(x = NULL)+
  ylab("Monthly percent of genomes")+
  #geom_text(aes(label = ..count..), stat = "bin", vjust = 1.5)+
  #scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y", limits = c() )+
  scale_fill_manual(values =c("grey", "red")) +
  theme(axis.text.x = element_blank())+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = NA), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))


#percentnumbergenomes

###############################################################################################

monthly_counts <- metadata_df %>%
  group_by(simpledates, source) %>%
  summarize(count = n())

barplot <- ggplot(monthly_counts, aes(x = simpledates, y = count, fill=source)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values =c("grey", "red")) +
    xlab("Sampling date") +
  ylab("Number of Genomes")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))


#barplot



numberofgenomes <- ggarrange(percentnumbergenomes, barplot, ncol = 1, nrow = 2)

#numberofgenomes

ggsave("plotnumberandpropgenomes.pdf", numberofgenomes,width = 6, height = 4, units = "in", dpi = 300)

#Figure of Variant distributions lineages

figures17b<- 
  ggplot(data=metadata_df, mapping = aes(x = moddates, fill=charsumm ))+
  geom_bar(position= "fill", width=27,color='black')+
  #geom_bar(width=5)+
  #geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=7))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month", limits = as.Date(c('2020-02-01','2021-09-01')))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(color="black", size=7))+
  scale_fill_manual(values = custom3, name='Lineage')+
  theme(legend.text = element_text(size=7))+
  theme(legend.title = element_text(size=7))+
  #theme(legend.position = "none")+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  xlab('Date')+
  #ylab('Genome Count')
  ylab('Proportion of Genomes')


#figures17b

ggsave("plotnumberoflineagepercent.pdf", figures17b ,width = 6, height = 4, units = "in", dpi = 300)



figures17b2<- 
  ggplot(data=metadata_df, mapping = aes(x = moddates, y=charsumm, color=charsumm ))+
  geom_line()+
  geom_point(aes(alpha=0.3))+
  #geom_bar(width=5)+
  #geom_bar(width=5,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=7))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month", limits = as.Date(c('2020-02-01','2021-09-01')))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(color="black", size=7))+
  scale_color_manual(values = custom3, name='Lineage')+
  theme(legend.text = element_text(size=7))+
  theme(legend.title = element_text(size=7))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  xlab('Date')+
  #ylab('Genome Count')
  ylab('Detection time of Genomes')


#figures17b2

ggsave("plotnumberoflineagesintime.pdf", figures17b2 ,width = 6, height = 4, units = "in", dpi = 300)

#transform the data
metadatasummary2 <- metadata_df %>% 
  #select the dataset that i need
  select (moddates,charsumm) %>% 
  #group by dates
  group_by (moddates, charsumm) %>% 
  #sum the number of unique pangolineages that are in the dataset
  dplyr::summarise(pangosum= n())

figures17b3 <- ggplot(data = metadatasummary2, aes(x = moddates, y = pangosum, group = charsumm, color= charsumm)) +
  geom_line(size=0.8) +
  theme_classic()+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month", limits = as.Date(c('2020-02-01','2021-09-01')))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(color="black", size=7))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(color="black", size=7))+
  scale_color_manual(values = custom3, name='Lineage')+
  theme(legend.text = element_text(size=7))+
  theme(legend.title = element_text(size=7))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  xlab("Date") +
  ylab("Number of genomes per lineage") 
  #scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month", limits = as.Date(c("2020-02-01", "2021-09-01"))) +

#figures17b3

ggsave("plotnumberoflineages.pdf", figures17b3,width = 6, height = 4, units = "in", dpi = 300)

allplotsgenomiclineages <- ggarrange(figures17b, figures17b2, figures17b3, ncol=1, nrow = 3)

#allplotsgenomiclineages

ggsave("allplotsgenomedist.pdf", allplotsgenomiclineages,width = 6, height = 4, units = "in", dpi = 300)

#totalnumberoflineagesinthewholedataset

totalnumberlineages <- metadata_df %>% group_by(pangolineage) %>% summarise(counts=n()) %>% arrange(desc(counts))

figurelin<- 
  ggplot(data=totalnumberlineages, mapping = aes(x = reorder(pangolineage, -counts), y = counts))+
  geom_bar(stat = "identity", fill = "darkgray")+
  theme_classic()+
  easy_rotate_x_labels(90)+
  theme(axis.text.x = element_text(color="black", size=7))+
  theme(axis.text.y = element_text(color="black", size=7))+
xlab("Pango lineages") 
  

#figurelin

ggsave("plotnumberoflineagestotal.pdf", figurelin ,width = 6, height = 4, units = "in", dpi = 300)



#transform the data as how many lineages per time 
metadatasummary3 <- metadata_df %>% 
  #select the dataset that i need
  select (pangolineage,state, NPI) %>% 
  #group by dates
  group_by (state, NPI) %>% 
  #sum the number of unique pangolineages that are in the dataset
  dplyr::summarise (unique(pangolineage)) %>% 
  #do acol with the number of lineages
  dplyr::summarise(countpangos= n())

#Load country boarders
library(sf)
library(sp)
library(rmapshaper)
library(fuzzyjoin)
library(raster)

States_ECUPRE <- st_as_sf (getData('GADM', country='ECU', level=1)) 
States_ECU <- ms_simplify (States_ECUPRE, keep = 0.01 ,keep_shapes = TRUE)
Boarders_ECUPRE <- st_as_sf(getData('GADM', country='ECU', level=0))
Boarders_ECU <- ms_simplify (Boarders_ECUPRE, keep = 0.001 ,keep_shapes = TRUE)


metadatasummary3$state <- str_to_lower(metadatasummary3$state)
metadatasummary3 <- metadatasummary3 %>%  mutate(state = ifelse(state == "quito", "pichincha", 
                                                                ifelse(state == "santo_domingo", "Santo Domingo de los Tsachilas", state)))

metadatasummap <- metadatasummary3 %>% stringdist_left_join(States_ECU, by= c("state" ="NAME_1" ), max_dist = 3)

#merge complete data                                                                                            

Samples_states <- st_as_sf(metadatasummap)     #Make the merged data a shapefile again to plot 

ggplot() +
  geom_sf(data=States_ECU, fill=NA, color="black", show.legend = FALSE)+
  geom_sf(data=Samples_states,aes(fill= countpangos, show.legend = TRUE))+
  #geom_sf_label(data = st_centroid(Samples_states), aes(label = countpangos), size = 3) +  # Add labels to centroids
  coord_sf(xlim=c(-75, -81.5))+
  facet_wrap(vars(NPI))+
  scale_fill_viridis_c(begin = 0, end = 1, option= "E", breaks = seq(0, max(Samples_states$countpangos), by = 2))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("Ecuadorpositivitypertimesturbo.pdf")

#add Ecuador classification of provinces = Coastal , andean , amazon 

metadatasummap$natregion <- ifelse (metadatasummap$state %in% c("esmeraldas", "manabi", "santa_elena", "guayas", "los_rios", "el_oro"), "coastal",
                             ifelse (metadatasummap$state %in% c("sucumbios", "orellana", "pastaza", "morona_santiago", "zamora_chinchipe", "napo"), "amazon",
                                     ifelse(metadatasummap$state %in% c("galapagos"), "island", "andean")))


relative_change <- metadatasummap %>% filter (NPI != "NAnalized") %>%
  group_by(natregion) %>%
  summarize(
    rel_change = ifelse(
      sum(countpangos[NPI == "HIGHNPI"]) != 0,
      ((sum(countpangos[NPI == "LOWNPI"]) - sum(countpangos[NPI == "HIGHNPI"])) / sum(countpangos[NPI == "HIGHNPI"])* 100),
      NA
    )
  )

relative_change$rel_change <- -relative_change$rel_change


ggplot(relative_change, aes(x = natregion, y = rel_change)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "State", y = "Percent Change from Baseline (%)") +
  theme_classic () +
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
