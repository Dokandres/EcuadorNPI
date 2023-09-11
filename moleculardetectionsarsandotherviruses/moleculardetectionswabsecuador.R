
################################################################################
#                                                                              #
# Purpose:       Code to reproduce molecular detection of SARS and other resp  #
#                iratory viruses in Ecuador                                    #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2023-01-06                                                    #
# Last updated:  2023-01-06                                                    #
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
library (RColorBrewer)
library (viridis)
library (ggridges)
library(summarytools)
library(pastecs)
library(directlabels)
library (tidyverse)
library (janitor)
}

################### set working directory and load the data ####################
data <- read_excel("ecuadorsamplesswaballdatarreadycurated2023.xlsx")
data <- data %>% clean_names()
data$weeks <- isoweek(data$fecha_de_toma_de_muestra)
data$year <- year(data$fecha_de_toma_de_muestra)
data$weekyr <- paste (data$weeks, data$year, sep= "/")

#add stringency months between low and high

data <- data %>% mutate (npi = ifelse(fecha_de_toma_de_muestra >= "2020-03-16" & fecha_de_toma_de_muestra <= "2020-09-14", "HighNPI", "LowNPI" ) )

# descriptive statistics
freq (data$sexo)
#tableS1 
ctable (data$sars_2, data$sexo)

# number of SARS detections in the whole dataset, irrespective of other respiratory viruses 

freq (data$sars_2)

#number of detections of other viruses without specifically testing which (virus) , irrespective of mono or coinfection 

freq(data$monoco3)

#number of viruses detected per agent 
freq(data$positivity)
freq (data$agent)

#number of agents in the whole dataset 
unique (data$agent)  #"Adeno"       "MPV"         "MPV/Picorna" "PIV"         "Picorna"     "NL63"        "OC43"        "RSVA/B"      "229E"        "neg"         "SARS-CoV-2" 

#perform fischer tests between low and high NPI months per agent

#significant
#adenovirus
table(x= data$npi, y= data$adeno)
fisher.test(x= data$npi, y= data$adeno)
#enterovirus
table(x= data$npi, y= data$picorna)
fisher.test(x= data$npi, y= data$picorna)
# MPV
table (x= data$npi, y= data$mpv)
fisher.test(x= data$npi, y= data$mpv)
# PIV
table(x= data$npi, y= data$piv)
fisher.test(x= data$npi, y= data$piv)

#non significant = two or less detections in the whole dataset

#oc43 
table(x= data$npi, y= data$oc43 )
fisher.test(x= data$npi, y= data$oc43 )

#rsv 
table(x= data$npi, y= data$rsva_b )
fisher.test(x= data$npi, y= data$rsva_b )

#number of coinfections in the dataset 
freq(data$monocooccurence )

#number of viruses in coinfected patients 

datacooccurence <- data %>% dplyr::filter ( data$monocooccurence == "co-occurrence")
freq (datacooccurence$agent)

#sample general view

#Figure S3. age of patients for which respiratory samples were available in the dataset
#Median Age
summary(data$edad)

ggplot(data, aes(x=edad,  freq=FALSE))+
  geom_histogram(aes (y=..count..), color = "black")+
  scale_fill_viridis(discrete = TRUE)+
  ylab("Number of samples")+
  xlab("Age")+
  scale_y_continuous(breaks=seq(0,300,10))+
  scale_x_continuous(breaks=seq(0,100,10))+
  theme_Publication (18)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA)) 

ggsave("countpositives allsamples.pdf")

#Figure S4. Number of respiratory samples in the dataset monthly
ggplot(data, aes(x=factor (mes,level= c("march", "april","may", "june", "july", "august", "september", "october", "november","december", "january2021", "february2021")),  freq=FALSE))+
  geom_bar(aes (y=..count..))+
  geom_text(aes(label=after_stat(..count..)), stat='count')+
  scale_fill_viridis(discrete = TRUE)+
  ylab("Count")+
  xlab("Date")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA)) 

ggsave("countpositives allsamples.pdf")


# Figure 2B

ggplot(data, aes(x=factor (mes,level= c("march", "april","may", "june", "july", "august", "september", "october", "november","december", "january2021", "february2021")), group =monoco2, color=monoco2))  + 
  geom_density() + 
  scale_color_viridis(discrete = TRUE)+
  xlab("Date")+ 
  ylab("Density of detections in time")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("Densityplotcasesovertime.pdf")


#Figure 2C
ggplot(data, aes(x=factor (mes,level= c("march", "april","may", "june", "july", "august", "september", "october", "november","december", "january2021", "february2021")), 
                 group =agent, color=agent))  + 
  geom_density() + 
  scale_color_viridis(discrete = TRUE)+
  xlab("Date")+ 
  ylab("Density of detections in time")+
  #scale_y_continuous(breaks=seq(0,15,1))+
  #scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("Densityplotcasesovertime.pdf")

#6 Figure S13 A and B 
FigS13 <- data %>% filter (agent != "neg", agent != "SARS-CoV-2") %>%
ggplot()  + 
  geom_bar (aes(x =reorder(agent, agent, function(x)-length(x)), y=..count.., fill=agent), show.legend = FALSE) + 
  xlab("Agent")+ 
  ylab("Number of detections")+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28))+
  scale_fill_grey()+
  facet_wrap (~sars_2 )+ 
  theme_Publication(20)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))
FigS13

#ggsave("Densityplotcasesovertimewithdotsallagents.pdf")

#Figure S13C

ggplot(data, aes(x = as.Date(fecha_de_toma_de_muestra) , y=agent, fill=sars_2))  + 
  geom_density_ridges (scale = 0.9, jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
                      show.legend = FALSE) + 
  xlab("recepcion de la muestra")+ 
  ylab("coinfection")+
  scale_fill_grey()+
  theme_Publication(15)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("Densityplotcasesovertimewithdotsallagentssarsnosars.pdf")


#Figure S2panelC


ggplot(data, aes(x=reorder(prov_procedencia, prov_procedencia, function(x)-length(x))))+
  geom_bar(aes(y = ..count..,fill=sars_2), stat = "count", binwidth=10, alpha=5, size=1) +
  xlab("Sampling date")+
  ylab("Number of samples")+
  scale_fill_manual (values= c("darkgrey", "red"))+
  scale_y_continuous(breaks=c(0,100,200,300,400,500,600,700,800,900))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))


ggsave("plot number of samplesrbarplotpermonth.pdf")
