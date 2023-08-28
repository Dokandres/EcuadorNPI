
################################################################################
#                                                                              #
# Purpose:       Code to reproduce the serology analyses in Ecuador            #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2022-01-11                                                    #
# Last updated:  2022-01-11                                                    #
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
library (sp)
library(raster)
library(viridis)
library(writexl)
library(tidyverse)
library (RColorBrewer)
library (viridis)
library (ggridges)
library(summarytools)
library(lubridate)
library (gtsummary)
library (flextable)
library (lubridate)
library (ungeviz)
library(janitor)
}

Sys.setenv("LANGUAGE"="EN")

#Load data and select data-----------------------------------------------------------------------------
setwd("~/serologicalanalysesecuador")
data <- read_excel("ecuadorfinallistforpublic1.xlsx")
data <- data[vapply(data$mes,function(x){x %in%c("october2019", "november2019", "december2019", "january2020", "february2020",  "march2020", "april2020","may2020", "juni2020", "july2020", "august2020", "september2020", "october2020", "november2020","december2020", "january2021", "february2021", "march2021") },T),]

#will only use data from 2020 and 2021. therefore filter:
data <- data %>% filter(YEAR !=2019) %>% clean_names()


data$recepcionplata2 <- as.Date(data$recepcionplata, format = "%Y-%m-%d")
data$semanaepidemiologica <- sprintf ("%02d", as.numeric (data$semanaepidemiologica))
data$semanaepi2 <- paste (data$year,data$semanaepidemiologica,sep="-")

# rename or recode variables 

data$resultcode <- data$result %>%
  recode("positive" = "1",
         "negative" = "0")

data$resultcode <- as.numeric ( data$resultcode)
#merge dengue results into only one 

data$denvmerged[!is.na(data$den_ig_m) & !is.na(data$den_ns1)] <-
  data$den_ig_m[!is.na(data$den_ig_m) & !is.na(data$den_ns1)]

data$denvmerged[is.na(data$den_ig_m)] <- data$den_ns1[is.na(data$den_ig_m)]
data$denvmerged[is.na(data$den_ns1)] <- data$den_ig_m[is.na(data$den_ns1)]


#descriptive statistics ---------------------------------------------------------------------------------------------

freq(data$sexo)
freq (data$result)
ctable(x = data$result_c_pass,y = data$resultsmaglumiborderline)

summary (data$sexo)

by(data, data$sexo, summary)

ctable ( x= data$mes, y=resultsalgemein)

ctable (x=mes, y= data$resprnt)


#Table S2. Data from patients with fever of unknown origin investigated at INSPI 

tbl <- data %>% select (sexo, edad, year, result, provincia, merged)%>% tbl_summary (by= merged) %>% add_overall() %>% 
bold_labels()

tbl
#save table
tbl %>% as_flex_table() %>% flextable::save_as_docx (tbl, path="tbl.docx")




#Figure S6. Proportion of patients diagnosed with acute Dengue virus infection.. 

datadeng <- data %>% filter(denvmerged ==1| denvmerged ==0) %>% 
ggplot(aes(x=factor (mes,level= c("october2019", "november2019", "december2019", "january2020", "february2020",  "march2020", "april2020","may2020", "juni2020", "july2020", "august2020", "september2020", "october2020", "november2020","december2020", "january2021", "february2021", "march2021", "april2021")),  freq=FALSE))+
  geom_bar(aes (fill=denvmerged), color ="black" , position= "fill")+
  #geom_text(aes(label=after_stat(..count..)), stat='count', y=0.1)+
  ylab("count")+
  xlab("date")+
  scale_fill_manual(values =c("grey", "red")) +
  theme_Publication(20)

datadeng

ggsave("countpositives allsamplesserology dengue ecuador and positives per month.pdf")

#Figure S5. Age distribution of the patients with fever of unknown origin from which serum samples were available. -------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = edad)) + 
geom_histogram(aes (y=..count..), binwidth = 5, color ="black")+
  scale_x_continuous(breaks=seq(0,80,5))+
  scale_y_continuous(breaks=seq(0,1000,100))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("plot number of samplesedadhistogram.pdf")


#Section 3. Figure S7. Serologic testing-----------------------------------------------------------------------------------------------


ggplot(data, aes(x = ratiocpass, y =log(concentration), color= result))  + 
  geom_point(position= "jitter") + 
  geom_abline(intercept = 0.59243019, slope = 0.03326021)+
  xlab("Svntpercent inhibition")+ 
  ylab("Log Clia concentration")+
  ggtitle ("Intercept = 0.59243019, Slope = 0.03326021")+
  scale_color_manual(values =c("darkgray", "darkorange")) +
  geom_hline(yintercept= log(1), linetype="dashed")+
  geom_vline(xintercept=20, linetype="dashed")+
  #scale_y_continuous(breaks=seq(0,15,1))+
  scale_x_continuous(breaks=seq(-50,100,10))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("maglumivscpass1.pdf")

# Calculate slope and intercept of line of best fit
coef(lm(log (concentration) ~ ratiocpass, data = data))


#plot data in time Figure 3B--------------------------------------------------------------------------------------------------------

ggplot(data, aes(x = recepcionplata2, y =concentration, color=result_c_pass))  + 
  geom_point() + 
  geom_smooth()+
  scale_color_manual(values =c("grey", "darkorange")) +
  xlab("recepcion de la muestra")+ 
  ylab("MAglumi concentration")+
  geom_hline(yintercept=c(1), linetype="dashed")+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("maglumivscpassin timeshowingabwaningintime2021.pdf")


#Figure S7. Serologic testing. B

#filtermaglumi just positives iggmaglumiconc <-data$concentration

concentrationpos <- filter (data, concentration>=1)

#either or
concentrationpos$mes <- factor (concentrationpos$mes, levels= c("october2019", "november2019", "december2019", "january2020", "february2020",  "march2020", "april2020","may2020", "juni2020", "july2020", "august2020", "september2020", "october2020", "november2020","december2020", "january2021", "february2021", "march2021"))
concentrationpos <- concentrationpos[vapply(concentrationpos$mes ,function(x){x %in%c("october2019", "november2019", "december2019", "january2020", "february2020",  "march2020", "april2020","may2020", "juni2020", "july2020", "august2020", "september2020", "october2020", "november2020","december2020", "january2021", "february2021", "march2021") },T),]

ggplot(concentrationpos, aes(x = mes, y =concentration, group=mes))  + 
  geom_boxplot() +
  #scale_color_manual(values =c("grey", "darkorange")) +
  #geom_hline(yintercept=c(0.8, 1.1), linetype="dashed")+
  xlab("recepcion de la muestra")+ 
  ylab("MAglumi concentration")+
  geom_hline(yintercept=c(1), linetype="dashed")+
  scale_y_continuous(breaks=seq(0,100,10))+
  #scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("maglumivscpassin timeshowingabwaningintime2021.pdf")


#################number of positives per time in weekly intervals for modeling and Figure 3A ######################################

ggplot(data, aes(semanaepi2, fill=result))+
  geom_bar(position='fill')+
  xlab("Epidemiological weeks")+
  #scale_x_date(date_breaks = "1 week", date_labels = "%d-%m-%y")+
  scale_fill_manual(values =c("white", "darkgrey")) +
  #geom_text(aes(label=after_stat(..count..)), stat='count', position='fill',angle = 90)+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

ggsave("plot number of samplesperepiweekreadylinehistogram2.pdf")

# 8. lines 
densityplotsero <- data %>% filter (result!= "negative") %>%
ggplot(aes(x= semanaepi2, group= result, color= result))+
  geom_density(adjust= 0.3)+
  #stat_confidence_density(aes(moe= 0.95, fill = stat(ndensity)), height = 0.8) +
  scale_color_manual(values ="orange")+
  xlab("Epidemiological weeks")+
  #geom_hline(yintercept=c(0.25,0.5, 0.75, 1), linetype="dashed")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

densityplotsero

ggsave("plot number of samples percent line geom densityadjusted3.pdf")


#Figure S8. Seroprevalence estimates using only samples from coastal Ecuador. 

#samples just from coastal Ecuador 
datacoastal <- data %>% filter (provincia %in% c("ESMERALDAS", "SANTA ELENA", "CHIMBORAZO", "BOLIVAR", "LOJA", "SANTO DOMINGO", "EL ORO", "	
LOS RIOS", "MANABI", "GUAYAS"))

datacoastal$semanaepidemiologica <- sprintf ("%02d", as.numeric (datacoastal$semanaepidemiologica))
semanaepi3 <- paste (datacoastal$year,datacoastal$semanaepidemiologica,sep="-")

ggplot(datacoastal, aes(semanaepi3, fill=result))+
  geom_bar(position='fill')+
  xlab("Epidemiological weeks")+
  #scale_x_date(date_breaks = "1 week", date_labels = "%d-%m-%y")+
  scale_fill_manual(values =c("white", "darkgrey")) +
  #geom_text(aes(label=after_stat(..count..)), stat='count', position='fill',angle = 90)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

# 8. lines 
densityplotserocoastal <- datacoastal %>% filter (result!= "negative") %>%
  ggplot(aes(x= semanaepi3, group= result, color= result))+
  geom_density(adjust= 0.3)+
  #stat_confidence_density(aes(moe= 0.95, fill = stat(ndensity)), height = 0.8) +
  scale_color_manual(values ="orange")+
  xlab("Epidemiological weeks")+
  #geom_hline(yintercept=c(0.25,0.5, 0.75, 1), linetype="dashed")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))

densityplotsero

ggsave("plot number of samples percent line geom densityadjusted3.pdf")
