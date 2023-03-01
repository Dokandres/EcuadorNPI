
################################################################################
#                                                                              #
# Purpose:       Code to reproduce analyses with  Oxford stringency data       #
#                 and pearson monthly correlations                             #
# taken from: https:/ourworldindata.org/explorers/coronavirus-data-explorer     #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
#                                                                              #
# Code created:  2022-01-05                                                    #
# Last updated:  2022-01-05                                                    #
#                                                                              #
#                                                                              #
# Comment:       Edouard Mathieu, Hannah Ritchie, Lucas Rodés-Guirao, Cameron  #
#                Appel, Charlie Giattino, Joe Hasell, Bobbie Macdonald,        #
#                Saloni Dattani, Diana Beltekian, Esteban Ortiz-Ospina and     #
#                Max Roser (2020) - Coronavirus Pandemic (COVID-19).           #
#                Published online at OurWorldInData.org. Retrieved from:       #
#                'https://ourworldindata.org/coronavirus' [Online Resource]    #
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
library (RColorBrewer)
library (viridis)
library (ggridges)
library(summarytools)
library(lubridate)
library (gtsummary)
library (flextable)
}

################### set working directory and load the data ####################

data <- read.csv ("covid-stringency-index.csv", sep=";" ) # load Ecuadors stringency data from OWID from 21.01.2020-31-01-2022
data$Day <- as.Date (data$Day, format = "%d.%m.%Y")
data$stringency_index <- as.numeric (data$stringency_index)
data2020 <- subset(data, Day> "2020-01-01" & Day < "2020-12-31")
write.xlsx(data2020, "contingencyindex2020.xlsx") #only use 2020 data


#plot stringency index "Figure S10. OxCGRT stringency index in 2020-2021 per month in Ecuador. "
ggplot(data, aes(x= Day, y=stringency_index))+
  geom_line (color="darkred", size=1)+
  xlab("Date")+
  ylab("Stringency Index")+
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y")+
  scale_y_continuous(breaks=seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))


ggsave("plot number of samplesreadylinehistogram.pdf") #save plot

#add new variables of month 

data2020$month <- month(data2020$Day)
data2020table <- data2020 %>% group_by (month) %>% summarize (strpermonthmax= max (stringency_index), strpermonthmedian= median(stringency_index), strmonthsIQR= IQR(stringency_index)) 

write.xlsx(data2020, "contingencyindex2020.xlsx")

############ Code for the Pearson correlations monthly across 2020 #############

#join all OWID data
dataowid <- read.csv ("incidencedataowid.csv", sep="," )
dataowiddecu <- subset (dataowid, location == "Ecuador") #select only ecuador
dataowiddecu$date <- as.Date (dataowiddecu$date)
dataowiddecu2020 <- subset(dataowiddecu, date> "2020-01-01" & date < "2020-12-31")  #subset only 2020
  
dataowidjoint <- dataowiddecu2020 %>% inner_join(data2020, by= c("date" ="Day")) # join with the stringency index table


# plot total tests per thousand used in Figure 2A
ggplot(dataowidjoint, aes(x= date, y=total_tests_per_thousand))+
  geom_smooth(span=0.15, color="darkred")+
  #geom_line (color="darkred", size=1)+
  xlab("Date")+
  ylab("TotaltestsperThousand")+
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%m-%y")+
  #scale_y_continuous(breaks=seq(0,100,5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(panel.grid.major = element_line(color = NA, 
                                        linetype = "dashed", 
                                        size = 0), 
        panel.background = element_rect(fill = NA),  
        panel.border = element_rect(fill = NA))


ggsave("plot number of samplesreadylinehistogram.pdf")
 
# build a correlation table with median per month 
tableforcorrelations <- dataowidjoint %>% mutate(date = lubridate::month(dataowidjoint$date)) %>%
  replace_na(list(total_deaths = 0, total_deaths_per_million=0)) %>%
  group_by(date) %>% 
 summarize (Stringind= median (stringency_index.y), medtotalcases= median (total_cases - lag(total_cases, default = first (total_cases))), medtotaldeaths= median (total_deaths - lag(total_deaths, default = first (total_deaths))), medtotacasespermil= median (total_cases_per_million- lag(total_cases_per_million, default = first (total_cases_per_million))) , medtotalmortpermil = median (total_deaths_per_million - lag(total_deaths_per_million, default = first (total_deaths_per_million))))

#add our PCR and seroprevalence data 
datapcr <- read_excel("pcrdetectionEcu.xlsx")
datasero <- read_excel("serodetectionEcu.xlsx")
datalineages <- read.csv("numberoflineages.csv", sep= ";")

tableforcorrelationsready <- tableforcorrelations %>% full_join(datapcr, by =c( "date"= "no")) %>% full_join(datasero, by = c("date" = "no")) %>% full_join(datalineages, by = c("date" = "date")) %>% slice(-(11:21))
#add other variables that are of interest
tableforcorrelationsready$r0 <- c(2.5,2.3,2.1,1.6,1.25,1.15,0.9,1,1.15,1.25) # taken from the model
tableforcorrelationsready$virusonly <- c(2,3,6,3,5,3,14,10,19,13) # only viruses

# write table
write.xlsx(tableforcorrelationsready, "contingencyversusotherfactors.xlsx")

#spearman test
# stringency per median total cases 
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$medtotalcases, method = "pearson",use="pairwise.complete.obs")
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$medtotacasespermil , method = "pearson", use="pairwise.complete.obs")
# stringency per median total deaths 
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$medtotaldeaths , method = "pearson", use="pairwise.complete.obs")
# stringency per PCR detection per month
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$sarsposnumber , method = "pearson", use="pairwise.complete.obs")
# stringency per seropositivity
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$seropercent , method = "pearson", use="pairwise.complete.obs")
#stringency per lineage 
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$countpangos , method = "pearson", use="pairwise.complete.obs")
# stringency per R0
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$r0 , method = "pearson", use="pairwise.complete.obs")
# stringency per other respviruses
cor.test(tableforcorrelationsready$Stringind, tableforcorrelationsready$virusonly , method = "pearson", use="pairwise.complete.obs")

############ plot the pearson correlations in Figure 2B and 2C, 4C #############

# plot median total cases 

mediantotalcases <- ggplot (tableforcorrelationsready, aes(Stringind, medtotalcases))+ 
  geom_point (size=3, alpha=.5)+
  geom_smooth (method= "lm", color= "darkred")+
  xlab("Stringency index")+ 
  ylab("Median montly cases")+
  annotate("text", x = 63, y = 1100, label = "p-value = 0.5002,rho =-0.2421665")#+
  #theme_Publication()

mediantotalcases

# plot meantotaldeaths 
mediantotaldeaths <- ggplot (tableforcorrelationsready, aes(Stringind, medtotaldeaths))+ 
   geom_point (size=3, alpha=.5)+
   geom_smooth (method= "lm", color= "darkred")+
   xlab("Stringency index") + 
   ylab("Median monthly deaths")+
   annotate("text", x = 63, y = 45, label = "p-value = 0.1691, rho =0.4713538 ")#+
   #theme_Publication()

mediantotaldeaths

 # plot molecular detection of SARS 
 moleculardetection <- ggplot (tableforcorrelationsready, aes(Stringind, sarsposnumber))+ 
   geom_point (size=3, alpha=.5)+
   geom_smooth (method= "lm", color= "darkred")+
   xlab("Stringency index") + 
   ylab("Detection of SARS-coV-2")+
   annotate("text", x = 63, y = 130, label = "p-value = 0.01139, rho =-0.7425446")#+
   #theme_Publication() 
 
 moleculardetection

  # plot number of lineages
 lineagedetection <- ggplot (tableforcorrelationsready, aes(Stringind, countpangos))+ 
   geom_point (size=3, alpha=.5)+
   geom_smooth (method= "lm", color= "darkred")+
   xlab("Stringency index") + 
   ylab("SARS-CoV-2 lineages")+
   annotate("text", x = 63, y = 22, label = "p-value = 0.02678, rho =-0.6914411")#+
   #theme_Publication() 
 
lineagedetection
 
 # plot R0

 reff <- ggplot (tableforcorrelationsready, aes(Stringind, r0))+ 
   geom_point (size=3, alpha=.5)+
   geom_smooth (method= "lm", color= "darkred")+
   xlab("Stringency index") + 
   ylab("Reff")+
   annotate("text", x = 63, y = 3, label = "p-value = 0.1655,rho =0.4748509")#+
   #theme_Publication() 
 
 reff
 
 # plot Respath
 virusplot <-ggplot (tableforcorrelationsready, aes(Stringind, virusonly))+ 
   geom_point (size=3, alpha=.5)+
   geom_smooth (method= "lm", color= "darkred")+
   xlab("Stringency index") + 
   ylab("Respiratory viral pathogen")+
   annotate("text", x = 63, y = 30, label = "p-value = 0.0283, rho =0.4748509")#+
   #theme_Publication() 
 
 virusplot
 ggpubr::ggarrange(mediantotalcases,mediantotaldeaths,moleculardetection, lineagedetection,virusplot, reff, ncol=2,nrow=3)
