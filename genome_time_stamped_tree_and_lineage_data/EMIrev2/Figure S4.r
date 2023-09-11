################################################################################
#                                                                              #
# Purpose:       Code to reproduce Figure S16. Number of SARS-CoV-2 tips per   #
#                time in the time-stamped phylogenetic tree                    #
#                                                                              #
# Author:        Andres Moreira-Soto                                           #
# Contact:       andres.moreira-soto@charite.de                                #
# Client:                                                                      #
#                                                                              #
# Code created:  2022-01-05                                                    #
# Last updated:  2023-08-15                                                    #
# Source:                                                                      #
#                                                                              #
# Comment:                                                                     #
#                                                                              #
################################################################################

########################### Load required libraries ############################
{
library(phytools)
library(ape)
library(maps)

}
################### load the data ####################
tree<-read.nexus('timetree.nexus')
######################### set object and plot the data #########################
obj<-ltt(tree,plot=TRUE)
plot(obj,log.lineages=FALSE,show.tree=TRUE, ylim=c(1,500))
