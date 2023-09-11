{
library(seraphim)
library(rmapshaper)
library(diagram)
library(raster)
library(tidyverse)
}
# 1. Extracting spatio-temporal information embedded in posterior trees

localTreesDirectory = "localtrees"
allTrees = scan(file="onlyb11filteredstrict.trees.txt", what="", sep="\n", quiet=T)
burnIn = 25
randomSampling = TRUE
nberOfTreesToSample = 500
mostRecentSamplingDatum = 2020.9945355191257
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)


# 2. Extracting spatio-temporal information embedded in the MCC tree

source("mccExtractions.r")
mcc_tre = readAnnotatedNexus("onlyb11filteredstrict.tree")
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "b11_MCC.csv", row.names=F, quote=F)

#2.1 Statistics

nberOfExtractionFiles = 500
timeSlices = 500
onlyTipBranches = FALSE
showingPlots = FALSE
outputName = "SARS2"
nberOfCores = 2
slidingWindow = 1

spreadStatistics(localTreesDirectory, nberOfExtractionFiles, timeSlices, onlyTipBranches, simulations= FALSE, 
                 showingPlots, outputName, nberOfCores, slidingWindow)

# 3. Estimating the HPD region for each time slice

nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))


# 4. Defining the different colour scales to use

colour_scale = colorRampPalette(brewer.pal(11,"RdYlBu"))(141)[21:121]
minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))
for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"40")
}


# 5. Co-plotting the HPD regions and MCC tree
ECU_munPre <- sf::st_as_sf (raster::getData('GADM', country='ECU', level=1))

template_raster <- ms_simplify (ECU_munPre, keep = 0.001 ,keep_shapes = TRUE)
template_raster2 <- template_raster$geometry
borders <- sf::st_as_sf(raster::getData('GADM', country='ECU', level=0))
borders2 <- borders$geometry

plotacrosstimeandspace <- function (mccdataframe=mcc_tab) {
  dev.new(width=6, height=6.3)
  par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
  
  plot(template_raster2, col="white", box=F, axes=F, colNA="grey90", legend=F, xlim=c(-82,-74))
  
  for (i in 1:length(polygons))
  {
    plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
  }
  
  plot(borders2, add=T, lwd=0.1, border="gray10",  xlim=c(-82,-74))
  
 for (i in 1:dim(mcc_tab)[1])
  {
  curvedarrow(cbind(mcc_tab[i,"startLon"],mcc_tab[i,"startLat"]), cbind(mcc_tab[i,"endLon"],mcc_tab[i,"endLat"]), arr.length=0,
               arr.width=0, lwd=0.2, lty=1, lcol="gray10", arr.col=NA, arr.pos=FALSE, curve=0.3, dr=NA, endhead=F)
  }
  for (i in dim(mcc_tab)[1]:1)
  {
    if (i == 1)
    {
      points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=0.8)
      points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=0.8)
    }
    points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=0.8)
    points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=0.8)
  }
  
  rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab[,"startYear"]); rast[2] = max(mcc_tab[,"endYear"])
  plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.15),
       legend.args=list(text="", cex=0.7, line=0.3, col="gray30"), horizontal=T,
       axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0), at=seq(2019.9,2020.999,0.2)))
}

plotacrosstimeandspace (mcc_tab)