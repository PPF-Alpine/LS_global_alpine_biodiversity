library(elevatr)
library(sf)
library(dplyr)
library(raster)
library(here)
library(sf)
library(tidyverse)
library(data.table)
library(openxlsx)

# Read in the raster
treeline_karger <- raster::raster("~/Desktop/Datasets/Mountains/Treeline_CHELSA/Treeline_CHELSA/Data/Tree Line Height.sdat")

# reclassify all values belo 0 to 1 and all values above 0 to two 
reclassMatrix <- matrix(c(-Inf, 0, 1, 0, Inf, 2), ncol=3, byrow=TRUE)
reclassifiedRaster <- reclassify(treeline_karger, reclassMatrix)

x11()
plot(reclassifiedRaster)

reclassifiedRaster[reclassifiedRaster == 2] <- NA


writeRaster(reclassifiedRaster,"~/Desktop/Datasets/Mountains/reclassifiedKarger_new2.tif",overwrite=TRUE)

#source the gmba regions whith alpine biome
mountain_shapes <- sf::st_read("~/Desktop/Datasets/Mountains/GMBA_Alpinebiome/GMBA_Ranges_workingfile.shp",options = "ENCODING=ISO-8859-1")
clippedRaster <- mask(reclassifiedRaster, mountain_shapes)


alpine_biome_processed <- raster::raster("~/Desktop/Datasets/Mountains/Treeline_CHELSA/Treeline_CHELSA/Data/Tree Line Height.sdat")