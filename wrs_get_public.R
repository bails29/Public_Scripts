#  Clear the Global environment before starting any new code
rm(list=ls(all=TRUE)) 

# Clear the Console
cat("\014")

#SET WD
setwd("Z:/")

# Load the libraries
#install.packages("rgdal"); install.packages("gtools"); install.packages("data.table"); install.packages("raster"); install.packages("sp")
library(rgdal);library(gtools); library(data.table); library(raster); library(sp); library(tools); library(rgeos)

# This changes the formatting of numbers in R (no scientific notation)
options(scipen=9999)

################################
# set personal file paths here #
################################
wrs_path <- "./path_to_wrs_shapefile/WRS2_descending.shp"
pathx <- "./path_to_polygon_shapefiles/"
x <- "your_file_name_sans_ext" #no need to define here if applying to multiple polygons

#file path if applying to many sites simultaneously, irrelevant if 
#only converting one file.
sites <- list.files("./path/to/your_files/")

############################
#function to convert to WRS#
############################

#x = name of relevant polygon withouth file extension
#wrs = preloaded WRS shapefile available for download here: https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/atoms/files/WRS2_descending_0.zip
#pathx = path to your polygon file

get_wrs <- function(x, wrs, pathx){
  if(file.exists(paste0(pathx, x, ".shp"))){
  your_poly <- readOGR(dsn = paste0(pathx, x, ".shp"), layer = x)
 
  your_poly <- spTransform(your_poly, crs(wrs))
  
  merged_attributes <- raster::intersect(your_poly, wrs)
  merged_attributes<-as.data.frame(merged_attributes)
  #head(merged_attributes)
  return(merged_attributes)
  rm(your_poly) }
  else{NULL}
}


#load wrs data
wrs <- readOGR(dsn = wrs_path, layer = "WRS2_descending")

#apply function to single your_poly/polygon
get_wrs(x = x, wrs=wrs, pathx = pathx)

####################################
#apply to many sites simultaneously#
####################################

#remove file paths from list of files
polygons <- file_path_sans_ext(sites)


multi_test <- lapply(polygons, get_wrs, wrs = wrs, pathx = pathx)
multi_df <- do.call(rbind, multi_test) #returns data frame of all converted values 
