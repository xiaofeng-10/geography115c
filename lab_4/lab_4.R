# Xiaofeng Cai, May 2, 2024, GEOG 115C
# Ground Control Points

library(terra)
setwd("/Users/xiaofengcai/Desktop/Geography 115C/Week 5/Lab 4/Data")

min_GCPs <- function(t){
  gcp_function <- (t+1)*(t+2)/2
  return(gcp_function)
} # create the function to calculate the minimal number of GCPs needed

min_GCPs(c(1,2,3,4,5,6))  # Calculate the minimum number of GCPs needed for first through sixth order polynomial equations

poly_order <- c(1,2,3,4,5,6)

plot(poly_order, min_GCPs(poly_order), xlab = "Polynomial Order", ylab = "Minimum Number of GCPs", main = "Minimum Number of GCPs vs. Different Polynomial Order") # create a plot for the first through six order polynomial equations

# Visualizing Raster and Vector Data
landsat_20070621 <- rast("landsat_ucsb_20070621.tif")
landsat_20070621 <- landsat_20070621/100
names(landsat_20070621) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2") # load imgae

landsat_20080725 <- rast("landsat_ucsb_20080725.tif")
landsat_20080725 <- landsat_20080725/100
names(landsat_20080725) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")

landsat_20090712 <- rast("landsat_ucsb_20090712.tif")
landsat_20090712 <- landsat_20090712/100
names(landsat_20090712) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")

landsat_20100715 <- rast("landsat_ucsb_20100715.tif")
landsat_20100715 <- landsat_20100715/100
names(landsat_20100715) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")

landsat_20110718 <- rast("landsat_ucsb_20110718.tif")
landsat_20110718 <- landsat_20110718/100
names(landsat_20110718) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")

plotRGB(landsat_20070621, r = 4, g = 3, b = 2, stretch = "lin") # plot a false-color composite with a linear stretch
plotRGB(landsat_20080725, r = 4, g = 3, b = 2, stretch = "lin")
plotRGB(landsat_20090712, r = 4, g = 3, b = 2, stretch = "lin")

# Masking and Cropping
# Load the two shapefiles as SpatVectors
gap <- vect("gap.shp") # gap polygon
jesusita <- vect("jesusita.shp")

jesusita

crs(landsat_20070621, proj = T)
crs(landsat_20070621, describe = T)
ext(landsat_20070621)

crs(gap, describe = T)
gap_utm <- project(gap, crs(landsat_20070621)) # Reproject the polygons to UTM Zone 11N
crs(gap_utm, describe = T)
crs(landsat_20070621, describe = T) # compare again

crs(jesusita, describe = T)
jesusita_utm <- project(jesusita, crs(landsat_20070621)) # Reproject the polygons to UTM Zone 11N
crs(jesusita_utm, describe = T)

ext(jesusita_utm)

plotRGB(landsat_20090712, r = 3, g = 2, b = 1, stretch = "lin")
plot(gap_utm, col = NA, border = "red", lwd = 2, add = TRUE) # Plot the two polygons on top of the Landsat image
plot(jesusita_utm, col = NA, border = "blue", lwd = 2, add = TRUE)

gap_2007 <- mask(landsat_20070621, gap_utm)
plotRGB(gap_2007, r = 4, g = 3, b = 2, stretch = "lin")
ext(landsat_20070621)
ext(gap_2007)

gap_2007_cropped <- crop(gap_2007, ext(gap_utm)) # Crop the masked image to remove most of the NA values around the edge of the image

plotRGB(gap_2007_cropped, r = 4, g = 3, b = 2, stretch = "lin") # plot the new SpatRaster as a false color composite to see how it looks

rm(gap_2007) # remove the earlier version of the image

gap_2007_values <- values(gap_2007_cropped) # values() function to create a matrix with all of the values from your new image
View(gap_2007_values)

# Write a function that masks and crops an image using a polygon. The function should accept two arguments: a SpatRaster object and a SpatVector object. 
mask_crop<- function(SpatRaster, SpatVector){
  method <- crop(SpatRaster, ext(SpatVector))
  return(method)
}

# Use your function to create two new SpatRasters. The new images should be derived from the 2009 Landsat image and they should cover the Gap and Jesusita study sites.
gap_SR <- mask_crop(landsat_20090712, gap_utm)
jesusita_SR <- mask_crop(landsat_20090712, jesusita_utm)
plotRGB(gap_SR, r = 4, g = 3, b = 2, stretch = "lin") 
plotRGB(jesusita_SR, r = 4, g = 3, b = 2, stretch = "lin") 

# Control Site
new_shape <- vect("new_shape_file.shp")
plotRGB(landsat_20090712, r = 3, g = 2, b = 1, stretch = "lin")
plot(new_shape, col = NA, border = "green", lwd = 2, add = TRUE)

library(sp)
library(raster)

gap_values <- extract(landsat_20090712, gap, df = T) #extract reflectance
head(gap_values) 
gap_values <- gap_values[,2:7]
head(gap_values)

jesusita_values <- extract(landsat_20090712, jesusita, df = T) #extract reflectance
head(jesusita_values) 
jesusita_values <- jesusita_values[,2:7]
head(jesusita_values)

new_shape_values <- extract(landsat_20090712, new_shape, df = T) #extract reflectance
head(new_shape_values) 
new_shape_values <- new_shape_values[,2:7]
head(new_shape_values)

# plot the reflectance by writing a function
wavelength <- c(480, 560, 660, 830, 1650, 2220)
plot_reflectance <- function(wavelength, reflectance, xlab = "Wavelength (nm)", ylab = "Reflectance (%)", main = "Mean Reflectance Spectrum", type = "b") {
  mean_reflectance <- apply(reflectance, 2, mean, na.rm = TRUE)
  plot(wavelength, mean_reflectance, type = type, xlab = xlab, ylab = ylab, main = main)
}

plot_reflectance(wavelength, gap_values, main = "Gap Mean Reflectance Spectrum")
plot_reflectance(wavelength, jesusita_values, main = "Jesusita Mean Reflectance Spectrum")
plot_reflectance(wavelength, new_shape_values, main = "New Shape Mean Reflectance Spectrum")

# Time Series Analysis
# Define NDVI function
NDVI <- function(red, NIR){
  
  value <- (NIR - red)/(NIR + red)
  
  return(value)
  
}
# Define Calculate Mean NDVI function
calcMeanNDVI <- function(image, shapefile){
  #Extract values from image
  ref_values <- extract(image, shapefile, df = TRUE)
  #Drop ID column
  ref_values <- ref_values[, 2:7]
  #Calculate mean red reflectance for all pixels
  mean_red <- mean(ref_values[, 3], na.rm = TRUE)
  #Calculate mean NIR reflectance for all pixels
  mean_NIR <- mean(ref_values[, 4], na.rm = TRUE)
  #Calculate NDVI
  mean_ndvi <- NDVI(red = mean_red, NIR = mean_NIR)
  return(mean_ndvi)
}

# Gap_NDVI
gap_ndvi_2007 <- calcMeanNDVI(landsat_20070621, gap_utm)
gap_ndvi_2008 <- calcMeanNDVI(landsat_20080725, gap_utm)
gap_ndvi_2009 <- calcMeanNDVI(landsat_20090712, gap_utm)
gap_ndvi_2010 <- calcMeanNDVI(landsat_20100715, gap_utm)
gap_ndvi_2011 <- calcMeanNDVI(landsat_20110718, gap_utm)

year <- 2007:2011

mean_ndvi <- c(gap_ndvi_2007, gap_ndvi_2008, gap_ndvi_2009, gap_ndvi_2010, gap_ndvi_2011)

gap_ndvi <- data.frame(year, mean_ndvi)
View(gap_ndvi)
plot(x = gap_ndvi$year, 
     y = gap_ndvi$mean_ndvi, 
     xlab = "Year", 
     ylab = "Mean NDVI",
     main = "Gap Fire",
     type = "b")

# Jesusita_NDVI
jesusita_ndvi_2007 <- calcMeanNDVI(landsat_20070621, jesusita_utm)
jesusita_ndvi_2008 <- calcMeanNDVI(landsat_20080725, jesusita_utm)
jesusita_ndvi_2009 <- calcMeanNDVI(landsat_20090712, jesusita_utm)
jesusita_ndvi_2010 <- calcMeanNDVI(landsat_20100715, jesusita_utm)
jesusita_ndvi_2011 <- calcMeanNDVI(landsat_20110718, jesusita_utm)

mean_ndvi <- c(jesusita_ndvi_2007, jesusita_ndvi_2008, jesusita_ndvi_2009, jesusita_ndvi_2010, jesusita_ndvi_2011)

jesusita_ndvi <- data.frame(year, mean_ndvi)
View(jesusita_ndvi)
plot(x = jesusita_ndvi$year, 
     y = jesusita_ndvi$mean_ndvi, 
     xlab = "Year", 
     ylab = "Mean NDVI",
     main = "Jesusita Fire",
     type = "b")

# New_Shape_NDVI
new_shape_ndvi_2007 <- calcMeanNDVI(landsat_20070621, new_shape)
new_shape_ndvi_2008 <- calcMeanNDVI(landsat_20080725, new_shape)
new_shape_ndvi_2009 <- calcMeanNDVI(landsat_20090712, new_shape)
new_shape_ndvi_2010 <- calcMeanNDVI(landsat_20100715, new_shape)
new_shape_ndvi_2011 <- calcMeanNDVI(landsat_20110718, new_shape)

mean_ndvi <- c(new_shape_ndvi_2007, new_shape_ndvi_2008, new_shape_ndvi_2009, new_shape_ndvi_2010, new_shape_ndvi_2011)

new_shape_ndvi <- data.frame(year, mean_ndvi)
View(jnew_shape_ndvi)
plot(x = new_shape_ndvi$year, 
     y = new_shape_ndvi$mean_ndvi, 
     xlab = "Year", 
     ylab = "Mean NDVI",
     main = "New Shape Fire",
     type = "b")