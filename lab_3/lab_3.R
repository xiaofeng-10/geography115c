# Xiaofeng Cai | April 25, 2024 | GEOG 115C
# Extracting Values from a Region of Interest

library(terra)

landsat <- rast("./Data/landsat_ucsb_20070925.tif") # load landsat image

names(landsat) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2") # assign band
names(landsat)
landsat_rescaled <- landsat/100 # rescale

more_mesa <- vect("more_mesa.shp")
sandpiper <- vect("sandpiper.shp")
el_capitan <- vect("el_capitan.shp") # load three shapefiles

more_mesa_values <- extract(landsat_rescaled, more_mesa, df = T) # extract reflectance
head(more_mesa_values) # preview
summary(more_mesa_values) # summuary the reflectance values for the shape files

sandpiper_values <- extract(landsat_rescaled, sandpiper, df = T)
head(sandpiper_values)
summary(sandpiper_values)

el_capitan_values <- extract(landsat_rescaled, el_capitan, df = T)
head(el_capitan_values)
summary(el_capitan_values)

# Reflectance Spectra
more_mesa_values <- more_mesa_values[,2:7]
head(more_mesa_values)

sandpiper_values <- sandpiper_values[,2:7]
head(sandpiper_values)

el_capitan_values <- el_capitan_values[,2:7] # remove ID column
head(el_capitan_values)

more_mesa_mean_reflectance <- apply(more_mesa_values, 2, mean)
more_mesa_mean_reflectance

sandpiper_mean_reflectance <- apply(sandpiper_values, 2, mean)
sandpiper_mean_reflectance

el_capitan_mean_reflectance <- apply(el_capitan_values, 2, mean) # get the mean values for each coloum
el_capitan_mean_reflectance

wavelength <- c(480, 560, 660, 830, 1650, 2220) 

sandpiper_wvl_ref <- data.frame(wavelength, sandpiper_mean_reflectance) # build a data frame that has two column, one is the wavelength and another one is the mean reflectance
head(sandpiper_wvl_ref)

more_mesa_wvl_ref <- data.frame(wavelength, more_mesa_mean_reflectance)
head(more_mesa_wvl_ref)

el_capitan_wvl_ref <- data.frame(wavelength, el_capitan_mean_reflectance)
head(el_capitan_wvl_ref)

names(sandpiper_wvl_ref)[2] <- "reflectance" # change column name
names(more_mesa_wvl_ref)[2] <- "reflectance"
names(el_capitan_wvl_ref)[2] <- "reflectance"

plot(sandpiper_wvl_ref$wavelength, sandpiper_wvl_ref$reflectance, xlab = "Wavelength (nm)", ylab = "Reflectance (%)", main = "Sandpiper Mean Reflectance Spectrum", type = "b")

plot(more_mesa_wvl_ref$wavelength, more_mesa_wvl_ref$reflectance, xlab = "Wavelength (nm)", ylab = "Reflectance (%)", main = "More Mesa Mean Reflectance Spectrum", type = "b")

plot(el_capitan_wvl_ref$wavelength, el_capitan_wvl_ref$reflectance, xlab = "Wavelength (nm)", ylab = "Reflectance (%)", main = "El Capitan Mean Reflectance Spectrum", type = "b") # plot the reflectance vs wavelength

# Writing Functions
NDVI <- function(red, NIR) {
  value <- (NIR-red)/(NIR+red)
  return(value)
  } # create a NDVI function
sandpiper_ndvi <- NDVI(sandpiper_values$red, sandpiper_values$NIR) # apply the function to get NDVI
more_mesa_ndvi <- NDVI(more_mesa_values$red, more_mesa_values$NIR)
el_capitan_ndvi <- NDVI(el_capitan_values$red, el_capitan_values$NIR)
mean(sandpiper_ndvi) # get the mean NDVI
mean(more_mesa_ndvi)
mean(el_capitan_ndvi)

# Create a function that plots the mean reflectance spectrum of a Thematic Mapper reflectance values data set. The code should accept a single argument: a data frame where every row is a different pixel and every column is a different band.  The resulting plot should have appropriate axis labels and contain both points and lines. The plot should also have a title. 

plot_reflectance <- function(wavelength, reflectance, xlab = "Wavelength (nm)", ylab = "Reflectance (%)", main = "Mean Reflectance Spectrum", type = "b") {
  mean_reflectance <- apply(reflectance, 2, mean)
  wavelength <- c(480, 560, 660, 830, 1650, 2220)
  plot(wavelength, mean_reflectance, type = type, xlab = xlab, ylab = ylab, main = main)
}

plot_reflectance(wavelength, sandpiper_values, main = "Sandpiper Mean Reflectance Spectrum")

plot_reflectance(wavelength, more_mesa_values, main = "More Mesa Mean Reflectance Spectrum")

plot_reflectance(wavelength, el_capitan_values, main = "El Capitan Mean Reflectance Spectrum")
