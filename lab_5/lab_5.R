# Xiaofeng Cai | May 9, 2024 | GEOG 115C

## Pre-processing

setwd("/Users/xiaofengcai/Desktop/Geography 115C/Week 6/Lab 5/Data")

library(terra)
landsat_20070925 <- c(
  rast("./LT05_L2SP_042036_20070925_20200829_02_T1/LT05_L2SP_042036_20070925_20200829_02_T1_SR_B1.tif"), 
  rast("./LT05_L2SP_042036_20070925_20200829_02_T1/LT05_L2SP_042036_20070925_20200829_02_T1_SR_B2.tif"),
  rast("./LT05_L2SP_042036_20070925_20200829_02_T1/LT05_L2SP_042036_20070925_20200829_02_T1_SR_B3.tif"),
  rast("./LT05_L2SP_042036_20070925_20200829_02_T1/LT05_L2SP_042036_20070925_20200829_02_T1_SR_B4.tif"),
  rast("./LT05_L2SP_042036_20070925_20200829_02_T1/LT05_L2SP_042036_20070925_20200829_02_T1_SR_B5.tif"),
  rast("./LT05_L2SP_042036_20070925_20200829_02_T1/LT05_L2SP_042036_20070925_20200829_02_T1_SR_B7.tif")) # stacked raster
landsat_20070925
names(landsat_20070925)
names(landsat_20070925) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2") # Change band's name

names(landsat_20070925)
plotRGB(landsat_20070925, r = 3, g = 2, b = 1, stretch = "lin")
SB_county_south <- vect("SB_county_south.shp")
crs(landsat_20070925)
crs(SB_county_south)
SB_county_south_utm <- project(SB_county_south, crs(landsat_20070925)) # project the polygon to same coordinate reference system
crs(SB_county_south_utm)
# below crop and mask
landsat_20070925_cropped <- crop(landsat_20070925, ext(SB_county_south_utm))
landsat_ucsb_20070925 <- mask(landsat_20070925_cropped, SB_county_south_utm)

plotRGB(landsat_ucsb_20070925, r = 3, g = 2, b = 1, stretch = "lin")
rm(landsat_20070925, SB_county_south, SB_county_south_utm, landsat_20070925_cropped)

summary(landsat_ucsb_20070925)
# clamp() function in the terra library subsets all of the values within a defined range and replaces them with a new value
landsat_ucsb_20070925 <- clamp(landsat_ucsb_20070925, lower=7273, upper=43636, values=FALSE)
summary(landsat_ucsb_20070925)

# change the scale factor of the image so a value of 100 corresponds with 100% reflectance
landsat_ucsb_20070925 <- (landsat_ucsb_20070925 * 0.0000275 - 0.2) * 100
plotRGB(landsat_ucsb_20070925, r = 3, g = 2, b = 1, stretch = "lin")
# export image
writeRaster(landsat_ucsb_20070925, "landsat_ucsb_20070925.tif", overwrite=TRUE)

## Training Data Set
training_data <- vect("trainingdata.shp")
crs(training_data)
crs(landsat_ucsb_20070925)

training_data_values <- extract(landsat_ucsb_20070925, training_data) # extract the reflectance values for the 40 training sites
View(training_data_values)
names(training_data_values) <- c("ID", "blue", "green", "red", "NIR", "SWIR1", "SWIR2") # change column name

attribute_table <- as.data.frame(training_data) # extract the attribute table from a SpatVector and save it as a data frame

View(attribute_table)

SB_training_data <- merge(training_data_values, attribute_table, by.x = "ID", by.y = "id") # merge the two data frames based on the ID column

View(SB_training_data)
SB_training_data$Type <- as.factor(SB_training_data$Type) # convert the type column
levels(SB_training_data$Type) # examine level

View(SB_training_data)
summary(SB_training_data)

## Training a Decision Tree Classifier
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
SB_formula <- Type ~ red + green + blue + NIR + SWIR1 + SWIR2 # create the formula for our analysis 
# create a decision tree
SB_decision_tree <- rpart(formula = SB_formula, 
                          data = SB_training_data, 
                          method = "class",
                          na.action = na.omit) # remove NA value
prp(SB_decision_tree) # plot decision tree
SB_decision_tree

## Classifying an Image
names(landsat_ucsb_20070925)
names(SB_training_data)
levels(SB_training_data$Type) # examine the factor levels using the levels() function

SB_classification <- predict(landsat_ucsb_20070925, SB_decision_tree, type = "class", na.rm = TRUE) # predict() function will return a SpatRaster with integer values

levels(SB_classification)
plot(SB_classification, col=c("forestgreen", "tan", "violet", "blue"))
writeRaster(SB_classification, "SB_classification.tif", overwrite=TRUE)
png(file="/Users/xiaofengcai/Desktop/Geography 115C/Week 6/Lab 5/Data/SB_classification.png", width = 1000, height = 300) # export image as png

plot(SB_classification, col=c("forestgreen", "tan", "violet", "blue"))
dev.off()

## Validating the Classification
validation_points <- vect("SB_validation_points.shp") # load shape file
View(as.data.frame(validation_points))
prediction_validation_points <- extract(SB_classification, validation_points)
View(prediction_validation_points)
validation_merge <- merge(prediction_validation_points, validation_points, by.x = "ID", by.y = "ID") # merge data
View(validation_merge)

SB_validation <- data.frame(predicted = validation_merge$class, 
                            reference = validation_merge$type, 
                            stringsAsFactors = TRUE)
View(SB_validation)
# install.packages("caret")
library(caret)
str(SB_validation) # summuarise the sturcture

SB_confusion_matrix <- confusionMatrix(data = SB_validation$predicted, reference = SB_validation$reference) #create the confusion matrix

SB_confusion_matrix
# install.packages("e1071")
library(e1071)
sink(file = "SB_confusion_matrix.txt")

SB_confusion_matrix

sink() # sink() function creates an empty .txt file and creates a connection with R. 