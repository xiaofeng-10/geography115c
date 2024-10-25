library(terra)
library(raster)
library(ggplot2)
library(reshape2)

## Gap fire
setwd("C:/Users/Student/Desktop/XF/gap/LC08_L2SP_045031_20160906_20200906_02_T1")

# stack raster
landsat_gap <- c(rast("LC08_L2SP_045031_20160906_20200906_02_T1_SR_B1.TIF"),
                    rast("LC08_L2SP_045031_20160906_20200906_02_T1_SR_B2.TIF"),
                    rast("LC08_L2SP_045031_20160906_20200906_02_T1_SR_B3.TIF"),
                    rast("LC08_L2SP_045031_20160906_20200906_02_T1_SR_B4.TIF"),
                    rast("LC08_L2SP_045031_20160906_20200906_02_T1_SR_B5.TIF"),
                    rast("LC08_L2SP_045031_20160906_20200906_02_T1_SR_B6.TIF"),
                    rast("LC08_L2SP_045031_20160906_20200906_02_T1_SR_B7.TIF"))
names(landsat_gap)
names(landsat_gap) <- c("Coastal Aerosol", "blue", "green", "red", "NIR", "SWIR1", "SWIR2")

# Import fire severity
setwd("C:/Users/Student/Desktop/XF/gap")
gap_severity <- rast("gap_severity.tif")

# Import shape file 
setwd("C:/Users/Student/Desktop/XF/gap/gap_fire")
gap <- vect("ca4185312311520160828_20150725_20170714_burn_bndy.shp")

# Check Coordinate Reference System (CRS)
crs(landsat_gap)
crs(gap)
crs(gap_severity)

# Reproject the gap to the landsat_gap
gap_project <- project(gap, crs(landsat_gap))
crs(gap_project)

# Crop and Mask the gap landsat to the size of fire (shape file)
gap_cropped <- crop(landsat_gap, ext(gap_project))
gap_mask <- mask(gap_cropped, gap_project)
gap_rescaled <- gap_mask * 0.0000275 - 0.2

gap_mask <- clamp(gap_rescaled, 0, 1)  # Clamp values between 0 and 1
gap_values <- extract(gap_mask, gap_project, df = TRUE)
summary(gap_values)

# We will also reproject the gap severity
gap_severity_project <- project(gap_severity, crs(landsat_gap))
crs(gap_severity_project)

# Check resolution of the landsat and severity
res(gap_mask)
res(gap_severity_project)

# resample to match the resolution
gap_severity_project_resampled <- resample(gap_severity_project, gap_mask, method = "bilinear")
res(gap_severity_project_resampled)

# resacled the value of gap_landsat, and extract the values of landsat
#gap_rescaled <- gap_mask * 0.0000275 - 0.2
#summary(gap_rescaled)
#gap_rescaled <- clamp(gap_rescaled, values = F)
#summary(gap_new_rescaled)
#gap_values <- extract(gap_rescaled, gap_project, df = TRUE)


# Extract the severity values
gap_severity_values <- extract(gap_severity_project_resampled, gap_project, df = T)
gap_final_severity <- gap_severity_values$gap_severity
summary(gap_final_severity)

# Function of NDVI
NDVI <- function(red, NIR) {
  value <- (NIR-red)/(NIR+red)
  return(value)
}
gap_NDVI <- NDVI(gap_values$red, gap_values$NIR)
mean(gap_NDVI, na.rm = T)
summary(gap_NDVI)

# Function of NBR
NBR <- function(NIR, SWIR2){
  value <- (NIR-SWIR2)/ (NIR +SWIR2)
  return(value)
}
gap_NBR <- NBR(gap_values$NIR, gap_values$SWIR2)
mean(gap_NBR, na.rm = T)
summary(gap_NBR)

# Function of BAI
BAI <- function(red, NIR){
  value <- 1/((0.1 - red)^2 + (0.06 - NIR)^2)
  return(value)
}
gap_BAI <- BAI(gap_values$red, gap_values$NIR)
summary(gap_BAI)

# linear regression model
gap_model <- lm(gap_final_severity ~ gap_NDVI + gap_NBR + gap_BAI)
summary(gap_model)

## Analysis

# qqplot of residuals
#qqnorm(residuals(gap_model))
#qqline(residuals(gap_model))
# the point falls along the reference line, thus the residuals follow a normal distribution

# residual plot
#plot(gap_model, which = 1)
# The residual plot shows non-linear relationship between residuals and fitted values, which means the linear regression model may not be the best fit

# Graph
gap_data <- data.frame(Fire_Severity = gap_final_severity, 
                          NDVI = gap_NDVI, 
                          NBR = gap_NBR,
                       BAI = gap_BAI)
gap_data_long <- melt(gap_data, id.vars = "Fire_Severity")

gap_plot <- ggplot(gap_data_long, aes(x = value, y = Fire_Severity, color = variable)) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Gap Fire Severity vs. Flame Ability Indices",
       x = "Value",
       y = "Fire Severity",
       color = "Flame Ability Indices") +
  theme_minimal() +
  theme(legend.position = "none")
gap_plot

## Mccash Fire
setwd("C:/Users/Student/Desktop/XF/Mccash/LC08_L2SP_045031_20210920_20210925_02_T1")

# stack raster
landsat_mccash <- c(rast("LC08_L2SP_045031_20210920_20210925_02_T1_SR_B1.TIF"),
                 rast("LC08_L2SP_045031_20210920_20210925_02_T1_SR_B2.TIF"),
                 rast("LC08_L2SP_045031_20210920_20210925_02_T1_SR_B3.TIF"),
                 rast("LC08_L2SP_045031_20210920_20210925_02_T1_SR_B4.TIF"),
                 rast("LC08_L2SP_045031_20210920_20210925_02_T1_SR_B5.TIF"),
                 rast("LC08_L2SP_045031_20210920_20210925_02_T1_SR_B6.TIF"),
                 rast("LC08_L2SP_045031_20210920_20210925_02_T1_SR_B7.TIF"))
names(landsat_mccash)
names(landsat_mccash) <- c("Coastal Aerosol", "blue", "green", "red", "NIR", "SWIR1", "SWIR2")

# Import shape file 
setwd("C:/Users/Student/Desktop/XF/Mccash/mccash_fire")
mccash <- vect("ca4156412340420210801_20210718_20220720_burn_bndy.shp")

# Import fire severity
setwd("C:/Users/Student/Desktop/XF/Mccash")
mccash_severity <- rast("mccash_severity_2021.tif")

# Check Coordinate Reference System (CRS)
crs(landsat_mccash)
crs(mccash)
crs(mccash_severity)

# Reproject the mccash shape file to the landsat_maccash
mccash_project <- project(mccash, crs(landsat_mccash))
crs(mccash_project)

# Crop and Mask the mccash landsat to the size of fire (shape file)
mccash_cropped <- crop(landsat_mccash, ext(mccash_project))
mccash_mask <- mask(mccash_cropped, mccash_project)
mccash_rescaled <- mccash_mask * 0.0000275 - 0.2

mccash_mask <- clamp(mccash_rescaled, 0, 1)  # Clamp values between 0 and 1
mccash_values <- extract(mccash_mask, mccash_project, df = TRUE)
summary(mccash_values)

# We will also reproject the mccash severity
mccash_severity_project <- project(mccash_severity, crs(landsat_mccash))
crs(mccash_severity_project)

# Check resolution of the landsat and severity
res(mccash_mask)
res(mccash_severity_project)

# resample to match the resolution
mccash_severity_project_resampled <- resample(mccash_severity_project, mccash_mask, method = "bilinear")
res(mccash_severity_project_resampled)


# Extract the severity values
mccash_severity_values <- extract(mccash_severity_project_resampled, mccash_project, df = T)
mccash_final_severity <- mccash_severity_values$mccash_severity_2021
summary(mccash_final_severity)

# NDVI
mccash_NDVI <- NDVI(mccash_values$red, mccash_values$NIR)
mean(mccash_NDVI, na.rm = T)

# NBR
mccash_NBR <- NBR(mccash_values$NIR, mccash_values$SWIR2)
mean(mccash_NBR, na.rm = T)

mccash_values$NIR
# BAI
mccash_BAI <- BAI(mccash_values$red, mccash_values$NIR)
summary(mccash_BAI, na.rm = T)

# linear regression model
mccash_model <- lm(mccash_final_severity ~ mccash_NDVI + mccash_NBR + mccash_BAI)
summary(mccash_model)

## Analysis
# qqplot of residuals
#qqnorm(residuals(mccash_model))
#qqline(residuals(mccash_model))
# the point falls along the reference line, thus the residuals follow a normal distribution

# residual plot
#plot(mccash_model, which = 1)
# The residual plot shows non-linear relationship between residuals and fitted values, which means the linear regression model may not be the best fit

# Graph
mccash_data <- data.frame(Fire_Severity = mccash_final_severity, 
                          NDVI = mccash_NDVI, 
                          NBR = mccash_NBR, 
                          BAI = mccash_BAI)
mccash_data_long <- melt(mccash_data, id.vars = "Fire_Severity")

mccash_plot <- ggplot(mccash_data_long, aes(x = value, y = Fire_Severity, color = variable)) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Mccash Fire Severity vs. Flame Ability Indices",
       x = "Value",
       y = "Fire Severity",
       color = "Flame Ability Indices") +
  theme_minimal() +
  theme(legend.position = "none")
mccash_plot

  
## Sherpa Fire
setwd("C:/Users/Student/Desktop/XF/sherpa/LC08_L2SP_042036_20160629_20200906_02_T1")

# stack raster
landsat_sherpa <- c(rast("LC08_L2SP_042036_20160629_20200906_02_T1_SR_B1.TIF"),
                    rast("LC08_L2SP_042036_20160629_20200906_02_T1_SR_B2.TIF"),
                    rast("LC08_L2SP_042036_20160629_20200906_02_T1_SR_B3.TIF"),
                    rast("LC08_L2SP_042036_20160629_20200906_02_T1_SR_B4.TIF"),
                    rast("LC08_L2SP_042036_20160629_20200906_02_T1_SR_B5.TIF"),
                    rast("LC08_L2SP_042036_20160629_20200906_02_T1_SR_B6.TIF"),
                    rast("LC08_L2SP_042036_20160629_20200906_02_T1_SR_B7.TIF"))
names(landsat_sherpa)
names(landsat_sherpa) <- c("Coastal Aerosol", "blue", "green", "red", "NIR", "SWIR1", "SWIR2")

# Import shape file
setwd("C:/Users/Student/Desktop/XF/sherpa/sherpa_fire")
sherpa <- vect("ca3453012005420160615_20150713_20160629_burn_bndy.shp")

# Import fire severity
setwd("C:/Users/Student/Desktop/XF/sherpa")
sherpa_severity <- rast("sherpa_severity.tif")

# Check Coordinate Reference System (CRS)
crs(landsat_sherpa)
crs(sherpa)
crs(sherpa_severity)

# Reproject the shape file to the landsat_sherpa
sherpa_project <- project(sherpa, crs(landsat_sherpa))
crs(sherpa_project)

# Crop and Mask the landsat to the size of fire (shape file)
sherpa_cropped <- crop(landsat_sherpa, ext(sherpa_project))
sherpa_mask <- mask(sherpa_cropped, sherpa_project)
sherpa_rescaled <- sherpa_mask * 0.0000275 - 0.2

sherpa_mask <- clamp(sherpa_rescaled, 0, 1)  # Clamp values between 0 and 1
sherpa_values <- extract(sherpa_mask, sherpa_project, df = TRUE)
summary(sherpa_values)


# We will also reproject the mccash severity
sherpa_severity_project <- project(sherpa_severity, crs(landsat_sherpa))
crs(sherpa_severity_project)

# Check resolution of the landsat and severity
res(sherpa_mask)
res(sherpa_severity_project)

# resample to match the resolution
sherpa_severity_project_resampled <- resample(sherpa_severity_project, sherpa_mask, method = "bilinear")
res(sherpa_severity_project_resampled)


# Extract the severity values
sherpa_severity_values <- extract(sherpa_severity_project_resampled, sherpa_project, df = T)
sherpa_final_severity <- sherpa_severity_values$sherpa_severity
summary(mccash_final_severity)

# NDVI
sherpa_NDVI <- NDVI(sherpa_values$red, sherpa_values$NIR)
mean(sherpa_NDVI, na.rm = T)

# NBR
sherpa_NBR <- NBR(sherpa_values$NIR, sherpa_values$SWIR2)
mean(sherpa_NBR, na.rm = T)

# BAI
sherpa_BAI <- BAI(sherpa_values$red, sherpa_values$NIR)
mean(sherpa_BAI, na.rm = T)

# linear regression model
sherpa_model <- lm(sherpa_final_severity ~ sherpa_NDVI + sherpa_NBR + sherpa_BAI)
summary(sherpa_model)


# qqplot of residuals
qqnorm(residuals(sherpa_model))
qqline(residuals(sherpa_model))
# most of the point falls along the reference line, though the end of the QQ plot deviates from the reference line, we can still assume the residuals follow a normal distribution

# residual plot
plot(sherpa_model, which = 1)
# The residual plot looks scattered randomly, good to use lm

# Graph
sherpa_data <- data.frame(Fire_Severity = sherpa_final_severity, 
                          NDVI = sherpa_NDVI, 
                          NBR = sherpa_NBR, 
                          BAI = sherpa_BAI)
sherpa_data_long <- melt(sherpa_data, id.vars = "Fire_Severity")

sherpa_plot <- ggplot(sherpa_data_long, aes(x = value, y = Fire_Severity, color = variable)) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Sherpa Fire Severity vs. Flame Ability Indices",
       x = "Value",
       y = "Fire Severity",
       color = "Flame Indices") +
  theme_minimal() +
  theme(legend.position = "none")
sherpa_plot

## Alisal Fire
setwd("C:/Users/Student/Desktop/XF/alisal/LC08_L2SP_042036_20211102_20211109_02_T1")

# stack raster
landsat_alisal <- c(rast("LC08_L2SP_042036_20211102_20211109_02_T1_SR_B1.TIF"),
                    rast("LC08_L2SP_042036_20211102_20211109_02_T1_SR_B2.TIF"),
                    rast("LC08_L2SP_042036_20211102_20211109_02_T1_SR_B3.TIF"),
                    rast("LC08_L2SP_042036_20211102_20211109_02_T1_SR_B4.TIF"),
                    rast("LC08_L2SP_042036_20211102_20211109_02_T1_SR_B5.TIF"),
                    rast("LC08_L2SP_042036_20211102_20211109_02_T1_SR_B6.TIF"),
                    rast("LC08_L2SP_042036_20211102_20211109_02_T1_SR_B7.TIF"))
names(landsat_alisal)
names(landsat_alisal) <- c("Coastal Aerosol", "blue", "green", "red", "NIR", "SWIR1", "SWIR2")

# Import shape file
setwd("C:/Users/Student/Desktop/XF/alisal/alisal_fire")
alisal <- vect("ca3451712013120211011_20211003_20211122_burn_bndy.shp")

# Import fire severity
setwd("C:/Users/Student/Desktop/XF/alisal")
alisal_severity <- rast("alisal_severity_2021.tif")

# Check Coordinate Reference System (CRS)
crs(landsat_alisal)
crs(alisal)
crs(alisal_severity)

# Reproject the shape file to the landsat_alisal
alisal_project <- project(alisal, crs(landsat_alisal))
crs(alisal_project)

# Crop and Mask the landsat to the size of fire (shape file)
alisal_cropped <- crop(landsat_alisal, ext(alisal_project))
alisal_mask <- mask(alisal_cropped, alisal_project)
alisal_rescaled <- alisal_mask * 0.0000275 - 0.2

alisal_mask <- clamp(alisal_rescaled, 0, 1)  # Clamp values between 0 and 1
alisal_values <- extract(alisal_mask, alisal_project, df = TRUE)
summary(alisal_values)


# We will also reproject the alisal severity
alisal_severity_project <- project(alisal_severity, crs(landsat_alisal))
crs(alisal_severity_project)

# Check resolution of the landsat and severity
res(alisal_mask)
res(alisal_severity_project)

# resample to match the resolution
alisal_severity_project_resampled <- resample(alisal_severity_project, alisal_mask, method = "bilinear")
res(alisal_severity_project_resampled)

# Extract the severity values
alisal_severity_values <- extract(alisal_severity_project_resampled, alisal_project, df = T)
alisal_final_severity <- alisal_severity_values$alisal_severity_2021
summary(alisal_final_severity)

# NDVI
alisal_NDVI <- NDVI(alisal_values$red, alisal_values$NIR)
mean(alisal_NDVI, na.rm = T)
summary(alisal_NDVI)

# NBR
alisal_NBR <- NBR(alisal_values$NIR, alisal_values$SWIR2)
mean(alisal_NBR, na.rm = T)

# BAI
alisal_BAI <- BAI(alisal_values$red, alisal_values$NIR)
mean(alisal_BAI, na.rm = T)

# linear regression model
alisal_model <- lm(alisal_final_severity ~ alisal_NDVI + alisal_NBR + alisal_BAI)
summary(alisal_model)

# qqplot of residuals
#qqnorm(residuals(alisal_model))
#qqline(residuals(alisal_model))
# most of the point falls along the reference line, though the end of the QQ plot deviates from the reference line, we can still assume the residuals follow a normal distribution

# residual plot
#plot(alisal_model, which = 1)
# The residual plot looks scattered randomly, good to use lm

# Graph
alisal_data <- data.frame(Fire_Severity = alisal_final_severity, 
                          NDVI = alisal_NDVI, 
                          NBR = alisal_NBR, 
                          BAI = alisal_BAI)
alisal_data_long <- melt(alisal_data, id.vars = "Fire_Severity")

alisal_plot <- ggplot(alisal_data_long, aes(x = value, y = Fire_Severity, color = variable)) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Alisal Fire Severity vs. Flame Ability Indices",
       x = "Value",
       y = "Fire Severity",
       color = "Flame Indices") +
  theme_minimal() +
  theme(legend.position = "none")
alisal_plot

mccash_data_long$panel <- "McCash Fire"
sherpa_data_long$panel <- "Sherpa Fire"
gap_data_long$panel <- "Gap Fire"
alisal_data_long$panel <- "Alisal Fire"

combined_data <- rbind(mccash_data_long, sherpa_data_long, gap_data_long, alisal_data_long)
View(combined_data)

# Plot with facet_wrap
final_plot <- ggplot(combined_data, aes(x = value, y = Fire_Severity, color = variable)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fire Severity vs. Flammability Indices",
       x = "Flammability Index Value",
       y = "Fire Severity",
       color = "Flammability Indices") +
  facet_wrap(~ panel, scale = "free") + 
  theme_minimal()+
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)))+
  scale_x_continuous(limits = c(-1,1))+
  scale_color_manual(values = c("red", "blue", "green")) 

final_plot

BAI_plot <- ggplot(combined_data, aes(x = value, y = Fire_Severity, color = variable)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Plot 2: Fire Severity vs. Flammability Indices",
       x = "Flammability Index Value",
       y = "Fire Severity",
       color = "Flammability Indices") +
  facet_wrap(~ panel, scale = "free") + 
  theme_minimal()+
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)))+
  scale_x_continuous(limits = c(-1,500))+
  scale_color_manual(values = c("red", "blue", "green")) 
BAI_plot
