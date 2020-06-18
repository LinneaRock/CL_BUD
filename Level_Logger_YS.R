library(tidyverse)

level_Mar3 <- read_rds("Data/HOBO_Loggers/YS/Feb3_Mar16/level_data.rds")
level_Mar16 <- read_rds("Data/HOBO_Loggers/YS/Mar16_Jun17/level_data.rds")

level.data <- rbind(level_Mar3, level_Mar16)

#Methods adapted from Onset's Tech Notes on their Barometric Compensation Method: https://www.onsetcomp.com/support/tech-note/barometric-compensation-method/

#1) temperature and density corrected depth array is computed. This is the depth assuming all pressure is from hydraulic head (no air pressure)
  #a) For this, water density must be calculated and converted to [lb m^-3]
  #b) Then, the measured pressure values are converted to density dependent fluid depths in meters

###Water Density [kg m^-3]; x = temperature in °C
waterDensity <- function(temp){
  pt1 <- temp + 288.9414
  pt2 <- 508929 * (temp + 68.129630)
  pt3 <- (temp - 3.9863) ^2
  1000 * (1 - (pt1/pt2) * pt3)
}

#density is converted to [lb ft^-3] via:
densityFT <- function(waterDensity) {
  waterDensity * 0.0624279606
}

#Array of downwell pressure values are converted to a density dependent fluid depth array [m] via: 
#D= FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * P) / ρ   
#P = measured pressure, rho = calculated density in [lb ft^-3]
#The same equation is used to calculate the Barometric Depth, using Barometric pressure rather than measured absoulte pressure

fluidDepth <- function(Pressure, denistyFT) {
  0.3048 * (0.1450377 * 144.0 * Pressure)/denistyFT
}

#2) Add columns for density and depths
level.data <- level.data %>%
  mutate(Density_kgm3 = waterDensity(Temp)) %>%
  mutate(Density_lbft3 = densityFT(Density_kgm3)) %>%
  mutate(Density_Dependent_Depth = fluidDepth(Pressure, Density_lbft3)) %>%
  mutate(Barometric_Depth = fluidDepth(Bar.Pressure, Density_lbft3))

#3) Calculate water depth
  #a) calculate the compensation depth using measured depth to sensor, reference density dependent depth, and barometric depth at reference time
  #b) calculate the real length (length from surface to sensor) using compensation constant and barometric compensated depth
  #c) calculate the water depth by adding the depth to the bottom of the lake

#The barometric pressure at the reference time is converted to a barometric “depth” 
barometricDepth_Mar3 <- level.data$Barometric_Depth[1]
barometricDepth_Mar16 <- level.data$Barometric_Depth[Date = "2020-03-16 10:00:00"]
#The Density dependent depth at reference time
referenceDepth_Mar3 <- level.data$Density_Dependent_Depth[1] 

#compensation constant is determined using the measured length (.085m) at the time of deployment and the barometric depth at the reference time
k_Mar3 <- .085 - (referenceDepth_Mar3 - barometricDepth_Mar3)


#Water level from reference point is determined using the following equation:
waterLevel <- function(densityDepth, barometricDepth) {
  densityDepth - barometricDepth + k_Mar3
}

level.data <- level.data %>%
  mutate(Lreal = L(Density_Dependent_Depth, Barometric_Depth)) %>%
  mutate(Water_Depth = Lreal + .4225) #Add the depth from substrate to the reference depth for the total water depth at the sampling location in meters



ggplot(level.data) +
  geom_line(aes(Date, Water_Depth)) 
ggplot(level.data) +
  geom_line(aes(Date, -(Bar.Pressure - Pressure)))




