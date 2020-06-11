library(tidyverse)
library(lubridate)

level.data <- read_rds("Data/HOBO_Loggers/YS/Feb3_Mar16/level_data.rds")

#Methods adapted from Onset's Tech Notes on their Barometric Compensation Method: https://www.onsetcomp.com/support/tech-note/barometric-compensation-method/

#1) temperature and density corrected depth array is computed. This is the depth assuming all pressure is from hydraulic head (no air pressure)
  #a) For this, water density must be calculated and converted to [lb m^-3]
  #b) Then, the measured pressure values are converted to density dependent fluid depths in meters


###Water Density [kg m^-3]; x = temperature in °C
watden <- function(x){
  pt1 <- x + 288.9414
  pt2 <- 508929 * (x + 68.129630)
  pt3 <- (x - 3.9863) ^2
  1000 * (1 - (pt1/pt2) * pt3)
}

#density is converted to [lb ft^-3] via:
densft <- function(d) {
  d * .0624279606
}

#Array of downwell pressure values are converted to a density dependent fluid depth array [m] via: 
#D= FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * P) / ρ   
#P = measured pressure, rho = calculated density in [lb ft^-3]
#The same equation is used to calculate the Barometric Depth, using Barometric pressure rather than measured absoulte pressure
D <- function(P, rho) {
  .3048 * (.1450377 * P)/rho
}

#2) Add columns for density and depths
level.data <- level.data %>%
  mutate(Density_kgm3 = watden(Temp)) %>%
  mutate(Density_lbft3 = densft(Density_kgm3)) %>%
  mutate(Density_Dependent_Depth = D(Pressure, Density_lbft3)) %>%
  mutate(Barometric_Depth = D(Bar.Pressure, Density_lbft3))


#3) Calculate water depth
  #a) calculate the compensation depth using measured depth to sensor, reference density dependent depth, and barometric depth at reference time
  #b) calculate the real length (length from surface to sensor) using compensation constant and barometric compensated depth
  #c) calculate the water depth by adding the depth to the bottom of the lake

#The barometric pressure at the reference time is converted to a barometric “depth” 
D.bar0 <- level.data$Barometric_Depth[1]
#The Density dependent depth at reference time
D.ref <- level.data$Density_Dependent_Depth[1] 

#compensation constant is determined using the measured length (.085m) at the time of deployment and the barometric depth at the reference time
k <- .085 - (D.ref - D.bar0)

#Water level from reference point is determined using the following equation:
L <- function(D, BD) {
  D - BD + k
}

level.data <- level.data %>%
  mutate(Lreal = L(Density_Dependent_Depth, Barometric_Depth)) %>%
  mutate(Water_Depth = Lreal + .4225) #Add the depth from substrate to the reference depth for the total water depth at the sampling location in meters
 



