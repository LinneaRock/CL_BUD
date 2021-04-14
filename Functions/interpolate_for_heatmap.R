####### Load packages 
library(tidyverse)
library(lubridate)
library(scales)

# Vertical linear interpolation of water column concentrations  -- function
interpData <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$chloride_mgL)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(chloride_mgL))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$chloride_mgL, xout = c(0, 5, 10, 15, 20, 23.5), rule = 2)
  return(yout$y)
}

