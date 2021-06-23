library(tidyverse)
library(zoo)

#Function to convert raw, absolute conductivity to specific conductivit @ 25 degrees C
convert_to_sp.cond <- function(ActualCond, temp) {
  ActualCond / (1 - (25 - temp) * 0.021)
}



