#function to read in data for conductivity from handheld field meter

source("Functions/SC.R")
source("Functions/SC_backwards.R")

readfieldcond <- function(X, ID) {
  lab <- read_xlsx("Data/conductivity_field.xlsx", sheet = X) %>%
    mutate(date = as.POSIXct(datetime, format = "%m-%d-%Y %h:%m:%s", tz = "Etc/GMT-6")) %>% #CST
    mutate(conductivity_uscm = as.numeric(conductivity_uscm)) %>%
    mutate(temp_C = as.numeric(temp_C)) %>%
    mutate(sp.cond = SC(conductivity_uscm, temp_C)) %>%
    mutate(ID = ID)
  
  lab2 <- lab %>%
    mutate(sp.cond = ifelse(month(date) == "1" & 
                              day(date) == "19", conductivity_uscm, sp.cond)) %>%
    mutate(conductivity_uscm = ifelse(month(date) == "1" & 
                                        day(date) == "19", SC_backwards(sp.cond, temp_C), conductivity_uscm))
}
