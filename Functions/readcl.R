#function to read in data for chloride

readCL <- function(X) {
  lab <- read_xlsx("C:/Users/linne/Box Sync/CL_BUD/Data/chloride_lab.xlsx", sheet = X) %>%
    mutate(datetime_collected = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "GMT")) %>%
    mutate(datetime_collected = datetime_collected + hours(6)) %>% #actually converting to GMT
    mutate(ID = X)
}
