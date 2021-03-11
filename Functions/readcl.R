#function to read in data for chloride

readCL <- function(X) {
  lab <- read_xlsx("C:/Users/linne/Box Sync/CL_BUD/Data/chloride_lab.xlsx", sheet = X) %>%
    mutate(datetime_collected = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "Etc/GMT-6")) %>% #CST
    mutate(ID = X) %>%
    mutate(date = datetime_collected) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
                             mon == "November" |
                             mon == "December" |
                             mon == "January" |
                             mon == "February" |
                             mon == "March", "November - March", season),
           season = ifelse(is.na(season), "April - October", season))

}
