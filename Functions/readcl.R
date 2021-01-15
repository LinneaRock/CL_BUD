#function to read in data for chloride

readCL <- function(X) {
  lab <- read_xlsx("C:/Users/linne/Box Sync/CL_BUD/Data/chloride_lab.xlsx", sheet = X) %>%
    mutate(datetime_collected = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "GMT")) %>%
    mutate(date = datetime_collected + hours(6)) %>% #actually converting to GMT
    mutate(ID = X) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
                             mon == "November" |
                             mon == "December" |
                             mon == "January" |
                             mon == "February" |
                             mon == "March", "November - March", season),
           season = ifelse(is.na(season), "April - October", season))
    # mutate(season = ifelse(mon == "January" |
    #                          mon == "February" |
    #                          mon == "March", "Winter", season),
    #        season = ifelse(mon == "April" |
    #                          mon == "May" |
    #                          mon == "June", "Spring", season),
    #        season = ifelse(mon == "July" |
    #                          mon == "August" |
    #                          mon == "September", "Summer", season),
    #        season = ifelse(mon == "October" |
    #                          mon == "November" |
    #                          mon == "December", "Fall", season))
   
}
