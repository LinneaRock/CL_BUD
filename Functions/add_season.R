add_season <- function(data) {
  data <- data %>%
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
