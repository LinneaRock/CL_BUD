#function to read in data for chloride
readCL <- function(X) {
  lab <- read_xlsx("Data/chloride_lab.xlsx", sheet = X) %>%
    mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
    mutate(date = round_date(datetime_collected, "30 minutes")) %>%
    mutate(ID = X)
}
