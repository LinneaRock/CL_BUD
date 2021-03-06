#function to read in data for chloride from handheld field YSI meter
readfieldcl <- function(X) {
  lab <- read_xlsx("Data/chloride_field.xlsx", sheet = X) %>%
    mutate(date = as.POSIXct(datetime, format = "%m-%d-%Y %h:%m:%s", tz = "GMT")) %>% #GMT -5:00
    mutate(date = round_date(datetime, "30 minutes")) %>%
    mutate(date = date + hours(6)) #actually converting to GMT
}