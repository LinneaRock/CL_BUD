#function to read in data for conductivity from handheld field meter
readfieldcond <- function(X) {
  lab <- read_xlsx("Data/conductivity_field.xlsx", sheet = X) %>%
    mutate(date = as.POSIXct(datetime, format = "%m-%d-%Y %h:%m:%s", tz = "GMT")) %>%
    mutate(date = date + hours(6)) %>% #actually converting to GMT
    mutate(conductivity_uscm = as.numeric(conductivity_uscm)) %>%
    mutate(temp_C = as.numeric(temp_C))
}
