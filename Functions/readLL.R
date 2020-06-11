
readLL <- function(HOBO, AOS) {
  logger <- read.csv(HOBO[1]) # read in first file
  
  for(i in 1:length(HOBO)){ # if there are more than one file, rbind files(HOBO) 
    logger <- rbind(logger,read.csv(HOBO[i]))
  }
  
  logger <- logger %>% 
    mutate(char = as.character(Date),
           Date = as_datetime(char)) %>%
    select(Date, Pressure, Temp)
  
  AOS <- read.csv(AOS) %>% #The file downloaded from AOS
    mutate(Bar.Pressure = Pressure * .1) %>% #convert hPa to kPa
    mutate(char = as.character(Date)) %>%
    mutate(char = gsub("T", " ", char),
           char = gsub("Z", "", char)) %>%
    mutate(date = as_datetime(char)) %>%
    select(date, Bar.Pressure) %>%
    rename(Date = date)
  
  level.data <- left_join(levellogger, AOS, by = "Date")
  write_rds(level.data, "Data/HOBO_Loggers/YS/Feb3_Mar16/level_data.rds") 
}


