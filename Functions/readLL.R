
readLL <- function(HOBO, AOS, subfile) {
  logger <- read.csv(HOBO[1]) # read in first file
  
  for(i in 1:length(HOBO)){ # if there are more than one file, rbind files(HOBO) 
    logger <- rbind(logger,read.csv(HOBO[i]))
  }
  
  levellogger <- logger %>% 
    mutate(char = as.character(Date),
           Date = as_datetime(char)) %>%
           #Date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
    select(Date, Pressure, Temp)
  
  AOS <- read.csv(AOS) %>% #The file downloaded from AOS
    mutate(Bar.Pressure = Pressure * .1) %>% #convert hPa to kPa
    mutate(char = as.character(Date)) %>%
    mutate(char = gsub("T", " ", char),
           char = gsub("Z", "", char)) %>%
    mutate(date = as_datetime(char)) %>%
    #mutate(Date = as.POSIXct(date, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
    select(date, Bar.Pressure) %>%
    rename(Date = date)
  
  level.data <- left_join(levellogger, AOS, by = "Date")  %>%
    mutate(Date = as.POSIXct(Date, format = "%Y-%M-%D %H:%M:%S", tz = "America/Chicago"))
  
  write_rds(level.data, paste("Data/HOBO_Loggers/YS/", subfile, "/level_data.rds", sep = "")) 
}


