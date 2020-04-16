


#function to convert actual conductivity to specific conductivity 
SC <- function(AC, t) {
  AC / (1 - (25 - t) * .019)
}

# Example of how to read in data from different standardized files using a function 
readSP <- function(files) {
  logger <- read.csv(files[1]) # read in first file
  
  for(i in 1:length(files)){ # if there are more than one file, rbind files 
    logger <- rbind(logger,read.csv(files[i]))
  }
  
  logger =  logger %>% 
    drop_na() %>%
    select(Date, Low.Range, Full.Range, Temp) %>%
    mutate(char = as.character(Date),
           date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
    select(date, Low.Range, Full.Range, Temp) %>%
    mutate(sp.cond = SC(Full.Range, Temp))
  
  return(logger)
}

##

##6MC####
#Loading in data from conductivity loggers:
fileNames = c("HOBO_Loggers/6MC/Dec19_Feb4/20758342_6MC.csv", "HOBO_Loggers/6MC/Feb4_Mar16/20758342_6MC.csv")
logger6MC = readSP(fileNames) 

##YS#### 
#Loading in data from conductivity loggers:
loggerYS = readSP(c("HOBO_Loggers/YS/Dec19_Feb3/20758348_YS.csv","HOBO_Loggers/YS/Feb3_Mar16/20758348_YS.csv")) %>% 
  filter(date != "2020-03-16 08:30:00") #getting rid of data that were collected while logger was out of the water



