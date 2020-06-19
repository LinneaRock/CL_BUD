readSP <- function(files) {
  logger <- read.csv(files[1]) # read in first file
  
  for(i in 1:length(files)){ # if there are more than one file, rbind files 
    logger <- rbind(logger,read.csv(files[i]))
  }
  
  logger =  logger %>% 
    drop_na() %>%
    select(Date, Low.Range, Full.Range, Temp) %>%
    mutate(#char = as.character(Date),
           date = as.POSIXct(strptime(Date, format = "%m-%d-%Y %H:%M:%S", tz = 'GMT'))) %>%
    select(date, Low.Range, Full.Range, Temp) %>%
    mutate(sp.cond = SC(Full.Range, Temp))
  
  return(logger)
}


