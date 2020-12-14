#chloride_data = chloride dataset (MUST be first)
#other_dataset = the other dataset, e.g. conductivity or discharge



join_datasets_chloride <- function(chloride_data, other_dataset) {
  
  labx <- chloride_data %>%
    mutate(join_time = date) %>%
    mutate(date = date) %>%
    mutate(check = date(date)) %>%
    data.table()
  
  
  d.y <- other_dataset %>%
    mutate(join_time = date) %>%
    mutate(check = date(date)) %>%
    data.table()
  
  setkey(labx, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[labx, roll = "nearest"]%>%
    drop_na(chloride_mgL) %>%
    filter(check == i.check)
}

join_datasets_chloride_predict <- function(chloride_data, other_dataset) {
  
  labx <- chloride_data %>%
    mutate(join_time = date) %>%
    mutate(date = date) %>%
    mutate(check = date(date)) %>%
    data.table()
  
  
  d.y <- other_dataset %>%
    mutate(join_time = date) %>%
    mutate(check = date(date)) %>%
    data.table()
  
  setkey(labx, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[labx, roll = "nearest"]%>%
    drop_na(chloride_predict) %>%
    filter(check == i.check)
}
