#chloride_data = chloride dataset (MUST be first)
#other_dataset = the other dataset, e.g. conductivity or discharge

join_for_linreg <- function(chloride_data, field_cond, logger) {
  
  cl_edit <- chloride_data %>%
    select(date, chloride_mgL, mon, season, ID) %>%
    mutate(date2 = round_date(date, unit = "30 minutes"))
  cond_edit <- field_cond %>%
    select(date, sp.cond, ID)  %>%
    mutate(date2 = round_date(date, unit = "30 minutes"))
  
  join <- left_join(cl_edit, cond_edit, by = "date2") %>%
    select(date2, chloride_mgL, sp.cond, season) %>%
    rename(date = date2)
  join2 <- left_join(join %>% mutate(date2 = as.character(date)), logger %>% mutate(date2 = as.character(date)), by = "date2")
  join2 <- join2 %>% 
    mutate(sp.cond.x = ifelse(is.na(sp.cond.x), runningmean, sp.cond.x))
  
}



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
