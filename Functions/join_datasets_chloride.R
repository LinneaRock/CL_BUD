#cl = chloride dataset (MUST be first)
#other = the other dataset, e.g. conductivity or discharge



join_datasets_chloride <- function(cl, other) {
  
  labx <- cl %>%
    mutate(join_time = date) %>%
    mutate(date = date) %>%
    mutate(check = date(date)) %>%
    data.table()
  
  
  d.y <- other %>%
    mutate(join_time = date) %>%
    mutate(check = date(date)) %>%
    data.table()
  
  setkey(labx, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[labx, roll = "nearest"]%>%
    drop_na(chloride_mgL) %>%
    filter(check == i.check)
}
