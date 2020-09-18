
#cond = conducvitity dataset (MUST be first)
#discharge = discharge dataset

join_datasets_cond <- function(cond, discharge) {
  
  labx <- cond %>%
    mutate(join_time = date) %>%
    mutate(date = date) %>%
    data.table()
  
  
  d.y <- discharge %>%
    mutate(join_time = date) %>%
    data.table()
  
  setkey(labx, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[labx, roll = "nearest"] %>%
    distinct()
  
}
