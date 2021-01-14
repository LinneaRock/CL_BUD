
#impute missing data

impute_missing <- function(data_with_seq) {
  
  to_imp <- data_with_seq %>%
    select(date, sp.cond) %>%
    rename(imputed = sp.cond)
  
  imputed <- to_imp %>%
    as.ts() %>%
    na_ma(6, "exponential") %>%
    as.data.frame() %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00", tz = "GMT")) %>%
    left_join(data_with_seq, by = "date") %>%
    mutate(imputed = ifelse(is.na(sp.cond), imputed, NA)) %>%
    mutate(sp.cond = ifelse(is.na(sp.cond), imputed, sp.cond))
  
}

#loggerYN1 <- loggerYN %>%#HOBO conductivity data
# complete(date = seq.POSIXt(as.POSIXct("2020-10-22 10:30:00"), as.POSIXct("2020-10-30 9:30:00"), by = "30 mins")) %>%
#arrange(date)

#loggerYN <- impute_missing(loggerYN1)