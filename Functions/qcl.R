#Function to join chloride and discharge datasets together then plots chloride vs. discharge
#dfx = chloride data
#dfy = discharge data

q.cl <- function(dfx, dfy) {
  labx <- dfx %>%
    mutate(join_time = datetime_collected) %>%
    mutate(date = datetime_collected) %>%
    data.table()
  
  
  d.y <- dfy %>%
    mutate(join_time = date) %>%
    data.table()
  
  setkey(labx, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[labx, roll = "nearest"] %>%
    drop_na(chloride_mgL)
  
  
  ggplot(qsc, aes(discharge, chloride_mgL)) +
    geom_point() +
    #stat_cor() + 
    #stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = bquote("Chloride Concentration"~(mg~L^-1)~"\n"), 
         x = "\nDischarge"~(m^3~s^-1)) +
    L_theme()
}

#function to evaluate residuals
evalq <- function(dfx, dfy) {
  labx <- dfx %>%
    mutate(join_time = datetime_collected) %>%
    mutate(date = datetime_collected) %>%
    data.table()
  
  
  d.y <- dfy %>%
    mutate(join_time = date) %>%
    data.table()
  
  setkey(labx, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[labx, roll = "nearest"] %>%
    drop_na(chloride_mgL)
  
  info <- lm(chloride_mgL ~ discharge, qsc)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
infoq <- function(dfx, dfy) {
  labx <- dfx %>%
    mutate(join_time = datetime_collected) %>%
    mutate(date = datetime_collected) %>%
    data.table()
  
  
  d.y <- dfy %>%
    mutate(join_time = date) %>%
    data.table()
  
  setkey(labx, join_time)
  setkey(d.y, join_time)
  
  qsc <- d.y[labx, roll = "nearest"] %>%
    drop_na(chloride_mgL)
  
  info <- lm(chloride_mgL ~ discharge, qsc)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
captqc <- function(customTitle, location, dfx, dfy) {
  plot_annotation(
    title = customTitle,
    caption = paste("Concentration - Discharge relationship in the",location, ". The linear regression is 
represented by the equation y=", round(coef(infoq(dfx, dfy))[2,1], 4), "x + ", round(coef(infoq(dfx, dfy))[1,1], 4), ". The correlation has an r-squared value of ", round(glance(infoq(dfx, dfy))$r.squared, 4), " 
and a p-value of ", round(glance(infoq(dfx, dfy))$p.value, 4), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 
