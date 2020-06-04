#Function to join chloride and discharge datasets together then plots chloride vs. discharge

q.cl <- function(dfx, dfy) {
  qsc <- dfx %>%
    left_join(dfy, by = "date") %>%
    na.omit()
  
  ggplot(qsc, aes(discharge, chloride_mgL)) +
    geom_point() +
    #stat_cor() + 
    #stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = bquote("Chloride Concentration"~(mg~L^-1)~"\n"), 
         x = "\nDischarge"~(m^3~s^-1)) +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =11),
          axis.title = element_text(size =11))  
}

#function to evaluate residuals
evalq <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  info <- lm(chloride_mgL ~ discharge, d)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
infoq <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  info <- lm(chloride_mgL ~ discharge, d)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
captqc <- function(customTitle, location, df1, df2) {
  plot_annotation(
    title = customTitle,
    caption = paste("Concentration - Discharge relationship in the",location, ". The linear regression is 
represented by the equation y=", round(coef(infoq(df1, df2))[2,1], 4), "x + ", round(coef(infoq(df1, df2))[1,1], 4), ". The correlation has an r-squared value of ", round(glance(infoq(df1, df2))$r.squared, 4), " 
and a p-value of ", round(glance(infoq(df1, df2))$p.value, 4), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 
