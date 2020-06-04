##Function to join conductivity and discharge datasets together then plots specific conductivity vs. discharge

q.sc <- function(dfx, dfy) {
  qsc <- dfx %>%
    left_join(dfy, by = "date")
  
  ggplot(qsc, aes(discharge, sp.cond)) +
    geom_point() +
    #stat_cor() + 
    #stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
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
evalqec <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  info <- lm(sp.cond ~ discharge, d)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
infoqec <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  info <- lm(sp.cond ~ discharge, d)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
captqec <- function(customTitle, location, df1, df2) {
  plot_annotation(
    title = customTitle,
    caption = paste("Concentration - Discharge relationship in the",location, ". The linear regression is 
represented by the equation y=", round(coef(infoqec(df1, df2))[2,1], 4), "x + ", round(coef(infoqec(df1, df2))[1,1], 4), ".", " The correlation has an r-squared value of 
", round(glance(infoqec(df1, df2))$r.squared, 4), " and a p-value of ", round(glance(infoqec(df1, df2))$p.value, 4), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 
