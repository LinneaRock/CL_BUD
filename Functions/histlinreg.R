
#Function for linear regression of specific conductance vs. chloride for historical data

histlinreg <- function(df) {
  ggplot(df, aes(sp.cond, chloride_mgL)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    #stat_cor() + 
    #stat_regline_equation() + 
    labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         y = "\nChloride Concentration"~(mg~L^-1)) +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11))
}

#function to evaluate residuals
evalh <- function(df) {
  
  info <- lm(chloride_mgL ~ sp.cond, df)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}

#function to obtain coefficient information 
infoh <- function(df) {
  
  info <- lm(chloride_mgL ~ sp.cond, df)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
capthlm <- function(customTitle, location, df) {
  plot_annotation(
    title = customTitle,
    caption = paste("Chloride concentration vs. specific conductivity relationship in the ",location, ". The 
linear regression is represented by the equation y=", round(coef(infoh(df))[2,1], 4), "x + ", round(coef(infoh(df))[1,1], 4), ". The correlation has an r-squared 
value of ", round(glance(infoh(df))$r.squared, 4),"and a p-value of ", round(glance(infoh(df))$p.value, 4), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 