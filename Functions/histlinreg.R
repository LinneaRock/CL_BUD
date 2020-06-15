#Function for linear regression of specific conductance vs. chloride for historical data

histlinreg <- function(df) {
  ggplot(df, aes(chloride_mgL, sp.cond)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    #stat_cor() + 
    #stat_regline_equation() + 
    labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         x = "\nChloride Concentration"~(mg~L^-1)) +
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
eval <- function(df) {
  
  info <- lm(chloride_mgL ~ sp.cond, df)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}

#function to obtain coefficient information 
info <- function(df) {
  
  info <- lm(chloride_mgL ~ sp.cond, df)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
capthlm <- function(customTitle, location, df) {
  plot_annotation(
    title = customTitle,
    caption = paste("Chloride concentration vs. specific conductivity relationship in the ",location, ". The 
linear regression is represented by the equation y=", round(coef(info(df))[2,1], 4), "x + ", round(coef(info(df))[1,1], 4), ". The correlation has an r-squared 
value of ", round(glance(info(df))$r.squared, 4),"and a p-value of ", round(glance(info(df))$p.value, 4), ".", " Data from Public Health Madison Dane County.", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 