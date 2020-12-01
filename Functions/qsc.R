
source("Functions/L_theme.R")

##Function to join conductivity and discharge datasets together then plots specific conductivity vs. discharge
#cond = logger dataset
#discharge_data = discharge dataset

source("Functions/join_datasets_cond.R")

q.sc <- function(cond, discharge_data) {
  
  qsc <- join_datasets_cond(cond, discharge_data)
  
  ggplot(qsc, aes(runningmeandis, runningmean)) +
    geom_point() +
    #stat_cor() + 
    #stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         x = "\nDischarge"~(m^3~s^-1)) +
    L_theme() +
    geom_jitter(width = 0.5, size = 1)
    
}

#function to evaluate residuals
evalqec <- function(cond, discharge_data) {
 
  qsc <- join_datasets_cond(cond, discharge_data)
  
  info <- lm(runningmean ~ runningmeandis, qsc)
  
  #print plots
    layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
infoqec <- function(cond, discharge_data) {
  
  qsc <- join_datasets_cond(cond, discharge_data)
  
  info <- lm(runningmean ~ runningmeandis, qsc)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
captqec <- function(customTitle, location, df1, df2) {
  plot_annotation(
    title = customTitle,
    caption = paste("Concentration - Discharge relationship in the ",location, ". The linear regression is 
represented by the equation y = ", round(coef(infoqec(df1, df2))[2,1], 2), "x + ", round(coef(infoqec(df1, df2))[1,1], 2), ". The correlation has an r-squared value of ", round((infoqec(df1, df2))$adj.r.squared, 2), " 
and a p-value of ", round(coef(infoqec(df1, df2))[2,4], 6), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 
