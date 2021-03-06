source("Functions/L_theme.R")

#Function to join chloride and discharge datasets together then plots chloride vs. discharge
#cl = chloride data
#discharge = discharge data

source("Functions/join_datasets_chloride.R")

q.cl.predict <- function(cl, other) {
  
  qsc <- join_datasets_chloride_predict(cl, other)
  
  
  ggplot(qsc, aes(runningmeandis, chloride_predict)) +
    geom_point() +
    #stat_cor() + 
    #stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = bquote("Chloride Concentration"~(mg~L^-1)~"\n"), 
         x = "\nDischarge"~(m^3~s^-1)) +
    L_theme()
}

#function to evaluate residuals
evalq.predict <- function(cl, other) {
  
  qsc <- join_datasets_chloride_predict(cl, other)
  
  info <- lm(chloride_predict ~ runningmeandis, qsc)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
infoq.predict <- function(cl, other) {
  
  qsc <- join_datasets_chloride_predict(cl, other)
  
  info <- lm(chloride_predict ~ runningmeandis, qsc)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
captqc.predict <- function(customTitle, location, dfx, dfy) {
  plot_annotation(
    title = customTitle,
    caption = paste("Concentration - Discharge relationship in the ",location, ". The linear regression is 
represented by the equation y = ", round(coef(infoq(dfx, dfy))[2,1], 2), "x + ", round(coef(infoq(dfx, dfy))[1,1], 2), ". The correlation has an r-squared value of ", round((infoq(dfx, dfy))$adj.r.squared, 2), " 
and a p-value of ", round(coef(infoq(dfx, dfy))[2,4], 6), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 

