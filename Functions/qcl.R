source("Functions/L_theme.R")

#Function to join chloride and discharge datasets together then plots chloride vs. discharge
#cl = chloride data
#discharge = discharge data

source("Functions/join_datasets_chloride.R")

q.cl <- function(cl, other) {

  qsc <- join_datasets_chloride(cl, other)
  
  
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
evalq <- function(cl, other) {

  qsc <- join_datasets_chloride(cl, other)
  
  info <- lm(chloride_mgL ~ discharge, qsc)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
infoq <- function(cl, other) {
 
  qsc <- join_datasets_chloride(cl, other)
  
  info <- lm(chloride_mgL ~ discharge, qsc)
  
  #print coefficient information
  return(summary(info))
  
}

#function to add captions
captqc <- function(customTitle, location, dfx, dfy) {
  plot_annotation(
    title = customTitle,
    caption = paste("Concentration - Discharge relationship in the",location, ". The linear regression is 
represented by the equation y=", round(coef(infoq(dfx, dfy))[2,1], 4), "x + ", round(coef(infoq(dfx, dfy))[1,1], 4), ". The correlation has an r-squared value of ", round((infoq(dfx, dfy))$adj.r.squared, 4), " 
and a p-value of ", round(coef(infoq(dfx, dfy))[2,4], 2), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 
