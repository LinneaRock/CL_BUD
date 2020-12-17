#Function for linear regression of specific conductance vs. chloride
#cl = chloride dataset
#other = conductivity dataset

source("Functions/join_datasets_chloride.R")

linreg <- function(cl, other) {
 
  qsc <- join_datasets_chloride(cl, other)
  
  
  ggplot(qsc, aes(runningmean, chloride_mgL)) +
    geom_point(aes(color = season)) + 
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    #stat_cor() + 
    #stat_regline_equation() + 
    labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         y = "\nChloride Concentration"~(mg~L^-1)) +
    L_theme()
    
}




#function to evaluate residuals
eval <- function(cl, other, filename) {
 
  qsc <- join_datasets_chloride(cl, other)
  
  info <- lm(chloride_mgL ~ runningmean, qsc)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
info <- function(cl, other) {
  
  qsc <- join_datasets_chloride(cl, other)
  
  info <- lm(chloride_mgL ~ runningmean, qsc)
  
  #print coefficient information
  return(summary(info))
  
}

#funtion to calculate p-value from f statistic 
pvalue <- function(cl,other) {
  qsc <- join_datasets_chloride(cl, other)
  
  info <- lm(chloride_mgL ~ runningmean, qsc)
  
  pvalue <- 1-pf(as.numeric(info$fstatistic[1]), as.numeric(info$fstatistic[2]), as.numeric(info$fstatistic[3]))
  
  return(pvalue)
}

#function to add captions
captlm <- function(customTitle, location, cl, other) {
  plot_annotation(
    title = customTitle,
    caption = paste("Chloride concentration vs. specific conductivity relationship in the ",location, ". The 
linear regression is represented by the equation y=", round(coef(info(cl, other))[2,1], 2), "x + ", round(coef(info(cl, other))[1,1], 2), ". The correlation has an r-squared 
value of ", round((info(cl, other)$adj.r.squared), 2)," and a p-value of ", round(coef(info(cl, other))[2,4], 6), ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 








#round(coef(info(cl, other))[2,4], 2) #another option to get the p-value (I think)





















