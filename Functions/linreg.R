#Function for linear regression of specific conductance vs. chloride
#cl = chloride dataset
#other = conductivity dataset

source("Functions/join_datasets_chloride.R")

linreg <- function(chloride_data, field_cond, logger) {
 
  qsc <- join_for_linreg(chloride_data, field_cond, logger)
  
  
  ggplot(qsc, aes(sp.cond.x, chloride_mgL)) +
    geom_point(aes(color = season)) + 
    scale_color_manual(labels = c("April-October", "November-March"),
                       values = c("#1C366B", "#F24D29"))  +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    #stat_cor() + 
    #stat_regline_equation() + 
    labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         y = "\nChloride Concentration"~(mg~L^-1)) +
    L_theme()
    
}




#function to evaluate residuals
eval <- function(choride_data, field_cond, logger) {
  
  qsc <- join_for_linreg(choride_data, field_cond, logger)
  
  info <- lm(chloride_mgL ~ sp.cond.x, qsc)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
info <- function(choride_data, field_cond, logger) {
  
  qsc <- join_for_linreg(choride_data, field_cond, logger)
  
  info <- lm(chloride_mgL ~ sp.cond.x, qsc)
  
  #print coefficient information
  return(summary(info))
  
}

#funtion to calculate p-value from f statistic 
pvalue <- function(choride_data, field_cond, logger) {
  
  qsc <- join_for_linreg(choride_data, field_cond, logger)
  
  info <- lm(chloride_mgL ~ sp.cond.x, qsc)
  
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




# #using segmented
library(segmented)
   
seglinreg <- function(chloride_data, conductance_data) {
 
  joined <- join_datasets_chloride(chloride_data, conductance_data)
  orig.lm <- lm(chloride_mgL ~ runningmean, joined)
  seg.lm <- segmented(orig.lm, sseg.Z = ~runningmean)
  fit <- seg.lm$fit
  joined <- joined %>%
    mutate(fit = fit)
  
  ggplot(joined, aes(x = runningmean, y = chloride_mgL)) + 
    geom_point(aes(color = season)) +
    scale_color_viridis_d() +
    geom_line(aes(x = runningmean, y = fit)) +
    L_theme() +
    labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         y = "\nChloride Concentration"~(mg~L^-1))
}

# joined <- join_datasets_chloride(labYI, YI_cond_data)
# model <- lm(chloride_mgL ~ ifelse(season == "October - April", runningmean, 0) + ifelse(season == "May - September", runningmean, 0), data = joined)
#  
# summary(model)
#  
#  
#  
 
 
 
 
 
 
 

















