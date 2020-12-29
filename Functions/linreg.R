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

#using segmented
joined <- join_datasets_chloride(labYS, YS_cond_data)
orig.lm <- lm(chloride_mgL ~ runningmean, joined)
summary(orig.lm)
seg.lm <- segmented(orig.lm, seg.Z = ~runningmean)
summary(seg.lm)
seg.lm$psi[2]
upper <- joined %>% filter(runningmean > seg.lm$psi[2])
lower <- joined %>% filter(runningmean < seg.lm$psi[2])



plot_segreg <- function(data) {
  
  ggplot(data, aes(runningmean, chloride_mgL)) +
  geom_point(aes(color = season)) + 
  scale_color_viridis_d() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  #stat_cor() + 
  #stat_regline_equation() + 
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  L_theme()
}

info_seg_reg <- function(data) {
  info <- lm(chloride_mgL ~ runningmean, data)
  return(summary(info))
}

plot_segreg(upper)
info_seg_reg(upper)
plot_segreg(lower)
info_seg_reg(lower)


ggplot() +
  geom_point(upper, mapping = aes(runningmean, chloride_mgL, color = season)) + 
  geom_point(lower, mapping = aes(runningmean, chloride_mgL, color = season)) + 
  scale_color_viridis_d() +
  geom_smooth(upper, mapping = aes(runningmean, chloride_mgL),  method = "lm", se = FALSE, color = "black") +
  geom_smooth(lower, mapping = aes(runningmean, chloride_mgL),  method = "lm", se = FALSE, color = "black") +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  L_theme()

#using dates Nov - April

salting <- joined %>% filter(date >= "2019-10-15 00:00:00" &
                               date <= "2020-04-15 23:30:00" |
                               date >= "2020-10-15 00:00:00" &
                               date <= "2021-04-15 23:30:00")


# 
# (mon == "January" |
#                                mon == "February" |
#                                mon == "March" |
#                                mon == "April" |
#                                mon == "November" |
#                                mon == "December")
not_salting <- joined %>% filter(date >= "2020-04-16 00:00:00" &
                                   date <= "2020-10-14 23:30:00" |
                                   date >= "2021-04-16 00:00:00" &
                                   date <= "2021-10-14 23:30:00")



# (mon == "May" |
#                                    mon == "June" |
#                                    mon == "July" |
#                                    mon == "August" |
#                                    mon == "September" |
#                                    mon == "October")


info_seg_reg(salting)
plot_segreg(salting)

info_seg_reg(not_salting)
plot_segreg(not_salting)


ggplot() +
  geom_point(salting, mapping = aes(runningmean, chloride_mgL, color = season)) + 
  geom_point(not_salting, mapping = aes(runningmean, chloride_mgL, color = season)) + 
  scale_color_viridis_d() +
  geom_smooth(salting, mapping = aes(runningmean, chloride_mgL),  method = "lm", se = FALSE, color = "black") +
  geom_smooth(not_salting, mapping = aes(runningmean, chloride_mgL),  method = "lm", se = FALSE, color = "black") +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  L_theme()
