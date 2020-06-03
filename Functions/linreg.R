#Function for linear regression of specific conductance vs. chloride
#df1 = chloride dataset
#df2 = conductivity dataset
linreg <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date") %>%
    na.omit()
  
  ggplot(d, aes(sp.cond, chloride_mgL)) +
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
eval <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  info <- lm(chloride_mgL ~ sp.cond, d)
  
  #print plots
  layout(matrix(1:4,2,2))
  return(plot(info))
  
}


#function to obtain coefficient information 
info <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  info <- lm(chloride_mgL ~ sp.cond, d)
  
  #print coefficient information
  return(summary(info))
  
}
