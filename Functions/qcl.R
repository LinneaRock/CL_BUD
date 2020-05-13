#Function to join chloride and discharge datasets together then plots chloride vs. discharge

q.cl <- function(dfx, dfy, X) {
  qsc <- dfx %>%
    left_join(dfy, by = "date")
  
  ggplot(qsc, aes(discharge, chloride_mgL)) +
    geom_point() +
    #stat_cor() + 
    #stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = "Chloride Concentration (mg/L)\n", 
         x = "\nDischarge (m^3/S)") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =11),
          axis.title = element_text(size =11))  
}