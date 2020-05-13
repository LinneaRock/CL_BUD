#Function for linear regression of specific conductance vs. chloride for historical data

histlinreg <- function(df) {
  ggplot(df, aes(chloride_mgL, sp.cond)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    #stat_cor() + 
    #stat_regline_equation() + 
    labs(y = "Specific Conductivity (µS/cm) @ 25°C\n", 
         x = "\nChloride Concentration (mg/L)") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11))
}
