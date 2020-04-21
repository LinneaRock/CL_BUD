library(tidyverse)

#Function to plot time series of specific conductance 
cond <- function(df, X) {
  ggplot(df, aes(date, sp.cond)) +
    geom_line() +
    labs(y = "Specific Conductivity (µS/cm) @ 25°C\n", 
         x = "\nDate") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =13, face = "bold"),
          axis.title = element_text(size =13, face = "bold"))
  
  ggsave(filename = paste("Plots/conductance_time_series/", X, ".png", sep = ""))
}

cond(upstream, "WC - Upstream")
cond(downstream, "WC - Downstream")


#Function to plot time series of chloride concentrations
cl <- function(df, X) {
  ggplot(df, aes(date, chloride_mgL)) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE)
    labs(y = "Chloride Concentration (mg/L)\n", 
         x = "\nDate") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =13, face = "bold"),
          axis.title = element_text(size =13, face = "bold"))
  
  ggsave(filename = paste("Plots/chloride_time_series/", X, ".png", sep = ""))
}

cl(labSH, "SH")



