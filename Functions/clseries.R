#Function to plot time series of chloride
clseries <- function(df) {
  ggplot(df, aes(date, chloride_mgL)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(y = "Chloride Concentration"~(mg~L^-1)~"\n", 
       x = "") + L_theme()
}



#function to add captions
capt_clseries <- function(customTitle, location) {
  plot_annotation(
    title = customTitle,
    caption = paste("Time series of chloride concentrations in the ",location, ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 

