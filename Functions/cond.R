source("Functions/L_theme.R")



#Function to plot time series of specific conductance 
#df = name of conductivity dataset
cond <- function(df) {
  ggplot(df) +
    geom_line(aes(date, corr_sp.cond), color = "snow3") +
    geom_line(aes(date, runningmean)) +
    labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         x = "") + L_theme()
}

#df1 = epi dataset
#df2 = hypo dataset
#X = hypo name for legend
#Y = epi name for legend
lakecond <- function(df1, df2, X, Y) {
  ggplot() +
    geom_line(df1, mapping = aes(date, runningmean, color = "#1DACE8")) +
    geom_line(df2, mapping = aes(date, runningmean, color = "#1C366B")) +
    labs(x = "",
         y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n") +
    scale_color_manual(labels = c(X, Y),
                       values = c("#1C366B", "#1DACE8")) +
    theme(legend.title = element_blank(), legend.position = "top",
          panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =11),
          axis.title = element_text(size =11),
          legend.text = element_text(size =9))
}


#function to add captions
capt_scseries <- function(customTitle, location) {
  plot_annotation(
    title = customTitle,
    caption = paste("Time series of specific conductivity concentrations in the ",location, ".", sep = ""),
    theme = theme(plot.caption = element_text(hjust = 0)))
} 


#function to plot historical conductance time series 
hist_cond <- function(df) {
  ggplot(df, aes(date, sp.cond)) +
    geom_point() +
    labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         x = "") + L_theme()
}