
L_theme <- function() {
theme(panel.background = element_rect(fill = "white", colour = "white",
                                      size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                      colour = "gray88"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "gray88"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      plot.caption = element_text(size = 10, hjust = 0),
      legend.title = element_blank(),
      plot.title = element_text(size = 10, face = "plain")) 
}
