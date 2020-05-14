#Function to look at field meter and grab sample chloride concentrations 
#X = field meter dataset
#Y = lab dataset

cl_compare <- function(X, Y) {
  ggplot() +
    geom_point(X, mapping = aes(date, chloride_mgL, color = "Field Chloride")) +
    geom_point(Y, mapping = aes(date, chloride_mgL, color = "Grab Sample Chloride")) +
    labs(x = "",
         y = "Chloride Concentration"~(mg~L^-1)) +
    theme(legend.title = element_blank(), legend.position = "top",
          panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =11),
          axis.title = element_text(size =11),
          legend.text = element_text(size =11))
}