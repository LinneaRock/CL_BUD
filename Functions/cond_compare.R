#function to compare HOBO collected actual cond measurements to handheld meter collected measurements
#X = field data
#Y = HOBO data
cond_compare <- function(X, Y) {
  ggplot() +
    geom_line(Y, mapping = aes(date, Low.Range, color = "Low Range")) +
    geom_line(Y, mapping = aes(date, Full.Range, color = "Full Range")) +
    geom_point(X, mapping = aes(date, conductivity_uscm, color = "Field Snapshot")) +
    labs(x = "",
         y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C) +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =11),
          axis.title = element_text(size =11),
          legend.text = element_text(size =9)) +
    scale_color_manual("",
                       breaks = c("Low Range", "Full Range", "Field Snapshot"),
                       values = c("grey", "black", "red")) 
}
