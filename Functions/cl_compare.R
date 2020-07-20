#Function to look at field meter and grab sample chloride concentrations 
#X = field meter dataset
#Y = lab dataset

cl_compare <- function(X, Y) {
  ggplot() +
    geom_point(X, mapping = aes(date, chloride_mgL, color = "Field Chloride")) +
    geom_point(Y, mapping = aes(date, chloride_mgL, color = "Grab Sample Chloride")) +
    labs(x = "",
         y = "Chloride Concentration"~(mg~L^-1)) +
    L_theme()
}