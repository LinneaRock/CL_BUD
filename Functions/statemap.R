#Function for mapping statewide chloride data from WQP

statemap <- function(x, years) {
  ggplot() +
    geom_polygon(counties, mapping = aes(long, lat, group = group), fill = "#f7f7f7", color = "#969696") +
    coord_quickmap() +
    geom_point(x, mapping = aes(Long, Lat, color = br), size = 2) +
    scale_color_manual(name = "Chloride Concentration"~(mg~L^-1),
                       labels = c("0-1", "1-10", "10-100", "100-1000", ">1000"),
                       values = c("1" = "darkmagenta",
                                  "2" = "darkorchid3",
                                  "3" = "firebrick3",
                                  "4" = "darkorange2",
                                  "5" = "gold1")) +
    labs(caption = "Figure by Linnea Rock using data from waterqualitydata.us",
         title = years)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.text = element_text(size =9),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
}


statemap2 <- function(x, years) {
  ggplot() +
    geom_sf(counties, mapping = aes(), fill = "#f7f7f7", color = "#969696") +
    coord_sf() +
    geom_sf(x, mapping = aes(color = br), size = 1.5) +
    scale_color_manual(name = "Chloride Concentration"~(mg~L^-1),
                       labels = c("0-1", "1-10", "10-100", "100-1000", ">1000"),
                       values = c("1" = "darkmagenta",
                                  "2" = "darkorchid3",
                                  "3" = "firebrick3",
                                  "4" = "darkorange2",
                                  "5" = "gold1")) +
    #labs(#caption = "Figure by Linnea Rock using data from waterqualitydata.us",
         #title = years)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.text = element_text(size =9),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
}
