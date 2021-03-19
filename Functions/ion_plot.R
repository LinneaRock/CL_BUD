#Functions for plotting ions (LTER)
#df = dataframe
#X = ion
#axislabel = "ion concentration"
ion <- function(df, X, axislabel) {
  ggplot(df, aes(date, X)) +
    geom_point(aes(color = lakeid)) +
  labs(x = "",
       y = axislabel) +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =10),
          axis.title = element_text(size =10),
          legend.title = element_blank(),
          legend.text = element_text(size = 9),
          legend.position = "top")
}



#making custom plot for proposal - plotting cl:
ion_cl <- function(x, axislabel) {
  ggplot(x %>% filter(item == "cl"), aes(sampledate, value)) +
    geom_point(aes(color = lakeid), size = 0.5) +
    geom_smooth(aes(group = lakeid, color = lakeid), se = FALSE, size = 1) +
    labs(x = "", y = axislabel)+
    ylim(0, 175) +
    theme(legend.title = element_blank(), legend.position = "top",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          legend.text = element_text(size =9)) +
    scale_color_manual(labels = c("Mendota", "Monona"),
                       values = c("#1C366B", "#F24D29")) 
}

#making custom plot for proposal - plotting ions besides cl:
ion_other <- function(x, y, axislabel) {
  ggplot(x %>% filter(item == y), aes(sampledate, value)) +
    geom_point(aes(color = lakeid), size = 0.5) +
    geom_smooth(aes(group = lakeid, color = lakeid), se = FALSE, size = 1) +
    labs(x = "", 
         y = axislabel)+
    theme(
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          legend.position = "none") +
    scale_color_manual(labels = c("Mendota", "Monona"),
                       values = c("#1C366B", "#F24D29")) 
}

