library(tidyverse)
library(lubridate)
library(ggpubr)


##This function joins the conductivity and discharge datasets together then plots specific conductivity vs. discharge

q.sc <- function(dfx, dfy) {
  qsc <- dfx %>%
    left_join(dfy, by = "date")
  
  ggplot(qsc, aes(discharge, sp.cond)) +
    geom_point() +
    #geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = "Specific Conductivity (uS/cm) @ 25 deg C\n", 
         x = "\nDischarge (m^3/S)") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =13, face = "bold"),
          axis.title = element_text(size =13, face = "bold"))  
}

q.sc(loggerYN, d.YN)
q.sc(loggerYI, d.YI)
q.sc(logger6MC, d.6MC)
q.sc(loggerDC, d.DC)
q.sc(loggerPBMS, d.PBMS)
q.sc(loggerPBSF, d.PBSF)
