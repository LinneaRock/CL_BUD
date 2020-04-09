library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)


#This function joins the conductivity and chloride datasets together then plots a linear model of specific conductivity vs. chloride


cl.cond.plot <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  ggplot(d, aes(chloride_mgL, sp.cond)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    #stat_cor(label.x = 64.75, label.y = 200) +
    #stat_regline_equation(label.x = 64.75, label.y = 250) + #simple way to get r^2 value?  
    labs(y = "Specific Conductivity (uS/cm) @ 25 deg C\n", 
         x = "\nChloride Concentration (mg/L)") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =13, face = "bold"),
          axis.title = element_text(size =13, face = "bold"))
}


cl.cond.plot(loggerYN, labYN)
cl.cond.plot(loggerYI, labYI)
cl.cond.plot(loggerYS, labYS)
cl.cond.plot(loggerSW, labSW)
cl.cond.plot(logger6MC, lab6MC)
cl.cond.plot(loggerDC, labDC)
cl.cond.plot(loggerPBMS, labPBMS)


#Needed to round time because the logger was collecting at H:15 and H:45 for a few weeks rather than at H:00 and H:30 and I am having trouble finding a better solution
a <- loggerPBSF %>%
mutate(date = round_date(date, "30 minutes")) 

cl.cond.plot(a, labPBSF)
       