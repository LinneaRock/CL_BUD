library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)


## Plotting Conductivity vs. Discharge

##YI
PBSF_QC <- loggerPBSF %>%
  select(date, sp.cond) %>%
  left_join(d.PBSF, by = "date") #%>%

ggplot(PBSF_QC, aes(discharge, sp.cond)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
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


##PBSF
#combining discharge and specific conductance data by collection date/time
PBSF_QC <- loggerPBSF %>%
  select(date, sp.cond) %>%
  left_join(d.PBSF, by = "date") #%>%

ggplot(PBSF_QC, aes(discharge, sp.cond)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
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


