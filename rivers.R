library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)




#YS############################

#combining chloride and conductivity into single dataframe
d.1 <- loggerYS %>%
  select(date, sp.cond) 

d.2 <- labYS %>%
  select(date, chloride_mgL) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Yahara River - South") +
  #stat_cor(label.x = 64.75, label.y = 200) +
  #stat_regline_equation(label.x = 64.75, label.y = 250) + #simple way to get r^2 value?  
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))



#SW###########################


#combining chloride and conductivity into single dataframe
d.1 <- loggerSW %>%
  select(date, sp.cond) 

d.2 <- labSW %>%
  select(date, chloride_mgL)

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Starkweather Creek") +
  #stat_cor(label.x = 50, label.y = 3500) +
  #stat_regline_equation(label.x = 50, label.y = 4000) + #how do I get r^2 value? 
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))


#YI#############################

#combining chloride and conductivity into single dataframe
d.1 <- loggerYI %>%
  select(date, sp.cond) 

d.2 <- labYI %>%
  select(date, chloride_mgL) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Yahara River - Isthmus") +
  #stat_cor(label.x = 52, label.y = 50) +
  #stat_regline_equation(label.x = 52, label.y = 100) + #how do I get r^2 value? 
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))


#YN############################

#combining chloride and conductivity into single dataframe
d.1 <- loggerYN %>%
  select(date, sp.cond) 

d.2 <- labYN %>%
  select(date, chloride_mgL) %>%

d.3 <- left_join(d.1, d.2, by = "date")


#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Yahara River - North") +
  #stat_cor(label.x = 35, label.y = 450) +
  #stat_regline_equation(label.x = 35, label.y = 475) + #how do I get r^2 value? 
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))


#6MC##################################

#combining chloride and conductivity into single dataframe
d.1 <- logger6MC %>%
  select(date, sp.cond) 

d.2 <- lab6MC %>%
  select(date, chloride_mgL) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Sixmile Creek") +
  #stat_cor(label.x = 37.5, label.y = 600) +
  #stat_regline_equation(label.x = 37.5, label.y = 650) + #how do I get r^2 value?
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))

#DC###############################

#combining chloride and conductivity into single dataframe
d.1 <- loggerDC %>%
  select(date, sp.cond) 

d.2 <- labDC %>%
  select(date, chloride_mgL) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Dorn Creek") +
  #stat_cor(label.x = 36, label.y = 350) +
  #stat_regline_equation(label.x = 36, label.y = 400) + #how do I get r^2 value?
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))

#PBMS###########################

#combining chloride and conductivity into single dataframe
d.1 <- loggerPBMS %>%
  select(date, sp.cond) 

d.2 <- labPBMS %>%
  select(date, chloride_mgL) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Pheasant Branch Main Stem") +
  #stat_cor(label.x = 117.5, label.y = 750) +
  #stat_regline_equation(label.x = 117.5, label.y = 825) + #how do I get r^2 value?
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))

#PBSF#########################

loggerPBSF$date <- round_date(loggerPBSF$date, "30 minutes") #Needed to round time because the logger was collecting at H:15 and H:45 for a few weeks rather than at H:00 and H:30.


#combining chloride and conductivity into single dataframe
d.1 <- loggerPBSF %>%
  select(date, sp.cond) 

d.2 <- labPBSF %>%
  select(date, chloride_mgL) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity (uS/cm) @ 25 deg C", x = "Chloride (mg/L)", title = "Pheasant Branch South Fork") +
  #stat_cor(label.x = 250, label.y = 2500) +
  #stat_regline_equation(label.x = 250, label.y = 3000) + #how do I get r^2 value?
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))
