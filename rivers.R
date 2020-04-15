library(tidyverse)
library(lubridate)
library(ggpubr)


#This function joins the conductivity and chloride datasets together then plots a linear model of specific conductivity vs. chloride


cl.cond.plot <- function(df1, df2) {
  d <- df1 %>%
    left_join(df2, by = "date")
  
  ggplot(d, aes(chloride_mgL, sp.cond)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    #stat_cor() + 
    #stat_regline_equation() + 
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


#Function to plot time series of specific conductance 
cond <- function(df) {
  ggplot(df, aes(date, sp.cond)) +
    geom_line() +
    labs(y = "Specific Conductivity (uS/cm) @ 25 deg C\n", 
         x = "\nDate") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =13, face = "bold"),
          axis.title = element_text(size =13, face = "bold"))
}


cond(loggerYN)
cond(loggerYI)
cond(YS)

YS <- loggerYS %>%
  filter(date < "2020-01-29 00:00:00")

#function to plot a time series of conductivity with chloride points overlain     
sccl <- function(logger, lab) {

par(mar = c(5,5,5,5)) 

plot(lab$datetime_collected, lab$chloride_mgL,
     col = "#D1362F", pch = 19,
     ylab = "",
     xlab = "",
     yaxt = "n")

axis(2, col = "#D1362F", col.axis = "#D1362F")

mtext("Chloride Concentration (mg/L)", side = 2, line = 3, col = "#D1362F")

par(new = TRUE)

plot(logger$date, logger$sp.cond,
     col="#27223C",type="l",xaxt="n",yaxt="n",xlab="",ylab="",axes=FALSE)

axis(4, col = "#27223C", col.axis = "#27223C")

mtext("Specific Conductivity (µS/cm) @ 25°C)", side = 4, line = 3, col = "#27223C")
}

sccl(loggerYN, labYN)
sccl(loggerYI, labYI)
sccl(loggerYS, labYS) 
sccl(loggerSW, labSW)
sccl(logger6MC, lab6MC)
sccl(loggerDC, labDC)
sccl(loggerPBMS, labPBMS)
sccl(loggerPBSF, labPBSF)


