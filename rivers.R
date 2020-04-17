library(tidyverse)
library(lubridate)
library(ggpubr)


#This function joins the conductivity and chloride datasets together then plots a linear model of specific conductivity vs. chloride


cl.cond.plot <- function(df1, df2, X) {
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
  
  ggsave(filename = paste("Plots/cl_cond_linear_regression/", X, ".png", sep = ""))
}


cl.cond.plot(loggerYN, labYN, "YN")
cl.cond.plot(loggerYI, labYI, "YI")
cl.cond.plot(loggerYS, labYS, "YS")
cl.cond.plot(loggerSW, labSW, "SW")
cl.cond.plot(logger6MC, lab6MC, "6MC")
cl.cond.plot(loggerDC, labDC, "DC")
cl.cond.plot(loggerPBMS, labPBMS, "PBMS")


#Needed to round time because the logger was collecting at H:15 and H:45 for a few weeks rather than at H:00 and H:30 and I am having trouble finding a better solution
a <- loggerPBSF %>%
mutate(date = round_date(date, "30 minutes")) 

cl.cond.plot(a, labPBSF, "PBSF")


#Function to plot time series of specific conductance 
cond <- function(df, X) {
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
  
  ggsave(filename = paste("Plots/conductance_time_series/", X, ".png", sep = ""))
}


cond(loggerYN, "YN")
cond(loggerYI, "YI")
cond(loggerYS, "YS")
cond(loggerSW, "SW")
cond(logger6MC, "6MC")
cond(loggerDC, "DC")
cond(loggerPBMS, "PBMS")
cond(loggerPBSF, "PBSF")


#Function to plot time series of chloride
cl <- function(df, X) {
  ggplot(df, aes(date, chloride_mgL)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  labs(y = "Chloride Concentration (mg/L)\n", 
       x = "\nDate") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =13, face = "bold"),
          axis.title = element_text(size =13, face = "bold"))
  
  ggsave(filename = paste("Plots/chloride_time_series/", X, ".png", sep = ""))
}

cl(labYN, "YN")
cl(labYI, "YI")
cl(labYS, "YS")
cl(labSW, "SW")
cl(lab6MC, "6MC")
cl(labDC, "DC")
cl(labPBMS, "PBMS")
cl(labPBSF, "PBSF")


#function to plot a time series of conductivity with chloride points overlain
#I don't know  if this will be useful
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


