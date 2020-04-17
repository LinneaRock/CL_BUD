library(tidyverse)
library(lubridate)
library(ggpubr)


##This function joins the conductivity and discharge datasets together then plots specific conductivity vs. discharge

q.sc <- function(dfx, dfy, X) {
  qsc <- dfx %>%
    left_join(dfy, by = "date")
  
  ggplot(qsc, aes(discharge, sp.cond)) +
    geom_point() +
    #stat_cor() + 
    #stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
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
  
  ggsave(filename = paste("Plots/QC_plots/", X, ".png", sep = ""))
}

q.sc(loggerYN, d.YN, "YN_cond")
q.sc(loggerYI, d.YI, "YI_cond")
q.sc(logger6MC, d.6MC, "6MC_cond")
q.sc(loggerDC, d.DC, "DC_cond")
q.sc(loggerPBMS, d.PBMS, "PBMS_cond")
q.sc(loggerPBSF, d.PBSF, "PBSF_cond")


##This function joins the chloride and discharge datasets together then plots specific conductivity vs. discharge

q.cl <- function(dfx, dfy, X) {
  qsc <- dfx %>%
    left_join(dfy, by = "date")
  
  ggplot(qsc, aes(discharge, chloride_mgL)) +
    geom_point() +
    stat_cor() + 
    stat_regline_equation() +
    geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
    labs(y = "Chloride Concentration (mg/L)\n", 
         x = "\nDischarge (m^3/S)") +
    theme(panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray88"),
          axis.text = element_text(size =13, face = "bold"),
          axis.title = element_text(size =13, face = "bold"))  
  
  ggsave(filename = paste("Plots/QC_plots/", X, ".png", sep = ""))
}

q.cl(labYN, d.YN, "YN_cl")
q.cl(labYI, d.YI, "YI_cl")
q.cl(lab6MC, d.6MC, "6MC_cl")
q.cl(labDC, d.DC, "DC_cl")
q.cl(labPBMS, d.PBMS, "PBMS_cl")
q.cl(labPBSF, d.PBSF, "PBSF_cl")
