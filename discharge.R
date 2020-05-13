library(tidyverse)
library(lubridate)
library(ggpubr)
source("Functions/qsc.R")
source("Functions/qcl.R")
source("Functions/splot.R")


#Discharge - conductivity plots

q.sc(loggerYN, d.YN)
splot("QC_plots/", "YN_cond")
q.sc(loggerYI, d.YI)
splot("QC_plots/", "YI_cond")
q.sc(logger6MC, d.6MC)
splot("QC_plots/", "6MC_cond")
q.sc(loggerDC, d.DC)
splot("QC_plots/", "DC_cond")
q.sc(loggerPBMS, d.PBMS)
splot("QC_plots/", "PBMS_cond")
q.sc(loggerPBSF, d.PBSF)
splot("QC_plots/", "PBSF_cond")


#Discharge - chloride plots

q.cl(labYN, d.YN)
splot("QC_plots/", "YN_cl")
q.cl(labYI, d.YI)
splot("QC_plots/", "YI_cl")
q.cl(lab6MC, d.6MC)
splot("QC_plots/", "6MC_cl")
q.cl(labDC, d.DC)
splot("QC_plots/", "DC_cl")
q.cl(labPBMS, d.PBMS)
splot("QC_plots/", "PBMS_cl")
q.cl(labPBSF, d.PBSF)
splot("QC_plots/", "PBSF_cl")

