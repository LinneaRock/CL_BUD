library(tidyverse)
library(lubridate)
library(ggpubr)
library(patchwork)
library(broom)
library(data.table)
source("Functions/qsc.R")
source("Functions/qcl.R")
source("Functions/splot.R")
source("Functions/L_theme.R")


#Discharge - conductivity plots

q.sc(loggerYN, d.YN)+
  captqec('Yahara River @ 113',"Yahara River at Highway 113", loggerYN, d.YN)
splot("QC_plots/", "YN_cond")

q.sc(loggerYI, d.YI)+
  captqec('Yahara River @ Main St.',"Yahara River at E. Main St", loggerYI, d.YI)
splot("QC_plots/", "YI_cond")

q.sc(logger6MC, d.6MC)+
  captqec('Sixmile Creek @ M',"Sixmile Creek at Highway M", logger6MC, d.6MC)
splot("QC_plots/", "6MC_cond")

q.sc(loggerDC, d.DC)+
  captqec('Dorn Creek @ M',"Dorn Creek at Highway M", loggerDC, d.DC)
splot("QC_plots/", "DC_cond")

q.sc(loggerPBMS, d.PBMS)+
  captqec('Pheasant Branch Main Stem',"Main Stem of Pheasant Branch Creek", loggerPBMS, d.PBMS)
splot("QC_plots/", "PBMS_cond")

q.sc(loggerPBSF, d.PBSF)+
  captqec('Pheasant Branch S.Fork', "South Fork of Pheasant Branch", loggerPBSF, d.PBSF)
splot("QC_plots/", "PBSF_cond")

infoqec(loggerPBSF, d.PBSF)


#Discharge - chloride plots

q.cl(labYN, d.YN) +
  captqc('Yahara River @ 113',"Yahara River at Highway 113", labYN, d.YN)
splot("QC_plots/", "YN_cl")

q.cl(labYI, d.YI) +
  captqc('Yahara River @ Main St.',"Yahara River at E. Main St", labYI, d.YI)
splot("QC_plots/", "YI_cl")

q.cl(lab6MC, d.6MC)+
  captqc('Sixmile Creek @ M',"Sixmile Creek at Highway M", lab6MC, d.6MC)
splot("QC_plots/", "6MC_cl")

q.cl(labDC, d.DC) +
  captqc('Dorn Creek @ M',"Dorn Creek at Highway M", labDC, d.DC)
splot("QC_plots/", "DC_cl")

q.cl(labPBMS, d.PBMS) +
  captqc('Pheasant Branch Main Stem',"Main Stem of Pheasant Branch Creek", labPBMS, d.PBMS)
splot("QC_plots/", "PBMS_cl")

q.cl(labPBSF, d.PBSF) +
  captqc('Pheasant Branch S.Fork', "South Fork of Pheasant Branch", labPBSF, d.PBSF)
splot("QC_plots/", "PBSF_cl")





