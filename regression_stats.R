library(gt)
library(tidyverse)
library(webshot)
source("Functions/linreg.R")

#functions for each of the variables in the table
slope <- function(choride_data, field_cond, logger) {
  round(coef(info(choride_data, field_cond, logger))[2,1], 2)
}

intercept <- function(choride_data, field_cond, logger) {
  round(coef(info(choride_data, field_cond, logger))[1,1], 2)
}

r.sqr.lm <- function(choride_data, field_cond, logger) {
  #round((info(cl, cond)$adj.r.squared), 2)
  round((info(choride_data, field_cond, logger)$r.squared), 2)
}

pvalue <- function(choride_data, field_cond, logger) {
 coef(info(choride_data, field_cond, logger))[2,4]
}

SH.lm  =  lm(chloride_mgL ~ runningmean, labSH1)
SH.slope = round(coef(SH.lm)[2], 2)
SH.intercept = round(coef(SH.lm)[1], 2)
SH.rsq = round(summary(SH.lm)$r.squared, 2)
SH.p =  coef(summary(SH.lm))[2,4]

# Make the table
River_stats <- data.frame(
  River = c(
    "Yahara River North",
    "Sixmile Creek",
    "Dorn Creek",
    "Pheasant Branch - Main Stem",
    "Pheasant Branch - South Fork",
    "Yahara River Isthmus",
    "Wingra Creek",
    "Starkweather Creek",
    "Yahara River South",
    "Spring Harbor Storm Sewer"
  ),
  Slope = c(
    slope(labYN, fieldcondYN, YN_cond_data),
    slope(lab6MC, fieldcond6MC, SMC_cond_data),
    slope(labDC, fieldcondDC, DC_cond_data),
    slope(labPBMS, fieldcondPBMS, PBMS_cond_data),
    slope(labPBSF, fieldcondPBSF, PBSF_cond_data),
    slope(labYI, fieldcondYI, YI_cond_data),
    slope(labWIC, fieldcondWIC, WIC_cond_data),
    slope(labSW, fieldcondSW, SW_cond_data),
    slope(labYS, fieldcondYS, YS_cond_data),
    SH.slope
  ),
  Intercept = c(
    intercept(labYN, fieldcondYN, YN_cond_data),
    intercept(lab6MC, fieldcond6MC, SMC_cond_data),
    intercept(labDC, fieldcondDC, DC_cond_data),
    intercept(labPBMS, fieldcondPBMS, PBMS_cond_data),
    intercept(labPBSF, fieldcondPBSF, PBSF_cond_data),
    intercept(labYI, fieldcondYI, YI_cond_data),
    intercept(labWIC, fieldcondWIC, WIC_cond_data),
    intercept(labSW, fieldcondSW, SW_cond_data),
    intercept(labYS, fieldcondYS, YS_cond_data),
    SH.intercept
  ),
  Adjusted_R2 = c(
    r.sqr.lm(labYN, fieldcondYN, YN_cond_data),
    r.sqr.lm(lab6MC, fieldcond6MC, SMC_cond_data),
    r.sqr.lm(labDC, fieldcondDC, DC_cond_data),
    r.sqr.lm(labPBMS, fieldcondPBMS, PBMS_cond_data),
    r.sqr.lm(labPBSF, fieldcondPBSF, PBSF_cond_data),
    r.sqr.lm(labYI, fieldcondYI, YI_cond_data),
    r.sqr.lm(labWIC, fieldcondWIC, WIC_cond_data),
    r.sqr.lm(labSW, fieldcondSW, SW_cond_data),
    r.sqr.lm(labYS, fieldcondYS, YS_cond_data),
    SH.rsq
  ),
  P_value = c(">0.01", ">0.01", ">0.01",">0.01",">0.01",">0.01",">0.01",">0.01",0.1,">0.01"
    # pvalue(labYN, fieldcondYN, YN_cond_data),
    # pvalue(lab6MC, fieldcond6MC, SMC_cond_data),
    # pvalue(labDC, fieldcondDC, DC_cond_data),
    # pvalue(labPBMS, fieldcondPBMS, PBMS_cond_data),
    # pvalue(labPBSF, fieldcondPBSF, PBSF_cond_data),
    # pvalue(labYI, fieldcondYI, YI_cond_data),
    # pvalue(labWIC, fieldcondWIC, WIC_cond_data),
    # pvalue(labSW, fieldcondSW, SW_cond_data),
    # pvalue(labYS, fieldcondYS, YS_cond_data),
    # SH.p
  )
)


gt_tbl <- gt(River_stats)
simpleregtable <- gt_tbl %>%
  cols_label(
    River = "River Name",
    Slope = "Slope",
    Intercept = "Intercept",
    Adjusted_R2 = html("R<sup>2<sup>"),
    P_value = "P-Value"
  ) %>%
  tab_header(
    title = "Chloride - Specific Conductivity Linear Regression Statistics",
  ); simpleregtable

# whitespace can be set, zoom sets resolution
gtsave(data = simpleregtable, "Plots/cl_cond_linear_regression/stats_tbl.png", expand = 10, zoom = 10)







info(labYN, fieldcondYN, YN_cond_data)
info(lab6MC, fieldcond6MC, SMC_cond_data)
info(labDC, fieldcondDC, DC_cond_data)
info(labPBMS, fieldcondPBMS, PBMS_cond_data)
info(labPBSF, fieldcondPBSF, PBSF_cond_data)
info(labYI, fieldcondYI, YI_cond_data)
info(labWIC, fieldcondWIC, WIC_cond_data)
info(labSW, fieldcondSW, SW_cond_data)
info(labYS, fieldcondYS, YS_cond_data)









#stats for segmented linear regressions 
#functions for each of the variables in the table
# 
# 
# intercept_up <- function(cl, cond) {
#   joined <- join_datasets_chloride(cl, cond)
#   orig.lm <- lm(chloride_mgL ~ runningmean, joined)
#   seg.lm <- segmented(orig.lm, seg.Z = ~runningmean)
#   upper <- lm(chloride_mgL ~ runningmean,joined %>% filter(runningmean >= seg.lm$psi[2]))
#   round(coef(summary(upper))[1,1], 2)
# }
# 
# intercept_low <- function(cl, cond) {
#   joined <- join_datasets_chloride(cl, cond)
#   orig.lm <- lm(chloride_mgL ~ runningmean, joined)
#   seg.lm <- segmented(orig.lm, seg.Z = ~runningmean)
#   lower <- lm(chloride_mgL ~ runningmean,joined %>% filter(runningmean < seg.lm$psi[2]))
#   round(coef(summary(lower))[1,1], 2)
# }
# 
# slope_up <- function(cl, cond) {
#   joined <- join_datasets_chloride(cl, cond)
#   orig.lm <- lm(chloride_mgL ~ runningmean, joined)
#   seg.lm <- segmented(orig.lm, seg.Z = ~runningmean)
#   upper <- lm(chloride_mgL ~ runningmean,joined %>% filter(runningmean >= seg.lm$psi[2]))
#   round(coef(summary(upper))[2,1], 2)
# }
# 
# slope_low <- function(cl, cond) {
#   joined <- join_datasets_chloride(cl, cond)
#   orig.lm <- lm(chloride_mgL ~ runningmean, joined)
#   seg.lm <- segmented(orig.lm, seg.Z = ~runningmean)
#   lower <- lm(chloride_mgL ~ runningmean,joined %>% filter(runningmean < seg.lm$psi[2]))
#   round(coef(summary(lower))[2,1], 2)
# }
# 
# r.sqr <- function(cl, cond) {
#   joined <- join_datasets_chloride(cl, cond)
#   orig.lm <- lm(chloride_mgL ~ runningmean, joined)
#   seg.lm <- segmented(orig.lm, seg.Z = ~runningmean)
#   round((summary(seg.lm)$r.squared), 2)
# }
# 
# 
# 
# breakpoint <- function(cl, cond) {
#   joined <- join_datasets_chloride(cl, cond)
#   orig.lm <- lm(chloride_mgL ~ runningmean, joined)
#   seg.lm <- segmented(orig.lm, seg.Z = ~runningmean)
#   round(seg.lm$psi[2], 2)
# }
# 
# 
# 
# # Make the table
# River_stats <- data.frame(
#   River = c("Yahara River North", "", "Sixmile Creek", "", "Dorn Creek", "", "Pheasant Branch - Main Stem", "", "Pheasant Branch - South Fork", "", "Yahara River Isthmus", "", "Wingra Creek", "", "Starkweather Creek", "", "Yahara River South", ""),
#   
#   Breakpoint = c(paste(">=", breakpoint(labYN, YN_cond_data)), paste("<", breakpoint(labYN, YN_cond_data)), paste(">=", breakpoint(lab6MC, SMC_cond_data)), paste("<", breakpoint(lab6MC, SMC_cond_data)), paste(">=", breakpoint(labDC, DC_cond_data)), paste("<", breakpoint(labDC, DC_cond_data)), paste(">=", breakpoint(labPBMS, PBMS_cond_data)), paste("<", breakpoint(labPBMS, PBMS_cond_data)), paste(">=", breakpoint(labPBSF, PBSF_cond_data)), paste("<", breakpoint(labPBSF, PBSF_cond_data)), paste(">=", breakpoint(labYI, YI_cond_data)), paste("<", breakpoint(labYI, YI_cond_data)), paste(">=", breakpoint(labWIC, WIC_cond_data)), paste("<", breakpoint(labWIC, WIC_cond_data)), paste(">=", breakpoint(labSW, SW_cond_data)), paste("<", breakpoint(labSW, SW_cond_data)), paste(">=", breakpoint(labYS, YS_cond_data)), paste("<", breakpoint(labYS, YS_cond_data))),
#   
#   Slope = c(slope_up(labYN, YN_cond_data), slope_low(labYN, YN_cond_data), slope_up(lab6MC, SMC_cond_data), slope_low(lab6MC, SMC_cond_data), slope_up(labDC, DC_cond_data), slope_low(labDC, DC_cond_data), slope_up(labPBMS, PBMS_cond_data), slope_low(labPBMS, PBMS_cond_data), slope_up(labPBSF, PBSF_cond_data), slope_low(labPBSF, PBSF_cond_data), slope_up(labYI, YI_cond_data), slope_low(labYI, YI_cond_data), slope_up(labWIC, WIC_cond_data), slope_low(labWIC, WIC_cond_data), slope_up(labSW, SW_cond_data), slope_low(labSW, SW_cond_data), slope_up(labYS, YS_cond_data), slope_low(labYS, YS_cond_data)),
#   
#   Intercept = c(intercept_up(labYN, YN_cond_data), intercept_low(labYN, YN_cond_data), intercept_up(lab6MC, SMC_cond_data), intercept_low(lab6MC, SMC_cond_data), intercept_up(labDC, DC_cond_data), intercept_low(labDC, DC_cond_data), intercept_up(labPBMS, PBMS_cond_data), intercept_low(labPBMS, PBMS_cond_data), intercept_up(labPBSF, PBSF_cond_data), intercept_low(labPBSF, PBSF_cond_data), intercept_up(labYI, YI_cond_data), intercept_low(labYI, YI_cond_data), intercept_up(labWIC, WIC_cond_data), intercept_low(labWIC, WIC_cond_data), intercept_up(labSW, SW_cond_data), intercept_low(labSW, SW_cond_data), intercept_up(labYS, YS_cond_data), intercept_low(labYS, YS_cond_data)),
#   
#   R2 = c(r.sqr(labYN, YN_cond_data), "", r.sqr(lab6MC, SMC_cond_data), "", r.sqr(labDC, DC_cond_data), "", r.sqr(labPBMS, PBMS_cond_data), "", r.sqr(labPBSF, PBSF_cond_data), "", r.sqr(labYI, YI_cond_data), "", r.sqr(labWIC, WIC_cond_data), "", r.sqr(labSW, SW_cond_data), "", r.sqr(labYS, YS_cond_data), ""))
#   
# 
# gt_tbl <- gt(River_stats)
# segmented_tbl <- gt_tbl %>%
#   cols_label(
#     River = "River Name",
#     Breakpoint = "Breakpoint",
#     Slope = "Slope",
#     Intercept = "Intercept",
#     R2 = html("R<sup>2<sup>")
#     #P_value = "P-Value"
#   ) %>%
#   tab_header(
#     title = "Chloride - Specific Conductivity Linear Regression Statistics",
#   ); segmented_tbl
# 
# 


