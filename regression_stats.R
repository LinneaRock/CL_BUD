library(gt)
library(tidyverse)
library(webshot)
source("Functions/linreg.R")

#functions for each of the variables in the table
slope <- function(cl, cond) {
  round(coef(info(cl, cond))[2,1], 2)
}

intercept <- function(cl, cond) {
  round(coef(info(cl, cond))[1,1], 2)
}

r.sqr <- function(cl, cond) {
  round((info(cl, cond)$adj.r.squared), 2)
}

pvalue <- function(cl, cond) {
 coef(info(cl, cond))[2,4]
}

# Make the table
River_stats <- data.frame(
  River = c("Yahara River North", "Sixmile Creek", "Dorn Creek", "Pheasant Branch - Main Stem", "Pheasant Branch - South Fork", "Yahara River Isthmus", "Wingra Creek", "Starkweather Creek", "Yahara River South"),
  Slope = c(slope(labYN, YN_cond_data), slope(lab6MC, SMC_cond_data), slope(labDC, DC_cond_data), slope(labPBMS, PBMS_cond_data), slope(labPBSF, PBSF_cond_data), slope(labYI, YI_cond_data), slope(labWIC, WIC_cond_data), slope(labSW, SW_cond_data), slope(labYS, YS_cond_data)),
  Intercept = c(intercept(labYN, YN_cond_data), intercept(lab6MC, SMC_cond_data), intercept(labDC, DC_cond_data), intercept(labPBMS, PBMS_cond_data), intercept(labPBSF, PBSF_cond_data), intercept(labYI, YI_cond_data), intercept(labWIC, WIC_cond_data), intercept(labSW, SW_cond_data), intercept(labYS, YS_cond_data)),
  Adjusted_R2 = c(r.sqr(labYN, YN_cond_data), r.sqr(lab6MC, SMC_cond_data), r.sqr(labDC, DC_cond_data), r.sqr(labPBMS, PBMS_cond_data), r.sqr(labPBSF, PBSF_cond_data), r.sqr(labYI, YI_cond_data), r.sqr(labWIC, WIC_cond_data), r.sqr(labSW, SW_cond_data), r.sqr(labYS, YS_cond_data)),
  P_value = c(pvalue(labYN, YN_cond_data), pvalue(lab6MC, SMC_cond_data), pvalue(labDC, DC_cond_data), pvalue(labPBMS, PBMS_cond_data), pvalue(labPBSF, PBSF_cond_data), pvalue(labYI, YI_cond_data), pvalue(labWIC, WIC_cond_data), pvalue(labSW, SW_cond_data), pvalue(labYS, YS_cond_data))
)


gt_tbl <- gt(River_stats)
t1 <- gt_tbl %>%
  cols_label(
    River = "River Name",
    Slope = "Slope",
    Intercept = "Intercept",
    Adjusted_R2 = html("R<sup>2<sup>"),
    P_value = "P-Value"
  ) %>%
  tab_header(
    title = "Chloride - Specific Conductivity Linear Regression Statistics",
  ); t1

# whitespace can be set, zoom sets resolution
gtsave(data = t1, "Plots/cl_cond_linear_regression/stats_tbl.png", expand = 10, zoom = 10)
