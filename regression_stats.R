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
    "YN",
    "SMC",
    "DC",
    "PBMS",
    "PBSF",
    "YI",
    "WIC",
    "SW",
    "YS",
    "SH"
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
  P_value = c("<0.001", "<0.001", "<0.001","<0.001","<0.001","<0.001","<0.001","<0.001",0.13,"<0.001"
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
    title = "Chloride - Specific Conductivity Linear Regression Statistics") %>%
  tab_source_note(source_note = "Table 5."
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



#stats table for the lakes multiple linear regressions
#make the table
lake_stats <- data.frame(
  Lake = c("Lake Mendota", 'Lake Monona'),
  R = c(0.17, 0.64),
  P = c("<0.05", "<0.001")
)

gt_tbl <- gt(lake_stats)
simpleregtable <- gt_tbl %>%
  cols_label(
    Lake = "Lake Name",
    R = html("R<sup>2<sup>"),
    P = "P-Value"
  ) %>%
  tab_header(
    title = "Multiple Regression Statistics",
    subtitle = "Chloride concentration as a function of conductivity and depth") %>%
  tab_source_note(source_note = "Table 6."
  ); simpleregtable

# whitespace can be set, zoom sets resolution
gtsave(data = simpleregtable, "Plots/cl_cond_linear_regression/Lakestats_tbl.png", expand = 10, zoom = 10)




#table of some stats from sampling
grab_stats <- data.frame(
  water = c("YN",
            "SMC",
            "DC",
            "PBMS",
            "PBSF",
            "YI",
            "WIC",
            "SW",
            "YS",
            "ME",
            "MO"),
  median = c(median(labYN$chloride_mgL), 
             median(lab6MC$chloride_mgL), 
             median(labDC$chloride_mgL),
             median(labPBMS$chloride_mgL),
             median(labPBSF$chloride_mgL),
             median(labYI$chloride_mgL),
             median(labWIC$chloride_mgL),
             median(labSW$chloride_mgL),
             median(labYS$chloride_mgL),
             median(labME$chloride_mgL),
             median(labMO$chloride_mgL)),
  mean = c(mean(labYN$chloride_mgL), 
          mean(lab6MC$chloride_mgL), 
          mean(labDC$chloride_mgL),
          mean(labPBMS$chloride_mgL),
          mean(labPBSF$chloride_mgL),
          mean(labYI$chloride_mgL),
          mean(labWIC$chloride_mgL),
          mean(labSW$chloride_mgL),
          mean(labYS$chloride_mgL),
          mean(labME$chloride_mgL),
          mean(labMO$chloride_mgL)),
  max = c(max(labYN$chloride_mgL), 
          max(lab6MC$chloride_mgL), 
          max(labDC$chloride_mgL),
          max(labPBMS$chloride_mgL),
          max(labPBSF$chloride_mgL),
          max(labYI$chloride_mgL),
          max(labWIC$chloride_mgL),
          max(labSW$chloride_mgL),
          max(labYS$chloride_mgL),
          max(labME$chloride_mgL),
          max(labMO$chloride_mgL)),
  min = c(min(labYN$chloride_mgL), 
          min(lab6MC$chloride_mgL), 
          min(labDC$chloride_mgL),
          min(labPBMS$chloride_mgL),
          min(labPBSF$chloride_mgL),
          min(labYI$chloride_mgL),
          min(labWIC$chloride_mgL),
          min(labSW$chloride_mgL),
          min(labYS$chloride_mgL),
          min(labME$chloride_mgL),
          min(labMO$chloride_mgL)),
  sd = c(sd(labYN$chloride_mgL), 
          sd(lab6MC$chloride_mgL), 
          sd(labDC$chloride_mgL),
          sd(labPBMS$chloride_mgL),
          sd(labPBSF$chloride_mgL),
          sd(labYI$chloride_mgL),
          sd(labWIC$chloride_mgL),
          sd(labSW$chloride_mgL),
          sd(labYS$chloride_mgL),
          sd(labME$chloride_mgL),
          sd(labMO$chloride_mgL)),
  n = c(nrow(labYN), 
         nrow(lab6MC), 
         nrow(labDC),
         nrow(labPBMS),
         nrow(labPBSF),
         nrow(labYI),
         nrow(labWIC),
         nrow(labSW),
         nrow(labYS),
         nrow(labME),
         nrow(labMO))
)

gt_tbl <- gt(grab_stats)
simpleregtable <- gt_tbl %>%
  cols_label(
    water = "Waterbody Name",
    median = "Median",
    mean = "Mean",
    max = "Maximum",
    min = "Minimum",
    sd = "Standard Deviation",
    n = "Number of Observations"
  ) %>%
  tab_header(
    title = "Statistics from Collected Chloride Concentrations",
    subtitle = "Data from grab sampling. Chloride concentration in units: mg/L") %>%
  tab_source_note(source_note = "Table 4."
  ); simpleregtable

# whitespace can be set, zoom sets resolution
gtsave(data = simpleregtable, "Plots/chloridestats_tbl.png", expand = 10, zoom = 10)





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


