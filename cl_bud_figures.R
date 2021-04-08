#run files in cl_mass_load.R 
#fun files in Data/code/datasets_asFunction.R
options(scipen = 999)


#linear regressions on one graph
a <- join_for_linreg(labWIC, fieldcondWIC, WIC_cond_data) %>% mutate(ID = "WIC")
b <- join_for_linreg(labYS, fieldcondYS, YS_cond_data) %>% mutate(ID = "YS")
c <- join_for_linreg(labSW, fieldcondSW, SW_cond_data) %>% mutate(ID = "SW")
d <- join_for_linreg(labYI, fieldcondYI, YI_cond_data) %>% mutate(ID = "YI")
e <- join_for_linreg(labYN, fieldcondYN, YN_cond_data) %>% mutate(ID = "YN")
f <- join_for_linreg(lab6MC, fieldcond6MC, SMC_cond_data) %>% mutate(ID = "SMC")
g <- join_for_linreg(labDC, fieldcondDC, DC_cond_data) %>% mutate(ID = "DC")
h <- join_for_linreg(labPBMS, fieldcondPBMS, PBMS_cond_data) %>% mutate(ID = "PBMS")
i <- join_for_linreg(labPBSF, fieldcondPBSF, PBSF_cond_data) %>% mutate(ID = "PBSF")

all_tribs <- rbind(a, b, c, d, e, f, g, h, i)

ggplot(all_tribs, aes(sp.cond.x, chloride_mgL)) +
  geom_point(aes(color = ID)) + 
  scale_color_viridis_d(option = "inferno")  +
  geom_smooth(method = "lm", se = FALSE, aes(color = ID)) +
  #stat_cor() + 
  #stat_regline_equation() + 
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1),
       caption = "Figure X. Linear regressions of chloride vs. conductivity for all tributaries in the Upper Yahara River Watershed.") +
  L_theme() 
  

ggsave("Plots/cl_cond_linear_regression/all_tribs.png", height = 15, width = 20, units = "cm")


#linear regressions patched

library(patchwork)
library(cowplot)

total_blank <- theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
only_y <- theme(legend.position = "none", axis.title.x = element_blank())
only_x <- theme(legend.position = "none", axis.title.y = element_blank()) 
only_legend <- theme( axis.title.x = element_blank(), axis.title.y = element_blank())

patchwork_linreg <- ((YN_linreg_plot + total_blank) | (YI_linreg_plot + total_blank) | (YS_linreg_plot + total_blank)) /
  ((DC_linreg_plot + only_y) | (SMC_linreg_plot + total_blank) | (WIC_linreg_plot + only_legend)) /
  ((PBSF_linreg_plot + total_blank) | (PBMS_linreg_plot + only_x) | (SW_linreg_plot + total_blank))

patchwork_linreg + 
  plot_annotation(caption = "Figure X. Linear regression of chloride vs. conductivity for each tributary in the Upper Yahara River Watershed. Colors 
indicate whether the data were collected during the salting season (November-March) or the non-salting season (April-
October). Note the x and y axes are different for each regression.",
                  theme = theme(plot.tag = element_text(size = 8), 
                                plot.caption = element_text(size = 10, hjust = 0)))

ggsave("Plots/cl_cond_linear_regression/patch_tribs.png", height = 20, width = 20, units = "cm")
  

##average daily concentrations timeseries
#a <- WIC_daily_mass %>% mutate(ID = "WIC")
#b <- YS_daily_mass %>% mutate(ID = "YS")
#c <- SW_daily_mass %>% mutate(ID = "SW")
d <- YI_daily_mass %>% mutate(ID = "YI")
e <- YN_daily_mass %>% mutate(ID = "YN")
f <- SMC_daily_mass %>% mutate(ID = "SMC")
g <- DC_daily_mass %>% mutate(ID = "DC")
h <- PBMS_daily_mass %>% mutate(ID = "PBMS")
i <- PBSF_daily_mass %>% mutate(ID = "PBSF")

tribs_daily_ME_ag <- rbind(e,f,g)

g1 <- ggplot(tribs_daily_ME_ag) +
  geom_line(aes(date, daily_concentration_mgL, color = ID)) +
  L_theme() +
  scale_color_manual(labels = c("DC", "SMC", "YN"), values = c("#F24D29", "#E5C4A1", "#C4CFD0")) +
  labs(y = "",
       x = "",
       caption = "Timeseries of average daily chloride concentrations for Dorn Creek (DC), Sixmile Creek (SMC), and Yahara River 
North (YN). These are tributaries to Lake Mendota that have primarily agricultural (mean 79%) subwatersheds.")

#ggsave("Plots/chloride_loading/cl_conc_dcsmcyn.png", height = 15, width = 20, units = "cm")

#tribs_daily_ME_PBs <- rbind(h,i)

g2 <- ggplot(h) +
  geom_line(aes(date, daily_concentration_mgL), color = "#1DACE8") +
  L_theme() +
  labs(y = "Average Daily Chloride Concentration"~(mg~L^-1),
       x = "",
       caption = "Timeseries of average daily chloride concentrations for Pheasant Branch Main Stem (PBMS). The PBMS 
subwatershed is 39% developed and 56% agricultural.")

#ggsave("Plots/chloride_loading/cl_conc_PBMS.png", height = 15, width = 20, units = "cm")
datemin = min(h$date)
datemax = max(h$date)

g3 <- ggplot(i) +
  geom_line(aes(date, daily_concentration_mgL), color = "#1C366B") +
  L_theme() +
  labs(y = "",
       x = "",
       caption = "Timeseries of average daily chloride concentrations for Pheasant Branch South Fork (PBSF). The PBSF 
subwatershed is 71% developed.") +
  scale_x_date(limits = c(datemin, datemax))


plot_grid(g1, g2, g3, align = "v", ncol = 1)
ggsave("Plots/chloride_loading/cl_conc_MEtribs.png", height = 15, width = 20, units = "cm")
