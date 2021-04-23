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
  #scale_color_manual(values = wes_palette("Darjeeling1", 10, "continuous")) +
  #stat_cor() + 
  #stat_regline_equation() + 
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1),
       caption = "Figure X. Linear regressions of chloride vs. conductivity for all tributaries in the Upper Yahara River Watershed.") +
  L_theme() 
  
#ggsave("Plots/figsforpres/all_tribs.png", height = 15, width = 20, units = "cm")
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
  


##chloride source in subwatersheds -- ratioooooo
source_by_ws <- cl_roads_by_subwatershed %>% #dataset from subwatersheds_road_salting.R
  filter(watershed != "WC") %>%
  left_join(usgs_ws_info_roads_aggregated, by = c("watershed" = "name")) %>% #dataset from Mapping/USGSdelWatersheds.R
  mutate(ratio_cl_drainage2020 = val2020 / DRNAREA) %>%
  mutate(ratio_cl_drainage2021 = val2021 / DRNAREA) %>%
  mutate(ave_ratio = (ratio_cl_drainage2020 + ratio_cl_drainage2021)/2)



ggplot(source_by_ws, aes(reorder(watershed, ave_ratio), ave_ratio)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Chloride input:Drainage Area",
       caption = "Figure X. Ratio of chloride input from road salt application to subwatershed drainage area.  
activities.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/cl_from_roadsalt.png", height = 15, width = 20, units = "cm")


#subwatershed size ascending
ggplot(source_by_ws, aes(reorder(watershed, DRNAREA), DRNAREA)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Drainage Area (ha)",
       caption = "Figure X. Size of each subwatershed.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/drainagearea.png", height = 15, width = 20, units = "cm")

## % development ascending
ggplot(source_by_ws, aes(reorder(watershed, DEVNLCD01), DEVNLCD01)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Developed Area %",
       caption = "Figure X. Percentage of subwatershed considered developed land.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/developed.png", height = 15, width = 20, units = "cm")


## % road density ascending
ggplot(source_by_ws, aes(reorder(watershed, road_density_mha), road_density_mha)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Road Density"~(m~ha^-1),
       caption = "Figure X. Road density in each subwatershed.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/roaddensity.png", height = 15, width = 20, units = "cm")

fit <- lm(ave_ratio ~ DRNAREA + road_density_mha + DEVNLCD01, source_by_ws)
summary(fit)
library(broom)
tidy(fit)

summary(lm(ave_ratio ~ DEVNLCD01, source_by_ws)) #p value 0.004754

##adding in load info
# source_load <- left_join(source_by_ws, loading, by = c("watershed" = "names")) #loading from cl_mass_load.R
# 
# sources_loads_by_ws <- source_load %>%
#   rename(salt_ratio = ave_ratio) %>%
#   mutate(load_ratio1920 = winter1920/DRNAREA) %>%
#   mutate(load_ratio2021 = winter2021/DRNAREA) %>%
#   mutate(load_ratio20 = notwinter20/DRNAREA) %>%
#   mutate(entire_ratio = entireload/DRNAREA)

#write_rds(sources_loads_by_ws, "Data/sources_loads_by_ws.rds")

sources_loads_by_ws <- readRDS("Data/sources_loads_by_ws.rds")

##chloride load for entire period
ggplot(sources_loads_by_ws, aes(reorder(watershed, entire_ratio), entire_ratio)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Chloride load:Drainage Area",
       caption = "Figure X. Ratio of chloride loading from each tributary during the entire study period (December 2019 - April 2021).") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/all_cl_loading.png", height = 15, width = 20, units = "cm")

##chloride load for winter 2019-2020
ggplot(sources_loads_by_ws, aes(reorder(watershed, load_ratio1920), load_ratio1920)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Chloride load:Drainage Area",
       caption = "Figure X. Ratio of chloride loading from each tributary during November 2019 - March 2020.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/cl_load_winter2019-2020.png", height = 15, width = 20, units = "cm")

##chloride load for winter 2020-2021
ggplot(sources_loads_by_ws, aes(reorder(watershed, load_ratio2021), load_ratio2021)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Chloride load:Drainage Area",
       caption = "Figure X. Ratio of chloride loading from each tributary during November 2020 - March 2021.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/cl_load_winter2020-2021.png", height = 15, width = 20, units = "cm")

##chloride load for not-winter 2020
ggplot(sources_loads_by_ws, aes(reorder(watershed, load_ratio20), load_ratio20)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Chloride load:Drainage Area",
       caption = "Figure X. Ratio of chloride loading from each tributary between winters in 2020.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/Ratio_comparing_subwatersheds/cl_load_notwinter.png", height = 15, width = 20, units = "cm")



