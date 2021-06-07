source("Functions/interpolate_for_heatmap.R")

#formatting chloride data to make this work (just study period)
labME2 <- labME %>% bind_rows(ME_profile_chloride %>% rename(chloride_mgL = chloride_use_mgL)) %>%
  bind_rows(ME_winter_chloride %>% rename(chloride_mgL = chloride_use_mgL) %>% mutate(Date = as.Date(date))) %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(sampledate = Date)  %>%
  rename(depth = Depth_m) %>%
  dplyr::select(sampledate, depth, chloride_mgL) %>%
  drop_na(chloride_mgL)

###try with LTER data-- use ME_mass
labME2 <- ME_mass %>%
  dplyr::select(sampledate, depth, chloride_mgL) %>%
  drop_na(chloride_mgL)

maxdepth = 23.5 # Should be depth of lowest sample, not necessarily depth of lake 
usedates = labME2 %>%
  dplyr::distinct(sampledate) 

f <- lapply(X = usedates$sampledate, FUN = interpData, observationDF = labME2,
            maxdepth = maxdepth)

f = as.data.frame(do.call(cbind, f))
names(f) = usedates$sampledate

# Bind list into dataframe
f2 = bind_cols(depth = c(0, 5, 10, 15, 20, 23.5),f) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
ggplot() +
  guides(fill = guide_colorsteps(barheight = unit(3, "in"))) +
  geom_contour_filled(f2, mapping = aes(x = sampledate, y = depth, z = var), binwidth = 1) +
  geom_point(labME2, mapping = aes(sampledate, depth), color = "#C4CFD0") +
  #theme_minimal() +
  scale_y_reverse() +
  scale_fill_viridis_d("Chloride
Concentration"~(mg~L^-1), option = "inferno", direction = -1) +
  labs(x = "", y = "Depth (m)", 
  caption = "Figure 42. Interpolated chloride concentrations in Lake Mendota based on manual sampling
and continuously measured conductivity (grey points).") +
  theme(plot.caption = element_text(size = 10, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  L_theme()#+
  #scale_x_date(date_breaks = "6 months", date_labels = "%Y-%M") 

ggsave("Plots/chloride_time_series/mendota_heatmap.png", height = 4.25, width = 6.25, units = "in")
#ggsave("Plots/figsforpres/cl_annual_change/mendota_heatmap.png", height = 4.25, width = 6.25, units = "in")



###Monona data LTER
labMo2 <- MO_mass %>%
  dplyr::select(sampledate, depth, chloride_mgL) %>%
  drop_na(chloride_mgL)
#Monona collected data
labMO2 <- labMO %>%
  bind_rows(MO_winter_chloride %>% rename(chloride_mgL = chloride_use_mgL) %>% mutate(Date = as.Date(date))) %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(sampledate = date)  %>%
  mutate(sampledate = as.Date(sampledate)) %>%
  rename(depth = Depth_m) %>%
  dplyr::select(sampledate, depth, chloride_mgL) %>%
  drop_na(chloride_mgL)

maxdepth = 20 # Should be depth of lowest sample, not necessarily depth of lake 
usedates = labMO2 %>%
  dplyr::distinct(sampledate) 

f <- lapply(X = usedates$sampledate, FUN = interpData, observationDF = labMO2,
            maxdepth = maxdepth)

f = as.data.frame(do.call(cbind, f))
names(f) = usedates$sampledate

# Bind list into dataframe
f2 = bind_cols(depth = c(0,4, 8, 12, 16, 20),f) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
ggplot() +
  guides(fill = guide_colorsteps(barheight = unit(3, "in"))) +
  geom_contour_filled(f2, mapping = aes(x = sampledate, y = depth, z = var), binwidth = 20) +
  geom_point(labMO2, mapping = aes(sampledate, depth), color = "#C4CFD0") +
  theme_minimal() +
  scale_y_reverse() +
  scale_fill_viridis_d("Chloride
Concentration"~(mg~L^-1), option = "inferno", direction = -1, end = 0.9) +
  labs(x = "", y = "Depth (m)", 
       caption = "Figure 43. Interpolated chloride concentrations in Lake Monona based on manual sampling
and continuously measured conductivity (grey points).") +
  theme(plot.caption = element_text(size = 10, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))  #+
  #scale_x_date(date_breaks = "3 years", date_labels = "%Y") 

ggsave("Plots/chloride_time_series/monona_heatmap.png", height = 4.25, width = 6.25, units = "in")
#ggsave("Plots/figsforpres/cl_annual_change/monona_heatmap.png", height = 15, width = 30, units = "cm")
