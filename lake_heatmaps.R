source("Functions/interpolate_for_heatmap.R")

#formatting chloride data to make this work (just study period)
labME2 <- labME %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(sampledate = as.Date(date))  %>%
  rename(depth = Depth_m) %>%
  select(sampledate, depth, chloride_mgL)

###try with all Mendota data??? -- use ME_mass
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
  guides(fill = guide_colorsteps(barheight = unit(10, "cm"))) +
  geom_contour_filled(f2, mapping = aes(x = sampledate, y = depth, z = var), binwidth = 5) +
  geom_point(labME2, mapping = aes(sampledate, depth), color = "#C4CFD0") +
  theme_minimal() +
  scale_y_reverse() +
  scale_fill_viridis_d(option = "inferno") +
  labs(title= "Lake Mendota", x = "", y = "Depth (m)", fill = "Chloride Concentration"~(mg~L^-1)) +
  #caption = "Figure X. Chloride concentrations (mg/L) in Lake Mendota over study period. Grey points indicate dates and where 
  #in the water column water samples were taken and analyzed for chloride.") +
  theme(plot.caption = element_text(size = 10, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

#ggsave("Plots/chloride_time_series/mendota_heatmap.png", height = 15, width = 20, units = "cm")
ggsave("Plots/figsforpres/cl_annual_change/mendota_heatmap.png", height = 15, width = 30, units = "cm")


###Monona data
labME2 <- MO_mass %>%
  dplyr::select(sampledate, depth, chloride_mgL) %>%
  drop_na(chloride_mgL)

maxdepth = 20 # Should be depth of lowest sample, not necessarily depth of lake 
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
  guides(fill = guide_colorsteps(barheight = unit(10, "cm"))) +
  geom_contour_filled(f2, mapping = aes(x = sampledate, y = depth, z = var), binwidth = 20) +
  geom_point(labME2, mapping = aes(sampledate, depth), color = "#C4CFD0") +
  theme_minimal() +
  scale_y_reverse() +
  scale_fill_viridis_d(option = "inferno") +
  labs(title = "Lake Monona", x = "", y = "Depth (m)", fill = "Chloride Concentration"~(mg~L^-1)) +
  #caption = "Figure X. Chloride concentrations (mg/L) in Lake Mendota over study period. Grey points indicate dates and where 
  #in the water column water samples were taken and analyzed for chloride.") +
  theme(plot.caption = element_text(size = 10, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

#ggsave("Plots/chloride_time_series/mendota_heatmap.png", height = 15, width = 20, units = "cm")
ggsave("Plots/figsforpres/cl_annual_change/monona_heatmap.png", height = 15, width = 30, units = "cm")
