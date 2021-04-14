
labME2 <- labME %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(date = as.Date(date))

#heatmap of chloride concentration
ggplot(labME2, aes(x = date, y = Depth_m, z = chloride_mgL)) +
  geom_contour_filled() +
  geom_point(aes(date, Depth_m), color = "#C4CFD0") +
  theme_minimal() +
  scale_y_reverse() +
  scale_fill_viridis_d(option = "inferno") +
  labs(x = "", y = "Depth (m)", fill = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Chloride concentrations (mg/L) in Lake Mendota over study period. Grey points indicate dates and where 
in the water column water samples were taken and analyzed for chloride.") +
  theme(plot.caption = element_text(size = 10, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

ggsave("Plots/chloride_time_series/mendota_heatmap.png", height = 15, width = 20, units = "cm")