#figures for presentation

# precip 
datemin = as.POSIXct("2019-12-19 00:00:00", tz = "ETC/GMT-6")
datemax = as.POSIXct("2021-04-10 04:30:00", tz = "ETC/GMT-6")

precip_plot <- ggplot(precip_data, aes(date, PRCP)) +
  geom_bar(stat = "identity") +
  L_theme() +
  scale_x_datetime(limits = c(datemin, datemax)) +
  labs(x = "", y = "",
       title = "Daily Precipitation (mm)")

cl_roads <- ggplot(TOTALS_BY_DATE %>% filter(year == "2019-2020" | year == "2020-2021"), aes(as.POSIXct(DATE), chloride)) +
  geom_bar(stat = "identity") +
  L_theme() +
  scale_x_datetime(limits = c(datemin, datemax)) +
  labs(x = "", y = "",
       title = "Daily Chloride Application from Road Salting in Madison (Mg)")

library(patchwork)

precip_plot / cl_roads

ggsave("Plots/figsforpres/precip_salt.png", height = 15, width = 20, units = "cm")


##boxplot of trib chloride concentrations
all_cl1 <- rbind(labDC, lab6MC, labYI, labYN, labYS)
all_cl2 <- rbind(labPBMS, labPBSF, labSW, labWIC)

ggplot(all_cl1, aes(ID, chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), values = wes_palette("Darjeeling1")) +
                     #values = c("#1C366B", "#F24D29")) +
    labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme()

ggsave("Plots/figsforpres/spatial_distribution/low_conc_tribs.png", height = 15, width = 20, units = "cm")


ggplot(all_cl2, aes(ID, chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), values = wes_palette("Darjeeling1")) +
                     #values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme()

ggsave("Plots/figsforpres/spatial_distribution/high_conc_tribs.png", height = 15, width = 20, units = "cm")

#low concentration cl timeseries 
timeseries_lowconc <- rbind(DC_daily_mass %>% mutate(ID = "DC"), SMC_daily_mass %>% mutate(ID = "SMC"), YI_daily_mass %>% mutate(ID = "YI", YN_daily_mass %>% mutate(ID = "YN")))
timeseries_highconc <- rbind(PBMS_daily_mass %>% mutate(ID = "PBMS"), PBSF_daily_mass %>% mutate(ID = "PBSF"), SH_daily_mass %>% mutate(ID = "SH"))

library(wesanderson)

a <- ggplot(timeseries_lowconc, aes(as.POSIXct(date), daily_concentration_mgL, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  L_theme() +
  scale_x_datetime(limits = c(datemin, datemax)) +
  labs(x = "", y = "", title = "Chloride Concentration"~(mg~L^-1)) + theme(legend.position = "bottom")

ggsave("Plots/figsforpres/low_conc_ts.png", height = 15, width = 20, units = "cm")

b <- ggplot(timeseries_highconc %>% filter(ID != "PBMS"), aes(as.POSIXct(date), daily_concentration_mgL, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  scale_x_datetime(limits = c(datemin, datemax)) +
  L_theme() +
  labs(x = "", y = "", title = "Chloride Concentration"~(mg~L^-1)) + theme(legend.position = "bottom")

c <- ggplot(timeseries_highconc %>% filter(ID == "PBMS"), aes(as.POSIXct(date), daily_concentration_mgL, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1")[4]) +
  scale_x_datetime(limits = c(datemin, datemax)) +
  L_theme() +
  labs(x = "", y = "", title = "Chloride Concentration"~(mg~L^-1)) + theme(legend.position = "bottom")

b/c
ggsave("Plots/figsforpres/high_conc_ts.png", height = 15, width = 20, units = "cm")

#plots with precip and salt app.
precip_plot / cl_roads / a 
ggsave("Plots/figsforpres/lows.png", height = 15, width = 20, units = "cm")

precip_plot / cl_roads / b / c
ggsave("Plots/figsforpres/highs.png", height = 15, width = 20, units = "cm")



#mass into Mendota per season
timeseries_mass <- rbind(DC_monthly_mass %>% mutate(ID = "DC"), SMC_monthly_mass %>% mutate(ID = "SMC"), YI_monthly_mass %>% mutate(ID = "YI"), YN_monthly_mass %>% mutate(ID = "YN")) %>%
  rbind(PBMS_monthly_mass %>% mutate(ID = "PBMS"), PBSF_monthly_mass %>% mutate(ID = "PBSF"), SH_monthly_mass %>% mutate(ID = "SH")) %>%
  mutate(date = as.Date(as.yearmon(year_mon))) %>%
  mutate(monthly_mass_Mg = ifelse(ID == "YI", monthly_mass_Mg * -1, monthly_mass_Mg))

ggplot(timeseries_mass %>% filter(ID != "PBSF")) +
  geom_bar(aes(fill = ID, x = date, y = monthly_mass_Mg), position = "stack", stat = "identity") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 6, type = "continuous")) +
  #scale_fill_viridis_d(option = "inferno", direction = -1) +
  L_theme() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(n.breaks = 10)

ggsave("Plots/figsforpres/loadingME/massin&outME.png", height = 15, width = 20, units = "cm")



#maps in AverageChlorideMap.R
