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
  scale_color_manual(labels = c("April-October", "November-March"),
                     values = c("#1C366B", "#F24D29")) +
    labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme()

ggsave("Plots/figsforpres/low_conc_tribs.png", height = 15, width = 20, units = "cm")


ggplot(all_cl2, aes(ID, chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"),
                     values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme()

ggsave("Plots/figsforpres/high_conc_tribs.png", height = 15, width = 20, units = "cm")

#low concentration cl timeseries 
timeseries_lowconc <- rbind(DC_daily_mass %>% mutate(ID = "DC"), SMC_daily_mass %>% mutate(ID = "SMC"), YI_daily_mass %>% mutate(ID = "YI", YN_daily_mass %>% mutate(ID = "YN")))
timeseries_highconc <- rbind(PBMS_daily_mass %>% mutate(ID = "PBMS"), PBSF_daily_mass %>% mutate(ID = "PBSF"), SH_daily_mass %>% mutate(ID = "SH"))

library(wesanderson)

a <- ggplot(timeseries_lowconc, aes(date, daily_concentration_mgL, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("BottleRocket2")) +
  L_theme() +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1))

ggsave("Plots/figsforpres/low_conc_ts.png", height = 15, width = 20, units = "cm")

b <- ggplot(timeseries_highconc, aes(date, daily_concentration_mgL, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("BottleRocket2")) +
  L_theme() +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1))

ggsave("Plots/figsforpres/high_conc_ts.png", height = 15, width = 20, units = "cm")


precip_plot / cl_roads / a
ggsave("Plots/figsforpres/lows.png", height = 15, width = 20, units = "cm")

precip_plot / cl_roads / b
ggsave("Plots/figsforpres/highs.png", height = 15, width = 20, units = "cm")

