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
all_cllakes <- rbind(labME, labMO)

ggplot(all_cl1, aes(reorder(ID, chloride_mgL), chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), values = wes_palette("Darjeeling1")) +
                     #values = c("#1C366B", "#F24D29")) +
    labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme()

ggsave("Plots/figsforpres/spatial_distribution/low_conc_tribs.png", height = 15, width = 20, units = "cm")


ggplot(all_cllakes %>% filter(ID == "ME"), aes(reorder(ID, chloride_mgL), chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), values = wes_palette("Darjeeling1")) +
                     #values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme() +
  scale_y_continuous(n.breaks = 6)

ggsave("Plots/figsforpres/spatial_distribution/ME.png", height = 15, width = 20, units = "cm")

ggplot(all_cllakes %>% filter(ID == "MO"), aes(ID, chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), values = wes_palette("Darjeeling1")) +
  #values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme() #+
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 250))

ggsave("Plots/figsforpres/spatial_distribution/MO.png", height = 15, width = 20, units = "cm")

ggplot(all_cl2, aes(reorder(ID, chloride_mgL), chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), values = wes_palette("Darjeeling1")) +
  #values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme()

ggsave("Plots/figsforpres/spatial_distribution/high_conc_tribs.png", height = 15, width = 20, units = "cm")

#boxplot of winter vs not winter chloride
all <- bind_rows(all_cl1, all_cl2, all_cllakes)

ggplot(all, aes(reorder(season, chloride_mgL), chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), values = wes_palette("Darjeeling1")) +
  #values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + L_theme()

#Mann-Whitney test
wilcox.test((all %>% filter(season == "April - October"))$chloride_mgL, (all %>% filter(season == "November - March"))$chloride_mgL)

ggsave("Plots/figsforpres/spatial_distribution/saltvsno.png", height = 15, width = 20, units = "cm")

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



#mass into Mendota per month
timeseries_mass <- rbind(DC_monthly_mass %>% mutate(ID = "DC"), SMC_monthly_mass %>% mutate(ID = "SMC"), YI_monthly_mass %>% mutate(ID = "YI"), YN_monthly_mass %>% mutate(ID = "YN")) %>%
  rbind(PBMS_monthly_mass %>% mutate(ID = "PBMS"), PBSF_monthly_mass %>% mutate(ID = "PBSF"), SH_monthly_mass %>% mutate(ID = "SH")) %>%
  mutate(date = as.Date(as.yearmon(year_mon))) %>%
  mutate(monthly_mass_Mg = ifelse(ID == "YI", monthly_mass_Mg * -1, monthly_mass_Mg))

flux <- ts_all_month %>%
  dplyr::select(date, flux) %>%
  unique() %>%
  mutate(flux = round(flux, digits = 0))
  
ggplot(timeseries_mass %>% filter(ID != "PBSF")) +
  geom_bar(aes(fill = ID, x = date, y = monthly_mass_Mg), position = "stack", stat = "identity") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 6, type = "continuous")) +
  geom_text(flux, mapping = aes(date, flux, label = flux), position = position_dodge(width = 0.9), vjust = -0.25) +
  #scale_fill_viridis_d(option = "inferno", direction = -1) +
  L_theme() +
  labs(y = "Mass of Chloride (Mg)", x = "") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(n.breaks = 10)

ggsave("Plots/figsforpres/loadingME/massin&outME.png", height = 15, width = 20, units = "cm")



#maps in AverageChlorideMap.R



#lake mass changes

MO_mass <- read_rds("Data/MO_mass.rds") %>%
  mutate(tonnes_cl = (chloride_mgL * (vols * 1000)) / 1000000000) #first convert volume from m^3 to liter, then convert mg to Mg
ME_mass <- read_rds("Data/ME_mass.rds") %>%
  mutate(tonnes_cl = (chloride_mgL * (vols * 1000)) / 1000000000) #first convert volume from m^3 to liter, then convert mg to Mg

MO_mass_daynum <- MO_mass %>%
  group_by(year4, sampledate, daynum) %>%
  summarise(total = sum(tonnes_cl))

ME_mass_daynum <- ME_mass %>%
  group_by(year4, sampledate, daynum) %>%
  summarise(total = sum(tonnes_cl))



ggplot() +
  geom_smooth(method = loess, data = ME_mass_daynum, mapping = aes(x = daynum, y = total, group = year4, color = year4)) +
  scale_color_viridis_c(option = "inferno", direction = -1) + L_theme() + 
  labs(x = "", y = "Mass of Chloride (Mg)", title = "Lake Mendota") #+
  #geom_smooth(method = loess, ME_mass_daynum %>% filter(year4 == 2020), mapping = aes(daynum, total, group = year4), color = "red")

ggsave("Plots/figsforpres/cl_annual_change/ME.png", height = 15, width = 20, units = "cm") 



ggplot() +
  geom_smooth(method = loess, MO_mass_daynum, mapping = aes(daynum, total, group = year4, color = year4)) +
  scale_color_viridis_c(option = "inferno", direction = -1) + L_theme() + 
  labs(x = "", y = "Mass of Chloride (Mg)", title = "Lake Monona")# +
 # geom_smooth(method = loess, MO_mass_daynum %>% filter(year4 == 2020), mapping = aes(daynum, total, group = year4), color = "red")

ggsave("Plots/figsforpres/cl_annual_change/MO.png", height = 15, width = 20, units = "cm") 





#loading plots

timeseries_mass_dens <- timeseries_mass %>%
  mutate(monthly_mass_Mg = ifelse(ID == "YI", monthly_mass_Mg * -1, monthly_mass_Mg)) %>%
  filter(ID != "PBSF") 

YI_out <- timeseries_mass_dens %>%
  filter(ID == "YI") %>%
  group_by(date) %>%
  summarise(totalout = sum(monthly_mass_Mg))

ts_all_month <- timeseries_mass_dens %>%
  filter(ID != "YI") %>%
  group_by(date) %>%
  summarise(totalin = sum(monthly_mass_Mg)) %>%
  left_join(YI_out, by = "date") %>% 
  mutate(flux = totalin - totalout) %>%
  pivot_longer(cols = 2:3)

spline_int1 <- as.data.frame(spline(ts_in_month$date, ts_in_month$total)) %>%
  mutate(x = as.Date(x, origin = "1970-01-01"))
spline_int2 <- as.data.frame(spline((timeseries_mass_dens %>% filter(ID == "YI"))$date, (timeseries_mass_dens %>% filter(ID == "YI"))$monthly_mass_Mg)) %>%
  mutate(x = as.Date(x, origin = "1970-01-01"))

ME_mass_dens <- ME_mass_daynum  %>% 
  filter(year4 > 2019) %>%
  mutate(sampledate = as.Date(sampledate))

spline_int3 <- as.data.frame(spline(ME_mass_dens$sampledate, ME_mass_dens$total)) %>%
  mutate(x = as.Date(x, origin = "1970-01-01 00:00:00")) %>%
  mutate(x = as.POSIXct(x))


datemin = as.Date("2019-11-01 00:00:00")  
datemax = as.Date("2021-04-18 00:00:00") 

a <- ggplot() +
  geom_point(ts_all_month, mapping = aes(x = date, y = value, color = name)) +
  scale_color_manual(values = wes_palette("Darjeeling1")[1:2],
                     labels = c("chloride in", "chloride out")) +
  geom_line(data = spline_int1, mapping = aes(x = x, y = y), color= wes_palette("Darjeeling1")[1]) +
  geom_line(data = spline_int2, mapping = aes(x = x, y = y), color = wes_palette("Darjeeling1")[2]) +
  L_theme() + theme(legend.position = "top") + labs(x = "", y = "Mass of Chloride (Mg)", title = "Tributaries") +
  scale_x_date(limits = c(datemin, datemax), date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-12"))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-22"))) +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-03"))) +
  geom_vline(xintercept = as.numeric(as.Date("2021-03-20")))



b <- ggplot() +
  geom_point(ME_mass_dens, mapping = aes(x = as.POSIXct(sampledate), y = total)) +
  geom_line(data = spline_int3, mapping = aes(x = as.POSIXct(x), y = y)) +
  L_theme() +
  labs(x = "", y = "Mass of Chloride (Mg)", title = "Lake Mendota") +
  scale_x_datetime(limits = c(as.POSIXct(datemin), as.POSIXct(datemax)), date_breaks = "3 months", date_labels = "%b %Y") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-01-12 00:00:00"))) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-22 00:00:00"))) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-01-03 00:00:00"))) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-03-20 00:00:00")))

a/b

ggsave("Plots/figsforpres/loadingME/flux.png", height = 15, width = 20, units = "cm")



#linreg for example 


qsc <- join_for_linreg(labPBMS, fieldcondPBMS, PBMS_cond_data)


ggplot(qsc, aes(sp.cond.x, chloride_mgL)) +
  geom_point(aes(color = season)) + 
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  #scale_color_manual(labels = c("April-October", "November-March"),
                    # values = c("#1C366B", "#F24D29"))  +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  #stat_cor() + 
  #stat_regline_equation() + 
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  L_theme()

ggsave("Plots/figsforpres/pbmslinreg.png", width = 20, height = 15, units = "cm")



#log of daily average chloride concentrations for all tribs on one graph

all_tribs_low <-
  rbind(
    YN_daily_mass %>% mutate(ID = "YN"),
    #YI_daily_mass %>% mutate(ID = "YI"),
    #YS_daily_mass %>% mutate(ID = "YS"),
    SMC_daily_mass %>% mutate(ID = "SMC"),
    DC_daily_mass %>% mutate(ID = "DC")
  )

all_tribs_med <- rbind(
  YI_daily_mass %>% mutate(ID = "YI"),
  YS_daily_mass %>% mutate(ID = "YS")
)


all_tribs_high <-
  rbind(
    WIC_daily_mass %>% mutate(ID = "WIC"),
    SW_daily_mass %>% mutate(ID = "SW"),
    PBMS_daily_mass %>% mutate(ID = "PBMS"),
    PBSF_daily_mass %>% mutate(ID = "PBSF"),
    SH_daily_mass %>% mutate(ID = "SH")
  )

a <- ggplot(all_tribs_low %>% filter(date > "2020-11-01"), aes(date, log(daily_concentration_mgL), group = ID, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1", 10, "continuous")[6:10]) +
  L_theme() + theme(legend.position = "bottom") +
  labs(
       y = "",
       x = "")

b <- ggplot(all_tribs_high %>% filter(date > "2020-11-01"), aes(date, log(daily_concentration_mgL), group = ID, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1", 10, "continuous")[1:5]) +
  L_theme() + theme(legend.position = "bottom") +
  labs(
    y = "Log of Chloride Concentration"~(mg~L^-1),
    x = "")

c <- ggplot(all_tribs_med %>% filter(date > "2020-11-01"), aes(date, log(daily_concentration_mgL), group = ID, color = ID)) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1", 10, "continuous")[1:5]) +
  L_theme() + theme(legend.position = "bottom") +
  labs(
    y = "Log of Chloride Concentration"~(mg~L^-1),
    x = "")
#ggsave("Plots/figsforpres/logcl.png", height = 15, width = 20, units = "cm")

a/b

ggsave("Plots/figsforpres/logcl.png", height = 15, width = 20, units = "cm")
