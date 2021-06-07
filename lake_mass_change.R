#lake mass changes

#PHMDC data
watershed <- read_xlsx("Data/Historical_External/YaharaHist.xlsx")

ME_point_dat <- watershed %>%
  dplyr::select(Date, ME) %>%
  drop_na %>%
  rename(sampledate = Date,
         chloride_mgL = ME) %>%
  mutate(vols = ME_vol) %>%
  mutate(total = (chloride_mgL * (vols * 1000)) / 1000000000) %>% #first convert volume from m^3 to liter, then convert mg to Mg
  mutate(LTER = "N") %>%
  mutate(year4 = year(sampledate))

MO_point_dat <- watershed %>%
  dplyr::select(Date, MO) %>%
  drop_na %>%
  rename(sampledate = Date,
         chloride_mgL = MO) %>%
  mutate(vols = MO_vol) %>%
  mutate(total = (chloride_mgL * (vols * 1000)) / 1000000000)%>% #first convert volume from m^3 to liter, then convert mg to Mg
  mutate(LTER = "N") %>%
  mutate(year4 = year(sampledate))

#LTER and my data
MO_mass <- read_rds("Data/MO_mass.rds") %>%
  mutate(tonnes_cl = (chloride_mgL * (vols * 1000)) / 1000000000) #first convert volume from m^3 to liter, then convert mg to Mg
ME_mass <- read_rds("Data/ME_mass.rds") %>%
  mutate(tonnes_cl = (chloride_mgL * (vols * 1000)) / 1000000000) #first convert volume from m^3 to liter, then convert mg to Mg


MO_mass_daynum <- MO_mass %>%
  group_by(year4, sampledate, daynum) %>%
  summarise(total = sum(tonnes_cl)) %>%
  bind_rows(MO_point_dat %>% filter(year4 == 2019)) %>%
  arrange(sampledate) 


ME_mass_daynum <- ME_mass %>%
  group_by(year4, sampledate, daynum) %>%
  summarise(total = sum(tonnes_cl)) %>%
 bind_rows(ME_point_dat %>% filter(year4 == 2019)) %>%
  arrange(sampledate) 

#just the spring values for mass
ME_spring <- ME_mass_daynum %>%
  filter(month(sampledate) == 04 | month(sampledate) == 05)
  #filter(month(sampledate) == 01 | month(sampledate) == 02)

#lag will not work in the dataframe for some reason, so I'm doing this dumb workaround
change <- ME_spring$total - lag(ME_spring$total)
diff <- data.frame(
  change = change,
  sampledate = ME_spring$sampledate
)

ME_change <- ME_spring %>%
  left_join(diff)
sum(ME_change$change, na.rm= TRUE)


MO_spring <- MO_mass_daynum %>%
  filter(month(sampledate) == 04 | month(sampledate) == 05) %>%
  filter(year4 < 2019) %>%
  bind_rows(MO_mass_daynum %>% filter(month(sampledate) == 06 & year4 >= 2019)) %>%
  bind_rows(MO_mass_daynum %>% filter(month(sampledate) == 04 & year4 >= 2021)) %>%
  arrange(sampledate)

#lag will not work in the dataframe for some reason, so I'm doing this dumb workaround
change <- MO_spring$total - lag(MO_spring$total)
diff <- data.frame(
  change = change,
  sampledate = MO_spring$sampledate
)

MO_change <- MO_spring %>%
  left_join(diff)
sum(MO_change$change, na.rm = TRUE)


ggplot() +
  geom_point(data = ME_mass_daynum %>% filter(year4 != 2021), mapping = aes(x = daynum, y = total, group = year4, color = year4)) +
  geom_smooth(method = "loess", data = ME_mass_daynum %>% filter(year4 != 2021), mapping = aes(x = daynum, y = total, group = year4, color = year4)) +
  scale_color_viridis_c(option = "inferno", direction = -1) + L_theme() + 
  labs(x = "", y = "Mass of Chloride (Mg)", title = "Lake Mendota") 

ggplot(ME_change) +
  geom_bar(aes(year4, change), stat = "identity") +
  labs(x = "", y = "Change in Mass (Mg)",
       caption = "Figure 48. Change in spring chloride mass in metric tonnes (Mg) each year in Lake 
Mendota.") +
  L_theme() 

ggsave("Plots/changesME.png", height = 4.25, width = 6.25, units = "in")

ggplot() +
  geom_point(ME_mass_daynum, mapping = aes(sampledate, total)) +
  #geom_point(ME_point_dat, mapping = aes(as.Date(sampledate), tonnes_cl), color = "orange") +
  geom_point(ME_spring, mapping = aes(sampledate, total), color = "#1DACE8") +
  labs(x = "", y = "Chloride Mass (Mg)",
       caption = "Figure 46. Total mass of chloride in metric tonnes (Mg) in Lake Mendota from 1996 - 2021.
Blue points represent total chloride mass in spring of each year, while black points are
chloride mass for other times in the year.") +
  L_theme()

ggsave("Plots/masschangeME.png", height = 4.25, width = 6.25, units = "in")




ggplot() +
  geom_point(data = MO_mass_daynum %>% filter(year4 != 2021), mapping = aes(x = daynum, y = total, group = year4, color = year4)) +
  geom_smooth(method = loess, MO_mass_daynum %>% filter(year4 != 2021), mapping = aes(daynum, total, group = year4, color = year4)) +
  scale_color_viridis_c(option = "inferno", direction = -1) + L_theme() + 
  labs(x = "", y = "Mass of Chloride (Mg)", title = "Lake Monona")

ggplot(MO_change) +
  geom_bar(aes(year4, change), stat = "identity") +
  labs(x = "", y = "Change in Mass (Mg)",
       caption = "Figure 49. Change in spring chloride mass in metric tonnes (Mg) each year in Lake Monona.") +
  L_theme() 

ggsave("Plots/changesMO.png", height = 4.25, width = 6.25, units = "in")

ggplot() +
  geom_point(MO_mass_daynum, mapping = aes(sampledate, total)) +
  geom_point(MO_spring, mapping = aes(sampledate, total), color = "#1DACE8") +
  labs(x = "", y = "Chloride Mass (Mg)",
       caption = "Figure 47. Total mass of chloride in metric tonnes (Mg) in Lake Monona from 1996 - 2021.
Blue points represent total chloride mass in spring of each year, while black points are
chloride mass for other times in the year.") +  
  L_theme()

ggsave("Plots/masschangeMO.png", height = 4.25, width = 6.25, units = "in")


#lake mass changes - both lakes on the plots

ggplot() +
  geom_point(ME_mass_daynum, mapping = aes(sampledate, total, color = "ME")) +
  geom_point(ME_spring, mapping = aes(sampledate, total, shape = "ME"), color = "#1C366B") +
  geom_point(MO_mass_daynum, mapping = aes(sampledate, total, color = "MO")) +
  geom_point(MO_spring, mapping = aes(sampledate, total, shape = "MO"), color = "#F24D29") +
  scale_color_manual(labels = c("Mendota", "Monona"),
                     values = c("#1C366B", "#F24D29")) +
  scale_shape_manual(labels = "Spring Mass",
                     values = c(8,8)) +
  labs(x = "", y = "Chloride Mass (Mg)",
       caption = "Figure X. Total mass of chloride in metric tonnes (Mg) in Lake Monona from 1996 - 2021.
Colored points represent total chloride mass in spring of each year.") +  
  L_theme() + theme(legend.title = element_blank())



#percent mass change in lakes
lakes_change <- MO_change %>% 
  mutate(ID = "MO") %>%
  mutate(percentchange = (change/total) * 100) %>%
  bind_rows(ME_change %>% 
              mutate(ID = "ME") %>%
              mutate(percentchange = (change/total) * 100))


ggplot(lakes_change) +
  geom_bar(aes(year(sampledate), percentchange, group = ID, fill = ID), stat = "identity", position = "dodge") +
  labs(x = "", y = "Percent Change in Mass of Chloride",
       caption = "Figure 50. Percent change in spring chloride mass in metric tonnes each year in Lakes
Mendota and Monona.") +
  L_theme() + scale_fill_manual(labels = c("Mendota", "Monona"),
                                values = c("#1C366B", "#F24D29")) +
  theme(legend.title = element_blank()) #,axis.text.x = element_text(angle = 45, hjust = 1)) +
 # scale_x_date(date_breaks = "1 year", date_labels = "%Y")  
ggsave("Plots/masschangeinlakes.png", height = 4.25, width = 6.25, units = "in")



