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

#just the spring values for mass
ME_spring <- ME_mass_daynum %>%
  filter(month(sampledate) == 04 | month(sampledate) == 05 | month(sampledate) == 06)

ME_change <- ME_spring %>%
  mutate(change = total - lag(total))

MO_spring <- MO_mass_daynum %>%
  filter(month(sampledate) == 04 | month(sampledate) == 05 | month(sampledate) == 06)

MO_change <- MO_spring %>%
  mutate(change = total - lag(total))

ggplot() +
  geom_point(data = ME_mass_daynum %>% filter(year4 != 2021), mapping = aes(x = daynum, y = total, group = year4, color = year4)) +
  geom_smooth(method = "loess", data = ME_mass_daynum %>% filter(year4 != 2021), mapping = aes(x = daynum, y = total, group = year4, color = year4)) +
  scale_color_viridis_c(option = "inferno", direction = -1) + L_theme() + 
  labs(x = "", y = "Mass of Chloride (Mg)", title = "Lake Mendota") 

ggplot() +
  geom_point(ME_mass_daynum, mapping = aes(sampledate, total)) +
  geom_point(ME_spring, mapping = aes(sampledate, total), color = "#1DACE8") +
  labs(x = "", y = "Chloride Mass (Mg)",
       caption = "Figure X. Total mass of chloride in metric tonnes (Mg) in Lake Mendota from 1996 - 2021.
Colored points represent total chloride mass in spring of each year.") +
  L_theme()

ggsave("Plots/masschangeME.png", height = 4.25, width = 6.25, units = "in")




ggplot() +
  geom_point(data = MO_mass_daynum %>% filter(year4 != 2021), mapping = aes(x = daynum, y = total, group = year4, color = year4)) +
  geom_smooth(method = loess, MO_mass_daynum %>% filter(year4 != 2021), mapping = aes(daynum, total, group = year4, color = year4)) +
  scale_color_viridis_c(option = "inferno", direction = -1) + L_theme() + 
  labs(x = "", y = "Mass of Chloride (Mg)", title = "Lake Monona")

ggplot() +
  geom_point(MO_mass_daynum, mapping = aes(sampledate, total)) +
  geom_point(MO_spring, mapping = aes(sampledate, total), color = "#1DACE8") +
  labs(x = "", y = "Chloride Mass (Mg)",
       caption = "Figure X. Total mass of chloride in metric tonnes (Mg) in Lake Monona from 1996 - 2021.
Colored points represent total chloride mass in spring of each year.") +  
  L_theme()

ggsave("Plots/masschangeMO.png", height = 4.25, width = 6.25, units = "in")



