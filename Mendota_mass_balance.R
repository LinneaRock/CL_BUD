
#need timeseries combined into TS_ME in cl_mass_load 
#Mendota loading
ggplot(TS_ME %>% filter(ID != "PBSF")) +
  geom_line(aes(date, (cl_load_g / 1000))) +
  facet_wrap(~ID, scales = "free_y") +
  labs(x = "", y = "Chloride Mass"~(kg),
       caption = "Figure X. Estimated chloride mass loading timeseries for waters flowing into Lake Mendota.") +
  L_theme() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/chloride_loading/ME_inlets_ts.png", height = 4.25, width = 6.25, units = "in")

#Mendota exiting
ggplot(YI_ts_mass) +
  geom_line(aes(date, (cl_load_g / 1000))) +
  #facet_wrap(~ID, scales = "free_y") +
  labs(x = "", y = "Chloride Mass"~(kg),
       caption = "Figure X. Estimated chloride mass loading timeseries for the outflow of Lake Mendota.") +
  L_theme() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/chloride_loading/ME_outlet_ts(YI).png", height = 4.25, width = 6.25, units = "in")



#monthly mass balance
timeseries_mass <- rbind(DC_monthly_mass %>% mutate(ID = "DC"), SMC_monthly_mass %>% mutate(ID = "SMC"), YI_monthly_mass %>% mutate(ID = "YI"), YN_monthly_mass %>% mutate(ID = "YN")) %>%
  rbind(PBMS_monthly_mass %>% mutate(ID = "PBMS"), SH_monthly_mass %>% mutate(ID = "SH")) %>%
  mutate(date = as.Date(as.yearmon(year_mon))) %>%
  mutate(monthly_mass_Mg = ifelse(ID == "YI", monthly_mass_Mg * -1, monthly_mass_Mg))

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

flux <- ts_all_month %>%
  dplyr::select(date, flux) %>%
  unique() %>%
  mutate(flux = round(flux, digits = 0))



ggplot(timeseries_mass) +
  geom_bar(aes(fill = ID, x = date, y = monthly_mass_Mg), position = "stack", stat = "identity") +
  geom_text(flux, mapping = aes(date, flux, label = flux), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  scale_fill_viridis_d(option = "inferno") +
  L_theme() +
  labs(y = "Mass of Chloride (Mg)", x = "",
       caption = "Figure X. Monthly mass balance of surface water and Spring Harbor storm sewer chloride 
mass flows into and out of Lake Mendota") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/chloride_loading/massin&outME.png", height = 4.25, width = 6.25, units = "in")

net = sum((timeseries_mass %>% filter(ID != "YI"))$monthly_mass_Mg) + sum((timeseries_mass %>% filter(ID == "YI"))$monthly_mass_Mg) #net is -1790.93 tonnes (YI is outlet so negative)


cl_roads_by_subwatershed = read_rds("Data/chloridefromroadsinsubws.rds")

ME_roads = cl_roads_by_subwatershed %>%
  filter(watershed == "DC" | watershed == "PBMS" | watershed == "SH" | watershed == "SMC" | watershed == "YN" | watershed == "YI") 

total = rowSums(ME_roads[2:3])

ME_roads <- ME_roads %>%
  mutate(tot = total)
#Full study period
sum((ME_roads%>%filter(watershed != "YI"))$tot) #15607.85
#madison/county + Middleton 2 years + agriculture (cattle + optimal K content #)
15607.85 + ((811 + 933) * 0.907185) + cattle + 1292.1879 #=20240.74
#just 2019-20
sum((ME_roads%>%filter(watershed != "YI"))$val2020) #8473.926 
8473.926 + (811 * 0.907185) + cattle + 1292.1879 #=12260.42
