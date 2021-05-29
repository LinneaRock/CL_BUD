
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

#total net
net = sum((timeseries_mass %>% filter(ID != "YI"))$monthly_mass_Mg) + sum((timeseries_mass %>% filter(ID == "YI"))$monthly_mass_Mg) #net is -1791.333 tonnes (YI is outlet so negative)

#19-20 through October 2020
sum((timeseries_mass %>% 
             filter(ID != "YI") %>%
             filter(year_mon != "2020-11" &
                      year_mon != "2020-12" &
                      year_mon != "2021-1" &
                      year_mon != "2021-2" &
                      year_mon != "2021-3" &
                      year_mon != "2021-4"))$monthly_mass_Mg)# 6974.988 tonnes
sum((timeseries_mass %>% 
       filter(ID == "YI") %>%
       filter(year_mon != "2020-11" &
                year_mon != "2020-12" &
                year_mon != "2021-1" &
                year_mon != "2021-2" &
                year_mon != "2021-3" &
                year_mon != "2021-4"))$monthly_mass_Mg) #net is -8116.41 tonnes (YI is outlet so negative)


cl_roads_by_subwatershed = read_rds("Data/chloridefromroadsinsubws.rds")

ME_roads = cl_roads_by_subwatershed %>%
  filter(watershed == "DC" | watershed == "PBMS" | watershed == "SH" | watershed == "SMC" | watershed == "YN" | watershed == "YI") 

#total salt in 19-20 entering Mendota
sum((ME_roads %>% filter(watershed != "YI"))$val2020) #8473.93 tonnes 


ME_roads_vs_loads <- loading %>% #loading df from cl_mass_load.R
  left_join(ME_roads %>% rename(names = watershed)) %>%
  drop_na(val2020) %>%
  mutate(total_roadsalt = rowSums(ME_roads_vs_loads[6:7])) %>%
  mutate(load20 = rowSums(ME_roads_vs_loads[2:3])) %>%
  mutate(perc20 = (load20/val2020) * 100)


#get percentage of total road salt applied to landscape that is seen flowing into mendota

total_riverload = sum((ME_roads_vs_loads %>% filter(names != "YI"))$load20) #6974.99 tonnes
total_saltload = sum((ME_roads_vs_loads %>% filter(names != "YI"))$val2020) + 2719.97 #11193.89

total_riverload/total_saltload #62.3%


YI_riverload = sum((ME_roads_vs_loads %>% filter(names == "YI"))$load20) #8116.41 tonnes
YI_saltload = sum((ME_roads_vs_loads %>% filter(names == "YI"))$val2020) + 2719.97 #12466.30 tonnes

YI_riverload/YI_saltload #65.1%






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
