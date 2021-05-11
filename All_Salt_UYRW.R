

##data from road salt scripts first in file
##and Pools.R
##and Charlie D.

##manually enter winter totals for 2019-2020 for City of Monona, City of Middleton
towns1920 <- (301 + 811) * 0.907185
##manually enter winter totals for 2020-21 for City of Monona, City of Middleton
towns2021 <- (303 + 933) * 0.907185
##manually enter uw estimate by WRM
uw1819 <- 923.623 * 0.907185
#Ag load from Charlie's estimates
cattle = 1758.574891
#cropshigh = 47.35071595 + 177.3545567 + 467.7263132 + 1403.17894 
#cropslow =  8.163916543 + 41.73048393 + 73.85152314 + 295.4060926
Wheat <- c(47.35071595, 42.45236602, 29.39009955, 14.69504978, 8.163916543)
Soybeans <- c(177.3545567, 161.7056252, 130.4077623, 83.46096786, 41.73048393) 
#Corn_grain <- c(467.7263132, 393.8747901, 174.2622216, 123.0858719, 73.85152314) 
Corn <- c(1403.17894, 1329.327417, 1132.390021, 566.1950107, 295.4060926)
kcontent_soil <- c("very low", "low", "optimum", "high", "very high")

crops <- data.frame(kcontent_soil = kcontent_soil,
                    Wheat = Wheat,
                    Soybeans = Soybeans,
                    Corn = Corn)
                    #Corn_grain = Corn_grain,
                    #Corn_silage = Corn_silage)

crops <- crops %>%
  pivot_longer(2:4, names_to = "croptype", values_to = "tonnes") 

crops_sums <- crops %>%
  group_by(kcontent_soil) %>%
  summarise(total = sum(tonnes))


  Road_Salt_2019_2020 = (ys$winter1920_salt_outside_MadisonMg + (WINTER_SALT_TOTALS %>% filter(Year == "2019-20"))$Total_Salt_Mg + towns1920 + uw1819)* 0.60663 #Mg chloride
  Road_Salt_2020_2021 = (ys$winter2021_salt_outside_MadisonMg + (WINTER_SALT_TOTALS %>% filter(Year == "2020-21"))$Total_Salt_Mg + towns2021 + uw1819)* 0.60663 #Mg chloride
  University_201819RoadSalt = uw1819 * 0.60663 #Mg chloride
  pools2019_low = sum((Chloride_Mass_Load_SP %>% filter(year == 2019))$Low_end) / 1000 #Mg
  pools2019_high = sum((Chloride_Mass_Load_SP %>% filter(year == 2019))$High_end) / 1000 #Mg
# 
  Chloride_Sources <- data.frame(
    #year = c("2019_2020", "2020_2021", "2019-2020", "2019-2020" , "2017"),
    Source = c("2019-2020 Road Salt", "2020-2021 Road Salt", "Swimming Pools in Madison", "Swimming Pools in Madison", "2017 Agricultural Load"),
    Val = c(Road_Salt_2019_2020, Road_Salt_2020_2021, pools2019_low, pools2019_high, cattle + 1466.4501)
  )
# 
 saveRDS(Chloride_Sources, "Data/Chloride_Sources_Table.rds")

Chloride_Sources <- read_rds("Data/Chloride_Sources_Table.rds")


ggplot(data = Chloride_Sources %>% filter(Source != "Swimming Pools in Madison"), mapping = aes(Source, Val)) +
  geom_bar(stat = "identity") + #, fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Chloride Mass (Mg)",
       caption = "Figure X. Mass of chloride in metric tonnes (Mg) entering the Upper Yahara River Watershed due to anthropogenic 
activities.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/All_Cl_Mass_Anthro.png", height = 15, width = 20, units = "cm")
#ggsave("Plots/figsforpres/anthrosource.png", height = 15, width = 20, units = "cm")



#plot of range of chloride from potash based on soil k content

ggplot(crops) +
  geom_bar(aes(fill = croptype, reorder(kcontent_soil, tonnes), tonnes), position = "stack", stat = "identity") +
  #scale_fill_manual(values = wes_palette("Darjeeling1", n = 4, type = "continuous")) +
  scale_fill_manual(labels = c("Corn", "Soybeans", "Wheat"),
                     values = c("#F24D29", "#E5C4A1", "#C4CFD0")) +
  L_theme() +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Soil potassium content",
       y = "Chloride Mass (Mg)",
       caption = "Figure X. Range in chloride load in metric tonnes (Mg) from using potash (KCl) to fertilize 
crops. Estimates based on the three major crop types and the range is based on 
recommended application rates for a range of potassium content in the soil.")

ggsave("Plots/crops.png", height = 4.25, width = 6.25, units = "in")


#road salt by location
road_salt_chloride_locations <- data.frame(
  Road_Salt_2019_2020 = c(ys$winter1920_salt_outside_MadisonMg, (WINTER_SALT_TOTALS %>% filter(Year == "2019-20"))$Total_Salt_Mg, towns1920, uw1819), 
  Road_Salt_2020_2021 = c(ys$winter2021_salt_outside_MadisonMg, (WINTER_SALT_TOTALS %>% filter(Year == "2020-21"))$Total_Salt_Mg, towns2021, uw1819), 
  Location = c("County Roads", "Madison", "Middleton and Monona", "UW Campus (2018)")
)

road_salt_chloride_locations <- road_salt_chloride_locations %>%
  mutate(Road_Salt_2019_2020 = Road_Salt_2019_2020 *0.60663) %>%
  mutate(Road_Salt_2020_2021 = Road_Salt_2020_2021 *0.60663) %>% #salt to chloride
  pivot_longer(1:2, names_to = "year", values_to = "tonnes_chloride")

road_salt_chloride_locations <- road_salt_chloride_locations %>%
  mutate(year = ifelse(year == "Road_Salt_2019_2020", "Winter 2019-2020", "Winter 2020-2021"))
  
ggplot(road_salt_chloride_locations) +
  geom_bar(aes(fill = Location, year, tonnes_chloride), position = "stack", stat = "identity") +
  scale_fill_manual(labels = c("County Roads", "Madison", "Middleton and Monona", "UW Campus (2018)"),
                    values = c("#F24D29", "#E5C4A1", "#C4CFD0", "#1DACE8")) +
  L_theme() +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "",
       y = "Chloride Mass (Mg)",
       caption = "Figure X. Mass of chloride in metric tonnes (Mg) from road salt application for the two 
study winters in different areas of the watershed.")

ggsave("Plots/road_salt_by_location.png", height = 4.25, width = 6.25, units = "in")

sum((road_salt_chloride_locations %>% filter(year == "Winter 2019-2020"))$tonnes_chloride)
