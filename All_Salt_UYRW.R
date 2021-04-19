

##data from road salt scripts first in file
##and Pools.R
##and Charlie D.

##manually enter winter totals for 2019-2020 for City of Monona, City of Middleton, Town of Westport
#towns1920 <- 301 + 811 + 500
##manually enter winter totals for 2020-21 for City of Monona
#towns2021 <- 303
# 
#  Road_Salt_2019_2020 = (ys$winter1920_salt_outside_MadisonMg + (WINTER_SALT_TOTALS %>% filter(Year == "2019-20"))$Total_Salt_Mg + towns1920)* 0.60663 #Mg
#  Road_Salt_2020_2021 = (ys$winter2021_salt_outside_MadisonMg + (WINTER_SALT_TOTALS %>% filter(Year == "2020-21"))$Total_Salt_Mg + towns2021)* 0.60663 #Mg
#  pools2019_low = sum((Chloride_Mass_Load_SP %>% filter(year == 2019))$Low_end) / 1000 #Mg
#  pools2019_high = sum((Chloride_Mass_Load_SP %>% filter(year == 2019))$High_end) / 1000 #Mg
# 
#  Chloride_Sources <- data.frame(
#    #year = c("2019_2020", "2020_2021", "2019-2020", "2019-2020" , "2017"),
#    Source = c("2019-2020 Road Salt", "2020-2021 Road Salt", "Swimming Pools in Madison", "Swimming Pools in Madison", "2017 Agriculture Load"),
#    Val = c(Road_Salt_2019_2020, Road_Salt_2020_2021, pools2019_low, pools2019_high, 3750)
#  )
# 
# saveRDS(Chloride_Sources, "Data/Chloride_Sources_Table.rds")

Chloride_Sources <- read_rds("Data/Chloride_Sources_Table.rds")


ggplot(data = Chloride_Sources %>% filter(Source != "Swimming Pools in Madison"), mapping = aes(Source, Val)) +
  geom_bar(stat = "identity", fill = "#1C366B") +
  theme_minimal() +
  labs(x = "", y = "Chloride Mass (Mg)",
       caption = "Figure X. Mass of chloride in metric tonnes (Mg) entering the Upper Yahara River Watershed due to anthropogenic 
activities.") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/All_Cl_Mass_Anthro.png", height = 15, width = 20, units = "cm")
