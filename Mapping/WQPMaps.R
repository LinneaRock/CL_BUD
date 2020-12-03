library(tidyverse)
library(urbnmapr)
library(lubridate)
library(sf)
source("Functions/statemap.R")

#read in and manipulate dataset to be useable
wicl <- read.csv("Data/Historical_External/WQP.csv") %>%
  mutate(char = as.character(Date),
         Date = as.Date(char)) %>%
  mutate(county_fips = CountyCode + 55000) %>%
  mutate(Result.2 = gsub("\\*","", as.character(Result))) %>%
  mutate(result2 = as.numeric(Result.2)) %>%
  mutate(unit = "mg/L") %>%
  mutate(result3 = ifelse((as.character(ResultUnit)) == "ueq/L", result2 * .001 * 35.5, result2)) %>% #converting all units to mg/L
  mutate(result4 = ifelse((as.character(ResultUnit)) == "ug/l", result2 * .001, result2)) %>% #converting all units to mg/L
  filter(result4 >= 0) %>% #omit results less than or equal to 0
  na.omit()

#filtering for surface water only
SW <- wicl %>%
  filter(LocationType == "Lake" | LocationType == "Pond-Stormwater" | LocationType == "Lake, Reservoir, Impoundment" | LocationType == "Other-Surface Water" | LocationType == "Reservoir" | LocationType == "River/Stream" | LocationType == "River/Stream Perennial" | LocationType == "Riverine Impoundment" | LocationType == "Stream" | LocationType == "Stream: Canal" | LocationType == "Stream: Ditch")

#using urbnmapr to get state/county shape
counties <- st_read("C:/Users/linne/Downloads/County_Boundaries_24K-shp/County_Boundaries_24K.shp")
  filter(state_abbv == "WI") %>%
  mutate(county_fips = as.numeric(county_fips))
  
counties <-  get_urbn_map("counties", sf = TRUE) %>%
  filter(state_abbv == "WI") %>%
  mutate(county_fips = as.numeric(county_fips))
  
  
  
breaks <- SW %>% #creating categories for legend
  mutate(br = ifelse(result4 <= 1, 1, NA),
         br = ifelse(result4 > 1 & result4 <= 10, 2, br),
         br = ifelse(result4 > 10 & result4 <= 100, 3, br),
         br = ifelse(result4 > 100 & result4 <= 1000, 4, br),
         br = ifelse(result4 > 1000, 5, br),
         br = as.character(br)) %>%
  mutate(year = year(Date))

#historical river/lake data from 1960-1990
breaks6090 <- breaks %>%
  filter(year < 1990)

#historical river/lake data from 1990-2010
breaks902010 <- breaks %>%
  filter(year >= 1990 & year < 2010)

#historical river/lake data from 2010-2020
breaks1020 <- breaks %>%
  filter(year >= 2010)

breaks_as_sf <- st_as_sf(breaks, coords = c("Long", "Lat"), remove = FALSE, 
                         crs = 4326, agr = "constant")


#Map of all historical river/lake data
statemap2(breaks_as_sf, "1960-2020")

#Map of all historical river/lake data from 1960-1990
statemap(breaks6090, "1960-1990")

#Map of all historical river/lake data from 1990-2010
statemap(breaks902010, "1990-2010")

#Map of all historical river/lake data from 2010-2020
statemap(breaks1020, "2010-2020")




#plot of historical data
ggplot(SW, aes(Date, result4)) +
  geom_point(color = "#1C366B") +
  # geom_smooth(method = "lm", color = "#F24D29") +
  theme(axis.text = element_text(size =11),
        axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)~"\n",
       caption = "Figure by Linnea Rock using data from waterqualitydata.us") 
