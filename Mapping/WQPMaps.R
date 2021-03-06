library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(sf)
library(viridisLite)
library(urbnmapr)
library(ggspatial)
library(raster)

#read in site information
sites <- whatWQPsites(statecode = "US:55",
             siteType = c("Lake%2C%20Reservoir%2C%20Impoundment", "Stream"),
             characteristicName = 'Chloride')

#filtering sites
sites2 <- sites %>%
  dplyr::select(MonitoringLocationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure, HUCEightDigitCode, DrainageAreaMeasure.MeasureValue, DrainageAreaMeasure.MeasureUnitCode)

#read in data
data <- readWQPdata(statecode = "US:55",
                    siteType = c("Lake%2C%20Reservoir%2C%20Impoundment", "Stream"),
                    characteristicName = 'Chloride')

#filtering data
data1 <- data %>%
  dplyr::select(OrganizationIdentifier, ActivityMediaName, ActivityMediaSubdivisionName, ActivityStartDate, MonitoringLocationIdentifier, ResultMeasureValue, ResultStatusIdentifier, ResultMeasure.MeasureUnitCode, ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode) %>%
  filter(ActivityMediaName == "Water") %>%
  dplyr::select(-ActivityMediaName) %>%
  rename(Units = ResultMeasure.MeasureUnitCode) %>%
  rename(Result = ResultMeasureValue) %>%
  mutate(Chloride = as.numeric(Result)) %>%
  filter(Chloride > 0)



# Make sure units are the same from HD's github
replaceUnits <- function(unit,scalingfactor){
  indx = which(data1$Units== unit)
  data1$Chloride[indx] = data1$Chloride[indx] * scalingfactor
  data1$Units[indx] = 'mg/l'
  return(data1)
}
data1 = replaceUnits('ug/l',scalingfactor = 1/1000)
data1 = replaceUnits('ug/l      ',scalingfactor = 1/1000)
data1 = replaceUnits('umol      ',scalingfactor = 35.45/1000)
data1 = replaceUnits('umol/L',scalingfactor = 35.45/1000)
data1 = replaceUnits('ueq/L',scalingfactor = 35.45/1000)
data1 = replaceUnits('ueq/L     ',scalingfactor = 35.45/1000)
data1 = replaceUnits('mg/L',scalingfactor = 1)
data1 = replaceUnits('mg/l      ',scalingfactor = 1)
data1 = replaceUnits('mg/l      ',scalingfactor = 1)
data1 = replaceUnits('mg/kg',scalingfactor = 1)
data1 = replaceUnits('mg/kg     ',scalingfactor = 1)
data1 = replaceUnits('ppm',scalingfactor = 1)
data1 = replaceUnits('ppm       ',scalingfactor = 1)
data1 = replaceUnits('mg/g',scalingfactor = 1000)
data1 = replaceUnits('mg/g      ',scalingfactor = 1000)

# Delete rows with no units from HD's github
data2 <- data1 %>% dplyr::filter(Units == 'mg/l') %>%
  dplyr::filter(!is.na(Chloride)) %>%
  left_join(sites2, by = 'MonitoringLocationIdentifier') %>%
  mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='River/Stream', 'Stream',MonitoringLocationTypeName)) %>%
  mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='River/Stream Perennial', 'Stream',MonitoringLocationTypeName)) %>%
  mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='River/Stream Intermittent', 'Stream',MonitoringLocationTypeName)) %>%
  # mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='Well: Collector or Ranney type well', 'Well',MonitoringLocationTypeName)) %>%
  mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='Lake, Reservoir, Impoundment', 'Lake',MonitoringLocationTypeName)) 

data3 <- data2 %>%
  filter(ResultStatusIdentifier == 'Accepted' | ResultStatusIdentifier =='Final') %>%
  filter(MonitoringLocationTypeName == "Stream" | MonitoringLocationTypeName == "Lake") 

wi_cl <- data3 %>%
  filter(LatitudeMeasure > 0) %>% #get rid of some data without geo
  mutate(LongitudeMeasure = ifelse(LongitudeMeasure > 0, -1 * LongitudeMeasure, LongitudeMeasure)) %>% #any longs that are not negative need to be
  filter(LongitudeMeasure < -86.99518) %>% #there a few points out in Lake Michigan that I do not want included
  mutate(group = case_when(Chloride <= 10 ~ '< 10',
                           Chloride > 10 & Chloride <= 100 ~ '10-100',
                           Chloride > 100 & Chloride <= 250 ~ '100-250',
                           Chloride > 250 & Chloride <= 395 ~ '250-395',
                           Chloride > 395 & Chloride <= 757 ~ '395-757',
                           Chloride > 757 ~ '> 757')) %>% 
  mutate(group = factor(group, levels =  c('< 10', '10-100', '100-250', '250-395', '395-757', '> 757')))

wi_cl_2000s <- wi_cl %>%
  mutate(year = year(ActivityStartDate)) %>% 
  filter(year >= 2000)


wi_cl_sf <- wi_cl_2000s %>%
  st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)

st_crs(wi_cl_sf)

counties <- get_urbn_map("counties", sf = TRUE) %>%
  filter(state_abbv == "WI") %>%
  mutate(county_fips = as.numeric(county_fips))



ggplot(gage.bb.sf) + # I don't know why but R aborts if I run this without this simple sf feature in here. This feature is created in SamplingSiteMap.R
  geom_sf(counties, mapping = aes(), fill = "#f7f7f7", color = "#969696") +
  geom_sf(wi_cl_sf, mapping = aes(color = group)) +
  coord_sf(crs = st_crs(4326)) +
  scale_color_viridis_d(option = "inferno", name = "Chloride Concentration"~(mg~L^-1)) +
  labs(caption = "Figure 3. Chloride concentrations (mg/L) in Wisconsin lakes and rivers from 2000-2021. Less 
than 10 mg/L indicates these are likely unaffected by anthropogenic chloride. 250 mg/L is the
taste threshold, 395 mg/L is Wisconsin's chronic toxicity threshold, and 757 mg/L is
Wisconsin's acute toxicity threshold. Data from waterqualitydata.us.") +
  theme(legend.title = element_text(size =10),
        panel.background = element_blank(), #element_rect(fill = "white", colour = "white"),
        plot.caption = element_text(size = 10, hjust = 0)) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
        
ggsave("Plots/statemap.png", height = 4.25, width = 6.25, units = "in")

# library(wesanderson)
# zis <- wes_palette(6, name = "Zissou1", type = "continuous")
# zis1 <- wes_palette(10, name = "Zissou1", type = "continuous")

