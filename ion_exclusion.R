library(tidyverse)
library(lubridate)
source("Functions/L_theme.R")


#code example from Hilary::

#Here’s an example for Mendota.
area = 39610000 #m2
# say the ice is 27 cm thick
ice = 39610000 * 0.27 * 1000 #L
# Say concentation at the surface is 50 mg/L
conc.ice = ice * 50 # mg
rejected = conc.ice * 0.95
#This is the amount rejected in mg
volume.mendota = 39610000 * 12.5 *1000 #L
#If all the ions rejected from the ice were mixed with the water column, this is how much it would increase in concentation
total.increase = rejected / (volume.mendota -ice)
#~ 1 mg/L
#But what if it all went to the bottom 5 m (here I just took the bottom 5 hypso factors for mendota)
rejected / ((0.0120+0.0076+0.0042+0.0016+0.0002) * volume.mendota)
#So this would be about a 40 mg/L increase



#########################################################################################
#########################################################################################


#joining datasets####
#load and format the datasets
ions <- read_csv("Data/LTER_ions.csv") %>% #all ion data from LTER
  filter(lakeid == "MO" | lakeid == "ME") %>%
  mutate(depth = replace(depth, depth == 0, 0.25)) %>% 
  mutate(depth = replace(depth, depth == 21.9, 22)) #rounding depths to nearest depth with hp_factor to ensure dall data is included
  
hypso <- read_csv("Data/lake_hypsometry.csv") #hypsometry factor to multiply by total lake volume

icedepth <- read_csv("Data/winter.csv") %>% #depth of ice
  rename(icedate = sampledate) %>%  #icedate refers to the date the ice depth was measured
  group_by(lakeid, year4) %>%
  mutate(ave_icedepth = mean(totice)) %>% #the ice thickness is measured over a few different days 
  select(lakeid, year4, icedate, ave_icedepth) %>%
  distinct(lakeid, year4, ave_icedepth)

  
icedates <- read_csv("Data/icedates.csv") %>% #ice on and off dates
  filter(lakeid == "MO" | lakeid == "ME") %>%
  mutate(ice_on = mdy(ice_on),
         ice_off = mdy(ice_off),
         year4 = year4 + 1)

#joining all datasets together
combined_data <- ions %>%
  left_join(hypso, by = c("lakeid", "depth")) %>%
  left_join(icedates, by = c("lakeid", "year4")) %>%
  left_join(icedepth, by = c("lakeid", "year4"))


#mass calculations####
mass <- combined_data %>%
  drop_na(cl) %>%
  filter(cl > 0) %>%
  mutate(vol_cubicmeters = ifelse(lakeid == "ME", hp_factor * 506880000, hp_factor * 111520000)) %>% #hypsometrically weighting lake volumes [m^-3]
  mutate(vol_liters = vol_cubicmeters * 1000) %>% #converting cubic meters to liters
  mutate(cl_mass = cl * vol_liters * 0.000000001,
         so4_mass = so4 * vol_liters * 0.000000001,
         ca_mass = ca * vol_liters * 0.000000001,
         mg_mass = mg * vol_liters * 0.000000001,
         na_mass = na * vol_liters * 0.000000001,
         k_mass = k * vol_liters * 0.000000001,
         fe_mass = fe * vol_liters * 0.000000001,
         mn_mass = mn * vol_liters * 0.000000001) %>% #converting concentration [mg L^-1] to mass in tonnes [Mg]
  rowwise() %>%
  mutate(winter = ifelse(between(sampledate, ice_on, ice_off), "Y", "N")) %>% #to easily distinguish ice on vs. ice off data
  mutate_if(is.numeric, ~replace_na(., 0)) %>% #converting NAs to 0 for the next step
  mutate(All_ions = sum(cl_mass + so4_mass + ca_mass + mg_mass + na_mass + k_mass + fe_mass + mn_mass)) %>%
  rename(iondate = sampledate,
         iondepth = depth) 

#just a couple of visuals 
#Cl mass over time 
ggplot(mass %>% filter(lakeid == "MO")) +
  geom_point(aes(iondate, cl_mass, color = iondepth, shape = winter)) +
  scale_color_viridis_c(direction = -1) 

#Cl concentration over time
ggplot(mass %>% filter(lakeid == "MO"))+
  geom_point(aes(iondate, cl, shape = winter, color = iondepth)) +
  scale_color_viridis_c(direction = -1) 


#Ion exclusion calculations####
#The ion exclusion calculations below only account for .25m of ice or less because data is not collected between .25 and 4m


#Mendota ion exclusion

surface_mass_ME <- mass %>%
  filter(lakeid == "ME",
         iondepth <= 4, #Only 0.25m will be appropriate to calculate the ion exclusion becuase data is collected at 0.25, 4, 8, etc.
         winter == "N") %>% #Selecting the salt mass when the ice is off
  select(year4, iondate, iondepth, cl_mass, ave_icedepth) %>%
  group_by(year4, iondepth) %>%
  mutate(ave_clmass = mean(cl_mass)) %>% #getting the average chloride mass for the year based on ~2 sampling days (ice off)
  distinct(year4, iondepth, ave_clmass, ave_icedepth) %>%
  mutate(iondepth = iondepth * 100)  # converting depth in m to cm


ME_Salt_Exclusion <- surface_mass_ME %>%
  filter(iondepth == 25) %>%
  mutate(exclusion_mass = ifelse(iondepth <= ave_icedepth, 0.95 * ave_clmass, (ave_icedepth / iondepth) * ave_clmass * .95)) %>% #salt mass excluded from ice. If ice thickness is less than 25cm, I took the proportion of the layer that is ice and assuming that the ions are evenly distributed, calculated the amount of ions then took 95% of that 
  mutate(ice_label = ifelse(25 < ave_icedepth, ">25cm", NA)) %>%
  mutate(ice_label = ifelse(25 > ave_icedepth, "<25cm", ice_label)) %>%
  mutate(ice_label = ifelse(ave_icedepth == 0, "No ice", ice_label)) %>%
  mutate(ice_label = ifelse(25 == ave_icedepth, "=25cm", ice_label))
  

ggplot(ME_Salt_Exclusion, aes(year4, exclusion_mass)) +
  geom_bar(stat = "identity", fill = "#1C366B", position = "dodge") +
  L_theme() +
  labs(x = "",
       y = "Chloride Mass (Mg)") +
  geom_text(aes(label = ice_label), position = position_dodge(width = 0.9), vjust = -0.25)

#Monona ion exclusion

surface_mass_MO <- mass %>%
  filter(lakeid == "MO",
         iondepth <= 4, #Only 0.25m will be appropriate to calculate the ion exclusion becuase data is collected at 0.25, 4, 8, etc.
         winter == "N") %>% #Selecting the salt mass when the ice is off
  select(year4, iondate, iondepth, cl_mass, ave_icedepth) %>%
  group_by(year4, iondepth) %>%
  mutate(ave_clmass = mean(cl_mass)) %>% #getting the average chloride mass for the year based on ~2 sampling days (ice off)
  distinct(year4, iondepth, ave_clmass, ave_icedepth) %>%
  mutate(iondepth = iondepth * 100)  # converting depth in m to cm


MO_Salt_Exclusion <- surface_mass_MO %>%
  filter(iondepth == 25) %>%
  mutate(exclusion_mass = ifelse(iondepth <= ave_icedepth, 0.95 * ave_clmass, (ave_icedepth / iondepth) * ave_clmass * .95)) %>% #salt mass excluded from ice. If ice thickness is less than 25cm, I took the proportion of the layer that is ice and assuming that the ions are evenly distributed, calculated the amount of ions then took 95% of that 
  mutate(ice_label = ifelse(25 < ave_icedepth, ">25cm", NA)) %>%
  mutate(ice_label = ifelse(25 > ave_icedepth, "<25cm", ice_label)) %>%
  mutate(ice_label = ifelse(ave_icedepth == 0, "No ice", ice_label)) %>%
  mutate(ice_label = ifelse(25 == ave_icedepth, "=25cm", ice_label))

ggplot(MO_Salt_Exclusion, aes(year4, exclusion_mass)) +
  geom_bar(stat = "identity", fill = "#1C366B", position = "dodge") +
  L_theme() +
  labs(x = "",
       y = "Chloride Mass (Mg)") +
  geom_text(aes(label = ice_label), position = position_dodge(width = 0.9), vjust = -0.25)

