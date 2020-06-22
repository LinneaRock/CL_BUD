library(tidyverse)
library(lubridate)
library(readxl)


#function to convert tons and gallons to metric tonnes and liters
format_salt <- function(original) {
  df <- read_xlsx(original, sheet = "SHIFT TOTALS") %>%
    mutate(DATE = as.Date(as.character(DATE))) %>%
    filter(DATE < "2020-03-04") %>% #excel sheet had a lot of extra rows with no information
    mutate(Total_Salt_Mg = `TOTAL SALT TONS` * 0.907185,
           SALT_Mg = SALT_ton * 0.907185,
           SAND_Mg = SAND_ton * 0.907185,
           BRINE_l = BRINE_gal * 3.785411784) %>%
    dplyr::select(Total_Salt_Mg, DATE, SHIFT, TRUCK, ROUTE, SALT_load, SAND_load, BRINE_l, SALT_Mg, SAND_Mg)
    
}


E19 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2019.xlsx")


E19_perdate <- E19 %>%
  group_by(DATE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))

sum(E19_perdate$total_ton)

W19 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2019.xlsx")


W19_perdate <- W19 %>%
  group_by(DATE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))

sum(W19_perdate$total_ton) + sum(E19_perdate$total_ton)

Winter19 <- left_join(E19_perdate, W19_perdate, by = "DATE")
Winter19[is.na(Winter19)] <- 0
Winter19 <- Winter19 %>%
  rename(total_salt_EAST = total_ton.x,
         total_salt_WEST = total_ton.y) %>%
  mutate(total_salt = (total_salt_EAST + total_salt_WEST))


