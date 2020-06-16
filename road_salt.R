library(tidyverse)
library(lubridate)
library(readxl)

format_salt <- function(original) {
  S <- read_xlsx(original, sheet = "SHIFT TOTALS") %>%
    mutate(DATE = as.Date(as.character(DATE))) %>%
    filter(DATE < "2020-03-04") #excel sheet had a lot of extra rows with no information
}

E19 <- read_xlsx("Data/Road_Salt/Madison/MaterialUseTrackingEast2019.xlsx", sheet = "SHIFT TOTALS") %>%
  mutate(DATE = as.Date(as.character(DATE))) %>%
  filter(DATE < "2020-03-04") #excel sheet had a lot of extra rows with no information


E19_perdate <- E19 %>%
  group_by(DATE) %>%
  summarise(total_ton = sum(`TOTAL SALT TONS`))

sum(E19_perdate$total_ton)

W19 <- read_xlsx("Data/Road_Salt/Madison/MaterialUseTrackingWest2019.xlsx", sheet = "SHIFT TOTALS") %>%
  mutate(DATE = as.Date(as.character(DATE))) %>%
  filter(DATE < "2020-03-04") #excel sheet had a lot of extra rows with no information

W19_perdate <- W19 %>%
  group_by(DATE) %>%
  summarise(total_ton = sum(`TOTAL SALT TONS`))

sum(W19_perdate$total_ton) + sum(E19_perdate$total_ton)

Winter19 <- left_join(E19_perdate, W19_perdate, by = "DATE")
Winter19[is.na(Winter19)] <- 0
Winter19 <- Winter19 %>%
  rename(total_salt_EAST = total_ton.x,
         total_salt_WEST = total_ton.y) %>%
  mutate(total_salt = (total_salt_EAST + total_salt_WEST) / 1.1023113109244) #combine and convert to tonnes

ggplot(Winter19 %>%
         filter(DATE < "2020-02-01" & DATE > "2019-12-10"), aes(DATE, total_salt)) +
  geom_line()
