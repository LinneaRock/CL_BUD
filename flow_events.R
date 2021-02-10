library(tidyverse)



library(EcoHydRology)

baseflow_DC <- BaseflowSeparation(DC_discharge$discharge, filter_parameter = 0.925, passes = 3) %>%
  mutate(obs = 1:n())

ggplot(baseflow_DC) + geom_line(aes(x = obs, y = bt)) + geom_line(aes(x = obs, y = qft), color = "red")

hydrograph(input = DC_discharge, streamflow2 = baseflow_DC[,1])


??EcoHydRology::BaseflowSeparation

detach(package:EcoHydRology)


event_DC <- cbind(DC_discharge, baseflow_DC) 



event_DC2 <- event_DC %>%
  mutate(diff = discharge - bt) %>%
  mutate(diff = diff * 1000) %>% 
  mutate(test = ifelse(diff > 200, 0.25, NA))



ggplot(event_DC2) +
  geom_point(aes(date, discharge)) +
  geom_point(aes(date, bt),color = "red") +
  geom_point(aes(date, test), color = "blue")

mean(event_DC$discharge) * 1000

#combine discharge and conductivity
DC_event_cond <- event_DC2 %>%
  left_join(DC_cond_data, by = "date") %>%
  mutate(mon = months.POSIXt(date)) %>%
  mutate(year = year(date)) 


#prepare precip data to only have dates before, after, and including precip events
precip <- precip_temp_data %>%
  mutate(rain = ifelse(PRCP == 0, "N", "Y")) %>%
  mutate(keep = ifelse(lag(rain, n = 1L) == "Y" | lag(rain, n = 2L) == "Y" | PRCP > 0 | lead(rain) == "Y", 1, 0)) %>%
  filter(keep == 1)

testonly <- DC_event_cond %>%
  select(date, test) %>%
  filter(test == 0.25) %>%
  mutate(date2 = as.Date(date)) %>%
  select(date2, test) %>%
  distinct()

precip2 <- precip %>%
  left_join(testonly, by = c("date" = "date2")) %>%
  mutate(keep2 = ifelse(lag(test, n = 1L) == 0.25 | lag(test, n = 2L) == 0.25 | lead(test, n = 2L) == 0.25 | lead(test, n = 1L) == 0.25 | test == 0.25, 1, 0)) %>%
  filter(keep2 == 1)

#conmbine discharge/cond with precip. Keep only day prior to rain event, during rain event, and day after rain event
DC_event_cond <- DC_event_cond %>%
  mutate(date2 = as.Date(date)) %>%
  left_join(precip2, by = c("date2" = "date")) 

DC_event_cond2 <- DC_event_cond %>%
  filter(keep2 == 1) %>%
  select(date, discharge, runningmeandis, runningmean, PRCP, mon, year, bt, qft, diff, date2, test.x)

DC_next <- DC_event_cond %>%
  mutate(minus1 = date2 - 86400) %>%
  mutate(plus1 = date2 + 86400) %>%
  mutate(plus2 = date2 + 86400 + 86400) %>%
  mutate(event = ifelse(is.na(test), "N", "Y")) %>%
  mutate(pre_event = ifelse(event == "Y" & 
                              date2 == minus1, "Y", "N")) %>%
  select(date, discharge, runningmeandis, runningmean, PRCP, mon, year, bt, qft, diff, date2, test, minus1, plus1, plus2, event, pre_event)



dates <- as.vector(as.character(date_group$date2))
dates1 <- sort(as.Date((dates)))
split(dates1, cumsum(c(TRUE, diff(dates1) != 1)))
df1 <- data.frame(dates1, group = cumsum(c(TRUE, diff(dates1) != 1))) 


date_group <- DC_event_cond2 %>%
  left_join(df1, by = c("date2" = "dates1"))


ggplot(date_group) +
  geom_point(aes(date, runningmeandis)) +
  geom_point(aes(date, bt),color = "red") +
  geom_point(aes(date, test.x), color = "blue") +
  facet_wrap("group", scales = "free")


mean(DC_discharge$runningmeandis) #0.4644761
median(DC_discharge$runningmeandis) #0.3859804





flow_prcp <- DC_discharge %>%
  separate(date, c("date", "time"), sep = " ") %>%
  group_by(date) %>%
  mutate(meandis = mean(discharge)) %>%
  select(date, meandis) %>%
  unique() %>%
  ungroup() %>%
  left_join(precip_temp_data, by = c("date" = "DATE"))


ggplot(flow_prcp) + geom_point(aes(PRCP, meandis))





























