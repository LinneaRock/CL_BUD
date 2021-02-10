library(tidyverse)
library(lubridate)


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


mean(DC_discharge$runningmeandis) * 2 #0.4644761
median(DC_discharge$runningmeandis) #0.3859804


check_max <- date_group %>%
  group_by(group) %>%
  mutate(peak = max(runningmeandis)) %>%
  ungroup() %>%
  mutate(is.greater = ifelse(peak >= (2 * mean(DC_discharge$runningmeandis)), "keep", "don't")) %>%
  filter(is.greater == "keep")


ggplot(check_max %>% filter(group == c(1, 6, 8, 18, 23, 25, 26))) +
  geom_point(aes(date, runningmeandis)) +
  geom_point(aes(date, bt),color = "red") +
  #geom_point(aes(date, test.x), color = "blue") +
  facet_wrap("group", scales = "free")




x <- c(1, 4:6, 8, 10, 12:14, 16:18, 23, 26, 27)
y <- LETTERS[1:15]

labels <- as.data.frame(x) %>%
  mutate(y = y)

#function to obtain coefficient information from regression
info <- function() {
  
  info <- lm(runningmean ~ runningmeandis, date_group)

 
  coef(info())[2]

}

summary(lm(runningmean~runningmeandis, date_group))$adj.r.squared

date_group <- date_group %>%
  left_join(labels, by = c("group" = "x"))
  #mutate(slope = lapply(split(date_group, group), lm(runningmean ~ runningmeandis)))


for(i in (1:length(date_group$group))) {
  
  test <- date_group %>%
    mutate(slope = coef(lm(runningmean~runningmeandis))[2]) %>%
    mutate(r.sq = summary(lm(runningmean~runningmeandis))$adj.r.squared)
  
}
  


 ggplot(date_group %>% filter(group == c(1, 4:6, 8, 10, 12:14, 16:18, 23, 26, 27))) +
  geom_point(aes(runningmeandis, runningmean)) +
  geom_smooth(aes(runningmeandis, runningmean), method = "lm", se = FALSE, color = "#1DACE8") +
  labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       x = "\nDischarge"~(m^3~s^-1)) +
  L_theme() +
  facet_wrap("y", scales = "free")



ggplot(date_group %>% filter(group == c(1, 4:6, 8, 10, 12:14, 16:18, 23, 26, 27))) +
  geom_line(aes(date, runningmean, color = "#F24D29")) +
  labs(x = "",
       y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n") +
  L_theme() + theme(legend.position = "none") +
  facet_wrap("y", scales = "free") 



ggplot(date_group %>% filter(group == c(1, 4:6, 8, 10, 12:14, 16:18, 23, 26, 27))) +
  geom_line(aes(date, runningmeandis, color = "Flow (cms)")) +
  geom_line(aes(date, bt, color = "Baseflow (cms)")) +
  facet_wrap("y", scales = "free") +
  labs(x = "", y = "\nDischarge"~(m^3~s^-1)) +
  L_theme() +
  scale_color_manual("",
                     labels = c("Baseflow", "Flow"),
                     values = c("#C4CFD0", "#1C366B")) 





x <- c(1, 4:6, 8, 10, 12:14, 16:18, 23, 26, 27)
y <- LETTERS[1:15]

labels <- as.data.frame(x) %>%
  mutate(y = y)

date_group <- date_group %>%
  left_join(labels, by = c("group" = "x"))





df_subs <- lapply(unique(date_group$group), function(i){
  date_group[date_group$group ==  i,] %>%
    mutate(slope = coef(lm(runningmean~runningmeandis))[2]) %>%
    mutate(r.sq = summary(lm(runningmean~runningmeandis))$adj.r.squared)
}) 





