library(tidyverse)
library(lubridate)
library(broom)
library(gt)
library(webshot)
source("Functions/L_theme.R")


#part1
find_all_events <- function(discharge_data, baseflow_data, fileName, cond_data) {

  threshold <- mean(discharge_data$runningmeandis)/2 #the difference between the the peak event discharge and the baseflow must be at least half the total average discharge. Adapted from Knapp et al., 2020
  
  
  df <- cbind(discharge_data, baseflow_data) %>%
    mutate(difference = runningmeandis - bt) %>%
    mutate(test = ifelse(difference > threshold, 0.25, NA)) #this just helps narrow down the dataset right away. 0.25 is a dummy variable
  
 #saving a plot to check for problems 
  ggplot(df) +
    geom_point(aes(date, runningmeandis)) +
    geom_point(aes(date, bt),color = "red") +
    geom_point(aes(date, test), color = "blue")
  ggsave(paste("Plots/QC_plots/", fileName, "/checkfolder/checkplot.png", sep = ""))

#combine discharge data with conductivity data
df1 <- df %>%
  left_join(cond_data, by = "date") %>%
  mutate(mon = months.POSIXt(date)) %>%
  mutate(year = year(date))

#specifying 1 day pre rain and 2 days post rain
precip <- precip %>%
  mutate(rain = ifelse(PRCP == 0, "N", "Y")) %>%
  mutate(keep = ifelse(lag(rain, n = 1L) == "Y" | lag(rain, n = 2L) == "Y" | PRCP > 0 | lead(rain) == "Y", 1, 0)) %>%
  filter(keep == 1)

#intermediary step
temporary <- df1 %>%
  select(date, test) %>%
  filter(test == 0.25) %>%
  mutate(date2 = as.Date(date)) %>%
  select(date2, test) %>%
  distinct()

#Noting which dates of data to keep (i.e., before and after a rain event and flow exceeds threshold)
precip2 <- precip %>%
  left_join(temporary, by = c("date" = "date2")) %>%
  mutate(keep2 = ifelse(lag(test, n = 1L) == 0.25 | lag(test, n = 2L) == 0.25 | lead(test, n = 2L) == 0.25 | lead(test, n = 1L) == 0.25 | test == 0.25, 1, 0)) %>%
  filter(keep2 == 1)


#conmbine discharge/cond with precip. Keep only day prior to rain event, during rain event, and day after rain event
df2 <- df1 %>%
  mutate(date2 = as.Date(date)) %>%
  left_join(precip2, by = c("date2" = "date")) %>%
  filter(keep2 == 1) %>%
  select(date, discharge, runningmeandis, runningmean, PRCP, mon, year, bt, qft, difference, date2, test.x)


#create groupings based on dates of events
dates_only <- df2 %>%
  select(date2) %>%
  unique()
dates <- as.vector(as.character(dates_only$date2))
dates1 <- sort(as.Date((dates)))
split(dates1, cumsum(c(TRUE, diff(dates1) != 1)))
dates2 <- data.frame(dates1, group = cumsum(c(TRUE, diff(dates1) != 1))) 

 #putting the date groups into the dataset
 date_group <- df2 %>%
   left_join(dates2, by = c("date2" = "dates1"))

 #save a plot to check for problems
 ggplot(date_group) +
   geom_point(aes(date, runningmeandis)) +
   geom_point(aes(date, bt),color = "red") +
   geom_point(aes(date, test.x), color = "blue") +
   facet_wrap("group", scales = "free")
 ggsave(paste("Plots/QC_plots/", fileName, "/checkfolder/flowevents.png", sep = ""), height = 12, width = 12, units = "in")

 #save a plot of conductivity. This and previous plot need to be checked to ensure we have both conductivity data and flow data at each event that we want to use
 ggplot(date_group) +
   geom_point(aes(date, runningmean)) +
  facet_wrap("group", scales = "free")
 ggsave(paste("Plots/QC_plots/", fileName, "/checkfolder/cond.png", sep = ""), height = 12, width = 12, units = "in")

 return(date_group)
 
}


#part2

QC_plots <- function(x,y, event_data, fileName, creekName, discharge_data, cond_data) {
  
  labels <- as.data.frame(x) %>%
    mutate(y = y)
  
  final <- event_data %>%
    left_join(labels, by = c("group" = "x"))
  
  
  ggplot(final %>% filter(group == x)) +
    geom_point(aes(runningmeandis, runningmean)) +
    geom_smooth(aes(runningmeandis, runningmean), method = "lm", se = FALSE, color = "#1DACE8") +
    labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         x = "\nDischarge"~(m^3~s^-1)) +
    L_theme() +
    facet_wrap("y", scales = "free")
  ggsave(paste("Plots/QC_plots/", fileName, "/regressions.png", sep = ""), height = 12, width = 12, units = "in")
  
  
  ggplot(final %>% filter(group == x)) +
    geom_line(aes(date, runningmean, color = "#F24D29")) +
    labs(x = "",
         y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n") +
    L_theme() + theme(legend.position = "none") +
    facet_wrap("y", scales = "free") 
  ggsave(paste("Plots/QC_plots/", fileName, "/conductivity.png", sep = ""), height = 12, width = 12, units = "in")
  
  
  ggplot(final %>% filter(group == x)) +
    geom_line(aes(date, runningmeandis, color = "Flow (cms)")) +
    geom_line(aes(date, bt, color = "Baseflow (cms)")) +
    facet_wrap("y", scales = "free") +
    labs(x = "", y = "\nDischarge"~(m^3~s^-1)) +
    L_theme() +
    scale_color_manual("",
                       labels = c("Baseflow", "Flow"),
                       values = c("#C4CFD0", "#1C366B")) 
  ggsave(paste("Plots/QC_plots/", fileName, "/discharge.png", sep = ""), height = 12, width = 12, units = "in")
  
  
  df <- event_data %>%
    #na.omit() %>%
    #filter(group == x)
    nest(data = -group) %>% 
    mutate(
      fit = map(data, ~ lm(runningmean~runningmeandis, data = .x)),
      tidied = map(fit, tidy),
      glanced = map(fit, glance),
      augmented = map(fit, augment)
    ) %>% 
    unnest(tidied)
  
  df1 <- df %>%
    select(group, term, estimate, glanced) 
  
  df2 <- df1 %>%
    unnest(glanced) %>%
    left_join(labels, by = c("group" = "x")) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    rename("slope" = "runningmeandis",
           "intercept" = "(Intercept)") %>%
    select(y, adj.r.squared, intercept, slope) %>%
    drop_na(y)
  
  
  gt_tbl <- gt(df2)
  table <- gt_tbl %>%
    cols_label(
      y = "Event",
      slope = "Slope",
      intercept = "Intercept",
      adj.r.squared = "R Squared"
    ) %>%
    tab_header(
      title = paste("Regression stats for", creekName, "events", sep = ""),
    ); table
  
  # whitespace can be set, zoom sets resolution
  gtsave(data = table, paste("Plots/QC_plots/", fileName, "/stats.png", sep = ""), expand = 10, zoom = 10)
  
  
  library(data.table)
  qsc <- join_datasets_cond(discharge_data, cond_data)
  
  ggplot() +
    geom_point(qsc, mapping = aes(runningmeandis, runningmean), color = "#C4CFD0") +
    geom_smooth(qsc, mapping = aes(runningmeandis, runningmean), method = "lm", se = FALSE, color = "grey") +
    labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
         x = "\nDischarge"~(m^3~s^-1)) +
    L_theme() +
    geom_jitter(width = 0.5, size = 1) +
    geom_smooth(final %>% drop_na(y), mapping = aes(runningmeandis, runningmean, color = y), method = "lm", se = FALSE) +
    scale_color_viridis_d()
  ggsave(paste("Plots/QC_plots/", fileName, "/events_vs_alldata.png", sep = ""), height = 12, width = 12, units = "in")
}
