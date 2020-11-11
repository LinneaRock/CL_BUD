#library(tsoutliers)
library(tidyverse)
library(anomalize)
library(zoo)











YN_runmean <- loggerYN %>%
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA)) %>%
  mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>%
  mutate(runningmean = ifelse(row_number() >= (nrow(YN_runmean) - 5), mean(sp.cond[(nrow(YN_runmean) - 5):nrow(YN_runmean)]), runningmean)) %>%
  mutate(res = sp.cond - runningmean) 

ggplot(YN_runmean) +
  geom_line(aes(date, sp.cond), color = "blue") +
  geom_line(aes(date, runningmean), color = "red")


outlier_YN_runmean <- YN_runmean %>% 
  filter(res >= 120 |
           res <= -120) 

YN_tib <- loggerYN %>%
  as_tibble() %>%
  time_decompose(sp.cond, method = "twitter") %>%
  anomalize(remainder, 
            method = "GESD",
            alpha = 0.01)

p_twitter_gesd <- YN_tib %>%
  plot_anomaly_decomposition() 

p2 <- YN_tib %>%
  plot_anomalies() 


p2

p_twitter_gesd


ggplot() +
  geom_point(loggerYN, mapping = aes(date, sp.cond)) +
  geom_line(YN_runmean, mapping = aes(date, runningmean), color = "blue") +
  geom_point(outlier_YN_runmean, mapping = aes(date, sp.cond), color = "red")

library(plyr)
`%not_in%` <- purrr::negate(`%in%`)
YN_detected <- YN_runmean[YN_runmean$date %not_in% outlier_YN_runmean$date, ]
detach(package:plyr)

YN_runmean2 <- YN_detected %>%
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA)) %>%
  mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>%
  mutate(runningmean = ifelse(row_number() >= (nrow(YN_runmean2) - 5), mean(sp.cond[(nrow(YN_runmean2) - 5):nrow(YN_runmean2)]), runningmean)) %>%
  mutate(res = sp.cond - runningmean) 

outlier_YN_runmean2 <- YN_runmean2 %>% 
  filter(res >= 120 |
           res <= -120) 

g1 <- ggplot() +
  geom_point(YN_runmean2, mapping = aes(date, sp.cond)) +
  geom_line(YN_runmean2, mapping = aes(date, runningmean), color = "blue") +
  geom_point(outlier_YN_runmean2, mapping = aes(date, sp.cond), color = "red")

g2 <- ggplot(d.YN %>% filter(date >= "2019-12-19 06:00:00" &
                               date <= "2020-10-22 15:30:00")) +
  geom_line(aes(date, discharge))

library(cowplot)
g <- align_plots(g1, g2, align = "v")
plot_grid(g1, g2, ncol = 1, align = "v")
