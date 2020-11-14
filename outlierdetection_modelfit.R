library(tidyverse)
library(lubridate)
library(anomalize)


#step 1: using the specific conductance, find a 6 hour rolling mean 
rollavg6hr <- function(loggerdata) {
  
  meandata <- loggerdata %>%
    mutate(runningmean = rollmean(sp.cond, 13, fill = NA)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of the remaining rows
    mutate(runningmean = ifelse(row_number() >= (nrow(loggerdata) - 5), mean(sp.cond[(nrow(loggerdata) - 5):nrow(loggerdata)]), runningmean)) %>%
    mutate(res = sp.cond - runningmean) # residuals used to determine outliers in the next step
  
}

#step 2: create a dataset of outliers based on rolling average and 120 µS/cm
find_outliers <- function(mean_dataset) {
  
#find residuals that are greater than 120 µS/cm or less than -120 µS/cm
  outlier <- mean_dataset %>% 
    filter(res >= 120 |
             res <= -120) 
  
}

#step 3: create visual of original data, rolling average, and outliers
plot_outliers <- function(mean_dataset, outlier_dataset) {
  
  ggplot() +
    geom_point(mean_dataset, mapping = aes(date, sp.cond)) +
    geom_line(mean_dataset, mapping = aes(date, runningmean), color = "blue") +
    geom_point(outlier_dataset, mapping = aes(date, sp.cond), color = "red")
  
}

#step 4: compare outliers above with those detected through package anomalize
#This step takes a few moments to run 
anom_detect <- function(loggerdata) {
  tib <- loggerdata %>%
    as_tibble() %>%
    time_decompose(sp.cond, method = "twitter") %>%
    anomalize(remainder, 
              method = "iqr",
              alpha = 0.025) %>%
    filter(anomaly == "Yes")
  
  outliers <- left_join(tib, YI_runmean, by ="date") %>%
    select(date, observed, sp.cond, Temp, runningmean, res) %>%
    mutate(anom = ifelse(sp.cond >  runningmean | sp.cond <  runningmean, "Y", "N")) %>%
    filter(anom == "Y")
  
  return(outliers)
  
}

#step 5: remove outliers and re-do 6 hour rolling average
remove_outliers <- function(meandataset, outlier_dataset) {

library(plyr)
`%not_in%` <- purrr::negate(`%in%`)
detected <- meandataset[meandataset$date %not_in% outlier_dataset$date, ]
detach(package:plyr)

md2 <- detected %>%
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA)) %>%
  mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>%
  mutate(runningmean = ifelse(row_number() >= (nrow(detected) - 5), mean(sp.cond[(nrow(detected) - 5):nrow(detected)]), runningmean)) %>%
  mutate(res = sp.cond - runningmean) 

}

#step 6: plot new 6 hour rolling average
plot_6hrave <- function(md2) {
  
  ggplot(md2) +
    geom_point(aes(date, sp.cond)) +
    geom_line(aes(date, runningmean), color = "blue")
  
}

ggplot(loggerPBSF) +
  geom_point(aes(date, sp.cond)) +
  geom_point(check, mapping = aes(date, sp.cond), color = "red")


quantile(loggerPBSF$sp.cond, c(0.01, 0.99))


ggplot(d.PBSF) +
  geom_point(aes(date, discharge))






PBSF_tib <- loggerPBSF %>%
  as_tibble() %>%
  time_decompose(sp.cond, method = "twitter") %>%
  anomalize(remainder, 
            method = "iqr",
            alpha = 0.025) %>%
  time_recompose()
  #filter(anomaly == "Yes")


PBSF_tib %>%
  plot_anomalies(time_recomposed = TRUE)





check <- loggerPBSF %>%
  filter(sp.cond < 100)












YI_runmean <- rollavg6hr(loggerYI)
YI_outlier <- find_outliers(YI_runmean)
plot_outliers(YI_runmean, YI_outlier)
YI_anomalize <- anom_detect(loggerYI)
YI_runmean2 <- remove_outliers(YI_runmean, YI_anomalize)
plot_6hrave(YI_runmean2)

YI_tib_anom <- YI_tib %>%
  filter(anomaly == "Yes")

Check_YI <- left_join(YI_tib_anom, YI_runmean, by ="date") %>%
  select(date, observed, sp.cond, Temp, runningmean, res) %>%
  mutate(anom = ifelse(sp.cond > runningmean | sp.cond < runningmean, "Y", "N"))

YI_dummy <- remove_outliers(YI_runmean2, Check_YI)

quantile(loggerYI$sp.cond, c(0.01, 0.99))

ggplot(loggerYI) +
  geom_line(aes(date, sp.cond)) +
  geom_hline(yintercept = c(450.2572, 601.2260))

YN_runmean <- rollavg6hr(loggerYN)
YN_outlier <- find_outliers(YN_runmean)
plot_outliers(YN_runmean, YN_outlier)
YN_anomalize <- anom_detect(loggerYN)
YN_runmean2 <- remove_outliers(YN_runmean, YN_outlier)
plot_6hrave(YN_runmean2)

YN_tib_anom <- YN_tib %>%
  filter(anomaly == "Yes")

Check_YN <- left_join(YN_tib_anom, YN_runmean, by ="date") %>%
  select(date, observed, sp.cond, Temp, runningmean, res) %>%
  mutate(anom = ifelse(sp.cond > runningmean | sp.cond < runningmean, "Y", "N"))

YN_dummy <- remove_outliers(YN_runmean2, Check_YN)



quantile(loggerYN$sp.cond, c(0.01, 0.99))

ggplot(loggerYN) +
  geom_line(aes(date, sp.cond)) +
  geom_hline(yintercept = c(457.1854, 894.4162))


YN_anomalize <- loggerYN %>%
  as_tibble() %>%
  time_decompose(sp.cond, method = "twitter") %>%
  anomalize(remainder, 
            method = "iqr",
            alpha = 0.025) %>%
  time_recompose()

ggplot() +
  geom_point(loggerYN, mapping = aes(date, sp.cond)) +
  #geom_point(YN_anomalize, mapping = aes(date, sp.cond), color = "red") +
  geom_hline(yintercept = c(457.1854, 894.4162))

YN_anomalize %>%
  plot_anomalies(time_recomposed = TRUE) +
  geom_hline(yintercept = c(457.1854, 894.4162))



g1 <- plot_6hrave(YI_dummy) #+
  geom_point(Check_YI, mapping = aes(date, sp.cond), color = "turquoise")

g1 <- plot_outliers(YI_runmean2, YI_outlier) +
  geom_point(Check_YI, mapping = aes(date, sp.cond), color = "turquoise")

YI_anomalize <- loggerYI %>%
  as_tibble() %>%
  time_decompose(sp.cond, method = "twitter") %>%
  anomalize(remainder, 
            method = "iqr",
            alpha = 0.025) %>%
  time_recompose()

YI_anomalize %>%
  plot_anomalies(time_recomposed = TRUE) +
  geom_hline(yintercept = c(450.2572, 601.2260))







YI.dis1 <- join_datasets_cond(YI_runmean, d.YI)
YI.dis1 <- YI.dis1 %>%
  mutate(runningmean = rollmean(discharge, 13, fill = NA)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
  mutate(runningmean = ifelse(row_number() <= 6, mean(discharge[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of the remaining rows
  mutate(runningmean = ifelse(row_number() >= (nrow(YI.dis1) - 5), mean(discharge[(nrow(YI.dis1) - 5):nrow(YI.dis1)]), runningmean)) %>%
  mutate(res = discharge - runningmean)
  

g2 <- ggplot(YI.dis1) +
  geom_line(aes(date, discharge)) +
  geom_line(aes(date, runningmean), color = "blue")

SMC_runmean <- rollavg6hr(logger6MC)
SMC_outlier <- find_outliers(SMC_runmean)
plot_outliers(SMC_runmean, SMC_outlier)
anom_detect(logger6MC)
SMC_runmean2 <- remove_outliers(SMC_runmean, SMC_outlier)
plot_6hrave(SMC_runmean2)



  



g1 <- ggplot() +
  geom_point(YN_runmean2, mapping = aes(date, sp.cond)) +
  geom_line(YN_runmean2, mapping = aes(date, runningmean), color = "blue") +
  geom_point(outlier_YN_runmean2, mapping = aes(date, sp.cond), color = "red")



g2 <- ggplot(YN.dis.cond) +
  geom_line(aes(date, discharge)) +
  geom_line(aes(date, runningmean), color = "blue")

library(cowplot)

plot_grid(g1, g2, ncol = 1, align = "v")




