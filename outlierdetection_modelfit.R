library(tidyverse)
library(lubridate)
library(zoo)



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
plot_outliers <- function(mean_dataset, outlier_dataset, field_cond) {
  
  ggplot() +
    geom_point(mean_dataset, mapping = aes(date, sp.cond)) +
    geom_line(mean_dataset, mapping = aes(date, runningmean), color = "blue") +
    geom_point(outlier_dataset, mapping = aes(date, sp.cond), color = "red") +
    geom_point(field_cond, mapping = aes(date, sp.cond), color = "green")
  
}

#step 4: compare outliers above with those detected through package anomalize
#This step takes a few moments to run 
anom_detect <- function(loggerdata) {
  tib <- loggerdata %>%
    as_tibble() %>%
    time_decompose(sp.cond, 
                   frequency = 12,
                   trend = 12,
                   method = "stl") %>%
    anomalize(remainder, 
              method = "iqr",
              alpha = 0.05,
              max_anoms = 0.05) #%>%
    #filter(anomaly == "Yes")
  

  
   #outliers <- left_join(tib, mean_dataset, by = "date") #%>%
     #mutate()
  
  #return(outliers)
  
}

plot_anom <- function(mean_dataset, anom_dataset, field_cond) {
  
  ggplot() +
    geom_point(mean_dataset, mapping = aes(date, sp.cond)) +
    geom_line(mean_dataset, mapping = aes(date, runningmean), color = "blue") +
    geom_point(anom_dataset, mapping = aes(date, observed), color = "red") +
    geom_point(field_cond, mapping = aes(date, sp.cond), color = "green")
  
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

tryPBSF <- loggerPBSF %>%
  filter(sp.cond >50)

pbsf1 <- rollavg6hr(tryPBSF)
pbsf2 <- find_outliers(pbsf1)
plot_outliers(pbsf1, pbsf2)
anom_pbsf <- anom_detect(tryPBSF) 


 


check <- loggerPBSF %>%
  filter(sp.cond < 50)

ggplot(loggerPBSF) +
  geom_point(aes(date, sp.cond)) +
  geom_point(check, mapping = aes(date, sp.cond), color = "red") +
  geom_point(fieldcondPBSF, mapping = aes(date, sp.cond), color = "blue")


quantile(loggerPBSF$sp.cond, c(0.01, 0.99))


ggplot(d.PBSF) +
  geom_point(aes(date, discharge))






str(check)


PBSF_tib <- tryPBSF %>%
  as_tibble() %>%
  time_decompose(sp.cond, 
                 frequency = 12,
                 trend = "3 months",
                 method = "stl") %>%
  anomalize(remainder, 
            method = "GESD",
            alpha = 0.05,
            max_anoms = 0.05) %>%
  time_recompose() #%>%
  #filter(anomaly == "Yes")


PBSF_tib %>%
  plot_anomalies(time_recomposed = TRUE)



checkout <- left_join(PBSF_tib, pbsf2, by = "date")

plot_outliers(pbsf1, checkout)

test <- PBSF_tib %>%
  clean_anomalies() %>%
  filter(anomaly == "Yes")


ggplot() +
  geom_point(loggerPBSF %>% filter(date >= "2020-01-25 00:00:00" & date <= "2020-01-26 23:00:00"), mapping = aes(date, sp.cond)) 

ggplot() +
  geom_point(d.PBSF %>% filter(date >= "2020-01-25 00:00:00" & date <= "2020-01-26 23:00:00"), mapping = aes(date, discharge)) 


ggplot() + 
  geom_point(loggerPBSF %>% filter(date >= "2020-05-01 00:00:00" & date <= "2020-6-30 23:00:00"), mapping = aes(date, sp.cond)) +
  geom_point(fieldcondPBSF %>% filter(date >= "2020-05-01 00:00:00" & date <= "2020-6-30 23:00:00"), mapping = aes(date, sp.cond), color = "red") +
  geom_point(checkout %>% filter(date >= "2020-05-01 00:00:00" & date <= "2020-6-30 23:00:00"), mapping = aes(date, observed), color = "purple") +
  geom_point(test %>% filter(date >= "2020-05-01 00:00:00" & date <= "2020-6-30 23:00:00"), mapping = aes(date, observed_cleaned), color = "blue")

YN_tib <- loggerYN %>%
  as_tibble() %>%
  time_decompose(sp.cond, 
                 frequency = 12,
                 trend = "3 months",
                 method = "stl") %>%
  anomalize(remainder, 
            method = "GESD",
            alpha = 0.05,
            max_anoms = 0.05) %>%
  time_recompose() #%>%
#filter(anomaly == "Yes")


YN_tib %>%
  plot_anomalies(time_recomposed = TRUE)

YI_tib <- loggerYI %>%
  as_tibble() %>%
  time_decompose(sp.cond, 
                 frequency = 12,
                 trend = "3 months",
                 method = "stl") %>%
  anomalize(remainder, 
            method = "GESD",
            alpha = 0.05,
            max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE)
YI_tib

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


#step 4: compare outliers above with those detected through package anomalize
#This step takes a few moments to run 
anom_detect <- function(loggerdata) {
  tib <- loggerdata %>%
    as_tibble() %>%
    time_decompose(sp.cond, 
                   frequency = 30,
                   trend = 360,
                   method = "stl") %>%
    anomalize(remainder, 
              method = "iqr",
              alpha = 0.05,
              max_anoms = 0.05) #%>%
  #filter(anomaly == "Yes")

}




YSanom <- anom_detect(loggerYS) %>%
  filter(remainder > 120 | remainder < -120)
plot_outliers(YS_runmean, YS_outlier, fieldcondYS) + geom_point(YS_anom2, mapping = aes(date, observed), color = "purple")

quantile(loggerYN$sp.cond, c(0.01, 0.99))

loggerYN$sp.cond < (457.1854/2)
loggerYN$sp.cond > 894.4162 * 2

YNanom <- anom_detect(loggerYN) %>%
  filter(remainder > 100 | remainder < -100)


plot_outliers(YN_runmean, YN_outlier, fieldcondYN) + geom_point(YNanom, mapping = aes(date, observed), color = "purple")


PBSFanom <- anom_detect(PBSF2) %>%
  filter(remainder > 120 | remainder < -120)
plot_outliers(PBSF_runmean, PBSF_outlier, fieldcondPBSF) 

ggplot() +
  geom_point(loggerPBSF, mapping = aes(date, sp.cond)) +
  geom_point(fieldcondPBSF, mapping = aes(date, sp.cond), color = "red") +
  geom_point(PBSFanom, mapping = aes(date, observed), color = "purple")

PBSF2 <- loggerPBSF %>%
  mutate(Year_Month = paste(year(date), month(date), sep = "-"))

ggplot(PBSF2) +
  geom_line(aes(date, sp.cond)) +
  facet_wrap("Year_Month", scales = "free_x") 
  
YN2 <- loggerYN %>%
  mutate(Year_Month = paste(year(date), month(date), sep = "-")) %>%
  group_by(Year_Month) %>%
  mutate(IQR_0.01 = quantile(sp.cond, 0.01)) %>%
  mutate(IQR_0.99 = quantile(sp.cond, 0.99)) %>%
  ungroup() %>%
  mutate(outlier = ifelse(sp.cond < (0.75 * IQR_0.01) | sp.cond > (1.5 * IQR_0.99), "Y", "N"))


ggplot(YN2) +
  geom_line(aes(date, sp.cond)) +
  facet_wrap("Year_Month", scales = "free") 
#start here tomorrow
YS2 <- loggerYS %>%
  mutate(Year_Month = paste(year(date), month(date), sep = "-")) %>%
  group_by(Year_Month) %>%
  mutate(Q_0.01 = quantile(sp.cond, 0.01)) %>%
  mutate(Q_0.99 = quantile(sp.cond, 0.99)) %>%
  mutate(monthly_ave = mean(sp.cond)) %>%
  ungroup() %>%
  mutate(outlier = ifelse(sp.cond < (0.75 * Q_0.01) | sp.cond > (1.5 * Q_0.99), "Y", "N")) #%>%
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
  mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of the remaining rows
  mutate(runningmean = ifelse(row_number() >= (nrow(YS2) - 5), mean(sp.cond[(nrow(YS2) - 5):nrow(YS2)]), runningmean)) %>%
  mutate(res = sp.cond - runningmean) %>% # residuals used to determine outliers in the next step
  mutate(outlier2 = ifelse(res >= 120 | res <= -120, "Yes", outlier))

ggplot(YS4) +
  geom_point(aes(date, corr_sp.cond)) +
  geom_line(aes(date, runningmean), color = "blue") +
  #geom_line(aes(date, monthly_ave)) +
  facet_wrap("Year_Month", scales = "free") 

YS3 <- YS2 %>%
  group_by(Year_Month) %>%
  mutate(corr_sp.cond = ifelse(outlier2 != "N", runningmean, sp.cond)) %>%
  mutate(IQR_0.01 = quantile(sp.cond, 0.01)) %>% #monthly 1th percentile
  mutate(IQR_0.99 = quantile(sp.cond, 0.99)) %>% #monthly 99th percentile
  mutate(monthly_ave = mean(sp.cond)) %>% #monthly mean
  ungroup() %>%
  mutate(outlier1 = ifelse(sp.cond < (0.75 * IQR_0.01) | sp.cond > (1.5 * IQR_0.99), "Y", "N")) %>% #finding obvious outliers 
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
  mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of the remaining rows
  mutate(runningmean = ifelse(row_number() >= (nrow(YS2) - 5), mean(sp.cond[(nrow(YS2) - 5):nrow(YS2)]), runningmean)) %>%
  mutate(res = sp.cond - runningmean) %>% # residuals used to determine outliers in the next step
  mutate(outlier2 = ifelse(res >= 120 | res <= -120, "Yes", outlier)) #finding outliers far from the 6 hour moving average

YS4 <- YS3 %>%
  group_by(Year_Month) %>%
  mutate(corr_sp.cond = ifelse(outlier2 != "N", runningmean, sp.cond)) %>%
  mutate(IQR_0.01 = quantile(sp.cond, 0.01)) %>%
  mutate(IQR_0.99 = quantile(sp.cond, 0.99)) %>%
  mutate(monthly_ave = mean(sp.cond)) %>%
  ungroup() %>%
  mutate(outlier = ifelse(sp.cond < (0.75 * IQR_0.01) | sp.cond > (1.5 * IQR_0.99), "Y", "N")) %>%
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
  mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of the remaining rows
  mutate(runningmean = ifelse(row_number() >= (nrow(YS2) - 5), mean(sp.cond[(nrow(YS2) - 5):nrow(YS2)]), runningmean)) %>%
  mutate(res = sp.cond - runningmean) %>% # residuals used to determine outliers in the next step
  mutate(outlier2 = ifelse(res >= 120 | res <= -120, "Yes", outlier))
