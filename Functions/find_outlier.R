library(tidyverse)
library(lubridate)
library(zoo)

find_outlier <- function(loggerdata, fieldcond, filename1, filename2) {

#data1 contains the obvious outliers that are very far from the rest of the data
data1 <- loggerdata %>%
mutate(Year_Month = paste(year(date), month(date), sep = "-"), #year month 
       YMD = paste(year(date), month(date), day(date), sep = "-")) %>% #year month day
  group_by(Year_Month) %>%
  mutate(Q_0.01 = quantile(sp.cond, 0.01)) %>% #1% of monthly data is below this threshold
  mutate(Q_0.99 = quantile(sp.cond, 0.99)) %>% #1% of monthly data is above this threshold
  mutate(monthly_ave = mean(sp.cond)) %>% #monthly average
  ungroup() %>%
  group_by(YMD) %>%
  mutate(daily_ave = mean(sp.cond)) %>% #daily average
  ungroup() %>%
  mutate(outlier = ifelse(sp.cond < (0.75 * Q_0.01) | sp.cond > (1.5 * Q_0.99), "Y", "N")) #finding the obvious outliers, should be few to none in some datasets

#data2 gets rid of the big outliers and takes a rolling 6-hour average, then determines outliers by the residuals
data2 <- data1 %>%
  mutate(corr_sp.cond = ifelse(outlier == "Y", NA, sp.cond)) %>%
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
  mutate(runningmean = ifelse(row_number() <= 6, mean(corr_sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
  mutate(runningmean = ifelse(row_number() >= (nrow(loggerdata) - 5), mean(corr_sp.cond[(nrow(loggerdata) - 5):nrow(loggerdata)]), runningmean)) %>%
  mutate(res = corr_sp.cond - runningmean) %>% # residuals used to determine outliers in the next step
  mutate(outlier = ifelse(res >= 200 | res <= -200, "Y", outlier)) #finding outliers far from the 6 hour moving average

#data3 gets rid of outliers from previous step, 6-hour rolling mean 
data3 <- data2 %>%
  mutate(corr_sp.cond = ifelse(outlier == "Y", runningmean, sp.cond)) %>%
  mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
  mutate(runningmean = ifelse(row_number() <= 6, mean(corr_sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
  mutate(runningmean = ifelse(row_number() >= (nrow(loggerdata) - 5), mean(corr_sp.cond[(nrow(loggerdata) - 5):nrow(loggerdata)]), runningmean)) %>%
  mutate(res = corr_sp.cond - runningmean) %>% # residuals used to determine outliers in the next step
  mutate(outlier = ifelse(res >= 200 | res <= -200, "Y", "N")) #finding outliers far from the 6 hour moving average

g <- ggplot() +
  geom_point(loggerdata, mapping = aes(date, sp.cond), color = "#C7CEF6") +
  geom_point(data3, mapping = aes(date, corr_sp.cond), color = "#E6A2C5") +
  geom_point(data1 %>% filter(outlier == "Y"), mapping = aes(date, sp.cond), color = "#D8A49B") +
  geom_point(data2 %>% filter(outlier == "Y"), mapping = aes(date, sp.cond), color = "#D8A49B" ) +
  geom_point(data3 %>% filter(outlier == "Y"), mapping = aes(date, sp.cond), color = "green") +
  geom_point(fieldcond, mapping = aes(date, sp.cond), color = "purple") +
  geom_line(data3, mapping = aes(date, runningmean), color = "#7496D2") +
  geom_jitter(width = 0.5, size = 1) +
  theme_bw()

g2 <- ggplot(data3) +
  geom_point(aes(date, sp.cond), color = "#C7CEF6") +
  geom_point(aes(date, corr_sp.cond), color = "#E6A2C5") +
  geom_line(aes(date, runningmean), color = "#7496D2") +
  facet_wrap("Year_Month", scales = "free") +
  theme_bw()

ggsave(g, filename = paste("Plots/outlier/", filename1, ".png", sep = ""), width = 6, height = 4, units = 'in')
ggsave(g2, filename = paste("Plots/outlier/", filename2, ".png", sep = ""), width = 6, height = 4, units = 'in')



return(data3)

}

#WIC_outlier <- find_outlier(loggerWIC, "WICoutliers", "WICoutliers_month")
# YS_outlier <- find_outlier(loggerYS, "YSoutliers", "YSoutliers_month")
# SW_outlier <- find_outlier(loggerSW, "SWoutliers", "SWoutliers_month")
# YN_outlier <- find_outlier(loggerYN, "YNoutliers", "YNoutliers_month")
# YI_outlier <- find_outlier(loggerYI, "YIoutliers", "YIoutliers_month")
# SMC_outlier <- find_outlier(logger6MC, "6MCoutliers", "6MCoutliers_month")
# DC_outlier <- find_outlier(loggerDC, "DCoutliers", "DCoutliers_month") #needs help
# PBMS_outlier <- find_outlier(loggerPBMS, "PBMSoutliers", "PBMSoutliers_month")
# PBSF_outlier <- find_outlier(loggerPBSF, "PBSFoutliers", "PBSFoutliers_month") #needs help
