#library(tsoutliers)
library(tidyverse)
library(anomalize)
library(TTR)
library(zoo)


YN_tib <- loggerYN %>%
  #filter(date < "2020-02-01 00:00:00") %>%
  as_tibble() %>%
  time_decompose(sp.cond, method = "twitter") %>%
  anomalize(remainder, 
            method = "GESD",
            alpha = 0.01)

nrow(subset(YN_tib, anomaly == "Yes")) #482
nrow(subset(YN_tib, anomaly == "No")) #14321
 (482 / 14803) * 100 #3.3% using twitter, IQR
 (530 / 14803) * 100 #3.6% using stl, IQR
 (775 / 14803) * 100 #5.2% using twitter, GESD

p_twitter_gesd <- YN_tib %>%
  plot_anomaly_decomposition() 

p2 <- YN_tib %>%
  plot_anomalies()


p2

p_twitter_gesd


YN_runmean <- loggerYN %>%
  mutate(runningmean = rollmean(sp.cond, 12, fill = NA)) %>%
  mutate(res = sp.cond - runningmean)

ggplot(YN_runmean) +
  geom_line(aes(date, sp.cond), color = "blue") +
  geom_line(aes(date, runningmean), color = "red")


ggplot(YN_runmean %>% filter(res >= 100 |
                               res <= -100)) +
  geom_point(aes(date, res))


# Create a boxplot of the dataset where outliers are shown as distinct points 
#add $out after the end parenthesis to get a dataset with all outliers according to this boxplot
boxplot(loggerYN$Full.Range)
boxplot(loggerYI$Full.Range)
boxplot(loggerYS$Full.Range)
boxplot(loggerSW$Full.Range)
boxplot(logger6MC$Full.Range)
boxplot(loggerDC$Full.Range)
boxplot(loggerPBMS$Full.Range)
boxplot(loggerPBSF$Full.Range)

ggplot(loggerYN) +
  geom_line(aes(date, Full.Range, color = "Full Range")) +
  geom_line(aes(date, sp.cond, color = "Specific Conducticity")) +
  geom_line(aes(date, temp * 1000, color = "temperature"))

logYN <- loggerYN %>%
  mutate(lograw = log(Full.Range)) %>%
  mutate(logsp = log(sp.cond))

ggplot(logYN) +
  geom_line(aes(date, lograw, color = "lograw")) +
  geom_line(aes(date, logsp, color = "logsp"))

quantile(loggerYN$Full.Range, c(.01, .99)) #325.800 & 673.699
quantile(loggerYN$sp.cond, c(.01, .99)) #454.6133, 902.5490 
quantile(logYN$lograw, c(.01, .99)) #5.786284 6.512783
quantile(logYN$logsp, c(.01, .99)) #6.119447 6.805223




YN_check_1perc<- logYN %>%
  filter(Full.Range < 325.8 |
           sp.cond < 454.6133 |
           lograw < 5.786284 |
           logsp < 6.119447)
  
outlier.YN <- tsoutliers::tso(check, types = c("AO", "IO", "LS", "TC", "SLS"))


check <- loggerYN %>%
  filter(date < "2020-01-01 00:00:00") %>%
  select(sp.cond)

check <- ts(check) 

ggplot(check) +
  geom_line(aes(date, sp.cond))









quantile(loggerYI$Full.Range, c(.01, .99))

#Finding the highest and lowest 
subYN <- loggerYN %>%
  filter(Full.Range > 1.25 * quantile(Full.Range, .99))

subYI <- loggerYI %>%
  filter(Full.Range < .5 * quantile(Full.Range, .01) |
           Full.Range > 1.25 * quantile(Full.Range, .99))
library(plyr)
loggery <- loggerYI[!loggerYI$Full.Range %in% subYI$Full.Range, ]
detach("package:plyr", unload = TRUE)


cond_compare(fieldcondYI, loggery)
cond(loggery)

cond(loggerYI)


YIoutliertest <- tso(y = ts(loggerYI$Full.Range))

log <- loggerYI %>%
  mutate(log = log(loggerYI$Full.Range)) %>%
  mutate(logsp = log(loggerYI$sp.cond))

ggplot(log) +
  geom_line(aes(date, log), color = "red") +
  geom_line(aes(date, logsp), color = "blue")

YI_outliers <- loggerYI %>%
  filter(log < quantile(logsp, .01))


test <- ts(loggerYI$Full.Range)





