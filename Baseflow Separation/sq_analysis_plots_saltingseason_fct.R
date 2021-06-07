library(cowplot)
library(patchwork)

#plots for cq stuff

#functions for each of the variables in the table
slope_cq <- function(fit) {
  round(coef(fit)[2,1], 2)
}

intercept_cq <- function(fit) {
  round(coef(fit)[1,1], 2)
}

r.sqr.lm_cq <- function(fit) {
  #round((info(cl, cond)$adj.r.squared), 2)
  round(fit$r.squared, 2)
}

pvalue_cq <- function(fit) {
  coef(fit)[2,4]
}


##event-averaged stormflow cq relationships #####
each_event_cqSS <- function(df) {
  
  df <- df %>%
    mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
    #mutate(event.flag = as.character(event.flag)) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "November" |
        mon == "December" |
        mon == "January" |
        mon == "February" |
        mon == "March", "November - March", season),
      season = ifelse(is.na(season), "April - October", season))%>%
    drop_na(event.flag)
  
  
  
  df <- df %>%
    drop_na(event_cond) %>%
    filter(event_flow > 0) %>%
    filter(event_cond > 0) %>%
    mutate(discharge = log10(event_flow)) %>%
    mutate(sp.cond = log10(event_cond)) %>%
    group_by(event.flag) %>%
    mutate(slope = slope_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(intercept = intercept_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(r = r.sqr.lm_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(pvalue = pvalue_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    dplyr::select(event.flag, mon, season, slope, intercept, pvalue, r) %>%
    distinct() %>%
    mutate(new = ifelse(event.flag == lag(event.flag), "X", event.flag)) %>%
    filter(is.na(new))
  
  
  
}

averaged_seasonal_cqSS <- function(df_each_event, name) {
  df <- df_each_event %>%
    group_by(season) %>%
    summarise(slope = mean(slope)) %>%
    mutate(trib = name)
  
  df2 <-df_each_event %>%
    mutate(trib = name)%>%
    group_by(trib) %>%
    summarise(slope = mean(slope)) %>%
    mutate(season = "annual")
  
  df_final <- df %>%
    bind_rows(df2)
}

#seasonal_baseflow cq relationships#####
seasonal_baseflowSS <- function(df, name) {
  
  df <- df %>%
    filter(is.na(event.flag)) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "November" |
        mon == "December" |
        mon == "January" |
        mon == "February" |
        mon == "March", "November - March", season),
      season = ifelse(is.na(season), "April - October", season))
  
  
  
  df1 <- df %>%
    drop_na(bf_cond) %>%
    filter(bf > 0) %>%
    filter(bf_cond > 0) %>%
    mutate(discharge = log10(bf)) %>%
    mutate(sp.cond = log10(bf_cond)) %>%
    group_by(season) %>%
    mutate(slope = slope_cq(summary(lm(sp.cond~discharge, data = df%>% group_by(event.flag))))) %>%
    mutate(intercept = intercept_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(r = r.sqr.lm_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(pvalue = pvalue_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    dplyr::select(season, slope, intercept, pvalue, r) %>%
    distinct() %>%
    mutate(trib = name)
  
  df2 <-df %>%
    mutate(trib = name)%>%
    drop_na(bf_cond) %>%
    filter(bf > 0) %>%
    filter(bf_cond > 0) %>%
    mutate(discharge = log10(bf)) %>%
    mutate(sp.cond = log10(bf_cond)) %>%
    group_by(trib) %>%
    mutate(slope = slope_cq(summary(lm(sp.cond~discharge, data = df%>% group_by(event.flag))))) %>%
    mutate(intercept = intercept_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(r = r.sqr.lm_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(pvalue = pvalue_cq(summary(lm(sp.cond~discharge, data = df %>% group_by(event.flag))))) %>%
    dplyr::select(season, slope, intercept, pvalue, r) %>%
    distinct() %>%
    mutate(season = "annual")
  
  df_final <- df1 %>%
    bind_rows(df2) %>% distinct()
  
}

#bulk-averaged stormflow cq
bulk_stormflowSS <- function(df, name) {
  df1 <- df %>%
    mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
    #mutate(event.flag = as.character(event.flag)) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "November" |
        mon == "December" |
        mon == "January" |
        mon == "February" |
        mon == "March", "November - March", season),
      season = ifelse(is.na(season), "April - October", season))
  
  
  df2 <- df1 %>%
    drop_na(event_cond) %>%
    filter(event_flow > 0) %>%
    filter(event_cond > 0) %>%
    mutate(discharge = log10(event_flow)) %>%
    mutate(sp.cond = log10(event_cond)) 
  
  bulk_fit <- summary(lm(sp.cond~discharge, data = df2))
  bulk_salting <- summary(lm(sp.cond~discharge, data = df2 %>% filter(season == "November - March")))
  bulk_nonsalting <- summary(lm(sp.cond~discharge, data = df2 %>% filter(season == "April - October")))

  
  bulk_stormflow <- data.frame(
    trib = c(name, name, name),
    season = c("annual", "November - March", "April - October"),
    slope = c(
      slope_cq(bulk_fit),
      slope_cq(bulk_salting),
      slope_cq(bulk_nonsalting)
    ),
    intercept = c(intercept_cq(bulk_fit),
                  intercept_cq(bulk_salting),
                  intercept_cq(bulk_nonsalting)
              
    ),
    p = c(pvalue_cq(bulk_fit),
          pvalue_cq(bulk_salting),
          pvalue_cq(bulk_nonsalting)
     
    ),
    r = c(r.sqr.lm_cq(bulk_fit),
          r.sqr.lm_cq(bulk_salting),
          r.sqr.lm_cq(bulk_nonsalting)
       
    )
  )
  
  return(bulk_stormflow)
  
}


count_dilution_events <- function(df, name) {
  
  df <- df %>%
    filter(trib == name) %>%
    filter(slope < -0.05)
  
  nrow(df)
 
}

count_chemostatic_events <- function(df, name) {
  
  df <- df %>%
    filter(trib == name) %>%
    filter(slope >= -0.05 & slope <= 0.05)
  
  nrow(df)
  
}

count_mobilization_events <- function(df, name) {
  
  df <- df %>%
    filter(trib == name) %>%
    filter(slope > 0.05)
  
  nrow(df)
  
}
