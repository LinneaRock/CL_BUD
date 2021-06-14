library(cowplot)
library(patchwork)

#plots for cq stuff

grid <- function(df, cond_data, figno, name) {
  #date min/max for grid
  datemin <- min(cond_data$date)
  datemax <- max(cond_data$date)
  
  #chemograph (EC)
  a <- ggplot(df) +
    geom_line(aes(date, bf_cl)) +
    geom_line(aes(date, event_cl), color = "#F24D29") +
    L_theme() +
    labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + 
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    scale_x_datetime(limits = c(datemin, datemax))
  #hydrograph
  b <- ggplot(df) +
    geom_line(aes(date, bf)) +
    geom_line(aes(date, event_flow, color = "#F24D29")) +
    geom_ribbon(mapping = aes(x = date, ymin = 0, ymax = value, fill = "#E5C4A1")) +
    L_theme() +
    scale_color_manual(labels = "Stormflow",
                       values = "#F24D29") +
    scale_fill_manual(labels = "Eckhardt",
                      values = "#E5C4A1") +
    labs(x = "", y = "Discharge"~(m^3~s^-1)) + 
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    scale_x_datetime(limits = c(datemin, datemax)) +
    labs(caption = paste("Figure ", figno, ". Chloride concentration chemograph and hydrograph for ", name, ". Events are shown in 
orange. Tan ribbon on the hydrograph is the Eckhardt baseflow.", sep = ""))
  
  plot_grid(a, b, align = "v", ncol = 1) 
  
  ggsave(paste("Plots/QC_plots/Eckhardt_Method/grids/chloride/", name, ".png", sep = ""), height = 7.25, width = 6.25, units = "in")
  
}


all_cq <- function(df, name) {
  
  df1 <- df %>%
    mutate(discharge = bf) %>%
    mutate(discharge = ifelse(is.na(discharge), event_flow, discharge)) %>%
    mutate(chloride = bf_cl) %>%
    mutate(chloride = ifelse(is.na(chloride), event_cl, chloride))
  
  ggplot(df1) +
    geom_point(aes(log10(discharge), log10(chloride))) +
    geom_smooth(aes(log10(discharge), log10(chloride)), method = "lm", se = FALSE, color = "#1DACE8") +
    labs(y = "Log(Chloride)", 
         x = "Log(Discharge)",
         title = name) +
    L_theme()  
  #  scale_y_log10() +
  #  scale_x_log10()
}

all_cq_stats <- function(df) {
  df1 <- df %>%
    mutate(discharge = bf) %>%
    mutate(discharge = ifelse(is.na(discharge), event_flow, discharge)) %>%
    mutate(chloride = bf_cl) %>%
    mutate(chloride = ifelse(is.na(chloride), event_cl, chloride)) %>%
    drop_na(chloride)
  
  df2 <- df1 %>%
    drop_na(chloride) %>%
    filter(discharge > 0) %>%
    filter(chloride > 0) %>%
    mutate(discharge = log10(discharge)) %>%
    mutate(chloride = log10(chloride)) 
  
  fit <- summary(lm(chloride~discharge, data = df2))
  
  return(fit)
}

#seasonal full record cq relationships#####
seasonal_fullrecord <- function(df, name) {
  
  df1 <- df %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "October" |
        mon == "November" |
        mon == "December" , "Oct-Dec", season),
      season =  ifelse(
        mon == "January" |
          mon == "February" |
          mon == "March", "Jan-Mar", season),
      season = ifelse(
        mon == "April" |
          mon == "May" |
          mon == "June", "Apr-Jun", season),
      season = ifelse(
        mon == "July" |
          mon == "August" |
          mon == "September", "Jul-Sep", season),
    )
  
  df2 <- df1 %>%
    mutate(discharge = bf) %>%
    mutate(discharge = ifelse(is.na(discharge), event_flow, discharge)) %>%
    mutate(chloride = bf_cl) %>%
    mutate(chloride = ifelse(is.na(chloride), event_cl, chloride)) %>%
    filter(chloride > 0) %>%
    filter(discharge > 0) %>%
    mutate(chloride = log10(chloride)) %>%
    mutate(discharge = log10(discharge))
  
  bulk_fit <- summary(lm(chloride~discharge, data = df2))
  bulk_oct <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Oct-Dec")))
  bulk_jan <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Jan-Mar")))
  bulk_apr <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Apr-Jun")))
  bulk_jul <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Jul-Sep")))
  
  seasonal_fits <- data.frame(
    trib = c(name, name, name, name, name),
    season = c("annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    #season = c("annual", "Oct-Dec", "Apr-Jun"),
    slope = c(
      slope_cq(bulk_fit),
      slope_cq(bulk_oct),
      slope_cq(bulk_jan),
      slope_cq(bulk_apr),
      slope_cq(bulk_jul)
    ),
    intercept = c(intercept_cq(bulk_fit),
                  intercept_cq(bulk_oct),
                  intercept_cq(bulk_jan),
                  intercept_cq(bulk_apr),
                  intercept_cq(bulk_jul)
    ),
    p = c(pvalue_cq(bulk_fit),
          pvalue_cq(bulk_oct),
          pvalue_cq(bulk_jan),
          pvalue_cq(bulk_apr),
          pvalue_cq(bulk_jul)
    ),
    r = c(r.sqr.lm_cq(bulk_fit),
          r.sqr.lm_cq(bulk_oct),
          r.sqr.lm_cq(bulk_jan),
          r.sqr.lm_cq(bulk_apr),
          r.sqr.lm_cq(bulk_jul)
    )
  )
  
  return(seasonal_fits)
  
}


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
each_event_cq <- function(df) {
  
  df <- df %>%
    mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
    #mutate(event.flag = as.character(event.flag)) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "October" |
        mon == "November" |
        mon == "December" , "Oct-Dec", season),
      season =  ifelse(
        mon == "January" |
          mon == "February" |
          mon == "March", "Jan-Mar", season),
      season = ifelse(
        mon == "April" |
          mon == "May" |
          mon == "June", "Apr-Jun", season),
      season = ifelse(
        mon == "July" |
          mon == "August" |
          mon == "September", "Jul-Sep", season),
    ) %>%
    drop_na(event.flag)
  
  
  
  df <- df %>%
    drop_na(event_cl) %>%
    filter(event_flow > 0) %>%
    filter(event_cl > 0) %>%
    mutate(discharge = log10(event_flow)) %>%
    mutate(chloride = log10(event_cl)) %>%
    group_by(event.flag) %>%
    mutate(slope = slope_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(intercept = intercept_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(r = r.sqr.lm_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(pvalue = pvalue_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    dplyr::select(event.flag, mon, season, slope, intercept, pvalue, r) %>%
    distinct() %>%
    mutate(new = ifelse(event.flag == lag(event.flag), "X", event.flag)) %>%
    filter(is.na(new))
  
  
  
}

averaged_seasonal_cq <- function(df_each_event, name) {
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
seasonal_baseflow <- function(df, name) {
  
  df <- df %>%
    filter(is.na(event.flag)) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "October" |
        mon == "November" |
        mon == "December" , "Oct-Dec", season),
      season =  ifelse(
        mon == "January" |
          mon == "February" |
          mon == "March", "Jan-Mar", season),
      season = ifelse(
        mon == "April" |
          mon == "May" |
          mon == "June", "Apr-Jun", season),
      season = ifelse(
        mon == "July" |
          mon == "August" |
          mon == "September", "Jul-Sep", season),
    ) 
  
  
  df1 <- df %>%
    drop_na(bf_cl) %>%
    filter(bf > 0) %>%
    filter(bf_cl > 0) %>%
    mutate(discharge = log10(bf)) %>%
    mutate(chloride = log10(bf_cl)) %>%
    group_by(season) %>%
    mutate(slope = slope_cq(summary(lm(chloride~discharge, data = df%>% group_by(event.flag))))) %>%
    mutate(intercept = intercept_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(r = r.sqr.lm_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(pvalue = pvalue_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    dplyr::select(season, slope, intercept, pvalue, r) %>%
    distinct() %>%
    mutate(trib = name)
  
  df2 <-df %>%
    mutate(trib = name)%>%
    drop_na(bf_cl) %>%
    filter(bf > 0) %>%
    filter(bf_cl > 0) %>%
    mutate(discharge = log10(bf)) %>%
    mutate(chloride = log10(bf_cl)) %>%
    group_by(trib) %>%
    mutate(slope = slope_cq(summary(lm(chloride~discharge, data = df%>% group_by(event.flag))))) %>%
    mutate(intercept = intercept_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(r = r.sqr.lm_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    mutate(pvalue = pvalue_cq(summary(lm(chloride~discharge, data = df %>% group_by(event.flag))))) %>%
    dplyr::select(season, slope, intercept, pvalue, r) %>%
    distinct() %>%
    mutate(season = "annual")
  
  df_final <- df1 %>%
    bind_rows(df2) %>% distinct()
  
}

#bulk-averaged stormflow cq
bulk_stormflow <- function(df, name) {
  df1 <- df %>%
    mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
    #mutate(event.flag = as.character(event.flag)) %>%
    mutate(mon = months.POSIXt(date)) %>%
    mutate(season = NA) %>%
    mutate(season = ifelse(
      mon == "October" |
        mon == "November" |
        mon == "December" , "Oct-Dec", season),
      season =  ifelse(
        mon == "January" |
          mon == "February" |
          mon == "March", "Jan-Mar", season),
      season = ifelse(
        mon == "April" |
          mon == "May" |
          mon == "June", "Apr-Jun", season),
      season = ifelse(
        mon == "July" |
          mon == "August" |
          mon == "September", "Jul-Sep", season),
    )
  
  df2 <- df1 %>%
    drop_na(event_cl) %>%
    filter(event_flow > 0) %>%
    filter(event_cl > 0) %>%
    mutate(discharge = log10(event_flow)) %>%
    mutate(chloride = log10(event_cl)) 
  
  bulk_fit <- summary(lm(chloride~discharge, data = df2))
  bulk_oct <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Oct-Dec")))
  bulk_jan <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Jan-Mar")))
  bulk_apr <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Apr-Jun")))
  bulk_jul <- summary(lm(chloride~discharge, data = df2 %>% filter(season == "Jul-Sep")))
  
  bulk_stormflow <- data.frame(
    trib = c(name, name, name, name, name),
    season = c("annual", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
    #season = c("annual", "Oct-Dec", "Apr-Jun"),
    slope = c(
      slope_cq(bulk_fit),
      slope_cq(bulk_oct),
      slope_cq(bulk_jan),
      slope_cq(bulk_apr),
      slope_cq(bulk_jul)
    ),
    intercept = c(intercept_cq(bulk_fit),
                  intercept_cq(bulk_oct),
                  intercept_cq(bulk_jan),
                  intercept_cq(bulk_apr),
                  intercept_cq(bulk_jul)
    ),
    p = c(pvalue_cq(bulk_fit),
          pvalue_cq(bulk_oct),
          pvalue_cq(bulk_jan),
          pvalue_cq(bulk_apr),
          pvalue_cq(bulk_jul)
    ),
    r = c(r.sqr.lm_cq(bulk_fit),
          r.sqr.lm_cq(bulk_oct),
          r.sqr.lm_cq(bulk_jan),
          r.sqr.lm_cq(bulk_apr),
          r.sqr.lm_cq(bulk_jul)
    )
  )
  
  return(bulk_stormflow)
  
}
