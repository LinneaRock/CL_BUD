library(cowplot)

ts_grid <- function(precip_data, dis_data, cond_data, cl_data) {

datemin <- min(cond_data$date)
datemax <- max(cond_data$date)

precip_data <- precip_data %>%
  filter(date >= datemin & date <= datemax)

cl_data <- cl_data %>%
  filter(date >= datemin & date <= datemax)




precip_plot <- ggplot(precip_data, aes(date, PRCP)) +
  geom_bar(stat = "identity") +
  L_theme() +
  scale_x_datetime(limits = c(datemin, datemax)) +
  labs(x = "", y = "",
       title = "Daily Precipitation (mm)")

discharge_plot <- ggplot(dis_data) +
  geom_line(aes(date, discharge), color = "snow3") +
  geom_line(aes(date, runningmeandis)) +
  L_theme() +
  scale_x_datetime(limits = c(datemin, datemax)) +
  labs(x = "", y = "", 
       title = "Discharge"~(m^3~s^-1))


cond_plot <- ggplot(cond_data) +
  geom_line(aes(date, sp.cond), color = "snow3") +
  geom_line(aes(date, runningmean)) +
  L_theme() +
  scale_x_datetime(limits = c(datemin, datemax)) +
  labs(x = "", y = "",
       title = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C) 
  
chloride_plot <- ggplot(cl_data) +
  geom_point(aes(date, chloride_mgL)) +
  L_theme() +
  scale_x_datetime(limits = c(datemin, datemax)) +
  labs(x = "", y = "",
       title = "Chloride Concentration"~(mg~L^-1))


plot_grid(precip_plot, discharge_plot, cond_plot, chloride_plot, align = "v", ncol = 1)
    
   }
       