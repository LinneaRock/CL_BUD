discharge_ts <- function(dischargedata) {
  ggplot(dischargedata) +
    geom_line(aes(date, discharge)) +
    L_theme() +
    labs(x = "",
         y = "Discharge (cms)")
}