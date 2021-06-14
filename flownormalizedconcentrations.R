options(scipen = 999)
library(tidyverse)
#flow normalized daily average SC and chloride
flow_normalize <- function(ts, id) {
  
  ts1 <- ts %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(discharge_cms = mean(runningmeandis, na.rm = TRUE), 
              SpCond_uScm = mean(sp.cond, na.rm = TRUE),
              chloride_mgL = mean(chloride_predict, na.rm = TRUE)) %>%
    mutate(normalized_SpCond = SpCond_uScm / discharge_cms) %>%
    mutate(normalized_chloride = chloride_mgL / discharge_cms) %>%
    mutate(zscore_cond = (normalized_SpCond - mean(normalized_SpCond, na.rm = TRUE)) / sd(normalized_SpCond, na.rm = TRUE)) %>%
    mutate(zscore_chloride = (normalized_chloride - mean(normalized_chloride, na.rm = TRUE)) / sd(normalized_chloride, na.rm = TRUE)) %>%
    mutate(ID = id)
}



All_flow_norm <- flow_normalize(YN_ts_mass, "YR-I") %>%
  bind_rows(flow_normalize(DC_ts_mass, "DC"),
            flow_normalize(SMC_ts_mass, "SMC"),
            flow_normalize(PBMS_ts_mass, "PBMS"),
            flow_normalize(YI_ts_mass, "YR-O"))

library(wesanderson)

ggplot() +
  geom_line(All_flow_norm %>% filter(ID != "YR-O"), mapping = aes(date, zscore_chloride, group = ID, color = ID)) +
  geom_line(All_flow_norm %>% filter(ID == "YR-O"), mapping = aes(date, zscore_chloride, group = ID, color = ID), size = 1) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  L_theme() + theme(legend.title = element_blank()) +
  labs(y = "Z-Score", x = "",
       caption = "Figure X. Z-score timeseries for each river's daily flow-normalized chloride concentration. 
The thick line is the Yahara River outlet (YR-O) of Lake Mendota.") 

ggsave("Plots/QC_plots/Eckhardt_Method/flownorm/zscore.png", width = 6.25, height = 4.25, units = "in")

ggplot() +
  geom_line(All_flow_norm %>% filter(ID != "YR-O"), mapping = aes(date, normalized_chloride, group = ID, color = ID)) +
  geom_line(All_flow_norm %>% filter(ID == "YR-O"), mapping = aes(date, normalized_chloride, group = ID, color = ID), size = 1) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_y_log10() +
  L_theme() + theme(legend.title = element_blank()) +
  labs(y = "Flow-normalized Chloride Concentration (C/Q)", x = "",
       caption = "Figure X. Flow-normalized chloride concentration daily timeseries on a log10 axis. The thick 
line is the Yahara River outlet (YR-O) of Lake Mendota.") 

ggsave("Plots/QC_plots/Eckhardt_Method/flownorm/flow-normCl.png", width = 6.25, height = 4.25, units = "in")



ts <- all_ts %>%
  filter(ID == "YN" |
  ID == "SMC"|
  ID == "PBMS" |
  ID == "DC" |
  ID == "YI") %>%
  mutate(ID = ifelse(ID == "YN", "YR-I", ID)) %>%
  mutate(ID = ifelse(ID == "YI", "YR-O", ID))

chloride_ts_plot <- function(id, color) {
  ggplot(ts %>% filter(ID == id)) +
    geom_line(aes(date, chloride_predict, group = ID, color = ID)) + 
    scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")[color]) +
    #scale_y_log10() +
    L_theme() + theme(legend.position = "none") +
    labs(y = "Cl"^-1~(mg~L^-1), x = "", title = id)
}


a <- chloride_ts_plot("YR-I", 4)

b <- chloride_ts_plot("SMC", 3)

c <- chloride_ts_plot("DC", 1)

d <- chloride_ts_plot("PBMS", 2)

e <- chloride_ts_plot("YR-O", 5)


(a | b | c) / (d | e) +
  plot_annotation(
                  caption = "Figure X. Chloride concentration timeseries in each river.",
                  theme = theme(plot.tag = element_text(size = 10), 
                                plot.caption = element_text(size = 10, hjust = 0)))

ggsave("Plots/QC_plots/Eckhardt_Method/flownorm/just-chloride.png", width = 6.25, height = 4.25, units = "in")


ggplot(ts) +
  geom_line(aes(date, cumulative_cl_g / 1000000, group = ID, color = ID)) +
  L_theme() + theme(legend.title = element_blank()) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "", y = "Cumulative Chloride Load (Mg)",
       caption = "Figure X. Cumulative load of chloride in each tributary over the full record.")


ggplot(ts) +
  geom_line(aes(date, (cumulative_cl_g / 1000000) / runningmeandis, group = ID, color = ID)) +
  L_theme() + theme(legend.title = element_blank()) + ylim(0,10000) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "", y = "Cumulative Chloride Load (Mg)",
       caption = "Figure X. Cumulative load of chloride in each tributary over the full record.")

#not sure about this.....

library(ggpubr)

dat <- All_flow_norm %>% drop_na()
# Edit from here
x <- which(names(dat) == "ID") # name of grouping variable
y <- which(names(dat) == "zscore_chloride") # names of variables to test
         
method1 <- "kruskal.test" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("YR-O", "Z-score"), c("YR-I", "Z-score"), c("SMC", "Z-score"), c("DC", "Z-score"), c("PBMS", "Z-score")) # comparisons for post-hoc tests
# Edit until here
# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(dat,
                   x = colnames(dat[j]), y = colnames(dat[i]),
                   color = colnames(dat[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter"
    )
    print(
      p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                             method = method1, label.y = max(dat[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}








