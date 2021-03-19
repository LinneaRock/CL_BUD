library(tidyverse)
library(gridExtra)
library(lubridate)
library(patchwork)
source("Functions/ion_plot.R")
source("Functions/splot.R")

# LTER <- read.csv("Data/LTER_ions.csv") %>%
#   mutate(date = as.Date(as.character(sampledate)))
# 
# North <- LTER %>%
#   filter(lakeid == "AL" | lakeid == "BM" | lakeid == "CR" | lakeid == "CB" | lakeid == "TB" | lakeid == "FI" | lakeid == "TR" | lakeid == "SP",
#          cl > 0)
# 
# South <- LTER %>%
#   filter(lakeid == "ME" | lakeid == "MO" | lakeid == "WI",
#          cl > 0)
# 
# ion(North, North$cl, "Chloride Concentration"~(mg~L^-1))
# 
# ion(South, South$cl, "Chloride Concentration"~(mg~L^-1))
# 
# #custom plot for proposal
# ME_MO_ion <- South %>%
#   filter(lakeid == "ME" | lakeid == "MO" ) 

library(NTLlakeloads)
all_ions <- loadLTERions() 
ME_MO_ion <- all_ions %>%
  filter(lakeid == "ME" | lakeid == "MO") %>%
  filter(fe < 4) %>%
  mutate(across(everything(),~replace(., .<0,NA))) %>%
  rename_at(vars(cl:cond),~str_c("value_",.)) %>%
  rename_at(vars(flagcl:flagcond),~str_c("error_",.)) %>%
  rename_all(~str_replace_all(.,"flag","")) %>%
  pivot_longer(-(lakeid:event),names_to =c('.value','item'),names_sep ='_') %>%
  filter(!is.na(value)&value>=0) %>%
  filter(!str_detect(error,'A|K|L')|is.na(error)) # Remove suspect data
# A sample suspect
# L data and blind differ by more than 15%
# K data suspect






cl <- ion_cl(ME_MO_ion, "Chloride")
ca <- ion_other(ME_MO_ion, "ca", "Calcium")
mg <- ion_other(ME_MO_ion, "mg", "Magnesium")
na <- ion_other(ME_MO_ion, "na", "Sodium")
k <- ion_other(ME_MO_ion, "k", "Potassium")
fe <- ion_other(ME_MO_ion, "fe", "Iron")
mn <- ion_other(ME_MO_ion, "mn", "Manganese")
so4 <- ion_other(ME_MO_ion, "so4", "Sulfate")

ionplot <- (cl | na) /
  (ca | mg) /
  (so4 | k) /
  (fe | mn) +
  plot_annotation(caption = "Figure X. Major ion concentrations in Lakes Mendota and Monona from 1995 . All units are mg/L. Besides chloride and sodium, 
no other ions show long term increasing trends in these lakes (Data source: N.Lead PI et al. (2019).",
                  theme = theme(text = element_text(size = 10, hjust = 0)))

ionplot


ggsave("Plots/ME_MO_ions.png", width = 20, height = 20, units = "cm")


