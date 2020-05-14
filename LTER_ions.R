library(tidyverse)
library(gridExtra)
library(lubridate)
library(patchwork)
source("Functions/ion_plot.R")
source("Functions/splot.R")

LTER <- read.csv("Data/LTER_ions.csv") %>%
  mutate(date = as.Date(as.character(sampledate)))

North <- LTER %>%
  filter(lakeid == "AL" | lakeid == "BM" | lakeid == "CR" | lakeid == "CB" | lakeid == "TB" | lakeid == "FI" | lakeid == "TR" | lakeid == "SP",
         cl > 0)

South <- LTER %>%
  filter(lakeid == "ME" | lakeid == "MO" | lakeid == "WI",
         cl > 0)

ion(North, North$cl, "Chloride Concentration"~(mg~L^-1))

ion(South, South$cl, "Chloride Concentration"~(mg~L^-1))


#custom plot for proposal
ME_MO_ion <- South %>%
  filter(lakeid == "ME" | lakeid == "MO" )

cl <- ion_cl(ME_MO_ion, "Chloride Concentration"~(mg~L^-1))
ca <- ion_other(ME_MO_ion, ME_MO_ion$ca, "Calcium Concentration"~(mg~L^-1))
mg <- ion_other(ME_MO_ion, ME_MO_ion$mg, "Magnesium Concentration"~(mg~L^-1))
na <- ion_other(ME_MO_ion, ME_MO_ion$na, "Sodium Concentration"~(mg~L^-1))
k <- ion_other(ME_MO_ion, ME_MO_ion$k, "Potassium Concentration"~(mg~L^-1))
fe <- ion_other(ME_MO_ion, ME_MO_ion$fe, "Iron Concentration"~(mg~L^-1))
mn <- ion_other(ME_MO_ion, ME_MO_ion$mn, "Manganese Concentration"~(mg~L^-1))
so <- ion_other(ME_MO_ion, ME_MO_ion$so4, "Sulfate Concentration"~(mg~L^-1))

ionplot <- (ca | mg | fe | mn) /
  (na | k | so) /
  cl

ionplot

ggsave("Plots/ME_MO_ions.png", width = 12, height = 10, units = "in")
