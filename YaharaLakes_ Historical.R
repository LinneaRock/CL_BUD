library(tidyverse)
library(readxl)
source("Functions/splot.R")
source("Functions/L_theme.R")

watershed <- read_xlsx("Data/Historical_External/YaharaHist.xlsx") %>%
  mutate(KA = as.numeric(KA))

ggplot(watershed) +
  geom_point(aes(Date, ME, color = "#1C366B"), size = .5) +
  geom_point(aes(Date, MO, color = "#F24D29"), size = .5) +
  geom_point(aes(Date, WI, color = "#C4CFD0"), size = .5) +
  geom_point(aes(Date, KA, color = "#E5C4A1"), size = .5) +
  geom_point(aes(Date, WA, color = "#1DACE8"), size = .5) +
  geom_smooth(aes(Date, ME, color = "#1C366B"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, MO, color = "#F24D29"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, WI, color = "#C4CFD0"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, KA, color = "#E5C4A1"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, WA, color = "#1DACE8"), size = 1, se = FALSE) +
  labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n",
       x = ""#,
       #caption = "Figure 3 The long-term increasing chloride concentration trend 
#in the Yahara River watershed lakes (Public Health Madison Dane County, 2020)."
) + L_theme() +
  scale_color_identity(guide = "legend",
                       breaks = c("#1C366B", "#F24D29", "#C4CFD0", "#1DACE8", "#E5C4A1"),
                       labels = c("Mendota", "Monona", "Wingra", "Waubesa", "Kegonsa"))


splot("Historical_Data_Viz/", "ChainLakes")



