source("Functions/chloride_mass_tribs.R")
source("Functions/L_theme.R")

a <- chloride_mass_load_rate(labYI, loggerYI, d.YI)

a <- chloride_mass_load_rate(labYN, loggerYN, d.YN)

a <- chloride_mass_load_rate(lab6MC, logger6MC, d.6MC)

a <- chloride_mass_load_rate(labDC, loggerDC, d.DC)

a <- chloride_mass_load_rate(labPBMS, loggerPBMS, d.PBMS)

a <- chloride_mass_load_rate(labPBSF, loggerPBSF, d.PBSF)

jointest <- join_datasets_chloride(labYI, loggerYI)


ggplot(a %>% filter(date < "2020-05-01 00:00:00")) +
  geom_line(aes(date, cl_load)) +
  L_theme() +
  labs(title = "Pheas",
       y = "Chloride Loading"~(g~s^-1),
       x = "")


#checkign starkweatehr low points
SW_lowpoints <- loggerSW %>%
  filter(date > "2020-04-30 23:30:00") %>%
  filter(sp.cond < 600)

SW_lowcond <- cond(SW_lowpoints)

AOS <- read_csv("C:/Users/linne/Downloads/data.csv") %>%
  mutate(char = as.character(Date)) %>%
  mutate(char = gsub("T", " ", char),
         char = gsub("Z", "", char)) %>%
  mutate(date = as_datetime(char)) 

ggplot() +
  geom_bar(AOS, mapping = aes(date, (precip * 200)), stat = "identity") +
  geom_line(SW_lowpoints, mapping = aes(date, sp.cond))
rm(intercept)
rm(slope)
