source("Functions/chloride_mass_tribs.R")

a <- chloride_mass_load_rate(labYI, loggerYI, d.YI)

slopeQ <- coef(infoq(labYI, d.YI))[2,1] #get slope value
interceptQ <- coef(infoq(labYI, d.YI))[1,1] #intercept value 

clq <- join_datasets_chloride(labYI, d.YI)

cl.2 <- d.YI %>%
  left_join(clq, by = "date")

cl.q.2 <- cl.2 %>%
  mutate(cl_fromQ = ifelse(is.na(chloride_mgL), (slopeQ * discharge) + interceptQ, chloride_mgL))



ggplot() +
  geom_line(a, mapping = aes(date, chloride_mgL, color = "From Sp. Cond")) +
  geom_line(cl.q.2, mapping = aes(date, cl_fromQ, color = "From Discharge"))


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
  geom_bar(AOS, mapping = aes(date, (precip * 100)), stat = "identity") +
  geom_line(SW_lowpoints, mapping = aes(date, sp.cond))
