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
            