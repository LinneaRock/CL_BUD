##boxplot of trib chloride concentrations
all_cl1 <- rbind(labDC, lab6MC, labYI, labYN, labYS)
all_cl2 <- rbind(labPBMS, labPBSF, labSW, labWIC)
all_cllakes <- rbind(labME, labMO)
all_data <- bind_rows(all_cl1, all_cl2, all_cllakes)

lows <- ggplot(all_data %>% filter(ID != "PBMS" & ID != "PBSF" & ID != "WIC" & ID != "SW" & ID != "MO"), mapping = aes(reorder(ID, chloride_mgL), chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), 
                     values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "") + L_theme()  + theme_bw() + theme(legend.position = "none")

all <- ggplot(all_data, mapping = aes(reorder(ID, chloride_mgL), chloride_mgL)) +
  geom_boxplot() +
  geom_jitter(aes(color = season)) +
  scale_color_manual(labels = c("April-October", "November-March"), 
                     values = c("#1C366B", "#F24D29")) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Chloride concentrations at each sampling location. The six sites
with the lowest chloride concentrations are shown in the inset.") + L_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


all + inset_element(lows, 0.12, 0.4, 0.6, 1, align_to = "full")

ggsave("Plots/boxplots.png", height = 4.25, width = 6.25, units = "in")



#Mann-Whitney test seasonal
wilcox.test((all_data %>% 
               filter(season == "April - October"))$chloride_mgL, (all_data %>% 
                                                                     filter(season == "November - March"))$chloride_mgL) #p-value = 4.544e-05
#Mann-Whitney test threshold 100 m/ha road density
wilcox.test((bind_rows(all_cl1, all_cllakes))$chloride_mgL, all_cl2$chloride_mgL) #p-value < 2.2e-16



