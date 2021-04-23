#read in chloride data
library(NTLlakeloads)

LTERions <- loadLTERions()

ions_all <- LTERions %>%
  filter(lakeid == "ME" | lakeid == "MO") %>%
  dplyr::select(-c(sta, event, cond))


MEcl <- ions_all %>%
  filter(lakeid == "ME") %>% 
  dplyr:: select(2:7, 15) %>%
  mutate(keep = ifelse(flagcl == "ANL" |
                         flagcl == "L", F, T)) %>%
  mutate(keep = ifelse(is.na(flagcl), T, keep)) %>%
  filter(keep == T) %>%
  filter(rep == 1) %>%
  mutate(chloride_mgL = cl) %>%
  mutate(ID = "ME") %>%
  dplyr::select(year4, sampledate, daynum, depth, chloride_mgL, ID)

MOcl <- ions_all %>%
  filter(lakeid == "ME") %>% 
  dplyr:: select(2:7, 15) %>%
  mutate(keep = ifelse(flagcl == "ANL" |
                         flagcl == "L", F, T)) %>%
  mutate(keep = ifelse(is.na(flagcl), T, keep)) %>%
  filter(keep == T) %>%
  filter(rep == 1) %>%
  mutate(chloride_mgL = cl) %>%
  mutate(ID = "MO") %>%
  dplyr::select(year4, sampledate, daynum, depth, chloride_mgL, ID)


#build hypsometry dataframes
library(glmtools)
library(pracma)
MEhypso <- read_nml(nml_file = 'Data/ME_hypso.nml')
MEhypso$morphometry$H
H <- abs(MEhypso$morphometry$H - max(MEhypso$morphometry$H))
A <- MEhypso$morphometry$A
ME_vol = trapz(x = rev(H), y = rev(A))

MOhypso <- read_nml(nml_file = 'Data/MO_hypso.nml')
MOhypso$morphometry$H
H <- abs(MOhypso$morphometry$H - max(MOhypso$morphometry$H))
A <- MOhypso$morphometry$A
MO_vol = trapz(x = rev(H), y = rev(A))

hypso <- read.csv("Data/lake_hypsometry.csv")

ME_hypso <- hypso %>% filter(lakeid == "ME") %>% mutate(vols = ME_vol * hp_factor)
MO_hypso <- hypso %>% filter(lakeid == "MO") %>% mutate(vols = MO_vol * hp_factor)


MO_vols <- data.frame(
  depth_layer = c("0.25-2", "3-6", "7-10", "11-14", "15-18", "19-22"),
  vols = c(sum(MO_hypso$vols[1:3]), sum(MO_hypso$vols[4:7]), sum(MO_hypso$vols[8:11]), sum(MO_hypso$vols[12:15]), sum(MO_hypso$vols[16:19]), sum(MO_hypso$vols[20:23])),
  depth = c(0, 4, 8, 12, 16, 20)
) %>% mutate(ID = "MO")


Mo_vols_4s <- data.frame(
  depth_layer = c("0.25-2", "3-6", "7-14", "15-22"),
  vols = c(sum(MO_hypso$vols[1:3]), sum(MO_hypso$vols[4:7]), sum(MO_hypso$vols[8:15]), sum(MO_hypso$vols[16:23])),
  depth = c(0, 4, 8, 20)
) %>% mutate(ID = "MO")




#aye aye aye -- LTER data is really annoying to work with

# ME_vols_5 <- data.frame(
#   depth_layer = c("0.25-2", "3-7", "8-12", "13-17", "18-22", "23-25"),
#   vols = c(sum(ME_hypso$vols[1:3]), sum(ME_hypso$vols[4:8]), sum(ME_hypso$vols[9:13]), sum(ME_hypso$vols[14:18]), sum(ME_hypso$vols[19:23]), sum(ME_hypso$vols[24:26])),
#   depth = c(0, 5, 10, 15, 20, 23.5)
# ) %>%
#   mutate(ID = "ME")
# 
# ME_vols_0820 <- data.frame(
#   depth_layer = c("0.25-7", "8-17", "18-25"),
#   vols = c(sum(ME_hypso$vols[1:8]), sum(ME_hypso$vols[9:18]), sum(ME_hypso$vols[19:26])),
#   depth = c(0, 8, 20)
# ) %>%
#   mutate(ID = "ME")
# 
# ME_vols_4 <- data.frame(
#   depth_layer = c("0.25-3", "4-7", "8-11", "12-15", "16-19", "20+"),
#   vols = c(sum(ME_hypso$vols[1:4]), sum(ME_hypso$vols[5:8]), sum(ME_hypso$vols[9:12]),sum(ME_hypso$vols[13:16]),sum(ME_hypso$vols[17:20]),sum(ME_hypso$vols[21:26])),
#   depth = c(0, 4, 8,12,16, 20)
# ) %>%
#   mutate(ID = "ME")
# 
# ME_vols_04820 <- data.frame(
#   depth_layer = c("0.25-2", "3-6", "7-12", "13-25"),
#   vols = c(sum(ME_hypso$vols[1:3]), sum(ME_hypso$vols[4:7]), sum(ME_hypso$vols[8:13]),sum(ME_hypso$vols[14:26])),
#   depth = c(0, 4, 8, 20)
# ) %>%
#   mutate(ID = "ME")
# 
# 
# ME_Mass_2000 <- MEcl %>%
#   filter(year4 < 2000) %>%
#   mutate(depth = ifelse(depth == 1, 0, depth)) %>%
#   mutate(depth = ifelse(daynum == 299 & depth == 4, 8, depth)) %>%
#   mutate(depth = ifelse(daynum == 187 & depth == 22, NA, depth)) %>%
#   drop_na(depth) 
#   
#   test <- ME_Mass_2000 %>%
#     dplyr::select(sampledate, depth) %>%
#     count(sampledate)
# 
#   test <- ME_up20s %>%
#     dplyr::select(sampledate, depth) %>%
#     count(sampledate)
#   
# ME_up20s <- MEcl %>%
#   filter(year4 >=2000) %>%
#   mutate(depth = ifelse(depth == 1, 0, depth)) %>%
#   mutate(chloride_mgL = ifelse(depth == 22 | depth == 6, NA, chloride_mgL)) %>%
#  # drop_na(chloride_mgL) %>%
#   left_join(test, by = "sampledate") %>%
#   mutate(chloride_mgL = ifelse(n == 2, NA, chloride_mgL)) %>%
#   mutate(depth = ifelse(depth == 4 & n == 3, 8, depth)) %>%
#   drop_na(chloride_mgL) %>%
#   mutate(depth = ifelse(depth == 17.5, 20, depth)) %>%
#   left_join(test, by = "sampledate") %>%
#   dplyr::select(-n.x) %>%
#   rename(n = n.y) %>%
#   mutate(depth = ifelse(depth == 16 & n == 3, 20, depth))
# 
# 
#   
# 
ME_mass1 <- labME %>%
  dplyr::select(date, Depth_m, chloride_mgL, ID, mon, season) %>%
  mutate(sampledate = as.Date(date)) %>%
  mutate(year4 = year(date)) %>%
  mutate(daynum = yday(date)) %>%
  rename(depth = Depth_m) %>%
  dplyr::select(year4, sampledate, daynum, depth, chloride_mgL, ID, mon, season) %>%
  left_join(ME_vols_5, by = c("depth", "ID")) %>%
  mutate(vols = ifelse(depth == 2, 359849115.4, vols),
         vols = ifelse(depth == 24, 134739380.1, vols))
# 
# 
# ME_Mass2 <- ME_Mass_2000 %>% filter(n == 3) %>% left_join(ME_vols_0820, by = c("depth", "ID")) %>% 
#   dplyr::select(-whichset, -n)
# 
# 
# ME_Mass4 <- ME_Mass_2000 %>% filter(n != 3) %>% left_join(ME_vols_4, by = c("depth", "ID")) %>% 
#   dplyr::select(-whichset, -n)
# 
# ME_mass5 <- ME_up20s %>% filter(n == 3) %>% left_join(ME_vols_0820, by = c("depth", "ID")) 
# ME_mass6 <- ME_up20s %>% filter(n == 4) %>% left_join(ME_vols_04820, by = c("depth", "ID")) 
# ME_mass7 <- ME_up20s %>% filter(n == 6) %>% left_join(ME_vols_4, by = c("depth", "ID"))
# 
# 
# 
 ME_mass <- bind_rows(ME_mass1, ME_Mass2, ME_Mass4, ME_mass5, ME_mass6, ME_mass7) %>% dplyr::select(-n) %>% arrange(sampledate)
write_rds(ME_mass, "Data/ME_mass.rds")



# #all that nonsense for monona
# 
MO_mass1 <- labMO %>%
  dplyr::select(date, Depth_m, chloride_mgL, ID, mon, season) %>%
  mutate(sampledate = as.Date(date)) %>%
  mutate(year4 = year(date)) %>%
  mutate(daynum = yday(date)) %>%
  rename(depth = Depth_m) %>%
  dplyr::select(year4, sampledate, daynum, depth, chloride_mgL, ID, mon, season) %>%
  left_join(MO_vols, by = c("depth", "ID"))
# 
# 
# MOcl__ <- MOcl %>%
#   mutate(chloride_mgL = ifelse(depth == 6 | depth == 22, NA, chloride_mgL)) %>%
#   drop_na(chloride_mgL) %>%
#   mutate(depth = ifelse(depth == 1, 0, depth))
# 
# test <- MOcl__ %>%
#       dplyr::select(sampledate, depth) %>%
#       count(sampledate)
# 
# MOcl__ <- MOcl__ %>%
#   left_join(test, by ='sampledate') %>%
#   mutate(chloride_mgL = ifelse(n == 2, NA, chloride_mgL)) %>%
#   mutate(depth = ifelse(depth == 4 & n == 3, 8, depth)) %>%
#   drop_na(chloride_mgL)
# 
# 
# MO_mass2 <- MOcl__ %>% filter(n == 6) %>% left_join(MO_vols, by = c("depth", "ID"))
# MO_mass3 <- MOcl__ %>% filter(n == 3) %>% mutate(depth = ifelse(depth ==16, 20, depth)) %>% left_join(Mo_vols_0820, by = c("depth", "ID"))
# MO_mass4 <- MOcl__ %>% filter(n ==4) %>% left_join(Mo_vols_4s, by = c("depth", "ID"))
# 
# 
 MO_mass <- bind_rows(MO_mass1, MO_mass2, MO_mass3, MO_mass4) %>% dplyr::select(-n) %>% arrange(sampledate)
 write_rds(MO_mass, "Data/MO_mass.rds")
