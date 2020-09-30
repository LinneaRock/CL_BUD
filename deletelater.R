check <- SaltRoutes %>%
  filter(segment_name == "VALLEY VIEW RD")


mutate(SaltRt_Name = ifelse(mslink ==	6844 |
                              mslink == 6846 |
                              segment_name == "N WHITNEY WAY"
              , "W Salt Route 8", SaltRt_Name)) %>% #manually adding segments for w route 8
  




WestCHCK <- road_info %>%
  filter(SaltRt_Name == "W Salt Route 2") 


look <- W_Map_Geo %>%
  filter(SaltRt_Name == "W Salt Route 9" |
    SaltRt_Name == "W Salt Route 10" |
    SaltRt_Name == "W Salt Route 11" |
    SaltRt_Name == "W Salt Route 12" |
    SaltRt_Name == "W Salt Route 13"|
           SaltRt_Name == "W Salt Route 14" |
           SaltRt_Name == "W Salt Route 15" |
           SaltRt_Name == "W Salt Route 16")
