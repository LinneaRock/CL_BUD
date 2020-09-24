check <- SaltRoutes %>%
  filter(SaltRt_Name == "W Salt Route 17" |
           SaltRt_Name == "W Salt Route 18" |
           SaltRt_Name == "W Salt Route 20" |
           SaltRt_Name == "W Salt Route 21" |
           SaltRt_Name == "W Salt Route 22" |
           SaltRt_Name == "W Salt Route 23" |
           SaltRt_Name == "W Salt Route 24" |
           SaltRt_Name == "W Salt Route 27" |
           SaltRt_Name == "W Salt Route 30")


  
  
mutate(SaltRt_Name = ifelse(mslink == 1002 |
                              mslink == 1003 |
                              mslink == 5700 |
                              mslink == 5701 |
                              mslink == 1004 |
                              mslink == 1011 |
                              mslink == 1012 
                            , "W Salt Route 14", SaltRt_Name)) %>%





West <- SaltRoutes %>%
  filter(segment_name == "BRADER WAY") 


