library(DataCombine)
#Function to calculate land use percetages for each subwatershed (HUC12) area using LAGOS LULC data combined with data on the area of each subwatershed

LandUse <- function(lulcdata) {
  
  #Calculate the area for the watershed and each land use [hectare]:
  WatershedArea <- lulcdata %>%
    mutate(WatershedArea = sum(hu12_ha_in_usa)) %>%
    select(WatershedArea) %>%
    distinct()
  
  OpenWater <- lulcdata %>%
    mutate(OpenWater = sum(hu12_nlcd2011_ha_11)) %>%
    select(OpenWater) %>%
    distinct()
  
  DevelopedOpenSpace <- lulcdata %>%
    mutate(DevelopedOpenSpace = sum(hu12_nlcd2011_ha_21)) %>%
    select(DevelopedOpenSpace) %>%
    distinct()
  
  DevelopedLowIntensity <- lulcdata %>%
    mutate(DevelopedLowIntensity = sum(hu12_nlcd2011_ha_22)) %>%
    select(DevelopedLowIntensity) %>%
    distinct()
  
  DevelopedMedIntensity <- lulcdata %>%
    mutate(DevelopedMedIntensity = sum(hu12_nlcd2011_ha_23)) %>%
    select(DevelopedMedIntensity) %>% 
    distinct()
  
  DevelopedHighIntensity <- lulcdata %>%
    mutate(DevelopedHighIntensity = sum(hu12_nlcd2011_ha_24)) %>%
    select(DevelopedHighIntensity) %>%
    distinct()
  
  BarrenLand <- lulcdata %>%
    mutate(BarrenLand = sum(hu12_nlcd2011_ha_31)) %>%
    select(BarrenLand) %>%
    distinct()
  
  DeciduousForest <- lulcdata %>%
    mutate(DeciduousForest = sum(hu12_nlcd2011_ha_41)) %>%
    select(DeciduousForest) %>%
    distinct()
  
  EvergreenForest <- lulcdata %>%
    mutate(EvergreenForest = sum(hu12_nlcd2011_ha_42)) %>%
    select(EvergreenForest) %>%
    distinct()
  
  MixedForest <- lulcdata %>%
    mutate(MixedForest = sum(hu12_nlcd2011_ha_43)) %>%
    select(MixedForest) %>%
    distinct()
  
  
  ShrubScrub <- lulcdata %>%
    mutate(ShrubScrub = sum(hu12_nlcd2011_ha_52)) %>%
    select(ShrubScrub) %>%
    distinct()
  
  GrasslandHerb <- lulcdata %>%
    mutate(GrasslandHerb = sum(hu12_nlcd2011_ha_71)) %>%
    select(GrasslandHerb) %>%
    distinct()
  
  PastureHay <- lulcdata %>%
    mutate(PastureHay = sum(hu12_nlcd2011_ha_81)) %>%
    select(PastureHay) %>%
    distinct()
  
  CultivatedCrop <- lulcdata %>%
    mutate(CultivatedCrop = sum(hu12_nlcd2011_ha_82)) %>%
    select(CultivatedCrop) %>%
    distinct()
  
  WoodyWetlands <- lulcdata %>%
    mutate(WoodyWetlands = sum(hu12_nlcd2011_ha_90)) %>%
    select(WoodyWetlands) %>%
    distinct()
  
  EmergentWetlands <- lulcdata %>%
    mutate(EmergentWetlands = sum(hu12_nlcd2011_ha_95)) %>%
    select(EmergentWetlands) %>%
    distinct()
  
  #build dataframe 
  newrow <- rep(NA, 16) #percentages will be in the second row, this is an empty row to add to the dataframe
  
  df <- WatershedArea %>%
    cbind(c(OpenWater, DevelopedOpenSpace, DevelopedLowIntensity, DevelopedMedIntensity, DevelopedHighIntensity, BarrenLand, DeciduousForest, EvergreenForest, MixedForest, ShrubScrub, GrasslandHerb, PastureHay, CultivatedCrop, WoodyWetlands, EmergentWetlands)) %>%
    InsertRow(newrow)

  #Percentages calcualted
  for(i in 2:nrow(df)) {
    for(j in 1:ncol(df)) {
      df[i,j] = (df[i-1, j]/WatershedArea) * 100
    }
  }
  
  
  return(df)
  
}













