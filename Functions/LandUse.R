library(DataCombine)
#Function to calculate land use percetages for each subwatershed (HUC12) area using LAGOS LULC data combined with data on the area of each subwatershed

LandUse <- function(lulcdata) {
  
  #Calculate the area for the watershed and each land use [hectare]:
  WatershedArea <- lulcdata %>%
    mutate(WatershedArea = sum(hu12_ha_in_usa)) %>%
    dplyr::select(WatershedArea) %>%
    distinct()
  
  OpenWater <- lulcdata %>%
    mutate(OpenWater = sum(hu12_nlcd2011_ha_11)) %>%
    dplyr::select(OpenWater) %>%
    distinct()
  
  DevelopedOpenSpace <- lulcdata %>%
    mutate(DevelopedOpenSpace = sum(hu12_nlcd2011_ha_21)) %>%
    dplyr::select(DevelopedOpenSpace) %>%
    distinct()
  
  DevelopedLowIntensity <- lulcdata %>%
    mutate(DevelopedLowIntensity = sum(hu12_nlcd2011_ha_22)) %>%
    dplyr::select(DevelopedLowIntensity) %>%
    distinct()
  
  DevelopedMedIntensity <- lulcdata %>%
    mutate(DevelopedMedIntensity = sum(hu12_nlcd2011_ha_23)) %>%
    dplyr::select(DevelopedMedIntensity) %>% 
    distinct()
  
  DevelopedHighIntensity <- lulcdata %>%
    mutate(DevelopedHighIntensity = sum(hu12_nlcd2011_ha_24)) %>%
    dplyr::select(DevelopedHighIntensity) %>%
    distinct()
  
  BarrenLand <- lulcdata %>%
    mutate(BarrenLand = sum(hu12_nlcd2011_ha_31)) %>%
    dplyr::select(BarrenLand) %>%
    distinct()
  
  DeciduousForest <- lulcdata %>%
    mutate(DeciduousForest = sum(hu12_nlcd2011_ha_41)) %>%
    dplyr::select(DeciduousForest) %>%
    distinct()
  
  EvergreenForest <- lulcdata %>%
    mutate(EvergreenForest = sum(hu12_nlcd2011_ha_42)) %>%
    dplyr::select(EvergreenForest) %>%
    distinct()
  
  MixedForest <- lulcdata %>%
    mutate(MixedForest = sum(hu12_nlcd2011_ha_43)) %>%
    dplyr::select(MixedForest) %>%
    distinct()
  
  
  ShrubScrub <- lulcdata %>%
    mutate(ShrubScrub = sum(hu12_nlcd2011_ha_52)) %>%
    dplyr::select(ShrubScrub) %>%
    distinct()
  
  GrasslandHerb <- lulcdata %>%
    mutate(GrasslandHerb = sum(hu12_nlcd2011_ha_71)) %>%
    dplyr::select(GrasslandHerb) %>%
    distinct()
  
  PastureHay <- lulcdata %>%
    mutate(PastureHay = sum(hu12_nlcd2011_ha_81)) %>%
    dplyr::select(PastureHay) %>%
    distinct()
  
  CultivatedCrop <- lulcdata %>%
    mutate(CultivatedCrop = sum(hu12_nlcd2011_ha_82)) %>%
    dplyr::select(CultivatedCrop) %>%
    distinct()
  
  WoodyWetlands <- lulcdata %>%
    mutate(WoodyWetlands = sum(hu12_nlcd2011_ha_90)) %>%
    dplyr::select(WoodyWetlands) %>%
    distinct()
  
  EmergentWetlands <- lulcdata %>%
    mutate(EmergentWetlands = sum(hu12_nlcd2011_ha_95)) %>%
    dplyr::select(EmergentWetlands) %>%
    distinct()
  
  TotalRoad <- lulcdata %>%
    mutate(TotalRoad = (sum(hu12_roaddensity_sum_lengthm) / 100)) %>% #dividing by 100 for ease in a later step of multiplying the entire row by 100
    dplyr::select(TotalRoad) %>%
    distinct()
  
  
  #build dataframe 
  newrow <- rep(NA, 17) #percentages will be in the second row, this is an empty row to add to the dataframe
  
  df <- WatershedArea %>%
    cbind(c(OpenWater, DevelopedOpenSpace, DevelopedLowIntensity, DevelopedMedIntensity, DevelopedHighIntensity, BarrenLand, DeciduousForest, EvergreenForest, MixedForest, ShrubScrub, GrasslandHerb, PastureHay, CultivatedCrop, WoodyWetlands, EmergentWetlands, TotalRoad)) %>%
    InsertRow(newrow)

  #Percentages calcualted
  for(i in 2:nrow(df)) {
    for(j in 1:ncol(df)) {
      df[i,j] = (df[i-1, j]/WatershedArea) * 100
    }
  }
  
  
  return(df)
  
}


SimpleLandUse <- function(calculated_land_use) {
  
  calculated_land_use <- calculated_land_use[-1, ]
  
  use <- data.frame(matrix(ncol = 6, nrow = 1))
  colnames(use) <- c("Water", "DevelopedLand", "Forest", "Shrub_Grass_BarrenLand", "Agricultural", "Wetland")
  
  use <- use %>%
    mutate(Water = calculated_land_use$OpenWater,
           DevelopedLand = calculated_land_use$DevelopedOpenSpace + calculated_land_use$DevelopedLowIntensity + calculated_land_use$DevelopedMedIntensity + calculated_land_use$DevelopedHighIntensity,
           Forest = calculated_land_use$DeciduousForest + calculated_land_use$EvergreenForest + calculated_land_use$MixedForest,
           Shrub_Grass_BarrenLand = calculated_land_use$BarrenLand + calculated_land_use$ShrubScrub + calculated_land_use$GrasslandHerb,
           Agricultural = calculated_land_use$PastureHay + calculated_land_use$CultivatedCrop,
           Wetland = calculated_land_use$WoodyWetlands + calculated_land_use$EmergentWetlands)
  
}



LandUse2 <- function(lulcdata) {
  
  data <- lulcdata %>%
    rename(WatershedArea = hu12_ha_in_usa,
           OpenWater = hu12_nlcd2011_ha_11,
           DevelopedOpenSpace = hu12_nlcd2011_ha_21,
           DevelopedLowIntensity = hu12_nlcd2011_ha_22,
           DevelopedMedIntensity = hu12_nlcd2011_ha_23,
           DevelopedHighIntensity = hu12_nlcd2011_ha_24,
           BarrenLand = hu12_nlcd2011_ha_31,
           DeciduousForest = hu12_nlcd2011_ha_41,
           EvergreenForest = hu12_nlcd2011_ha_42,
           MixedForest = hu12_nlcd2011_ha_43,
           ShrubScrub = hu12_nlcd2011_ha_52,
           GrasslandHerb = hu12_nlcd2011_ha_71,
           PastureHay = hu12_nlcd2011_ha_81,
           CultivatedCrop = hu12_nlcd2011_ha_82,
           WoodyWetlands = hu12_nlcd2011_ha_90,
           EmergentWetlands = hu12_nlcd2011_ha_95,
           TotalRoadLength = hu12_roaddensity_sum_lengthm,
           TotalRoadDensity = hu12_roaddensity_density_mperha) %>%
    select(Name, HUC12, WatershedArea, OpenWater,DevelopedOpenSpace, DevelopedLowIntensity, DevelopedMedIntensity, DevelopedHighIntensity,
           BarrenLand, DeciduousForest, EvergreenForest, MixedForest, GrasslandHerb, PastureHay, CultivatedCrop,
           WoodyWetlands, EmergentWetlands,TotalRoadLength, TotalRoadDensity)
  
  

  
  #build dataframe 
  newrow <- rep(NA, 17) #percentages will be in the second row, this is an empty row to add to the dataframe
  
  df <- WatershedArea %>%
    cbind(c(OpenWater, DevelopedOpenSpace, DevelopedLowIntensity, DevelopedMedIntensity, DevelopedHighIntensity, BarrenLand, DeciduousForest, EvergreenForest, MixedForest, ShrubScrub, GrasslandHerb, PastureHay, CultivatedCrop, WoodyWetlands, EmergentWetlands, TotalRoad)) %>%
    InsertRow(newrow)
  
  #Percentages calcualted
  for(i in 2:nrow(df)) {
    for(j in 1:ncol(df)) {
      df[i,j] = (df[i-1, j]/WatershedArea) * 100
    }
  }
  
  
  return(df)
  
}




