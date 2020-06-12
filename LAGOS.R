library(tidyverse)
library(LAGOSNE)

lagosne_get(dest_folder = lagos_path())


dt <- lagosne_load()
names(dt)

lake_info(name = "Mendota", state = "Wisconsin")
lake_info(name = "Monona", state = "Wisconsin")

lgME <- lagosne_select()