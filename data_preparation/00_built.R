# 00_built


source("data_preparation/01_import.R")
source("data_preparation/02_recoding_dhs.R")
source("data_preparation/03_recoding_mics.R")

save(
  dhs_recoded, # DHS surveys
  mics_recoded, # MICS surveys
  file = "data/data_atlas.RData")

