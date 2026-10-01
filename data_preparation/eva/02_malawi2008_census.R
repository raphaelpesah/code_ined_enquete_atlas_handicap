##### INTERNATIONAL MALAWI 2008

#import library
library(tidyverse)
library(gtsummary)

# load data
load("data/after1.RData")

#import donnees
malawi08_census <- read.csv("data/ipumsi_00005.csv")
head(malawi08_census)

#table regroupement par districts
malawi08_district <- malawi08_census %>% select(GEO1_MW2008, AGE, SEX, PERSONS, URBAN, YRSCHOOL, BEDROOMS) %>%   
  group_by(GEO1_MW2008) %>% summarise(mean_age = mean(AGE[AGE != 999]), 
                                 prop_female = sum(SEX == 2) / n(), 
                                 mean_nbpersons = mean(PERSONS),
                                 prop_rural = sum(URBAN == 1) / n(),
                                 mean_yearsschool = mean(YRSCHOOL[!(YRSCHOOL %in% c(90,91,92,93,94,95,96,97,98,99))]),
                                 mean_nbbedrooms = mean(BEDROOMS[!(BEDROOMS %in% c(98,99))]) ) 


#nommer districts
malawi08_district <- malawi08_district %>%
  mutate(district2 = case_when(
    GEO1_MW2008 == 101 ~ "Chitipa",
    GEO1_MW2008 == 102 ~ "Karonga",
    GEO1_MW2008 == 103 ~ "Nkhatabay, Likoma",
    GEO1_MW2008 == 104 ~ "Rumphi",
    GEO1_MW2008 == 105 ~ "Mzimba",
    GEO1_MW2008 == 107 ~ "Mzuzu City",
    GEO1_MW2008 == 201 ~ "Kasungu",
    GEO1_MW2008 == 202 ~ "Nkhotakota",
    GEO1_MW2008 == 203 ~ "Ntchisi",
    GEO1_MW2008 == 204 ~ "Dowa",
    GEO1_MW2008 == 205 ~ "Salima",
    GEO1_MW2008 == 206 ~ "Lilongwe",
    GEO1_MW2008 == 207 ~ "Mchinji",
    GEO1_MW2008 == 208 ~ "Dedza",
    GEO1_MW2008 == 209 ~ "Ntcheu",
    GEO1_MW2008 == 210 ~ "Lilongwe City",
    GEO1_MW2008 == 301 ~ "Mangochi",
    GEO1_MW2008 == 302 ~ "Machinga",
    GEO1_MW2008 == 303 ~ "Zomba",
    GEO1_MW2008 == 304 ~ "Chiradzulu",
    GEO1_MW2008 == 305 ~ "Blantyre",
    GEO1_MW2008 == 306 ~ "Mwanza",
    GEO1_MW2008 == 307 ~ "Thyolo",
    GEO1_MW2008 == 308 ~ "Mulanje",
    GEO1_MW2008 == 309 ~ "Phalombe",
    GEO1_MW2008 == 310 ~ "Chikwawa",
    GEO1_MW2008 == 311 ~ "Nsanje",
    GEO1_MW2008 == 312 ~ "Balaka",
    GEO1_MW2008 == 313 ~ "Neno",
    GEO1_MW2008 == 314 ~ "Zomba City",
    GEO1_MW2008 == 315 ~ "Blantyre City"
  ))


# sauvagarde des fichiers --------
save(malawi16dis,malawi16dis_region,regions,districts,malawi08_district,  file = "data/malawi_dt.RData")
