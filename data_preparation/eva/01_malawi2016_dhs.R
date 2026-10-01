##### DHS MALAWI 2016

#import library
library(tidyverse)
library(gtsummary)
library(labelled)
library(sf)

#import donnees
malawi16 <- read.csv("data/idhs_00007.csv")

#filtre sur age, usual resident
malawi16dis <- malawi16 %>% filter(AGEIND>=10 & AGEIND<=17 & HHRESIDENT==1 & HHAGE <=17)

#summary des variables module handicap
malawi16dis %>% select(DISAGECHILD,DISSEXCHILD,DISGLASSES,DISSIGHTGC,DISSIGHTNOGC,DISHEARAID,DISHEARDIFFAID,
                    DISHEARDIFFNOAID,DISWALK,DISCOG,DISSELFCARE,DISCOM) %>%  
  tbl_summary(missing_text  = "Missing")

#renommer variables
malawi16dis <- malawi16dis %>%
  rename(disability_wear_glasses = DISGLASSES, disability_sight_with_glasses = DISSIGHTGC, disability_sight_without_glasses = DISSIGHTNOGC,
         disability_use_hearing_aid = DISHEARAID, disability_hear_with_aid = DISHEARDIFFAID, disability_hear_without_aid = DISHEARDIFFNOAID,
         disability_walk = DISWALK, disability_remember_concentrate = DISCOG, disability_selfcare = DISSELFCARE,
         disability_communicate = DISCOM, disability_age = DISAGECHILD, disability_sex = DISSEXCHILD)
head(malawi16dis)


#summary
malawi16dis %>% select(disability_age, disability_sex,disability_wear_glasses, disability_sight_with_glasses, disability_sight_without_glasses, disability_use_hearing_aid,
                       disability_hear_with_aid, disability_hear_without_aid, disability_walk, disability_remember_concentrate,
                       disability_selfcare, disability_communicate) %>%  
  tbl_summary(missing_text  = "Missing")

#creation indicateur pour dimension sight
malawi16dis <- malawi16dis %>%
  mutate(disability_dim_sight = case_when(
    disability_wear_glasses == 0 & (disability_sight_without_glasses == 0 | disability_sight_without_glasses == 97) |
      (disability_wear_glasses == 1 & disability_sight_with_glasses == 0) ~ 0,
    (disability_wear_glasses == 1 & (disability_sight_with_glasses == 11 | disability_sight_with_glasses == 97 )) | 
      (disability_wear_glasses == 0 & (disability_sight_without_glasses == 10 | disability_sight_without_glasses == 11))~ 1,
    
    (disability_wear_glasses == 1 & disability_sight_with_glasses == 12) | 
      (disability_wear_glasses == 0 & disability_sight_without_glasses == 12)~ 2,
    (disability_wear_glasses == 1 & disability_sight_with_glasses == 13) | 
      (disability_wear_glasses == 0 & disability_sight_without_glasses == 13)~ 3,
    TRUE ~ NA
  )) %>% 
  set_value_labels(disability_dim_sight = c( "No" = 0, "Some difficulties" = 1, "A lot of  difficulties" =2,"Cannot do" = 3)) %>%
  set_variable_labels(disability_dim_sight = "Sight disability")

summary(malawi16dis$dis_sight)
table(malawi16dis$dis_sight)


#creation indicateur pour dimension hear
malawi16dis <- malawi16dis %>%
  mutate(disability_dim_hear = case_when(
    (disability_use_hearing_aid == 0 & (disability_hear_without_aid == 0 | disability_hear_without_aid == 97)) |
      (disability_use_hearing_aid == 7 & (disability_hear_without_aid == 0 | disability_hear_without_aid == 97)) |
      (disability_use_hearing_aid == 1 & disability_hear_with_aid == 0)
      ~ 0,
    (disability_use_hearing_aid == 1 & (disability_hear_with_aid == 11 | disability_hear_with_aid == 97 | disability_hear_with_aid == 0)) | 
      (disability_use_hearing_aid == 0 & (disability_hear_without_aid == 10 | disability_hear_without_aid == 11 )) |
      (disability_use_hearing_aid == 7 & (disability_hear_without_aid == 10 | disability_hear_without_aid == 11 ))~ 1,
    (disability_use_hearing_aid == 1 & disability_hear_with_aid == 12) | 
      (disability_use_hearing_aid == 0 & disability_hear_without_aid == 12) |
      (disability_use_hearing_aid == 7 & disability_hear_without_aid == 12)~ 2,
    (disability_use_hearing_aid == 1 & disability_hear_with_aid == 13) | 
      (disability_use_hearing_aid == 0 & disability_hear_without_aid == 13) |
      (disability_use_hearing_aid == 7 & disability_hear_without_aid == 13)~ 3,
    TRUE ~ NA
  )) %>% 
  set_value_labels(disability_dim_hear = c( "No" = 0, "Some difficulties" = 1, "A lot of  difficulties" =2,"Cannot do" = 3)) %>%
  set_variable_labels(disability_dim_hear = "Visual disability")


#verification de ceux valant 9
malawi16dis %>% filter(disability_dim_hear == 9) %>% select(AGEIND, HHAGE, HHRESIDENT, disability_use_hearing_aid, disability_hear_with_aid, disability_hear_without_aid)

#creation indicateur pour dimension communication
malawi16dis <- malawi16dis %>%
  mutate(disability_dim_communication = case_when(
    disability_communicate == 0 |disability_communicate == 97 ~ 0,
    disability_communicate == 11 | disability_communicate == 14~ 1,
    disability_communicate == 12~ 2,
    disability_communicate == 13~ 3
  )) %>% 
  set_value_labels(disability_dim_communication = c( "No" = 0, "Some difficulties" = 1, "A lot of  difficulties" =2,"Cannot do" = 3)) %>%
  set_variable_labels(disability_dim_communication = "Communication disability")

#creation indicateur pour dimension cognition
malawi16dis <- malawi16dis %>%
  mutate(disability_dim_cognition = case_when(
    disability_remember_concentrate == 0 | disability_remember_concentrate == 97~ 0,
    disability_remember_concentrate == 11 | disability_remember_concentrate == 10~ 1,
    disability_remember_concentrate == 12~ 2,
    disability_remember_concentrate == 13~ 3
  )) %>% 
  set_value_labels(disability_dim_cognition = c( "No" = 0, "Some difficulties" = 1, "A lot of  difficulties" =2,"Cannot do" = 3)) %>%
  set_variable_labels(disability_dim_cognition = "Cognitive disability")

#creation indicateur pour dimension mobility
malawi16dis <- malawi16dis %>%
  mutate(disability_dim_mobility = case_when(
    disability_walk == 0 | disability_walk == 97~ 0,
    disability_walk == 11~ 1,
    disability_walk == 12~ 2,
    disability_walk == 13~ 3
  )) %>% 
  set_value_labels(disability_dim_mobility = c( "No" = 0, "Some difficulties" = 1, "A lot of  difficulties" =2,"Cannot do" = 3)) %>%
  set_variable_labels(disability_dim_mobility = "Mobility disability")

#creation indicateur pour dimension selfcare
malawi16dis <- malawi16dis %>%
  mutate(disability_dim_selfcare = case_when(
    disability_selfcare == 0 | disability_selfcare == 97 ~ 0,
    disability_selfcare == 11 | disability_selfcare == 10 ~ 1,
    disability_selfcare == 12~ 2,
    disability_selfcare == 13~ 3
  )) %>% 
  set_value_labels(disability_dim_selfcare = c( "No" = 0, "Some difficulties" = 1, "A lot of  difficulties" =2,"Cannot do" = 3)) %>%
  set_variable_labels(disability_dim_selfcare = "Selfcare disability")


#creation indicateur au - 1 handicap
malawi16dis <- malawi16dis %>%
  mutate(disability_at_least_one = case_when(
    disability_dim_sight == 1 | disability_dim_sight == 2 | disability_dim_sight == 3 |
      disability_dim_hear == 1 | disability_dim_hear == 2 | disability_dim_hear == 3 |
      disability_dim_communication != 0 | disability_dim_cognition != 0 | 
      disability_dim_mobility != 0 | disability_dim_selfcare != 0~ 1,
    TRUE~0
  )) %>%
  set_value_labels(disability_at_least_one = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(disability_at_least_one = "Any disability")

#creation indicateur au - 1 handicap severe
malawi16dis <- malawi16dis %>%
  mutate(disability_any_severe_one = case_when(
    disability_dim_sight == 2 | disability_dim_sight == 3 |
      disability_dim_hear == 2 | disability_dim_hear == 3 |
      disability_dim_communication == 2 | disability_dim_communication == 3 |
      disability_dim_cognition == 2 | disability_dim_cognition == 3 |
      disability_dim_mobility == 2 | disability_dim_mobility == 3 |
      disability_dim_selfcare == 2 | disability_dim_selfcare == 3~ 1,
    TRUE~0
  ))  %>%
  set_value_labels(disability_any_severe_one = c("Yes" = 1, "No" = 0)) %>%
  set_variable_labels(disability_any_severe_one = "Severe disability")



#nommer regions
malawi16dis <- malawi16dis %>%
  mutate(region = case_when(
    GEO_MW2016 == 1 ~ "Northern",
    GEO_MW2016 == 2 ~ "Central",
    GEO_MW2016 == 3 ~ "Southern"))


#nommer districts
malawi16dis <- malawi16dis %>%
  mutate(district = case_when(
    GEOALT_MW2016 == 101 ~ "Chitipa",
    GEOALT_MW2016 == 102 ~ "Karonga",
    GEOALT_MW2016 == 103 ~ "Nkhatabay",
    GEOALT_MW2016 == 104 ~ "Rumphi",
    GEOALT_MW2016 == 105 ~ "Mzimba",
    GEOALT_MW2016 == 106 ~ "Likoma",
    GEOALT_MW2016 == 107 ~ "Mzuzu City",
    GEOALT_MW2016 == 201 ~ "Kasungu",
    GEOALT_MW2016 == 202 ~ "Nkhotakota",
    GEOALT_MW2016 == 203 ~ "Ntchisi",
    GEOALT_MW2016 == 204 ~ "Dowa",
    GEOALT_MW2016 == 205 ~ "Salima",
    GEOALT_MW2016 == 206 ~ "Lilongwe",
    GEOALT_MW2016 == 207 ~ "Mchinji",
    GEOALT_MW2016 == 208 ~ "Dedza",
    GEOALT_MW2016 == 209 ~ "Ntcheu",
    GEOALT_MW2016 == 210 ~ "Lilongwe City",
    GEOALT_MW2016 == 301 ~ "Mangochi",
    GEOALT_MW2016 == 302 ~ "Machinga",
    GEOALT_MW2016 == 303 ~ "Zomba",
    GEOALT_MW2016 == 304 ~ "Chiradzulu",
    GEOALT_MW2016 == 305 ~ "Blantyre",
    GEOALT_MW2016 == 306 ~ "Mwanza",
    GEOALT_MW2016 == 307 ~ "Thyolo",
    GEOALT_MW2016 == 308 ~ "Mulanje",
    GEOALT_MW2016 == 309 ~ "Phalombe",
    GEOALT_MW2016 == 310 ~ "Chikwawa",
    GEOALT_MW2016 == 311 ~ "Nsanje",
    GEOALT_MW2016 == 312 ~ "Balaka",
    GEOALT_MW2016 == 313 ~ "Neno",
    GEOALT_MW2016 == 314 ~ "Zomba City",
    GEOALT_MW2016 == 315 ~ "Blantyre City"
  ))

#import shapefile region
regions<-st_read("shapefile/mwi_admbnda_adm1_nso_hotosm_20230405.shp")

#import shapefile district
districts<-st_read("shapefile/mwi_admbnda_adm2_nso_hotosm_20230405.shp")

#preparation table merge region
malawi16dis_region <- malawi16dis %>% select(region, disability_at_least_one, disability_any_severe_one,HHWEIGHT) %>%   
  group_by(region) %>% summarise(total_at_least_one_dis = sum(disability_at_least_one*HHWEIGHT), 
                                 total_any_severe_dis = sum(disability_any_severe_one*HHWEIGHT), 
                                 total_pop = sum(HHWEIGHT)) %>% 
  mutate(freq_at_least_one_dis = (total_at_least_one_dis/total_pop)*100, 
         freq_any_severe_dis =(total_any_severe_dis/total_pop)*100)

regions <- regions %>% rename(region = ADM1_EN)
malawi16dis_region <- merge(regions, malawi16dis_region, by="region")

# conversion des données labellisées
malawi16dis <- malawi16dis %>% unlabelled() # conversion en facteurs

#summary des indicateurs
malawi16dis %>% select(disability_dim_sight, disability_dim_hear, disability_dim_communication, 
                       disability_dim_cognition, disability_dim_mobility, disability_dim_selfcare, 
                       disability_at_least_one, disability_any_severe_one) %>%  
  tbl_summary(missing_text  = "Missing")

# sauvagarde des fichiers --------
save(malawi16dis,malawi16dis_region,regions,districts, file = "data/after1.RData")




