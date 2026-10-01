# Recoding of MICS surveys


# 0) Loading packages and RData from 01_import ----

library(srvyr) # loaded before tidyverse to give priority to tidyverse version of functions like filter
library(tidyverse)
library(labelled)
library(survey)
library(questionr)
library(sf)

load("data/tmp/after03.RData")
# open the list containing data in the environment as dataframe separated for each country
list2env(lsms, envir = .GlobalEnv) 

# 0bis) Merging of datasets ----
# Note that for every country, we put at the left of the left_join() the file that has the more observations 
# (We favor the health file when it has the same number of observations as other files like "indiv_charact", put the "roster" file at the left when it is the one with the most observations)

## 0bis.A) benin21lsms ----

benin21lsms_health <- benin21lsms_health |> unite("householdid", c("grappe", "menage"), remove = F)
benin21lsms_health <- benin21lsms_health |> unite("id", c("grappe", "menage", "membres__id"), remove = F)

benin21lsms_indiv_charact <- benin21lsms_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

benin21lsms_cover <- benin21lsms_cover |> unite("householdid", c("grappe", "menage"), remove = T)
benin21lsms_weighting <- benin21lsms_weighting |> unite("householdid", c("grappe", "menage"), remove = T)

benin21lsms <- benin21lsms_health |> left_join(benin21lsms_indiv_charact, c("id" = "id"))
benin21lsms <- benin21lsms |> left_join(benin21lsms_cover, c("householdid" = "householdid"))
benin21lsms <- benin21lsms |> left_join(benin21lsms_weighting, c("householdid" = "householdid"))

## 0bis.B) benin18 ----

benin18_health <- benin18_health |> unite("householdid", c("grappe", "menage"), remove = F)
benin18_health <- benin18_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = F)

benin18_indiv_charact <- benin18_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

benin18_cover <- benin18_cover |> unite("householdid", c("grappe", "menage"), remove = T)

benin18 <- benin18_health |> left_join(benin18_indiv_charact, c("id" = "id"))
benin18 <- benin18 |> left_join(benin18_cover, c("householdid" = "householdid"))
benin18 <- benin18 |> left_join(benin18_weighting, c("grappe" = "grappe"))
benin18 <- benin18 |> left_join(benin18_gps, c("grappe" = "grappe"))

## 0bis.C) burki21cross ----

burki21cross_health <- burki21cross_health |> unite("householdid", c("grappe", "menage"), remove = F)
burki21cross_health <- burki21cross_health |> unite("id", c("grappe", "menage", "pid"), remove = F)

burki21cross_indiv_charact <- burki21cross_indiv_charact |> unite("id", c("grappe", "menage", "pid"), remove = T)

burki21cross_cover <- burki21cross_cover |> unite("householdid", c("grappe", "menage"), remove = T)

burki21cross <- burki21cross_health |> left_join(burki21cross_indiv_charact, c("id" = "id"))
burki21cross <- burki21cross |> left_join(burki21cross_cover, c("householdid" = "householdid"))
burki21cross <- burki21cross |> left_join(burki21cross_weighting, c("grappe" = "grappe"))


## 0bis.D) burki21panel ----

burki21panel_health <- burki21panel_health |> unite("householdid", c("grappe", "menage"), remove = F)
burki21panel_health <- burki21panel_health |> unite("id", c("grappe", "menage", "pid"), remove = F)

burki21panel_indiv_charact <- burki21panel_indiv_charact |> unite("id", c("grappe", "menage", "pid"), remove = T)

burki21panel_cover <- burki21panel_cover |> unite("householdid", c("grappe", "menage"), remove = T)

burki21panel <- burki21panel_health |> left_join(burki21panel_indiv_charact, c("id" = "id"))
burki21panel <- burki21panel |> left_join(burki21panel_cover, c("householdid" = "householdid"))
burki21panel <- burki21panel |> left_join(burki21panel_weighting, c("grappe" = "grappe"))


## 0bis.E) burki18 ----

burki18_health <- burki18_health |> unite("householdid", c("grappe", "menage"), remove = F)
burki18_health <- burki18_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = F)

burki18_indiv_charact <- burki18_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

burki18_cover <- burki18_cover |> unite("householdid", c("grappe", "menage"), remove = T)

burki18 <- burki18_health |> left_join(burki18_indiv_charact, c("id" = "id"))
burki18 <- burki18 |> left_join(burki18_cover, c("householdid" = "householdid"))
burki18 <- burki18 |> left_join(burki18_weighting, c("grappe" = "grappe"))
burki18 <- burki18 |> left_join(burki18_gps, c("grappe" = "grappe"))

## 0bis.F) cotediv21 ----

cotediv21_health <- cotediv21_health |> unite("householdid", c("grappe", "menage"), remove = F)
cotediv21_health <- cotediv21_health |> unite("id", c("grappe", "menage", "membres__id"), remove = F)

cotediv21_indiv_charact <- cotediv21_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

cotediv21_cover <- cotediv21_cover |> unite("householdid", c("grappe", "menage"), remove = T)
cotediv21_weighting <- cotediv21_weighting |> unite("householdid", c("grappe", "menage"), remove = T)

cotediv21 <- cotediv21_health |> left_join(cotediv21_indiv_charact, c("id" = "id"))
cotediv21 <- cotediv21 |> left_join(cotediv21_cover, c("householdid" = "householdid"))
cotediv21 <- cotediv21 |> left_join(cotediv21_weighting, c("householdid" = "householdid"))

## 0bis.G) cotediv18 ----

cotediv18_health <- cotediv18_health |> unite("householdid", c("grappe", "menage"), remove = F)
cotediv18_health <- cotediv18_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = F)

cotediv18_indiv_charact <- cotediv18_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

cotediv18_cover <- cotediv18_cover |> unite("householdid", c("grappe", "menage"), remove = T)

cotediv18 <- cotediv18_health |> left_join(cotediv18_indiv_charact, c("id" = "id"))
cotediv18 <- cotediv18 |> left_join(cotediv18_cover, c("householdid" = "householdid"))
cotediv18 <- cotediv18 |> left_join(cotediv18_weighting, c("grappe" = "grappe"))
cotediv18 <- cotediv18 |> left_join(cotediv18_gps, c("grappe" = "grappe"))

## 0bis.H) ethiop21 ----

ethiop21_health <- ethiop21_health |> unite("householdid", c("ea_id", "household_id"), remove = F)
ethiop21_health <- ethiop21_health |> unite("id", c("ea_id", "household_id", "individual_id"), remove = T)

ethiop21_roster <- ethiop21_roster |> unite("id", c("ea_id", "household_id", "individual_id"), remove = F)

ethiop21_cover <- ethiop21_cover |> unite("householdid", c("ea_id", "household_id"), remove = T)

ethiop21 <- ethiop21_roster |> left_join(ethiop21_health, c("id" = "id"))
ethiop21 <- ethiop21 |> left_join(ethiop21_cover, c("householdid" = "householdid"))

## 0bis.I) ethiop19 ----

ethiop19_health <- ethiop19_health |> unite("householdid", c("ea_id", "household_id"), remove = F)
ethiop19_health <- ethiop19_health |> unite("id", c("ea_id", "household_id", "individual_id"), remove = F)

ethiop19_roster <- ethiop19_roster |> unite("id", c("ea_id", "household_id", "individual_id"), remove = T)

ethiop19_cover <- ethiop19_cover |> unite("householdid", c("ea_id", "household_id"), remove = T)
ethiop19_gps_ph <- ethiop19_gps_ph |> unite("householdid", c("ea_id", "household_id"), remove = T)
ethiop19_gps_pp <- ethiop19_gps_pp |> unite("householdid", c("ea_id", "household_id"), remove = T)
ethiop19_gps_ls <- ethiop19_gps_ls |> unite("householdid", c("ea_id", "household_id"), remove = T)

ethiop19 <- ethiop19_health |> left_join(ethiop19_roster, c("id" = "id"))
ethiop19 <- ethiop19 |> left_join(ethiop19_cover, c("householdid" = "householdid"))
# We get a warning message Each row in `x` is expected to match at most 1 row in `y`. so for now we leave it commented
#ethiop19 <- ethiop19 |> left_join(ethiop19_gps_ph, c("householdid" = "householdid"))
#ethiop19 <- ethiop19 |> left_join(ethiop19_gps_pp, c("householdid" = "householdid"))
#ethiop19 <- ethiop19 |> left_join(ethiop19_gps_ls, c("householdid" = "householdid"))

## 0bis.J) ethiop16 ----

ethiop16_health <- ethiop16_health |> unite("householdid", c("ea_id2", "household_id2"), remove = F)
ethiop16_health <- ethiop16_health |> unite("id", c("ea_id2", "household_id2", "individual_id2"), remove = T)

ethiop16_roster <- ethiop16_roster |> unite("id", c("ea_id2", "household_id2", "individual_id2"), remove = F)

ethiop16_cover <- ethiop16_cover |> unite("householdid", c("ea_id2", "household_id2"), remove = T)

ethiop16 <- ethiop16_roster |> left_join(ethiop16_health, c("id" = "id"))
ethiop16 <- ethiop16 |> left_join(ethiop16_cover, c("householdid" = "householdid"))

## 0bis.K) ethiop14 ----

ethiop14_health <- ethiop14_health |> unite("householdid", c("ea_id2", "household_id2"), remove = F)
ethiop14_health <- ethiop14_health |> unite("id", c("ea_id2", "household_id2", "individual_id2"), remove = T)

ethiop14_roster <- ethiop14_roster |> unite("id", c("ea_id2", "household_id2", "individual_id2"), remove = F)

ethiop14_cover <- ethiop14_cover |> unite("householdid", c("ea_id2", "household_id2"), remove = T)

ethiop14 <- ethiop14_roster |> left_join(ethiop14_health, c("id" = "id"))
ethiop14 <- ethiop14 |> left_join(ethiop14_cover, c("householdid" = "householdid"))

## 0bis.L) ethiop12 ----

ethiop12_health <- ethiop12_health |> unite("householdid", c("ea_id", "household_id"), remove = F)
ethiop12_health <- ethiop12_health |> unite("id", c("ea_id", "household_id", "individual_id"), remove = T)

ethiop12_roster <- ethiop12_roster |> unite("id", c("ea_id", "household_id", "individual_id"), remove = F)

ethiop12_cover <- ethiop12_cover |> unite("householdid", c("ea_id", "household_id"), remove = T)
ethiop12_gps <- ethiop12_gps |> unite("householdid", c("ea_id", "household_id"), remove = T)

ethiop12 <- ethiop12_roster |> left_join(ethiop12_health, c("id" = "id"))
ethiop12 <- ethiop12 |> left_join(ethiop12_cover, c("householdid" = "householdid"))
ethiop12 <- ethiop12 |> left_join(ethiop12_gps, c("householdid" = "householdid"))

## 0bis.M) guibi21 ----

guibi21_health <- guibi21_health |> unite("householdid", c("grappe", "menage"), remove = F)
guibi21_health <- guibi21_health |> unite("id", c("grappe", "menage", "membres__id"), remove = T)

guibi21_indiv_charact <- guibi21_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = F)

guibi21_cover <- guibi21_cover |> unite("householdid", c("grappe", "menage"), remove = T)
guibi21_weighting <- guibi21_weighting |> unite("householdid", c("grappe", "menage"), remove = T)

guibi21 <- guibi21_indiv_charact |> left_join(guibi21_health, c("id" = "id"))
guibi21 <- guibi21 |> left_join(guibi21_cover, c("householdid" = "householdid"))
guibi21 <- guibi21 |> left_join(guibi21_weighting, c("householdid" = "householdid"))

## 0bis.N) guibi19 ----

guibi19_health <- guibi19_health |> unite("householdid", c("grappe", "menage"), remove = F)
guibi19_health <- guibi19_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = F)

guibi19_indiv_charact <- guibi19_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

guibi19_cover <- guibi19_cover |> unite("householdid", c("grappe", "menage"), remove = T)

guibi19 <- guibi19_health |> left_join(guibi19_indiv_charact, c("id" = "id"))
guibi19 <- guibi19 |> left_join(guibi19_cover, c("householdid" = "householdid"))
guibi19 <- guibi19 |> left_join(guibi19_weighting, c("grappe" = "grappe"))
guibi19 <- guibi19 |> left_join(guibi19_gps |> distinct(grappe, .keep_all = T), c("grappe" = "grappe")) # We had "grappe" that where present multiple times in guibi19_gps (that we checked with "guibi19_gps |> group_by(grappe) %>% filter(n() > 1) |> count(grappe) |>  ungroup()")
# We used "guibi19_gps_complete |> filter(grappe == 155325 | grappe == 155426 | grappe == 199719 | grappe == 199816 | grappe == 1771231) |> View()" to confirm that the duplicated rows had the same values for all their variables, including the gps coordinates
# And so with "guibi19_gps |> distinct(grappe, .keep_all = T)" we remove the duplicated rows

## 0bis.O) malawi20 ----

#No need for this line here: malawi20_health <- malawi20_health |> unite("householdid", c("case_id"), remove = F)
malawi20_health <- malawi20_health |> unite("id", c("case_id", "PID"), remove = F)

malawi20_roster <- malawi20_roster |> unite("id", c("case_id", "PID"), remove = T)

#No need for this line here: malawi20_ <- malawi20_ |> unite("householdid", c("", ""), remove = T)

malawi20 <- malawi20_health |> left_join(malawi20_roster, c("id" = "id"))
malawi20 <- malawi20 |> left_join(malawi20_cover, c("case_id" = "case_id"))
malawi20 <- malawi20 |> left_join(malawi20_gps, c("case_id" = "case_id"))

## 0bis.P) malawi19lsms ----

#No need for this line here: malawi19lsms_health <- malawi19lsms_health |> unite("householdid", c("", ""), remove = F)
malawi19lsms_health <- malawi19lsms_health |> unite("id", c("y4_hhid", "PID"), remove = F)

malawi19lsms_roster <- malawi19lsms_roster |> unite("id", c("y4_hhid", "PID"), remove = T)

#No need for this line here: malawi19lsms_ <- malawi19lsms_ |> unite("householdid", c("", ""), remove = T)

malawi19lsms <- malawi19lsms_health |> left_join(malawi19lsms_roster, c("id" = "id"))
malawi19lsms <- malawi19lsms |> left_join(malawi19lsms_cover, c("y4_hhid" = "y4_hhid"))
malawi19lsms <- malawi19lsms |> left_join(malawi19lsms_gps, c("y4_hhid" = "y4_hhid"))

## 0bis.Salpha) malawi11fs ----

malawi11fs_health <- malawi11fs_health |> unite("householdid", c("ea_id", "case_id"), remove = F)
malawi11fs_health <- malawi11fs_health |> unite("id", c("ea_id", "case_id", "id_code"), remove = T)

malawi11fs_roster <- malawi11fs_roster |> unite("id", c("ea_id", "case_id", "id_code"), remove = F)

malawi11fs_cover <- malawi11fs_cover |> unite("householdid", c("ea_id", "case_id"), remove = T)
malawi11_gps <- malawi11_gps |> unite("householdid", c("ea_id", "case_id"), remove = T)
malawi11_cons <- malawi11_cons |> unite("householdid", c("ea_id", "case_id"), remove = T)

malawi11fs <- malawi11fs_roster |> left_join(malawi11fs_health, c("id" = "id"))
malawi11fs <- malawi11fs |> left_join(malawi11fs_cover, c("householdid" = "householdid"))
malawi11fs <- malawi11fs |> left_join(malawi11_gps, c("householdid" = "householdid"))
malawi11fs <- malawi11fs |> left_join(malawi11_cons, c("householdid" = "householdid"))

## 0bis.Sbeta) malawi11panel ----

malawi11panel_health <- malawi11panel_health |> unite("householdid", c("ea_id", "case_id"), remove = F)
malawi11panel_health <- malawi11panel_health |> unite("id", c("ea_id", "case_id", "id_code"), remove = F)

malawi11panel_roster <- malawi11panel_roster |> unite("id", c("ea_id", "case_id", "id_code"), remove = T)

malawi11panel_cover <- malawi11panel_cover |> unite("householdid", c("ea_id", "case_id"), remove = T)
malawi11_gps <- malawi11_gps |> unite("householdid", c("ea_id", "case_id"), remove = T)
malawi11_cons <- malawi11_cons |> unite("householdid", c("ea_id", "case_id"), remove = T)

malawi11panel <- malawi11panel_health |> left_join(malawi11panel_roster, c("id" = "id"))
malawi11panel <- malawi11panel |> left_join(malawi11panel_cover, c("householdid" = "householdid"))
malawi11panel <- malawi11panel |> left_join(malawi11_gps, c("householdid" = "householdid"))
malawi11panel <- malawi11panel |> left_join(malawi11_cons, c("householdid" = "householdid"))

## 0bis.T) mali21 ----

mali21_health <- mali21_health |> unite("householdid", c("grappe", "menage"), remove = F)
mali21_health <- mali21_health |> unite("id", c("grappe", "menage", "membres__id"), remove = T)

mali21_indiv_charact <- mali21_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = F)

mali21_cover <- mali21_cover |> unite("householdid", c("grappe", "menage"), remove = T)
mali21_weighting <- mali21_weighting |> unite("householdid", c("grappe", "menage"), remove = T)

mali21 <- mali21_indiv_charact |> left_join(mali21_health, c("id" = "id"))
mali21 <- mali21 |> left_join(mali21_cover, c("householdid" = "householdid"))
mali21 <- mali21 |> left_join(mali21_weighting, c("householdid" = "householdid"))

## 0bis.U) mali18lsms ----

mali18lsms_health <- mali18lsms_health |> unite("householdid", c("grappe", "menage"), remove = F)
mali18lsms_health <- mali18lsms_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = F)

mali18lsms_indiv_charact <- mali18lsms_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

mali18lsms_cover <- mali18lsms_cover |> unite("householdid", c("grappe", "menage"), remove = T)

mali18lsms <- mali18lsms_health |> left_join(mali18lsms_indiv_charact, c("id" = "id"))
mali18lsms <- mali18lsms |> left_join(mali18lsms_cover, c("householdid" = "householdid"))
mali18lsms <- mali18lsms |> left_join(mali18lsms_gps, c("grappe" = "grappe"))

## 0bis.V) niger21 ----

niger21_health <- niger21_health |> unite("householdid", c("grappe", "menage"), remove = F)
niger21_health <- niger21_health |> unite("id", c("grappe", "menage", "membres__id"), remove = F)

niger21_indiv_charact <- niger21_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

niger21_cover <- niger21_cover |> unite("householdid", c("grappe", "menage"), remove = T)
niger21_cons <- niger21_cons |> unite("householdid", c("grappe", "menage"), remove = T)

niger21 <- niger21_health |> left_join(niger21_indiv_charact, c("id" = "id"))
niger21 <- niger21 |> left_join(niger21_cover, c("householdid" = "householdid"))

## 0bis.W) niger18 ----

niger18_health <- niger18_health |> unite("householdid", c("grappe", "menage"), remove = F)
niger18_health <- niger18_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = F)

niger18_indiv_charact <- niger18_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

niger18_cover <- niger18_cover |> unite("householdid", c("grappe", "menage"), remove = T)

niger18 <- niger18_health |> left_join(niger18_indiv_charact, c("id" = "id"))
niger18 <- niger18 |> left_join(niger18_cover, c("householdid" = "householdid"))
niger18 <- niger18 |> left_join(niger18_gps, c("grappe" = "grappe"))

## 0bis.X) nigeria24 ----

nigeria24_health <- nigeria24_health |> unite("householdid", c("ea", "hhid"), remove = F)
nigeria24_health <- nigeria24_health |> unite("id", c("ea", "hhid", "indiv"), remove = T)

nigeria24_indiv_charact <- nigeria24_indiv_charact |> unite("id", c("ea", "hhid", "indiv"), remove = F)

nigeria24_cover <- nigeria24_cover |> unite("householdid", c("ea", "hhid"), remove = T)

nigeria24 <- nigeria24_indiv_charact |> left_join(nigeria24_health, c("id" = "id"))
nigeria24 <- nigeria24 |> left_join(nigeria24_cover, c("householdid" = "householdid"))

## 0bis.Y) nigeria19unif

#nigeria19unif_health <- nigeria19unif_health |> unite("id", c("ea", "hhid", "indiv"), remove = F)

#nigeria19unif_roster <- nigeria19unif_roster |> unite("id", c("ea", "hhid", "indiv"), remove = T)

#nigeria19unif <- nigeria19unif_roster |> left_join(nigeria19unif_health, c("id" = "id"))

## 0bis.Z) nigeria19 ----

#nigeria19_health <- nigeria19_health |> unite("householdidokok", c("hhid", "indiv"), remove = F)
nigeria19_health <- nigeria19_health |> unite("id", c("ea", "hhid", "indiv"), remove = T)

nigeria19_roster <- nigeria19_roster |> unite("id", c("ea", "hhid", "indiv"), remove = F)

#No need for this line here: nigeria19_ <- nigeria19_ |> unite("householdid", c("", ""), remove = T)

nigeria19 <- nigeria19_roster |> left_join(nigeria19_health, c("id" = "id"))
nigeria19 <- nigeria19 |> left_join(nigeria19_gps, c("hhid" = "hhid"))

## 0bis.Abis) nigeria18lsms ----

nigeria18lsms_health <- nigeria18lsms_health |> unite("householdid", c("ea", "hhid"), remove = F)
nigeria18lsms_health <- nigeria18lsms_health |> unite("id", c("ea", "hhid", "indiv"), remove = F)

nigeria18lsms_roster <- nigeria18lsms_roster |> unite("id", c("ea", "hhid", "indiv"), remove = T)

nigeria18lsms_cover <- nigeria18lsms_cover |> unite("householdid", c("ea", "hhid"), remove = T)

nigeria18lsms <- nigeria18lsms_health |> left_join(nigeria18lsms_roster, c("id" = "id"))
nigeria18lsms <- nigeria18lsms |> left_join(nigeria18lsms_cover, c("householdid" = "householdid"))

## 0bis.Bbis) nigeria13 ----

nigeria13_health <- nigeria13_health |> unite("householdid", c("ea", "hhid"), remove = F)
nigeria13_health <- nigeria13_health |> unite("id", c("ea", "hhid", "indiv"), remove = T)

nigeria13_roster <- nigeria13_roster |> unite("id", c("ea", "hhid", "indiv"), remove = F)

nigeria13_cover <- nigeria13_cover |> unite("householdid", c("ea", "hhid"), remove = T)
nigeria13_gps <- nigeria13_gps |> unite("householdid", c("ea", "hhid"), remove = T)

nigeria13 <- nigeria13_roster |> left_join(nigeria13_health, c("id" = "id"))
nigeria13 <- nigeria13 |> left_join(nigeria13_cover, c("householdid" = "householdid"))
nigeria13 <- nigeria13 |> left_join(nigeria13_gps, c("householdid" = "householdid"))

## 0bis.Cbis) nigeria11 ----

nigeria11_health <- nigeria11_health |> unite("householdid", c("ea", "hhid"), remove = F)
nigeria11_health <- nigeria11_health |> unite("id", c("ea", "hhid", "indiv"), remove = T)

nigeria11_indiv_charact <- nigeria11_indiv_charact |> unite("id", c("ea", "hhid", "indiv"), remove = T)

nigeria11_cover <- nigeria11_cover |> unite("householdid", c("ea", "hhid"), remove = T)
nigeria11_gps <- nigeria11_gps |> unite("householdid", c("ea", "hhid"), remove = T)

nigeria11 <- nigeria11_indiv_charact |> left_join(nigeria11_health, c("id" = "id"))
nigeria11 <- nigeria11 |> left_join(nigeria11_cover, c("householdid" = "householdid"))
nigeria11 <- nigeria11 |> left_join(nigeria11_gps, c("householdid" = "householdid"))

## 0bis.Dbis) senegal21 ----

senegal21_health <- senegal21_health |> unite("householdid", c("grappe", "menage"), remove = F)
senegal21_health <- senegal21_health |> unite("id", c("grappe", "menage", "membres__id"), remove = T)

senegal21_indiv_charact <- senegal21_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

senegal21_cover <- senegal21_cover |> unite("householdid", c("grappe", "menage"), remove = T)
senegal21_weighting <- senegal21_weighting |> unite("householdid", c("grappe", "menage"), remove = T)

senegal21 <- senegal21_indiv_charact |> left_join(senegal21_health, c("id" = "id"))
senegal21 <- senegal21 |> left_join(senegal21_cover, c("householdid" = "householdid"))
senegal21 <- senegal21 |> left_join(senegal21_weighting, c("householdid" = "householdid"))

## 0bis.Ebis) senegal18 ----

senegal18_health <- senegal18_health |> unite("householdid", c("grappe", "menage"), remove = F)
senegal18_health <- senegal18_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = F)

senegal18_indiv_charact <- senegal18_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

senegal18_cover <- senegal18_cover |> unite("householdid", c("grappe", "menage"), remove = T)

senegal18 <- senegal18_health |> left_join(senegal18_indiv_charact, c("id" = "id"))
senegal18 <- senegal18 |> left_join(senegal18_cover, c("householdid" = "householdid"))
senegal18 <- senegal18 |> left_join(senegal18_gps, c("grappe" = "grappe"))

## 0bis.Fbis) tanza21 ----

#No need for this line here: tanza21_health <- tanza21_health |> unite("householdid", c("", ""), remove = F)
tanza21_health <- tanza21_health |> unite("id", c("y5_hhid", "indidy5"), remove = F)

tanza21_roster <- tanza21_roster |> unite("id", c("y5_hhid", "indidy5"), remove = T)

#No need for this line here: tanza21_ <- tanza21_ |> unite("householdid", c("", ""), remove = T)

tanza21 <- tanza21_health |> left_join(tanza21_roster, c("id" = "id"))
tanza21 <- tanza21 |> left_join(tanza21_cover, c("y5_hhid" = "y5_hhid"))

## 0bis.Gbis) tanza20 ----

#No need for this line here : tanza20_health <- tanza20_health |> unite("householdid", c("", ""), remove = F)
tanza20_health <- tanza20_health |> unite("id", c("sdd_hhid", "sdd_indid"), remove = F)

tanza20_roster <- tanza20_roster |> unite("id", c("sdd_hhid", "sdd_indid"), remove = T)

#No need for this line here: tanza20_ <- tanza20_ |> unite("householdid", c("", ""), remove = T)

tanza20 <- tanza20_health |> left_join(tanza20_roster, c("id" = "id"))
tanza20 <- tanza20 |> left_join(tanza20_cover, c("sdd_hhid" = "sdd_hhid"))

## 0bis.Hbis) tanza15 ----

#No need for this line here: tanza15_health <- tanza15_health |> unite("householdid", c("", ""), remove = F)
tanza15_health <- tanza15_health |> unite("id", c("y4_hhid", "indidy4"), remove = F)

tanza15_roster <- tanza15_roster|> unite("id", c("y4_hhid", "indidy4"), remove = T)

#No need for this line here: tanza15_ <- tanza15_ |> unite("householdid", c("", ""), remove = T)

tanza15 <- tanza15_health |> left_join(tanza15_roster, c("id" = "id"))
tanza15 <- tanza15 |> left_join(tanza15_cover, c("y4_hhid" = "y4_hhid"))
tanza15 <- tanza15 |> left_join(tanza15_gps, c("clusterid" = "clusterid"))

## 0bis.Ibis) tanza15ext ----

#No need for this line here: tanza15ext_health <- tanza15ext_health |> unite("householdid", c("", ""), remove = F)
tanza15ext_health <- tanza15ext_health |> unite("id", c("y4_hhid", "indidy4"), remove = F)

tanza15ext_roster <- tanza15ext_roster |> unite("id", c("y4_hhid", "indidy4"), remove = T)

#No need for this line here: tanza15ext_ <- tanza15ext_ |> unite("householdid", c("", ""), remove = T)

tanza15ext <- tanza15ext_health |> left_join(tanza15ext_roster, c("id" = "id"))
tanza15ext <- tanza15ext |> left_join(tanza15ext_cover, c("y4_hhid" = "y4_hhid"))

## 0bis.Jbis) tanza15unif

#No need for this line here: tanza15unif_health <- tanza15unif_health |> unite("householdid", c("", ""), remove = F)
#tanza15unif_health <- tanza15unif_health |> unite("id", c("r_hhid", "r_id"), remove = F)

#tanza15unif_roster <- tanza15unif_roster |> unite("id", c("r_hhid", "r_id"), remove = T)

#No need for this line here: tanza15unif_ <- tanza15unif_ |> unite("householdid", c("", ""), remove = T)

#tanza15unif <- tanza15unif_health |> left_join(tanza15unif_roster, c("id" = "id"))
#tanza15unif <- tanza15unif |> left_join(tanza15unif_cover, c("r_hhid" = "r_hhid"))

## 0bis.Kbis) tanza11 ----
# Note that the health file has 3 more observations than the roster file

#No need for this line here: tanza11_health <- tanza11_health |> unite("householdid", c("", ""), remove = F)
tanza11_health <- tanza11_health |> unite("id", c("y2_hhid", "indidy2"), remove = F)

tanza11_roster <- tanza11_roster |> unite("id", c("y2_hhid", "indidy2"), remove = T)

#No need for this line here: tanza11_ <- tanza11_ |> unite("householdid", c("", ""), remove = T)

tanza11 <- tanza11_health |> left_join(tanza11_roster, c("id" = "id"))
tanza11 <- tanza11 |> left_join(tanza11_cover, c("y2_hhid" = "y2_hhid"))
tanza11 <- tanza11 |> left_join(tanza11_gps, c("y2_hhid" = "y2_hhid"))

## 0bis.Lbis) togo21 ----

togo21_health <- togo21_health |> unite("householdid", c("grappe", "menage"), remove = F)
togo21_health <- togo21_health |> unite("id", c("grappe", "menage", "membres__id"), remove = F)

togo21_indiv_charact <- togo21_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = T)

togo21_cover <- togo21_cover |> unite("householdid", c("grappe", "menage"), remove = T)
togo21_weighting <- togo21_weighting |> unite("householdid", c("grappe", "menage"), remove = T)

togo21 <- togo21_health |> left_join(togo21_indiv_charact, c("id" = "id"))
togo21 <- togo21 |> left_join(togo21_cover, c("householdid" = "householdid"))
togo21 <- togo21 |> left_join(togo21_weighting, c("householdid" = "householdid"))

## 0bis.Mbis) togo18 ----

togo18_health <- togo18_health |> unite("householdid", c("grappe", "menage"), remove = F)
togo18_health <- togo18_health |> unite("id", c("grappe", "menage", "s01q00a"), remove = T)

togo18_indiv_charact <- togo18_indiv_charact |> unite("id", c("grappe", "menage", "numind"), remove = F)

togo18_cover <- togo18_cover |> unite("householdid", c("grappe", "menage"), remove = T)

togo18 <- togo18_indiv_charact |> left_join(togo18_health, c("id" = "id"))
togo18 <- togo18 |> left_join(togo18_cover, c("householdid" = "householdid"))
togo18 <- togo18 |> left_join(togo18_gps, c("grappe" = "grappe"))

## 0bis.Nbis) uganda11 ----

#No need for this line here: uganda11_health <- uganda11_health |> unite("householdid", c("", ""), remove = F)
uganda11_health <- uganda11_health |> unite("id", c("HHID", "PID"), remove = T)

uganda11_roster <- uganda11_roster |> unite("id", c("HHID", "PID"), remove = F)

#No need for this line here: uganda11_ <- uganda11_ |> unite("householdid", c("", ""), remove = T)

uganda11 <- uganda11_roster |> left_join(uganda11_health, c("id" = "id"))
uganda11 <- uganda11 |> left_join(uganda11_cover, c("HHID" = "HHID"))
uganda11 <- uganda11 |> left_join(uganda11_gps, c("HHID" = "HHID"))

## 0bis.Obis) uganda09 ----

#No need for this here: uganda09_health <- uganda09_health |> unite("householdid", c("", ""), remove = F)
uganda09_health <- uganda09_health |> unite("id", c("Hhid", "Pid"), remove = T)

uganda09_roster <- uganda09_roster |> unite("id", c("HHID", "PID"), remove = F)

#Ne need for this line here: uganda09_ <- uganda09_ |> unite("householdid", c("", ""), remove = T)

uganda09 <- uganda09_roster |> left_join(uganda09_health, c("id" = "id"))
uganda09 <- uganda09 |> left_join(uganda09_cover, c("HHID" = "HHID"))
uganda09 <- uganda09 |> left_join(uganda09_gps, c("HHID" = "HHID"))


# 1) Creation of useful variables + labeling and cleaning of variable ----

## 1.A) benin21lsms ----

# Assigning descriptive names and labels to variables

benin21lsms <- benin21lsms |> mutate(
  
)
# 26/1 TO CONTINUE HERE


# Assigning descriptive names and labels to variables
benin21lsms <- benin21lsms |> mutate(
  wt = hhweight,
  psu = grappe,
  strata = strat, # TODO CHECK THE PROBLEM HERE WITH THIS VARIABLE WHY THE MUTATE DOESN'T WORK
  #TO COMPLETE cluster_number = grappe,
  #TO COMPLETE household_number = HH2, # This variable and LN are described in "Guidelines for Merging Data Files of a MICS Survey" found here https://mics.unicef.org/faq
  #TO COMPLETE household_member_line_number = LN,
  # TO COMPLETE, AND PAY ATTENTION TO THIS VARIABLE WHEN I DO THE UNLABELLED age = age,
  # TO COMPLETE OR DELETE uses_glasses = AF2,
  # TO COMPLETE OR DELETE uses_hearing_aid = AF3,
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45,
  type_place_residence = milieu,
  region = departement #, # called "Département" in the original label
  #TO COMPLETE age_groups = WAGE, # not the same groups as in DHS
  #TO COMPLETE education_level_ever = welevel, # not the same levels as in DHS
  #TO COMPLETE disability_binary = disability, # at least when a lot of difficulty
  #TO COMPLETE wealth_quintile = windex5
) |> 
  select(wt, psu, strat, age, diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash, type_place_residence, region, GPS__Latitude, GPS__Longitude)


## 1.A) benin21lsms ----
# For here and all countries below, remove the "_health" from the term of the right after I have done the merging of files above
benin21lsms <- benin21lsms_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.B) benin18 ----

benin18 <- benin18_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.C) burki21cross ----

burki21cross <- burki21cross_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.D) burki21panel ----

burki21panel <- burki21panel_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.E) burki18 ----

burki18 <- burki18_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.F) cotediv21 ----

cotediv21 <- cotediv21_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.G) cotediv18 ----

cotediv18 <- cotediv18_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.H) ethiop21 ----

ethiop21 <- ethiop21_health |> mutate(
  diff_see = s3q21,
  diff_hear = s3q22,
  diff_com = s3q26,
  diff_remem = s3q24,
  diff_walk = s3q23,
  diff_wash = s3q25
)

## 1.I) ethiop19 ----

ethiop19 <- ethiop19_health |> mutate(
  diff_see = s3q21,
  diff_hear = s3q22,
  diff_com = s3q26,
  diff_remem = s3q24,
  diff_walk = s3q23,
  diff_wash = s3q25
)

## 1.J) ethiop16 ----

ethiop16 <- ethiop16_health |> mutate(
  diff_see = hh_s3q12,
  diff_hear = hh_s3q13,
  diff_com = hh_s3q17,
  diff_remem = hh_s3q15,
  diff_walk = hh_s3q14,
  diff_wash = hh_s3q16
)

## 1.K) ethiop14 ----

ethiop14 <- ethiop14_health |> mutate(
  diff_see = hh_s3q12,
  diff_hear = hh_s3q13,
  diff_com = hh_s3q17,
  diff_remem = hh_s3q15,
  diff_walk = hh_s3q14,
  diff_wash = hh_s3q16
)

## 1.L) ethiop12 ----

ethiop12 <- ethiop12_health |> mutate(
  diff_see = hh_s3q12,
  diff_hear = hh_s3q13,
  diff_com = hh_s3q17,
  diff_remem = hh_s3q15,
  diff_walk = hh_s3q14,
  diff_wash = hh_s3q16
)

## 1.M) guibi21 ----

guibi21 <- guibi21_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.N) guibi19 ----

guibi19 <- guibi19_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.O) malawi20 ----

malawi20 <- malawi20_health |> mutate(
  diff_see = hh_d24,
  diff_hear = hh_d25,
  diff_com = hh_d29,
  diff_remem = hh_d27,
  diff_walk = hh_d26,
  diff_wash = hh_d28
)

## 1.P) malawi19lsms ----

malawi19lsms <- malawi19lsms_health |> mutate(
  diff_see = hh_d24,
  diff_hear = hh_d25,
  diff_com = hh_d29,
  diff_remem = hh_d27,
  diff_walk = hh_d26,
  diff_wash = hh_d28
)

## 1.Salpha) malawi11fs ----

malawi11fs <- malawi11_health_fs |> mutate(
  diff_see = hh_d24,
  diff_hear = hh_d25,
  diff_com = hh_d29,
  diff_remem = hh_d27,
  diff_walk = hh_d26,
  diff_wash = hh_d28
)

## 1.Sbeta) malawi11panel ----

malawi11panel <- malawi11_health_panel |> mutate(
  diff_see = hh_d24,
  diff_hear = hh_d25,
  diff_com = hh_d29,
  diff_remem = hh_d27,
  diff_walk = hh_d26,
  diff_wash = hh_d28
)

## 1.T) mali21 ----

mali21 <- mali21_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.U) mali18lsms ----

mali18lsms <- mali18lsms_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.V) niger21 ----

niger21 <- niger21_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.W) niger18 ----

niger18 <- niger18_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.X) nigeria24 ----

nigeria24 <- nigeria24_health |> mutate(
  diff_see = s3q23,
  diff_hear = s3q24,
  diff_com = s3q28,
  diff_remem = s3q26,
  diff_walk = s3q25,
  diff_wash = s3q27
)

## 1.Y) nigeria19unif ----

nigeria19unif <- nigeria19unif_health |> mutate(
  diff_see = hd_26,
  diff_hear = hd_28,
  diff_com = hd_36,
  diff_remem = hd_32,
  diff_walk = hd_30,
  diff_wash = hd_34
)

## 1.Z) nigeria19 ----

nigeria19 <- nigeria19_health |> mutate(
  diff_see = s4aq23,
  diff_hear = s4aq25,
  diff_com = s4aq33,
  diff_remem = s4aq29,
  diff_walk = s4aq27,
  diff_wash = s4aq31
)

## 1.Abis) nigeria18lsms ----

nigeria18lsms <- nigeria18lsms_health |> mutate(
  diff_see = s03q22,
  diff_hear = s03q23,
  diff_com = s03q27,
  diff_remem = s03q25,
  diff_walk = s03q24,
  diff_wash = s03q26
)

## 1.Bbis) nigeria13 ----

nigeria13 <- nigeria13_health |> mutate(
  diff_see = s4aq23,
  diff_hear = s4aq25,
  diff_com = s4aq33,
  diff_remem = s4aq29,
  diff_walk = s4aq27,
  diff_wash = s4aq31
)

## 1.Cbis) nigeria11 ----

nigeria11 <- nigeria11_health |> mutate(
  diff_see = s4aq23,
  diff_hear = s4aq25,
  diff_com = s4aq33,
  diff_remem = s4aq29,
  diff_walk = s4aq27,
  diff_wash = s4aq31
)

## 1.Dbis) senegal21 ----

senegal21 <- senegal21_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.Ebis) senegal18 ----

senegal18 <- senegal18_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.Fbis) tanza21 ----

tanza21 <- tanza21_health |> mutate(
  diff_see = hh_d17,
  diff_hear = hh_d19,
  diff_com = hh_d27,
  diff_remem = hh_d23,
  diff_walk = hh_d21,
  diff_wash = hh_d25
)

## 1.Gbis) tanza20 ----

tanza20 <- tanza20_health |> mutate(
  diff_see = hh_d17,
  diff_hear = hh_d19,
  diff_com = hh_d27,
  diff_remem = hh_d23,
  diff_walk = hh_d21,
  diff_wash = hh_d25
)

## 1.Hbis) tanza15 ----

tanza15 <- tanza15_health |> mutate(
  diff_see = hh_d17,
  diff_hear = hh_d19,
  diff_com = hh_d27,
  diff_remem = hh_d23,
  diff_walk = hh_d21,
  diff_wash = hh_d25
)

## 1.Ibis) tanza15ext ----

tanza15ext <- tanza15ext_health |> mutate(
  diff_see = hh_d17,
  diff_hear = hh_d19,
  diff_com = hh_d27,
  diff_remem = hh_d23,
  diff_walk = hh_d21,
  diff_wash = hh_d25
)

## 1.Jbis) tanza15unif ----

tanza15unif <- tanza15unif_health |> mutate(
  diff_see = hd_23,
  diff_hear = hd_25,
  diff_com = hd_33,
  diff_remem = hd_29,
  diff_walk = hd_27,
  diff_wash = hd_31
)

## 1.Kbis) tanza11 ----

tanza11 <- tanza11_health |> mutate(
  diff_see = hh_d17,
  diff_hear = hh_d19,
  diff_com = hh_d27,
  diff_remem = hh_d23,
  diff_walk = hh_d21,
  diff_wash = hh_d25
)

## 1.Lbis) togo21 ----

togo21 <- togo21_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.Mbis) togo18 ----

togo18 <- togo18_health |> mutate(
  diff_see = s03q41,
  diff_hear = s03q42,
  diff_com = s03q46,
  diff_remem = s03q44,
  diff_walk = s03q43,
  diff_wash = s03q45
)

## 1.Nbis) uganda11 ----

uganda11 <- uganda11_health |> mutate(
  diff_see = h7q2a,
  diff_hear = h7q3a,
  diff_com = h7q7a,
  diff_remem = h7q5a,
  diff_walk = h7q4a,
  diff_wash = h7q6a
)

## 1.Obis) uganda09 ----

uganda09 <- uganda09_health |> mutate(
  diff_see = H7q2a,
  diff_hear = H7q3a,
  diff_com = H7q7a,
  diff_remem = H7q5a,
  diff_walk = H7q4a,
  diff_wash = H7q6a
)








# 2) Creation of useful variables  ----

## 2.A) benin21lsms ----

# TODO the variable below
# # Creation of variable Highest difficulty in any domain
# benin21 <- benin21 %>%
#   mutate(highest_diff_any_domain = case_when(
#     diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_com == "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" | diff_wash == "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME" ~ "cannot do at all",  # the label for diff_com is not in benin21, we found it in chad19 (! Note that in chad19 the labels are in english for diff_walk !)
#     if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
#     if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
#     .default = "no difficulty" # there are cases of "NON REPONSE"  for some disability variables, but this case_when() should have appropriate logic (29/11 for info I didn't check for countries other than benin21)
#   ))

# TODO the variable below
# # Creation of variable Has at least a lot of difficulty in any domain
# benin21 <- benin21 %>%
#   mutate(at_least_alotof_diff_any_domain = case_when(
#     highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
#     highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
#   ) |> factor(levels = c("yes", "no"))) %>%
#   set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain")

# TODO the variable below
# Creation of variable At least a lot of difficulty seeing
benin21lsms <- benin21lsms %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("Oui, un peu de difficultés", "Non, aucune difficulté", NA) ~ "no",
    diff_see %in% c("Oui, beaucoup de difficultés", "Ne peut pas du tout") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing")

# TODO check to see the frequency of NA: benin21lsms$diff_see |> describe()
# 
# # TODO the variable below
# # Creation of variable At least a lot of difficulty hearing
# benin21 <- benin21 %>%
#   mutate(at_least_alotof_diff_hear = case_when(
#     diff_hear %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
#     diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
#   ) |> factor(levels = c("yes", "no"))) %>%
#   set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing")
# 
# # TODO the variable below
# # Creation of variable At least a lot of difficulty communicating
# benin21 <- benin21 |> 
#   mutate(at_least_alotof_diff_com = case_when(
#     diff_com %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
#     diff_com %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE") ~ "yes"
#   ) |> factor(levels = c("yes", "no"))) %>%
#   set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating")
# 
# # TODO the variable below
# # Creation of variable At least a lot of difficulty remembering
# benin21 <- benin21 %>%
#   mutate(at_least_alotof_diff_remem = case_when(
#     diff_remem %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
#     diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER") ~ "yes"
#   ) |> factor(levels = c("yes", "no"))) %>%
#   set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating")
# 
# # TODO the variable below
# # Creation of variable At least a lot of difficulty walking
# benin21 <- benin21 %>%
#   mutate(at_least_alotof_diff_walk = case_when(
#     diff_walk %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
#     diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS") ~ "yes"
#   ) |> factor(levels = c("yes", "no"))) %>%
#   set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking")
# 
# # TODO the variable below
# # Creation of variable At least a lot of difficulty washing
# benin21 <- benin21 %>%
#   mutate(at_least_alotof_diff_wash = case_when(
#     diff_wash %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
#     diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME") ~ "yes"
#   ) |> factor(levels = c("yes", "no"))) %>%
#   set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing")


# 3) Loading the basemaps ---- 
# We load basemaps in objects specifically for each survey (even if there are multiple survey in an individual country)

benin21lsms_basemap0 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_0")
benin21lsms_basemap1 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_1")
#benin21_basemap2 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_2") # Not possible to use this level because we don't have a variable for this level in the datasets
#benin21_basemap3 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_3") # Not possible to use this level because we don't have a variable for this level in the datasets



# 5) Survey design (and extraction of the "working" subsample) ----

options(survey.lonely.psu = "adjust") # Added for the case where there may be a single PSU in a strata, as indicated here https://www.youtube.com/watch?v=pJwd2-m3QBY

benin21lsms <- benin21lsms |> filter(!is.na(wt), !is.na(at_least_alotof_diff_see)) # check
benin21lsmswt <- benin21lsms %>% as_survey_design(ids = psu, strata = strat, weights = wt, nest = TRUE) # psu for Primary Sampling Units, strata for Strata variable, weights for Weight variable, nest To handle nesting ; this says that "The DHS and MICS surveys have very similar two-stage sampling designs and the weights are calculated in the same way": https://userforum.dhsprogram.com/index.php?t=msg&goto=17946
# PROBABLY TODO HERETO FILTER THE INDIVUDALS AGED LESS THAN 5 benin21lsmswt <- benin21lsmswt |> srvyr::filter(age >= 18) # what follows was for when there was observations from the hl file in the dataset we applied the weighting to: |> srvyr::filter(file_source == "mn" | file_source == "wm") # respondants under 18 were not asked the unmodified WG-SS 


# 6) Creation of prevalence tables ----
# At the levels Admin 0 (country), then Admin 1 (e.g. region), then when available Admin 2 (e.g. district -> not sure it is this unit in MICS)
# TODO COMPLETE HERE
# + joining the basemaps and the prevalences at the same level


## 6.A) benin21 ----

benin21lsms_prev_admin0 <- benin21lsmswt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)) # add ","
    # hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    # com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    # remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    # walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    # wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

benin21_prev_admin1 <- benin21lsmswt |> 
  srvyr::mutate(
    region_cleaned = str_to_title(region) # add "," # To capitalize first letter of each word
    # region_cleaned = case_when(
    #   region_cleaned == "Atacora" ~ "Atakora",
    #   region_cleaned == "Couffo" ~ "Kouffo",
    #   region_cleaned == "Oueme" ~ "Ouémé",
    #   .default = region_cleaned
    # )
  ) |>
  srvyr::group_by(region_cleaned) |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)) # add ","
    # hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    # com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    # remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    # walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    # wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

benin21lsms_prev_admin1 <- left_join(benin21lsms_basemap1, benin21_prev_admin1, by = join_by(NAME_1 == region_cleaned)) # Check that all names match : anti_join(benin21_basemap1, benin21_prev_admin1, by = join_by(NAME_1 == region_cleaned))

ggplot(benin21lsms_prev_admin1) + geom_sf(aes(fill = see_prev))
