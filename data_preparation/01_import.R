# Import of DHS data


# 0) Loading packages ----
library(tidyverse)
library(haven) # To import DTA files (Stata) and sav files (SPSS)
library(labelled) # For the function unlabelled() for dealing with labelled vectors
library(questionr)
library(sf) # For the function st_read()


# 1.1) Import of the DHS databases + Selection of variables + conversion of labelled vectors to factors  ----
## 1.1.A) kenya22 ----
kenya22_complete <- read_dta("data/dhs/kenya/KEPR8CDT/KEPR8CFL.DTA")

kenya22 <- kenya22_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, sheduc, shshort, starts_with("hdis")) |>   # If I want to keep all variables relocate(hv104, hv105, starts_with("hdis"), ...) |> # Maybe it will be relevant to add the variables included in the final reports' cross tables
  unlabelled() # Mentioned in Webin-R 5 44min30 ; pay attention to the values of hv105 (95 corresponds to 95+ and 98 to "don't know")

## 1.1.B) malawi16 ----
malawi16_complete <- read_dta("data/dhs/malawi/MWPR7ADT/MWPR7AFL.DTA")


malawi16 <- malawi16_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, shdist, hv102, hv103, hv104, hv105, hv106, hv115, hv270, sh306:sh323) |> #  hv102 for malawi16 because the selection is on de jure
  unlabelled()


## 1.1.C) mali18 ----
mali18_complete <- read_dta("data/dhs/mali/MLPR7ADT/MLPR7AFL.DTA")

mali18 <- mali18_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

## 1.1.D) maurit19 ----
maurit19_complete <- read_dta("data/dhs/mauritania/MRPR71DT/MRPR71FL.DTA")


maurit19 <- maurit19_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()


## 1.1.E) mozam22 ----
mozam22_complete <- read_dta("data/dhs/mozambique/MZPR81DT/MZPR81FL.DTA")


mozam22 <- mozam22_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

## 1.1.F) nigeria18 ----
nigeria18_complete <- read_dta("data/dhs/nigeria/NGPR7BDT/NGPR7BFL.DTA")

nigeria18 <- nigeria18_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

## 1.1.G) pakis17 ----
pakis17_complete <- read_dta("data/dhs/pakistan/PKPR71DT/PKPR71FL.DTA")

pakis17 <- pakis17_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, sheduc, starts_with("hdis")) |> # sheduc is only present in kenya22 and pakis17 but most likely won't be used
  unlabelled()

## 1.1.H) rwanda19 ----
rwanda19_complete <- read_dta("data/dhs/rwanda/RWPR81DT/RWPR81FL.DTA")

rwanda19 <- rwanda19_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, shdistrict, starts_with("hdis")) |> 
  unlabelled()

## 1.1.I18) senegal18 ----
senegal18_complete <- read_dta("data/dhs/senegal18/SNPR81DT/SNPR81FL.DTA")

senegal18 <- senegal18_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("sh20g")) |> 
  unlabelled()

## 1.1.I) senegal19 ----
senegal19_complete <- read_dta("data/dhs/senegal/SNPR8BDT/SNPR8BFL.DTA")

senegal19 <- senegal19_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("sh20g")) |> 
  unlabelled()

## 1.1.J) safrica16 ----
safrica16_complete <- read_dta("data/dhs/south_africa/ZAPR71DT/ZAPR71FL.DTA")

safrica16 <- safrica16_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

## 1.1.K) tanza22 ----
tanza22_complete <- read_dta("data/dhs/tanzania/TZPR82DT/TZPR82FL.DTA")

tanza22 <- tanza22_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

## 1.1.L) uganda16 ----
uganda16_complete <- read_dta("data/dhs/uganda/UGPR7BDT/UGPR7BFL.DTA")

uganda16 <- uganda16_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, sh23:sh32) |> 
  unlabelled()

# 1.2) Import of the DHS-GPS databases  ----

kenya22gps <- st_read("data/dhs/gps_datasets/KEGE8AFL/KEGE8AFL.shp") # DHSCLUST 306 not there in gps
malawi16gps <- st_read("data/dhs/gps_datasets/MWGE7AFL/MWGE7AFL.shp")
mali18gps <- st_read("data/dhs/gps_datasets/MLGE7AFL/MLGE7AFL.shp")
maurit19gps <- st_read("data/dhs/gps_datasets/MRGE71FL/MRGE71FL.shp")
mozam22gps <- st_read("data/dhs/gps_datasets/MZGE81FL/MZGE81FL.shp")
nigeria18gps <- st_read("data/dhs/gps_datasets/NGGE7BFL/NGGE7BFL.shp")
rwanda19gps <- st_read("data/dhs/gps_datasets/RWGE81FL/RWGE81FL.shp")
senegal18gps <- st_read("data/dhs/gps_datasets/SNGE81FL/SNGE81FL.shp")
senegal19gps <- st_read("data/dhs/gps_datasets/SNGE8BFL/SNGE8BFL.shp")
safrica16gps <- st_read("data/dhs/gps_datasets/ZAGE71FL/ZAGE71FL.shp")
tanza22gps <- st_read("data/dhs/gps_datasets/TZGE81FL/TZGE81FL.shp")
uganda16gps <- st_read("data/dhs/gps_datasets/UGGE7AFL/UGGE7AFL.shp")



# 2.1) Import of MICS databases +  Selection of variables + conversion of labelled vectors to factors ----


# 27/11 : for the process of each MICS :
# 1 : DONE check in the codebook for each country the existence of variables HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, religion, windex5, PSU, stratum
# 2 : DONE Check that for the (M)WB4 variable there are no values of [97] "INCONSISTENT" with the code: # zimbab19_mn_complete |> group_by(MWB4) |> count() |> print(n = 150) # zimbab19_wm_complete |> group_by(WB4) |> count() |> print(n = 150)
# 3 : DONE Check that doing unlabelled without the windex5 mutate prevents it from working for windex5, then do the mutate
# 4 : DONE In 03_recoding_mics, start the pipe of the mn file by renaming MWB4, MWAGE, mnweight, mwelevel, mdisability
# 5 : DONE Check in the codebook that all disability variables in mn files are in the form MAF, then add the rename_with line
# 6 : DONE Check in the codebook that the disability level PAS DE DIFFICULTE/AUCUNE DIFFICULTE/No difficulty is consistent between the mn and the wm file for each country
# 7 : DONE Add the mutate(across(...)) line for the eventual relevant countries
# 8 : DONE Check in the codebook wich countries have different ELLE-MEME/LUI-MEME for the wash variable between the mn and wm files
# 9 : DONE Add the mutate(ELLE-MEME = LUI-MEME...) line for the eventual revelant countries
# 10 : DONE Check that the number of observations in "bind_rows(mn = ., wm = benin21_wm, .id = "file_source")" corresponds to the numbers in mn + wm (with nrow(country99) and then nrow(country99_mn) + nrow(country99_wm)), then add this line
# 11 : DONE Check in the codebook that the walk variable has no label mistake except in benin_wm for each country
# 12 : DONE Checked that the observations with NA weights have NA disability variables too for each country
# 13 : DONE Check that boys 15-17 and girls 15-17 are NA for the disability questions in the mn and wm datasets, and add the answer to the Atlas sheet
# 14 : DONE Add the filter(isnaweights...) line
# 15 : DONE Check in the codebook Name of the region variables (include HH7, and put it in Atlas in the column on the right) 
# 16 : DONE Check in the codebook, to fill the Atlas, the Smallest Admin level available (variable name and number)
# ###
# 17 : DONE Fill the section Assigning descriptive names to variables for each country
# 18 : NOT USEFUL IN FACT Do a fct_recode for all the disability variables of all the mics countries (levels in english)
# 19 : NOT USEFUL IN FACT Change the "Creation of useful variables" code for benin with the new levels names
# 20 : DONE Create all useful variables (including highest difficulty and has at least some in any domain), one country after the other
# 21 : DONE Load the basemaps at the three levels for all countries
# 22 : DONE In the first README of the 8 MICS GPS datasets, check the level for the displacement, and add in Atlas the column "Number of possible valid (in the sense of Displacement Restriction) levels to represent -> for countries without GPS, check the different variables for regions/provinces... to see if there is one or more + check if the region is not itself already a 2nd level rather than a 1st level", as well as the column "Name of the level of displacement"
# 23 : NOT USEFUL (because I've directly organized properly the basemaps) Add in Atlas the column "To which GADM level does the Displacement Restriction entity mentioned in the GPS Readme file correspond?" (so check in the loaded basemaps)
# 24 : NOT USEFUL Replace for the 8 countries with GPS the relevant map
# 25 : NOT USEFUL Do the process of recodingdhs section4 for the 8 countries with GPS (with the 2 anti_join and the 2 ggplot)
# 27 : DONE Do the survey design code for each country
# 28 : Create the prevalence tables for each country at the (level0 +) two levels if possible (for the countries without gps, if there is only one level use the first_level_variable, and if there are two levels, use the first level variable then the second level variable to do the group_by in the prevalence table) + check the display with a small ggplot() like I did in DHS
# 29 : In the analyses_raphael quarto, add the make_prev_tb() for the mics countries
# 30 : Add the ggplots at admin1 and admin2
# 31 : Add the mics countries in Africa map admin0
# 32 : Add the mics countries in Africa map admin1 (if necessary mutate on the go the variable with the admin1 names to NAME_1, as I did for malawi and uganda)
# 33 : Add the mics countries in Africa map admin2 (if necessary mutate on the go the variable with the admin2 names to NAME_2)
# THINK TO CHECK THE NUMBER OF NA ANSWERS FOR THE DISABILITY VARIABLES IN THE country99wt datasets (i.e. just after the weighting, for instance with a country99wt$variables |> View())

# Note 29/10 : For now, we make the hypothesis that the to find the same results as in the Survey findings, we have to use the weighting variable in the hl file (we could use hh, but hl since the hhweight variable are grouped by household, but hl has one row per respondant as in the mn and wm files)

# Note 29/10 : (At least for Benin 2021 survey findings for seeing and hearing for people with equipement) Disability is considered starting from "[3] BEAUCOUP DE DIFFICULTES or [4] NE PEUT PAS DU TOUT" (i.e. [2] QUELQUES DIFFICULTES" is not considered as disability)

# Steps [Not relevant now, to displace in sandbox when I'm done with the mics process] : (1) Include disability (and others??) variables in benin21_mn selected, (2) select hhweight in hl file, 
# (3) create a variable HH1HH2LN concatenated for mn and hl, 
# (4) merge the two files and check all observations of men are kept, 
# (5) maybe check if unlablled function works fine for mn and hl datasets (after rewatching relevant part of larmarange)
# (6) do the same for wm
# (7) merge mn and wm
# above is done
# (8) check the weighting (applying the survey_design...() function ) for them
# (9) if it works fine, create the same variables as for DHS from the file DHS recoding (note that difficulty level is "some" in dhs, and they used "a lot" in mics survey findings documents)
# (10) do this for all mics surveys
# (11) merge the datasets to the gps data when available (check on the survey page in mics website)
# (12) download necessary basemaps and put them in the right folder
# (13) create prevalence tibbles for the mics countries and link them to the basemaps while checking that the units correspond well (do this each time at the smallest level available in the mics datasets or mics gps datasets)
# (14) in the quarto file draw the maps created with ggplot as I did with DHS, and add the mics countries in the whole africa maps (at the two levels)



## 2.1.A) benin21 ----

benin21_mn_complete <- read_sav("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/mn.sav")
benin21_mn <- benin21_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, religion, windex5, PSU, stratum) |> # mwelevel rather than MWB6A because mwelevel corresponds to the variable in the Survey findings ; MWB4 for the age not in factor form (but in hl file there is also the age variable HL6)
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

benin21_wm_complete <- read_sav("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/wm.sav")
benin21_wm <- benin21_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, religion, windex5, PSU, stratum) |> # welevel rather than MWB6A because welevel corresponds to the variable in the Survey findings ; WB4 for the age not in factor form (but in hl file there is also the age variable HL6)
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"


## 2.1.B) centraf18 ----

centraf18_mn_complete <- read_sav("data/mics/Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/mn.sav")
centraf18_mn <- centraf18_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, religion, windex5, PSU, stratum) |> 
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

centraf18_wm_complete <- read_sav("data/mics/Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/wm.sav")
centraf18_wm <- centraf18_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, religion, windex5, PSU, stratum) |> 
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.C) chad19 ----

chad19_mn_complete <- read_sav("data/mics/Chad MICS6 Datasets/Chad MICS6 SPSS Datasets/mn.sav")
chad19_mn <- chad19_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, religion, windex5, PSU, stratum) |> 
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

chad19_wm_complete <- read_sav("data/mics/Chad MICS6 Datasets/Chad MICS6 SPSS Datasets/wm.sav")
chad19_wm <- chad19_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, religion, windex5, PSU, stratum) |> 
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.D) comor22 ----

comor22_mn_complete <- read_sav("data/mics/Comoros MICS6 Datasets/Comoros MICS6 Datasets/Comoros MICS6 SPSS Datasets/mn.sav")
comor22_mn <- comor22_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # For comor22: additional variables PSU_SE and stratum_SE ; # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

comor22_wm_complete <- read_sav("data/mics/Comoros MICS6 Datasets/Comoros MICS6 Datasets/Comoros MICS6 SPSS Datasets/wm.sav")
comor22_wm <- comor22_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # For comor22: additional variables PSU_SE and stratum_SE ; # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.E) drcong17 ----

drcong17_mn_complete <- read_sav("data/mics/DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/mn.sav")
drcong17_mn <- drcong17_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # For drcong17: additional variables mwelevel1 and mwelevel2 ; # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

drcong17_wm_complete <- read_sav("data/mics/DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/wm.sav")
drcong17_wm <- drcong17_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # For drcong17: additional variables welevel1 and welevel2 ; # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.F) eswat21 ----

eswat21_mn_complete <- read_sav("data/mics/Eswatini MICS6 Datasets/Eswatini MICS6 SPSS Datasets/mn.sav")
eswat21_mn <- eswat21_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, psu, stratum) |>  # Absence of variable religion in mn and wm files ; psu not capitalized
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

eswat21_wm_complete <- read_sav("data/mics/Eswatini MICS6 Datasets/Eswatini MICS6 SPSS Datasets/wm.sav")
eswat21_wm <- eswat21_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, psu, stratum) |>  # Absence of variable religion in mn and wm files ; psu not capitalized
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.G) gamb18 ----

gamb18_mn_complete <- read_sav("data/mics/The Gambia MICS6 Datasets/The Gambia MICS6 SPSS Datasets/mn.sav")
gamb18_mn <- gamb18_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

gamb18_wm_complete <- read_sav("data/mics/The Gambia MICS6 Datasets/The Gambia MICS6 SPSS Datasets/wm.sav")
gamb18_wm <- gamb18_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.H) ghana17 ----

ghana17_mn_complete <- read_sav("data/mics/Ghana MICS6 SPSS Datasets/Ghana MICS6 SPSS Datasets/mn.sav")
ghana17_mn <- ghana17_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

ghana17_wm_complete <- read_sav("data/mics/Ghana MICS6 SPSS Datasets/Ghana MICS6 SPSS Datasets/wm.sav")
ghana17_wm <- ghana17_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.I) guibi18 ----

guibi18_mn_complete <- read_sav("data/mics/Guinea Bissau MICS6 Datasets/Guinea Bissau MICS6 SPSS Datasets/mn.sav")
guibi18_mn <- guibi18_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

guibi18_wm_complete <- read_sav("data/mics/Guinea Bissau MICS6 Datasets/Guinea Bissau MICS6 SPSS Datasets/wm.sav")
guibi18_wm <- guibi18_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.J) lesot18 ----

lesot18_mn_complete <- read_sav("data/mics/Lesotho_MICS6_datasets/Lesotho_MICS6_datasets/mn.sav")
lesot18_mn <- lesot18_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, HH7A, MWAGE, mnweight, mwelevel, mdisability, windex5, psu, stratum) |>  # Absence of variable religion in mn and wm files ; psu not capitalized
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

lesot18_wm_complete <- read_sav("data/mics/Lesotho_MICS6_datasets/Lesotho_MICS6_datasets/wm.sav")
lesot18_wm <- lesot18_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, HH7A, WAGE, wmweight, welevel, disability, windex5, psu, stratum) |>  # Absence of variable religion in mn and wm files ; psu not capitalized
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.K) madag18 ----

madag18_mn_complete <- read_sav("data/mics/Madagascar MICS6 datasets/Madagascar MICS6 datasets/Madagascar MICS6 SPSS datasets/mn.sav")
madag18_mn <- madag18_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

madag18_wm_complete <- read_sav("data/mics/Madagascar MICS6 datasets/Madagascar MICS6 datasets/Madagascar MICS6 SPSS datasets/wm.sav")
madag18_wm <- madag18_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.L) malawi19 ----

malawi19_hh_complete <- read_sav("data/mics/Malawi MICS6 SPSS/Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/hh.sav")
malawi19_hh <- malawi19_hh_complete |> 
  select(HH1, HH2, DISTRICT) |> 
  unlabelled()

malawi19_mn_complete <- read_sav("data/mics/Malawi MICS6 SPSS/Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/mn.sav")
malawi19_mn <- malawi19_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

malawi19_wm_complete <- read_sav("data/mics/Malawi MICS6 SPSS/Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/wm.sav")
malawi19_wm <- malawi19_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.x) nigeria21 ----
#nigeria21_mn_complete <- read_sav("data/mics/Nigeria MICS6 Datasets/Nigeria MICS6 Datasets/Nigeria MICS6 SPSS Datasets/mn.sav")
#nigeria21_wm_complete <- read_sav("data/mics/Nigeria MICS6 Datasets/Nigeria MICS6 Datasets/Nigeria MICS6 SPSS Datasets/wm.sav")

## 2.1.M) saoto19 ----

saoto19_mn_complete <- read_sav("data/mics/Sao Tome and Principe MICS6 Datasets/Sao Tome and Principe MICS6 SPSS Datasets/mn.sav")
saoto19_mn <- saoto19_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files # Additional variable dstratum
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

saoto19_wm_complete <- read_sav("data/mics/Sao Tome and Principe MICS6 Datasets/Sao Tome and Principe MICS6 SPSS Datasets/wm.sav")
saoto19_wm <- saoto19_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files # Additional variable dstratum
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.N) sierral17 ----

sierral17_hh_complete <- read_sav("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/hh.sav")
sierral17_hh <- sierral17_hh_complete |> 
  select(HH1, HH2, stratum) |> 
  unlabelled()

sierral17_mn_complete <- read_sav("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/mn.sav")
sierral17_mn <- sierral17_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, HH7A, MWAGE, mnweight, mwelevel, mdisability, windex5) |>  # Absence of variable religion in mn and wm files ; # Absence of variable PSU ; # Absence of variable stratum (but present in sierral17_hh)
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

sierral17_wm_complete <- read_sav("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/wm.sav")
sierral17_wm <- sierral17_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, HH7A, WAGE, wmweight, welevel, disability, windex5) |>  # Absence of variable religion in mn and wm files ; # Absence of variable PSU ; # Absence of variable stratum (but present in sierral17_hh)
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.O) togo17 ----

togo17_mn_complete <- read_sav("data/mics/Togo MICS6 SPSS Datasets/Togo MICS6 SPSS Datasets/mn.sav")
togo17_mn <- togo17_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

togo17_wm_complete <- read_sav("data/mics/Togo MICS6 SPSS Datasets/Togo MICS6 SPSS Datasets/wm.sav")
togo17_wm <- togo17_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in mn and wm files
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.P) yemen22 ----

#yemen22_mn_complete <- read_sav("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/") # No men dataset available for yemen22
yemen22_wm_complete <- read_sav("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/wm.sav")
yemen22_wm <- yemen22_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in wm file (no mn file for yemen22)
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.1.Q) zimbab19 ----

zimbab19_mn_complete <- read_sav("data/mics/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/mn.sav")
zimbab19_mn <- zimbab19_mn_complete |> 
  select(HH1, HH2, LN, MWB4, starts_with("MAF"), HH6, HH7, MWAGE, mnweight, mwelevel, mdisability, religion, windex5, psu, stratum) |>  # psu not capitalized
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the MWB4 variable there where no values of [97] "INCONSISTENT"

zimbab19_wm_complete <- read_sav("data/mics/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/wm.sav")
zimbab19_wm <- zimbab19_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, religion, windex5, psu, stratum) |>  # psu not capitalized
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"


# APPENDIX: Code for the creation of codebooks for all MICS datasets
# # # One instance for the example:
# # benin21_hh <- read_sav("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/hh.sav")
# # benin21_hh_codebook <- benin21_hh |> look_for() |> tibble() |> mutate(levels = as.character(levels), value_labels = as.character(value_labels))
# # addWorksheet(wb, "benin21_hh")
# # writeData(wb, "benin21_hh", benin21_hh_codebook)
# 
# 
# library(openxlsx)
# codebook_save <- function(path, countryname_file, file) {
#   dataset_codebook <- paste0(path, file, ".sav") |> read_sav() |> look_for() |> tibble() |> mutate(levels = as.character(levels), label = as.character(label), value_labels = as.character(value_labels)) # Construction of the full file path, then reading it and creating the codebook table
#   addWorksheet(wb, countryname_file)
#   writeData(wb, countryname_file, dataset_codebook)
#   return(wb)   # returning the workbook (to allow chaining or reuse)
# }
# 
# wb <- createWorkbook() # To create a workbook (with potentially multiple sheets), method found here https://stackoverflow.com/a/50833546
# 
# codebook_save("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/", "benin21_hh", "hh")
# codebook_save("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/", "benin21_hl", "hl")
# codebook_save("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/", "benin21_wm", "wm")
# codebook_save("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/", "benin21_mn", "mn")
# 
# codebook_save("data/mics/Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/", "centraf18_hh", "hh")
# codebook_save("data/mics/Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/", "centraf18_hl", "hl")
# codebook_save("data/mics/Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/", "centraf18_wm", "wm")
# codebook_save("data/mics/Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/", "centraf18_mn", "mn")
# 
# codebook_save("data/mics/Chad MICS6 Datasets/Chad MICS6 SPSS Datasets/", "chad19_hh", "hh")
# codebook_save("data/mics/Chad MICS6 Datasets/Chad MICS6 SPSS Datasets/", "chad19_hl", "hl")
# codebook_save("data/mics/Chad MICS6 Datasets/Chad MICS6 SPSS Datasets/", "chad19_wm", "wm")
# codebook_save("data/mics/Chad MICS6 Datasets/Chad MICS6 SPSS Datasets/", "chad19_mn", "mn")
# 
# codebook_save("data/mics/Comoros MICS6 Datasets/Comoros MICS6 Datasets/Comoros MICS6 SPSS Datasets/", "comor22_hh", "hh")
# codebook_save("data/mics/Comoros MICS6 Datasets/Comoros MICS6 Datasets/Comoros MICS6 SPSS Datasets/", "comor22_hl", "hl")
# codebook_save("data/mics/Comoros MICS6 Datasets/Comoros MICS6 Datasets/Comoros MICS6 SPSS Datasets/", "comor22_wm", "wm")
# codebook_save("data/mics/Comoros MICS6 Datasets/Comoros MICS6 Datasets/Comoros MICS6 SPSS Datasets/", "comor22_mn", "mn")
# 
# codebook_save("data/mics/DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/", "drcong17_hh", "hh")
# codebook_save("data/mics/DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/", "drcong17_hl", "hl")
# codebook_save("data/mics/DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/", "drcong17_wm", "wm")
# codebook_save("data/mics/DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/", "drcong17_mn", "mn")
# 
# codebook_save("data/mics/Eswatini MICS6 Datasets/Eswatini MICS6 SPSS Datasets/", "eswat21_hh", "hh")
# codebook_save("data/mics/Eswatini MICS6 Datasets/Eswatini MICS6 SPSS Datasets/", "eswat21_hl", "hl")
# codebook_save("data/mics/Eswatini MICS6 Datasets/Eswatini MICS6 SPSS Datasets/", "eswat21_wm", "wm")
# codebook_save("data/mics/Eswatini MICS6 Datasets/Eswatini MICS6 SPSS Datasets/", "eswat21_mn", "mn")
# 
# codebook_save("data/mics/The Gambia MICS6 Datasets/The Gambia MICS6 SPSS Datasets/", "gamb18_hh", "hh")
# codebook_save("data/mics/The Gambia MICS6 Datasets/The Gambia MICS6 SPSS Datasets/", "gamb18_hl", "hl")
# codebook_save("data/mics/The Gambia MICS6 Datasets/The Gambia MICS6 SPSS Datasets/", "gamb18_wm", "wm")
# codebook_save("data/mics/The Gambia MICS6 Datasets/The Gambia MICS6 SPSS Datasets/", "gamb18_mn", "mn")
# 
# codebook_save("data/mics/Ghana MICS6 SPSS Datasets/Ghana MICS6 SPSS Datasets/", "ghana17_hh", "hh")
# codebook_save("data/mics/Ghana MICS6 SPSS Datasets/Ghana MICS6 SPSS Datasets/", "ghana17_hl", "hl")
# codebook_save("data/mics/Ghana MICS6 SPSS Datasets/Ghana MICS6 SPSS Datasets/", "ghana17_wm", "wm")
# codebook_save("data/mics/Ghana MICS6 SPSS Datasets/Ghana MICS6 SPSS Datasets/", "ghana17_mn", "mn")
# 
# codebook_save("data/mics/Guinea Bissau MICS6 Datasets/Guinea Bissau MICS6 SPSS Datasets/", "guibi18_hh", "hh")
# codebook_save("data/mics/Guinea Bissau MICS6 Datasets/Guinea Bissau MICS6 SPSS Datasets/", "guibi18_hl", "hl")
# codebook_save("data/mics/Guinea Bissau MICS6 Datasets/Guinea Bissau MICS6 SPSS Datasets/", "guibi18_wm", "wm")
# codebook_save("data/mics/Guinea Bissau MICS6 Datasets/Guinea Bissau MICS6 SPSS Datasets/", "guibi18_mn", "mn")
# 
# codebook_save("data/mics/Lesotho_MICS6_datasets/Lesotho_MICS6_datasets/", "lesot18_hh", "hh")
# codebook_save("data/mics/Lesotho_MICS6_datasets/Lesotho_MICS6_datasets/", "lesot18_hl", "hl")
# codebook_save("data/mics/Lesotho_MICS6_datasets/Lesotho_MICS6_datasets/", "lesot18_wm", "wm")
# codebook_save("data/mics/Lesotho_MICS6_datasets/Lesotho_MICS6_datasets/", "lesot18_mn", "mn")
# 
# codebook_save("data/mics/Madagascar MICS6 datasets/Madagascar MICS6 datasets/Madagascar MICS6 SPSS datasets/", "madag18_hh", "hh")
# codebook_save("data/mics/Madagascar MICS6 datasets/Madagascar MICS6 datasets/Madagascar MICS6 SPSS datasets/", "madag18_hl", "hl")
# codebook_save("data/mics/Madagascar MICS6 datasets/Madagascar MICS6 datasets/Madagascar MICS6 SPSS datasets/", "madag18_wm", "wm")
# codebook_save("data/mics/Madagascar MICS6 datasets/Madagascar MICS6 datasets/Madagascar MICS6 SPSS datasets/", "madag18_mn", "mn")
# 
# codebook_save("data/mics/Malawi MICS6 SPSS/Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/", "malawi19_hh", "hh")
# codebook_save("data/mics/Malawi MICS6 SPSS/Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/", "malawi19_hl", "hl")
# codebook_save("data/mics/Malawi MICS6 SPSS/Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/", "malawi19_wm", "wm")
# codebook_save("data/mics/Malawi MICS6 SPSS/Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/", "malawi19_mn", "mn")
# 
# codebook_save("data/mics/Sao Tome and Principe MICS6 Datasets/Sao Tome and Principe MICS6 SPSS Datasets/", "saoto19_hh", "hh")
# codebook_save("data/mics/Sao Tome and Principe MICS6 Datasets/Sao Tome and Principe MICS6 SPSS Datasets/", "saoto19_hl", "hl")
# codebook_save("data/mics/Sao Tome and Principe MICS6 Datasets/Sao Tome and Principe MICS6 SPSS Datasets/", "saoto19_wm", "wm")
# codebook_save("data/mics/Sao Tome and Principe MICS6 Datasets/Sao Tome and Principe MICS6 SPSS Datasets/", "saoto19_mn", "mn")
# 
# codebook_save("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/", "sierral17_hh", "hh")
# codebook_save("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/", "sierral17_hl", "hl")
# codebook_save("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/", "sierral17_wm", "wm")
# codebook_save("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/", "sierral17_mn", "mn")
# 
# codebook_save("data/mics/Togo MICS6 SPSS Datasets/Togo MICS6 SPSS Datasets/", "togo17_hh", "hh")
# codebook_save("data/mics/Togo MICS6 SPSS Datasets/Togo MICS6 SPSS Datasets/", "togo17_hl", "hl")
# codebook_save("data/mics/Togo MICS6 SPSS Datasets/Togo MICS6 SPSS Datasets/", "togo17_wm", "wm")
# codebook_save("data/mics/Togo MICS6 SPSS Datasets/Togo MICS6 SPSS Datasets/", "togo17_mn", "mn")
# 
# codebook_save("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/", "yemen22_hh", "hh")
# codebook_save("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/", "yemen22_hl", "hl")
# codebook_save("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/", "yemen22_wm", "wm")
# # codebook_save("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/", "yemen22_mn", "mn") # doesn't exist
# 
# codebook_save("data/mics/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/", "zimbab19_hh", "hh")
# codebook_save("data/mics/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/", "zimbab19_hl", "hl")
# codebook_save("data/mics/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/", "zimbab19_wm", "wm")
# codebook_save("data/mics/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/", "zimbab19_mn", "mn")
# 
# saveWorkbook(wb, "data/mics/mics_codebooks.xlsx", overwrite = TRUE) # To save the workbook (an Excel file with multiple sheets)






# 3.1) Import of LSMS databases ----
# "indiv_charact" means individual characteristics
# "nsu" means non-standard units (Note 20/1: but I'm not sure the nsu files are useful)
# "ph" means post-harvest, "pp" means post-planting, "ls" means livestock
# "fs" means full sample
# "cons" means consumption

## 3.1.A) benin21lsms ----

benin21lsms_cover_complete <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/s00_me_ben2021.dta")
benin21lsms_cover <- benin21lsms_cover_complete |> 
  select(grappe, menage, PanelHH)

benin21lsms_health_complete <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/s03_me_ben2021.dta")
benin21lsms_health <- benin21lsms_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, vague, membres__id)

benin21lsms_indiv_charact_complete <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_ben2021.dta")
benin21lsms_indiv_charact <- benin21lsms_indiv_charact_complete |> 
  select(hhid, grappe, menage, numind)

benin21lsms_weighting_complete <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/ehcvm_ponderations_ben2021.dta")
benin21lsms_weighting <- benin21lsms_weighting_complete |> 
  select(grappe, menage)

# TOCONTINUE HERE with "_complete" and then do similarily as the other surveys above (complete, then selection of the right variables, then unlabelled)

# TODO MAYBE HERE CHECK THAT IT WAS DONE CORRECTLY (for instance/in particular the age variable)
benin21lsms_cover <- benin21lsms_cover |> unlabelled()
benin21lsms_health <- benin21lsms_health |> unlabelled()
benin21lsms_indiv_charact <- benin21lsms_indiv_charact |> unlabelled()
benin21lsms_weighting <- benin21lsms_weighting |> unlabelled()


## 3.1.B) benin18 ----

# TODO TO CONTINUE HERE FOR LSMS COUNTRIES (put country99_ and read_dta() until the folder of the country, to map the country99 to the right folder)

benin18_cover_complete <- read_dta("data/lsms/BEN_2018_EHCVM_v02_M_Stata/s00_me_ben2018.dta")
benin18_cover <- benin18_cover_complete |> 
  select(grappe, menage)

benin18_health_complete <- read_dta("data/lsms/BEN_2018_EHCVM_v02_M_Stata/s03_me_ben2018.dta")
benin18_health <- benin18_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a)

benin18_indiv_charact_complete <- read_dta("data/lsms/BEN_2018_EHCVM_v02_M_Stata/ehcvm_individu_ben2018.dta")
benin18_indiv_charact <- benin18_indiv_charact_complete |> 
  select(grappe, menage, numind)

benin18_weighting_complete <- read_dta("data/lsms/BEN_2018_EHCVM_v02_M_Stata/ehcvm_ponderations_ben2018.dta")
benin18_weighting <- benin18_weighting_complete |> 
  select(grappe)

benin18_gps_complete <- read_dta("data/lsms/BEN_2018_EHCVM_v02_M_Stata/grappe_gps_ben2018.dta")
benin18_gps <- benin18_gps_complete |> 
  select(grappe)


## 3.1.C) burki21cross ----

burki21cross_cover_complete <- read_dta("data/lsms/BFA_2021_EHCVM-2_v01_M_Stata/s00_me_bfa2021.dta")
burki21cross_cover <- burki21cross_cover_complete |> 
  select(grappe, menage)

burki21cross_health_complete <- read_dta("data/lsms/BFA_2021_EHCVM-2_v01_M_Stata/s03_me_bfa2021.dta")
burki21cross_health <- burki21cross_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, pid)

burki21cross_indiv_charact_complete <- read_dta("data/lsms/BFA_2021_EHCVM-2_v01_M_Stata/ehcvm_individu_bfa2021.dta")
burki21cross_indiv_charact <- burki21cross_indiv_charact_complete |> 
  select(grappe, menage, pid)

burki21cross_weighting_complete <- read_dta("data/lsms/BFA_2021_EHCVM-2_v01_M_Stata/ehcvm_ponderations_bfa2021.dta")
burki21cross_weighting <- burki21cross_weighting_complete |> 
  select(grappe)

# Below for strata but no key to join to the other datasets
#burki21cross_nsu_complete <- read_dta("data/lsms/BFA_2021_EHCVM-2_v01_M_Stata/ehcvm_nsu_bfa2021.dta")
#burki21cross_nsu <- burki21cross_nsu_complete |> 
#  select()


## 3.1.D) burki21panel ----

burki21panel_cover_complete <- read_dta("data/lsms/BFA_2021_EHCVM-P_v04_M_Stata/s00_me_bfa2021.dta")
burki21panel_cover <- burki21panel_cover_complete |> 
  select(grappe, menage)

burki21panel_health_complete <- read_dta("data/lsms/BFA_2021_EHCVM-P_v04_M_Stata/s03_me_bfa2021.dta")
burki21panel_health <- burki21panel_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, pid)

burki21panel_indiv_charact_complete <- read_dta("data/lsms/BFA_2021_EHCVM-P_v04_M_Stata/ehcvm_individu_bfa2021.dta")
burki21panel_indiv_charact <- burki21panel_indiv_charact_complete |> 
  select(grappe, menage, pid)

burki21panel_weighting_complete <- read_dta("data/lsms/BFA_2021_EHCVM-P_v04_M_Stata/ehcvm_panel_ponderations_bfa2021.dta")
burki21panel_weighting <- burki21panel_weighting_complete |> 
  select(grappe)

# Below for strata but no key to join to the other datasets
#burki21panel_nsu_complete <- read_dta("data/lsms/BFA_2021_EHCVM-P_v04_M_Stata/ehcvm_nsu_bfa2021.dta")
#burki21panel_nsu <- burki21panel_nsu_complete |> 
#  select()


## 3.1.E) burki18 ----


burki18_cover_complete <- read_dta("data/lsms/BFA_2018_EHCVM_v03_M_Stata/s00_me_bfa2018.dta")
burki18_cover <- burki18_cover_complete |> 
  select(grappe, menage)

burki18_health_complete <- read_dta("data/lsms/BFA_2018_EHCVM_v03_M_Stata/s03_me_bfa2018.dta")
burki18_health <- burki18_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a)

burki18_indiv_charact_complete <- read_dta("data/lsms/BFA_2018_EHCVM_v03_M_Stata/ehcvm_individu_bfa2018.dta")
burki18_indiv_charact <- burki18_indiv_charact_complete |> 
  select(grappe, menage, numind)

burki18_weighting_complete <- read_dta("data/lsms/BFA_2018_EHCVM_v03_M_Stata/ehcvm_ponderations_bfa2018.dta")
burki18_weighting <- burki18_weighting_complete |> 
  select(grappe)

burki18_gps_complete <- read_dta("data/lsms/BFA_2018_EHCVM_v03_M_Stata/grappe_gps_bfa2018.dta")
burki18_gps <- burki18_gps_complete |> 
  select(grappe)


## 3.1.F) cotediv21 ----


cotediv21_cover_complete <- read_dta("data/lsms/CIV_2021_EHCVM-2_v01_M_STATA14/s00_me_civ2021.dta")
cotediv21_cover <- cotediv21_cover_complete |> 
  select(grappe, menage)

cotediv21_health_complete <- read_dta("data/lsms/CIV_2021_EHCVM-2_v01_M_STATA14/s03_me_civ2021.dta")
cotediv21_health <- cotediv21_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, membres__id)

cotediv21_indiv_charact_complete <- read_dta("data/lsms/CIV_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_civ2021.dta")
cotediv21_indiv_charact <- cotediv21_indiv_charact_complete |> 
  select(grappe, menage, numind)

cotediv21_weighting_complete <- read_dta("data/lsms/CIV_2021_EHCVM-2_v01_M_STATA14/ehcvm_ponderations_civ2021.dta")
cotediv21_weighting <- cotediv21_weighting_complete |> 
  select(grappe, menage)

# Below for strata but no key to join to the other datasets -> not sure but I think so
#cotediv21_nsu_complete <- read_dta("data/lsms/CIV_2021_EHCVM-2_v01_M_STATA14/ehcvm_nsu_civ2021.dta")
#cotediv21_nsu <- cotediv21_nsu_complete |> 
#  select()


## 3.1.G) cotediv18 ----

cotediv18_cover_complete <- read_dta("data/lsms/CIV_2018_EHCVM_v02_M_Stata/s00_me_civ2018.dta")
cotediv18_cover <- cotediv18_cover_complete |> 
  select(grappe, menage)

cotediv18_health_complete <- read_dta("data/lsms/CIV_2018_EHCVM_v02_M_Stata/s03_me_civ2018.dta")
cotediv18_health <- cotediv18_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a) # s01q00a is the right variable for making an individual identifier, contrary to s03q00

cotediv18_indiv_charact_complete <- read_dta("data/lsms/CIV_2018_EHCVM_v02_M_Stata/ehcvm_individu_civ2018.dta")
cotediv18_indiv_charact <- cotediv18_indiv_charact_complete |> 
  select(grappe, menage, numind) # Variables of this file not indicated on the website, but we checked that numind seemed to correpond to s01q00a (similarly as for the surveys above)

cotediv18_weighting_complete <- read_dta("data/lsms/CIV_2018_EHCVM_v02_M_Stata/ehcvm_ponderations_civ2018.dta")
cotediv18_weighting <- cotediv18_weighting_complete |> 
  select(grappe)

cotediv18_gps_complete <- read_dta("data/lsms/CIV_2018_EHCVM_v02_M_Stata/grappe_gps_civ2018.dta")
cotediv18_gps <- cotediv18_gps_complete |> 
  select(grappe)


## 3.1.H) ethiop21 ----

ethiop21_cover_complete <- read_dta("data/lsms/ETH_2021_ESPS-W5_v01_M_Stata/sect_cover_hh_w5.dta")
ethiop21_cover <- ethiop21_cover_complete |> 
  select(ea_id, household_id)

ethiop21_roster_complete <- read_dta("data/lsms/ETH_2021_ESPS-W5_v01_M_Stata/sect1_hh_w5.dta")
ethiop21_roster <- ethiop21_roster_complete |> 
  select(ea_id, household_id, individual_id)

ethiop21_health_complete <- read_dta("data/lsms/ETH_2021_ESPS-W5_v01_M_Stata/sect3_hh_w5.dta")
ethiop21_health <- ethiop21_health_complete |> 
  select(s3q21, s3q22, s3q23, s3q24, s3q25, s3q26, ea_id, household_id, individual_id)


## 3.1.I) ethiop19 ----

ethiop19_cover_complete <- read_dta("data/lsms/ETH_2018_ESS_v03_M_Stata/sect_cover_hh_w4.dta")
ethiop19_cover <- ethiop19_cover_complete |> 
  select(ea_id, household_id)

ethiop19_roster_complete <- read_dta("data/lsms/ETH_2018_ESS_v03_M_Stata/sect1_hh_w4.dta")
ethiop19_roster <- ethiop19_roster_complete |> 
  select(ea_id, household_id, individual_id)

ethiop19_health_complete <- read_dta("data/lsms/ETH_2018_ESS_v03_M_Stata/sect3_hh_w4.dta")
ethiop19_health <- ethiop19_health_complete |> 
  select(s3q21, s3q22, s3q23, s3q24, s3q25, s3q26, ea_id, household_id, individual_id)

ethiop19_gps_ph_complete <- read_dta("data/lsms/ETH_2018_ESS_v03_M_Stata/sect_cover_ph_w4.dta") # Based on the description on the Scope section on the Study description page of the survey webpage, this seems to be the most relevant gps data (it's written "Post Harvest: Household roster")
ethiop19_gps_ph <- ethiop19_gps_ph_complete |> 
  select(ea_id, household_id)

ethiop19_gps_pp_complete <- read_dta("data/lsms/ETH_2018_ESS_v03_M_Stata/sect_cover_pp_w4.dta")
ethiop19_gps_pp <- ethiop19_gps_pp_complete |> 
  select(ea_id, household_id)

ethiop19_gps_ls_complete <- read_dta("data/lsms/ETH_2018_ESS_v03_M_Stata/sect_cover_ls_w4.dta")
ethiop19_gps_ls <- ethiop19_gps_ls_complete |> 
  select(ea_id, household_id)


## 3.1.J) ethiop16 ----
# We use ea_id2, household_id2, individual_id2 as indicated in Basic Information Document, Ethiopia Socioeconomic Survey 2015/16, here https://microdata.worldbank.org/index.php/catalog/2783/related-materials

ethiop16_cover_complete <- read_dta("data/lsms/ETH_2015_ESS_v03_M_STATA/sect_cover_hh_w3.dta")
ethiop16_cover <- ethiop16_cover_complete |> 
  select(ea_id2, household_id2)

ethiop16_roster_complete <- read_dta("data/lsms/ETH_2015_ESS_v03_M_STATA/sect1_hh_w3.dta")
ethiop16_roster <- ethiop16_roster_complete |> 
  select(saq07, saq08, ea_id2, household_id2, individual_id2)

ethiop16_health_complete <- read_dta("data/lsms/ETH_2015_ESS_v03_M_STATA/sect3_hh_w3.dta")
ethiop16_health <- ethiop16_health_complete |> 
  select(hh_s3q12, hh_s3q13, hh_s3q14, hh_s3q15, hh_s3q16, hh_s3q17, ea_id2, household_id2, individual_id2)


## 3.1.K) ethiop14 ----
# We use ea_id2, household_id2, individual_id2 as indicated in Ethiopia Socioeconomic Survey (ESS) Wave Two (2013/2014), Basic Information Document, here https://microdata.worldbank.org/index.php/catalog/2247/related-materials

ethiop14_cover_complete <- read_dta("data/lsms/ETH_2013_ESS_v03_M_STATA/sect_cover_hh_w2.dta")
ethiop14_cover <- ethiop14_cover_complete |> 
  select(ea_id2, household_id2)

ethiop14_roster_complete <- read_dta("data/lsms/ETH_2013_ESS_v03_M_STATA/sect1_hh_w2.dta")
ethiop14_roster <- ethiop14_roster_complete |> 
  select(ea_id2, household_id2, individual_id2)

ethiop14_health_complete <- read_dta("data/lsms/ETH_2013_ESS_v03_M_STATA/sect3_hh_w2.dta")
ethiop14_health <- ethiop14_health_complete |> 
  select(hh_s3q12, hh_s3q13, hh_s3q14, hh_s3q15, hh_s3q16, hh_s3q17, ea_id2, household_id2, individual_id2)


## 3.1.L) ethiop12 ----

ethiop12_cover_complete <- read_dta("data/lsms/ETH_2011_ERSS_v02_M_Stata8/sect_cover_hh_w1.dta")
ethiop12_cover <- ethiop12_cover_complete |> 
  select(ea_id, household_id)

ethiop12_roster_complete <- read_dta("data/lsms/ETH_2011_ERSS_v02_M_Stata8/sect1_hh_w1.dta")
ethiop12_roster <- ethiop12_roster_complete |> 
  select(ea_id, household_id, individual_id)

ethiop12_health_complete <- read_dta("data/lsms/ETH_2011_ERSS_v02_M_Stata8/sect3_hh_w1.dta")
ethiop12_health <- ethiop12_health_complete |> 
  select(hh_s3q12, hh_s3q13, hh_s3q14, hh_s3q15, hh_s3q16, hh_s3q17, ea_id, household_id, individual_id)

ethiop12_gps_complete <- read_dta("data/lsms/ETH_2011_ERSS_v02_M_Stata8/Pub_ETH_HouseholdGeovariables_Y1.dta")
ethiop12_gps <- ethiop12_gps_complete |> 
  select(ea_id, household_id)


## 3.1.M) guibi21 ----

guibi21_cover_complete <- read_dta("data/lsms/GNB_2021_EHCVM-2_v01_M_STATA14/s00_me_gnb2021.dta")
guibi21_cover <- guibi21_cover_complete |> 
  select(grappe, menage)

guibi21_health_complete <- read_dta("data/lsms/GNB_2021_EHCVM-2_v01_M_STATA14/s03_me_gnb2021.dta")
guibi21_health <- guibi21_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, membres__id)

guibi21_indiv_charact_complete <- read_dta("data/lsms/GNB_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_gnb2021.dta")
guibi21_indiv_charact <- guibi21_indiv_charact_complete |> 
  select(grappe, menage, numind)

guibi21_weighting_complete <- read_dta("data/lsms/GNB_2021_EHCVM-2_v01_M_STATA14/ehcvm_ponderations_gnb2021.dta")
guibi21_weighting <- guibi21_weighting_complete |> 
  select(grappe, menage)

# Below no key to join to the other datasets -> not sure but I think so
#guibi21_nsu_complete <- read_dta("data/lsms/GNB_2021_EHCVM-2_v01_M_STATA14/ehcvm_nsu_gnb2021.dta")
#guibi21_nsu <- guibi21_nsu_complete |> 
#  select()


## 3.1.N) guibi19 ----

guibi19_cover_complete <- read_dta("data/lsms/GNB_2018_EHCVM_v02_M_Stata/s00_me_gnb2018.dta")
guibi19_cover <- guibi19_cover_complete |> 
  select(grappe, menage)

guibi19_health_complete <- read_dta("data/lsms/GNB_2018_EHCVM_v02_M_Stata/s03_me_gnb2018.dta")
guibi19_health <- guibi19_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a)

guibi19_indiv_charact_complete <- read_dta("data/lsms/GNB_2018_EHCVM_v02_M_Stata/ehcvm_individu_gnb2018.dta")
guibi19_indiv_charact <- guibi19_indiv_charact_complete |> 
  select(grappe, menage, numind)

guibi19_weighting_complete <- read_dta("data/lsms/GNB_2018_EHCVM_v02_M_Stata/ehcvm_ponderations_gnb2018.dta")
guibi19_weighting <- guibi19_weighting_complete |> 
  select(grappe)

guibi19_gps_complete <- read_dta("data/lsms/GNB_2018_EHCVM_v02_M_Stata/grappe_gps_gnb2018.dta")
guibi19_gps <- guibi19_gps_complete |> 
  select(grappe)


## 3.1.O) malawi20 ----
# For the merging of tables at the household level, we only use the variable case_id (unique household identifier) because we don't have this variable for the health and roster files, and case_id uniquely identifies each household

malawi20_cover_complete <- read_dta("data/lsms/MWI_2019_IHS-V_v06_M_Stata/hh_mod_a_filt.dta")
malawi20_cover <- malawi20_cover_complete |> 
  select(ea_id, case_id)

malawi20_roster_complete <- read_dta("data/lsms/MWI_2019_IHS-V_v06_M_Stata/HH_MOD_B.dta")
malawi20_roster <- malawi20_roster_complete |> 
  select(case_id, PID)

malawi20_health_complete <- read_dta("data/lsms/MWI_2019_IHS-V_v06_M_Stata/HH_MOD_D.dta")
malawi20_health <- malawi20_health_complete |> 
  select(hh_d24, hh_d25, hh_d26, hh_d27, hh_d28, hh_d29, case_id, PID)

malawi20_gps_complete <- read_dta("data/lsms/MWI_2019_IHS-V_v06_M_Stata/householdgeovariables_ihs5.dta")
malawi20_gps <- malawi20_gps_complete |> 
  select(case_id)


## 3.1.P) malawi19lsms ----

malawi19lsms_cover_complete <- read_dta("data/lsms/MWI_2010-2019_IHPS_v06_M_Stata/hh_mod_a_filt_19.dta")
malawi19lsms_cover <- malawi19lsms_cover_complete |> 
  select(ea_id, y4_hhid)

malawi19lsms_roster_complete <- read_dta("data/lsms/MWI_2010-2019_IHPS_v06_M_Stata/hh_mod_b_19.dta")
malawi19lsms_roster <- malawi19lsms_roster_complete |> 
  select(y4_hhid, PID)

malawi19lsms_health_complete <- read_dta("data/lsms/MWI_2010-2019_IHPS_v06_M_Stata/hh_mod_d_19.dta")
malawi19lsms_health <- malawi19lsms_health_complete |> 
  select(hh_d24, hh_d25, hh_d26, hh_d27, hh_d28, hh_d29, y4_hhid, PID)

malawi19lsms_gps_complete <- read_dta("data/lsms/MWI_2010-2019_IHPS_v06_M_Stata/householdgeovariables_y4.dta")
malawi19lsms_gps <- malawi19lsms_gps_complete |> 
  select(y4_hhid)


## 3.1.Q) malawi16lsms ----

# No disability data in this dataset
#malawi16lsms_ <- read_dta("data/lsms/")


## 3.1.R) malawi13 ----

# No disability data in this dataset
#malawi13_ <- read_dta("data/lsms/")


## 3.1.S) malawi11 ----

malawi11fs_cover_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/hh_mod_a_filt_full_sample.dta")
malawi11fs_cover <- malawi11fs_cover_complete |> 
  select(ea_id, case_id)

malawi11panel_cover_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/hh_mod_a_filt_panel.dta")
malawi11panel_cover <- malawi11panel_cover_complete |> 
  select(ea_id, case_id)

malawi11fs_roster_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/hh_mod_b_full_sample.dta")
malawi11fs_roster <- malawi11fs_roster_complete |> 
  select(ea_id, case_id, id_code)

malawi11panel_roster_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/hh_mod_b_panel.dta")
malawi11panel_roster <- malawi11panel_roster_complete |> 
  select(ea_id, case_id, id_code)

malawi11fs_health_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/hh_mod_d_full_sample.dta")
malawi11fs_health <- malawi11fs_health_complete |> 
  select(hh_d24, hh_d25, hh_d26, hh_d27, hh_d28, hh_d29, ea_id, case_id, id_code)

malawi11panel_health_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/hh_mod_d_panel.dta")
malawi11panel_health <- malawi11panel_health_complete |> 
  select(hh_d24, hh_d25, hh_d26, hh_d27, hh_d28, hh_d29, ea_id, case_id, id_code)

malawi11_gps_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/householdgeovariables.dta")
malawi11_gps <- malawi11_gps_complete |> 
  select(ea_id, case_id)

malawi11_cons_complete <- read_dta("data/lsms/MWI_2010_IHS-III_v01_M_STATA8/Round 1 (2010) Consumption Aggregate.dta")
malawi11_cons <- malawi11_cons_complete |> 
  select(ea_id, case_id)


## 3.1.T) mali21 ----

mali21_cover_complete <- read_dta("data/lsms/MLI_2021_EHCVM-2_v01_M_STATA14/s00_me_mli2021.dta")
mali21_cover <- mali21_cover_complete |> 
  select(grappe, menage)

mali21_health_complete <- read_dta("data/lsms/MLI_2021_EHCVM-2_v01_M_STATA14/s03_me_mli2021.dta")
mali21_health <- mali21_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, membres__id)

mali21_indiv_charact_complete <- read_dta("data/lsms/MLI_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_mli2021.dta")
mali21_indiv_charact <- mali21_indiv_charact_complete |> 
  select(grappe, menage, numind)

mali21_weighting_complete <- read_dta("data/lsms/MLI_2021_EHCVM-2_v01_M_STATA14/ehcvm_ponderations_mli2021.dta")
mali21_weighting <- mali21_weighting_complete |> 
  select(grappe, menage)

# Below for strata but no key to join to the other datasets
#mali21_nsu_complete <- read_dta("data/lsms/MLI_2021_EHCVM-2_v01_M_STATA14/ehcvm_nsu_mli2021.dta")
#mali21_nsu <- mali21_nsu_complete |> 
#  select()


## 3.1.U) mali18lsms ----

mali18lsms_cover_complete <- read_dta("data/lsms/MLI_2018_EHCVM_v02_M_Stata/s00_me_mli2018.dta")
mali18lsms_cover <- mali18lsms_cover_complete |> 
  select(grappe, menage)

mali18lsms_health_complete <- read_dta("data/lsms/MLI_2018_EHCVM_v02_M_Stata/s03_me_mli2018.dta")
mali18lsms_health <- mali18lsms_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a)

mali18lsms_indiv_charact_complete <- read_dta("data/lsms/MLI_2018_EHCVM_v02_M_Stata/ehcvm_individu_mli2018.dta")
mali18lsms_indiv_charact <- mali18lsms_indiv_charact_complete |> 
  select(grappe, menage, numind)

mali18lsms_gps_complete <- read_dta("data/lsms/MLI_2018_EHCVM_v02_M_Stata/grappe_gps_mli2018.dta")
mali18lsms_gps <- mali18lsms_gps_complete |> 
  select(grappe)


## 3.1.V) niger21 ----

niger21_cover_complete <- read_dta("data/lsms/NER_2021_EHCVM-2_v01_M_STATA14/s00_me_ner2021.dta")
niger21_cover <- niger21_cover_complete |> 
  select(grappe, menage)

niger21_health_complete <- read_dta("data/lsms/NER_2021_EHCVM-2_v01_M_STATA14/s03_me_ner2021.dta")
niger21_health <- niger21_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, membres__id)

niger21_indiv_charact_complete <- read_dta("data/lsms/NER_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_ner2021.dta")
niger21_indiv_charact <- niger21_indiv_charact_complete |> 
  select(grappe, menage, numind)

# Below for strata but no key to join to the other datasets
#niger21_nsu_complete <- read_dta("data/lsms/NER_2021_EHCVM-2_v01_M_STATA14/ehcvm_nsu_ner2021.dta")
#niger21_nsu <- niger21_nsu_complete |> 
#  select()

niger21_cons_complete <- read_dta("data/lsms/NER_2021_EHCVM-2_v01_M_STATA14/ehcvm_conso_ner2021.dta")
niger21_cons <- niger21_cons_complete |> 
  select(grappe, menage)


## 3.1.W) niger18 ----

niger18_cover_complete <- read_dta("data/lsms/NER_2018_EHCVM_v02_M_Stata/s00_me_ner2018.dta")
niger18_cover <- niger18_cover_complete |> 
  select(grappe, menage)

niger18_health_complete <- read_dta("data/lsms/NER_2018_EHCVM_v02_M_Stata/s03_me_ner2018.dta")
niger18_health <- niger18_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a)

niger18_indiv_charact_complete <- read_dta("data/lsms/NER_2018_EHCVM_v02_M_Stata/ehcvm_individu_ner2018.dta")
niger18_indiv_charact <- niger18_indiv_charact_complete |> 
  select(grappe, menage, numind)

niger18_gps_complete <- read_dta("data/lsms/NER_2018_EHCVM_v02_M_Stata/grappe_gps_ner2018.dta")
niger18_gps <- niger18_gps_complete |> 
  select(grappe)


## 3.1.X) nigeria24 ----

nigeria24_cover_complete <- read_dta("data/lsms/NGA_2023_GHSP-W5_v01_M_Stata/secta_plantingw5.dta")
nigeria24_cover <- nigeria24_cover_complete |> 
  select(ea, hhid)

nigeria24_health_complete <- read_dta("data/lsms/NGA_2023_GHSP-W5_v01_M_Stata/sect3_plantingw5.dta")
nigeria24_health <- nigeria24_health_complete |> 
  select(s3q23, s3q24, s3q25, s3q26, s3q27, s3q28, ea, hhid, indiv)

nigeria24_indiv_charact_complete <- read_dta("data/lsms/NGA_2023_GHSP-W5_v01_M_Stata/sect1_plantingw5.dta")
nigeria24_indiv_charact <- nigeria24_indiv_charact_complete |> 
  select(ea, hhid, indiv)


## 3.1.Y) nigeria19unif ----

# This dataset only contains the gathered answers for waves 1, 2, 3 and 4 of the General Household Survey, so it doesn't add information

#nigeria19unif_roster_complete <- read_dta("data/lsms/NGA_2010-2019_NUPD_v01_M_Stata/nup_phx_mod_flap_a_roster_b.dta")
#nigeria19unif_roster <- nigeria19unif_roster_complete |> 
#  select(ea, hhid, indiv)

#nigeria19unif_health_complete <- read_dta("data/lsms/NGA_2010-2019_NUPD_v01_M_Stata/nup_phx_mod_d.dta")
#nigeria19unif_health <- nigeria19unif_health_complete |> 
#  select(hd_26, hd_28, hd_30, hd_32, hd_34, hd_36, ea, hhid, indiv)


## 3.1.Z) nigeria19 ----

nigeria19_roster_complete <- read_dta("data/lsms/NGA_2018_GHSP-W4_v03_M_Stata12/sect1_harvestw4.dta")
nigeria19_roster <- nigeria19_roster_complete |> 
  select(ea, hhid, indiv)

nigeria19_health_complete <- read_dta("data/lsms/NGA_2018_GHSP-W4_v03_M_Stata12/sect4a_harvestw4.dta")
nigeria19_health <- nigeria19_health_complete |> 
  select(s4aq23, s4aq25, s4aq27, s4aq29, s4aq31, s4aq33, ea, hhid, indiv)

nigeria19_gps_complete <- read_dta("data/lsms/NGA_2018_GHSP-W4_v03_M_Stata12/nga_householdgeovars_y4.dta")
nigeria19_gps <- nigeria19_gps_complete |> 
  select(hhid)


## 3.1.Abis) nigeria18lsms ----

nigeria18lsms_cover_complete <- read_dta("data/lsms/NGA_2018_LSS_v01_M_Stata/secta_cover.dta")
nigeria18lsms_cover <- nigeria18lsms_cover_complete |> 
  select(ea, hhid)

nigeria18lsms_roster_complete <- read_dta("data/lsms/NGA_2018_LSS_v01_M_Stata/sect1_roster.dta")
nigeria18lsms_roster <- nigeria18lsms_roster_complete |> 
  select(ea, hhid, indiv)

nigeria18lsms_health_complete <- read_dta("data/lsms/NGA_2018_LSS_v01_M_Stata/sect3_health.dta")
nigeria18lsms_health <- nigeria18lsms_health_complete |> 
  select(s03q22, s03q23, s03q24, s03q25, s03q26, s03q27, ea, hhid, indiv)


## 3.1.Bbis) nigeria13 ----

nigeria13_cover_complete <- read_dta("data/lsms/NGA_2012_GHSP-W2_v02_M_STATA/secta_harvestw2.dta")
nigeria13_cover <- nigeria13_cover_complete |> 
  select(ea, hhid)

nigeria13_roster_complete <- read_dta("data/lsms/NGA_2012_GHSP-W2_v02_M_STATA/sect1_harvestw2.dta")
nigeria13_roster <- nigeria13_roster_complete |> 
  select(ea, hhid, indiv)

nigeria13_health_complete <- read_dta("data/lsms/NGA_2012_GHSP-W2_v02_M_STATA/sect4a_harvestw2.dta")
nigeria13_health <- nigeria13_health_complete |> 
  select(s4aq23, s4aq25, s4aq27, s4aq29, s4aq31, s4aq33, ea, hhid, indiv)

nigeria13_gps_complete <- read_dta("data/lsms/NGA_2012_GHSP-W2_v02_M_STATA/NGA_HouseholdGeovars_Y2.dta")
nigeria13_gps <- nigeria13_gps_complete |> 
  select(ea, hhid)


## 3.1.Cbis) nigeria11 ----

nigeria11_cover_complete <- read_dta("data/lsms/NGA_2010_GHSP-W1_v03_M_STATA/secta_harvestw1.dta")
nigeria11_cover <- nigeria11_cover_complete |> 
  select(ea, hhid)

nigeria11_health_complete <- read_dta("data/lsms/NGA_2010_GHSP-W1_v03_M_STATA/sect4a_harvestw1.dta")
nigeria11_health <- nigeria11_health_complete |> 
  select(s4aq23, s4aq25, s4aq27, s4aq29, s4aq31, s4aq33, ea, hhid, indiv)

nigeria11_indiv_charact_complete <- read_dta("data/lsms/NGA_2010_GHSP-W1_v03_M_STATA/sect1_harvestw1.dta")
nigeria11_indiv_charact <- nigeria11_indiv_charact_complete |> 
  select(ea, hhid, indiv)

nigeria11_gps_complete <- read_dta("data/lsms/NGA_2010_GHSP-W1_v03_M_STATA/NGA_HouseholdGeovariables_Y1.dta")
nigeria11_gps <- nigeria11_gps_complete |> 
  select(ea, hhid)


## 3.1.Dbis) senegal21 ----

senegal21_cover_complete <- read_dta("data/lsms/SEN_2021_EHCVM-2_v01_M_STATA14/s00_me_sen2021.dta")
senegal21_cover <- senegal21_cover_complete |> 
  select(grappe, menage)

senegal21_health_complete <- read_dta("data/lsms/SEN_2021_EHCVM-2_v01_M_STATA14/s03_me_sen2021.dta")
senegal21_health <- senegal21_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, membres__id)

senegal21_indiv_charact_complete <- read_dta("data/lsms/SEN_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_sen2021.dta")
senegal21_indiv_charact <- senegal21_indiv_charact_complete |> 
  select(grappe, menage, numind)

senegal21_weighting <- read_dta("data/lsms/SEN_2021_EHCVM-2_v01_M_STATA14/ehcvm_ponderations_sen2021.dta")
senegal21_weighting <- senegal21_weighting |> 
  select(grappe, menage)


## 3.1.Ebis) senegal18 ----

senegal18_cover_complete <- read_dta("data/lsms/SEN_2018_EHCVM_v02_M_Stata/s00_me_sen2018.dta")
senegal18_cover <- senegal18_cover_complete |> 
  select(grappe, menage)

senegal18_health_complete <- read_dta("data/lsms/SEN_2018_EHCVM_v02_M_Stata/s03_me_sen2018.dta")
senegal18_health <- senegal18_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a) # s01q00a is the right variable for making an individual identifier, contrary to s03q00

senegal18_indiv_charact_complete <- read_dta("data/lsms/SEN_2018_EHCVM_v02_M_Stata/ehcvm_individu_sen2018.dta")
senegal18_indiv_charact <- senegal18_indiv_charact_complete |> 
  select(grappe, menage, numind)

senegal18_gps_complete <- read_dta("data/lsms/SEN_2018_EHCVM_v02_M_Stata/grappe_gps_sen2018.dta")
senegal18_gps <- senegal18_gps_complete |> 
  select(grappe)


## 3.1.Fbis) tanza21 ----

tanza21_cover_complete <- read_dta("data/lsms/TZA_2020_NPS-R5_v02_M_STATA14/hh_sec_a.dta")
tanza21_cover <- tanza21_cover_complete |> 
  select(y5_cluster, y5_hhid)

tanza21_roster_complete <- read_dta("data/lsms/TZA_2020_NPS-R5_v02_M_STATA14/hh_sec_b.dta")
tanza21_roster <- tanza21_roster_complete |> 
  select(y5_hhid, indidy5)

tanza21_health_complete <- read_dta("data/lsms/TZA_2020_NPS-R5_v02_M_STATA14/hh_sec_d.dta")
tanza21_health <- tanza21_health_complete |> 
  select(hh_d17, hh_d19, hh_d21, hh_d23, hh_d25, hh_d27, y5_hhid, indidy5)

# Latitudes and longitudes are confidential in this file
#tanza21_gps_complete <- read_dta("data/lsms/TZA_2020_NPS-R5_v02_M_STATA14/cm_sec_a.dta")
#tanza21_gps <- tanza21_gps_complete |> 
#  select()


## 3.1.Gbis) tanza20 ----

tanza20_cover_complete <- read_dta("data/lsms/TZA_2019_NPD-SDD_v06_M_STATA12/HH_SEC_A.dta")
tanza20_cover <- tanza20_cover_complete |> 
  select(sdd_cluster, sdd_hhid)

tanza20_roster_complete <- read_dta("data/lsms/TZA_2019_NPD-SDD_v06_M_STATA12/HH_SEC_B.dta")
tanza20_roster <- tanza20_roster_complete |> 
  select(sdd_hhid, sdd_indid)

tanza20_health_complete <- read_dta("data/lsms/TZA_2019_NPD-SDD_v06_M_STATA12/HH_SEC_D.dta")
tanza20_health <- tanza20_health_complete |> 
  select(hh_d17, hh_d19, hh_d21, hh_d23, hh_d25, hh_d27, sdd_hhid, sdd_indid)

# Latitudes and longitudes are confidential in this file
#tanza20_gps_complete <- read_dta("data/lsms/TZA_2019_NPD-SDD_v06_M_STATA12/CM_SEC_A.dta")
#tanza20_gps <- tanza20_gps_complete |> 
#  select()


## 3.1.Hbis) tanza15 ----

tanza15_cover_complete <- read_dta("data/lsms/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_a.dta")
tanza15_cover <- tanza15_cover_complete |> 
  select(clusterid, y4_hhid)

tanza15_roster_complete <- read_dta("data/lsms/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_b.dta")
tanza15_roster <- tanza15_roster_complete |> 
  select(y4_hhid, indidy4)

tanza15_health_complete <- read_dta("data/lsms/TZA_2014_NPS-R4_v03_M_STATA11/hh_sec_d.dta")
tanza15_health <- tanza15_health_complete |> 
  select(hh_d17, hh_d19, hh_d21, hh_d23, hh_d25, hh_d27, y4_hhid, indidy4)

tanza15_gps_complete <- read_dta("data/lsms/TZA_2014_NPS-R4_v03_M_STATA11/npsy4.ea.offset.dta")
tanza15_gps <- tanza15_gps_complete |> 
  select(clusterid)


## 3.1.Ibis) tanza15ext ----

tanza15ext_cover_complete <- read_dta("data/lsms/TZA_2014_NPS-R4_v03_M_v03_A_EXT_STATA11/hh_sec_a.dta")
tanza15ext_cover <- tanza15ext_cover_complete |> 
  select(clusterid, y4_hhid)

tanza15ext_roster_complete <- read_dta("data/lsms/TZA_2014_NPS-R4_v03_M_v03_A_EXT_STATA11/hh_sec_b.DTA")
tanza15ext_roster <- tanza15ext_roster_complete |> 
  select(y4_hhid, indidy4)

tanza15ext_health_complete <- read_dta("data/lsms/TZA_2014_NPS-R4_v03_M_v03_A_EXT_STATA11/hh_sec_d.DTA")
tanza15ext_health <- tanza15ext_health_complete |> 
  select(hh_d17, hh_d19, hh_d21, hh_d23, hh_d25, hh_d27, y4_hhid, indidy4)


## 3.1.Jbis) tanza15unif ----

# This dataset only contains the gathered answers for waves 1, 2, 3 and 4 of the General Household Survey, so it doesn't add information

# tanza15unif_cover_complete <- read_dta("data/lsms/TZA_2008-2014_NPS-UPD_v01_M_STATA/upd4_hh_a.dta")
# tanza15unif_cover <- tanza15unif_cover_complete |> 
#   select(clusterid, r_hhid)
# 
# tanza15unif_roster_complete <- read_dta("data/lsms/TZA_2008-2014_NPS-UPD_v01_M_STATA/upd4_hh_b.dta")
# tanza15unif_roster <- tanza15unif_roster_complete |> 
#   select(r_hhid, r_id)
# 
# tanza15unif_health_complete <- read_dta("data/lsms/TZA_2008-2014_NPS-UPD_v01_M_STATA/upd4_hh_d.dta")
# tanza15unif_health <- tanza15unif_health_complete |> 
#   select(hd_23, hd_25, hd_27, hd_29, hd_31, hd_33, r_hhid, r_id, round)


## 3.1.Kbis) tanza11 ----

tanza11_cover_complete <- read_dta("data/lsms/TZA_2010_NPS-R2_v03_M_STATA8/HH_SEC_A.dta")
tanza11_cover <- tanza11_cover_complete |> 
  select(clusterid, y2_hhid)

tanza11_roster_complete <- read_dta("data/lsms/TZA_2010_NPS-R2_v03_M_STATA8/HH_SEC_B.dta")
tanza11_roster <- tanza11_roster_complete |> 
  select(y2_hhid, indidy2)

tanza11_health_complete <- read_dta("data/lsms/TZA_2010_NPS-R2_v03_M_STATA8/HH_SEC_D.dta")
tanza11_health <- tanza11_health_complete |> 
  select(hh_d17, hh_d19, hh_d21, hh_d23, hh_d25, hh_d27, y2_hhid, indidy2)

tanza11_gps_complete <- read_dta("data/lsms/TZA_2010_NPS-R2_v03_M_STATA8/HH.Geovariables_Y2.dta")
tanza11_gps <- tanza11_gps_complete |> 
  select(y2_hhid)


## 3.1.Lbis) togo21 ----

togo21_cover_complete <- read_dta("data/lsms/TGO_2021_EHCVM-2_v01_M_STATA14/s00_me_tgo2021.dta")
togo21_cover <- togo21_cover_complete |> 
  select(grappe, menage)

togo21_health_complete <- read_dta("data/lsms/TGO_2021_EHCVM-2_v01_M_STATA14/s03_me_tgo2021.dta")
togo21_health <- togo21_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, membres__id)

togo21_indiv_charact_complete <- read_dta("data/lsms/TGO_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_tgo2021.dta")
togo21_indiv_charact <- togo21_indiv_charact_complete |> 
  select(grappe, menage, numind)

togo21_weighting_complete <- read_dta("data/lsms/TGO_2021_EHCVM-2_v01_M_STATA14/ehcvm_ponderations_tgo2021.dta")
togo21_weighting <- togo21_weighting_complete |> 
  select(grappe, menage)

# Below for strata but no key to join to the other datasets -> not sure but I think so
#togo21_nsu_complete <- read_dta("data/lsms/TGO_2021_EHCVM-2_v01_M_STATA14/ehcvm_nsu_tgo2021.dta")
#togo21_nsu <- togo21_nsu_complete |> 
#  select()


## 3.1.Mbis) togo18 ----

togo18_cover_complete <- read_dta("data/lsms/TGO_2018_EHCVM_v02_M_Stata/s00_me_tgo2018.dta")
togo18_cover <- togo18_cover_complete |> 
  select(grappe, menage)

togo18_health_complete <- read_dta("data/lsms/TGO_2018_EHCVM_v02_M_Stata/s03_me_tgo2018.dta")
togo18_health <- togo18_health_complete |> 
  select(s03q41, s03q42, s03q43, s03q44, s03q45, s03q46, grappe, menage, s01q00a)  # s01q00a is the right variable for making an individual identifier, contrary to s03q00

togo18_indiv_charact_complete <- read_dta("data/lsms/TGO_2018_EHCVM_v02_M_Stata/ehcvm_individu_tgo2018.dta")
togo18_indiv_charact <- togo18_indiv_charact_complete |> 
  select(grappe, menage, numind)

togo18_gps_complete <- read_dta("data/lsms/TGO_2018_EHCVM_v02_M_Stata/grappe_gps_tgo2018.dta")
togo18_gps <- togo18_gps_complete |> 
  select(grappe)


## 3.1.Nbis) uganda11 ----

uganda11_cover_complete <- read_dta("data/lsms/UGA_2010_UNPS_v02_M_STATA12/GSEC1.dta")
uganda11_cover <- uganda11_cover_complete |> 
  select(HHID)

uganda11_roster_complete <- read_dta("data/lsms/UGA_2010_UNPS_v02_M_STATA12/GSEC2.dta")
uganda11_roster <- uganda11_roster_complete |> 
  select(HHID, PID)

uganda11_health_complete <- read_dta("data/lsms/UGA_2010_UNPS_v02_M_STATA12/GSEC7A.dta")
uganda11_health <- uganda11_health_complete |> 
  select(h7q2a, h7q3a, h7q4a, h7q5a, h7q6a, h7q7a, HHID, PID)

uganda11_gps_complete <- read_dta("data/lsms/UGA_2010_UNPS_v02_M_STATA12/UNPS_Geovars_1011.dta")
uganda11_gps <- uganda11_gps_complete |> 
  select(HHID)


## 3.1.Obis) uganda09 ----

uganda09_cover_complete <- read_dta("data/lsms/UGA_2005_2009_UNPS_v02_M_STATA8/2009_GSEC1.dta")
uganda09_cover <- uganda09_cover_complete |> 
  select(HHID)

uganda09_roster_complete <- read_dta("data/lsms/UGA_2005_2009_UNPS_v02_M_STATA8/2009_GSEC2.dta")
uganda09_roster <- uganda09_roster_complete |> 
  select(HHID, PID)

uganda09_health_complete <- read_dta("data/lsms/UGA_2005_2009_UNPS_v02_M_STATA8/2009_GSEC7.dta")
uganda09_health <- uganda09_health_complete |> 
  select(H7q2a, H7q3a, H7q4a, H7q5a, H7q6a, H7q7a, Hhid, Pid)

uganda09_gps_complete <- read_dta("data/lsms/UGA_2005_2009_UNPS_v02_M_STATA8/2009_UNPS_Geovars_0910.dta")
uganda09_gps <- uganda09_gps_complete |> 
  select(HHID)


# 4.) Saving all the imported datasets ----
# In case it is useful to use the full DHS datasets:
# save(kenya22_complete, malawi16_complete, mali18_complete, maurit19_complete, mozam22_complete, nigeria18_complete, pakis17_complete, rwanda19_complete, senegal19_complete, safrica16_complete, tanza22_complete, uganda16_complete, file = "data/tmp/after01_complete.RData") # Multiple datasets are put in one RData file (see webin-R 3 at 16min30 to check the correct use of save() or here https://larmarange.github.io/analyse-R/export-de-donnees.html)


# DHS
dhs= tibble::lst(
  kenya22,
  malawi16,
  mali18,
  maurit19,
  mozam22,
  nigeria18,
  pakis17 ,
  rwanda19,
  senegal18,
  senegal19,
  safrica16,
  tanza22,
  uganda16,
  
  kenya22gps,
  malawi16gps,
  mali18gps,
  maurit19gps,
  mozam22gps,
  nigeria18gps,
  rwanda19gps,
  senegal18gps,
  senegal19gps,
  safrica16gps,
  tanza22gps,
  uganda16gps)

# MICS
mics= tibble::lst(
  benin21_mn,
  benin21_wm,
  centraf18_mn,
  centraf18_wm,
  chad19_mn,
  chad19_wm,
  comor22_mn,
  comor22_wm,
  drcong17_mn,
  drcong17_wm,
  eswat21_mn,
  eswat21_wm,
  gamb18_wm,
  gamb18_mn,
  ghana17_wm,
  ghana17_mn,
  guibi18_mn,
  guibi18_wm,
  lesot18_mn,
  lesot18_wm,
  madag18_mn,
  madag18_wm,
  malawi19_hh,
  malawi19_mn,
  malawi19_wm,
  saoto19_mn,
  saoto19_wm,
  sierral17_hh,
  sierral17_mn,
  sierral17_wm,
  togo17_mn,
  togo17_wm,
  yemen22_wm,
  zimbab19_mn,
  zimbab19_wm )

# LSMS

lsms= tibble::lst(
  benin21lsms_cover,
  benin21lsms_health,
  benin21lsms_indiv_charact,
  benin21lsms_weighting)


save(dhs, mics, lsms,
     file = "data/tmp/after01.RData") # Multiple datasets are put in one RData file (see webin-R 3 at 16min30 to check the correct use of save() or here https://larmarange.github.io/analyse-R/export-de-donnees.html)
