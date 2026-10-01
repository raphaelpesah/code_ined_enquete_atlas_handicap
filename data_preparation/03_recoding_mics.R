# Recoding of MICS surveys


# Pour chaque pays
# (1) DONE Noter (sur mon doc Stage) quels sont les cas où j'ai besoin d'utiliser la localisation des clusters pour rattacher les observations à un niveau admin2 (laisser tel quel les autres, i.e. tous les niveau 1, et tous les niveau 2 déjà traités)
# (2) DONE Regarder si l'entité de admin2 mentionné dans le GPS_displacement file (et pour laquelle plus de détail peut être dispo dans le codebook, ou bien le final report, ou bien google) correspond bien aux éléments de GADM niveau 2 (variable NAME_2 je pense) 
# (3) DONE Noter (sur le sheet Atlas) les cas où admin2 de GPS_displacement ne correspond pas à un fonds de carte facilement trouvable, et dans ce cas utiliser le fonds de carte le plus pertinent (typiquement GADM level 2)
# (4) DONE Déterminer quel fichier (gadm vs dhs) je prends pour pays99_basemap2
# (5) DONE Vérifie que la jointure entre pays99gps et pays99_basemap2 se fait bien, dont anti_join vide des deux côtés
# (6) DONE En particulier peut-être noter ou faire afficher les admin2 de pays99_basemap2 pour lesquels il n'y a auquel cluster dans le dataset pays99gps
# (7) DONE Faire le calcul des prévalences au niveau 2 dans la section 6) Creation of prevalence tibbles pour les pays nécessaires (mais pas besoin de faire le rattachement au fond de carte parce que normalement il aura déjà été fait dans cette section Joining GPS data => En fait si!)
# (8) DONE Ecrire code pour l'affichage des cartes, le mettre dans le quarto d'analyse
# (9) DONE Modifier les tbl_svysummary tout en bas de ce fichier pour mettre les bons noms d'entité admin1 et les bons noms d'admin2
# (10) DONE Mettre ces tbl_svysummary dans le quarto d'analyse

# (11) DONE Ecrire le code pour traiter les autres pays MICS que Benin (au niveau 1 ou 2 si dispo en rattachant par nom le dataset avec le bon fond de carte)
# (12) DONE (but in fact we didn't need to to do the st_join, since for all the surveys with a gps dataset, there was a variable whose level corresponded to the level of displacement of the points ; this means that doing a st_join() to a smaller level, e.g. with a basemap3, would not be correct) Pour les MICS pour lesquels c'était pas possible de faire la carte admin2 et pour lesquels on a les données GPS, faire le processus st_join() (en fait faire le processus dès que GPS est dispo)
# (13) Mettre ces cartes MICS dans le quarto d'analyse
# (14) Faire les tbl_svysummary pour les MICS et les mettre dans le quarto d'analyse
# (15) Faire le même processus pour LSMS (import, traitement, rattachement fonds de cartes et gps, calcul prévalences, réalisation carte)



# 0) Loading packages and RData from 01_import ----

library(srvyr) # loaded before tidyverse to give priority to tidyverse version of functions like filter
library(tidyverse)
library(labelled)
library(survey)
library(questionr)
library(sf)


load("data/tmp/after02.RData")
# open the list containing data in the environment as dataframe separated for each country
list2env(mics, envir = .GlobalEnv)


# 0 bis) Merging of mn and wm datasets ----

benin21 <- benin21_mn |> # Since hl contains observations for mn and wm, we first merge the observations of mn and wm, then merge the resulting table with the column hhweight from hl
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>% # to remove the first character ; this technique was found in the help of the rename() function, and the sub() function was found here https://datasharkie.com/how-to-remove-first-character-from-string-in-r ; also I have to put this pipe %>% and not |> (for explanation, see https://www.tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/)
  mutate(across(AF6:AF12, ~ fct_recode(.x, "PAS DE DIFFICULTE" = "AUCUNE DIFFICULTE"))) |> # this renames the level "PAS DE DIFFICULTE" to "AUCUNE DIFFICULTE" for each variable from AF6 to AF12 (in the order of the dataset), we do this because "PAS DE DIFFICULTE" is the level name in the wm file for the disability variables
  mutate(AF11 = fct_recode(AF11, "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME" = "NE PEUT PAS DU TOUT  PRENDRE SOIN DE LUI-MEME")) %>% # "... D'ELLE MEME" is the name of the level in the wm file
  bind_rows(mn = ., wm = benin21_wm, .id = "file_source") |> # we checked that the number of observations in this table corresponds to the numbers in mn + wm
  mutate(AF9 = fct_recode(AF9, "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" = "NE PEUT PAS DU TOUT ENTENDRENE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIER")) |> # correcting a small label mistake that comes directly from the imported wm file (label mistake was not in mn file)
  filter(!is.na(wmweight)) # Not sure it's correct to do this, but Otherwise we get an error when we do the weighting # We checked that the 20 observations with NA wmweight also had NA for disability variables

centraf18 <- centraf18_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(across(AF6:AF12, ~ fct_recode(.x, "NON REPONSE" = "PAS DE REPONSE"))) |> # As in benin21, different levels names between mn and wm files for all disability variables
  mutate(AF11 = fct_recode(AF11, "NE PEUT PAS DU TOUT PRENDRE SOIN D?ELLE-MEME" = "NE PEUT PAS DU TOUT  PRENDRE SOIN DE LUI-MEME")) %>%
  bind_rows(mn = ., wm = centraf18_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm
  
chad19 <- chad19_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(AF11 = fct_recode(AF11, "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME" = "NE PEUT PAS DU TOUT  PRENDRE SOIN DE LUI-MEME")) |> 
  mutate(AF9 = fct_recode(AF9, "AUCUNE DIFFICULTE" = "NO DIFFICULTY", "QUELQUES DIFFICULTES" = "SOME DIFFICULTY", "BEAUCOUP DE DIFFICULTES" = "A LOT OF DIFFICULTY", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" = "CANNOT WALK/CLIMB STEPS AT ALL", "NON REPONSE" = "NO RESPONSE")) %>% # Labelling mistake where all the MAF9 levels are in english in chad19
  bind_rows(mn = ., wm = chad19_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm
  
comor22 <- comor22_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(across(AF6:AF12, ~ fct_recode(.x, "PAS DE DIFFICULTE" = "AUCUNE DIFFICULTE"))) %>% # this renames the level "PAS DE DIFFICULTE" to "AUCUNE DIFFICULTE" for each variable from AF6 to AF12 (in the order of the dataset), we do this because "PAS DE DIFFICULTE" is the level name in the wm file for the disability variables
  bind_rows(mn = ., wm = comor22_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

drcong17 <- drcong17_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(AF11 = fct_recode(AF11, "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME" = "NE PEUT PAS DU TOUT  PRENDRE SOIN DE LUI-MEME")) %>%
  bind_rows(mn = ., wm = drcong17_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

eswat21 <- eswat21_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  bind_rows(mn = ., wm = eswat21_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

gamb18 <- gamb18_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) |> 
  mutate(AF8 = fct_recode(AF8, "CANNOT HEAR  AT ALL" = "CANNOT HEAR AT ALL")) %>% # Labelling mistake of a different spacing between mn and wm
  mutate(AF9 = fct_recode(AF9, "CANNOT  WALK/ CLIMB STEPS AT ALL" = "CANNOT WALK/CLIMB STEPS AT ALL")) %>% # Labelling mistake of a different spacing between mn and wm
  mutate(AF10 = fct_recode(AF10, "CANNOT REMEMBER/ CONCENTRATE AT ALL" = "CANNOT REMEMBER/CONCENTRATE AT ALL")) %>% # Labelling mistake of a different spacing between mn and wm
  mutate(HH7 = fct_recode(HH7, "BANJUL" = "Banjul", "KANIFING" = "Kanifing", "BRIKAMA" = "Brikama", "MANSAKONKO" = "Mansakonko", "KEREWAN" = "Kerewan", "KUNTAUR" = "Kuntaur", "JANJANBUREH" = "Janjanbureh", "BASSE" = "Basse")) %>% # Capitalized in wm but not in mn
  bind_rows(mn = ., wm = gamb18_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

ghana17 <- ghana17_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(AF9 = fct_recode(AF9, "CANNOT WALK/CLIMB STEPS  AT ALL" = "CANNOT WALK/CLIMB STEPS AT ALL")) %>% # Labelling mistake of a different spacing between mn and wm
  mutate(AF10 = fct_recode(AF10, "CANNOT REMMEBER/CONCENTRATE AT ALL" = "CANNOT REMEMBER/CONCENTRATE AT ALL")) %>% # Labelling mistake of the word "remember" in the wm file
  mutate(AF11 = fct_recode(AF11, "CANNOT CARE FOR SELF  AT ALL" = "CANNOT CARE FOR SELF AT ALL")) %>% # Labelling mistake of a different spacing between mn and wm
  bind_rows(mn = ., wm = ghana17_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

guibi18 <- guibi18_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  bind_rows(mn = ., wm = guibi18_wm, .id = "file_source") |> # we checked that the number of observations in this table corresponds to the numbers in mn + wm
  filter(!is.na(wmweight)) # Not sure it's correct to do this, but Otherwise we get an error when we do the weighting # We checked that the 16 observations with NA wmweight also had NA for disability variables
  
lesot18 <- lesot18_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(AF9 = fct_recode(AF9, "CANNOT WALK/ CLIMB STEPS AT ALL" = "CANNOT WALK/CLIMB STEPS AT ALL")) %>% # Labelling mistake of a different spacing between mn and wm
  mutate(AF10 = fct_recode(AF10, "CANNOT REMEMBER/ CONCENTRATE AT ALL" = "CANNOT REMEMBER/CONCENTRATE AT ALL")) %>% # Labelling mistake of a different spacing between mn and wm
  bind_rows(mn = ., wm = lesot18_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

madag18 <- madag18_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(AF11 = fct_recode(AF11, "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME" = "NE PEUT PAS DU TOUT  PRENDRE SOIN DE LUI-MEME")) %>%
  bind_rows(mn = ., wm = madag18_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

malawi19 <- malawi19_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  bind_rows(mn = ., wm = malawi19_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

malawi19 <- malawi19 |> unite("id", c("HH1", "HH2"), sep = "_", remove = F) # https://tidyr.tidyverse.org/reference/unite.html
malawi19_hh <- malawi19_hh |> unite("id", c("HH1", "HH2"), sep = "_", remove = T) # we don't need HH1 and HH2 at this point: we only want to append DISTRICT to malawi19

malawi19 <- malawi19 |> # For now 15 to 17 years old from mn and wm files not excluded even if their AF variables are NA ; we check that the hl table and the table after the join have the same number of observations, since we'll need all observations for the weighting (hence the use of full_join() rather than left_join())
  left_join(malawi19_hh, c("id" = "id")) # Note: to get the district names for malawi19, that we use for computing prevalence at the district level, we could also have used the variable stratum by just removing the Rural/Urban appendix ; We followed "Guidelines for Merging Data Files of a MICS Survey" found here https://mics.unicef.org/faq ; Check that all id match, in this case that we don't loose mn or wm observations : anti_join(malawi19, malawi19_hh, by = join_by(id == id))

saoto19 <- saoto19_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) |> 
  mutate(across(AF6:AF12, ~ fct_recode(.x, "SEM RESPOSTA" = "NÃO RESPONDE"))) %>% # As in benin21, different levels names between mn and wm files for all disability variables
  bind_rows(mn = ., wm = saoto19_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

sierral17 <- sierral17_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  bind_rows(mn = ., wm = sierral17_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm

sierral17 <- sierral17 |> unite("id", c("HH1", "HH2"), sep = "_", remove = F) # https://tidyr.tidyverse.org/reference/unite.html
sierral17_hh <- sierral17_hh |> unite("id", c("HH1", "HH2"), sep = "_", remove = T) # we don't need HH1 and HH2 at this point: we only want to append stratum to sierral17

sierral17 <- sierral17 |> # For now 15 to 17 years old from mn and wm files not excluded even if their AF variables are NA ; we check that the hl table and the table after the join have the same number of observations, since we'll need all observations for the weighting (hence the use of full_join() rather than left_join())
  left_join(sierral17_hh, c("id" = "id")) # We followed "Guidelines for Merging Data Files of a MICS Survey" found here https://mics.unicef.org/faq ; Check that all id match, in this case that we don't loose mn or wm observations : anti_join(sierral17, sierral17_hh, by = join_by(id == id))

togo17 <- togo17_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  mutate(AF11 = fct_recode(AF11, "NE PEUT PAS DU TOUT PRENDRE SOIN D'ELLE-MEME, SE LAVER, S'HABILLER" = "NE PEUT PAS DU TOUT PRENDRE SOIN DE LUI-MEME, SE LAVER, S'HABILLER")) %>%
  mutate(HH7 = fct_recode(HH7, "Maritime" = "MARITIME", "Plateaux" = "PLATEAUX", "Centrale" = "CENTRALE", "Kara" = "KARA", "Savanes" = "SAVANES", "Lomé Commune" = "LOME COMMUNE", "Golfe Urbain" = "GOLFE URBAIN")) %>% # Capitalized in mn but not in wm
  bind_rows(mn = ., wm = togo17_wm, .id = "file_source") |> # we checked that the number of observations in this table corresponds to the numbers in mn + wm
  filter(!is.na(wmweight)) # Not sure it's correct to do this, but Otherwise we get an error when we do the weighting # We checked that the 23 observations with NA wmweight also had NA for disability variables

yemen22 <- yemen22_wm # There is only a wm file for yemen22 so we don't have modifications to do (since for the other countries here we modify the variables in the mn file to make their name and levels similar to the ones in the wm file)

zimbab19 <- zimbab19_mn |> 
  rename(WB4 = MWB4, WAGE = MWAGE, wmweight = mnweight, welevel = mwelevel, disability = mdisability) |> 
  rename_with(~ sub(".", "", .x), starts_with("MAF")) %>%
  bind_rows(mn = ., wm = zimbab19_wm, .id = "file_source") # we checked that the number of observations in this table corresponds to the numbers in mn + wm


# 1) Creation of useful variables + labeling and cleaning of variable ----

## 1.A) benin21 ----

# Assigning descriptive names and labels to variables
benin21 <- benin21 |> mutate(
  country= "Benin", # to be able to merge all the surveys together
  year= 2021, # to be able to merge all the surveys together
  wt = wmweight, # not entirely sure, but this variable seems to correspond to hv005 in DHS (it has a value per cluster (HH1), not per individual)
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2, # This variable and LN are described in "Guidelines for Merging Data Files of a MICS Survey" found here https://mics.unicef.org/faq
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7, # called "Département" in the original label
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# For information (and for all countries except yemen22), at this point in the process the levels for the disability variables in the datasets are precisely the ones from the wm file of each country


# Creation of variable gender (based on the source file since men and women are found in two different files)
#benin21 <- benin21 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 


# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#benin21 <- benin21 |> 
mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |> # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#benin21 <- benin21 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_com == "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" | diff_wash == "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME" ~ "cannot do at all",  # the label for diff_com is not in benin21, we found it in chad19 (! Note that in chad19 the labels are in english for diff_walk !)
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
    .default = "no difficulty" # there are cases of "NON REPONSE"  for some disability variables, but this case_when() should have appropriate logic (29/11 for info I didn't check for countries other than benin21)
    )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#benin21 <- benin21 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#benin21 <- benin21 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_see %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT VOIR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#benin21 <- benin21 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#benin21 <- benin21 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_com %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#benin21 <- benin21 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#benin21 <- benin21 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#benin21 <- benin21 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 


# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#benin21 <- benin21 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.B) centraf18 ----

# Assigning descriptive names and labels to variables
centraf18 <- centraf18 |> mutate(
  country= "Central African Republic", # to be able to merge all the surveys together
  year= 2018, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#centraf18 <- centraf18 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#centraf18 <- centraf18 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#centraf18 <- centraf18 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_com == "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" | diff_wash == "NE PEUT PAS DU TOUT PRENDRE SOIN D?ELLE-MEME" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#centraf18 <- centraf18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#centraf18 <- centraf18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_see %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT VOIR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#centraf18 <- centraf18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#centraf18 <- centraf18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_com %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#centraf18 <- centraf18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#centraf18 <- centraf18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#centraf18 <- centraf18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT PRENDRE SOIN D?ELLE-MEME") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#centraf18 <- centraf18 |> 
  mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.C) chad19 ----

# Assigning descriptive names and labels to variables
chad19 <- chad19 |> mutate(
  country= "Chad", # to be able to merge all the surveys together
  year= 2019, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#chad19 <- chad19 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#chad19 <- chad19 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#chad19 <- chad19 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_com == "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" | diff_wash == "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME" ~ "cannot do at all", 
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#chad19 <- chad19 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#chad19 <- chad19 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_see %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT VOIR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#chad19 <- chad19 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#chad19 <- chad19 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_com %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#chad19 <- chad19 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#chad19 <- chad19 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#chad19 <- chad19 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#chad19 <- chad19 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.D) comor22 ----

# Assigning descriptive names and labels to variables
comor22 <- comor22 |> mutate(
  country= "Comoros", # to be able to merge all the surveys together
  year= 2022, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#comor22 <- comor22 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#comor22 <- comor22 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#comor22 <- comor22 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_com == "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIER" | diff_wash == "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#comor22 <- comor22 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#comor22 <- comor22 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_see %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT VOIR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#comor22 <- comor22 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#comor22 <- comor22 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_com %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#comor22 <- comor22 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#comor22 <- comor22 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#comor22 <- comor22 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#comor22 <- comor22 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.E) drcong17 ----

# Assigning descriptive names and labels to variables
drcong17 <- drcong17 |> mutate(
  country= "Democratic Republic of the Congo", # to be able to merge all the surveys together
  year= 2017, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#drcong17 <- drcong17 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#drcong17 <- drcong17 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#drcong17 <- drcong17 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_com == "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" | diff_wash == "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#drcong17 <- drcong17 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#drcong17 <- drcong17 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_see %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT VOIR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#drcong17 <- drcong17 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#drcong17 <- drcong17 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_com %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT COMPRENDRE OU SE FAIRE COMPRENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#drcong17 <- drcong17 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#drcong17 <- drcong17 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#drcong17 <- drcong17 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("QUELQUES DIFFICULTES", "PAS DE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT  PRENDRE SOIN D?ELLE-MEME") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#drcong17 <- drcong17 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.F) eswat21 ----

# Assigning descriptive names and labels to variables
eswat21 <- eswat21 |> mutate(
  country= "Eswatini", # to be able to merge all the surveys together
  year= 2021, # to be able to merge all the surveys together
  wt = wmweight,
  psu = psu,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#eswat21 <- eswat21 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#eswat21 <- eswat21 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |> # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#eswat21 <- eswat21 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR AT ALL" | diff_remem == "CANNOT REMEMBER/CONCENTRATE AT ALL" | diff_walk == "CANNOT WALK AT ALL" | diff_wash == "CANNOT CARE FOR SELF AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#eswat21 <- eswat21 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#eswat21 <- eswat21 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#eswat21 <- eswat21 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#eswat21 <- eswat21 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#eswat21 <- eswat21 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMEMBER/CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#eswat21 <- eswat21 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT WALK AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#eswat21 <- eswat21 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#eswat21 <- eswat21 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.G) gamb18 ----

# Assigning descriptive names and labels to variables
gamb18 <- gamb18 |> mutate(
  country= "Gambia", # to be able to merge all the surveys together
  year= 2018, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 


# Creation of variable gender (based on the source file since men and women are found in two different files)
#gamb18 <- gamb18 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#gamb18 <- gamb18 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#gamb18 <- gamb18 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR  AT ALL" | diff_remem == "CANNOT REMEMBER/ CONCENTRATE AT ALL" | diff_walk == "CANNOT  WALK/ CLIMB STEPS AT ALL" | diff_wash == "CANNOT CARE FOR SELF AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#gamb18 <- gamb18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#gamb18 <- gamb18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#gamb18 <- gamb18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR  AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#gamb18 <- gamb18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#gamb18 <- gamb18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMEMBER/ CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#gamb18 <- gamb18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT  WALK/ CLIMB STEPS AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#gamb18 <- gamb18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#gamb18 <- gamb18 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.H) ghana17 ----

# Assigning descriptive names and labels to variables
ghana17 <- ghana17 |> mutate(
  country= "Ghana", # to be able to merge all the surveys together
  year= 2017, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  stratum = as.numeric(stratum), # This conversion of stratum (and so strata just below too) to numeric is useful when we do the bind_row() of all the mics datasets, because for other countries stratum and strata are categorical (so all mics countries except ghana17, guibi18 and malawi19)
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#ghana17 <- ghana17 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#ghana17 <- ghana17 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#ghana17 <- ghana17 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR AT ALL" | diff_remem == "CANNOT REMMEBER/CONCENTRATE AT ALL" | diff_walk == "CANNOT WALK/CLIMB STEPS  AT ALL" | diff_wash == "CANNOT CARE FOR SELF  AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#ghana17 <- ghana17 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#ghana17 <- ghana17 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#ghana17 <- ghana17 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#ghana17 <- ghana17 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#ghana17 <- ghana17 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMMEBER/CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#ghana17 <- ghana17 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT WALK/CLIMB STEPS  AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#ghana17 <- ghana17 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF  AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#ghana17 <- ghana17 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.I) guibi18  ----

# Assigning descriptive names and labels to variables
guibi18 <- guibi18 |> mutate(
  country= "Guinea-Bissau", # to be able to merge all the surveys together
  year= 2018, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  stratum = as.numeric(stratum), # This conversion of stratum (and so strata just below too) to numeric is useful when we do the bind_row() of all the mics datasets, because for other countries stratum and strata are categorical (so all mics countries except ghana17, guibi18 and malawi19)
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#guibi18 <- guibi18 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#guibi18 <- guibi18 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#guibi18 <- guibi18 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "Não consigo ver nada" | diff_hear == "Não consigo ver nada" | diff_remem == "Não consigo ver nada" | diff_walk == "Não consigo ver nada" | diff_wash == "Não consigo ver nada" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "Muitas dificuldades") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "Algumas dificuldades") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#guibi18 <- guibi18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#guibi18 <- guibi18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("Algumas dificuldades", "Nenhuma dificuldade") ~ "no", # No "non reponse level" (but we haven't checked the number of NA for this variable after filtering out the 15-17)
    diff_see %in% c("Muitas dificuldades", "Não consigo ver nada") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#guibi18 <- guibi18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("Algumas dificuldades", "Nenhuma dificuldade") ~ "no", # No "non reponse level" (but we haven't checked the number of NA for this variable after filtering out the 15-17)
    diff_hear %in% c("Muitas dificuldades", "Não consigo ver nada") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#guibi18 <- guibi18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("Algumas dificuldades", "Nenhuma dificuldade") ~ "no", # No "non reponse level" (but we haven't checked the number of NA for this variable after filtering out the 15-17)
    diff_com %in% c("Muitas dificuldades") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#guibi18 <- guibi18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("Algumas dificuldades", "Nenhuma dificuldade") ~ "no", # No "non reponse level" (but we haven't checked the number of NA for this variable after filtering out the 15-17)
    diff_remem %in% c("Muitas dificuldades", "Não consigo ver nada") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#guibi18 <- guibi18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("Algumas dificuldades", "Nenhuma dificuldade") ~ "no", # No "non reponse level" (but we haven't checked the number of NA for this variable after filtering out the 15-17)
    diff_walk %in% c("Muitas dificuldades", "Não consigo ver nada") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#guibi18 <- guibi18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("Algumas dificuldades", "Nenhuma dificuldade") ~ "no", # No "non reponse level" (but we haven't checked the number of NA for this variable after filtering out the 15-17)
    diff_wash %in% c("Muitas dificuldades", "Não consigo ver nada") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#guibi18 <- guibi18 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.J) lesot18 ----

# Assigning descriptive names and labels to variables
lesot18 <- lesot18 |> mutate(
  country= "Lesotho", # to be able to merge all the surveys together
  year= 2018, # to be able to merge all the surveys together
  wt = wmweight,
  psu = psu,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  district = HH7A, # specific to lesot18
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#lesot18 <- lesot18 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#lesot18 <- lesot18 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#lesot18 <- lesot18 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR AT ALL" | diff_remem == "CANNOT REMEMBER/ CONCENTRATE AT ALL" | diff_walk == "CANNOT WALK/ CLIMB STEPS AT ALL" | diff_wash == "CANNOT CARE FOR SELF AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#lesot18 <- lesot18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#lesot18 <- lesot18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#lesot18 <- lesot18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#lesot18 <- lesot18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#lesot18 <- lesot18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMEMBER/ CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#lesot18 <- lesot18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT WALK/ CLIMB STEPS AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#lesot18 <- lesot18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#lesot18 <- lesot18 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.K) madag18 ----

# Assigning descriptive names and labels to variables
madag18 <- madag18 |> mutate(
  country= "Madagascar", # to be able to merge all the surveys together
  year= 2018, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#madag18 <- madag18 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#madag18 <- madag18 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |> # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#madag18 <- madag18 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS" | diff_wash == "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#madag18 <- madag18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#madag18 <- madag18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_see %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT VOIR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#madag18 <- madag18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#madag18 <- madag18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_com %in% c("BEAUCOUP DE DIFFICULTES") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#madag18 <- madag18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR OU SE CONCENTRER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#madag18 <- madag18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER OU MONTER DES ESCALIERS") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#madag18 <- madag18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT  PRENDRE SOIN D’ELLE-MEME") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#madag18 <- madag18 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.L) malawi19 ----

# Assigning descriptive names and labels to variables
malawi19 <- malawi19 |> mutate(
  country= "Malawi", # to be able to merge all the surveys together
  year= 2019, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  stratum = as.numeric(stratum), # This conversion of stratum (and so strata just below too) to numeric is useful when we do the bind_row() of all the mics datasets, because for other countries stratum and strata are categorical (so all mics countries except ghana17, guibi18 and malawi19)
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#malawi19 <- malawi19 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#malawi19 <- malawi19 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#malawi19 <- malawi19 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR AT ALL" | diff_remem == "CANNOT REMEMBER/CONCENTRATE AT ALL" | diff_walk == "CANNOT WALK/CLIMB STEPS AT ALL" | diff_wash == "CANNOT CARE FOR SELF AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#malawi19 <- malawi19 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#malawi19 <- malawi19 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#malawi19 <- malawi19 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#malawi19 <- malawi19 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#malawi19 <- malawi19 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMEMBER/CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#malawi19 <- malawi19 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT WALK/CLIMB STEPS AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#malawi19 <- malawi19 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#malawi19 <- malawi19 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.M) saoto19 ----

# Assigning descriptive names and labels to variables
saoto19 <- saoto19 |> mutate(
  country= "São Tomé and Príncipe", # to be able to merge all the surveys together
  year= 2019, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#saoto19 <- saoto19 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#saoto19 <- saoto19 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |> # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#saoto19 <- saoto19 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NÃO CONSEGUE VER NADA" | diff_hear == "NÃO CONSEGUE OUVIR NADA" | diff_remem == "NÃO CONSEGUE LEMBRAR OU CONCENTRAR" | diff_walk == "NÃO CONSEGUE ANDAR OU SUBIR ESCADAS" | diff_wash == "NÃO CONSEGUE CUIDAR DE SI MESMO" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "MUITAS DIFICULDADES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "ALGUMAS DIFICULDADES") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#saoto19 <- saoto19 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#saoto19 <- saoto19 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("ALGUMAS DIFICULDADES", "NENHUMA DIFICULDADE", "SEM RESPOSTA") ~ "no",
    diff_see %in% c("MUITAS DIFICULDADES", "NÃO CONSEGUE VER NADA") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#saoto19 <- saoto19 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("ALGUMAS DIFICULDADES", "NENHUMA DIFICULDADE", "SEM RESPOSTA") ~ "no",
    diff_hear %in% c("MUITAS DIFICULDADES", "NÃO CONSEGUE OUVIR NADA") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#saoto19 <- saoto19 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("ALGUMAS DIFICULDADES", "NENHUMA DIFICULDADE", "SEM RESPOSTA") ~ "no",
    diff_com %in% c("MUITAS DIFICULDADES") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#saoto19 <- saoto19 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("ALGUMAS DIFICULDADES", "NENHUMA DIFICULDADE", "SEM RESPOSTA") ~ "no",
    diff_remem %in% c("MUITAS DIFICULDADES", "NÃO CONSEGUE LEMBRAR OU CONCENTRAR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#saoto19 <- saoto19 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("ALGUMAS DIFICULDADES", "NENHUMA DIFICULDADE", "SEM RESPOSTA") ~ "no",
    diff_walk %in% c("MUITAS DIFICULDADES", "NÃO CONSEGUE ANDAR OU SUBIR ESCADAS") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#saoto19 <- saoto19 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("ALGUMAS DIFICULDADES", "NENHUMA DIFICULDADE", "SEM RESPOSTA") ~ "no",
    diff_wash %in% c("MUITAS DIFICULDADES", "NÃO CONSEGUE CUIDAR DE SI MESMO") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#saoto19 <- saoto19 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.N) sierral17 ----

# Assigning descriptive names and labels to variables
sierral17 <- sierral17 |> mutate(
  country= "Sierra Leone", # to be able to merge all the surveys together
  year= 2017, # to be able to merge all the surveys together
  wt = wmweight,
  # psu = PSU, # No PSU found in sierral17
  strata = stratum, # stratum taken from the hh file for sierral17
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  district = HH7A, # specific to sierral17
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#sierral17 <- sierral17 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#sierral17 <- sierral17 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#sierral17  <- sierral17  %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR AT ALL" | diff_remem == "CANNOT REMEMBER/CONCENTRATE AT ALL" | diff_walk == "CANNOT WALK/CLIMB STEPS AT ALL" | diff_wash == "CANNOT CARE FOR SELF AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#sierral17 <- sierral17  %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#sierral17  <- sierral17  %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#sierral17 <- sierral17  %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#sierral17 <- sierral17  |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#sierral17 <- sierral17  %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMEMBER/CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#sierral17 <- sierral17  %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT WALK/CLIMB STEPS AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#sierral17 <- sierral17 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#sierral17 <- sierral17 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.O) togo17 ----

# Assigning descriptive names and labels to variables
togo17 <- togo17 |> mutate(
  country= "Togo", # to be able to merge all the surveys together
  year= 2017, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# We don't include in the recoding the level "INCOHERENT" (for the disability variables) because in all disability variables, no individual has this value as an answer

# Creation of variable gender (based on the source file since men and women are found in two different files)
#togo17 <- togo17 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#togo17 <- togo17 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |> # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#togo17 <- togo17 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "NE PEUT PAS DU TOUT VOIR" | diff_hear == "NE PEUT PAS DU TOUT ENTENDRE" | diff_remem == "NE PEUT PAS DU TOUT SE SOUVENIR/CONCENTRER" | diff_walk == "NE PEUT PAS DU TOUT MARCHER/MONTER DES ESCALIERS" | diff_wash == "NE PEUT PAS DU TOUT PRENDRE SOIN D'ELLE-MEME, SE LAVER, S'HABILLER" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "BEAUCOUP DE DIFFICULTES") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "QUELQUES DIFFICULTES") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#togo17 <- togo17 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#togo17 <- togo17 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_see %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT VOIR") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#togo17 <- togo17 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_hear %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT ENTENDRE") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#togo17 <- togo17 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_com %in% c("BEAUCOUP DE DIFFICULTES") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#togo17 <- togo17 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_remem %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT SE SOUVENIR/CONCENTRER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#togo17 <- togo17 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NO RESPONSE/NON REPONSE") ~ "no",
    diff_walk %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT MARCHER/MONTER DES ESCALIERS") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#togo17 <- togo17 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("QUELQUES DIFFICULTES", "AUCUNE DIFFICULTE", "NON REPONSE") ~ "no",
    diff_wash %in% c("BEAUCOUP DE DIFFICULTES", "NE PEUT PAS DU TOUT PRENDRE SOIN D'ELLE-MEME, SE LAVER, S'HABILLER") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#togo17 <- togo17 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.P) yemen22 ----

# Assigning descriptive names and labels to variables
yemen22 <- yemen22 |> mutate(
  country= "Yemen", # to be able to merge all the surveys together
  year= 2022, # to be able to merge all the surveys together
  wt = wmweight,
  psu = PSU,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#yemen22 <- yemen22 |> 
  mutate(gender = "male") |>  # In yemen22, all individuals are men

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#yemen22 <- yemen22 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#yemen22 <- yemen22 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR AT ALL" | diff_remem == "CANNOT REMEMBER/CONCENTRATE AT ALL" | diff_walk == "CANNOT WALK/CLIMB STEPS AT ALL" | diff_wash == "CANNOT CARE FOR SELF AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#yemen22 <- yemen22 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#yemen22 <- yemen22 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#yemen22 <- yemen22 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#yemen22 <- yemen22 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#yemen22 <- yemen22 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMEMBER/CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#yemen22 <- yemen22 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT WALK/CLIMB STEPS AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#yemen22 <- yemen22 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#yemen22 <- yemen22 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


## 1.Q) zimbab19 ----

# Assigning descriptive names and labels to variables
zimbab19 <- zimbab19 |> mutate(
  country= "Zimbabwe", # to be able to merge all the surveys together
  year= 2019, # to be able to merge all the surveys together
  wt = wmweight,
  psu = psu,
  strata = stratum,
  cluster_number = HH1,
  household_number = HH2,
  household_member_line_number = LN,
  age = WB4,
  uses_glasses = AF2,
  uses_hearing_aid = AF3,
  diff_see = AF6,
  diff_hear = AF8,
  diff_com = AF12,
  diff_remem = AF10,
  diff_walk = AF9,
  diff_wash = AF11,
  type_place_residence = HH6,
  region = HH7,
  age_groups = WAGE, # not the same groups as in DHS
  education_level_ever = welevel, # not the same levels as in DHS
  disability_binary = disability, # at least when a lot of difficulty
  wealth_quintile = windex5
) |> 

# Creation of variable gender (based on the source file since men and women are found in two different files)
#zimbab19 <- zimbab19 |> 
  mutate(gender = case_when(file_source == "mn" ~ "male", file_source == "wm" ~ "female", .default = NA_character_)) |> 

# Transformation of the values "NSP", "DK / Missing" etc to NA in the "education_level_ever" variable
#zimbab19 <- zimbab19 |> 
  mutate(education_level_ever = case_when(education_level_ever %in% c("NSP", "Manquant/NSP", "ND/NS", "DK / Missing", "Não sabe/Em falta", "Missing/DK") ~ NA_character_, .default = education_level_ever)) |>  # NA_character_ seems more appropriate than NA, but not sure (see https://stackoverflow.com/questions/37400665/r-what-is-na-character)

# Creation of variable Highest difficulty in any domain
#zimbab19 <- zimbab19 %>%
  mutate(highest_diff_any_domain = case_when(
    diff_see == "CANNOT SEE AT ALL" | diff_hear == "CANNOT HEAR AT ALL" | diff_remem == "CANNOT REMEMBER/CONCENTRATE AT ALL" | diff_walk == "CANNOT WALK/CLIMB STEPS AT ALL" | diff_wash == "CANNOT CARE FOR SELF AT ALL" ~ "cannot do at all",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "A LOT OF DIFFICULTY") ~ "a lot of difficulty",
    if_any(c(diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash), ~ . == "SOME DIFFICULTY") ~ "some difficulty",
    .default = "no difficulty"
  )) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#zimbab19 <- zimbab19 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#zimbab19 <- zimbab19 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_see %in% c("A LOT OF DIFFICULTY", "CANNOT SEE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#zimbab19 <- zimbab19 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_hear %in% c("A LOT OF DIFFICULTY", "CANNOT HEAR AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#zimbab19 <- zimbab19 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_com %in% c("A LOT OF DIFFICULTY") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#zimbab19 <- zimbab19 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_remem %in% c("A LOT OF DIFFICULTY", "CANNOT REMEMBER/CONCENTRATE AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#zimbab19 <- zimbab19 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_walk %in% c("A LOT OF DIFFICULTY", "CANNOT WALK/CLIMB STEPS AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#zimbab19 <- zimbab19 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("SOME DIFFICULTY", "NO DIFFICULTY", "NO RESPONSE") ~ "no",
    diff_wash %in% c("A LOT OF DIFFICULTY", "CANNOT CARE FOR SELF AT ALL") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 

# Transformation of the values "NON REPONSE", "NO RESPONSE", "SEM RESPOSTA" and "NO RESPONSE/NON REPONSE" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the no response answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like one the options in the function ; Note that it was answered by very few people and also that we do this transformation of these no response answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#zimbab19 <- zimbab19 |> 
mutate(across(starts_with("diff_"), ~ case_when(.x %in% c("NON REPONSE", "NO RESPONSE", "SEM RESPOSTA","NO RESPONSE/NON REPONSE") ~ NA_character_, .default = .x)))


# 2) Loading the basemaps ---- 

benin21_basemap0 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
benin21_basemap1 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
#benin21_basemap2 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_2") # Not possible to use this level because we don't have a variable for this level in the datasets
#benin21_basemap3 <- st_read("data/basemaps/gadm41_BEN.gpkg", layer = "ADM_ADM_3") # Not possible to use this level because we don't have a variable for this level in the datasets

centraf18_basemap0 <- st_read("data/basemaps/gadm41_CAF.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
centraf18_basemap1 <- st_read("data/basemaps/gadm41_CAF.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom) # !We don't have the info in the datasets at this level of 17 units, and we don't have a map at a level of 7 units like in the survey!
# centraf18_basemap2 <- st_read("data/basemaps/gadm41_CAF.gpkg", layer = "ADM_ADM_2") # Not possible to use this level because we don't have a variable for this level in the datasets
# centraf18_basemap3 <- st_read("data/basemaps/gadm41_CAF.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

chad19_basemap0 <- st_read("data/basemaps/gadm41_TCD.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
chad19_basemap1 <- st_read("data/basemaps/gadm41_TCD.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
# chad19_basemap2 <- st_read("data/basemaps/gadm41_TCD.gpkg", layer = "ADM_ADM_2") # Not possible to use this level because we don't have a variable for this level in the datasets
# chad19_basemap3 <- st_read("data/basemaps/gadm41_TCD.gpkg", layer = "ADM_ADM_3") # Not possible to use this level because we don't have a variable for this level in the datasets

comor22_basemap0 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
comor22_basemap1 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
comor22_basemap2 <- st_read("data/mics/mics_gps/ComorosMICS2022GPS/mics_boundaries_nr shp/mics_boundaries_nr.shp") # |> filter(!is.na(LEVELCO)) # [Not for now] We filter out the prefecture "Cratère du Karthala" that has an invalid geom according to st_is_valid(comor22_basemap2) # Not: comor22_basemap2 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_2") # Nonexistent layer in the file
# comor22_basemap3 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

drcong17_basemap0 <- st_read("data/basemaps/gadm41_COD.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
drcong17_basemap1 <- st_read("data/basemaps/gadm41_COD.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
# drcong17_basemap2 <- st_read("data/basemaps/gadm41_COD.gpkg", layer = "ADM_ADM_2") # Not possible to use this level because we don't have a variable for this level in the datasets
# drcong17_basemap3 <- st_read("data/basemaps/gadm41_COD.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

eswat21_basemap0 <- st_read("data/basemaps/gadm41_SWZ.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
eswat21_basemap1 <- st_read("data/basemaps/gadm41_SWZ.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
eswat21_basemap2 <- st_read("data/mics/mics_gps/EswatiniMICS2021-22GPS/mics_boundaries_nr shp/mics_boundaries_nr.shp") # Not: eswat21_basemap2 <- st_read("data/basemaps/gadm41_SWZ.gpkg", layer = "ADM_ADM_2") (because shapefile directly from the eswat21 gps folder, but both gadm and mics boundary shapefile have 55 units)
# eswat21_basemap3 <- st_read("data/basemaps/gadm41_SWZ.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

gamb18_basemap0 <- st_read("data/basemaps/gadm41_GMB.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
gamb18_basemap1 <- st_read("data/basemaps/The Gambia 2019 DHS - sdr_subnational_boundaries_2024-12-04/shps/sdr_subnational_boundaries.shp") # Not: gamb18_basemap1 <- st_read("data/basemaps/gadm41_GMB.gpkg", layer = "ADM_ADM_1") because this gadm layer has 6 units instead of 8 like in gamb18 mics (so to get the shapefile with 8 units we take the one from DHS)
gamb18_basemap2 <- st_read("data/mics/mics_gps/GambiaMICS2018GPS/mics_boundaries_nr shp/mics_boundaries_nr.shp") # Not: gamb18_basemap2 <- st_read("data/basemaps/gadm41_GMB.gpkg", layer = "ADM_ADM_2") because gadm has 37 units vs 48 for the shapefile from the gamb18 gps folder
# gamb18_basemap3 <- st_read("data/basemaps/gadm41_GMB.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

ghana17_basemap0 <- st_read("data/basemaps/gadm41_GHA.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
ghana17_basemap1 <- st_read("data/basemaps/Ghana 2014 DHS - sdr_subnational_boundaries_2024-12-04/shps/sdr_subnational_boundaries.shp") # Not:ghana17_basemap1 <- st_read("data/basemaps/gadm41_GHA.gpkg", layer = "ADM_ADM_1") because this gadm layer has 16 units instead of 10 like in ghana17 mics (so to get the shapefile with 10 units we take the one from DHS2014)
ghana17_basemap2 <- st_read("data/basemaps/gadm41_GHA.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom)# Not possible to use this level because we don't have a variable for this level in the datasets
# ghana17_basemap3 <- st_read("data/basemaps/gadm41_GHA.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

guibi18_basemap0 <- st_read("data/basemaps/gadm41_GNB.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
guibi18_basemap1 <- st_read("data/basemaps/gadm41_GNB.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
# guibi18_basemap2 <- st_read("data/basemaps/gadm41_GNB.gpkg", layer = "ADM_ADM_2") # Not possible to use this level because we don't have a variable for this level in the datasets
# guibi18_basemap3 <- st_read("data/basemaps/gadm41_GNB.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

lesot18_basemap0 <- st_read("data/basemaps/gadm41_LSO.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
lesot18_basemap1 <- st_read("data/basemaps/Lesotho 2014 DHS - sdr_subnational_boundaries_2024-12-05/shps/sdr_subnational_boundaries2.shp") # Not ADM_ADM_1 because we found a shapefile with 4 ecozones in DHS2014 and not in GADM
lesot18_basemap2 <- st_read("data/basemaps/gadm41_LSO.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
# lesot18_basemap2 <- st_read("data/basemaps/gadm41_LSO.gpkg", layer = "ADM_ADM_2") # Nonexistent layer in the file
# lesot18_basemap3 <- st_read("data/basemaps/gadm41_LSO.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

madag18_basemap0 <- st_read("data/basemaps/gadm41_MDG.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
# madag18_basemap1 <- st_read("data/basemaps/gadm41_MDG.gpkg", layer = "ADM_ADM_1") # Not possible to use this level because we don't have a variable for this level in the datasets (the 6 ex-provinces ; even if the region variable with 24 units is available) ; source on the ex-provinces: https://en.wikipedia.org/wiki/Provinces_of_Madagascar
madag18_basemap1 <- st_read("data/basemaps/gadm41_MDG.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom)
madag18_basemap2 <- st_read("data/mics/mics_gps/MadagascarMICS2018GPS/mics_boundaries_nr shp/mics_boundaries_nr.shp") # Not: madag18_basemap2 <- st_read("data/basemaps/gadm41_MDG.gpkg", layer = "ADM_ADM_3") because gadm has 110 units vs 114 for the shapefile from the madag18 gps folder

malawi19_basemap0 <- st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
malawi19_basemap1 <- st_read("data/basemaps/Malawi 2015 DHS - sdr_subnational_boundaries_2024-10-17/shps/sdr_subnational_boundaries.shp") # Not: st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_1") because this "ADM_ADM_1" layer is one level below with 28 units (so to get the shapefile with 3 units we take the one from DHS)
malawi19_basemap2 <- st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom) # It will be necessary to group together the 32 districts in the malawi19 hh file into 28 districts (by grouping together the units that are divided rural/urban)
malawi19_basemap3 <- st_read("data/mics/mics_gps/MalawiMICS2019-20GPS/mics_boundaries_nr shp/mics_boundaries_nr.shp") # malawi19_basemap3 |> group_by(GEONAMET) |> filter(n() > 1) |> ungroup() |> distinct(GEONAMET) # To check if there is no duplicated district name ; # Not: malawi19_basemap3 <- st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_2") because gadm has 256 units vs 436 for the shapefile from the malawi18 gps folder
malawi19_basemap3 <- malawi19_basemap3 |> group_by(GEONAMET) |> mutate(GEONAMET = case_when(n() > 1 ~ paste0(GEONAMET, " ", row_number()), TRUE ~ GEONAMET)) |> ungroup() # If there are multiple rows (duplicates) in the group (n() > 1), append a suffix (" 1"," 2", etc.) to GEONAMET using row_number()

saoto19_basemap0 <- st_read("data/basemaps/gadm41_STP.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
saoto19_basemap1 <- st_read("data/basemaps/gadm41_STP.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)# It will be necessary to group together all the HH7 Região together and have only two units (the island of Sao Tome and the unit "REGIÃO AUTÓNOMA DO PRÍNCIPE")
saoto19_basemap2 <- st_read("data/basemaps/gadm41_STP.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom)# We don't have the info in the datasets at this level of 7 units, and we don't have a map at a level of 5 units like in the survey: when creating the prevalence table, we considered that the values of respondants from "REGIÃO NORTE OESTE" in the survey go to both "Lembá" and "Lobata" in a duplicated way, and that the values of respondants from "REGIÃO SUL ESTE" go to both "Cantagalo" and "Caué" in a duplicated way
# saoto19_basemap3 <- st_read("data/basemaps/gadm41_STP.gpkg", layer = "ADM_ADM_3") # # Nonexistent layer in the file

sierral17_basemap0 <- st_read("data/basemaps/gadm41_SLE.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
sierral17_basemap1 <- st_read("data/basemaps/gadm41_SLE.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
sierral17_basemap2 <- st_read("data/basemaps/gadm41_SLE.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom)
sierral17_basemap3 <- st_read("data/mics/mics_gps/SierraLeoneMICS2017GPS/mics_boundaries_nr shp/mics_boundaries_nr.shp") #Not: sierral17_basemap3 <- st_read("data/basemaps/gadm41_SLE.gpkg", layer = "ADM_ADM_3") because gadm has 153 units vs 166 for the shapefile from the sierral17 gps folder ; # sierral17_basemap3 |> group_by(GEONAMET) |> filter(n() > 1) |> ungroup() |> distinct(GEONAMET) # To check if there is no duplicated district name # To check if there is no duplicated district name
sierral17_basemap3 <- sierral17_basemap3 |> group_by(GEONAMET) |> mutate(GEONAMET = case_when(n() > 1 ~ paste0(GEONAMET, " ", row_number()), TRUE ~ GEONAMET)) |> ungroup() # If there are multiple rows (duplicates) in the group (n() > 1), append a suffix (" 1"," 2", etc.) to GEONAMET using row_number() # We also used this method when processing DHS surveys

togo17_basemap0 <- st_read("data/basemaps/gadm41_TGO.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
togo17_basemap1 <- st_read("data/basemaps/gadm41_TGO.gpkg", layer = "ADM_ADM_1")|> rename(geometry = geom)# It will be necessary to put the units "Lomé Commune" and "Golfe Urbain" from the HH7 variable into "Maritime" to go from 7 to 5 units (logic of the grouping based on this thttps://fr.wikipedia.org/wiki/Subdivisions_du_Togo)
togo17_basemap2 <- st_read("data/basemaps/gadm41_TGO.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # It will be necessary to go from 42 units in the "pref" variable to 40 units like in this basemap by grouping a few units
# togo17_basemap3 <- st_read("data/basemaps/gadm41_TGO.gpkg", layer = "ADM_ADM_3") # Not possible to use this level because we don't have a variable for this level in the datasets

yemen22_basemap0 <- st_read("data/basemaps/gadm41_YEM.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
yemen22_basemap1 <- st_read("data/basemaps/gadm41_YEM.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom) # It will be necessary to go from 22 units in the HH7 variable to 21 units like in this basemap by grouping units (one or two I think)
# yemen22_basemap2 <- st_read("data/basemaps/gadm41_YEM.gpkg", layer = "ADM_ADM_2") # Not possible to use this level because we don't have a variable for this level in the datasets
# yemen22_basemap3 <- st_read("data/basemaps/gadm41_YEM.gpkg", layer = "ADM_ADM_3") # Nonexistent layer in the file

zimbab19_basemap0 <- st_read("data/basemaps/gadm41_ZWE.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
zimbab19_basemap1 <- st_read("data/basemaps/gadm41_ZWE.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
zimbab19_basemap2 <- st_read("data/mics/mics_gps/ZimbabweMICS2019GPS/mics_boundaries_nr shp/mics_boundaries_nr.shp") # Not: zimbab19_basemap2 <- st_read("data/basemaps/gadm41_ZWE.gpkg", layer = "ADM_ADM_2") (because shapefile directly from the zimbab19 gps folder, but both gadm and mics boundary shapefile have 91 units)
# zimbab19_basemap3 <- st_read("data/basemaps/gadm41_ZWE.gpkg", layer = "ADM_ADM_3") # Not possible to use this level because we don't have a variable for this level in the datasets


# 3) Joining GPS data for each survey, first each cluster data (in the gps dataset) to the admin2 unit it belongs to, then to the survey dataset ----
# The MICS surveys below are the only ones where GPS data is available

## 3.1) Loading of gps datasets for the availabel countries (Not: Joining of GPS datasets to smallest possible admin) ----

# comor22
comor22gps <- st_read("data/mics/mics_gps/ComorosMICS2022GPS/ComorosMICS2022GPS.shp")

# eswat21
eswat21gps <- st_read("data/mics/mics_gps/EswatiniMICS2021-22GPS/EswatiniMICS2021-22GPS.shp")

# gamb18
gamb18gps <- st_read("data/mics/mics_gps/GambiaMICS2018GPS/GambiaMICS2018GPS.shp")

# madag18
madag18gps <- st_read("data/mics/mics_gps/MadagascarMICS2018GPS/MadagascarMICS2018GPS.shp")

# malawi19
malawi19gps <- st_read("data/mics/mics_gps/MalawiMICS2019-20GPS/MalawiMICS2019-20GPS.shp")

# sierral17
sierral17gps <- st_read("data/mics/mics_gps/SierraLeoneMICS2017GPS/SierraLeoneMICS2017GPS.shp")

# zimbab19
zimbab19gps <- st_read("data/mics/mics_gps/ZimbabweMICS2019GPS/ZimbabweMICS2019GPS.shp")

## 3.2) Merging the gps datasets with the mics survey datasets ----

comor22 <- left_join(comor22, comor22gps, by = join_by(cluster_number == HH1)) # Maybe relevant: |> st_as_sf()
anti_join(comor22, comor22gps, by = join_by(cluster_number == HH1)) # This should have 0 rows if all rows are matched in the left_join

eswat21 <- left_join(eswat21, eswat21gps, by = join_by(cluster_number == HH1)) # Maybe relevant: |> st_as_sf()
anti_join(eswat21, eswat21gps, by = join_by(cluster_number == HH1)) # This should have 0 rows if all rows are matched in the left_join

gamb18 <- left_join(gamb18, gamb18gps, by = join_by(cluster_number == HH1)) # Maybe relevant: |> st_as_sf()
anti_join(gamb18, gamb18gps, by = join_by(cluster_number == HH1)) # This should have 0 rows if all rows are matched in the left_join

madag18 <- left_join(madag18, madag18gps, by = join_by(cluster_number == HH1)) # Maybe relevant: |> st_as_sf()
anti_join(madag18, madag18gps, by = join_by(cluster_number == HH1)) # This should have 0 rows if all rows are matched in the left_join

malawi19 <- left_join(malawi19, malawi19gps, by = join_by(cluster_number == HH1)) # Maybe relevant: |> st_as_sf()
anti_join(malawi19, malawi19gps, by = join_by(cluster_number == HH1)) # This should have 0 rows if all rows are matched in the left_join

sierral17 <- left_join(sierral17, sierral17gps, by = join_by(cluster_number == HH1)) # Maybe relevant: |> st_as_sf()
anti_join(sierral17, sierral17gps, by = join_by(cluster_number == HH1)) # This should have 0 rows if all rows are matched in the left_join

zimbab19 <- left_join(zimbab19, zimbab19gps, by = join_by(cluster_number == HH1)) # Maybe relevant: |> st_as_sf()
anti_join(zimbab19, zimbab19gps, by = join_by(cluster_number == HH1)) # This should have 0 rows if all rows are matched in the left_join

## 3.3) Creation of homogenous admin variables ----
# This is the variable that goes into the group_by() when we create the prevalence tables
# "admin1" corresponds to the largest possible administrative subdivision
# "admin2" corresponds to the second largest possible administrative subdivision
# "admin3" corresponds to the third largest possible administrative subdivision

benin21 <- benin21 |> mutate(
  region_cleaned = str_to_title(region), # To capitalize first letter of each word
  region_cleaned = case_when(
    region_cleaned == "Atacora" ~ "Atakora",
    region_cleaned == "Couffo" ~ "Kouffo",
    region_cleaned == "Oueme" ~ "Ouémé",
    .default = region_cleaned
  ),
  admin1 = region_cleaned
)

centraf18 <- centraf18 |> mutate(
  region_explicit_prefect_names = case_when(
    # /!\ The names separated below by a "&" are separate prefectures (so they are different units in basemaps) but were grouped in the survey, so later when we create the prevalence tibble at this admin level, we duplicate each row that corresponds to multiple prefectures and put its value in the row created for the individualized prefecture
    region == "Région 1" ~ "Ombella-M'Poko & Lobaye",
    region == "Région 2" ~ "Mambéré-Kadéï & Sangha-Mbaéré & Nana-Mambéré",
    region == "Région 3" ~ "Ouham & Ouham-Pendé",
    region == "Région 4" ~ "Kémo & Nana-Grébizi & Ouaka",
    region == "Région 5" ~ "Bamingui-Bangoran & Haute-Kotto & Vakaga",
    region == "Région 6" ~ "Basse-Kotto & Mbomou & Haut-Mbomou",
    region == "Région 7" ~ "Bangui",
    .default = region
  ),
  admin1 = region_explicit_prefect_names
)

chad19 <- chad19 |> mutate(
  region_cleaned = case_when(
    region == "Barh El Gazal" ~ "Barh el Ghazel",
    region == "Chari Baguirmi" ~ "Chari-Baguirmi",
    region == "Guera" ~ "Guéra",
    region == "Hadjer Lamis" ~ "Hadjer-Lamis",
    region == "Mayo Kebbi Est" ~ "Mayo-Kebbi Est",
    region == "Mayo Kebbi Ouest" ~ "Mayo-Kebbi Ouest",
    region == "Moyen Chari" ~ "Moyen-Chari",
    region == "Tandjile" ~ "Tandjilé",
    # region == "" ~ "Tibesti",
    region == "Ndjamena" ~ "Ville de N'Djamena",
    .default = region
  ),
  admin1 = region_cleaned
)

comor22 <- comor22 |> mutate(
  region_cleaned = case_when(
    region == "Ngazidja" ~ "Njazídja",
    region == "Ndzuwani" ~ "Nzwani",
    .default = region
  ),
  admin1 = region_cleaned,
  admin2 = GEONAMET
)

drcong17 <- drcong17 |> mutate(
  region_cleaned = case_when(
    region == "Bas Uele" ~ "Bas-Uele",
    region == "Equateur" ~ "Équateur",
    region == "Haut Katanga" ~ "Haut-Katanga",
    region == "Haut Lomami" ~ "Haut-Lomami",
    region == "Haut Uele" ~ "Haut-Uele",
    region == "Kasai" ~ "Kasaï",
    region == "Kasai Central" ~ "Kasaï-Central",
    region == "Kasai Oriental" ~ "Kasaï-Oriental",
    region == "Kongo Central" ~ "Kongo-Central",
    region == "Maindombe" ~ "Mai-Ndombe",
    region == "Nord Kivu" ~ "Nord-Kivu",
    region == "Nord Ubangi" ~ "Nord-Ubangi",
    region == "Sud Kivu" ~ "Sud-Kivu",
    region == "Sud Ubangi" ~ "Sud-Ubangi",
    .default = region
  ),
  admin1 = region_cleaned
)

eswat21 <- eswat21 |> mutate(
  region_cleaned = case_when(
    region == "HHOHHO" ~ "Hhohho",
    region == "LUBOMBO" ~ "Lubombo",
    region == "MANZINI" ~ "Manzini",
    region == "SHISELWENI" ~ "Shiselweni",
    .default = region
  ),
  admin1 = region_cleaned,
  admin2 = GEONAMES
)

gamb18 <- gamb18 |> mutate(
  region_cleaned = str_to_title(region), # To capitalize only the first letter of each word
  admin1 = region_cleaned,
  admin2 = GEONAMES
)

ghana17 <- ghana17 |> mutate(
  region_cleaned = str_to_title(region), # To capitalize only the first letter of each word
  admin1 = region_cleaned
)

guibi18 <- guibi18 |> mutate(
  region_cleaned = case_when(
    region == "SAB" ~ "Bissau",
    region == "Bolama/Bijagós" ~ "Bolama",
    .default = region
  ),
  admin1 = region_cleaned
)

lesot18 <- lesot18 |> mutate(
  region_cleaned = str_to_lower(region),
  admin1 = region_cleaned,
  district_cleaned = str_to_title(district), # To capitalize only the first letter of each word
  district_cleaned = case_when(
    district_cleaned == "Botha-Bothe" ~ "Butha-Buthe",
    district_cleaned == "Mohales Hoek" ~ "Mohale's Hoek",
    district_cleaned == "Qachas Nek" ~ "Qacha's Nek",
    .default = district_cleaned
  ),
  admin2 = district_cleaned
)

madag18 <- madag18 |> mutate(
  region_cleaned = str_to_title(region), # To capitalize only the first letter of each word
  region_cleaned = case_when(
    region_cleaned == "Amoron'i Mania" ~ "Amoron'i mania",
    region_cleaned == "Atsimo Atsinanana" ~ "Atsimo-Atsinana",
    region_cleaned == "Haute Matsiatra" ~ "Haute matsiatra",
    region_cleaned == "Alaotra Mangoro" ~ "Alaotra-Mangoro",
    region_cleaned == "Atsimo Andrefana" ~ "Atsimo-Andrefana",
    .default = region_cleaned
  ),
  admin1 = region_cleaned,
  admin2 = GEONAMES
)

malawi19 <- malawi19 |> mutate(
  region_cleaned = case_when(
    region == "North" ~ "Northern region",
    region == "Central" ~ "Central region",
    region == "South" ~ "Southern region",
    .default = region
  ),
  admin1 = region_cleaned,
  DISTRICT_cleaned = case_when(
    DISTRICT == " Likoma" ~ "Likoma", # To correct the small mistake of adding a space before Likoma in the label
    DISTRICT == " Chikwawa" ~ "Chikwawa", # Idem
    DISTRICT == "Mzuzu City" ~ "Mzimba", # Mzuzu is a value in the DISTRICT variable from MICS but it is not an official district (so not separated on the malawi19_basemap2): it is located in the Mzimba district according to https://en.wikipedia.org/wiki/Mzimba_District
    DISTRICT == "Blantyre Rural" ~ "Blantyre", # Rural and City not separated in malawi18_basemap2
    DISTRICT == "Blantyre City" ~ "Blantyre", # Idem
    DISTRICT == "Lilongwe Rural" ~ "Lilongwe",
    DISTRICT == "Lilongwe City" ~ "Lilongwe",
    DISTRICT == "Zomba Rural" ~ "Zomba",
    DISTRICT == "Zomba City" ~ "Zomba",
    .default = DISTRICT
  ),
  admin2 = DISTRICT_cleaned,
  GEONAMET_cleaned = case_when(
    GEONAMET == "TA Dambe" & GEONAMES == "Mchinji" ~ "TA Dambe 1", # Names after the "~" found in malawi19_basemap3 (after the naming differenciation between similarily named GEONAMET)
    GEONAMET == "TA Dambe" & GEONAMES == "Neno" ~ "TA Dambe 2",
    GEONAMET == "Lake Malawi National Park" & GEONAMES == "Salima" ~ "Lake Malawi National Park 1",
    GEONAMET == "Lake Malawi National Park" & GEONAMES == "Mangochi" ~ "Lake Malawi National Park 2",
    GEONAMET == "Vwaza Marsh Reserve" & GEONAMES == "Mzimba" ~ "Vwaza Marsh Reserve 1",
    GEONAMET == "Vwaza Marsh Reserve" & GEONAMES == "Rumphi" ~ "Vwaza Marsh Reserve 2",
    GEONAMET == "TA Mkumbira" & GEONAMES == "Nkhata Bay" ~ "TA Mkumbira 1",
    GEONAMET == "TA Mkumbira" & GEONAMES == "Zomba Rural" ~ "TA Mkumbira 2",
    GEONAMET == "Liwonde Town" & GEONAMES == "Balaka" ~ "Liwonde Town 1",
    GEONAMET == "Liwonde Town" & GEONAMES == "Machinga" ~ "Liwonde Town 2", # Liwonde Town 1 and 2 are in fact one town, but the basemap separates them so we don't merge them when computing prevalence (we can't merge them in the map visually, but we could merge them when computing prevalence, to make the two units have the same prevalence value ; this is possible for the other GEONAMET units that are separated)
    GEONAMET == "TA Lundu" & GEONAMES == "Blantyre Rural" ~ "TA Lundu 1",
    GEONAMET == "TA Lundu" & GEONAMES == "Chikwawa" ~ "TA Lundu 2",
    GEONAMET == "TA Ngabu" & GEONAMES == "Chikwawa" ~ "TA Ngabu 1",
    GEONAMET == "TA Ngabu" & GEONAMES == "Nsanje" ~ "TA Ngabu 2",
    GEONAMET == "TA Chiwalo" & GEONAMES == "Machinga" ~ "TA Chiwalo 1",
    GEONAMET == "TA Chiwalo" & GEONAMES == "Phalombe" ~ "TA Chiwalo 2",
    GEONAMET == "TA Malemia" & GEONAMES == "Nsanje" ~ "TA Malemia 1",
    GEONAMET == "TA Malemia" & GEONAMES == "Zomba Rural" ~ "TA Malemia 2",
    # is.na(GEONAMET) & MICSGEO == "Lake Malombe" ~ "NA 1", # We comment out the four lines below, because actually MICSGEO is not a variable present in malawi19 (since it is not in the gps file) so we can't link these GEONAMET from malawi19_basemap3 to the ones in malawi19_prev_admin3, and most likely these four GEONAMET are unpopulated as they are lakes
    # is.na(GEONAMET) & MICSGEO == "Lake Malawi" ~ "NA 2",
    # is.na(GEONAMET) & MICSGEO == "Lake Chilwa" ~ "NA 3",
    # is.na(GEONAMET) & MICSGEO == "Lake Chiuta" ~ "NA 4",
    .default = GEONAMET
  ),
  admin3 = GEONAMET_cleaned
)

saoto19 <- saoto19 |> mutate(
  island = case_when(
    region == "REGIÃO AUTÓNOMA DO PRÍNCIPE" ~ "Príncipe", # For the indication of which region (the 5 regions specific to MICS) belongs to which island, and belongs to which district, see e.g. Tabela SD.1 p.274 of the Survey Findings Report
    region == "DISTRITO DE ÁGUA GRANDE" ~ "São Tomé",
    region == "DISTRITO DE MÉ-ZÓCHI" ~ "São Tomé",
    region == "REGIÃO NORTE OESTE" ~ "São Tomé",
    region == "REGIÃO SUL ESTE" ~ "São Tomé",
    .default = "Error in the case_when logic" # This value shouldn't be assigned
  ),
  admin1 = island,
  # Here first we rename three units, then we duplicate the relevant ones (when we make the prevalence table below)
  region_7 = case_when(
    region == "REGIÃO AUTÓNOMA DO PRÍNCIPE" ~ "Pagué", # For the indication that Pagué is the only district of the Principe island (and also for info on the other districts), see https://en.wikipedia.org/wiki/Districts_of_S%C3%A3o_Tom%C3%A9_and_Pr%C3%ADncipe ; # For the indication of which region (the 5 regions specific to MICS) belongs to which island, and belongs to which district, see e.g. Tabela SD.1 p.274 of the Survey Findings Report
    region == "DISTRITO DE ÁGUA GRANDE" ~ "Água Grande",
    region == "DISTRITO DE MÉ-ZÓCHI" ~ "Mé-Zóchi",
    # region == "REGIÃO NORTE OESTE" ~ "São Tomé", TODELETE
    # region == "REGIÃO SUL ESTE" ~ "São Tomé", TODELETE
    .default = region # For REGIÃO NORTE OESTE and REGIÃO SUL ESTE (we duplicate them below, here we only renamed the units that we don't modify)
  ),
  region_7_explicit_district_names = case_when(
    # /!\ The names separated below by a "&" are separate districts (so they are different units in basemaps) but were grouped in the survey, so later when we create the prevalence tibble at this admin level, we duplicate each row that corresponds to multiple districts and put its value in the row created for the individualized district
    region_7 == "REGIÃO NORTE OESTE" ~ "Lembá & Lobata",
    region_7 == "REGIÃO SUL ESTE" ~ "Cantagalo & Caué",
    .default = region_7
  ),
  admin2 = region_7_explicit_district_names
)

sierral17 <- sierral17 |> mutate(
  region_cleaned = case_when(
    region == "EAST" ~ "Eastern",
    region == "NORTH" ~ "Northern",
    region == "SOUTH" ~ "Southern",
    region == "WEST" ~ "Western",
    .default = region
  ),
  admin1 = region_cleaned,
  district_cleaned = str_to_title(district), # To capitalize only the first letter of each word
  district_cleaned = case_when(
    district_cleaned == "Western Area Rural" ~ "Western Rural",
    district_cleaned == "Western Area Urban" ~ "Western Urban",
    .default = district_cleaned
  ),
  admin2 = district_cleaned,
  GEONAMET_cleaned = case_when(
    GEONAMET == "KOYA" & GEONAMES == "KENEMA" ~ "KOYA 1", # Names after the "~" found in sierral17_basemap3 (after the naming differenciation between similarily named GEONAMET)
    GEONAMET == "KOYA" & GEONAMES == "PORT LOKO" ~ "KOYA 2",
    .default = GEONAMET
  ),
  admin3 = GEONAMET_cleaned
)

togo17 <- togo17 |> mutate(
  region_cleaned = case_when(
    region == "Centrale" ~ "Centre", # "Centrale" is named "Centre" in togo17_basemap1
    region == "Lomé Commune" ~ "Maritime", # Lomé is in Maritime based on the map here https://en.wikipedia.org/wiki/Regions_of_Togo
    region == "Golfe Urbain" ~ "Maritime", # Golfe is in Maritime as indicated here https://en.wikipedia.org/wiki/Prefectures_of_Togo
    .default = region
  ),
  admin1 = region_cleaned
)

yemen22 <- yemen22 |> mutate(
  region_cleaned = case_when(
    region == "Aden" ~ "`Adan",
    region == "Al Bayda" ~ "Al Bayda'",
    region == "Al Dhale'e" ~ "Al Dali'",
    region == "Al Maharah" ~ "Al Mahrah",
    region == "Sana'a City" ~ "Amanat Al Asimah", # Source: https://en.wikipedia.org/wiki/Governorates_of_Yemen
    region == "Hadramaut" ~ "Hadramawt",
    region == "Socotra" ~ "Hadramawt", # Source: https://en.wikipedia.org/wiki/Governorates_of_Yemen
    region == "Lahj" ~ "Lahij",
    region == "Marib" ~ "Ma'rib",
    region == "Sa'ada" ~ "Sa`dah",
    region == "Sana'a" ~ "San`a'",
    region == "Taizz" ~ "Ta`izz",
    .default = region
  ),
  admin1 = region_cleaned
)

zimbab19 <- zimbab19 |> mutate(admin1 = region, admin2 = GEONAMES) 

# 4) Merging all the recoded MICS datasets with adults and children, and in SSA ----

dta_mics <- bind_rows(benin21, centraf18, chad19, comor22, drcong17, eswat21, gamb18, ghana17, guibi18, lesot18, madag18, malawi19, saoto19, sierral17, togo17, zimbab19)  |>
  select(country, year, cluster_number, wt, psu, strata, region, type_place_residence,
         age, age_groups, gender, wealth_quintile, education_level_ever, 
         starts_with("diff"), starts_with("at_least"), starts_with("NAME_"), 
         admin1, admin2, admin3, geometry # Note that the geometry variable doesn't exist for all countries, because some mics countries don't have gps information
         #, DATUM, ALT_GPS,ALT_DEM these three variable don't exist for (the concatenation of) the MICS datasets
  ) |> # We keep only the columns we need for the SAE analysis
  mutate(survey="MICS") 

# 5) Survey design (and extraction of the "working" subsample) ----

options(survey.lonely.psu = "adjust") # Added for the case where there may be a single PSU in a strata, as indicated here https://www.youtube.com/watch?v=pJwd2-m3QBY

benin21wt <- benin21 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE) # psu for Primary Sampling Units, strata for Strata variable, weights for Weight variable, nest To handle nesting ; this says that "The DHS and MICS surveys have very similar two-stage sampling designs and the weights are calculated in the same way": https://userforum.dhsprogram.com/index.php?t=msg&goto=17946
benin21wt <- benin21wt |> srvyr::filter(age >= 18) # what follows was for when there was observations from the hl file in the dataset we applied the weighting to: |> srvyr::filter(file_source == "mn" | file_source == "wm") # respondants under 18 were not asked the unmodified WG-SS 

centraf18wt <- centraf18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
centraf18wt <- centraf18wt |> srvyr::filter(age >= 18)

chad19wt <- chad19 %>% filter(!is.na(HH7)) |> as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE) # We filter out the 38 observations that are NA for many variables including the disability variables (we filter with HH7 as it is an important variable)
chad19wt <- chad19wt |> srvyr::filter(age >= 18)
chad19wt <- chad19wt |> srvyr::filter(!is.na(AF6) & !is.na(AF9)) # We filter out the 7 observations for which all disability variables are NA + the 1 observation with NA for walk disability AF9 (but not NA in the other disability variables)

comor22wt <- comor22 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
comor22wt <- comor22wt |> srvyr::filter(age >= 18)

drcong17wt <- drcong17 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
drcong17wt <- drcong17wt |> srvyr::filter(age >= 18)
drcong17wt <- drcong17wt |> srvyr::filter(!is.na(AF6)) # We filter out the 3 observations for which all disability variables are NA

eswat21wt <- eswat21 %>% filter(!is.na(HH7)) |> as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE) # We filter out the 15 observations that are NA for many variables including the disability variables (we filter with HH6 as it is an important variable)
eswat21wt <- eswat21wt |> srvyr::filter(age >= 18)

gamb18wt <- gamb18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
gamb18wt <- gamb18wt |> srvyr::filter(age >= 18)

ghana17wt <- ghana17 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
ghana17wt <- ghana17wt |> srvyr::filter(age >= 18)

guibi18wt <- guibi18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
guibi18wt <- guibi18wt |> srvyr::filter(age >= 18)
guibi18wt <- guibi18wt |> srvyr::filter(!is.na(AF6)) # We filter out the 2 observations for which all disability variables are NA

lesot18wt <- lesot18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
lesot18wt <- lesot18wt |> srvyr::filter(age >= 18)

madag18wt <- madag18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
madag18wt <- madag18wt |> srvyr::filter(age >= 18)

malawi19wt <- malawi19 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
malawi19wt <- malawi19wt |> srvyr::filter(age >= 18)
malawi19wt <- malawi19wt |> srvyr::filter(!is.na(AF6)) # We filter out the 1 observation for which all disability variables are NA

saoto19wt <- saoto19 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
saoto19wt <- saoto19wt |> srvyr::filter(age >= 18)

sierral17wt <- sierral17 %>% as_survey_design(ids = cluster_number, strata = strata, weights = wt, nest = TRUE) # The PSU are the 600 sample clusters (so the variable HH1, renamed as cluster_number), see Sierra Leone 2017 MICS Survey Findings Report p.340 + the MICS team said by email: "Thanks for reaching us and noticing the missing PSU variable in the data. You can use HH1 as PSU. In addition, if you need the stratum variable for your analysis. It is the combination of HH6 and HH7."
sierral17wt <- sierral17wt |> srvyr::filter(age >= 18)

togo17wt <- togo17 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
togo17wt <- togo17wt |> srvyr::filter(age >= 18)

yemen22wt <- yemen22 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
yemen22wt <- yemen22wt |> srvyr::filter(age >= 18)

zimbab19wt <- zimbab19 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
zimbab19wt <- zimbab19wt |> srvyr::filter(age >= 18)

# 6) Creation of prevalence tables ----
# At the levels Admin 0 (country), then Admin 1 (e.g. region), then when available Admin 2 (e.g. district -> not sure it is this unit in MICS)
# + joining the basemaps and the prevalences at the same level


## 6.A) benin21 ----

benin21_prev_admin0 <- benin21wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

benin21_prev_admin1 <- benin21wt |> 
  srvyr::group_by(admin1) |> # Departments
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

benin21_prev_admin1 <- left_join(benin21_basemap1, benin21_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(benin21_basemap1, benin21_prev_admin1, by = join_by(NAME_1 == admin1))


## 6.B) centraf18 ----

centraf18_prev_admin0 <- centraf18wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

centraf18_prev_admin1 <- centraf18wt |>
  srvyr::group_by(admin1) |>  # Prefectures
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  ) |> ungroup() %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Ombella-M'Poko & Lobaye" (which was previously named Région 1) and put its values in the Ombella-M'Poko row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Ombella-M'Poko & Lobaye") |> mutate(admin1 = "Ombella-M'Poko"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Ombella-M'Poko & Lobaye" (which was previously named Région 1) and put its values in the Lobaye row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Ombella-M'Poko & Lobaye") |> mutate(admin1 = "Lobaye"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Mambéré-Kadéï & Sangha-Mbaéré & Nana-Mambéré" (which was previously named Région 2) and put its values in the Mambéré-Kadéï row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Mambéré-Kadéï & Sangha-Mbaéré & Nana-Mambéré") |> mutate(admin1 = "Mambéré-Kadéï"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Mambéré-Kadéï & Sangha-Mbaéré & Nana-Mambéré" (which was previously named Région 2) and put its values in the Sangha-Mbaéré row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Mambéré-Kadéï & Sangha-Mbaéré & Nana-Mambéré") |> mutate(admin1 = "Sangha-Mbaéré"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Mambéré-Kadéï & Sangha-Mbaéré & Nana-Mambéré" (which was previously named Région 2) and put its values in the Nana-Mambéré row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Mambéré-Kadéï & Sangha-Mbaéré & Nana-Mambéré") |> mutate(admin1 = "Nana-Mambéré"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Ouham & Ouham-Pendé" (which was previously named Région 3) and put its values in the Ouham row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Ouham & Ouham-Pendé") |> mutate(admin1 = "Ouham"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Ouham & Ouham-Pendé" (which was previously named Région 3) and put its values in the Ouham-Pendé row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Ouham & Ouham-Pendé") |> mutate(admin1 = "Ouham-Pendé"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Kémo & Nana-Grébizi & Ouaka" (which was previously named Région 4) and put its values in the Kémo row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Kémo & Nana-Grébizi & Ouaka") |> mutate(admin1 = "Kémo"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Kémo & Nana-Grébizi & Ouaka" (which was previously named Région 4) and put its values in the Nana-Grébizi row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Kémo & Nana-Grébizi & Ouaka") |> mutate(admin1 = "Nana-Grébizi"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Kémo & Nana-Grébizi & Ouaka" (which was previously named Région 4) and put its values in the Ouaka row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Kémo & Nana-Grébizi & Ouaka") |> mutate(admin1 = "Ouaka"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Bamingui-Bangoran & Haute-Kotto & Vakaga" (which was previously named Région 5) and put its values in the Bamingui-Bangoran row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Bamingui-Bangoran & Haute-Kotto & Vakaga") |> mutate(admin1 = "Bamingui-Bangoran"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Bamingui-Bangoran & Haute-Kotto & Vakaga" (which was previously named Région 5) and put its values in the Haute-Kotto row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Bamingui-Bangoran & Haute-Kotto & Vakaga") |> mutate(admin1 = "Haute-Kotto"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Bamingui-Bangoran & Haute-Kotto & Vakaga" (which was previously named Région 5) and put its values in the Vakaga row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Bamingui-Bangoran & Haute-Kotto & Vakaga") |> mutate(admin1 = "Vakaga"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Basse-Kotto & Mbomou & Haut-Mbomou" (which was previously named Région 6) and put its values in the Basse-Kotto row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Basse-Kotto & Mbomou & Haut-Mbomou") |> mutate(admin1 = "Basse-Kotto"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Basse-Kotto & Mbomou & Haut-Mbomou" (which was previously named Région 6) and put its values in the Mbomou row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Basse-Kotto & Mbomou & Haut-Mbomou") |> mutate(admin1 = "Mbomou"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Basse-Kotto & Mbomou & Haut-Mbomou" (which was previously named Région 6) and put its values in the Haut-Mbomou row, since this is where this unit belongs according to Report p.24
    as_tibble(filter(., admin1 == "Basse-Kotto & Mbomou & Haut-Mbomou") |> mutate(admin1 = "Haut-Mbomou"))
  ) %>% # only this type of pipe works here
  # The three code line below are not useful since we already "Région 7" was already renamed "Bangui", as Région 7 is only composed of the Bengui prefecture
  #bind_rows( # this part is to "duplicate" the Région 7 row and put its values in the Bangui row, since this is where this unit belongs according to Report p.24
  #  as_tibble(filter(., admin1 == "Bangui") |> mutate(admin1 = "Bangui"))
  #) |> 
  filter(!str_detect(admin1, "&")) # To remove the rows with a "&" in their name (which were previously named Région 1 to Région 7), method found here: https://stackoverflow.com/a/48405217

centraf18_prev_admin1 <- left_join(centraf18_basemap1, centraf18_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(centraf18_basemap1, centraf18_prev_admin1, by = join_by(NAME_1 == admin1))

#ggplot(centraf18_prev_admin1) + geom_sf(aes(fill = see_prev)) + labs(caption = "The 17 prefectures of Central African Republic were grouped into \n7 regions in the MICS survey (which means that prefectures \ngrouped in the same region have the same prevalence value)")


## 6.C) chad19 ----

chad19_prev_admin0 <- chad19wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

chad19_prev_admin1 <- chad19wt |> 
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

chad19_prev_admin1 <- left_join(chad19_basemap1, chad19_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(chad19_basemap1, chad19_prev_admin1, by = join_by(NAME_1 == admin1)) -> 1 region left "Tibesti", since prev_admin1 has no value of Tibesti as there is no observation from there


## 6.D) comor22 ----

comor22_prev_admin0 <- comor22wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

comor22_prev_admin1 <- comor22wt |> 
  srvyr::group_by(admin1) |> # Islands
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

comor22_prev_admin1 <- left_join(comor22_basemap1, comor22_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(comor22_basemap1, comor22_prev_admin1, by = join_by(NAME_1 == admin1))

comor22_prev_admin2 <- comor22wt |> 
  srvyr::group_by(admin2) |> # Prefectures
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

comor22_prev_admin2 <- left_join(comor22_basemap2, comor22_prev_admin2, by = join_by(GEONAMET == admin2)) # Check that all names match : anti_join(comor22_basemap2, comor22_prev_admin2, by = join_by(GEONAMET == admin2))
# comor22_prev_admin2 has a hole inside an island because we filtered out a unit that had invalid geom (the hole seems to correspond to the Karthola volcano)


## 6.E) drcong17 ----

drcong17_prev_admin0 <- drcong17wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

drcong17_prev_admin1 <- drcong17wt |> 
  srvyr::group_by(admin1) |> # Provinces
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

drcong17_prev_admin1 <- left_join(drcong17_basemap1, drcong17_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(drcong17_basemap1, drcong17_prev_admin1, by = join_by(NAME_1 == admin1))


## 6.F) eswat21 ----

eswat21_prev_admin0 <- eswat21wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

eswat21_prev_admin1 <- eswat21wt |> 
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

eswat21_prev_admin1 <- left_join(eswat21_basemap1, eswat21_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(eswat21_basemap1, eswat21_prev_admin1, by = join_by(NAME_1 == admin1))

eswat21_prev_admin2 <- eswat21wt |>
  srvyr::group_by(admin2) |> # constituencies (Tinkhundla)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

eswat21_prev_admin2 <- left_join(eswat21_basemap2 |> st_make_valid(), eswat21_prev_admin2, by = join_by(GEONAMES == admin2)) # |> st_make_valid() if geometries don't work # Check that all names match : anti_join(eswat21_basemap2, eswat21_prev_admin2, by = join_by(GEONAMES == admin2))


## 6.G) gamb18 ----

gamb18_prev_admin0 <- gamb18wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

gamb18_prev_admin1 <- gamb18wt |>
  srvyr::group_by(admin1) |> # Local government areas
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

gamb18_prev_admin1 <- left_join(gamb18_basemap1, gamb18_prev_admin1, by = join_by(REGNAME == admin1)) # Check that all names match : anti_join(gamb18_basemap1, gamb18_prev_admin1, by = join_by(REGNAME == admin1))

gamb18_prev_admin2 <- gamb18wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

gamb18_prev_admin2 <- left_join(gamb18_basemap2, gamb18_prev_admin2, by = join_by(GEONAMES == admin2)) # Check that all names match : anti_join(gamb18_basemap2, gamb18_prev_admin2, by = join_by(GEONAMES == admin2)) -> 1 region left "Brikama", since prev_admin2 has no value of Brikama as there is no observation from there


## 6.H) ghana17 ----

ghana17_prev_admin0 <- ghana17wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

ghana17_prev_admin1 <- ghana17wt |> 
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

ghana17_prev_admin1 <- left_join(ghana17_basemap1, ghana17_prev_admin1, by = join_by(REGNAME == admin1)) # Check that all names match : anti_join(ghana17_basemap1, ghana17_prev_admin1, by = join_by(REGNAME == admin1))


## 6.I) guibi18 ----

guibi18_prev_admin0 <- guibi18wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

guibi18_prev_admin1 <- guibi18wt |> 
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

guibi18_prev_admin1 <- left_join(guibi18_basemap1, guibi18_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(guibi18_basemap1, guibi18_prev_admin1, by = join_by(NAME_1 == admin1))


## 6.J) lesot18 ----

lesot18_prev_admin0 <- lesot18wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )


lesot18_prev_admin1 <- lesot18wt |> 
  srvyr::group_by(admin1) |> # Ecological regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

lesot18_prev_admin1 <- left_join(lesot18_basemap1, lesot18_prev_admin1, by = join_by(REGNAME == admin1)) # Check that all names match : anti_join(lesot18_basemap1, lesot18_prev_admin1, by = join_by(REGNAME == admin1))


lesot18_prev_admin2 <- lesot18wt |> 
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

lesot18_prev_admin2 <- left_join(lesot18_basemap2, lesot18_prev_admin2, by = join_by(NAME_1 == admin2)) # Check that all names match : anti_join(lesot18_basemap2, lesot18_prev_admin2, by = join_by(NAME_1 == admin2))


## 6.K) madag18 ----

madag18_prev_admin0 <- madag18wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

madag18_prev_admin1 <- madag18wt |>
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

madag18_prev_admin1 <- left_join(madag18_basemap1, madag18_prev_admin1, by = join_by(NAME_2 == admin1)) # Check that all names match : anti_join(madag18_basemap1, madag18_prev_admin1, by = join_by(NAME_2 == admin1))


madag18_prev_admin2 <- madag18wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

madag18_prev_admin2 <- left_join(madag18_basemap2, madag18_prev_admin2, by = join_by(GEONAMES == admin2)) # Check that all names match : anti_join(madag18_basemap2, madag18_prev_admin2, by = join_by(GEONAMES == admin2))


## 6.L) malawi19 ----

malawi19_prev_admin0 <- malawi19wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )


malawi19_prev_admin1 <- malawi19wt |>
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

malawi19_prev_admin1 <- left_join(malawi19_basemap1, malawi19_prev_admin1, by = join_by(REGNAME == admin1)) # Check that all names match : anti_join(malawi19_basemap1, malawi19_prev_admin1, by = join_by(REGNAME == admin1))


malawi19_prev_admin2 <- malawi19wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

malawi19_prev_admin2 <- left_join(malawi19_basemap2, malawi19_prev_admin2, by = join_by(NAME_1 == admin2)) # Check that all names match : anti_join(malawi19_basemap2, malawi19_prev_admin2, by = join_by(NAME_1 == admin2))


malawi19_prev_admin3 <- malawi19wt |>
  srvyr::filter(GEONAMES != "Likoma") |> # We filter out the 750 individuals (out of a total of 26 734) that are located in the Likoma district, since no chiefdom (i.e. variable GEONAMET is NA) is indicated for any of them (The GPS Readme file indicates: "For the purpose of respondent protection, 16 clusters of Likoma district are not displaced. However, for these 16 clusters, you have information about which region and district they belong to.")
  srvyr::group_by(admin3) |> # Traditional authorities
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

malawi19_prev_admin3 <- left_join(malawi19_basemap3, malawi19_prev_admin3, by = join_by(GEONAMET == admin3)) # Check that all names match (except the 97 out of 436 without observations inside them, which leaves 339 that are indeed present in malawi19_prev_admin3) : anti_join(malawi19_basemap3, malawi19_prev_admin3, by = join_by(GEONAMET == admin3))


## 6.M) saoto19 ----

saoto19_prev_admin0 <- saoto19wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )


saoto19_prev_admin1 <- saoto19wt |>
  srvyr::group_by(admin1) |> # Provinces
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

saoto19_prev_admin1 <- left_join(saoto19_basemap1, saoto19_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(saoto19_basemap1, saoto19_prev_admin1, by = join_by(NAME_1 == admin1))


saoto19_prev_admin2 <- saoto19wt |> 
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  ) |> ungroup() %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Lembá & Lobata" (which was previously named REGIÃO NORTE OESTE) and put its values in the Lembá row, since this is where this unit belongs according to Report p.274
    as_tibble(filter(., admin2 == "Lembá & Lobata") |> mutate(admin2 = "Lembá"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Lembá & Lobata" (which was previously named REGIÃO NORTE OESTE) and put its values in the Lobata row, since this is where this unit belongs according to Report p.274
    as_tibble(filter(., admin2 == "Lembá & Lobata") |> mutate(admin2 = "Lobata"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Cantagalo & Caué" (which was previously named REGIÃO SUL ESTE) and put its values in the Cantagalo row, since this is where this unit belongs according to Report p.274
    as_tibble(filter(., admin2 == "Cantagalo & Caué") |> mutate(admin2 = "Cantagalo"))
  ) %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the row "Cantagalo & Caué" (which was previously named REGIÃO SUL ESTE) and put its values in the Caué row, since this is where this unit belongs according to Report p.274
    as_tibble(filter(., admin2 == "Cantagalo & Caué") |> mutate(admin2 = "Caué"))
  ) |> 
  filter(!str_detect(admin2, "&")) # To remove the rows "Lembá & Lobata" and "Cantagalo & Caué" (which were previously named REGIÃO NORTE OESTE and REGIÃO SUL ESTE), method found here: https://stackoverflow.com/a/48405217

saoto19_prev_admin2 <- left_join(saoto19_basemap2, saoto19_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match : anti_join(saoto19_basemap2, saoto19_prev_admin2, by = join_by(NAME_2 == admin2))


#ggplot(saoto19_prev_admin2) + geom_sf(aes(fill = see_prev)) + labs(caption = "The 7 districts of São Tomé and Príncipe were grouped into \n5 regions in the MICS survey (which means that districts \ngrouped in the same region have the same prevalence value)")


## 6.N) sierral17 ----

sierral17_prev_admin0 <- sierral17wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

sierral17_prev_admin1 <- sierral17wt |> 
  srvyr::group_by(admin1) |> # Provinces
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

sierral17_prev_admin1 <- left_join(sierral17_basemap1, sierral17_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(sierral17_basemap1, sierral17_prev_admin1, by = join_by(NAME_1 == admin1))


sierral17_prev_admin2 <- sierral17wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

sierral17_prev_admin2 <- left_join(sierral17_basemap2, sierral17_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match : anti_join(sierral17_basemap2, sierral17_prev_admin2, by = join_by(NAME_2 == admin2))


sierral17_prev_admin3 <- sierral17wt |> 
  srvyr::group_by(admin3) |> # Chiefdoms
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

sierral17_prev_admin3 <- left_join(sierral17_basemap3, sierral17_prev_admin3, by = join_by(GEONAMET == admin3)) # Check that all names match (except the 3 chiefdoms without observations inside them: NOMO, KAMAJEI, UPPER BANTA) : anti_join(sierral17_basemap3, sierral17_prev_admin3, by = join_by(GEONAMET == admin3))


## 6.O) togo17 ----

togo17_prev_admin0 <- togo17wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )


togo17_prev_admin1 <- togo17wt |> 
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

togo17_prev_admin1 <- left_join(togo17_basemap1, togo17_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(togo17_basemap1, togo17_prev_admin1, by = join_by(NAME_1 == admin1))


## 6.P) yemen22 ----

yemen22_prev_admin0 <- yemen22wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

yemen22_prev_admin1 <- yemen22wt |> 
  srvyr::group_by(admin1) |> # Governorates (Muhafazah)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

yemen22_prev_admin1 <- left_join(yemen22_basemap1, yemen22_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(yemen22_basemap1, yemen22_prev_admin1, by = join_by(NAME_1 == admin1))


## 6.Q) zimbab19 ----

zimbab19_prev_admin0 <- zimbab19wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

zimbab19_prev_admin1 <- zimbab19wt |> 
  srvyr::group_by(admin1) |> # Provinces
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

zimbab19_prev_admin1 <- left_join(zimbab19_basemap1, zimbab19_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(zimbab19_basemap1, zimbab19_prev_admin1, by = join_by(NAME_1 == admin1))


zimbab19_prev_admin2 <- zimbab19wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

zimbab19_prev_admin2 <- left_join(zimbab19_basemap2 |> st_make_valid(), zimbab19_prev_admin2, by = join_by(GEONAMES == admin2)) # |> st_make_valid() if geometries don't work # Check that all names match : anti_join(zimbab19_basemap2, zimbab19_prev_admin2, by = join_by(GEONAMES == admin2))



# 7) Saving the recoded datasets and prevalence tables, as well as DHS recoded and LSMS ----

mics_recoded= tibble::lst(
      dta_mics,
     benin21wt,
     benin21_prev_admin0,
     benin21_prev_admin1,
     centraf18wt,
     centraf18_prev_admin0,
     centraf18_prev_admin1,
     chad19wt,
     chad19_prev_admin0,
     chad19_prev_admin1,
     comor22wt,
     comor22_prev_admin0,
     comor22_prev_admin1,
     comor22_prev_admin2,
     drcong17wt,
     drcong17_prev_admin0,
     drcong17_prev_admin1,
     eswat21wt,
     eswat21_prev_admin0,
     eswat21_prev_admin1,
     eswat21_prev_admin2,
     gamb18wt,
     gamb18_prev_admin0,
     gamb18_prev_admin1,
     gamb18_prev_admin2,
     ghana17wt,
     ghana17_prev_admin0,
     ghana17_prev_admin1,
     guibi18wt,
     guibi18_prev_admin0,
     guibi18_prev_admin1,
     lesot18wt,
     lesot18_prev_admin0,
     lesot18_prev_admin1,
     lesot18_prev_admin2,
     madag18wt,
     madag18_prev_admin0,
     madag18_prev_admin1,
     madag18_prev_admin2,
     malawi19wt,
     malawi19_prev_admin0,
     malawi19_prev_admin1,
     malawi19_prev_admin2,
     malawi19_prev_admin3,
     saoto19wt,
     saoto19_prev_admin0,
     saoto19_prev_admin1,
     saoto19_prev_admin2,
     sierral17wt,
     sierral17_prev_admin0,
     sierral17_prev_admin1,
     sierral17_prev_admin2,
     sierral17_prev_admin3,
     togo17wt,
     togo17_prev_admin0,
     togo17_prev_admin1,
     #yemen22wt,
     #yemen22_prev_admin0,
     #yemen22_prev_admin1,
     zimbab19wt,
     zimbab19_prev_admin0,
     zimbab19_prev_admin1,
     zimbab19_prev_admin2)

save(dhs_recoded, mics_recoded, lsms,
     file = "data/tmp/after03.RData")
