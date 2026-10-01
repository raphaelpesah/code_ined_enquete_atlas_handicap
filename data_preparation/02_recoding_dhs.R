# Recoding of DHS surveys


# 0) Loading packages and RData from 01_import ----

library(srvyr) # loaded before tidyverse to give priority to tidyverse version of functions like filter
library(tidyverse)
library(labelled)
library(survey)
library(questionr)
library(sf)


load("data/tmp/after01.RData")
# open the list containing data in the environment as dataframe separated for each country
list2env(dhs, envir = .GlobalEnv)


# 1) Creation of useful variables + labeling and cleaning of variable ----

## 1.A) kenya22 ----

kenya22 <- kenya22 |>
  mutate(
    country= "Kenya", # to be able to merge all the surveys together
    year= 2022, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000, # to compute the weights, as indicated here https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm or here https://www.youtube.com/watch?v=pJwd2-m3QBY
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103, # relevant variable for de facto according to https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    education_level_ever2 = sheduc, #(!) specific to kenya22 # this variable sheduc is only provided in Kenya 2022 DHS, so this line doesn't apply to the other DHS surveys ; hv106 and not sheduc is used in the disability tables in the kenya 2022 final report
    long_short_quest = shshort, #(!) specific to kenya22
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  ) |> 
  set_variable_labels(
    region = "County", # For other surveys it's "Region"
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Highest difficulty in at least one domain"
  ) |> 

# Making age groups
#kenya22 <- kenya22 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |> 

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#kenya22 <- kenya22 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#kenya22 <- kenya22 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |> 


# Creation of variable At least a lot of difficulty seeing
#kenya22 <- kenya22 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |> 


# Creation of variable At least a lot of difficulty hearing
#kenya22 <- kenya22 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |> 


# Creation of variable At least a lot of difficulty communicating
#kenya22 <- kenya22 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |> 


# Creation of variable At least a lot of difficulty remembering
#kenya22 <- kenya22 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |> 


# Creation of variable At least a lot of difficulty walking
#kenya22 <- kenya22 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |> 


# Creation of variable At least a lot of difficulty washing
#kenya22 <- kenya22 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 


# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#kenya22 <- kenya22 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 




## 1.B) malawi16 (!) ----
# (!) is to signal the creation of the standard disability variables for malawi16, senegal18, senegal19 and uganda16

malawi16 <- malawi16 |> 
  mutate(
    country= "Malawi", # to be able to merge all the surveys together
    year= 2016, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    district = shdist,
    de_jure_resident = hv102, # special inclusion to do the same as the dhs report
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = factor(case_when(
      sh308 == "can't see at all" | sh310 == "can't see at all" ~ "cannot see at all",
      sh308 == "some difficulty"| sh310 == "some difficulty" ~ "some difficulty",
      sh308 == "a lot of difficulty" | sh310 == "a lot of difficulty" ~ "a lot of difficulty",
      #sh308 %in% c("some difficulty", "a lot of difficulty") ~ sh308,
      #sh310 %in% c("some difficulty", "a lot of difficulty") ~ sh310,
      sh307 == "don't know" | sh308 == "don't know" | sh309 == "don't know" | sh310 == "don't know" ~ "don't know",
      sh307 == "no" | sh309 == "no" ~ "no difficulty seeing",
      .default = NA), 
      levels = c("no difficulty seeing", "some difficulty", "a lot of difficulty", "cannot see at all", "don't know")),
    diff_hear = factor(case_when(
      sh313 == "can't hear at all" | sh315 == "can't hear at all" ~ "cannot hear at all",
      sh313 == "some difficulty" | sh315 == "some difficulty" ~ "some difficulty",
      sh313 == "a lot of difficulty" | sh315 == "a lot of difficulty" ~ "a lot of difficulty",
      #sh313 %in% c("some difficulty", "a lot of difficulty") ~ sh313,
      #sh315 %in% c("some difficulty", "a lot of difficulty") ~ sh315,
      sh312 == "don't know" | sh313 == "don't know" | sh314 == "don't know" | sh315 == "don't know" ~ "don't know",
      sh312 == "no" | sh314 == "no" ~ "no difficulty hearing",
      .default = NA
    ), levels = c("no difficulty hearing", "some difficulty", "a lot of difficulty", "cannot hear at all", "don't know")),
    diff_com = factor(case_when(
      sh317 == "can't communicate at all" ~ "cannot communicate at all",
      sh317 %in% c("some difficulty", "a lot of difficulty") ~ sh317,
      sh317 == "don't know" | sh316 == "don't know" ~ "don't know",
      sh316 == "no" ~ "no difficulty communicating",
      .default = NA
    ), levels = c("no difficulty communicating", "some difficulty", "a lot of difficulty", "cannot wash or dress at all", "don't know")),
    diff_remem = factor(case_when(
      sh319 == "can't remember or concentrate at all" ~ "cannot remember/concentrate at all",
      sh319 %in% c("some difficulty", "a lot of difficulty") ~ sh319,
      sh319 == "don't know" | sh318 == "don't know" ~ "don't know",
      sh318 == "no" ~ "no difficulty remembering/concentrating",
      .default = NA
    ), levels = c("no difficulty remembering/concentrating", "some difficulty", "a lot of difficulty", "cannot wash or dress at all", "don't know")),
    diff_walk = factor(case_when(
      sh321 == "can't walk or climb steps at all" ~ "cannot walk or climb at all",
      sh321 %in% c("some difficulty", "a lot of difficulty") ~ sh321,
      sh321 == "don't know" | sh320 == "don't know" ~ "don't know",
      sh320 == "no" ~ "no difficulty walking or climbing",
      .default = NA
    ), levels = c("no difficulty walking or climbing", "some difficulty", "a lot of difficulty", "cannot walk or climb at all", "don't know")),
    diff_wash = factor(case_when(
      sh323 == "can't wash all over or dress at all" ~ "cannot wash or dress at all",
      sh323 %in% c("some difficulty", "a lot of difficulty") ~ sh323,
      sh323 == "don't know" | sh322 == "don't know" ~ "don't know",
      sh322 == "no" ~ "no difficulty washing or dressing",
      .default = NA
    ), levels = c("no difficulty washing or dressing", "some difficulty", "a lot of difficulty", "cannot wash or dress at all", "don't know")),
    highest_diff_any_domain = factor(case_when( # to check the validity of this case_when() code, it was applied to mali18 and the results were successfully identical to "mali18 |> select(highest_diff_any_domain) |> summary()"
      diff_see == "cannot see at all" | diff_hear == "cannot hear at all" | diff_com == "cannot communicate at all" | diff_remem == "cannot remember/concentrate at all" | diff_walk == "cannot walk or climb at all" | diff_wash == "cannot wash or dress at all" ~ "cannot do at all",
      diff_see == "a lot of difficulty" | diff_hear == "a lot of difficulty" | diff_com == "a lot of difficulty" | diff_remem == "a lot of difficulty" | diff_walk == "a lot of difficulty" | diff_wash == "a lot of difficulty" ~ "a lot of difficulty",
      diff_see == "some difficulty" | diff_hear == "some difficulty" | diff_com == "some difficulty" | diff_remem == "some difficulty" | diff_walk == "some difficulty" | diff_wash == "some difficulty" ~ "some difficulty",
      diff_see == "no difficulty seeing" | diff_hear == "no difficulty hearing" | diff_com == "no difficulty communicating" | diff_remem == "no difficulty remembering/concentrating" | diff_walk == "no difficulty walking or climbing" | diff_wash == "no difficulty washing or dressing" ~ "no difficulty",
      diff_see == "don't know" | diff_hear == "don't know" | diff_com == "don't know" | diff_remem == "don't know" | diff_walk == "don't know" | diff_wash == "don't know" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty", "some difficulty", "a lot of difficulty", "cannot do at all", "don't know")) # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
  ) |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#malawi16 <- malawi16 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
# malawi16 <- malawi16 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#malawi16 <- malawi16 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#malawi16 <- malawi16 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#malawi16 <- malawi16 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#malawi16 <- malawi16 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#malawi16 <- malawi16 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#malawi16 <- malawi16 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#malawi16 <- malawi16 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#malawi16 <- malawi16 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 


## 1.C) mali18 ----

mali18 <- mali18 |> 
  mutate(
    country= "Mali", # to be able to merge all the surveys together
    year= 2018, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  ) |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#mali18 <- mali18 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#mali18 <- mali18 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#mali18 <- mali18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#mali18 <- mali18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#mali18 <- mali18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#mali18 <- mali18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#mali18 <- mali18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#mali18 <- mali18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#mali18 <- mali18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#mali18 <- mali18 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 


## 1.D) maurit19 ----

maurit19 <- maurit19 |> 
  mutate(
    country= "Mauritania", # to be able to merge all the surveys together
    year= 2019, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  ) |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#maurit19 <- maurit19 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#maurit19 <- maurit19 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#maurit19 <- maurit19 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#maurit19 <- maurit19 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#maurit19 <- maurit19 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#maurit19 <- maurit19 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#maurit19 <- maurit19 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#maurit19 <- maurit19 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#maurit19 <- maurit19 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#maurit19 <- maurit19 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 



## 1.E) mozam22 ----

mozam22 <- mozam22 |> mutate(
  country= "Mozambique", # to be able to merge all the surveys together
  year= 2022, # to be able to merge all the surveys together
  cluster_number = hv001,
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
  age = hv105,
  marital_status = hv115,
  wealth_quintile = hv270,
  education_level_ever = hv106,
  diff_see = hdis2,
  diff_hear = hdis4,
  diff_com = hdis5,
  diff_remem = hdis6,
  diff_walk = hdis7,
  diff_wash = hdis8,
  highest_diff_any_domain = hdis9
) |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>


# Making age groups
#mozam22 <- mozam22 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#mozam22 <- mozam22 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#mozam22 <- mozam22 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#mozam22 <- mozam22 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#mozam22 <- mozam22 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#mozam22 <- mozam22 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#mozam22 <- mozam22 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#mozam22 <- mozam22 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#mozam22 <- mozam22 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#mozam22 <- mozam22 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 



## 1.F) nigeria18 ----

nigeria18 <- nigeria18 |> 
  mutate(
    country= "Nigeria", # to be able to merge all the surveys together
    year= 2018, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  )  |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#nigeria18 <- nigeria18 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#nigeria18 <- nigeria18 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#nigeria18 <- nigeria18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#nigeria18 <- nigeria18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#nigeria18 <- nigeria18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#nigeria18 <- nigeria18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#nigeria18 <- nigeria18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#nigeria18 <- nigeria18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#nigeria18 <- nigeria18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#nigeria18 <- nigeria18 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 



## 1.G) pakis17 ----

pakis17 <- pakis17 |> 
  mutate(
    country= "Pakistan", # to be able to merge all the surveys together
    year= 2017, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  ) |> set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#pakis17 <- pakis17 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#pakis17 <- pakis17 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#pakis17 <- pakis17 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#pakis17 <- pakis17 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#pakis17 <- pakis17 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#pakis17 <- pakis17 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#pakis17 <- pakis17 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#pakis17 <- pakis17 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#pakis17 <- pakis17 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#pakis17 <- pakis17 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 



## 1.H) rwanda19 ----

rwanda19 <- rwanda19 |> 
  mutate(
    country= "Rwanda", # to be able to merge all the surveys together
    year= 2019, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    district = shdistrict,
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  ) |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#rwanda19 <- rwanda19 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#rwanda19 <- rwanda19 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#rwanda19 <- rwanda19 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#rwanda19 <- rwanda19 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#rwanda19 <- rwanda19 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#rwanda19 <- rwanda19 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#rwanda19 <- rwanda19 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#rwanda19 <- rwanda19 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#rwanda19 <- rwanda19 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#rwanda19 <- rwanda19 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 


## 1.I18) senegal18 (!) ----
# (!) is to signal the creation of the standard disability variables for malawi16, senegal18, senegal19 and uganda16

senegal18 <- senegal18 |> 
  mutate(
    country= "Senegal", # to be able to merge all the surveys together
    year= 2018, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = factor(case_when( # we checked that nobody answered both sh20gb and sh20gc with senegal18 |> select(sh20gb, sh20gc) |> filter(!is.na(sh20gb), !is.na(sh20gc))
      sh20gb == "ne peut pas voir du tout" | sh20gc == "ne peut pas voir du tout" ~ "cannot see at all",
      sh20gb == "beaucoup de difficultes" | sh20gc == "beaucoup de difficultes" ~ "a lot of difficulty",
      sh20gb == "quelques difficultes" | sh20gc == "quelques difficultes" ~ "some difficulty",
      sh20gb == "aucune difficulte pour voir" | sh20gc == "aucune difficulte pour voir" ~ "no difficulty seeing",
      sh20gb == "ne sait pas" | sh20gc == "ne sait pas" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty seeing", "some difficulty", "a lot of difficulty", "cannot see at all", "don't know")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order ; Note 16/1: we also add the level "don't know" similarily as for the variable "diff_hear" just below because it's useful to have this level when applying a mutate that converts the "don't know" as we do below
    diff_hear = factor(case_when( # we checked that nobody answered both sh20ge and sh20gf with senegal18 |> select(sh20ge, sh20gf) |> filter(!is.na(sh20ge), !is.na(sh20gf))
      sh20ge == "ne peut pas entendre du tout" | sh20gf == "ne peut pas entendre du tout" ~ "cannot hear at all",
      sh20ge == "beaucoup de difficultes" | sh20gf == "beaucoup de difficultes" ~ "a lot of difficulty",
      sh20ge == "quelques difficultes" | sh20gf == "quelques difficultes" ~ "some difficulty",
      sh20ge == "aucune difficulte pour entendre" | sh20gf == "aucune difficulte pour entendre" ~ "no difficulty hearing",
      sh20ge == "ne sait pas" | sh20gf == "ne sait pas" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty hearing", "some difficulty", "a lot of difficulty", "cannot hear at all", "don't know")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
    diff_com = fct_recode(sh20gg, "no difficulty communicating" = "aucune difficulte pour communiquer", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot communicate at all" = "ne peut pas communiquer du tout", "don't know" = "ne sait pas"),
    diff_remem = fct_recode(sh20gh, "no difficulty remembering/concentrating" = "aucune difficulte pour se rappeler ou se concentrer", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot remember/concentrate at all" = "ne peut pas se rappeler ou se concentrer", "don't know" = "ne sait pas"),
    diff_walk = fct_recode(sh20gi, "no difficulty walking or climbing" = "aucune difficulte pour marcher ou grimper", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot walk or climb at all" = "ne peut pas marcher ou grimper du tout", "don't know" = "ne sait pas"),
    diff_wash = fct_recode(sh20gj, "no difficulty washing or dressing" = "aucune difficulte pour se laver ou s'habiller", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot wash or dress at all" = "ne peut pas se laver ou s'habiller du tout", "don't know" = "ne sait pas"),
    highest_diff_any_domain = factor(case_when( # to check the validity of this case_when() code, it was applied to mali18 and the results were successfully identical to "mali18 |> select(highest_diff_any_domain) |> summary()"
      diff_see == "cannot see at all" | diff_hear == "cannot hear at all" | diff_com == "cannot communicate at all" | diff_remem == "cannot remember/concentrate at all" | diff_walk == "cannot walk or climb at all" | diff_wash == "cannot wash or dress at all" ~ "cannot do at all",
      diff_see == "a lot of difficulty" | diff_hear == "a lot of difficulty" | diff_com == "a lot of difficulty" | diff_remem == "a lot of difficulty" | diff_walk == "a lot of difficulty" | diff_wash == "a lot of difficulty" ~ "a lot of difficulty",
      diff_see == "some difficulty" | diff_hear == "some difficulty" | diff_com == "some difficulty" | diff_remem == "some difficulty" | diff_walk == "some difficulty" | diff_wash == "some difficulty" ~ "some difficulty",
      diff_see == "no difficulty seeing" | diff_hear == "no difficulty hearing" | diff_com == "no difficulty communicating" | diff_remem == "no difficulty remembering/concentrating" | diff_walk == "no difficulty walking or climbing" | diff_wash == "no difficulty washing or dressing" ~ "no difficulty",
      diff_see == "don't know" | diff_hear == "don't know" | diff_com == "don't know" | diff_remem == "don't know" | diff_walk == "don't know" | diff_wash == "don't know" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty", "some difficulty", "a lot of difficulty", "cannot do at all", "don't know")) # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
  )  |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>
  
  # Making age groups
  #senegal18 <- senegal18 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
                     include.lowest = TRUE,
                     right = FALSE,
                     dig.lab = 4,
                     breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                     labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
    )
  ) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>
  
  # Transformation of the value "don't know" to NA in the "education_level_ever" variable
  #senegal18 <- senegal18 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 
  
  
  # Creation of variable Has at least a lot of difficulty in any domain
  #senegal18 <- senegal18 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>
  
  
  # Creation of variable At least a lot of difficulty seeing
  #senegal18 <- senegal18 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>
  
  
  # Creation of variable At least a lot of difficulty hearing
  #senegal18 <- senegal18 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>
  
  
  # Creation of variable At least a lot of difficulty communicating
  #senegal18 <- senegal18 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>
  
  
  # Creation of variable At least a lot of difficulty remembering
  #senegal18 <- senegal18 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>
  
  
  # Creation of variable At least a lot of difficulty walking
  #senegal18 <- senegal18 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>
  
  
  # Creation of variable At least a lot of difficulty washing
  #senegal18 <- senegal18 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
  # Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
  # For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
  #senegal18 <- senegal18 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 



## 1.I) senegal19 (!) ----
# (!) is to signal the creation of the standard disability variables for malawi16, senegal18, senegal19 and uganda16

senegal19 <- senegal19 |> 
  mutate(
    country= "Senegal", # to be able to merge all the surveys together
    year= 2019, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = factor(case_when( # we checked that nobody answered both sh20gb and sh20gc with senegal19 |> select(sh20gb, sh20gc) |> filter(!is.na(sh20gb), !is.na(sh20gc))
      sh20gb == "ne peut pas voir du tout" | sh20gc == "ne peut pas voir du tout" ~ "cannot see at all",
      sh20gb == "beaucoup de difficultes" | sh20gc == "beaucoup de difficultes" ~ "a lot of difficulty",
      sh20gb == "quelques difficultes" | sh20gc == "quelques difficultes" ~ "some difficulty",
      sh20gb == "aucune difficulte pour voir" | sh20gc == "aucune difficulte pour voir" ~ "no difficulty seeing",
      sh20gb == "ne sait pas" | sh20gc == "ne sait pas" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty seeing", "some difficulty", "a lot of difficulty", "cannot see at all", "don't know")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order ; Note 16/1: we also add the level "don't know" similarily as for the variable "diff_hear" just below because it's useful to have this level when applying a mutate that converts the "don't know" as we do below
    diff_hear = factor(case_when( # we checked that nobody answered both sh20ge and sh20gf with senegal19 |> select(sh20ge, sh20gf) |> filter(!is.na(sh20ge), !is.na(sh20gf))
      sh20ge == "ne peut pas entendre du tout" | sh20gf == "ne peut pas entendre du tout" ~ "cannot hear at all",
      sh20ge == "beaucoup de difficultes" | sh20gf == "beaucoup de difficultes" ~ "a lot of difficulty",
      sh20ge == "quelques difficultes" | sh20gf == "quelques difficultes" ~ "some difficulty",
      sh20ge == "aucune difficulte pour entendre" | sh20gf == "aucune difficulte pour entendre" ~ "no difficulty hearing",
      sh20ge == "ne sait pas" | sh20gf == "ne sait pas" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty hearing", "some difficulty", "a lot of difficulty", "cannot hear at all", "don't know")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
    diff_com = fct_recode(sh20gg, "no difficulty communicating" = "aucune difficulte pour communiquer", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot communicate at all" = "ne peut pas communiquer du tout", "don't know" = "ne sait pas"),
    diff_remem = fct_recode(sh20gh, "no difficulty remembering/concentrating" = "aucune difficulte pour se rappeler ou se concentrer", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot remember/concentrate at all" = "ne peut pas se rappeler ou se concentrer", "don't know" = "ne sait pas"),
    diff_walk = fct_recode(sh20gi, "no difficulty walking or climbing" = "aucune difficulte pour marcher ou grimper", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot walk or climb at all" = "ne peut pas marcher ou grimper du tout", "don't know" = "ne sait pas"),
    diff_wash = fct_recode(sh20gj, "no difficulty washing or dressing" = "aucune difficulte pour se laver ou s'habiller", "some difficulty" = "quelques difficultes", "a lot of difficulty" = "beaucoup de difficultes", "cannot wash or dress at all" = "ne peut pas se laver ou s'habiller du tout", "don't know" = "ne sait pas"),
    highest_diff_any_domain = factor(case_when( # to check the validity of this case_when() code, it was applied to mali18 and the results were successfully identical to "mali18 |> select(highest_diff_any_domain) |> summary()"
      diff_see == "cannot see at all" | diff_hear == "cannot hear at all" | diff_com == "cannot communicate at all" | diff_remem == "cannot remember/concentrate at all" | diff_walk == "cannot walk or climb at all" | diff_wash == "cannot wash or dress at all" ~ "cannot do at all",
      diff_see == "a lot of difficulty" | diff_hear == "a lot of difficulty" | diff_com == "a lot of difficulty" | diff_remem == "a lot of difficulty" | diff_walk == "a lot of difficulty" | diff_wash == "a lot of difficulty" ~ "a lot of difficulty",
      diff_see == "some difficulty" | diff_hear == "some difficulty" | diff_com == "some difficulty" | diff_remem == "some difficulty" | diff_walk == "some difficulty" | diff_wash == "some difficulty" ~ "some difficulty",
      diff_see == "no difficulty seeing" | diff_hear == "no difficulty hearing" | diff_com == "no difficulty communicating" | diff_remem == "no difficulty remembering/concentrating" | diff_walk == "no difficulty walking or climbing" | diff_wash == "no difficulty washing or dressing" ~ "no difficulty",
      diff_see == "don't know" | diff_hear == "don't know" | diff_com == "don't know" | diff_remem == "don't know" | diff_walk == "don't know" | diff_wash == "don't know" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty", "some difficulty", "a lot of difficulty", "cannot do at all", "don't know")) # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
  )  |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#senegal19 <- senegal19 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#senegal19 <- senegal19 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#senegal19 <- senegal19 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#senegal19 <- senegal19 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#senegal19 <- senegal19 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#senegal19 <- senegal19 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#senegal19 <- senegal19 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#senegal19 <- senegal19 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#senegal19 <- senegal19 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#senegal19 <- senegal19 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 


## 1.J) safrica16 ----

safrica16 <- safrica16 |> 
  mutate(
    country= "South Africa", # to be able to merge all the surveys together
    year= 2016, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  )  |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#safrica16 <- safrica16 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#safrica16 <- safrica16 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 

# Creation of variable Has at least a lot of difficulty in any domain
#safrica16 <- safrica16 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#safrica16 <- safrica16 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#safrica16 <- safrica16 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#safrica16 <- safrica16 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#safrica16 <- safrica16 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#safrica16 <- safrica16 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#safrica16 <- safrica16 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#safrica16 <- safrica16 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 



## 1.K) tanza22 ----

tanza22 <- tanza22 |> 
  mutate(
    country= "Tanzania", # to be able to merge all the surveys together
    year= 2022, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    education_level_ever = hv106,
    diff_see = hdis2,
    diff_hear = hdis4,
    diff_com = hdis5,
    diff_remem = hdis6,
    diff_walk = hdis7,
    diff_wash = hdis8,
    highest_diff_any_domain = hdis9
  ) |> 
  set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>

# Making age groups
#tanza22 <- tanza22 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#tanza22 <- tanza22 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#tanza22 <- tanza22 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#tanza22 <- tanza22 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#tanza22 <- tanza22 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#tanza22 <- tanza22 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#tanza22 <- tanza22 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#tanza22 <- tanza22 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#tanza22 <- tanza22 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#tanza22 <- tanza22 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 



## 1.L) uganda16 (!) ----
# (!) is to signal the creation of the standard disability variables for malawi16, senegal18, senegal19 and uganda16


uganda16 <- uganda16 |> 
  mutate(
    country= "Uganda", # to be able to merge all the surveys together
    year= 2016, # to be able to merge all the surveys together
    cluster_number = hv001,
    household_sample_weight = hv005,
    wt = household_sample_weight / 1000000,
    psu = hv021,
    strata = hv022,
    region = hv024,
    type_place_residence = hv025,
    de_facto_resident = hv103,
    gender = hv104,
    age = hv105,
    marital_status = hv115,
    wealth_quintile = hv270,
    wealth_quintile = fct_recode(wealth_quintile, poorest = "lowest", poorer = "second", richer = "fourth", richest = "highest"), # this is the name of the levels of the wealth_quintile variable for all other DHS countries
    education_level_ever = hv106,
    diff_see = factor(case_when( # we checked that nobody answered both sh24 and sh25 with uganda16 |> select(sh27, sh28) |> filter(!is.na(sh27), !is.na(sh28))
      sh24 == "cannot see at all" | sh25 == "cannot see at all" ~ "cannot see at all",
      sh24 == "a lot of difficulty" | sh25 == "a lot of difficulty" ~ "a lot of difficulty",
      sh24 == "some difficulty" | sh25 == "some difficulty" ~ "some difficulty",
      sh24 == "no difficulty seeing" | sh25 == "no difficulty seeing" ~ "no difficulty seeing",
      sh24 == "don't know" | sh25 == "don't know" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty seeing", "some difficulty", "a lot of difficulty", "cannot see at all", "don't know")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order ;  ; Note 16/1: we also add the level "don't know" similarily as for the variable "diff_hear" just below because it's useful to have this level when applying a mutate that converts the "don't know" as we do below
    diff_hear = factor(case_when( # we checked that nobody answered both sh27 and sh28 with uganda16 |> select(sh27, sh28) |> filter(!is.na(sh27), !is.na(sh28))
      sh27 == "cannot hear at all" | sh28 == "cannot hear at all" ~ "cannot hear at all",
      sh27 == "a lot of difficulty" | sh28 == "a lot of difficulty" ~ "a lot of difficulty",
      sh27 == "some difficulty" | sh28 == "some difficulty" ~ "some difficulty",
      sh27 == "no difficulty hearing" | sh28 == "no difficulty hearing" ~ "no difficulty hearing",
      sh27 == "don't know" | sh28 == "don't know" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty hearing", "some difficulty", "a lot of difficulty", "cannot hear at all", "don't know")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
    diff_com = sh29,
    diff_remem = sh30,
    diff_walk = sh31,
    diff_wash = sh32,
    highest_diff_any_domain = factor(case_when( # to check the validity of this case_when() code, it was applied to mali18 and the results were successfully identical to "mali18 |> select(highest_diff_any_domain) |> summary()"
      diff_see == "cannot see at all" | diff_hear == "cannot hear at all" | diff_com == "cannot communicate at all" | diff_remem == "cannot remember/concentrate at all" | diff_walk == "cannot walk or climb at all" | diff_wash == "cannot wash or dress at all" ~ "cannot do at all",
      diff_see == "a lot of difficulty" | diff_hear == "a lot of difficulty" | diff_com == "a lot of difficulty" | diff_remem == "a lot of difficulty" | diff_walk == "a lot of difficulty" | diff_wash == "a lot of difficulty" ~ "a lot of difficulty",
      diff_see == "some difficulty" | diff_hear == "some difficulty" | diff_com == "some difficulty" | diff_remem == "some difficulty" | diff_walk == "some difficulty" | diff_wash == "some difficulty" ~ "some difficulty",
      diff_see == "no difficulty seeing" | diff_hear == "no difficulty hearing" | diff_com == "no difficulty communicating" | diff_remem == "no difficulty remembering/concentrating" | diff_walk == "no difficulty walking or climbing" | diff_wash == "no difficulty washing or dressing" ~ "no difficulty",
      diff_see == "don't know" | diff_hear == "don't know" | diff_com == "don't know" | diff_remem == "don't know" | diff_walk == "don't know" | diff_wash == "don't know" ~ "don't know",
      .default = NA
    ), levels = c("no difficulty", "some difficulty", "a lot of difficulty", "cannot do at all", "don't know")) # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
  ) |> set_variable_labels(
    region = "Region",
    type_place_residence = "Residence",
    education_level_ever = "Education",
    wealth_quintile = "Wealth quintile",
    gender = "Gender",
    diff_see = "Difficulty seeing",
    diff_hear = "Difficulty hearing",
    diff_com = "Difficulty communicating",
    diff_remem = "Difficulty remembering or concentrating",
    diff_walk = "Difficulty walking or climbing steps",
    diff_wash = "Difficulty washing all over or dressing",
    highest_diff_any_domain = "Difficulty in at least one domain"
  ) |>


# Making age groups
#uganda16 <- uganda16 |> 
  mutate(
    age = if_else(age == 98, NA, age), # Because "Don't know" answers were coded as 98
    age_groups = cut(age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
  )
) |>
  # fct_na_value_to_level("Don't know") |> # To make the NA a level in itself
  set_variable_labels(age_groups = "Age group") |>

# Transformation of the value "don't know" to NA in the "education_level_ever" variable
#uganda16 <- uganda16 |> 
  mutate(education_level_ever = na_if(education_level_ever, "don't know")) |> 


# Creation of variable Has at least a lot of difficulty in any domain
#uganda16 <- uganda16 %>%
  mutate(at_least_alotof_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("some difficulty", "no difficulty") ~ "no",
    highest_diff_any_domain %in% c("a lot of difficulty", "cannot do at all", "don't know") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_any_domain = "Has at least a lot of difficulty in any domain") |>


# Creation of variable At least a lot of difficulty seeing
#uganda16 <- uganda16 %>%
  mutate(at_least_alotof_diff_see = case_when(
    diff_see %in% c("some difficulty", "no difficulty seeing", "don't know") ~ "no",
    diff_see %in% c("a lot of difficulty", "cannot see at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_see = "At least a lot of difficulty seeing") |>


# Creation of variable At least a lot of difficulty hearing
#uganda16 <- uganda16 %>%
  mutate(at_least_alotof_diff_hear = case_when(
    diff_hear %in% c("some difficulty", "no difficulty hearing", "don't know") ~ "no",
    diff_hear %in% c("a lot of difficulty", "cannot hear at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_hear = "At least a lot of difficulty hearing") |>


# Creation of variable At least a lot of difficulty communicating
#uganda16 <- uganda16 |> 
  mutate(at_least_alotof_diff_com = case_when(
    diff_com %in% c("some difficulty", "no difficulty communicating", "don't know") ~ "no",
    diff_com %in% c("a lot of difficulty", "cannot communicate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_com = "At least a lot of difficulty communicating") |>


# Creation of variable At least a lot of difficulty remembering
#uganda16 <- uganda16 %>%
  mutate(at_least_alotof_diff_remem = case_when(
    diff_remem %in% c("some difficulty", "no difficulty remembering/concentrating", "don't know") ~ "no",
    diff_remem %in% c("a lot of difficulty", "cannot remember/concentrate at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_remem = "At least a lot of difficulty remembering or concentrating") |>


# Creation of variable At least a lot of difficulty walking
#uganda16 <- uganda16 %>%
  mutate(at_least_alotof_diff_walk = case_when(
    diff_walk %in% c("some difficulty", "no difficulty walking or climbing", "don't know") ~ "no",
    diff_walk %in% c("a lot of difficulty", "cannot walk or climb at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_walk = "At least a lot of difficulty walking") |>


# Creation of variable At least a lot of difficulty washing
#uganda16 <- uganda16 %>%
  mutate(at_least_alotof_diff_wash = case_when(
    diff_wash %in% c("some difficulty", "no difficulty washing or dressing", "don't know") ~ "no",
    diff_wash %in% c("a lot of difficulty", "cannot wash or dress at all") ~ "yes"
  ) |> factor(levels = c("yes", "no"))) %>%
  set_variable_labels(at_least_alotof_diff_wash = "At least a lot of difficulty washing or dressing") |> 
  
# Transformation of the value "don't know" to NA in all the "diff_" variables (diff_see, diff_hear, diff_com, diff_remem, diff_walk, diff_wash)
# For all countries, we checked that for all the disability variables, the "don't know" answer, when it was a possible answer (i.e. for most of the disability variables) was written precisely like this, uncapitalized ; Note that it was answered by very few people (around 1-50 people generally, less than 0.1%) and also that we do this transformation of “don’t know” answers to NA after having used these variables for the creation of the variables “a_least_alotof_diff…” above
#uganda16 <- uganda16 |> 
  mutate(across(starts_with("diff_"), ~ na_if(.x, "don't know"))) 


# 2) Loading the basemaps ---- 

kenya22_basemap0 <- st_read("data/basemaps/gadm41_KEN.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
kenya22_basemap1 <- st_read("data/basemaps/gadm41_KEN.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
kenya22_basemap2 <- st_read("data/basemaps/gadm41_KEN.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # kenya22_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
kenya22_basemap2 <- kenya22_basemap2 |> group_by(NAME_2) |> mutate(NAME_2 = case_when(n() > 1 ~ paste0(NAME_2, " ", row_number()), TRUE ~ NAME_2)) |> ungroup() # If there are multiple rows (duplicates) in the group (n() > 1), append a suffix (" 1"," 2", etc.) to NAME_2 using row_number()
#kenya22_basemap3 <- st_read("data/basemaps/gadm41_KEN.gpkg", layer = "ADM_ADM_3")

malawi16_basemap0 <- st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
malawi16_basemap1 <- st_read("data/basemaps/Malawi 2015 DHS - sdr_subnational_boundaries_2024-10-17/shps/sdr_subnational_boundaries.shp") # Not: st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_1") because this layer is one level below
malawi16_basemap2 <- st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
#malawi16_basemap3 <- st_read("data/basemaps/gadm41_MWI.gpkg", layer = "ADM_ADM_3")

mali18_basemap0 <- st_read("data/basemaps/gadm41_MLI.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
mali18_basemap1 <- st_read("data/basemaps/gadm41_MLI.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
mali18_basemap2 <- st_read("data/basemaps/gadm41_MLI.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # mali18_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
#mali18_basemap3 <- st_read("data/basemaps/gadm41_MLI.gpkg", layer = "ADM_ADM_3")

maurit19_basemap0 <- st_read("data/basemaps/gadm41_MRT.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
maurit19_basemap1 <- st_read("data/basemaps/gadm41_MRT.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
maurit19_basemap2 <- st_read("data/basemaps/gadm41_MRT.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # maurit19_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
# No layer 3 here, so no need to use: maurit19_basemap3 <- st_read("data/basemaps/gadm41_MRT.gpkg", layer = "ADM_ADM_3")

mozam22_basemap0 <- st_read("data/basemaps/gadm41_MOZ.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
mozam22_basemap1 <- st_read("data/basemaps/gadm41_MOZ.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
mozam22_basemap2 <- st_read("data/basemaps/gadm41_MOZ.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # mozam22_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
mozam22_basemap2 <- mozam22_basemap2 |> group_by(NAME_2) |> mutate(NAME_2 = case_when(n() > 1 ~ paste0(NAME_2, " ", row_number()), TRUE ~ NAME_2)) |> ungroup() # If there are multiple rows (duplicates) in the group (n() > 1), append a suffix (" 1"," 2", etc.) to NAME_2 using row_number()
#mozam22_basemap3 <- st_read("data/basemaps/gadm41_MOZ.gpkg", layer = "ADM_ADM_3")

nigeria18_basemap0 <- st_read("data/basemaps/gadm41_NGA.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
nigeria18_basemap1pre <- st_read("data/basemaps/Nigeria 2018 DHS - sdr_subnational_boundaries_2024-10-22/shps/sdr_subnational_boundaries.shp") # Not: st_read("data/basemaps/gadm41_NGA.gpkg", layer = "ADM_ADM_1") because this layer is one level below
nigeria18_basemap1 <- st_read("data/basemaps/gadm41_NGA.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
nigeria18_basemap2 <- st_read("data/basemaps/gadm41_NGA.gpkg", layer = "ADM_ADM_2")  |> rename(geometry = geom)# nigeria18_basemap3 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
nigeria18_basemap2 <- nigeria18_basemap2 |> group_by(NAME_2) |> mutate(NAME_2 = case_when(n() > 1 ~ paste0(NAME_2, " ", row_number()), TRUE ~ NAME_2)) |> ungroup() # If there are multiple rows (duplicates) in the group (n() > 1), append a suffix (" 1"," 2", etc.) to NAME_2 using row_number()

pakis17_basemap0 <- st_read("data/basemaps/gadm41_PAK.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
pakis17_basemap1 <- st_read("data/basemaps/gadm41_PAK.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
pakis17_basemap2 <- st_read("data/basemaps/gadm41_PAK.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom)# pakis17_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
pakis17_basemap2 <- pakis17_basemap2 |> group_by(NAME_2) |> mutate(NAME_2 = case_when(n() > 1 ~ paste0(NAME_2, " ", row_number()), TRUE ~ NAME_2)) |> ungroup() # If there are multiple rows (duplicates) in the group (n() > 1), append a suffix (" 1"," 2", etc.) to NAME_2 using row_number()
#pakis17_basemap3 <- st_read("data/basemaps/gadm41_PAK.gpkg", layer = "ADM_ADM_3")

rwanda19_basemap0 <- st_read("data/basemaps/gadm41_RWA.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
rwanda19_basemap1 <- st_read("data/basemaps/gadm41_RWA.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
rwanda19_basemap2 <- st_read("data/basemaps/gadm41_RWA.gpkg", layer = "ADM_ADM_2")  |> rename(geometry = geom)# rwanda19_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
#rwanda19_basemap3 <- st_read("data/basemaps/gadm41_RWA.gpkg", layer = "ADM_ADM_3")

senegal18_basemap0 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
senegal18_basemap1 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
senegal18_basemap2 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom)# senegal18_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
#senegal18_basemap3 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_3")

senegal19_basemap0 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
senegal19_basemap1 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
senegal19_basemap2 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # senegal19_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
#senegal19_basemap3 <- st_read("data/basemaps/gadm41_SEN.gpkg", layer = "ADM_ADM_3")

safrica16_basemap0 <- st_read("data/basemaps/gadm41_ZAF.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
safrica16_basemap1 <- st_read("data/basemaps/gadm41_ZAF.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
safrica16_basemap2 <- st_read("data/basemaps/gadm41_ZAF.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # safrica16_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
#safrica16_basemap3 <- st_read("data/basemaps/gadm41_ZAF.gpkg", layer = "ADM_ADM_3")

tanza22_basemap0 <- st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
tanza22_basemap1 <- st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom)
tanza22_basemap2 <- st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_2") |> rename(geometry = geom) # tanza22_basemap2 |> group_by(NAME_2) |> filter(n() > 1) |> ungroup() |> distinct(NAME_2) # To check if there is no duplicated district name
tanza22_basemap2 <- tanza22_basemap2 |> group_by(NAME_2) |> mutate(NAME_2 = case_when(n() > 1 ~ paste0(NAME_2, " ", row_number()), TRUE ~ NAME_2)) |> ungroup() # If there are multiple rows (duplicates) in the group (n() > 1), append a suffix (" 1"," 2", etc.) to NAME_2 using row_number()
#tanza22_basemap3 <- st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_3")

uganda16_basemap0 <- st_read("data/basemaps/gadm41_UGA.gpkg", layer = "ADM_ADM_0") |> rename(geometry = geom)
uganda16_basemap1 <- st_read("data/basemaps/Uganda 2016 DHS - sdr_subnational_boundaries_2024-10-23/shps/sdr_subnational_boundaries.shp") # Not: st_read("data/basemaps/gadm41_UGA.gpkg", layer = "ADM_ADM_1") 
uganda16_basemap2 <- st_read("data/basemaps/gadm41_UGA.gpkg", layer = "ADM_ADM_1") |> rename(geometry = geom) # 58 # uganda16_basemap2 |> group_by(NAME_1) |> filter(n() > 1) |> ungroup() |> distinct(NAME_1) # To check if there is no duplicated district name
#uganda16_basemap3 <- st_read("data/basemaps/gadm41_UGA.gpkg", layer = "ADM_ADM_2") # 166
#uganda16_basemap4 <- st_read("data/basemaps/gadm41_UGA.gpkg", layer = "ADM_ADM_3") # 965
# For info: "Since 2005, the Ugandan government has been in the process of dividing districts into smaller units." https://en.wikipedia.org/wiki/Districts_of_Uganda


# 3) Joining GPS data for each survey, first each cluster data (in the gps dataset) to the admin2 unit it belongs to, then to the survey dataset ----

## 3.1) Joining of GPS datasets to smallest possible admin ----

# kenya22

kenya22gps <- st_join(kenya22gps, kenya22_basemap2) # This line checks which points from the DHS gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

kenya22gps <- kenya22gps |> 
  mutate(NAME_2 = case_when(
    is.na(NAME_2) ~ "Laikipia West", # NA is located in Laikipia West according to its location on our map and the publication below (NA in basemap2 because in GADM this location is not in Kenya https://en.wikipedia.org/wiki/Ilemi_Triangle)
    # NAME_2 == "805" ~ "Mogotio", # Note 27/11: if we change the label I think we'll have to also change it when we do the join between the prevalence table and the basemap # Not sure it's useful to change the label # # 805 corresponds to Mogotio according to Supplementary file 2 of this https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-021-12210-9
    .default = NAME_2
  ))

anti_join(kenya22gps |> st_drop_geometry(), kenya22_basemap2, by = "NAME_2") # 1 before the mutate just above # To check the gps points that are not placed in any entity from the basemap
anti_join(kenya22_basemap2, kenya22gps |> st_drop_geometry(), by = "NAME_2") # 6 # To check the map entities that don't have any gps inside them


# malawi16

malawi16gps <- st_join(malawi16gps, malawi16_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

malawi16gps <- malawi16gps |> 
  mutate(NAME_1 = case_when(
    is.na(NAME_1) & ADM1NAME == "North" ~ "Likoma", # Found here https://spatialdata.dhsprogram.com/boundaries/#level=2&surveyId=483&countryId=MW&view=map
    is.na(NAME_1) & ADM1NAME == "South" ~ "Zomba", # Idem
    .default = NAME_1
  ))

anti_join(malawi16gps |> st_drop_geometry(), malawi16_basemap2, by = "NAME_1") # 2 before the mutate just above # To check the gps points that are not placed in any entity from the basemap
anti_join(malawi16_basemap2, malawi16gps |> st_drop_geometry(), by = "NAME_1") # 0 # To check the map entities that don't have any gps inside them


# mali18

mali18gps <- st_join(mali18gps, mali18_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

anti_join(mali18gps |> st_drop_geometry(), mali18_basemap2, by = "NAME_2") # We leave as NA the Name_2 variable for the 17 GPS clusters for which coordinates are not available (0,0 in the point geometry variable): this article does the same https://www.tandfonline.com/doi/full/10.1080/00324728.2023.2181383 "GPS coordinates are not available for 17 DHS clusters, so this analysis is limited to 328 unique clusters" # To check the gps points that are not placed in any entity from the basemap
anti_join(mali18_basemap2, mali18gps |> st_drop_geometry(), by = "NAME_2") # 7 # To check the map entities that don't have any gps inside them


# maurit19

maurit19gps <- st_join(maurit19gps, maurit19_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

maurit19gps <- maurit19gps |> 
  mutate(NAME_2 = case_when(
    is.na(NAME_2) & ADM1NAME == "Dakhlet Nouadhibou" ~ "Nouadhibou", # We checked that the GPS points with NAME_2 == "Nouadhibou" were in the same area
    .default = NAME_2
  ))

anti_join(maurit19gps |> st_drop_geometry(), maurit19_basemap2, by = "NAME_2") # 1 before the mutate just above + We leave as NA the NAME_2 variable for the 2 GPS clusters for which coordinates are not available (0,0 in the point geometry variable) # To check the gps points that are not placed in any entity from the basemap
anti_join(maurit19_basemap2, maurit19gps |> st_drop_geometry(), by = "NAME_2") # 1 (the tiny department of Zouerate) # To check the map entities that don't have any gps inside them


# mozam22

mozam22gps <- st_join(mozam22gps, mozam22_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)


mozam22gps <- mozam22gps |> 
  mutate(NAME_2 = case_when(
    is.na(NAME_2) ~ "Namaacha", # We checked that the GPS points with NAME_2 == "Namaacha" were in the same area
    .default = NAME_2
  ))

anti_join(mozam22gps |> st_drop_geometry(), mozam22_basemap2, by = "NAME_2") # 1 before the mutate just above # To check the gps points that are not placed in any entity from the basemap
anti_join(mozam22_basemap2, mozam22gps |> st_drop_geometry(), by = "NAME_2") # 11 # To check the map entities that don't have any gps inside them


# nigeria18

nigeria18gps <- st_join(nigeria18gps, nigeria18_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

nigeria18gps <- nigeria18gps |> 
  mutate(NAME_2 = case_when(
    is.na(NAME_2) & DHSCLUST == 1126 ~ "Asari-Toru", # Cluster located in the city of Buguma: we found its location by using tighter coordinates when creating maps, combined with looking at administrative maps of Nigeria (like https://maps-nigeria.com/map-of-rivers-state-nigeria) and google maps, and finally we looked at the coordinates on our created maps to find out the correct GPS cluster out of the two possible
    is.na(NAME_2) & DHSCLUST == 1132 ~ "Port Harcourt", # Cluster located in the city of Abuloma: same process as just above
    .default = NAME_2
  ))

# This map confirms the location of the 2 unmatched points, by displayed them inside their LGA, and along the other GPS points in their two LGA (Asari-Toru and Port Harcourt)
# ggplot() +
#   geom_sf(data = nigeria18_basemap2, fill = "lightgray", color = "darkgray") +  # Admin2 boundaries
#   geom_sf(data = nigeria18_basemap2 |> filter(NAME_2 == "Asari-Toru" | NAME_2 == "Port Harcourt"), color = "red", size = 2) +
#   geom_sf_text(data = nigeria18_basemap2 |> filter(NAME_2 == "Asari-Toru" | NAME_2 == "Port Harcourt"), aes(label = NAME_2), size = 3, color = "red") +
#   geom_sf(data = nigeria18gps |> filter(is.na(NAME_2)), color = "blue", size = 2) +
#   geom_sf(data = nigeria18gps |> filter(NAME_2 == "Asari-Toru" | NAME_2 == "Port Harcourt"), color = "green", size = 2) +
#   coord_sf(xlim = c(6.8, 7.2), ylim = c(4.5, 4.8), expand = FALSE) +
#   theme_minimal()

anti_join(nigeria18gps |> st_drop_geometry(), nigeria18_basemap2, by = "NAME_2") # # 2 before the mutate just above + We leave as NA the NAME_2 variable for the 7 GPS clusters for which coordinates are not available (0,0 in the point geometry variable) # To check the gps points that are not placed in any entity from the basemap
anti_join(nigeria18_basemap2, nigeria18gps |> st_drop_geometry(), by = "NAME_2") # 139 # To check the map entities that don't have any gps inside them

# rwanda19

rwanda19gps <- st_read("data/dhs/gps_datasets/RWGE81FL/RWGE81FL.shp")
rwanda19gps <- st_join(rwanda19gps, rwanda19_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)


rwanda19gps <- rwanda19gps |> 
  mutate(NAME_2 = case_when(
    is.na(NAME_2) ~ "Burera", # The 2 concerned GPS are just outside Burera border (out of the country), so no ambiguities.
    .default = NAME_2
  ))

anti_join(rwanda19gps |> st_drop_geometry(), rwanda19_basemap2, by = "NAME_2") # 2 before the mutate just above # To check the gps points that are not placed in any entity from the basemap
anti_join(rwanda19_basemap2, rwanda19gps |> st_drop_geometry(), by = "NAME_2") # 0 # To check the map entities that don't have any gps inside them


# senegal18

senegal18gps <- st_join(senegal18gps, senegal18_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

anti_join(senegal18gps |> st_drop_geometry(), senegal18_basemap2, by = "NAME_2") # 0 # To check the gps points that are not placed in any entity from the basemap
anti_join(senegal18_basemap2, senegal18gps |> st_drop_geometry(), by = "NAME_2") # 0 # To check the map entities that don't have any gps inside them

# senegal19

senegal19gps <- st_join(senegal19gps, senegal19_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

anti_join(senegal19gps |> st_drop_geometry(), senegal19_basemap2, by = "NAME_2") # 0 # To check the gps points that are not placed in any entity from the basemap
anti_join(senegal19_basemap2, senegal19gps |> st_drop_geometry(), by = "NAME_2") # 0 # To check the map entities that don't have any gps inside them


# safrica16

safrica16gps <- st_join(safrica16gps, safrica16_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

anti_join(safrica16gps |> st_drop_geometry(), safrica16_basemap2, by = "NAME_2") # 0 # To check the gps points that are not placed in any entity from the basemap
anti_join(safrica16_basemap2, safrica16gps |> st_drop_geometry(), by = "NAME_2") # 1 (the district of Central Karoo) # To check the map entities that don't have any gps inside them


# tanza22

tanza22gps <- st_join(tanza22gps, tanza22_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

anti_join(tanza22gps |> st_drop_geometry(), tanza22_basemap2, by = "NAME_2") # 0 # To check the gps points that are not placed in any entity from the basemap
anti_join(tanza22_basemap2, tanza22gps |> st_drop_geometry(), by = "NAME_2") # 17 # To check the map entities that don't have any gps inside them

# uganda16

uganda16gps <- st_join(uganda16gps, uganda16_basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

anti_join(uganda16gps |> st_drop_geometry(), uganda16_basemap2, by = "NAME_1") # We leave as NA the NAME_1 variable for the 11 GPS clusters for which coordinates are not available (0,0 in the point geometry variable) # To check the gps points that are not placed in any entity from the basemap
anti_join(uganda16_basemap2, uganda16gps |> st_drop_geometry(), by = "NAME_1") # 2 (Lake Albert and Lake Victoria that are 2 water bodies) # To check the map entities that don't have any gps inside them

# # Code below to visualize unmatched gps points, and then to visualize the areas without gps clusters inside them (just change country name to visualize the desired country)
# ggplot() +
#   geom_sf(data = malawi16_basemap2, fill = "lightgray", color = "darkgray") +  # Admin2 boundaries
#   geom_sf(data = malawi16gps |> filter(is.na(NAME_2)), color = "red", size = 2) +  # Unmatched points
#   labs(title = "Unmatched GPS Points",
#        subtitle = "Points without admin2 region association") +
#   theme_minimal()
# 
# ggplot() +
#   geom_sf(data = kenya22_basemap2, fill = "lightgray", color = "darkgray") +  # Admin2 boundaries
#   geom_sf(data = anti_join(kenya22_basemap2, kenya22gps |> st_drop_geometry(), by = "NAME_2"), color = "red", size = 2) +  # Unmatched admin 2
#   geom_sf_text(data = anti_join(kenya22_basemap2, kenya22gps |> st_drop_geometry(), by = "NAME_2"), aes(label = NAME_2), size = 3, color = "red") +
#   labs(title = "Unmatched admin2",
#        subtitle = "admin2 regions without gps observation inside them") +
#   theme_minimal()


## 3.2) Merging the completed gps datasets with the dhs survey datasets ----

kenya22 <- left_join(kenya22, kenya22gps, by = join_by(cluster_number == DHSCLUST)) # Maybe relevant: |> st_as_sf()
anti_join(kenya22, kenya22gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

malawi16 <- left_join(malawi16, malawi16gps, by = join_by(cluster_number == DHSCLUST))
anti_join(malawi16, malawi16gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

mali18 <- left_join(mali18, mali18gps, by = join_by(cluster_number == DHSCLUST))
anti_join(mali18, mali18gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

maurit19 <- left_join(maurit19, maurit19gps, by = join_by(cluster_number == DHSCLUST))
anti_join(maurit19, maurit19gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

mozam22 <- left_join(mozam22, mozam22gps, by = join_by(cluster_number == DHSCLUST))
anti_join(mozam22, mozam22gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

nigeria18 <- left_join(nigeria18, nigeria18gps, by = join_by(cluster_number == DHSCLUST))
anti_join(nigeria18, nigeria18gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

#pakis17gps <- st_read("data/gps_datasets/PKGE71FL/PKGE71FL.shp")
#pakis17 <- left_join(pakis17, pakis17gps, by = join_by(cluster_number == DHSCLUST))
#anti_join(pakis17, pakis17gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

rwanda19 <- left_join(rwanda19, rwanda19gps, by = join_by(cluster_number == DHSCLUST))
anti_join(rwanda19, rwanda19gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

senegal18 <- left_join(senegal18, senegal18gps, by = join_by(cluster_number == DHSCLUST))
anti_join(senegal18, senegal18gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

senegal19 <- left_join(senegal19, senegal19gps, by = join_by(cluster_number == DHSCLUST))
anti_join(senegal19, senegal19gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

safrica16 <- left_join(safrica16, safrica16gps, by = join_by(cluster_number == DHSCLUST))
anti_join(safrica16, safrica16gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

tanza22 <- left_join(tanza22, tanza22gps, by = join_by(cluster_number == DHSCLUST))
anti_join(tanza22, tanza22gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join

uganda16 <- left_join(uganda16, uganda16gps, by = join_by(cluster_number == DHSCLUST))
anti_join(uganda16, uganda16gps, by = join_by(cluster_number == DHSCLUST)) # This should have 0 rows if all rows are matched in the left_join


## 3.3) Creation of homogenous admin variables ----
# This is the variable that goes into the group_by() when we create the prevalence tables
# "admin1" corresponds to the largest possible administrative subdivision
# "admin2" corresponds to the second largest possible administrative subdivision
# "admin3" corresponds to the third largest possible administrative subdivision

kenya22 <- kenya22 |> mutate(admin1 = ADM1NAME, admin2 = NAME_2)  # We choose ADM1NAME rather than NAME_1 because ADM1NAME is given without missing data for each cluster (so for each individual), while we don't have the same insurance with NAME_1 (NAME_1 is attached to a cluster with the st_join() function, so more prone to errors or gps outside an admin 2, which means also outside an admin1) # DELETE WHAT FOLLOW # 15/11 JE PEUX METTRE NAME_1 MAIS DANS CE CAS IL FAUT REMPLACER LE CAS NA PAR SA VALEUR DANS LE MUTATE DE KENYA EN SECTION 4

malawi16 <- malawi16 |> mutate(
  admin1 = DHSREGNA,
  # For admin2: We don't use the NAME_1 variable (matched with each cluster then each observation using st_join()) because the strata variable already has the same information ; but using the NAME_1 variable would be similar
  districts_28 = str_remove_all(strata, " - rural| - urban"), # To remove " - rural" and " - urban"
  districts_28 = str_to_title(districts_28), # To capitalize first letter of each word
  districts_28 = case_when(
    districts_28 == "Nkhatabay" ~ "Nkhata Bay",
    districts_28 == "Nkhota Kota" ~ "Nkhotakota",
    .default = districts_28
  ),
  admin2 = districts_28
)

mali18 <- mali18 |> mutate(
  ADM1NAME_cleaned = str_to_title(ADM1NAME), # To capitalize first letter of each word
  ADM1NAME_cleaned = case_when(ADM1NAME_cleaned == "Segou" ~ "Ségou",
    ADM1NAME_cleaned == "Toumbouctou" ~ "Timbuktu",
    .default = ADM1NAME_cleaned
  ),
  admin1 = ADM1NAME_cleaned,
  admin2 = NAME_2
)


maurit19 <- maurit19 |> mutate(
  ADM1NAME_cleaned = case_when(
    ADM1NAME == "Guidimagha" ~ "Guidimaka",
    ADM1NAME == "Hodh Echargui" ~ "Hodh ech Chargui",
    ADM1NAME == "Hodh Gharbi" ~ "Hodh el Gharbi",
    ADM1NAME == "Nouakchott Nord" ~ "Nouakchott",
    ADM1NAME == "Nouakchott Ouest" ~ "Nouakchott",
    ADM1NAME == "Nouakchott Sud" ~ "Nouakchott",
    ADM1NAME == "Tiris Zemour Et Inchiri" ~ "Tiris Zemmour", # /!\ Tiris Zemour and Inchiri are separate regions (so they are different units in basemaps) but were grouped in the survey, so we label the values "Tiris Zemour Et Inchiri" as only "Tiris Zemmour", and later when we create the prevalence tibble at this admin level, we duplicate the Tiris Zemmour row and put its value in the Inchiri row
    .default = ADM1NAME
  ),
  admin1 = ADM1NAME_cleaned,
  admin2 = NAME_2
)

mozam22 <- mozam22 |> mutate(
  ADM1NAME_cleaned = case_when(
    ADM1NAME == "Cidade de Maputo" ~ "Maputo City",
    ADM1NAME == "Maputo Provincia" ~ "Maputo",
    .default = ADM1NAME
  ),
  admin1 = ADM1NAME_cleaned,
  admin2 = NAME_2
)

nigeria18 <- nigeria18 |> mutate(
  admin1 = DHSREGNA,
  ADM1NAME_cleaned = str_to_title(ADM1NAME), # To capitalize first letter of each word
  ADM1NAME_cleaned = case_when(
    ADM1NAME_cleaned == "Fct Abuja" ~ "Federal Capital Territory",
    .default = ADM1NAME_cleaned
  ),
  admin2 = ADM1NAME_cleaned,
  admin3 = NAME_2
)

rwanda19 <- rwanda19 |> mutate(
  ADM1NAME_cleaned = case_when(
    ADM1NAME == "East" ~ "Eastern Province",
    ADM1NAME == "Kigali" ~ "Kigali City",
    ADM1NAME == "North" ~ "Northern Province",
    ADM1NAME == "South" ~ "Southern Province",
    ADM1NAME == "West" ~ "Western Province",
    .default = ADM1NAME
  ),
  admin1 = ADM1NAME_cleaned,
  # For admin2: We don't use the NAME_2 variable (matched with each cluster then each observation using st_join()) because the strata variable already has the same information ; but using the NAME_2 variable would be similar
  district_cleaned = str_to_title(district), # To capitalize first letter of each word
  admin2 = district_cleaned
)

senegal18 <- senegal18 |> mutate(admin1 = ADM1NAME, admin2 = NAME_2)

senegal19 <- senegal19 |> mutate(admin1 = ADM1NAME, admin2 = NAME_2)

safrica16 <- safrica16 |> mutate(admin1 = ADM1NAME, admin2 = NAME_2)

tanza22 <- tanza22 |> mutate(admin1 = ADM1NAME, admin2 = NAME_2)

uganda16 <- uganda16 |> mutate(admin1 = DHSREGNA, admin2 = NAME_1)

# 4) Merging all the recoded DHS datasets with adults and children, and in SSA ----

dta_dhs <- bind_rows(kenya22, mali18, maurit19, mozam22, nigeria18, rwanda19, senegal18, senegal19, safrica16, tanza22, uganda16)  |>
          select(country, year, cluster_number, wt, psu, strata, region, type_place_residence, de_facto_resident, #de_jure_resident,
                 age, age_groups, gender, marital_status, wealth_quintile, education_level_ever, 
                 starts_with("diff"), starts_with("at_least") , starts_with("long_short_quest"), starts_with("NAME_"), 
                 admin1, admin2, admin3, geometry, DATUM, ALT_GPS,ALT_DEM
                 ) |> # We keep only the columns we need for the SAE analysis
          mutate(survey="DHS") 


# 5) Survey design (and extraction of the "working" subsample) ----
# Note 16/1 : We didn't check exactly the number of NA answers that were left in the disability variables after changing the "don't know" into NA in the "diff_..." variables: maybe we should systematically filter out all individuals with at least one NA in any disability variable (from what we have seen it doesn't seem to exclude many more people compared to excluding observations with NA in only one disability country, as we had already done for some countries)

# The following page advises to create the survey object after having done all the recoding, as well as the conversion of labelled vectors to factors https://larmarange.github.io/analyse-R/definir-un-plan-d-echantillonnage-complexe.html (or here https://larmarange.github.io/guide-R/donnees_ponderees/manipulation.html)
# Also, the survey design has to be defined on the entire dataset, then it's possible to subset() (not filter() that doesn't work on survey objects) the dataset (see Webin-R 10 41min30) (also here https://www.reddit.com/r/statistics/comments/bvahag/why_it_is_important_to_make_survey_design_object/)

# Finally I don't use the survey() package, but the srvyr() package, as do larmarange and simo fotso in their rmarkdown
# kenya22wt <- svydesign(ids = kenya22$psu, data = kenya22, strata = kenya22$strata, weights = kenya22$wt, nest = TRUE) # "wt" for "weighted" (I think this comes from DHS instructions)
# options(survey.lonely.psu = "adjust") # Added for the case where there may be a single PSU in a strata, as indicated here https://www.youtube.com/watch?v=pJwd2-m3QBY

options(survey.lonely.psu = "adjust") # Added for the case where there may be a single PSU in a strata, as indicated here https://www.youtube.com/watch?v=pJwd2-m3QBY


kenya22wt <- kenya22 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE) # psu for Primary Sampling Units, strata for Strata variable, weights for Weight variable, nest To handle nesting
kenya22wt <- kenya22wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes", long_short_quest == "long questionnaire") # Before it was "kenya22wt_de_facto_five_and_over" ; the condition age_groups != "0-4" does (voluntarily) include the respondants without age info)
kenya22wt <- kenya22wt |> srvyr::filter(age >= 15 & age <= 49) # Note 15/1/25: We keep population 15-49 years old to have the same population as in MICS surveys (we also checked that before the creation of the survey object all countries had anwsers from 0 to 95 to the age variable, as well as a few NAs for the age variable, that are filtered out here)

malawi16wt <- malawi16 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
malawi16wt <- malawi16wt |> srvyr::filter(age_groups != "0-4", de_jure_resident == "yes", age < 18) |> srvyr::filter(age_groups == "10-14" | age_groups == "15-19") |> srvyr::filter(!is.na(diff_hear) & !is.na(diff_com) & !is.na(diff_remem) & !is.na(diff_see)) # In Malawi 2016 Final Report disability tables like Table 2.14.2, frequencies are based on de jure children 10-17 (i.e. hv102 == "yes") but we keep de_facto_resident == "yes" for comparability with the other surveys (even if in malawi16 the population is 10-17 years old) ; also we remove the observations with NA for questions on hear, com or remem (they are not necessarily the same) (Note 31/1: we add "& !is.na(diff_see)" not to get NA for the prevalence table for seeing for the Central region in admin1 and for 6 units in admin2)
# We keep population 10-17 for Malawi because the disability questions are asked only to them and not adults

mali18wt <- mali18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
mali18wt <- mali18wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes")
mali18wt <- mali18wt |> srvyr::filter(age >= 15 & age <= 49)

maurit19wt <- maurit19 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
maurit19wt <- maurit19wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes")
maurit19wt <- maurit19wt |> srvyr::filter(age >= 15 & age <= 49)

mozam22wt <- mozam22 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
mozam22wt <- mozam22wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes", !is.na(diff_see), !is.na(diff_hear), !is.na(diff_com), !is.na(diff_remem), !is.na(diff_walk), !is.na(diff_wash)) # According to Table p.45 in Mozambique DHS Final Report, only a subsample (Subsample A) was asked the WG-SS ; I didn't find a subsample differenciation variable ; but all the disability questions used here had the same number of NA (38 530) so I used a condition all of them, and we get the same Number of 15+ respondants (16 260) as in the Final Report Quadro 20.1 p.546 
mozam22wt <- mozam22wt |> srvyr::filter(age >= 15 & age <= 49)

nigeria18wt <- nigeria18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
nigeria18wt <- nigeria18wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes") |> srvyr::filter(!is.na(diff_see) | !is.na(diff_hear) | !is.na(diff_com) | !is.na(diff_remem) | !is.na(diff_walk) | !is.na(diff_wash)) # According to Figure 1.1 in Final Report Nigeria 2018: 1/3 of households (14,000) are excluded from the disability module (see filter for mozam22 just above for justification of the filter)
nigeria18wt <- nigeria18wt |> srvyr::filter(age >= 15 & age <= 49)

#pakis17wt <- pakis17 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
#pakis17wt <- pakis17wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes") |> srvyr::filter(!is.na(diff_see) & !is.na(diff_hear) & !is.na(diff_com) & !is.na(diff_remem) & !is.na(diff_walk) & !is.na(diff_wash)) # Between 38 and 80 NA depeding on the disability question, here all questions have to be answered for the observation to be kept

rwanda19wt <- rwanda19 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
rwanda19wt <- rwanda19wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes")
rwanda19wt <- rwanda19wt |> srvyr::filter(age >= 15 & age <= 49)

senegal18wt <- senegal18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
senegal18wt <- senegal18wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes") |> srvyr::filter(!is.na(diff_see), !is.na(diff_hear), !is.na(diff_com), !is.na(diff_remem), !is.na(diff_walk), !is.na(diff_wash))
senegal18wt <- senegal18wt |> srvyr::filter(age >= 15 & age <= 49)

senegal19wt <- senegal19 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
senegal19wt <- senegal19wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes") |> srvyr::filter(!is.na(diff_see), !is.na(diff_hear), !is.na(diff_com), !is.na(diff_remem), !is.na(diff_walk), !is.na(diff_wash)) # Previous (as of 16/1/25) note: this filter condition !is.na(diff_see) drops 2 observations for which only diff_see was NA (not the other disability variables), the observations belonged to the Tambacounda (1) and Kolda (1) regions
senegal19wt <- senegal19wt |> srvyr::filter(age >= 15 & age <= 49)

safrica16wt <- safrica16 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
safrica16wt <- safrica16wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes") |> srvyr::filter(!is.na(diff_see) | !is.na(diff_hear) | !is.na(diff_com) | !is.na(diff_remem) | !is.na(diff_walk) | !is.na(diff_wash)) # Only one observation has NA values (for all disability questions) after the age_groups and de_facto_resident filter)
safrica16wt <- safrica16wt |> srvyr::filter(age >= 15 & age <= 49)

tanza22wt <- tanza22 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
tanza22wt <- tanza22wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes") |> srvyr::filter(!is.na(diff_see) | !is.na(diff_hear) | !is.na(diff_com) | !is.na(diff_remem) | !is.na(diff_walk) | !is.na(diff_wash)) # Only one observation has NA values (for all disability questions) after the age_groups and de_facto_resident filter)
tanza22wt <- tanza22wt |> srvyr::filter(age >= 15 & age <= 49)

uganda16wt <- uganda16 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
uganda16wt <- uganda16wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes") |> srvyr::filter(!is.na(diff_see)) # this filter condition !is.na(diff_see) drops 20 observations for which only diff_see was NA (not the other disability variables)
uganda16wt <- uganda16wt |> srvyr::filter(age >= 15 & age <= 49)

# Maybe here add a count of the number of NA for the disability variables for each country



# 6) Creation of prevalence tables ----
# At the levels Admin 0 (country), then Admin 1 (e.g. region), then when available Admin 2 (e.g. district)
# + joining the basemaps and the prevalences at the same level


## 6.A) kenya22 ----

kenya22_prev_admin0 <- kenya22wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )


kenya22_prev_admin1 <- kenya22wt |> 
  srvyr::group_by(admin1) |> # Counties
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

kenya22_prev_admin1 <- left_join(kenya22_basemap1, kenya22_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(kenya22_basemap1, kenya22_prev_admin1, by = join_by(NAME_1 == admin1))

kenya22_prev_admin2 <- kenya22wt |> 
  srvyr::group_by(admin2) |> # Sub-counties
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

kenya22_prev_admin2 <- left_join(kenya22_basemap2, kenya22_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match (except the 6 without gps observations inside them) : anti_join(kenya22_basemap2, kenya22_prev_admin2, by = join_by(NAME_2 == admin2))


## 6.B) malawi16 ----

malawi16_prev_admin0 <- malawi16wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

malawi16_prev_admin1 <- malawi16wt |> 
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

malawi16_prev_admin1 <- left_join(malawi16_basemap1, malawi16_prev_admin1, by = join_by(REGNAME == admin1)) # Check that all names match : anti_join(malawi16_basemap1, malawi16_prev_admin1, by = join_by(REGNAME == admin1))

malawi16_prev_admin2 <- malawi16wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

malawi16_prev_admin2 <- left_join(malawi16_basemap2, malawi16_prev_admin2, by = join_by(NAME_1 == admin2)) # Check that all names match : anti_join(malawi16_basemap2, malawi16_prev_admin2, by = join_by(NAME_1 == admin2))

## 6.C) mali18 ----

mali18_prev_admin0 <- mali18wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

mali18_prev_admin1 <- mali18wt |> 
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

mali18_prev_admin1 <- left_join(mali18_basemap1, mali18_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(mali18_basemap1, mali18_prev_admin1, by = join_by(NAME_1 == admin1))

mali18_prev_admin2 <- mali18wt |> 
  srvyr::group_by(admin2) |> # Cercles
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

mali18_prev_admin2 <- left_join(mali18_basemap2, mali18_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match (except the 7 without gps observations inside them) : anti_join(mali18_basemap2, mali18_prev_admin2, by = join_by(NAME_2 == admin2))


## 6.D) maurit19 ----

maurit19_prev_admin0 <- maurit19wt |> 
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

maurit19_prev_admin1 <- maurit19wt |>
  srvyr::group_by(admin1) |> # Regions (Wilayat)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  ) |> ungroup() %>% # only this type of pipe works here
  bind_rows( # this part is to "duplicate" the Tiris Zemour row and put its values in the Inchiri row, since they're separate regions but were grouped in the survey
    as_tibble(
      filter(., admin1 == "Tiris Zemmour") |> 
        mutate(admin1 = "Inchiri")
    )
  )

maurit19_prev_admin1 <- left_join(maurit19_basemap1, maurit19_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(maurit19_basemap1, maurit19_prev_admin1, by = join_by(NAME_1 == admin1))

maurit19_prev_admin2 <- maurit19wt |>
  srvyr::group_by(admin2) |> # Departments (Moughataas)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

maurit19_prev_admin2 <- left_join(maurit19_basemap2, maurit19_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match (except the 1 without gps observations inside it) : anti_join(maurit19_basemap2, maurit19_prev_admin2, by = join_by(NAME_2 == admin2))

#ggplot(maurit19_prev_admin1) + geom_sf(aes(fill = see_prev)) + labs(caption = "The regions Tiris Zemmour and Inchiri of Mauritania were grouped into \n1 region in the DHS survey (which means that \nthey have the same prevalence value)")



## 6.E) mozam22 ----

mozam22_prev_admin0 <- mozam22wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

mozam22_prev_admin1 <- mozam22wt |>
  srvyr::group_by(admin1) |> # Provinces
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

mozam22_prev_admin1 <- mozam22_basemap1 |> mutate(NAME_1 = str_replace(NAME_1, "Nassa", "Niassa")) |>  left_join(mozam22_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : mozam22_basemap1 |> mutate(NAME_1 = str_replace(NAME_1, "Nassa", "Niassa")) |> anti_join(mozam22_prev_admin1, by = join_by(NAME_1 == admin1)) # I just changed "Nassa" to "Niassa" in the basemap1


mozam22_prev_admin2 <- mozam22wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

mozam22_prev_admin2 <- left_join(mozam22_basemap2, mozam22_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match (except the 11 without gps observations inside them) : anti_join( mozam22_basemap2, mozam22_prev_admin2, by = join_by(NAME_2 == admin2))


## 6.F) nigeria18 ----

nigeria18_prev_admin0 <- nigeria18wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

nigeria18_prev_admin1 <- nigeria18wt |>
  srvyr::group_by(admin1) |> # Geopolitical zones
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

nigeria18_prev_admin1 <- left_join(nigeria18_basemap1pre, nigeria18_prev_admin1, by = join_by(REGNAME == admin1)) # Check that all names match : anti_join(nigeria18_basemap1pre, nigeria18_prev_admin1, by = join_by(REGNAME == admin1))

nigeria18_prev_admin2 <- nigeria18wt |>
  srvyr::group_by(admin2) |> # States
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

nigeria18_prev_admin2 <- left_join(nigeria18_basemap1, nigeria18_prev_admin2, by = join_by(NAME_1 == admin2)) # Check that all names match : anti_join(nigeria18_basemap1, nigeria18_prev_admin2, by = join_by(NAME_1 == admin2))


nigeria18_prev_admin3 <- nigeria18wt |>
  srvyr::group_by(admin3) |> # Local government areas
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

nigeria18_prev_admin3 <- left_join(nigeria18_basemap2, nigeria18_prev_admin3, by = join_by(NAME_2 == admin3)) # Check that all names match (except the 139 without gps observations inside them) : anti_join(nigeria18_basemap2, nigeria18_prev_admin3, by = join_by(NAME_2 == admin3))


## 6.G) pakis17 ----

# pakis17_prev_admin0 <- pakis17wt |>
#   srvyr::summarize(
#     see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
#     hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
#     com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
#     remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
#     walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
#     wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
#   )
# 
# pakis17_prev_admin1 <- pakis17wt |>
#   srvyr::mutate(
#     ADM1NAME_cleaned = str_to_title(ADM1NAME), # To capitalize first letter of each word
#     ADM1NAME_cleaned = case_when(
#       ADM1NAME_cleaned == "Azad Jammu And Kashmir" ~ "Azad Kashmir",
#       ADM1NAME_cleaned == "Federal Capital Territory" ~ "Islamabad",
#       ADM1NAME_cleaned == "Federally Administered Tribal Areas" ~ "Federally Administered Tribal Ar",
#       ADM1NAME_cleaned == "Gilgit Baltistan" ~ "Gilgit-Baltistan",
#       ADM1NAME_cleaned == "Khyber Pakhtunkhwa" ~ "Khyber-Pakhtunkhwa",
#       .default = ADM1NAME_cleaned
#     )
#   ) |>
#   srvyr::group_by(ADM1NAME_cleaned) |>
#   srvyr::summarize(
#     see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
#     hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
#     com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
#     remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
#     walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
#     wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
#   ) # I'm not sure exactly how it materializes in this code, but the fact that we find NaN values for Azad Jammu and Kashmir and Gilgit Baltistan (which is not a problem to let like this, since for now we won't need Pakistan but only SSA countries) is coherent with the fact that it's always separated from other regions in the DHS Final Report (most likely it's located in the weighting variables, since I managed to compute prevalences for all regions before doing the weighting, but after the weighting the group_by() |> summarise(prev = survey_mean(var)) doesn't work)
# 
# pakis17_prev_admin1 <- left_join(pakis17_basemap1, pakis17_prev_admin1, by = join_by(NAME_1 == ADM1NAME_cleaned)) # Check that all names match : anti_join(pakis17_basemap1, pakis17_prev_admin1, by = join_by(NAME_1 == ADM1NAME_cleaned))
# # It is possible to create a prevalence map by joining the hdist variable from parkis17 to the NAME_1 from the basemap3 (not basemap2) but we don't do it now, since for now we won't need Pakistan but only SSA countries

## 6.H) rwanda19 ----

rwanda19_prev_admin0 <- rwanda19wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

rwanda19_basemap1 <- rwanda19_basemap1 |> 
  mutate(
    NAME_1_en = case_when(
      NAME_1 == "Amajyaruguru" ~ "Northern Province",
      NAME_1 == "Amajyepfo" ~ "Southern Province",
      NAME_1 == "Iburasirazuba" ~ "Eastern Province",
      NAME_1 == "Iburengerazuba" ~ "Western Province",
      NAME_1 == "Umujyi wa Kigali" ~ "Kigali City",
      .default = NAME_1
    )
  )

rwanda19_prev_admin1 <- rwanda19wt |>
  srvyr::group_by(admin1) |> # Provinces (Intara)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  ) 

rwanda19_prev_admin1 <- left_join(rwanda19_basemap1, rwanda19_prev_admin1, by = join_by(NAME_1_en == admin1)) # Check that all names match : anti_join(rwanda19_basemap1, rwanda19_prev_admin1, by = join_by(NAME_1_en == admin1))

rwanda19_prev_admin2 <- rwanda19wt |>
  srvyr::group_by(admin2) |> # Districts (Uturere)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  ) 

rwanda19_prev_admin2 <- left_join(rwanda19_basemap2, rwanda19_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match : anti_join(rwanda19_basemap2, rwanda19_prev_admin2, by = join_by(NAME_2 == admin2))


## 6.I18) senegal18 ----

senegal18_prev_admin0 <- senegal18wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )


senegal18_basemap1 <- senegal18_basemap1 |> 
  mutate(
    NAME_1_en = case_when(
      NAME_1 == "Kédougou" ~ "Kedougou",
      NAME_1 == "Sédhiou" ~ "Sedhiou",
      NAME_1 == "Thiès" ~ "Thies",
      .default = NAME_1
    )
  )

senegal18_prev_admin1 <- senegal18wt |>
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

senegal18_prev_admin1 <- left_join(senegal18_basemap1, senegal18_prev_admin1, by = join_by(NAME_1_en == admin1)) # Check that all names match : anti_join(senegal18_basemap1, senegal18_prev_admin1, by = join_by(NAME_1_en == admin1))


senegal18_prev_admin2 <- senegal18wt |>
  srvyr::group_by(admin2) |> # Departments
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

senegal18_prev_admin2 <- left_join(senegal18_basemap2, senegal18_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match : anti_join(senegal18_basemap2, senegal18_prev_admin2, by = join_by(NAME_2 == admin2))


## 6.I) senegal19 ----

senegal19_prev_admin0 <- senegal19wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )


senegal19_basemap1 <- senegal19_basemap1 |> 
  mutate(
    NAME_1_en = case_when(
      NAME_1 == "Kédougou" ~ "Kedougou",
      NAME_1 == "Sédhiou" ~ "Sedhiou",
      NAME_1 == "Thiès" ~ "Thies",
      .default = NAME_1
    )
  )

senegal19_prev_admin1 <- senegal19wt |>
  srvyr::group_by(admin1) |> # Regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

senegal19_prev_admin1 <- left_join(senegal19_basemap1, senegal19_prev_admin1, by = join_by(NAME_1_en == admin1)) # Check that all names match : anti_join(senegal19_basemap1, senegal19_prev_admin1, by = join_by(NAME_1_en == admin1))


senegal19_prev_admin2 <- senegal19wt |>
  srvyr::group_by(admin2) |> # Departments
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

senegal19_prev_admin2 <- left_join(senegal19_basemap2, senegal19_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match : anti_join(senegal19_basemap2, senegal19_prev_admin2, by = join_by(NAME_2 == admin2))


## 6.J) safrica16 ----

safrica16_prev_admin0 <- safrica16wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

safrica16_prev_admin1 <- safrica16wt |>
  srvyr::group_by(admin1) |> # Provinces
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

safrica16_prev_admin1 <- left_join(safrica16_basemap1, safrica16_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(safrica16_basemap1, safrica16_prev_admin1, by = join_by(NAME_1 == admin1))

safrica16_prev_admin2 <- safrica16wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

safrica16_prev_admin2 <- left_join(safrica16_basemap2, safrica16_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match  (except the 1 without gps observations inside it) : anti_join(safrica16_basemap2, safrica16_prev_admin2, by = join_by(NAME_2 == admin2))



## 6.K) tanza22 ----

tanza22_prev_admin0 <- tanza22wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

tanza22_prev_admin1 <- tanza22wt |>
  srvyr::group_by(admin1) |> # Regions (Mikoa)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

tanza22_prev_admin1 <- left_join(tanza22_basemap1, tanza22_prev_admin1, by = join_by(NAME_1 == admin1)) # Check that all names match : anti_join(tanza22_basemap1, tanza22_prev_admin1, by = join_by(NAME_1 == admin1))


tanza22_prev_admin2 <- tanza22wt |>
  srvyr::group_by(admin2) |> # Districts (Wilaya)
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

tanza22_prev_admin2 <- left_join(tanza22_basemap2, tanza22_prev_admin2, by = join_by(NAME_2 == admin2)) # Check that all names match : anti_join(tanza22_basemap2, tanza22_prev_admin2, by = join_by(NAME_2 == admin2))


## 6.L) uganda16 ----

uganda16_prev_admin0 <- uganda16wt |>
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

uganda16_prev_admin1 <- uganda16wt |>
  srvyr::group_by(admin1) |> # Sub-regions
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

uganda16_prev_admin1 <- left_join(uganda16_basemap1, uganda16_prev_admin1, by = join_by(REGNAME == admin1)) # Check that all names match : anti_join(uganda16_basemap1, uganda16_prev_admin1, by = join_by(REGNAME == admin1))


uganda16_prev_admin2 <- uganda16wt |>
  srvyr::group_by(admin2) |> # Districts
  srvyr::summarize(
    see_prev = survey_mean(if_else(at_least_alotof_diff_see == "yes", 1, 0)),
    hear_prev = survey_mean(if_else(at_least_alotof_diff_hear == "yes", 1, 0)),
    com_prev = survey_mean(if_else(at_least_alotof_diff_com == "yes", 1, 0)),
    remem_prev = survey_mean(if_else(at_least_alotof_diff_remem == "yes", 1, 0)),
    walk_prev = survey_mean(if_else(at_least_alotof_diff_walk == "yes", 1, 0)),
    wash_prev = survey_mean(if_else(at_least_alotof_diff_wash == "yes", 1, 0))
  )

uganda16_prev_admin2 <- left_join(uganda16_basemap2, uganda16_prev_admin2, by = join_by(NAME_1 == admin2)) # Check that all names match (except the 2 - Lake Albert and Lake Victora - without gps observations inside them) : anti_join(uganda16_basemap2, uganda16_prev_admin2, by = join_by(NAME_1 == admin2))


# 7) Saving the recoded datasets and prevalence tables,  as well as MICS and LSMS ----


dhs_recoded= tibble::lst(
      dta_dhs,
     kenya22wt,
     malawi16wt,
     mali18wt,
     maurit19wt,
     mozam22wt,
     nigeria18wt,
     #pakis17wt,
     rwanda19wt,
     senegal18wt,
     senegal19wt,
     safrica16wt,
     tanza22wt,
     uganda16wt,
     kenya22_prev_admin0,
     kenya22_prev_admin1,
     kenya22_prev_admin2,
     malawi16_prev_admin0,
     malawi16_prev_admin1,
     malawi16_prev_admin2,
     mali18_prev_admin0,
     mali18_prev_admin1,
     mali18_prev_admin2,
     maurit19_prev_admin0,
     maurit19_prev_admin1,
     maurit19_prev_admin2,
     mozam22_prev_admin0,
     mozam22_prev_admin1,
     mozam22_prev_admin2,
     nigeria18_prev_admin0,
     nigeria18_prev_admin1,
     nigeria18_prev_admin2,
     nigeria18_prev_admin3,
     #pakis17_prev_admin0,
     #pakis17_prev_admin1,
     rwanda19_prev_admin0,
     rwanda19_prev_admin1,
     rwanda19_prev_admin2,
     senegal18_prev_admin0,
     senegal18_prev_admin1,
     senegal18_prev_admin2,
     senegal19_prev_admin0,
     senegal19_prev_admin1,
     senegal19_prev_admin2,
     safrica16_prev_admin0,
     safrica16_prev_admin1,
     safrica16_prev_admin2,
     tanza22_prev_admin0,
     tanza22_prev_admin1,
     tanza22_prev_admin2,
     uganda16_prev_admin0,
     uganda16_prev_admin1,
     uganda16_prev_admin2)

save(dhs_recoded,
     # MICS
     mics,
     # LSMS
     lsms,
     file = "data/tmp/after02.RData")


