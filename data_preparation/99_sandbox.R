# 99 Sandbox: for putting random bits of code I typed, test things etc



# 99) Sandbox ----



## 3/10 example: Example of why multiplication by 100 of prevalence is useful ----

# These two people who compute a prevalence multiply by 100 too: https://stackoverflow.com/questions/71086625/adding-the-prevalence-of-an-outcome-to-a-gtsummarytbl-uvregression-univariable
# I could use proportion_summary but (I think) it doesn't work with survey data https://larmarange.github.io/analyse-R/gtsummary.html#tbl_custom_summary-proportion_summary


# A executer après avoir executé 02_recoding_dhs


# Creation of variable At least some difficulty walking
mali18 <- mali18 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")

mali18wt <- mali18 %>% as_survey_design(ids = psu, strata = strata, weights = wt, nest = TRUE)
mali18wt <- mali18wt |> srvyr::filter(age_groups != "0-4", de_facto_resident == "yes")


mali18wt |>
  tbl_svysummary(
    include = c("no_diff_any_domain", "at_least_some_diff_see", "at_least_some_diff_hear", "at_least_some_diff_com", "at_least_some_diff_remem", "at_least_some_diff_walk", "at_least_some_diff_wash"),
    statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%"),
    digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
    type = list(
      no_diff_any_domain ~ "continuous",
      at_least_some_diff_see ~ "continuous",
      at_least_some_diff_hear ~ "continuous",
      at_least_some_diff_com ~ "continuous",
      at_least_some_diff_remem ~ "continuous",
      at_least_some_diff_walk ~ "categorical",
      at_least_some_diff_wash ~ "continuous"
    ) #line used to prevent continuous variable to be treated as categorical, mentioned here https://stackoverflow.com/questions/67388749/r-tbl-summary-treating-continuous-variables-correctly
  )

# => Le problème est que ça affiche également le résultat négatif 0








describe(malawi16wt$variables$at_least_some_diff_see)

malawi16wt |>
  tbl_svysummary(
    include = c("at_least_some_diff_see"),
    statistic = list(all_continuous() ~ "{mean} * 100"),
    digits = list(all_continuous() ~ 3),
    type = list(at_least_some_diff_see ~ "continuous"))


malawi16wt |>
  tbl_svysummary(
    include = c("at_least_some_diff_see"),
    statistic = list(all_continuous() ~ "({p}%)"),
    digits = list(all_continuous() ~ 3),
    type = list(at_least_some_diff_see ~ "continuous"))


yy <- tanza22 |> filter(age_groups != "0-4", de_facto_resident == "yes")

tanza22wt |>
  tbl_svysummary(
    include = c("at_least_some_diff_see"),
    statistic = list(all_continuous() ~ "{mean}"),
    digits = list(all_continuous() ~ 3),
    type = list(at_least_some_diff_see ~ "continuous"))
















### ----

if(F){
  
  kenya220 |> select(hv103) |> print(n = 30)
  kenya22 |> select(hv103) |> print(n = 30)
  # save(kenya22, malawi99, ... , file = "data/tmp/after01.RData") # Multiple datasets are put in one RData file (see webin-R 3 at 16min30 to check the correct use of save())
  
  
  # kenya22_svy <- svydesign(id = kenya22$, data = kenya22, strata = kenya22$, weight = kenya22$, nest = T) # Code found here : https://github.com/DHSProgram/DHS-Analysis-Code/blob/main/Intro_DHSdata_Analysis/2_SurveyDesign/SurveyDesign.R
  #str(kenya22)
  #look_for(kenya22)
  
  library(labelled)
  
  d <- unlabelled(kenya22)
  
  
  kenya22_selection <- kenya22 |> 
    #select(hv103, hv104, hv105, hdis2, hdis4, hdis5, hdis6, hdis7, hdis8) |> 
    filter(!is.na(hdis2) & !is.na(hdis4) & !is.na(hdis5) & !is.na(hdis6) & !is.na(hdis7) & !is.na(hdis8)) |> 
    #filter(!is.na(sh27)) |> 
    filter(hv103 == 1) |> 
    filter(hv105 >= 5)
  #filter(!is.na(hv105))
  
  #n_distinct(as_factor(kenya22_selection$hhid))
  
  freq(kenya22_selection$hv105)
  freq(kenya22_selection$sh27)
  
  kenya22_selection$wt <- kenya22_selection$hv005 / 1000000
  
  kenya22wt <- svydesign(ids = kenya22_selection$hv021, data = kenya22_selection, strata = kenya22_selection$hv022, weights = kenya22_selection$wt, nest = TRUE)
  options(survey.lonely.psu = "adjust")
  
  
  
  freq(kenya22wt$variables$hdis2)
  freq(kenya22wt$variables$hv104)
  freq(kenya22wt$variables$hv105)
  freq(kenya22wt$variables$hv003)
  
  
  
  #svytable(~hdis2, kenya22wt)
  prop.table(svytable(~hdis2, kenya22wt))
  prop.table(svytable(~sh27, kenya22wt))
  
  
  kenya22_selection |> count(hdis2)
  
  table(kenya22$hdis3)
  table(kenya22$hdis2)
  table(kenya22$hdis4)
  
  freq(kenya22$hv024) # regions (cf Manual)
  
  summary(kenya22$hdis2)
  summary(kenya22$hv023)
  
  look_for
  
  # Read the DCT file into a variable to inspect or use
  dct_file <- "data/kenya/KEPR8CDT/KEPR8CFL.DCT"
  # Read lines of the .DCT file
  dct_contents <- readLines(dct_file)
  
  # Display the first few lines of the .DCT file to understand the structure
  print(head(dct_contents, 20))
  
  
  ############
  # Mali 2018
  
  
  
  mali18 <- read_dta("data/mali/MLPR7ADT/MLPR7AFL.DTA")
  
  # kenya22_svy <- svydesign(id = kenya22$, data = kenya22, strata = kenya22$, weight = kenya22$, nest = T) # Code found here : https://github.com/DHSProgram/DHS-Analysis-Code/blob/main/Intro_DHSdata_Analysis/2_SurveyDesign/SurveyDesign.R
  
  
  mali18_selection <- mali18 |> 
    #select(hv103, hv104, hv105, hdis2, hdis4, hdis5, hdis6, hdis7, hdis8) |> 
    #filter(!is.na(hdis2) & !is.na(hdis4) & !is.na(hdis5) & !is.na(hdis6) & !is.na(hdis7) & !is.na(hdis8)) |> 
    #filter(!is.na(sh27)) |> 
    filter(hv103 == 1) |> 
    #filter(hml16 >= 5)
    filter(hv105 >= 5)
  #filter(!is.na(hv105))
  #n_distinct(as_factor(kenya22_selection$hhid))
  
  freq(kenya22_selection$hv102)
  freq(mali18_selection$hv105)
  freq(mali18$hv105)
  freq(mali18$hv103)
  freq(kenya22_selection$sh27)
  
  mali18_selection$wt <- mali18_selection$hv005 / 1000000
  
  mali18wt <- svydesign(ids = mali18_selection$hv021, data = mali18_selection, strata = mali18_selection$hv022, weights = mali18_selection$wt, nest = TRUE)
  options(survey.lonely.psu = "adjust")
  
  
  
  freq(mali18$hdis2)
  
  freq(mali18wt$variables$hdis2)
  freq(kenya22wt$variables$hv104)
  freq(kenya22wt$variables$hv105)
  freq(kenya22wt$variables$hv003)
  
  
  
  #svytable(~hdis2, kenya22wt)
  prop.table(svytable(~hdis2, kenya22wt))
  prop.table(svytable(~sh27, kenya22wt))
  
  
  
  
  
  library(qessmasteR)
  
  kenya22 |> desc_quali(hdis9)
  
  describe(kenya22$diff_see)
  
  
  kenya22 |> filter
  
  
  
  
  kenya220 |> filter(!is.na(hdis2)) |> nrow()
  
  kenya220 |> desc_quali(hdis2, NR = T)
  
  kenya22 |> filter(shshort == "long questionnaire", age_groups != "0-4", de_facto_resident == "yes") |> desc_quali(hdis2, NR = T)
  kenya22 |> filter(shshort == "long questionnaire", age_groups != "0-4", de_facto_resident == "yes") |> desc_quali(hdis4, NR = T)
  kenya22 |> filter(shshort == "long questionnaire", age_groups != "0-4", de_facto_resident == "yes") |> desc_quali(hdis5, NR = T)
  kenya22 |> filter(shshort == "long questionnaire", age_groups != "0-4", de_facto_resident == "yes") |> desc_quali(hdis6, NR = T)
  kenya22 |> filter(shshort == "long questionnaire", age_groups != "0-4", de_facto_resident == "yes") |> desc_quali(hdis7, NR = T)
  kenya22 |> filter(shshort == "long questionnaire", age_groups != "0-4", de_facto_resident == "yes") |> desc_quali(hdis8, NR = T)
  
  
  
  kenya22 |> filter(shshort == "short questionnaire") |> desc_quali(hdis4, NR = T)
  
  kenya220 |> filter(shshort == 0) |> desc_quali(hdis4, NR = T)
  
  kenya220$shshort
  kenya22$shshort
  
  
  
  
  summary(kenya22$hv104)
  
  
  kenya22 <- kenya22 |> 
    mutate
  
  
  
  # Using the filter from srvyr (https://larmarange.github.io/guide-R/donnees_ponderees/manipulation.html)
  
  kenya22wt_de_facto_five_and_over <- kenya22wt |> 
    srvyr::filter(
      age_groups != "0-4" & # this condition does (voluntarily) include the respondants without age info)
        de_facto_resident == "yes"
      
    )
  
  
  
  
  kenya22wt_de_facto_fifteen_and_over$variables |> as_tibble() |> select(age_groups) |> group_by(age_groups) |> count()
  
  
  
  
}














library(tidyverse)
library(srvyr)
library(gtsummary)
library(sf)
library(ggplot2)








# Ancienne manière de créer indice au moins un peu de difficulté


## Recodage de kenya22$diff_com en kenya22$at_least_some_diff_com
kenya22$at_least_some_diff_com <- kenya22$diff_com %>%
  fct_recode(
    "no" = "no difficulty communicating",
    "yes" = "some difficulty",
    "yes" = "a lot of difficulty",
    "yes" = "cannot communicate at all",
    "no" = "don't know"
  ) |> 
  fct_relevel("no", "yes") |> 
  as.numeric() - 1

kenya22$at_least_some_diff_com <- kenya22$at_least_some_diff_com * 100
var_label(kenya22$at_least_some_diff_com) <- "At least some difficulty communicating"




# kenya22wt_de_facto_five_and_over$variables <- copy_labels(from = kenya22wt$variables, to = kenya22wt_de_facto_five_and_over$variables) # With subset() the labels are lost so we take them back from kenya22wt


kenya22wt_de_facto_fifteen_and_over <- kenya22wt_de_facto_five_and_over |>
  srvyr::filter(age_groups != "5-9", age_groups != "10-14")


kenya22wt_de_facto_fifteen_and_over_women <- kenya22wt_de_facto_five_and_over |>
  srvyr::filter(gender == "female", age_groups != "5-9", age_groups != "10-14")


kenya22wt_de_facto_fifteen_and_over_men <- kenya22wt_de_facto_five_and_over |>
  srvyr::filter(gender == "male", age_groups != "5-9", age_groups != "10-14")




kenya22_complete |> select(hv105) |> describe()



## Check of highest_diff custom code  ----


mali18 |> select(highest_diff_any_domain) |> summary()

mali18 |> mutate(
  highest_diff_any_domain2 = factor(case_when(
    diff_see == "cannot see at all" | diff_hear == "cannot hear at all" | diff_com == "cannot communicate at all" | diff_remem == "cannot remember/concentrate at all" | diff_walk == "cannot walk or climb at all" | diff_wash == "cannot wash or dress at all" ~ "cannot do at all",
    diff_see == "a lot of difficulty" | diff_hear == "a lot of difficulty" | diff_com == "a lot of difficulty" | diff_remem == "a lot of difficulty" | diff_walk == "a lot of difficulty" | diff_wash == "a lot of difficulty" ~ "a lot of difficulty",
    diff_see == "some difficulty" | diff_hear == "some difficulty" | diff_com == "some difficulty" | diff_remem == "some difficulty" | diff_walk == "some difficulty" | diff_wash == "some difficulty" ~ "some difficulty",
    diff_see == "no difficulty seeing" & diff_hear == "no difficulty hearing" & diff_com == "no difficulty communicating" & diff_remem == "no difficulty remembering/concentrating" & diff_walk == "no difficulty walking or climbing" & diff_wash == "no difficulty washing or dressing" ~ "no difficulty",
    diff_see == "don't know" | diff_hear == "don't know" | diff_com == "don't know" | diff_remem == "don't know" | diff_walk == "don't know" | diff_wash == "don't know" ~ "don't know",
    .default = NA
  ), levels = c("no difficulty", "some difficulty", "a lot of difficulty", "cannot do at all", "don't know"))
) |> 
  select(highest_diff_any_domain2) |> summary()








kenya22 |> select(highest_diff_any_domain) |> describe()

uganda16 |> select(sh29) |> describe()
uganda16 |> select(sh27) |> summary()


uganda16 |> select(sh27) |> summary()
uganda16 |> select(sh28) |> summary()

uganda16 |> select(diff_hear) |> summary()

uganda16 |> select(diff_hear) |> describe()

uganda16 |> select(sh28) |> summary()

uganda16 |> select(sh24) |> describe()
uganda16 |> select(sh25) |> describe()
uganda16 |> select(diff_see) |> describe()


uganda16 |> select(sh24) |> summary()
uganda16 |> select(sh25) |> summary()
uganda16 |> select(diff_see) |> summary()

uganda16 |> select(sh24) |> class()
uganda16$sh25 |> class()
uganda16$diff_see |> class()

uganda16 |> select(sh27) |> summary()
uganda16 |> select(sh28) |> summary()


uganda16 |> select(sh24, sh25) |> filter(is.na(sh24), is.na(sh25))

uganda16 |> select(sh27, sh28) |> filter(is.na(sh27), is.na(sh28))

uganda16 |> select(sh24, sh25) |> filter(!is.na(sh24), !is.na(sh25))



uganda16 |> select(diff_see) |> describe()
uganda16 |> select(diff_hear) |> describe()
uganda16 |> select(diff_com) |> describe()
uganda16 |> select(diff_remem) |> describe()
uganda16 |> select(diff_walk) |> describe()
uganda16 |> select(diff_wash) |> describe()


fct_relabel()

senegal19 |> select(diff_com) |> describe()

senegal19 |> select(sh20gg) |> describe()
senegal19 |> select(sh20gh) |> describe()
senegal19 |> select(sh20gi) |> describe()
senegal19 |> select(sh20gj) |> describe()

senegal19 |> select(sh20gg) |> summary()
senegal19 |> select(sh20gh) |> summary()
senegal19 |> select(sh20gi) |> summary()
senegal19 |> select(sh20gj) |> summary()

# BC EF

senegal19 |> select(sh20gb, sh20gc) |> filter(is.na(sh20gb), is.na(sh20gc))

senegal19 |> select(sh20gb, sh20gc) |> filter(!is.na(sh20gb), !is.na(sh20gc))
senegal19 |> select(sh20ge, sh20gf) |> filter(!is.na(sh20ge), !is.na(sh20gf))

senegal19 |> select(sh27, sh28) |> filter(is.na(sh27), is.na(sh28))

senegal19 |> select(sh24, sh25) |> filter(!is.na(sh24), !is.na(sh25))

senegal19 |> select(sh20gb) |> summary()
senegal19 |> select(sh20gc) |> summary()
senegal19 |> select(sh20ge) |> summary()
senegal19 |> select(sh20gf) |> summary()




# In fact it doesn't apply according to the (debatable) logic we've put in the case_when() we voluntarily leave out the "don't know" because they're not mentioned in malawi 2016 final report table 2.14.2
malawi16 |> select(sh307) |> summary()
malawi16 |> select(sh308) |> summary()
malawi16 |> select(sh309) |> summary()
malawi16 |> select(sh310) |> summary()


malawi16 |> select(diff_wash) |> summary()

malawi16 |> select(sh307, sh309) |> filter(!is.na(sh307), !is.na(sh309))

malawi16 |> select(sh312, sh314) |> filter(!is.na(sh312), !is.na(sh314))


senegal19 |> select(sh20gb, sh20gc) |> filter(is.na(sh20gb), is.na(sh20gc))

senegal19 |> select(sh20gb, sh20gc) |> filter(!is.na(sh20gb), !is.na(sh20gc))
senegal19 |> select(sh20ge, sh20gf) |> filter(!is.na(sh20ge), !is.na(sh20gf))

senegal19 |> select(sh27, sh28) |> filter(is.na(sh27), is.na(sh28))

senegal19 |> select(sh24, sh25) |> filter(!is.na(sh24), !is.na(sh25))

senegal19 |> select(sh20gb) |> summary()
senegal19 |> select(sh20gc) |> summary()
senegal19 |> select(sh20ge) |> summary()
senegal19 |> select(sh20gf) |> summary()




# COPY CODE de 02_recoding.R 28/9 11h ----



# Recoding 


# 0) Loading packages and RData from 01_import ----

library(srvyr) # loaded before tidyverse to give priority to tidyverse version of functions like filter
library(tidyverse)
library(labelled)
library(survey)
library(questionr)


load("data/tmp/after01.RData")


# 1) Creation of useful variables ----

## A) kenya22 ----

# Assigning descriptive names and labels to variables
kenya22 <- kenya22 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000, # to compute the weights, as indicated here https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm or here https://www.youtube.com/watch?v=pJwd2-m3QBY
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103, # relevant variable for de facto according to https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm
  gender = hv104,
  marital_status = hv115,
  wealth_quintile = hv270,
  education_level_ever = hv106,
  education_level_ever2 = sheduc, # this variable sheduc is only provided in Kenya 2022 DHS, so this line doesn't apply to the other DHS surveys ; hv106 and not sheduc is used in the disability tables in the kenya 2022 final report
  long_short_quest = shshort,
  diff_see = hdis2,
  diff_hear = hdis4,
  diff_com = hdis5,
  diff_remem = hdis6,
  diff_walk = hdis7,
  diff_wash = hdis8,
  highest_diff_any_domain = hdis9
)



kenya22 <- kenya22 |> set_variable_labels(
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
  highest_diff_any_domain = "Difficulty in at least one domain"
)

# Making age groups
kenya22$age_groups <- cut(kenya22$hv105,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                          labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know") # To make the NA a level in itself
var_label(kenya22$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
kenya22 <- kenya22 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
kenya22 <- kenya22 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
kenya22 <- kenya22 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
kenya22 <- kenya22 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
kenya22 <- kenya22 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
kenya22 <- kenya22 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
kenya22 <- kenya22 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")


## B) FOR NOW I HAVE PUT HERE ONLY Assigning descriptive names and labels to variables, and creation of the same disability variables present in other surveys ||| malawi16 ----



malawi16 <- malawi16 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
  marital_status = hv115,
  wealth_quintile = hv270,
  education_level_ever = hv106,
  diff_see = factor(case_when(
    sh308 == "can't see at all" | sh310 == "can't see at all" ~ "cannot see at all",
    sh308 %in% c("some difficulty", "a lot of difficulty") ~ sh308,
    sh310 %in% c("some difficulty", "a lot of difficulty") ~ sh310,
    sh307 == "don't know" | sh308 == "don't know" | sh309 == "don't know" | sh310 == "don't know" ~ "don't know",
    sh307 == "no" | sh307 == "no" ~ "no difficulty seeing",
    .default = NA
  ), levels = c("no difficulty seeing", "some difficulty", "a lot of difficulty", "cannot see at all", "don't know")),
  diff_hear = factor(case_when(
    sh313 == "can't hear at all" | sh315 == "can't hear at all" ~ "cannot hear at all",
    sh313 %in% c("some difficulty", "a lot of difficulty") ~ sh313,
    sh315 %in% c("some difficulty", "a lot of difficulty") ~ sh315,
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
)


## C) mali18 ----

# Assigning descriptive names and labels to variables
mali18 <- mali18 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

mali18 <- mali18 |> set_variable_labels(
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
)

# Making age groups
mali18$age_groups <- cut(mali18$hv105,
                         include.lowest = TRUE,
                         right = FALSE,
                         dig.lab = 4,
                         breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                         labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(mali18$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
mali18 <- mali18 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
mali18 <- mali18 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
mali18 <- mali18 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
mali18 <- mali18 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
mali18 <- mali18 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
mali18 <- mali18 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
mali18 <- mali18 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")


## D) maurit19 ----


# Assigning descriptive names and labels to variables
maurit19 <- maurit19 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

maurit19 <- maurit19 |> set_variable_labels(
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
)

# Making age groups
maurit19$age_groups <- cut(maurit19$hv105,
                           include.lowest = TRUE,
                           right = FALSE,
                           dig.lab = 4,
                           breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                           labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(maurit19$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
maurit19 <- maurit19 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
maurit19 <- maurit19 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
maurit19 <- maurit19 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
maurit19 <- maurit19 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
maurit19 <- maurit19 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
maurit19 <- maurit19 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
maurit19 <- maurit19 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")



## E) mozam22 ----


# Assigning descriptive names and labels to variables
mozam22 <- mozam22 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

mozam22 <- mozam22 |> set_variable_labels(
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
)

# Making age groups
mozam22$age_groups <- cut(mozam22$hv105,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                          labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(mozam22$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
mozam22 <- mozam22 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
mozam22 <- mozam22 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
mozam22 <- mozam22 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
mozam22 <- mozam22 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
mozam22 <- mozam22 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
mozam22 <- mozam22 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
mozam22 <- mozam22 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")



## F) nigeria18 ----


# Assigning descriptive names and labels to variables
nigeria18 <- nigeria18 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

nigeria18 <- nigeria18 |> set_variable_labels(
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
)

# Making age groups
nigeria18$age_groups <- cut(nigeria18$hv105,
                            include.lowest = TRUE,
                            right = FALSE,
                            dig.lab = 4,
                            breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(nigeria18$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
nigeria18 <- nigeria18 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
nigeria18 <- nigeria18 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
nigeria18 <- nigeria18 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
nigeria18 <- nigeria18 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
nigeria18 <- nigeria18 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
nigeria18 <- nigeria18 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
nigeria18 <- nigeria18 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")



## G) pakis17 ----


# Assigning descriptive names and labels to variables
pakis17 <- pakis17 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

pakis17 <- pakis17 |> set_variable_labels(
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
)

# Making age groups
pakis17$age_groups <- cut(pakis17$hv105,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                          labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(pakis17$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
pakis17 <- pakis17 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
pakis17 <- pakis17 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
pakis17 <- pakis17 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
pakis17 <- pakis17 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
pakis17 <- pakis17 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
pakis17 <- pakis17 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
pakis17 <- pakis17 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")



## H) rwanda19 ----


# Assigning descriptive names and labels to variables
rwanda19 <- rwanda19 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

rwanda19 <- rwanda19 |> set_variable_labels(
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
)

# Making age groups
rwanda19$age_groups <- cut(rwanda19$hv105,
                           include.lowest = TRUE,
                           right = FALSE,
                           dig.lab = 4,
                           breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                           labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(rwanda19$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
rwanda19 <- rwanda19 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
rwanda19 <- rwanda19 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
rwanda19 <- rwanda19 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
rwanda19 <- rwanda19 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
rwanda19 <- rwanda19 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
rwanda19 <- rwanda19 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
rwanda19 <- rwanda19 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")



## I) FOR NOW I HAVE PUT HERE ONLY Assigning descriptive names and labels to variables, and creation of the same disability variables present in other surveys ||| senegal19 ----


senegal19 <- senegal19 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
  marital_status = hv115,
  wealth_quintile = hv270,
  education_level_ever = hv106,
  diff_see = factor(case_when( # we checked that nobody answered both sh20gb and sh20gc with senegal16 |> select(sh20gb, sh20gc) |> filter(!is.na(sh20gb), !is.na(sh20gc))
    sh20gb == "ne peut pas voir du tout" | sh20gc == "ne peut pas voir du tout" ~ "cannot see at all",
    sh20gb == "beaucoup de difficultes" | sh20gc == "beaucoup de difficultes" ~ "a lot of difficulty",
    sh20gb == "quelques difficultes" | sh20gc == "quelques difficultes" ~ "some difficulty",
    sh20gb == "aucune difficulte pour voir" | sh20gc == "aucune difficulte pour voir" ~ "no difficulty seeing",
    sh20gb == "ne sait pas" | sh20gc == "ne sait pas" ~ "don't know",
    .default = NA
  ), levels = c("no difficulty seeing", "some difficulty", "a lot of difficulty", "cannot see at all")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
  diff_hear = factor(case_when( # we checked that nobody answered both sh20ge and sh20gf with senegal16 |> select(sh20ge, sh20gf) |> filter(!is.na(sh20ge), !is.na(sh20gf))
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
)

## J) safrica16 ----


# Assigning descriptive names and labels to variables
safrica16 <- safrica16 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

safrica16 <- safrica16 |> set_variable_labels(
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
)

# Making age groups
safrica16$age_groups <- cut(safrica16$hv105,
                            include.lowest = TRUE,
                            right = FALSE,
                            dig.lab = 4,
                            breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(safrica16$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
safrica16 <- safrica16 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
safrica16 <- safrica16 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
safrica16 <- safrica16 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
safrica16 <- safrica16 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
safrica16 <- safrica16 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
safrica16 <- safrica16 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
safrica16 <- safrica16 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")



## K) tanza22 ----


# Assigning descriptive names and labels to variables
tanza22 <- tanza22 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
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
)

tanza22 <- tanza22 |> set_variable_labels(
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
)

# Making age groups
tanza22$age_groups <- cut(tanza22$hv105,
                          include.lowest = TRUE,
                          right = FALSE,
                          dig.lab = 4,
                          breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 95),
                          labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60+")
) |>
  fct_na_value_to_level("Don't know")
var_label(tanza22$age_groups) <- "Age group"


# Creation of variable No difficulty in any domain
tanza22 <- tanza22 %>%
  mutate(no_diff_any_domain = case_when(
    highest_diff_any_domain %in% c("no difficulty", "don't know") ~ 1,
    highest_diff_any_domain %in% c("some difficulty", "a lot of difficulty", "cannot do at all") ~ 0
  ) * 100) %>%
  set_variable_labels(no_diff_any_domain = "No difficulty in any domain")


# Creation of variable At least some difficulty seeing
tanza22 <- tanza22 %>%
  mutate(at_least_some_diff_see = case_when(
    diff_see %in% c("no difficulty seeing", "don't know") ~ 0,
    diff_see %in% c("some difficulty", "a lot of difficulty", "cannot see at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_see = "At least some difficulty seeing")


# Creation of variable At least some difficulty hearing
tanza22 <- tanza22 %>%
  mutate(at_least_some_diff_hear = case_when(
    diff_hear %in% c("no difficulty hearing", "don't know") ~ 0,
    diff_hear %in% c("some difficulty", "a lot of difficulty", "cannot hear at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_hear = "At least some difficulty hearing")


# Creation of variable At least some difficulty communicating
tanza22 <- tanza22 |> 
  mutate(at_least_some_diff_com = case_when(
    diff_com %in% c("no difficulty communicating", "don't know") ~ 0,
    diff_com %in% c("some difficulty", "a lot of difficulty", "cannot communicate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_com = "At least some difficulty communicating")


# Creation of variable At least some difficulty remembering
tanza22 <- tanza22 %>%
  mutate(at_least_some_diff_remem = case_when(
    diff_remem %in% c("no difficulty remembering/concentrating", "don't know") ~ 0,
    diff_remem %in% c("some difficulty", "a lot of difficulty", "cannot remember/concentrate at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_remem = "At least some difficulty remembering or concentrating")


# Creation of variable At least some difficulty walking
tanza22 <- tanza22 %>%
  mutate(at_least_some_diff_walk = case_when(
    diff_walk %in% c("no difficulty walking or climbing", "don't know") ~ 0,
    diff_walk %in% c("some difficulty", "a lot of difficulty", "cannot walk or climb at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_walk = "At least some difficulty walking")


# Creation of variable At least some difficulty washing
tanza22 <- tanza22 %>%
  mutate(at_least_some_diff_wash = case_when(
    diff_wash %in% c("no difficulty washing or dressing", "don't know") ~ 0,
    diff_wash %in% c("some difficulty", "a lot of difficulty", "cannot wash or dress at all") ~ 1
  ) * 100) %>%
  set_variable_labels(at_least_some_diff_wash = "At least some difficulty washing or dressing")



## L) FOR NOW I HAVE PUT HERE ONLY Assigning descriptive names and labels to variables, and creation of the same disability variables present in other surveys ||| uganda16 ----



uganda16 <- uganda16 |> mutate(
  household_sample_weight = hv005,
  wt = household_sample_weight / 1000000,
  psu = hv021,
  strata = hv022,
  region = hv024,
  type_place_residence = hv025,
  de_facto_resident = hv103,
  gender = hv104,
  marital_status = hv115,
  wealth_quintile = hv270,
  education_level_ever = hv106,
  diff_see = factor(case_when( # we checked that nobody answered both sh24 and sh25 with uganda16 |> select(sh27, sh28) |> filter(!is.na(sh27), !is.na(sh28))
    sh24 == "cannot see at all" | sh25 == "cannot see at all" ~ "cannot see at all",
    sh24 == "a lot of difficulty" | sh25 == "a lot of difficulty" ~ "a lot of difficulty",
    sh24 == "some difficulty" | sh25 == "some difficulty" ~ "some difficulty",
    sh24 == "no difficulty seeing" | sh25 == "no difficulty seeing" ~ "no difficulty seeing",
    sh24 == "don't know" | sh25 == "don't know" ~ "don't know",
    .default = NA
  ), levels = c("no difficulty seeing", "some difficulty", "a lot of difficulty", "cannot see at all")), # we put priority to a higher disability in the order of the conditions, so we put the levels back in order
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
)



# 2) Survey design (and extraction of the "working" subsample) ----

# The following page advises to create the survey object after having done all the recoding, as well as the conversion of labelled vectors to factors https://larmarange.github.io/analyse-R/definir-un-plan-d-echantillonnage-complexe.html (or here https://larmarange.github.io/guide-R/donnees_ponderees/manipulation.html)
# Also, the survey design has to be defined on the entire dataset, then it's possible to subset() (not filter() that doesn't work on survey objects) the dataset (see Webin-R 10 41min30) (also here https://www.reddit.com/r/statistics/comments/bvahag/why_it_is_important_to_make_survey_design_object/)

# Finally I don't use the survey() package, but the srvyr() package, as do larmarange and simo fotso in their rmarkdown
# kenya22wt <- svydesign(ids = kenya22$psu, data = kenya22, strata = kenya22$strata, weights = kenya22$wt, nest = TRUE) # "wt" for "weighted"
# options(survey.lonely.psu = "adjust") # Added for the case where there may be a single PSU in a strata, as indicated here https://www.youtube.com/watch?v=pJwd2-m3QBY


kenya22wt <- kenya22 %>% as_survey_design(
  ids = psu,            # PSU (Primary Sampling Units)
  strata = strata,       # Strata variable
  weights = wt,          # Weight variable
  nest = TRUE            # To handle nesting
)
options(survey.lonely.psu = "adjust") # Added for the case where there may be a single PSU in a strata, as indicated here https://www.youtube.com/watch?v=pJwd2-m3QBY

kenya22wt <- kenya22wt |> # Before it was "kenya22wt_de_facto_five_and_over"
  srvyr::filter(
    age_groups != "0-4", # this condition does (voluntarily) include the respondants without age info)
    de_facto_resident == "yes",
    long_short_quest == "long questionnaire"
  )


# 4) Prevalence ----


### A.b) Creation prevalence tibbles

kenya22_prevalence_region <- kenya22wt |> 
  srvyr::group_by(region) |> 
  srvyr::summarize(
    seeing_prevalence = survey_mean(at_least_some_diff_see),
    hearing_prevalence = survey_mean(at_least_some_diff_hear)
  )
kenya22_prevalence_region <- kenya22_prevalence_region %>%
  mutate(region = case_when(
    region == "mombasa" ~ "Mombasa",
    region == "kwale" ~ "Kwale",
    region == "kilifi" ~ "Kilifi",
    region == "tana river" ~ "Tana River",
    region == "lamu" ~ "Lamu",
    region == "taita taveta" ~ "Taita Taveta",
    region == "garissa" ~ "Garissa",
    region == "wajir" ~ "Wajir",
    region == "mandera" ~ "Mandera",
    region == "marsabit" ~ "Marsabit",
    region == "isiolo" ~ "Isiolo",
    region == "meru" ~ "Meru",
    region == "tharaka-nithi" ~ "Tharaka-Nithi",
    region == "embu" ~ "Embu",
    region == "kitui" ~ "Kitui",
    region == "machakos" ~ "Machakos",
    region == "makueni" ~ "Makueni",
    region == "nyandarua" ~ "Nyandarua",
    region == "nyeri" ~ "Nyeri",
    region == "kirinyaga" ~ "Kirinyaga",
    region == "murang'a" ~ "Murang'a",
    region == "kiambu" ~ "Kiambu",
    region == "turkana" ~ "Turkana",
    region == "west pokot" ~ "West Pokot",
    region == "samburu" ~ "Samburu",
    region == "trans nzoia" ~ "Trans Nzoia",
    region == "uasin gishu" ~ "Uasin Gishu",
    region == "elgeyo-marakwet" ~ "Elgeyo-Marakwet",
    region == "nandi" ~ "Nandi",
    region == "baringo" ~ "Baringo",
    region == "laikipia" ~ "Laikipia",
    region == "nakuru" ~ "Nakuru",
    region == "narok" ~ "Narok",
    region == "kajiado" ~ "Kajiado",
    region == "kericho" ~ "Kericho",
    region == "bomet" ~ "Bomet",
    region == "kakamega" ~ "Kakamega",
    region == "vihiga" ~ "Vihiga",
    region == "bungoma" ~ "Bungoma",
    region == "busia" ~ "Busia",
    region == "siaya" ~ "Siaya",
    region == "kisumu" ~ "Kisumu",
    region == "homa bay" ~ "Homa Bay",
    region == "migori" ~ "Migori",
    region == "kisii" ~ "Kisii",
    region == "nyamira" ~ "Nyamira",
    region == "nairobi" ~ "Nairobi",
    TRUE ~ region # Keeps the original names if there's no need for changes
  ))


kenya22_shape <- sf::st_read("data/shapefile/gadm41_KEN_shp/gadm41_KEN_1.shp")

kenya22_map_data <- left_join(kenya22_shape, kenya22_prevalence_region, by = join_by(NAME_1 == region)) # join_by() found in the function help page


ggplot(kenya22_map_data) +
  geom_sf(aes(fill = seeing_prevalence)) +
  scale_fill_viridis_c() +
  labs(title = "Kenya: Prevalence of Seeing Disability by Region")





















# 5) Saving the recoded datasets ----

save(kenya22wt, 
     file = "data/tmp/after02.RData")




####  ============================



maurit19 |> 
  filter(gender == "female", de_facto_resident == "yes", age_groups != "0-4")





malawi16 |> filter(age > 9 & age < 18) |>  select(sh306) |> summary()
malawi16 |> filter(age > 9 & age < 18) |>  select(sh310) |> summary()
# COMPRENDRE POURQUOI LA PLUPART DES ENFANTS N'ONT PAS REPONDU A CES QUESTIONS (regarder dans le questionnaire)

library(qessmasteR)

desc_quali()
malawi16 |> multi_croise(diff_see, age)

tab_malawi16_3_at_least_one_domain_fifteen_over <-
  malawi16 |>
  dplyr::filter(age > 9 & age < 18 ) |> 
  tbl_summary(
    include = c("diff_see"),
    statistic = list(
      all_categorical() ~ "{n}"
    ),
    digits = list(
      all_categorical() ~ 1
    )
  )
tab_malawi16_3_at_least_one_domain_fifteen_over





# What is just below is not used
st_is_valid(kenya22_prev_admin1$geometry)

kenya22_prevalence_region <- kenya22_prevalence_region %>%
  mutate(region = case_when(
    region == "mombasa" ~ "Mombasa",
    region == "kwale" ~ "Kwale",
    region == "kilifi" ~ "Kilifi",
    region == "tana river" ~ "Tana River",
    region == "lamu" ~ "Lamu",
    region == "taita taveta" ~ "Taita Taveta",
    region == "garissa" ~ "Garissa",
    region == "wajir" ~ "Wajir",
    region == "mandera" ~ "Mandera",
    region == "marsabit" ~ "Marsabit",
    region == "isiolo" ~ "Isiolo",
    region == "meru" ~ "Meru",
    region == "tharaka-nithi" ~ "Tharaka-Nithi",
    region == "embu" ~ "Embu",
    region == "kitui" ~ "Kitui",
    region == "machakos" ~ "Machakos",
    region == "makueni" ~ "Makueni",
    region == "nyandarua" ~ "Nyandarua",
    region == "nyeri" ~ "Nyeri",
    region == "kirinyaga" ~ "Kirinyaga",
    region == "murang'a" ~ "Murang'a",
    region == "kiambu" ~ "Kiambu",
    region == "turkana" ~ "Turkana",
    region == "west pokot" ~ "West Pokot",
    region == "samburu" ~ "Samburu",
    region == "trans nzoia" ~ "Trans Nzoia",
    region == "uasin gishu" ~ "Uasin Gishu",
    region == "elgeyo-marakwet" ~ "Elgeyo-Marakwet",
    region == "nandi" ~ "Nandi",
    region == "baringo" ~ "Baringo",
    region == "laikipia" ~ "Laikipia",
    region == "nakuru" ~ "Nakuru",
    region == "narok" ~ "Narok",
    region == "kajiado" ~ "Kajiado",
    region == "kericho" ~ "Kericho",
    region == "bomet" ~ "Bomet",
    region == "kakamega" ~ "Kakamega",
    region == "vihiga" ~ "Vihiga",
    region == "bungoma" ~ "Bungoma",
    region == "busia" ~ "Busia",
    region == "siaya" ~ "Siaya",
    region == "kisumu" ~ "Kisumu",
    region == "homa bay" ~ "Homa Bay",
    region == "migori" ~ "Migori",
    region == "kisii" ~ "Kisii",
    region == "nyamira" ~ "Nyamira",
    region == "nairobi" ~ "Nairobi999",
    TRUE ~ region # Keeps the original names if there's no need for changes
  ))



st_read("data/basemaps/gadm41_KEN.gpkg")
st_layers("data/basemaps/gadm41_KEN.gpkg")


kenya22_map_data <- left_join(kenya22_shape, kenya22_prevalence_region, by = join_by(NAME_1 == region)) # join_by() found in the function help page

# This outputs the rows from the basemap that didn't match with Admin 1 names
anti_join(kenya22_basemap1, kenya22_prevalence_region, by = join_by(NAME_1 == region)) # join_by() found in the function help page
anti_join(kenya22_prevalence_region, kenya22_basemap1, by = join_by(region == NAME_1))


#ggplot(kenya22_map_data) +
#  geom_sf(aes(fill = seeing_prevalence)) +
#  scale_fill_viridis_c() +
#  labs(title = "Kenya: Prevalence of Seeing Disability by Region")





# 24/10 : in the bottom of the recoding_dhs script

mali18_shape <- sf::st_read("data/shapefile/gadm41_MLI_shp/gadm41_MLI_1.shp")

tanza22_shapeshp <- sf::st_read("data/shapefile/gadm41_TZA_shp/gadm41_TZA_2.shp")

tanza22_shapeshp1 <- sf::st_read("data/basemaps/gadm41_TZA_shp/gadm41_TZA_1.shp")

# gpkg comes in one file and is recommended by everyone
tanza22_shape0 <- sf::st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_0")
tanza22_shape1 <- sf::st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_1")
tanza22_shape2 <- sf::st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_2")
tanza22_shape3 <- sf::st_read("data/basemaps/gadm41_TZA.gpkg", layer = "ADM_ADM_3")

#st_layers("data/shapefile/gadm41_TZA.gpkg")

#plot(tanza22_shape2)

1+1


# Joining 

# Check that all names match : anti_join(kenya22_prev_admin1, kenya22_basemap1, by = join_by(ADM1NAME == NAME_1))
###kenya22_prev_admin1 <- left_join(kenya22_basemap1, kenya22_prev_admin1, by = join_by(NAME_1 == ADM1NAME)) # join_by() found in the function help page
#above already done


# Some of the code below may better be put in the recoding file

kenya22_prevalence_region <- kenya22wt |> 
  srvyr::group_by(region) |> 
  srvyr::summarize(
    seeing_prevalence = survey_mean(at_least_some_diff_see),
    hearing_prevalence = survey_mean(at_least_some_diff_hear)
  )
kenya22_prevalence_region <- kenya22_prevalence_region %>%
  mutate(region = case_when(
    region == "mombasa" ~ "Mombasa",
    region == "kwale" ~ "Kwale",
    region == "kilifi" ~ "Kilifi",
    region == "tana river" ~ "Tana River",
    region == "lamu" ~ "Lamu",
    region == "taita taveta" ~ "Taita Taveta",
    region == "garissa" ~ "Garissa",
    region == "wajir" ~ "Wajir",
    region == "mandera" ~ "Mandera",
    region == "marsabit" ~ "Marsabit",
    region == "isiolo" ~ "Isiolo",
    region == "meru" ~ "Meru",
    region == "tharaka-nithi" ~ "Tharaka-Nithi",
    region == "embu" ~ "Embu",
    region == "kitui" ~ "Kitui",
    region == "machakos" ~ "Machakos",
    region == "makueni" ~ "Makueni",
    region == "nyandarua" ~ "Nyandarua",
    region == "nyeri" ~ "Nyeri",
    region == "kirinyaga" ~ "Kirinyaga",
    region == "murang'a" ~ "Murang'a",
    region == "kiambu" ~ "Kiambu",
    region == "turkana" ~ "Turkana",
    region == "west pokot" ~ "West Pokot",
    region == "samburu" ~ "Samburu",
    region == "trans nzoia" ~ "Trans Nzoia",
    region == "uasin gishu" ~ "Uasin Gishu",
    region == "elgeyo-marakwet" ~ "Elgeyo-Marakwet",
    region == "nandi" ~ "Nandi",
    region == "baringo" ~ "Baringo",
    region == "laikipia" ~ "Laikipia",
    region == "nakuru" ~ "Nakuru",
    region == "narok" ~ "Narok",
    region == "kajiado" ~ "Kajiado",
    region == "kericho" ~ "Kericho",
    region == "bomet" ~ "Bomet",
    region == "kakamega" ~ "Kakamega",
    region == "vihiga" ~ "Vihiga",
    region == "bungoma" ~ "Bungoma",
    region == "busia" ~ "Busia",
    region == "siaya" ~ "Siaya",
    region == "kisumu" ~ "Kisumu",
    region == "homa bay" ~ "Homa Bay",
    region == "migori" ~ "Migori",
    region == "kisii" ~ "Kisii",
    region == "nyamira" ~ "Nyamira",
    region == "nairobi" ~ "Nairobi",
    TRUE ~ region # Keeps the original names if there's no need for changes
  ))


kenya22_shape <- sf::st_read("../data/basemaps/gadm41_KEN_shp/gadm41_KEN_1.shp")

kenya22_map_data <- left_join(kenya22_shape, kenya22_prevalence_region, by = join_by(NAME_1 == region)) # join_by() found in the function help page


ggplot(kenya22_map_data) +
  geom_sf(aes(fill = seeing_prevalence)) +
  scale_fill_viridis_c() +
  labs(title = "Kenya: Prevalence of Seeing Disability by Region")
#theme(axis.text = element_text(family = 'Arial')) to make the symbol ° appear but doesn't do it





benin21 <- benin21 |> unite("id", c("HH1", "HH2", "LN"), sep = "_", remove = F) # https://tidyr.tidyverse.org/reference/unite.html
benin21_hl <- read_sav("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/hl.sav")
benin21_hl <- benin21_hl |> 
  select(HH1, HH2, HL1, hhweight, PSU, stratum) |> 
  unite("id", c("HH1", "HH2", "HL1"), sep = "_", remove = T)

benin21 <- benin21_hl |> # For now 15 to 17 years old from mn and wm files not excluded even if their AF variables are NA ; we check that the hl table and the table after the join have the same number of observations, since we'll need all observations for the weighting (hence the use of full_join() rather than left_join())
  full_join(benin21, c("id" = "id")) # We followed "Guidelines for Merging Data Files of a MICS Survey" found here https://mics.unicef.org/faq ; Check that all id match, in this case that we don't loose mn or wm observations : anti_join(benin21, benin21_hl, by = join_by(id == id))


# [Actually no filter before the weighting!] filter(!is.na(mnweight) & mnweight > 0) |> # we remove the 7 observations where mnweight is NA (for these observations all other variables except identification variables are also NA), and we also remove the 202 observations where mnweight is 0 (for these observations all other variables except identification variables are NA)
# [Actually no filter before the weighting!] filter(!is.na(wmweight) & wmweight > 0) |> # we remove the 13 observations where wmweight is NA (for these observations all other variables except identification variables are also NA), and we also remove the 222 observations where wmweight is 0 (for these observations all other variables except identification variables are NA)




# generic TO COPY

# Line for st_read function 
gps <- st_join(gps, _basemap2) # This line checks which points from the gps dataset fall inside the polygons of the basemap dataset, and adds polygon info to points (for each point, it attaches the attributes of the polygon it falls into, like region names, so it keeps point geometry, not the geom variable, i.e. multipolygons, from basemap)

gps <- gps |> 
  mutate(NAME_2 = case_when(
    is.na(NAME_2) ~ "Likoma", # INDICATE SOURCE FOR INFORMATION ON THE UNLOCATED GPS POINTS
    .default = NAME_2
  ))

anti_join(gps |> st_drop_geometry(), _basemap2, by = "NAME_2") # [X number of lines] before the mutate just above # To check the gps points that are not placed in any entity from the basemap
anti_join(_basemap2, gps |> st_drop_geometry(), by = "NAME_2") # [X number of lines] # To check the map entities that don't have any gps inside them



# check unknown 1, unknown 2...
kenya22_basemap2 |> 
  st_drop_geometry() |> 
  anti_join(kenya22gps |> st_drop_geometry(), by = "NAME_2") |> 
  select(NAME_1, NAME_2)
a <-  st_join(kenya22_basemap2, kenya22gps, join = st_contains, left = TRUE)
filter(is.na(NAME_2)) |> 
  select(NAME_2)
st_disjoint(kenya22_basemap2, kenya22gps)

kenya22_basemap2 %>%
  filter(
    !st_disjoint(., kenya22gps) %>% rowSums() > 0
  )

kenya22gps_map2 <- st_join(kenya22_basemap2, kenya22gps) # More observations because of points at boundaries: https://github.com/r-spatial/sf/issues/1901 => en fait non

kenya22gps %>%
  filter(!DHSCLUST %in% kenya22gps_map2$DHSCLUST)







# Create the sequence of expected numbers
expected_numbers <- min(kenya22gps$DHSCLUST, na.rm = TRUE):max(kenya22gps$DHSCLUST, na.rm = TRUE)

# Find the missing numbers
missing_numbers <- setdiff(min(kenya22gps$DHSCLUST, na.rm = TRUE):max(kenya22gps$DHSCLUST, na.rm = TRUE), kenya22gps$DHSCLUST)

kenya22gps_map2 %>%
  count(DHSCLUST) %>%
  filter(n > 1)

kenya22gps_map2 |> 
  
  
  multiple_intersections <- which(lengths(st_intersects(kenya22gps, kenya22_basemap2)) > 1)

summary(kenya22gps$geometry)
summary(kenya22_basemap2$geometry)

unmatched_points <- kenya22gps |> 
  filter(is.na(NAME_2) | NAME_2 == "805") # | DHSCLUST == "778" is the same row as is.na(NAME_2)  # Replace `admin2_column` with the actual column for admin2 in your dataset


ggplot() +
  geom_sf(data = kenya22_basemap2, fill = "lightgray", color = "darkgray") +  # Admin2 boundaries
  geom_sf(data = unmatched_points, color = "red", size = 2) +  # Unmatched points
  labs(title = "Unmatched GPS Points",
       subtitle = "Points without admin2 region association") +
  theme_minimal()




#https://cran.r-project.org/web/packages/surveyPrev/vignettes/vignette-main.html#spatial-information
poly.adm1 <- geodata::gadm(country="SN", level=1, path=tempdir())
poly.adm1 <- sf::st_as_sf(poly.adm1)
poly.adm2 <- geodata::gadm(country="SN", level=2, path=tempdir())
poly.adm2 <- sf::st_as_sf(poly.adm2)
str(poly.adm2)
plot(poly.adm2$geometry)

senegal19gps <- st_read("data/gps_datasets/SNGE8BFL/SNGE8BFL.shp")
str(senegal19gps)
plot(senegal19gps$geometry)

## joint DHS gps point data to GADM polygones for admin 2


kenya22gps_admin2 <- st_join(kenya22_basemap2, kenya22gps) # join points
plot(kenya22gps_admin2$geometry)


## 19/11 analyses raphael code block +  prevalence tables before putting their code in one function ----


theme_gtsummary_mean_sd() # to put mean rather than median in gtsummary for continuous variables
# National prevalences of disability (table)

#```{r}

make_table_of_prevalences <- function(country99wt) {
  country99wt |>
    tbl_svysummary(
      include = c("at_least_some_diff_any_domain", "at_least_some_diff_see", "at_least_some_diff_hear", "at_least_some_diff_com", "at_least_some_diff_remem", "at_least_some_diff_walk", "at_least_some_diff_wash"),
      statistic = list(all_continuous() ~ "{mean}"),
      digits = list(all_continuous() ~ 1),
      type = list(
        at_least_some_diff_any_domain ~ "continuous",
        at_least_some_diff_see ~ "continuous",
        at_least_some_diff_hear ~ "continuous",
        at_least_some_diff_com ~ "continuous",
        at_least_some_diff_remem ~ "continuous",
        at_least_some_diff_walk ~ "continuous",
        at_least_some_diff_wash ~ "continuous"
      ) #line used to prevent continuous variable to be treated as categorical, mentioned here https://stackoverflow.com/questions/67388749/r-tbl-summary-treating-continuous-variables-correctly
    )
}

# Maybe I can filter out the Unkown values for Malawi, Nigeria, Pakistan, South Africa and Tanzania with country99wt |> filter(criteria) |> make_table_of_prevalences
tab_kenya22_prevs <- make_table_of_prevalences(kenya22wt)
tab_malawi16_prevs <- make_table_of_prevalences(malawi16wt)
tab_mali18_prevs <- make_table_of_prevalences(mali18wt)
tab_maurit19_prevs <- make_table_of_prevalences(maurit19wt)
tab_mozam22_prevs <- make_table_of_prevalences(mozam22wt)
tab_nigeria18_prevs <- make_table_of_prevalences(nigeria18wt)
tab_pakis17_prevs <- make_table_of_prevalences(pakis17wt)
tab_rwanda19_prevs <- make_table_of_prevalences(rwanda19wt)
tab_safrica16_prevs <- make_table_of_prevalences(safrica16wt)
tab_senegal19_prevs <- make_table_of_prevalences(senegal19wt)
tab_tanza22_prevs <- make_table_of_prevalences(tanza22wt)
tab_uganda16_prevs <- make_table_of_prevalences(uganda16wt)


tab_all_countries_prevs <- tbl_merge(
  tbls = list(tab_kenya22_prevs, tab_malawi16_prevs, tab_mali18_prevs, tab_maurit19_prevs, tab_mozam22_prevs, tab_nigeria18_prevs, tab_pakis17_prevs, tab_rwanda19_prevs, tab_safrica16_prevs, tab_senegal19_prevs, tab_tanza22_prevs, tab_uganda16_prevs),
  tab_spanner = c("**Kenya 2022**", "**Malawi 2016**", "**Mali 2018**", "**Mauritania 2019**", "**Mozambique 2022**", "**Nigeria 2018**", "**Pakistan 2017**", "**Rwanda 2019**", "**South Africa 2016**", "**Senegal 2019**", "**Tanzania 2022**", "**Uganda 2016**")
)
tab_all_countries_prevs


#```





#######################
#######################
#######################


theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")

kenya22wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")


kenya22wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

malawi16wt |> 
  tbl_svysummary(
    by = c("strata"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")


mali18wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

maurit19wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")


mozam22wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

nigeria18wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

rwanda19wt |> 
  tbl_svysummary(
    by = c("district"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

senegal19wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

safrica16wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

tanza22wt |> 
  tbl_svysummary(
    by = c("NAME_2"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")

uganda16wt |> 
  tbl_svysummary(
    by = c("NAME_1"),
    include = c("at_least_some_diff_see"),
    type = list(
      at_least_some_diff_see ~ "categorical"
    ),
    statistic = ~"{p}% ({n_unweighted})",
    digits = ~ c(1, 0),
    label = list(
      variable = "Prevalence of variable"
    )
  ) |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(everything() ~ "**Prevalence (weighted)**")





# 26/11 import here for codebook
benin21_hl <- read_sav("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/hl.sav")
benin21_hl_codebook <- benin21_hl |> look_for() |> tibble() |> mutate(levels = as.character(levels), value_labels = as.character(value_labels))
benin21_wm <- read_sav("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/wm.sav")
benin21_wm_codebook <- benin21_wm |> look_for() |> tibble() |> mutate(levels = as.character(levels), value_labels = as.character(value_labels))
benin21_mn <- read_sav("data/mics/Benin MICS6 Datasets/Benin MICS6 SPSS Datasets/mn.sav")
benin21_mn_codebook <- benin21_mn |> look_for() |> tibble() |> mutate(levels = as.character(levels), value_labels = as.character(value_labels))

addWorksheet(wb, "benin21_hh")
writeData(wb, "benin21_hh", benin21_hh_codebook)
addWorksheet(wb, "benin21_hl")
writeData(wb, "benin21_hl", benin21_hl_codebook)
addWorksheet(wb, "benin21_wm")
writeData(wb, "benin21_wm", benin21_wm_codebook)
addWorksheet(wb, "benin21_mn")
writeData(wb, "benin21_mn", benin21_mn_codebook)


sierra17_hl <- read_sav("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/hl.sav")
sierra17_hl_codebook <- sierra17_hl |> look_for() |> tibble() |> mutate(levels = as.character(levels), value_labels = as.character(value_labels))
addWorksheet(wb, "sierra17_hl")
writeData(wb, "sierra17_hl", sierra17_hl_codebook)



#if (countryname_file == "sierra17_hl") { dataset_codebook$label[85] <- "Label not available" }


sierra17_hl_codebook$label[85] <- "Label not available"

codebook_save("data/mics/Sierra Leone MICS6 Datasets/Sierra Leone MICS6 Datasets/", "sierral17_hl", "hl")



# Construct the full file path
###file_path <- paste0(path, file, ".sav")
# Read the file and generate the codebook
###dataset <- read_sav(file_path)
###dataset_codebook <- dataset |> look_for() |> tibble() |> mutate(levels = as.character(levels), value_labels = as.character(value_labels))



# Add the codebook to the workbook
###sheet_name <- paste0(country_name, "_", file)

# DONE IDEA : As an alternative to the nonexistent codebooks, save the result of dataset |> look_for() |> View() in a spreadsheet for each MICS dataset 


library(leaflet)
library(shiny)
library(shinythemes)
library(sf)

leaflet() |>
  addTiles() |> 
  addPolygons(data = st_as_sf(rwanda19_prev_admin1))

class(rwanda19_prev_admin1$geom)






ui <- fluidPage(
  theme = shinytheme("cerulean"),
  headerPanel("Interactive Disability prevalence"),
  sidebarPanel(
    selectInput(inputId = "country",
                label = "Select a country",
                choices = list("Rwanda" = "rwanda", "Tanzania" = "tanza") # See at 40:57 for example use of "choices" https://www.youtube.com/watch?v=9uFQECk30kA
    ), # Example use of selectInput here https://mastering-shiny.org/basic-ui.html
    selectInput(inputId = "gender", label = "Select a gender", choices = list("Women" = "women", "Men" = "men")),
    selectInput(inputId = "age_group", label = "Select an age group", choices = list("15-30" = "young", "30-45" = "middle", "45-60" = "old")),
    selectInput(inputId = "type", label = "Select a disability type", choices = list("Vision" = "see", "Hearing" = "hear", "Communication" = "com"))
    # Add actionButton()? # At 43:25 actionButton() used to prevent the app from being reactive and calculating for every toggle by the user
  ),
  mainPanel(
    verbatimTextOutput("country_out"),
    verbatimTextOutput("gender_out"),
    verbatimTextOutput("age_group_out"),
    verbatimTextOutput("type_out"),
    leafletOutput("myMap")
    )
)



server <- function(input, output) {
  output$country_out <- renderText(print(input$country))
  output$gender_out <- renderText(print(input$gender))
  output$age_group_out <- renderText(print(input$age_group))
  output$type_out <- renderText(print(input$type))
  output$myMap <- renderLeaflet(
    leaflet() |> 
      addTiles() |> 
      addPolygons(data = st_as_sf(rwanda19_prev_admin1))
  )
}
# Connections to other elements
shinyApp(ui = ui, server = server)


rwanda19wt |> 
case_when(gender
  
)

renderGiraf

# DONE We haven't yet added the variable stratum that can be only found in the hh file for sierral17, probably it's better to add at the begining of this file


# Not sure that it would make sense to put the values from 7 units into 17 units (which would mean that e.g. three units from the 17 level have the same value for a variable, because in the 7 level they are in the same unit)

# TODO ADD the level with 28 districts (see Atlas that indicates the presence of a DISTRICT variable in the hh file, so need to do a left join somewhere, probably better in the begining [+ also don't forget the need to import from the hh file the stratum variable for sierral17])
# Like for malawi dhs
# malawi19_prev_admin3 DONE not sure why there are 92 observations (and not 102?) (96 if we use MICSGEO) # Maybe some names are the same?? => DONE, answer is because there were names that were similar and there were NA in prev_admin3

# Not sure that it would make sense to put the values from 5 units into 7 units (which would mean that e.g. two units from the 7 level have the same value for a variable, because in the 5 level they are in the same unit)
# Maybe group the units together to get to 2 islands (see commentary next to the creation of the basemap)

## 4.2) Merging the (??completed??) gps datasets with the mics survey datasets




# brouillon en dessous (pour l'import des MICS)
# Vu que hl a toutes les observations (hommes et femmes), ça parait plus logique de d'abord fusionner (les lignes/observations) les tableaux mn et wm, puis de fusionner les colonnes avec hl (pour obtenir la variable hhweight)
# Faire avant la fusion de hl et l'autre tableau, un select sur hl pour ne garder que hhweight ; on va faire un left_join() avec le tabeau mnwm à gauche et on rajoute avec les information du tableau hl à droite  
look_for(benin21_mn)
look_for(benin21_wm) |> View()


benin21_mn$windex5 |> describe()
benin21_hl$windex5 |> describe()

to_factor(benin21_hl$HL6) |> summary()

to_factor(benin21_mn_complete$MWB4) |> summary()
to_factor(benin21_mn_complete$MWAGE) |> summary()




benin21_mn_complete_codebook <- benin21_mn_complete |> look_for() |> tibble() |> mutate(levels = as.character(levels), value_labels = as.character(value_labels))
write_csv(benin21_mn_complete_codebook, file = "data/mics/Benin MICS6 Datasets/benin21_mn_codebook.csv")


comor22_basemap0 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_0")
comor22_basemap1 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_1")
comor22_basemap2 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_2")
comor22_basemap3 <- st_read("data/basemaps/gadm41_COM.gpkg", layer = "ADM_ADM_3")




quo_name()

benin21_mn_complete |> look_for() |> View()

benin21_mn |> look_for() |> View()
benin21_wm |> look_for() |> View()
benin21_hh |> look_for() |> View()
benin21_hl |> look_for() |> View()


benin21_mn |> select(mnweight) |> distinct() |> arrange(mnweight)
benin21_hl |> select(hhweight) |> distinct() |> arrange(hhweight)
benin21_hh |> select(hhweight) |> distinct() |> arrange(hhweight)

benin21_mn_complete |> describe("MAF9")

benin21_mn_complete |> describe("MWB6A")
benin21_mn_complete |> select(MWB6A) |> unlabelled() |> summary()
benin21_mn_complete |> select(mwelevel) |> unlabelled() |> summary()


benin21_mn_complete |> select(MWB4) |> unlabelled() |> summary()
benin21_mn_complete |> select(MWAGE) |> unlabelled() |> summary()

look_for(benin21_mn)
benin21_mn |> unlabelled() |> look_for()


# MAF6 - MAF12

benin21 |> select(MAF6) |> summary()
benin21 |> select(MAF6) |> str()

library(srvyr)

as_survey(benin21_mn)


benin21_mn <- benin21_mn |> filter(!is.na(mnweight))
benin21_mnwt <- benin21_mn %>% as_survey_design(ids = HH1, strata = stratum, weights = mnweight, nest = TRUE)


ggplot(data = africa_map) +
  geom_sf() +
  geom_sf(data = admin2_level_prevalences |> mutate(see_prev = if_else(origin_survey == "mics", see_prev, NA)), aes(fill = see_prev)) # This removes the values in the see_prev variable for countries not from MICS


"Uganda" = uganda16_prev_admin1 |> mutate(NAME_1_old = NAME_1, NAME_1 = REGNAME), # Here Admin 1 names are in REGNAME variable (because the basemap we use for admin1 is from DHS boundaries) (and we keep in NAME_1_old the values of the NAME_1 variable)

# Note: Here we didn't homogeneize the admin2 names (for instance for comor22 it is different from eswat21 and possibly also from other MICS countries)


#TODO CHANGE THIS VARIABLE FOR ALL COUNTRIES TO Has at least some difficulty in any domain -> DONE I THINK 4/12

# In the LSMS sheet Maybe to be sure check all the surveys on the LSMS data page: search "hear" or "entendre" in the searchbar on the top left in data description

# https://lat/long%20(see%20file:///D:/Download/ben_ehcvm2_information_de_base_eng%20(5).pdf%20and%20https://microdata.worldbank.org/index.php/catalog/6272/data-dictionary/F1?)file_name=s00_me_ben2021



# Import 12/01

# Import of DHS data


# 0) Loading packages ----
library(tidyverse)
library(haven) # To import DTA files (Stata) and sav files (SPSS)
library(labelled) # for the function unlabelled() for dealing with labelled vectors
library(questionr)


# 1.1) Import of the DHS databases ----

kenya22_complete <- read_dta("data/kenya/KEPR8CDT/KEPR8CFL.DTA")
malawi16_complete <- read_dta("data/malawi/MWPR7ADT/MWPR7AFL.DTA")
mali18_complete <- read_dta("data/mali/MLPR7ADT/MLPR7AFL.DTA")
maurit19_complete <- read_dta("data/mauritania/MRPR71DT/MRPR71FL.DTA")
mozam22_complete <- read_dta("data/mozambique/MZPR81DT/MZPR81FL.DTA")
nigeria18_complete <- read_dta("data/nigeria/NGPR7BDT/NGPR7BFL.DTA")
pakis17_complete <- read_dta("data/pakistan/PKPR71DT/PKPR71FL.DTA")
rwanda19_complete <- read_dta("data/rwanda/RWPR81DT/RWPR81FL.DTA")
senegal19_complete <- read_dta("data/senegal/SNPR8BDT/SNPR8BFL.DTA")
safrica16_complete <- read_dta("data/south_africa/ZAPR71DT/ZAPR71FL.DTA")
tanza22_complete <- read_dta("data/tanzania/TZPR82DT/TZPR82FL.DTA")
uganda16_complete <- read_dta("data/uganda/UGPR7BDT/UGPR7BFL.DTA")


# 1.2) Selection of variables + conversion of labelled vectors to factors  ----

kenya22 <- kenya22_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, sheduc, shshort, starts_with("hdis")) |>   # If I want to keep all variables relocate(hv104, hv105, starts_with("hdis"), ...) |> # Maybe it will be relevant to add the variables included in the final reports' cross tables
  unlabelled() # Mentioned in Webin-R 5 44min30 ; pay attention to the values of hv105 (95 corresponds to 95+ and 98 to "don't know")

malawi16 <- malawi16_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, shdist, hv102, hv103, hv104, hv105, hv106, hv115, hv270, sh306:sh323) |> #  hv102 for malawi16 because the selection is on de jure
  unlabelled()

mali18 <- mali18_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

maurit19 <- maurit19_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

mozam22 <- mozam22_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

nigeria18 <- nigeria18_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

pakis17 <- pakis17_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, sheduc, starts_with("hdis")) |> # sheduc is only present in kenya22 and pakis17 but most likely won't be used
  unlabelled()

rwanda19 <- rwanda19_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, shdistrict, starts_with("hdis")) |> 
  unlabelled()

senegal19 <- senegal19_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("sh20g")) |> 
  unlabelled()

safrica16 <- safrica16_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

tanza22 <- tanza22_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, starts_with("hdis")) |> 
  unlabelled()

uganda16 <- uganda16_complete |> 
  select(hv001, hv005, hv021, hv022, hv024, hv025, hv103, hv104, hv105, hv106, hv115, hv270, sh23:sh32) |> 
  unlabelled()


# 1.3) Saving the DHS imported datasets ----

# In case it is useful to use the full DHS datasets:
# save(kenya22_complete, malawi16_complete, mali18_complete, maurit19_complete, mozam22_complete, nigeria18_complete, pakis17_complete, rwanda19_complete, senegal19_complete, safrica16_complete, tanza22_complete, uganda16_complete, file = "data/tmp/after01_complete.RData") # Multiple datasets are put in one RData file (see webin-R 3 at 16min30 to check the correct use of save() or here https://larmarange.github.io/analyse-R/export-de-donnees.html)

save(kenya22,
     malawi16,
     mali18,
     maurit19,
     mozam22,
     nigeria18,
     pakis17,
     rwanda19,
     senegal19,
     safrica16,
     tanza22,
     uganda16,
     file = "data/tmp/after01.RData") # Multiple datasets are put in one RData file (see webin-R 3 at 16min30 to check the correct use of save() or here https://larmarange.github.io/analyse-R/export-de-donnees.html)


# 2.1) Import of MICS databases ----

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


# 2.2) Selection of variables + conversion of labelled vectors to factors ----

## 2.2.A) benin21 ----

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


## 2.2.B) centraf18 ----

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

## 2.2.C) chad19 ----

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

## 2.2.D) comor22 ----

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

## 2.2.E) drcong17 ----

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

## 2.2.F) eswat21 ----

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

## 2.2.G) gamb18 ----

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

## 2.2.H) ghana17 ----

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

## 2.2.I) guibi18 ----

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

## 2.2.J) lesot18 ----

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

## 2.2.K) madag18 ----

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

## 2.2.L) malawi19 ----

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

## 2.2.x) nigeria21 ----
#nigeria21_mn_complete <- read_sav("data/mics/Nigeria MICS6 Datasets/Nigeria MICS6 Datasets/Nigeria MICS6 SPSS Datasets/mn.sav")
#nigeria21_wm_complete <- read_sav("data/mics/Nigeria MICS6 Datasets/Nigeria MICS6 Datasets/Nigeria MICS6 SPSS Datasets/wm.sav")

## 2.2.M) saoto19 ----

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

## 2.2.N) sierral17 ----

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

## 2.2.O) togo17 ----

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

## 2.2.P) yemen22 ----

#yemen22_mn_complete <- read_sav("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/") # No men dataset available for yemen22
yemen22_wm_complete <- read_sav("data/mics/Yemen MICS6 Datasets/Yemen MICS6 SPSS Datasets/wm.sav")
yemen22_wm <- yemen22_wm_complete |> 
  select(HH1, HH2, LN, WB4, starts_with("AF"), HH6, HH7, WAGE, wmweight, welevel, disability, windex5, PSU, stratum) |>  # Absence of variable religion in wm file (no mn file for yemen22)
  mutate(windex5 = if_else(windex5 == 0, NA, windex5)) |> # the value 0 has no label associated, so if we don't replace 0 with NA then when the variable is converted to a factor the values stay 1, 2, 3, 4, 5 rather than their labels
  unlabelled() # we checked that for the WB4 variable there where no values of [97] "INCONSISTENT"

## 2.2.Q) zimbab19 ----

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

# 2.3) Saving the MICS imported datasets ----


save(benin21_mn,
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
     zimbab19_wm,
     file = "data/tmp/after01_mics.RData") # Multiple datasets are put in one RData file (see webin-R 3 at 16min30 to check the correct use of save() or here https://larmarange.github.io/analyse-R/export-de-donnees.html)



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

benin21lsms_cover <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/s00_me_ben2021.dta")
benin21lsms_health <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/s03_me_ben2021.dta")
benin21lsms_individual_characteristics <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/ehcvm_individu_ben2021.dta")
benin21lsms_weighting <- read_dta("data/lsms/BEN_2021_EHCVM-2_v01_M_STATA14/ehcvm_ponderations_ben2021.dta")







benin21lsms_cover <- benin21lsms_cover |> unlabelled()
benin21lsms_health <- benin21lsms_health |> unlabelled()
benin21lsms_individual_characteristics <- benin21lsms_individual_characteristics |> unlabelled()
benin21lsms_weighting <- benin21lsms_weighting |> unlabelled()




senegal19 |> desc_quali(cluster_number, wealth_quintile, marital_status, education_level_ever, NR = T) |> kableExtra::kbl()


library(qessmasteR)
kenya22 |> desc_quali(diff_see, NR = T)
malawi16 |> desc_quali(diff_see, NR = T)
mali18 |> desc_quali(diff_see, NR = T)
maurit19 |> desc_quali(diff_see, NR = T)
mozam22 |> desc_quali(diff_see, NR = T)
nigeria18 |> desc_quali(diff_see, NR = T)
pakis17 |> desc_quali(diff_see, NR = T)
rwanda19 |> desc_quali(diff_see, NR = T)
senegal19 |> desc_quali(diff_see, NR = T)
safrica16 |> desc_quali(diff_see, NR = T)
tanza22 |> desc_quali(diff_see, NR = T)
uganda16 |> desc_quali(diff_see, NR = T)


# 24/1 00_built

save(
  # DHS surveys
  kenya22wt,
  malawi16wt,
  mali18wt,
  maurit19wt,
  mozam22wt,
  nigeria18wt,
  #pakis17wt,
  rwanda19wt,
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
  #pakis17_prev_admin0,
  #pakis17_prev_admin1,
  rwanda19_prev_admin0,
  rwanda19_prev_admin1,
  rwanda19_prev_admin2,
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
  uganda16_prev_admin2,
  # MICS surveys
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
  yemen22wt,
  yemen22_prev_admin0,
  yemen22_prev_admin1,
  zimbab19wt,
  zimbab19_prev_admin0,
  zimbab19_prev_admin1,
  zimbab19_prev_admin2,
  file = "data/data_atlas.RData")




