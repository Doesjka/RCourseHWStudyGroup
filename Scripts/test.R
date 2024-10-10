# clear the workspace
rm(list = ls())


# load packages --------------

library(tidyverse)
library(here)
library(janitor)

# load data -----------
load("C:/Users/doesj/Github/RCourseHWStudyGroup/data/Example_LongFormatHashed.RData")

# pipe %>%
data_long_clean <- Example_LongFormat %>%
  clean_names() %>%
  select(behandeling,rounddescription,everything()) %>%
  select(-patient_traject_id)

# sort  and filter ----------
data_sorted <- arrange(data_long_clean,-vas_pijn_gemiddeld_1)

Females <- data_sorted %>%
  filter(geslacht == 'F') %>%
  select(vas_pijn_gemiddeld_1)

Males <- data_sorted %>%
  filter(geslacht == 'M') %>%
  select(vas_pijn_gemiddeld_1)

# grouping and summary ------------
PainMalesFemalesLeftRigth <- data_long_clean %>%
  group_by(zijde,geslacht) %>%
  summarize(max_pain = max(vas_pijn_gemiddeld_1,na.rm = TRUE))


# mutating ----------------
data_mutated <- data_long_clean %>%
  mutate(pain_average = (vas_pijn_gemiddeld_1 + vas_pijn_rust_1 + vas_pijn_belasten_1) /3) %>%
  mutate(lot_of_pain = vas_pijn_gemiddeld_1 > 50)


