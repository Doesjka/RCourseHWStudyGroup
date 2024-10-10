# clear the workspace
rm(list = ls())

# load packages --------------
library(tidyverse)
library(here)
library(janitor)
library(ggbeeswarm)

theme_set(theme_classic())

# load data -----------
load("C:/Users/doesj/Github/RCourseHWStudyGroup/data/Example_LongFormatHashed.RData")

data_long_clean <- Example_LongFormat %>%
  clean_names() %>%
  select(behandeling,rounddescription,everything()) %>%
  select(-patient_traject_id)

# plotting
data_long_clean %>%
  na.omit() %>%
  ggplot(aes(x=vas_pijn_gemiddeld_1, y=rounddescription, color=geslacht)) +
  geom_point() +
  coord_flip()

data_long_clean %>%
  na.omit() %>%
  filter(vas_pijn_gemiddeld_1<100) %>%
  filter(vas_pijn_gemiddeld_1>0) %>%
  ggplot(aes(x=vas_pijn_gemiddeld_1, y=geslacht, color=geslacht)) +
  geom_jitter() +
  facet_wrap(~rounddescription)

data_long_clean %>%
  na.omit() %>%
  filter(vas_pijn_gemiddeld_1<100) %>%
  filter(vas_pijn_gemiddeld_1>0) %>%
  group_by(geslacht) %>%
  summarise(mean = mean(vas_pijn_gemiddeld_1),
            sd = sd(vas_pijn_gemiddeld_1),
            n = n(),
            stderr = sd/sqrt(n)) %>%
  ggplot(aes(x=mean, y=geslacht)) +
  geom_col() +
  geom_errorbar(aes(xmin=mean-stderr, xmax=mean+stderr, y=geslacht))

data_long_clean %>%
  na.omit() %>%
  filter(vas_pijn_gemiddeld_1<100) %>%
  filter(vas_pijn_gemiddeld_1>0) %>%
  ggplot(aes(x=vas_pijn_gemiddeld_1, y=vas_functie_1, color=geslacht)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(title = "Relationship between pain and hand functon",
       x = "VAS pain (0-10)",
       y = "VAS function (0-10)")


