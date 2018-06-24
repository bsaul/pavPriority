#------------------------------------------------------------------------------#
#    TITLE : Create analysis data
#     DATE : 2018JUN24
#   AUTHOR : B. Saul
#  PURPOSE : prepare the analysis datasets
#------------------------------------------------------------------------------#

library(dplyr)

dt <- read.csv("data/2015_Fescue_disease_control_all_cohorts.csv",
               stringsAsFactors = FALSE) %>%
  group_by(LeafID) %>%
  mutate(
    date  = as.Date(date),
    emergence_date = as.Date(leaf.emergence),
    days_since_emergence = as.integer(date - min(emergence_date))) %>%
  group_by(Tiller, ID) %>%
  mutate(
    days_since_first_leaf = as.integer(date - min(emergence_date))) %>%
  ungroup() %>%
  mutate(
    days_since_trial_start = as.integer(date - min(emergence_date))) %>%
  tidyr::gather(key = pathogen, value = leaf_damage, Col, Rhiz, Rust) %>%
  mutate(
    infected = (leaf_damage > 0) * 1L,
    weeks_since_emergence   = cut(days_since_emergence, 
                                  breaks = seq(0, 70, by = 7), right = FALSE),
    weeks_since_first_leaf  = cut(days_since_first_leaf, 
                                  breaks = seq(0, 70, by = 7), right = FALSE),
    weeks_since_trial_start = cut(days_since_trial_start, 
                                  breaks = seq(0, 365, by = 7), right = FALSE)) 

## Save data ####
saveRDS(dt, file = "data/analysis_data.rds")

