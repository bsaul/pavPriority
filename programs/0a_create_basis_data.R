#------------------------------------------------------------------------------#
#    TITLE : Create basis analysis data
#     DATE : 2018JUN24
#   AUTHOR : B. Saul
#  PURPOSE : prepare the analysis datasets
#------------------------------------------------------------------------------#

library(dplyr)

dt1 <- read.csv("inst/extdata/2015_Fescue_disease_control_all_cohorts.csv",
               stringsAsFactors = FALSE) 

dt2 <- read.csv("inst/extdata/beats_bydv.csv",
                stringsAsFactors = FALSE) 


# Find anchor date
anchor1 <- dt1 %>% filter(cohort == "First Cohort") %>% pull(date) %>% min() %>% as.Date
anchor2 <- dt1 %>% filter(cohort == "Second Cohort") %>% pull(date) %>% min() %>% as.Date
anchor3 <- dt1 %>% filter(cohort == "Third Cohort") %>% pull(date) %>% min() %>% as.Date

create_variables <- function(.data){
  .data %>%
    group_by(LeafID) %>%
    mutate(
      date  = as.Date(date),
      emergence_date = as.Date(leaf.emergence),
      days_since_emergence   = as.integer(date - min(emergence_date)),
      days_since_last_anchor = as.integer(
        case_when(
          (date >= anchor1) & (date < anchor2) ~ (date - anchor1),
          (date >= anchor2) & (date < anchor3) ~ (date - anchor2),
          (date >= anchor3)                    ~ (date - anchor3)
        )
      ),
      current_experimental_period = case_when(
        (date >= anchor1) & (date < anchor2) ~ 1L,
        (date >= anchor2) & (date < anchor3) ~ 2L,
        (date >= anchor3)                    ~ 3L
      ),
      emergent_experimental_period = case_when(
        (emergence_date >= anchor1) & (emergence_date < anchor2) ~ 1L,
        (emergence_date >= anchor2) & (emergence_date < anchor3) ~ 2L,
        (emergence_date >= anchor3)                              ~ 3L  
      )
    ) %>%
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
                                    breaks = seq(0, 365, by = 7), right = FALSE),
      weeks_since_first_leaf  = cut(days_since_first_leaf, 
                                    breaks = seq(0, 365, by = 7), right = FALSE),
      weeks_since_trial_start = cut(days_since_trial_start, 
                                    breaks = seq(0, 365, by = 7), right = FALSE),
      weeks_since_last_anchor = cut(days_since_last_anchor, 
                                    breaks = seq(0, 365, by = 7), right = FALSE) 
    ) 
}

dt1 <- create_variables(dt1)
dt2 <- create_variables(dt2)


# ## Save data ####
# saveRDS(dt1, file = "data/analysis_data.rds")
# saveRDS(dt2, file = "data/analysis_data_pav.rds")
