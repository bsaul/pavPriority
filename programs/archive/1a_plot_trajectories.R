#------------------------------------------------------------------------------#
#    TITLE : Examine Trajectories
#     DATE : 2018JUN24
#   AUTHOR : B. Saul
#  PURPOSE :
#------------------------------------------------------------------------------#

library(dplyr)
library(ggplot2)

dt <- readRDS("data/analysis_data.rds")

dt %>%
  # Just look at Col and Rhiz in first two cohorts
  filter(
    pathogen %in% c("Col", "Rhiz"),
    cohort %in% c("First Cohort", "Second Cohort"),
    emergent_experimental_period %in% 1L:2L
  ) %>%
  group_by(ID) %>%
  select(-leaf_damage) %>%
  tidyr::spread(key = pathogen, value = infected) %>%
  mutate(
    status = case_when(
      Col == 0 & Rhiz == 0 ~ "n",
      Col == 1 & Rhiz == 0 ~ "c",
      Col == 0 & Rhiz == 1 ~ "r",
      Col == 1 & Rhiz == 1 ~ "b"
    )
  ) -> hold



nleaves_by_week_per_ID <- hold %>%
  group_by(cohort, ID, emergent_experimental_period, weeks_since_emergence) %>%
  tally
  
nleaves_by_week_per_cohort <- hold %>%
  group_by(cohort, emergent_experimental_period, weeks_since_emergence) %>%
  tally

hold %>% 
  group_by(cohort, ID, emergent_experimental_period, weeks_since_emergence, status) %>%
  tally() %>%
  right_join(
    nleaves_by_week_per_ID,
    by = c("cohort", "ID", "emergent_experimental_period", "weeks_since_emergence")
  ) %>%
  mutate(p = n.x/n.y) -> hold2

# hold2 %>%
#   group_by(cohort, emergent_experimental_period, weeks_since_last_anchor, ID) %>%
#   summarise(check = sum(p)) %>% pull(check) %>% all(. == 1)

hold2 %>% 
  group_by(cohort, emergent_experimental_period, weeks_since_emergence, status) %>%
  summarise(
    n_infected = sum(n.x),
    p1  = mean(p)
  ) %>%
  right_join(
    nleaves_by_week_per_cohort,
    by = c("cohort", "emergent_experimental_period", "weeks_since_emergence")
  ) %>%
  mutate(
    p2 = n_infected/n
  ) -> plot_data

# Play with plots
ggplot(
  data = plot_data, 
  aes(x = weeks_since_emergence, y  = p2,
      color = status, group = status)) +
  geom_point() + 
  geom_line() + 
  facet_grid(cohort ~ emergent_experimental_period)
