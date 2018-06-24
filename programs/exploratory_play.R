#------------------------------------------------------------------------------#
#    TITLE : 
#     DATE :
#   AUTHOR :
#  PURPOSE :
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
    weeks_since_emergence = cut(days_since_emergence, 
                                breaks = seq(0, 70, by = 7), right = FALSE),
    weeks_since_trial_start = cut(days_since_trial_start, 
                                 breaks = seq(0, 365, by = 7), right = FALSE)) 
  dt

dt$weeks_since_emergence


dt2 <- dt %>%
  group_by(cohort, ID, days_since_emergence, pathogen) %>%
  summarise(
    median_leaf_damage = median(leaf_damage),
    mean_leaf_damage   = mean(leaf_damage),
    presense           = any(infected)
  )



library(ggplot2)

dt3 <- dt2 %>%
  tidyr::spread(
    key = pathogen,
    value = median_leaf_damage
  )

ggplot(
  dt  %>% ungroup() %>%
    mutate(leaf_path = paste0(LeafID, pathogen),
           leaf_damage = leaf_damage + if_else(leaf_damage == 0, rnorm(n = 7485, sd = .4), 0)
           ), 
  aes(x = days_since_trial_start, y = leaf_damage, group = leaf_path,
      color = pathogen)) + 
  geom_line(alpha = .3) +
  facet_grid(cohort ~., scales = "free_y")

tempdt <-   dt  %>% filter(ID %in% c(14, 23, 24, 286), pathogen %in% c("Col", "Rhiz")) %>% 
  mutate(y = case_when(pathogen == "Col" ~ 0L,
                       pathogen == "Rhiz" ~ 1L,
                       pathogen == "Rust" ~ 2L)) %>%
  select(ID, Leaf, LeafID, date, emergence_date, days_since_emergence, 
         days_since_trial_start, pathogen, infected) %>%
  group_by(ID) %>%
  tidyr::spread(key = pathogen, value = infected) %>%
  mutate(
    color = case_when(
      Col == 0 & Rhiz == 0 ~ "none",
      Col == 1 & Rhiz == 0 ~ "col",
      Col == 0 & Rhiz == 1 ~ "rhi",
      Col == 1 & Rhiz == 1 ~ "both"
    ),
    y = case_when(
      Col == 0 & Rhiz == 0 ~ .25,
      Col == 1 & Rhiz == 0 ~ .5,
      Col == 0 & Rhiz == 1 ~ .5,
      Col == 1 & Rhiz == 1 ~ .75
    )
  )

ggplot(
  tempdt, 
  aes(x = days_since_trial_start, y = y, group = LeafID, 
      color = as.factor(color))) + 
  geom_line(color = "gray") + 
  geom_hline(color = "gray50", yintercept = 0) +
  geom_point() +
  scale_y_continuous(
    "",
    limits = c(0, 1),
    breaks = c(.25, .5, .75),
    expand = c(0,0)
  ) +
  facet_grid(Leaf ~ ID) + 
  theme_classic() + 
  theme(
    strip.text.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line    = element_line(color = "gray50"),
    axis.ticks.y = element_line(color = "gray50")
  )

