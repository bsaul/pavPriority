library(dplyr)
dt <- readRDS("data/analysis_data.rds")

dt %>%
  group_by(ID) %>%
  select(-leaf_damage) %>%
  tidyr::spread(key = pathogen, value = infected) %>%
  mutate(
    t        = as.integer(weeks_since_trial_start),
    leaf_age = as.integer(weeks_since_emergence)) %>%
  select(ID, Leaf, t,
         C = Col, R = Rhiz, P = Rust, leaf_age, cohort) -> 
  hold

hold %>% ungroup () %>%
  distinct(C, R, P)

library(MVB)

y <- as.matrix(hold[, c("C", "R")])
x <- model.matrix(~ t, data = hold)
test <- mvbfit(x = x, y = y, maxOrder = 2)

mvbfit(x = x, y = hold$C, maxOrder = 2)
glm(C ~ t, data = hold, family = "binomial")

test$beta
summary(test)
