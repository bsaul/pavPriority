#------------------------------------------------------------------------------#
#    TITLE : Examine Trajectories
#     DATE : 2018JUN24
#   AUTHOR : B. Saul
#  PURPOSE :
#------------------------------------------------------------------------------#

dt %>%
  # Just look at Col and Rhiz in first two cohorts
  filter(
    pathogen %in% c("Col", "Rhiz"),
    cohort %in% c("First Cohort", "Second Cohort")
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

nleaves_by_week <- hold %>%
  group_by(cohort, ID, weeks_since_emergence) %>%
  tally
  


hold %>% 
  group_by(cohort, ID, weeks_since_emergence, status) %>%
  tally() %>%
  right_join(nleaves_by_week, by = c("cohort", "ID", "weeks_since_emergence")) %>%
  mutate(p = n.x/n.y) %>%
  group_by(cohort, weeks_since_emergence, status) %>%
  summarise(n.x = sum(n.x),
            n.y = sum(n.y),
            p1  = mean(p),
            p2  = n.x/n.y) -> x
x
library(ggplot2)

ggplot(x, 
       aes(x = weeks_since_emergence, y  = p1,
           color = status, group = status)) +
  geom_point() + 
  geom_line() + 
  facet_grid(~cohort)
