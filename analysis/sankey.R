library(dplyr)
library(pavPriority)
library(networkD3)

links <- pC_dt$data %>%
  group_by(LeafID) %>%
  arrange(LeafID, leaf_age) %>%
  mutate(
    age_weeks = as.integer(cut(leaf_age, c(-Inf, 0, 7, 14, 21, 28, 35, 42, 49, 56, 61, 68, Inf))),
    state     = as.character(state),
    lag_state = lag(state, default = "0-0-0"),
    source    = paste(age_weeks, lag_state),
    target    = paste(age_weeks + 1, state)
    
    
  ) %>%
  select(age_weeks, source, target) %>%
  group_by(source, target) %>%
  summarise(value = n()) %>%
  as.data.frame


links <- links %>%
  arrange(source)

nodes <- pC_dt$states %>%
  select(name = state) %>%
  as.data.frame() 

nodes <- expand.grid(age_weeks = 1:13, name = nodes$name) %>%
  arrange(age_weeks) %>%
  transmute(name = paste(age_weeks, name)) 




links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1


links <- arrange(links, source)

nodes <- nodes %>%
  mutate(name = stringr::str_replace(name, "dead", "2-2-2"))

sankeyNetwork(Links = links, Nodes = nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'leaves', fontSize = 12, nodeWidth = 30,
              iterations = 0)