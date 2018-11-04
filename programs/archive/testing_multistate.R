library(dplyr)
dt <- readRDS("data/analysis_data.rds")

set_remaining_to_1 <- function(x){
  n <- length(x)
  if(sum(x) == 0 || (sum(x) == 1 & x[n] == 1)){ return(x) }
  
  # Find first x[i] == 1
  s <- min(which(x == 1))
  
  x[(s+1):n] <- 1
  x
}

dt %>%
  group_by(ID, Tiller) %>%
  select(-leaf_damage) %>%
  tidyr::spread(key = pathogen, value = infected) %>%
  mutate(
    t        = as.integer(weeks_since_trial_start),
    leaf_age = as.integer(weeks_since_emergence)) %>%
  select(ID, Tiller, Leaf, t,
         C = Col, R = Rhiz, leaf_age, cohort)  %>% 
  group_by(ID, Tiller, Leaf) %>% arrange(t) %>%
  mutate(
    C = set_remaining_to_1(C),
    R = set_remaining_to_1(R)) -> 
  hold



hold2 <- hold %>% 
  group_by(ID, Tiller, t) %>%
  mutate(
    ID2 = paste(ID, Tiller, Leaf),
    pC = ifelse(n() == 1, 0, (sum(C) - C)/(n() - 1)),
    pR = ifelse(n() == 1, 0, (sum(R) - R)/(n() - 1)),
    sC = case_when(
      # pC == 0               ~ 0,
      0   <= pC & pC <= 0.25 ~ 0,
      0.25 <= pC & pC <= 1   ~ 1),
    sR = case_when(
      # pR == 0               ~ 0,
      0   <=  pR & pR <= 0.25 ~ 0,
      0.25 <= pR & pR <= 1   ~ 1),
    state = paste(C, sC, R, sep = "-"),
    state2 = case_when(
      state == "0-0-0" ~ 1L,
      state == "1-0-0" ~ 2L,
      state == "0-1-0" ~ 3L,
      state == "0-0-1" ~ 4L,
      state == "1-1-0" ~ 5L,
      state == "0-1-1" ~ 6L,
      state == "1-0-1" ~ 7L,
      state == "1-1-1" ~ 8L
    )) %>%
  group_by(ID, Tiller, Leaf) %>% arrange(t) %>%
  mutate(
    state2_l = lag(state2, 1)
  ) %>%
  arrange(ID2, t) %>%
  group_by(ID2) %>%
  filter(n() > 1)

hold2 <- hold2 %>%
  split(hold2$ID2) %>%
  purrr::map_dfr(
    .f = function(dt){
      add_row(
        ungroup(dt),
        ID     = dt$ID[1],
        Tiller = dt$Tiller[1],
        Leaf   = dt$Leaf[1],
        ID2    = dt$ID2[1],
        state  = "X",
        state2 = 9L,
        t      = max(dt$t) + 1,
        leaf_age = max(dt$leaf_age) + 1
      )
    }
  )


hold2 %>%
  group_by(ID2) %>%
  summarise(leaf_age = max(leaf_age)) %>%
  summarise(median(leaf_age))


unique(hold2$state2)

library(msm)

Q <- rbind(
  c(0, .1, .1, .1,  0,  0,  0,  0, .1),
  c(0,  0,  0,  0, .2,  0, .1, .1, .1),
  c(.1,  .1,  0,  0, .2, .1,  0, .1, .1),
  c(0,  0,  0,  0,  0, .1, .1, .1, .1),
  c(0,  .1,  0,  0,  0,  0,  0, .1, .1),
  c(0,  0,  0,  .1,  0,  0,  0, .1, .1),
  c(0,  0,  0,  0,  0,  0,  0, .1, .1),
  c(0,  0,  0,  0,  0,  0,  .1, 0, .1),
  c(0,  0,  0,  0,  0,  0,  0, 0, 0)
)
statetable.msm(state, ID2, hold2)

hold2 %>%
  filter(ID2 %in% c("247 1 2"))

Q.crude <- crudeinits.msm(state2 ~ leaf_age, ID2, data=hold2,
                            qmatrix=Q)

attempt <- msm(state2 ~ leaf_age, ID2, data=hold2,
                          qmatrix=Q.crude)

PP <- pmatrix.msm(attempt, t = 3)

PP

PP[4 , 7]/PP[4, 4]
PP[6 , 8]/PP[6, 6]

PP[1 , 2]/PP[1, 1]

