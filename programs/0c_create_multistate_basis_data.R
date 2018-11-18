#------------------------------------------------------------------------------#
#    TITLE : Create data for basis of multistate functions
#     DATE : 2018NOV03
#   AUTHOR : B. Saul
#  PURPOSE : 
#------------------------------------------------------------------------------#

set_remaining_infected <- function(x){
  # finds the first element of x == 1, then sets any elements after that to 1
  n <- length(x)
  if(sum(x) == 0 || (sum(x) == 1 & x[n] == 1)){ return(x) }
  
  # Find first x[i] == 1
  s <- min(which(x == 1))
  
  x[(s+1):n] <- 1
  x
}

create_multistate_basis <- function(.data, .days_after_last_obs = 7,
                                   .keep_covariates = c("Endophyte")){
  .data %>%
    group_by(LeafID) %>%
    select(-leaf_damage) %>%
    group_by(ID, Tiller) %>%
    tidyr::spread(key = pathogen, value = infected) %>%
    mutate(
      t        = days_since_trial_start,
      leaf_age = days_since_emergence,
      w        = as.integer(weeks_since_trial_start)
    ) %>%
    select(date, LeafID, ID, Tiller, Leaf, PAV, t, leaf_age, cohort, current_experimental_period,
           C = Col, R = Rhiz, P = Rust, one_of(.keep_covariates))  %>% 
    group_by(LeafID) %>% 
    arrange(t) %>%
    mutate(
      emergent_experimental_period = current_experimental_period[1],
      C = set_remaining_infected(C),
      R = set_remaining_infected(R),
      P = set_remaining_infected(P)
    ) %>% 
    group_by(ID, Tiller, t) %>%
    # Compute proportion of other leaves on same tiller infected with C & R
    # at time t
    # TODO: this needs to be changed to find proportion of leaves in past X number of days.
    mutate(
      D  = 0L,
      n_leaves = n(),
      pC = if_else(n_leaves > 1, (sum(C) - C)/(n_leaves - 1), 0),
      pR = if_else(n_leaves > 1, (sum(R) - R)/(n_leaves - 1), 0),
      pP = if_else(n_leaves > 1, (sum(P) - P)/(n_leaves - 1), 0)
    ) %>% 
    # Add a row after the last observation per leaf for death
    # TODO: end of study censoring
    split(., .$LeafID) %>%
    purrr::map_dfr(
      .f = function(dt){
        add_row(
          ungroup(dt),
          date   = dt$date[nrow(dt)] + .days_after_last_obs,
          LeafID = dt$LeafID[1],
          ID     = dt$ID[1],
          Tiller = dt$Tiller[1],
          Leaf   = dt$Leaf[1],
          PAV    = dt$PAV[1],
          t      = max(dt$t) + .days_after_last_obs,
          cohort = dt$cohort[1],
          D      = 1L,
          leaf_age = max(dt$leaf_age) + .days_after_last_obs,
          emergent_experimental_period = dt$emergent_experimental_period[1],
          current_experimental_period = dt$current_experimental_period[1],
        )
      }
    ) %>%
    select(LeafID, ID, Tiller, Leaf, PAV, C, pC, R, pR, P, pP, D, everything())
}

# Add weather covariates ####
add_weather_vars <- function(.basis_data, .weather_data){
  weather_values <- .basis_data %>%
    distinct(date) %>%
    dplyr::mutate(
      avg_hi_temp = make_weather_variable("temp_max", mean, date, 6, .weather_data),
      precip      = make_weather_variable("precip", sum, date, 6, .weather_data)
    )
  
  .basis_data %>%
    left_join(weather_values, by = "date")
  
}

## Create data for PAV analysis ####
pav_dt <- dt2 %>%
  # Remove:
  # - leaves any with a single observatino
  # - missing PAV
  filter(n() > 1 | is.na(PAV)) %>%
  create_multistate_basis() %>%
  add_weather_vars(weather) %>%
  # TODO: handling cleanup here but could be handled in data creating function
  group_by(LeafID) %>%
  mutate(
    Endophyte = if_else(is.na(Endophyte), lag(Endophyte), Endophyte), 
    endo = case_when(
      Endophyte == "endo(-)" ~ 0L,
      Endophyte == "endo(+)" ~ 1L
    )
  ) %>%
  select(-Endophyte)

## Save data ####
save(pav_dt, file = "data/multistate_basis_pav.rda")