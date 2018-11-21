#------------------------------------------------------------------------------#
# Functions for preparing PAV data for multistate analysis
#------------------------------------------------------------------------------#

#' Add indicators of infection on other leaves with a plant
#' 
#' @param .data \code{multistate_basis_pav.rds}
#' @param .cutpoint the threshold at which to define the indicators
#' @export
add_interference_indicators <- function(.data, .cutpoint = 0.25){
  .data %>%
    mutate(
      pC = case_when(
        0   <= pC & pC <= .cutpoint ~ 0,
        .cutpoint <= pC & pC <= 1   ~ 1),
      pR = case_when(
        0   <=  pR & pR <= .cutpoint ~ 0,
        .cutpoint <= pR & pR <= 1    ~ 1),
      pP = case_when(
        0   <=  pP & pP <= .cutpoint ~ 0,
        .cutpoint <= pP & pP <= 1   ~ 1)
    )
}

#' Add indicators of infection on other leaves with a plant
#' 
#' @param .data \code{multistate_basis_pav.rds}
#' @param .states the names of variables to use in defining states. Note that death
#' is automatically included.
#' @export
add_states <- function(.data, .states = c("C", "R")){
  .data %>%
    ungroup() %>%
    mutate(
      state  = paste(!!!rlang::syms(.states), sep = "-"),
      state  = factor(if_else(grepl("NA", state), "dead", state)), 
      stateL = as.integer(state))
}


#' View the indices of the transitions
#' 
#' @param qmatrix a Q matrix
#' @export

view_q_indices <- function(qmatrix){
  Q <- qmatrix
  diag(Q) <- 0
  # MSM creates indices for the transition intensities by row
  t(matrix(cumsum(as.vector(t(Q))) * as.vector(t(Q)), nrow = nrow(Q)))
}

#' Create a data frame of possible transitions
#' 
#' @param qmatrix a Q matrix
#' @param states a data.frame of possible states
#' @export

create_q_dataset <- function(qmatrix, states){
  Q <- qmatrix 
  diag(Q) <- 0
  
  as.data.frame(which(Q & TRUE, arr.ind = TRUE)) %>%
    select(from = row, to = col) %>%
    arrange(from) %>%
    mutate(qind = 1:n()) %>%
    left_join(states, by = c("from" = "stateL")) %>%
    left_join(states, by = c("to" = "stateL")) %>%
    select(qind, everything())
}

#' Prepare analysis data objects
#' 
#' @param .data \code{multistate_basis_pav.rds}
#' @param states_of_interest the names of variables to use in defining states. Note that death
#' is automatically included.
#' @export

prepare_analysis_data <- function(.data, states_of_interest){
  
  leaf_pathogen_states <- states_of_interest[!grepl("^p", states_of_interest)]
  
  analysis_dt <- .data %>%
    add_interference_indicators(.cutpoint = 0.25) %>%
    add_states(.states = states_of_interest)
  
  states_dt <- analysis_dt %>%
    distinct(!!! rlang::syms(states_of_interest), state, stateL) %>%
    arrange(stateL) 
  
  qmatrix <- find_noreturn_states(1:nrow(states_dt),  leaf_pathogen_states, states_dt) %>%
    create_possible_transition_matrix()
  
  qdat <- create_q_dataset(qmatrix, states_dt)
  
  list(
    data    = analysis_dt,
    qmatrix = qmatrix, 
    states  = states_dt, 
    qdata   = qdat,
    lps     = leaf_pathogen_states, soi = states_of_interest
  )
}
