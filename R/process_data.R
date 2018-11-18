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

