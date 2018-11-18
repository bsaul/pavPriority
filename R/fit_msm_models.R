#------------------------------------------------------------------------------#
# Functions both for preparing and carrying out multistate modeling
#------------------------------------------------------------------------------#


#' Find other states that a reference state cannot go to
#' 
#' @name define_transitions
#' @param ref a vector of reference states
#' @param others a matrix of other states
#' @export

compare_reference <- function(ref, others){
  apply(others, 1, function(r) any((r == 0) & (ref == 1)))
}

#' @rdname define_transitions
#' @export
find_noreturn_states <- function(ref_index, no_return_states, state_dt){
  n_states <- nrow(state_dt)
  if(ref_index == n_states){
    return(list(1:(n_states - 1)))
  }
  row <- state_dt[ref_index, no_return_states]
  # nrow(state_dt) is dead - handle this as special case
  sts <- state_dt[-c(ref_index, n_states), "stateL"][[1]] 
  x <- state_dt[-c(ref_index, n_states), no_return_states]
  y <- compare_reference(row, x)
  list(sts[y])
}

find_noreturn_states <- Vectorize(find_noreturn_states, vectorize.args = "ref_index")

#' @rdname define_transitions
#' @export

create_possible_transition_matrix <- function(exclude_states_list){
  hold <- logical(length(exclude_states_list))
  do.call("rbind", lapply(exclude_states_list, function(x) {hold[x] <- TRUE; !hold})) * 1L
}

#' Fit an MSM for the PAV data
#' 
#' TODO: Fill in params
#' 
#' @importFrom msm msm crudeinits.msm
#' @export

fit_msm <- function(formula, 
                    subject,
                    data, 
                    covariates = NULL, 
                    startQ = NULL, 
                    Q = NULL,
                    censor = NULL, ...){
  
  if(is.null(Q)){
    Q_crude <- msm::crudeinits.msm(
      formula = formula,
      subject = subject,
      data    = data,
      censor  = censor,
      qmatrix = startQ)
  } else {
    Q_crude = Q
  }
  
  msm::msm(
    formula    = formula,
    subject    = subject,
    covariates = covariates, 
    data       = data,
    censor     = censor,
    qmatrix    = Q_crude,
    ...)
}
