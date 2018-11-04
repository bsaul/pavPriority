#------------------------------------------------------------------------------#
#    TITLE : Multistate analysis functions
#     DATE : 2018NOV03
#   AUTHOR : B. Saul
#  PURPOSE : functions to create a multistate analysis dataset
#------------------------------------------------------------------------------#

## Creating the analysis dataset ####

create_interference_indicators <- function(.data, .cutpoint = 0.25){
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

create_states <- function(.data, .states = c("C", "R")){
  .data %>%
    ungroup() %>%
    mutate(
      state  = factor(paste(!!!rlang::syms(.states), sep = "-")),
      stateL = as.integer(state))
}

# TODO
create_q_matrix <- function(nstates){}

## Fitting an MSM
fit_msm <- function(formula, subject, data, covariates = NULL, 
                    startQ = NULL, Q = NULL,
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

## Extracting results from an msm model ####

compare_probs <- function(msmfit, t, from, to, state_labels, .covariates = "mean"){
  
  purrr::map_dfr(.covariates, function(covars){
    P  <- msm::pmatrix.msm(msmfit, t = t, covariates = covars)
    p1 <- apply(P, 3, function(x) x[from, to])
    p2 <- apply(P, 3, function(x) x[from, from])
    
    to_label   <- state_labels[to]
    from_label <- state_labels[from]

    data_frame(
      comparison = sprintf("p1 = Pr(%s|%s)\np2 = Pr(%s|%s)", to_label, from_label, from_label, from_label),
      t = t,
      p1 = p1,
      p2 = p2,
      rd = p1 - p2,
      rr = p1/p2) %>%
      {if(.covariates[1] != "mean") cbind(., as.data.frame(covars)) else . }
  })

}
