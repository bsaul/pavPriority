#------------------------------------------------------------------------------#
# Functions both for preparing and carrying out multistate modeling
#------------------------------------------------------------------------------#

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

#
