### Developing a 'tidy' randomization framework

develop_this <- function(observed_data, causal_model, possible_assignments, test_statistic){
  od <- causal_model(observed_data)
  ob_ts <- test_statistic(od)
  
  
}

compute_distrubution
compute_test_statistic
compute_pvalue