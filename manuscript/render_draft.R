vers <- "V008"
rmarkdown::render(
  input = "manuscript/causal_priority_effects.Rmd",
  output_file = paste0("causal_priority_effects_", vers, ".pdf"))

