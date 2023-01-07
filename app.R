# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# QUESTION - what is the next line doing exactly?
# when my app is "finished" could I just use library(glucidum)?
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options('golem.app.prod' = TRUE)

kpi_spec <- system.file('insurance_kpi_spec.csv', package="glucidum")
feature_spec <- system.file('insurance_feature_spec.csv', package="glucidum")
filter_spec <- system.file('insurance_filter_spec.csv', package="glucidum")

glucidum::glucidum(
  data = insurance,
  BoostaR_models = NULL,
  GlimmaR_models = NULL,
  show_DevelopaR = T,
  show_MappaR = T,
  starting_response = 'price',
  starting_tab = 'ChartaR',
  kpi_spec = kpi_spec,
  feature_spec = feature_spec,
  filter_spec = filter_spec,
  num_threads = 1
  )
