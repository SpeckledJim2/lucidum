# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options('golem.app.prod' = TRUE)

kpi_spec <- system.file('insurance_kpi_spec.csv', package="lucidum")
feature_spec <- system.file('insurance_feature_spec.csv', package="lucidum")
filter_spec <- system.file('insurance_filter_spec.csv', package="lucidum")

lucidum::lucidum(
  data = insurance,
  BoostaR_models = NULL,
  GlimmaR_models = NULL,
  show_DevelopaR = TRUE,
  show_MappaR = TRUE,
  starting_response = 'price',
  starting_tab = 'DataR',
  kpi_spec = kpi_spec,
  feature_spec = feature_spec,
  filter_spec = filter_spec,
  sidebar_width = 250,
  num_threads = 1
  )
