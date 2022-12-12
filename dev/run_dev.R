# Set options here
# TRUE = production mode, FALSE = development mode
options(golem.app.prod = FALSE) 

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

kpi_spec <- system.file('insurance_kpi_spec.csv', package="glucidum")
feature_spec <- system.file('insurance_feature_spec.csv', package="glucidum")
filter_spec <- system.file('insurance_filter_spec.csv', package="glucidum")

# Run the application
glucidum(
  data = insurance,
  starting_response = 'price',
  starting_tab = 'ChartaR',
  show_DevelopaR = T,
  show_DataR = T,
  show_ChartaR = T,
  show_MappaR = F,
  show_BoostaR = T,
  show_GlimmaR = T,
  show_dataset_chooser = F,
  num_threads = 30,
  kpi_spec = kpi_spec,
  feature_spec = feature_spec,
  filter_spec = filter_spec
  )
