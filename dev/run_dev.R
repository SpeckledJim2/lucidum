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

# Run the application
glucidum(
  data = insurance,
  BoostaR_models = NULL,
  GlimmaR_models = NULL,
  starting_response = 'price',
  starting_tab = 'ChartaR',
  show_DevelopaR = T,
  show_DataR = T,
  show_ChartaR = T,
  show_MappaR = T,
  show_BoostaR = T,
  show_GlimmaR = T,
  num_threads = -1,
  specification_path = 'inst'
  )
