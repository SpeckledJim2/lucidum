# Set options her
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
glucidum(data = insurance,
         kpi_spec = 'data-raw/insurance_kpi_spec.csv',
         feature_spec = 'data-raw/insurance_feature_spec.csv',
         filter_spec = 'data-raw/insurance_filter_spec.csv',
         show_DevelopaR = F,
         show_GlimmaR = F,
         show_BoostaR = F)


