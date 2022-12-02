# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
#glucidum(insurance)
insurance[, birthday:= as.Date('1976-11-24')]
glucidum(data = insurance,
         starting_response = 'price',
         starting_tab = 'ChartaR',
         show_MappaR = F,
         show_DevelopaR = F,
         show_GlimmaR = F,
         kpi_spec = 'data/insurance_kpi_spec.csv',
         feature_spec = 'data/insurance_feature_spec.csv',
         filter_spec = 'data/insurance_filter_spec.csv')
