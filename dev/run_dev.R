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
#glucidum(insurance)
insurance[, birthday:= as.Date('1976-11-24')]
insurance[1, birthday:= as.Date('1976-10-24')]
insurance[2, birthday:= as.Date('1976-09-24')]
insurance[3, birthday:= as.Date('1976-11-25')]
glucidum(data = insurance,
         starting_response = 'price',
         starting_tab = 'ChartaR',
         show_DevelopaR = T,
         show_DataR = T,
         show_ChartaR = T,
         show_MappaR = T,
         show_BoostaR = T,
         show_GlimmaR = T,
         kpi_spec = 'data/insurance_kpi_spec.csv',
         feature_spec = 'data/insurance_feature_spec.csv',
         filter_spec = 'data/insurance_filter_spec.csv')
