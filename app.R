# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
glucidum::glucidum(
  insurance,
  kpi_spec = 'data/insurance_kpi_spec.csv',
  feature_spec = 'data/insurance_feature_spec.csv',
  filter_spec = 'data/insurance_filter_spec.csv'
  )
