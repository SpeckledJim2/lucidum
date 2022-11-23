# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
glucidum::glucidum(insurance,
                   show_ChartaR = FALSE,
                   show_DevelopaR = FALSE,
                   show_GlimmaR = FALSE,
                   show_BoostaR = FALSE) # add parameters here (if any)
