#' Launch glucidum
#'
#' @param data data.frame or data.table to analyse in lucidum.
#' @param BoostaR_models character, path to .rds file containing BoostaR_models.
#' @param GlimmaR_models character, path to .rds file containing GlimmaR_models.
#' @param kpi_spec character, path to kpi specification.
#' @param filter_spec character, path to filter specification.
#' @param feature_spec character, path to feature specification.
#' @param show_dataset_chooser logical, TRUE (not default) will show the dataset chooser.
#' @param show_DataR logical, TRUE (not default) will show the DataR menu item.
#' @param show_ChartaR logical, TRUE (default) will show the ChartaR menu item.
#' @param show_MappaR logical, TRUE (default) will show the MappaR menu item.
#' @param show_BoostaR logical, TRUE (default) will show the BoostaR menu item.
#' @param show_GlimmaR logical, TRUE (default) will show the GlimmaR menu item.
#' @param show_DevelopaR logical, TRUE (not default) will show the ShinyAce console.
#' @param starting_tab character, name of tab to show on startup.
#' @param starting_response character, name of response column to show on startup.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
glucidum <- function(data=NULL,
                     BoostaR_models = NULL,
                     GlimmaR_models = NULL,
                     kpi_spec = NULL,
                     filter_spec = NULL,
                     feature_spec = NULL,
                     show_dataset_chooser = F,
                     show_DataR = T,
                     show_ChartaR = T,
                     show_MappaR = T,
                     show_BoostaR = T,
                     show_GlimmaR = T,
                     show_DevelopaR = T,
                     starting_tab = 'DataR',
                     starting_response = NULL
                     ) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list(data = data,
                      kpi_spec = kpi_spec,
                      filter_spec = filter_spec,
                      feature_spec = feature_spec,
                      show_dataset_chooser = show_dataset_chooser,
                      show_DataR = show_DataR,                      
                      show_ChartaR = show_ChartaR,
                      show_MappaR = show_MappaR,
                      show_BoostaR = show_BoostaR,
                      show_GlimmaR = show_GlimmaR,
                      show_DevelopaR = show_DevelopaR,
                      starting_tab = starting_tab,
                      starting_response = starting_response
                      )
  )
}
