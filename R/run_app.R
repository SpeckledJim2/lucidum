#' Launch glucidum
#'
#' @param data data.frame or data.table to analyse in lucidum.
#' @param BoostaR_models character, path to .rds file containing BoostaR_models.
#' @param GlimmaR_models character, path to .rds file containing GlimmaR_models.
#' @param feature_spec character, path to feature specification.
#' @param filter_spec character, path to filter specification.
#' @param kpi_spec character, path to kpi specification.
#' @param show_ChartaR logical, TRUE (default) will show the ChartaR menu item.
#' @param show_MappaR logical, TRUE (default) will show the MappaR menu item.
#' @param show_BoostaR logical, TRUE (default) will show the BoostaR menu item.
#' @param show_GlimmaR logical, TRUE (default) will show the GlimmaR menu item.
#' @param show_DevelopaR logical, TRUE (not default) will show the ShinyAce console.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
glucidum <- function(data=NULL,
                    BoostaR_models = NULL,
                    GlimmaR_models = NULL,
                    feature_spec = NULL,
                    filter_spec = NULL,
                    kpi_spec = NULL,
                    show_ChartaR = TRUE,
                    show_MappaR = TRUE,
                    show_BoostaR = TRUE,
                    show_GlimmaR = TRUE,
                    show_DevelopaR = TRUE
                    ) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list(data=data,
                      feature_spec = feature_spec,
                      filter_spec = filter_spec,
                      kpi_spec = kpi_spec,
                      show_ChartaR = show_ChartaR,
                      show_MappaR = show_MappaR,
                      show_BoostaR = show_BoostaR,
                      show_GlimmaR = show_GlimmaR,
                      show_DevelopaR = show_DevelopaR)
  )
}
