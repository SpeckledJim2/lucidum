#' DataR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList span
mod_DataR_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns('tabsetPanel'),
                tabPanel(value = 'Dataset viewer', span(tagList(icon('bars'), 'Dataset viewer')), mod_datasetViewer_ui(ns('datasetViewer'))),
                tabPanel(value = 'Column summary', span(tagList(icon('table-columns'), 'Column summary')), mod_columnSummary_ui(ns('columnSummary')))
    ),
  )
}
    
#' DataR Server Functions
#'
#' @noRd 
mod_DataR_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_datasetViewer_server('datasetViewer', d, dt_update)
    mod_columnSummary_server('columnSummary', d, dt_update)
  })
}
