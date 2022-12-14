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
    tabsetPanel(
      id = ns('tabsetPanel'),
      tabPanel(
        value = 'Dataset viewer',
        title = span(tagList(tags$img(src='www/dataset_viewer.png', height="20px", width="20px"),'Dataset viewer')),
        mod_DataR_datasetViewer_ui(ns('datasetViewer'))
        ),
      tabPanel(
        value = 'Column summary',
        title = span(tagList(tags$img(src='www/column_summary.png', height="20px", width="20px"),'Column summary')),
        mod_DataR_columnSummary_ui(ns('columnSummary'))
        )
    ),
  )
}
    
#' DataR Server Functions
#'
#' @noRd 
mod_DataR_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_DataR_datasetViewer_server('datasetViewer', d, dt_update)
    mod_DataR_columnSummary_server('columnSummary', d, dt_update)
  })
}
