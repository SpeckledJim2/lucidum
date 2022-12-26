#' ChartaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ChartaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns('tabsetPanel'),
                tabPanel(value = '1-way line and bar',
                         title = span(tagList(tags$img(src='www/one_way_line_bar.png', height="30px", width="30px"),'1-way line and bar')),
                         br(),
                         mod_ChartaR_line_and_bar_ui(ns('line_and_bar'))
                ),
                tabPanel(value = 'Histogram',
                         title = span(tagList(tags$img(src='www/histogram.png', height="30px", width="30px"),'Histogram')),
                         br(),
                         mod_histogram_ui(ns('histogram_tab'))
                         ),
                tabPanel(value = 'SHAP', title = span(tagList(tags$img(src='www/SHAP.png', height="30px", width="30px"), 'SHAP')),
                         br(),
                         mod_ChartaR_SHAP_ui(ns('SHAP'))
                         )
                )
  )
}
    
#' ChartaR Server Functions
#'
#' @noRd 
mod_ChartaR_server <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_histogram_server('histogram_tab', d, dt_update, response, weight, kpi_spec)
    mod_ChartaR_line_and_bar_server('line_and_bar', d, dt_update, response, weight, kpi_spec, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx)
    mod_ChartaR_SHAP_server('SHAP', d, dt_update, weight, BoostaR_models, BoostaR_idx, feature_spec)
  })
}
    
## To be copied in the UI
# mod_ChartaR_ui("ChartaR_1")
    
## To be copied in the server
# mod_ChartaR_server("ChartaR_1")
