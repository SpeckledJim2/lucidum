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
                tabPanel(value = 'Histogram', title = span(tagList(tags$img(src='www/histogram.png', height="20px", width="20px"), 'Histogram')),
                         br(),
                         mod_histogram_ui(ns('histogram_tab'))
                         ),
                tabPanel(value = '1-way line and bar', title = span(tagList(tags$img(src='www/one_way_line_bar.png', height="20px", width="20px"),'1-way line and bar')),
                         br()
                         ),
                tabPanel(value = '2-way line and bar', title = span(tagList(tags$img(src='www/one_way_line_bar.png', height="20px", width="20px"), '2-way line and bar')), br()),
                tabPanel(value = 'Box plot', title = span(tagList(tags$img(src='www/one_way_line_bar.png', height="18px", width="18px"), 'Box plot')), br()),
                tabPanel(value = 'Scatterplot', title = span(tagList(tags$img(src='www/one_way_line_bar.png', height="15px", width="15px"), 'Scatterplot')), br())
                )
  )
}
    
#' ChartaR Server Functions
#'
#' @noRd 
mod_ChartaR_server <- function(id, d, dt_update, response, weight, kpi_spec){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_histogram_server('histogram_tab', d, dt_update, response, weight, kpi_spec)
  })
}
    
## To be copied in the UI
# mod_ChartaR_ui("ChartaR_1")
    
## To be copied in the server
# mod_ChartaR_server("ChartaR_1")
