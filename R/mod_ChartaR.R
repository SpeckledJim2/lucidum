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
                tabPanel(value = 'Histogram', title = span(tagList(tags$img(src='www/histogram.png', height="20px", width="18px"), 'Histogram')), br()),
                tabPanel(value = '1-way line and bar', title = span(tagList(tags$img(src='www/one_way_line_bar.png', height="20px", width="25px"), '1-way line and bar')), br()),
                tabPanel(value = '2-way line and bar', title = span(tagList(icon('chart-line'), '2-way line and bar')), br()),
                tabPanel(value = 'Box plot', title = span(tagList(icon('chart-line'), 'Box plot')), br()),
                tabPanel(value = 'Scatterplot', title = span(tagList(icon('chart-line'), 'Scatterplot')), br())
                )
  )
}
    
#' ChartaR Server Functions
#'
#' @noRd 
mod_ChartaR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ChartaR_ui("ChartaR_1")
    
## To be copied in the server
# mod_ChartaR_server("ChartaR_1")
