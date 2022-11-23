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
    h2('ChartaR')
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
