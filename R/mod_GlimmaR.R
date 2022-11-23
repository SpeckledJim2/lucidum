#' GlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_GlimmaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2('GlimmaR')
  )
}
    
#' GlimmaR Server Functions
#'
#' @noRd 
mod_GlimmaR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_GlimmaR_ui("GlimmaR_1")
    
## To be copied in the server
# mod_GlimmaR_server("GlimmaR_1")
