#' navigateGlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput
mod_GlimmaR_navigate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        DTOutput(ns('GlimmaR_model_summary'))
      )
    )
  )
}
    
#' navigateGlimmaR Server Functions
#'
#' @noRd 
mod_GlimmaR_navigate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_navigateGlimmaR_ui("navigateGlimmaR_1")
    
## To be copied in the server
# mod_navigateGlimmaR_server("navigateGlimmaR_1")
