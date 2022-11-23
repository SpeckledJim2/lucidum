#' DataR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DataR_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        DT::DTOutput(ns('my_data'), height = '90vh')
      )
    )
  )
}
    
#' DataR Server Functions
#'
#' @noRd 
mod_DataR_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$my_data <- DT::renderDT({
      dt_update()
      format_table(d())
    })
  })
}
