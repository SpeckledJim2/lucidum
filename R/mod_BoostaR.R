#' BoostaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_BoostaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2('BoostaR'),
    fluidRow(
      column(
        width = 12,
        actionButton(inputId=ns('add_col'), label = 'add_col'),
      )
    )
  )
}
    
#' BoostaR Server Functions
#'
#' @noRd 
mod_BoostaR_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$add_col, {
      add_col(d(), dt_update())
      dt_update(dt_update()+1)
    })
  })
}
    
## To be copied in the UI
# mod_BoostaR_ui("BoostaR_1")
    
## To be copied in the server
# mod_BoostaR_server("BoostaR_1")
