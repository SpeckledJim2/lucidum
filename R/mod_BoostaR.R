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
    tabsetPanel(id = 'BoostaR_tabsetPanel',
                tabPanel(span(tagList(icon('bars'), 'Features and parameters')),
                         mod_buildBoostaR_ui(ns('buildBoostaR'))
                         ),
                tabPanel(span(tagList(icon('table-columns'), 'Model navigator')),
                         mod_navigateBoostaR_ui(ns('navigateBoostaR'))
                         )
    ),
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

add_col <- function(d, dt_update){
  
}