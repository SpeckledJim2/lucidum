#' DevelopaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DevelopaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns('tabsetPanel'),
                tabPanel(value = 'KPI specification', title = span(tagList(icon('gears'), 'KPI specification')), br()),
                tabPanel(value = 'Feature specification', title = span(tagList(icon('list'), 'Feature specification')), br()),
                tabPanel(value = 'Filter specification', title = span(tagList(icon('filter'), 'Filter specification')), br()),
                tabPanel(value = 'shinyAce', title = span(tagList(icon('chevron-right'), 'shinyAce')),
                         shinyAce::aceEditor(
                           ns('shinyAce_code'),
                           height = "calc(50vh - 10px)",
                           mode = "r",
                           wordWrap = FALSE,
                           autoScrollEditorIntoView = TRUE,
                           placeholder = ''
                           )
                         )
                )
    )
}
    
#' DevelopaR Server Functions
#'
#' @noRd 
mod_DevelopaR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_DevelopaR_ui("DevelopaR_1")
    
## To be copied in the server
# mod_DevelopaR_server("DevelopaR_1")
