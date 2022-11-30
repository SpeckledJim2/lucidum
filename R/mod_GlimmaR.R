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
    tabsetPanel(id = ns('tabsetPanel'),
                tabPanel(value = 'Model formula', span(tagList(icon('bars'), 'Model formula')),
                         mod_GlimmaR_build_model_ui(ns('buildGlimmaR'))
                         ),
                tabPanel(value = 'Tabulated models', span(tagList(icon('bars'), 'Tabulated models')),
                         mod_GlimmaR_tabulated_models_ui(ns('tabulatedGlimmaR'))
                         ),
                tabPanel(value = 'Model navigator', span(tagList(icon('table-columns'), 'Model navigator')),
                         mod_GlimmaR_navigate_ui(ns('navigateGlimmaR'))
                         )
    ),
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
