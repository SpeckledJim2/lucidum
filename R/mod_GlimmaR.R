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
mod_GlimmaR_server <- function(id, d, dt_update, response, weight, GlimmaR_models, GlimmaR_idx, BoostaR_models, BoostaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_GlimmaR_build_model_server('buildGlimmaR', d, dt_update, response, weight, GlimmaR_models, GlimmaR_idx, BoostaR_models, BoostaR_idx)
    observeEvent(GlimmaR_idx(), {
      if(!is.null(GlimmaR_idx())){
        # copy model predictions to d
        rows_idx <- GlimmaR_models()[[GlimmaR_idx()]]$pred_rows
        preds <- GlimmaR_models()[[GlimmaR_idx()]]$predictions
        d()[rows_idx, glm_prediction := preds]
        dt_update(dt_update()+1)
        # copy LP cols
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_GlimmaR_ui("GlimmaR_1")
    
## To be copied in the server
# mod_GlimmaR_server("GlimmaR_1")
