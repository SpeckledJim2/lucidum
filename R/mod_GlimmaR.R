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
                tabPanel(value = 'Model formula', span(tagList(tags$img(src='www/beta.png', height="20px", width="20px"), 'Model formula')),
                         mod_GlimmaR_build_model_ui(ns('buildGlimmaR'))
                         ),
                tabPanel(value = 'Model navigator', span(tagList(tags$img(src='www/GlimmaR_navigate.png', height="20px", width="20px"), 'Model navigator')),
                         mod_GlimmaR_navigate_ui(ns('navigateGlimmaR'))
                ),
                tabPanel(value = 'Tabulated models', span(tagList(tags$img(src='www/tabulate.png', height="20px", width="20px"), 'Tabulated models')),
                         mod_GlimmaR_tabulated_models_ui(ns('tabulateGlimmaR'))
                         )
    ),
  )
}
    
#' GlimmaR Server Functions
#'
#' @noRd 
mod_GlimmaR_server <- function(id, d, dt_update, response, weight, feature_spec, GlimmaR_models, GlimmaR_idx, BoostaR_models, BoostaR_idx, crosstab_selector){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    tabulated_models <- reactiveVal(list())
    mod_GlimmaR_build_model_server('buildGlimmaR', d, dt_update, response, weight, GlimmaR_models, GlimmaR_idx, BoostaR_models, BoostaR_idx, crosstab_selector)
    mod_GlimmaR_navigate_server('navigateGlimmaR', d, response, weight, feature_spec, GlimmaR_models, GlimmaR_idx, tabulated_models)
    mod_GlimmaR_tabulated_models_server('tabulateGlimmaR', tabulated_models)
    observeEvent(GlimmaR_idx(), {
      if(!is.null(GlimmaR_idx())){
        # copy model predictions to d
        rows_idx <- GlimmaR_models()[[GlimmaR_idx()]]$pred_rows
        preds <- GlimmaR_models()[[GlimmaR_idx()]]$predictions
        if('glm_prediction' %in% names(d())){
          d()[, glm_prediction:= NULL]
        }
        d()[rows_idx, glm_prediction := preds]
        dt_update(dt_update()+1)
        # copy LP cols
        existing_LP_cols <- names(d())[grep('_LP_', names(d()))] # get rid of any existing SHAP columns
        if(length(existing_LP_cols)>0){
          d()[, (existing_LP_cols) := NULL]
        }
        new_LP_idx <- GlimmaR_models()[[GlimmaR_idx()]]$pred_rows
        new_LP_cols <- GlimmaR_models()[[GlimmaR_idx()]]$LP_contributions
        if(!is.null(new_LP_cols)){
          LP_names <- names(new_LP_cols)
          d()[new_LP_idx, (LP_names) := new_LP_cols]
        }
      }
    })
    
  })
}


