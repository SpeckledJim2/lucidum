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
                tabPanel(value = 'Model formula', span(tagList(tags$img(src='www/beta.png', height="30px", width="30px"), 'Model formula')),
                         mod_GlimmaR_build_model_ui(ns('buildGlimmaR'))
                         ),
                tabPanel(value = 'Model navigator', span(tagList(tags$img(src='www/GlimmaR_navigate.png', height="30px", width="30px"), 'Model navigator')),
                         mod_GlimmaR_navigate_ui(ns('navigateGlimmaR'))
                ),
                tabPanel(value = 'Tabulated models', span(tagList(tags$img(src='www/tabulate.png', height="30px", width="30px"), 'Tabulated models')),
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
    mod_GlimmaR_tabulated_models_server('tabulateGlimmaR', GlimmaR_models)
    observeEvent(c(GlimmaR_models(), GlimmaR_idx()), {
      if(!is.null(GlimmaR_models()) & !is.null(GlimmaR_idx())){
        # copy model predictions to d
        g <- GlimmaR_models()[[GlimmaR_idx()]]
        rows_idx <- g$pred_rows
        preds <- g$predictions
        if('glm_prediction' %in% names(d())){
          d()[, glm_prediction:= NULL]
        }
        # if there are tabulated predictions copy those
        if('glm_tabulated_prediction' %in% names(d())){
          d()[, glm_tabulated_prediction:= NULL]
        }
        if(!is.null(g$tabulated_predictions)){
          x <- g$tabulated_predictions$total
          x <- link_function(x, g$link)
          d()[rows_idx, glm_tabulated_prediction := x]
        }
        
        d()[rows_idx, glm_prediction := preds]
        dt_update(dt_update()+1)
        # copy LP cols
        existing_LP_cols <- names(d())[grep('_LP_', names(d()))] # get rid of any existing SHAP columns
        if(length(existing_LP_cols)>0){
          d()[, (existing_LP_cols) := NULL]
        }
        new_LP_idx <- g$pred_rows
        new_LP_cols <- g$LP_contributions
        if(!is.null(new_LP_cols)){
          LP_names <- names(new_LP_cols)
          d()[new_LP_idx, (LP_names) := new_LP_cols]
        }
      }
    })
    
  })
}

link_function <- function(x, link){
  if(link=='identity'){
    x
  } else if(link=='log'){
    exp(x)
  } else if(link=='logit'){
    exp(x)/(1+exp(x))
  }
}

