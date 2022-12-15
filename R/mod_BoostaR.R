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
    tabsetPanel(id = ns('tabsetPanel'),
                tabPanel(value = 'Features and parameters', span(tagList(tags$img(src='www/BoostaR_features.png', height="20px", width="20px"), 'Features and parameters')),
                         mod_BoostaR_build_model_ui(ns('buildBoostaR'))
                         ),
                tabPanel(value = 'Model navigator', span(tagList(tags$img(src='www/BoostaR_navigate.png', height="20px", width="20px"), 'Model navigator')),
                         mod_BoostaR_navigate_ui(ns('navigateBoostaR'))
                         ),
                tabPanel(value = 'Tree viewer', title = span(tagList(tags$img(src='www/tree.png', height="20px", width="20px"), 'Tree viewer')),
                         mod_BoostaR_tree_viewer_ui(ns('treeViewer'))
                )
    ),
  )
}
    
#' BoostaR Server Functions
#'
#' @noRd 
mod_BoostaR_server <- function(id, d, dt_update, response, weight, feature_spec, BoostaR_models, BoostaR_idx, dimensions, crosstab_selector){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_BoostaR_build_model_server('buildBoostaR', d, dt_update, response, weight, feature_spec, BoostaR_models, BoostaR_idx, dimensions, crosstab_selector)
    mod_BoostaR_navigate_server('navigateBoostaR', BoostaR_models, BoostaR_idx, crosstab_selector)
    mod_BoostaR_tree_viewer_server('treeViewer', BoostaR_models, BoostaR_idx)
    observeEvent(BoostaR_idx(), {
      if(!is.null(BoostaR_idx())){
        if(BoostaR_idx()=='No GBMs'){
          # e.g. if user deletes all the GBMs we will arrive here
          # delete the remaining GBM cols
          if('lgbm_prediction' %in% names(d())){
            d()[, lgbm_prediction:= NULL]
            dt_update(dt_update()+1)
          }
          existing_SHAP_cols <- names(d())[grep('_SHAP_', names(d()))] # get rid of any existing SHAP columns
          if(length(existing_SHAP_cols)>0){
            d()[, (existing_SHAP_cols) := NULL]
            dt_update(dt_update()+1)
          }
        } else {
          # copy model predictions to d
          rows_idx <- BoostaR_models()[[BoostaR_idx()]]$pred_rows
          preds <- BoostaR_models()[[BoostaR_idx()]]$predictions
          if('lgbm_prediction' %in% names(d())){
            d()[, lgbm_prediction:= NULL]
            dt_update(dt_update()+1)
          }
          d()[rows_idx, lgbm_prediction := preds]
          dt_update(dt_update()+1)
          # copy SHAP cols
          if(!is.null(BoostaR_models()[[BoostaR_idx()]]$SHAP_cols)){
            # copy SHAP values to d
            existing_SHAP_cols <- names(d())[grep('_SHAP_', names(d()))] # get rid of any existing SHAP columns
            if(length(existing_SHAP_cols)>0){
              d()[, (existing_SHAP_cols) := NULL]
              dt_update(dt_update()+1)
            }
            new_SHAP_idx <- BoostaR_models()[[BoostaR_idx()]]$SHAP_rows
            new_SHAP_cols <- BoostaR_models()[[BoostaR_idx()]]$SHAP_cols
            if(!is.null(new_SHAP_cols)){
              SHAP_names <- names(new_SHAP_cols[,2:ncol(new_SHAP_cols)])
              d()[new_SHAP_idx, (SHAP_names) := new_SHAP_cols[,2:ncol(new_SHAP_cols)]]
              dt_update(dt_update()+1)
            }
          }
        }
      }
    })
  })
}
    
## To be copied in the UI
# mod_BoostaR_ui("BoostaR_1")
    
## To be copied in the server
# mod_BoostaR_server("BoostaR_1")

add_col <- function(d, dt_update){
  
}