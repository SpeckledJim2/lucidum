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
                tabPanel(value = 'Features and parameters', span(tagList(tags$img(src='www/BoostaR_features.png', height="30px", width="30px"), 'Features and parameters')),
                         mod_BoostaR_build_model_ui(ns('buildBoostaR'))
                         ),
                tabPanel(value = 'Model navigator', span(tagList(tags$img(src='www/BoostaR_navigate.png', height="30px", width="30px"), 'Model navigator')),
                         mod_BoostaR_navigate_ui(ns('navigateBoostaR'))
                         ),
                tabPanel(value = 'Tree viewer', title = span(tagList(tags$img(src='www/tree.png', height="30px", width="30px"), 'Tree viewer')),
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
    tabulated_models <- reactiveVal(list())
    mod_BoostaR_build_model_server('buildBoostaR', d, dt_update, response, weight, feature_spec, BoostaR_models, BoostaR_idx, dimensions, crosstab_selector)
    mod_BoostaR_navigate_server('navigateBoostaR', d, BoostaR_models, BoostaR_idx, feature_spec, crosstab_selector, tabulated_models)
    mod_BoostaR_tree_viewer_server('treeViewer', BoostaR_models, BoostaR_idx)
    observeEvent(c(BoostaR_models(), BoostaR_idx()), {
      if(!is.null(BoostaR_idx())){
        # delete existing cols
        if('lgbm_prediction' %in% names(d())){d()[, lgbm_prediction:= NULL]}
        if('lgbm_prediction_rate' %in% names(d())){d()[, lgbm_prediction_rate:= NULL]}
        if('lgbm_tabulated_prediction' %in% names(d())){d()[, lgbm_tabulated_prediction:= NULL]}
        existing_SHAP_cols <- names(d())[grep('_SHAP_', names(d()))] # get rid of any existing SHAP columns
        if(length(existing_SHAP_cols)>0){d()[, (existing_SHAP_cols) := NULL]}
        dt_update(dt_update()+1)
        # update d() for the new GBM
        if(BoostaR_idx()!='No GBMs'){
          b <- BoostaR_models()[[BoostaR_idx()]]
          # copy over the new predictions
          pred_idx <- b$pred_rows
          predictions <- b$predictions
          d()[pred_idx, lgbm_prediction := predictions]
          # if there is a rate prediction, copy those
          if(!is.null(b$predictions_rate)){
            predictions_rate <- b$predictions_rate
            d()[pred_idx, lgbm_prediction_rate := predictions_rate]
          }
          # if there are tabulated predictions copy those
          if(!is.null(b$tabulated_predictions)){
            x <- b$tabulated_predictions$tabulated_lgbm
            x <- link_function(x, b$link)
            d()[pred_idx, lgbm_tabulated_prediction := x]
          }
          # if there are SHAP cols copy those
          if(!is.null(b$SHAP_cols)){
            # copy SHAP values to d
            existing_SHAP_cols <- names(d())[grep('_SHAP_', names(d()))] # get rid of any existing SHAP columns
            if(length(existing_SHAP_cols)>0){
              d()[, (existing_SHAP_cols) := NULL]
            }
            new_SHAP_idx <- b$SHAP_rows
            new_SHAP_cols <- b$SHAP_cols
            if(!is.null(new_SHAP_cols)){
              SHAP_names <- names(new_SHAP_cols[,2:ncol(new_SHAP_cols)])
              d()[new_SHAP_idx, (SHAP_names) := new_SHAP_cols[,2:ncol(new_SHAP_cols)]]
            }
          }
        }
      }
    })
  })
}
