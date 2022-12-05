#' buildBoostaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets dropdownButton
#' @importFrom rhandsontable rHandsontableOutput
mod_BoostaR_build_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        fluidRow(
          column(
            width = 9,
            h3('Feature scenarios & interactions')
          ),
          column(
            width = 3,
            style = 'margin-top:16px; padding-right:16px; padding-bottom:0px',
            align = 'right',
            dropdownButton(
              inputId = ns('BoostaR_fics_dropdown'),
              right = TRUE,
              up = FALSE,
              circle = FALSE,
              label = 'Custom',
              margin = "20px",
              inline = TRUE,
              checkboxInput(inputId = ns('BoostaR_use_custom_interaction_constraints'),label = "Apply custom feature interaction constraints", value = FALSE),
              textAreaInput(
                inputId = ns('BoostaR_custom_interaction_constraints'),
                value =
                  '# separate features with "x"
# any features selected for the model
# not included in an interaction constraint
# will be fitted with no interaction terms',
                label = 'Specify interactions',
                width = '520px',
                height = '600px',
                resize = 'vertical'
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns('BoostaR_feature_specification'),
              label = 'Feature specification',
              size = 10,
              selectize = FALSE,
              choices = c(''),
              width = '100%'
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = ns('BoostaR_interaction_contraints'),
              label = 'Feature interaction constraints',
              size = 10,
              multiple = TRUE,
              selectize = FALSE,
              choices = NULL,
              width = '100%'
            )
          )
        ),
        fluidRow(
          column(
            width = 5,
            htmlOutput(ns('BoostaR_num_features'))
          ),
          column(
            width = 7,
            align = 'right',
            style='margin-top:16px; padding-bottom:0px',
            actionButton(
              inputId = ns("BoostaR_add_features"),
              label = 'all',
              icon = icon("plus-circle")
            ),
            actionButton(
              inputId = ns("BoostaR_clear_features"),
              label = 'all',
              icon = icon("minus-circle")
            ),
            actionButton(
              inputId = ns("BoostaR_clear_interaction_groups"),
              label = 'int groups',
              icon = icon("minus-circle")
            ),
            actionButton(
              inputId = ns("BoostaR_goto_ChartaR"),
              icon = icon('chart-line'),
              label = NULL
            )
          )
        ),
        div(rHandsontableOutput(ns("BoostaR_features")), style = 'font-size: 12px')
      ),
      column(
        width = 6,
        fluidRow(
          column(
            width = 4,
            h3('Parameters')
          ),
          column(
            width = 8,
            style = 'margin-top:16px; padding-right:16px; padding-bottom:0px',
            align = 'right',
            dropdownButton(
              inputId = ns('BoostaR_additional_options'),
              right = TRUE,
              up = FALSE,
              circle = FALSE,
              label = 'Additional parameters',
              margin = "20px",
              inline = TRUE,
              textAreaInput(
                inputId = ns('BoostaR_additional_parameters'),
                value =
                  '#boosting: gbdt
#objective: gamma
#metric: gamma
#tree_learner: serial
#device_type: cpu
#data:
#valid:
#num_iterations: 5000
#learning_rate: 0.5
#num_leaves: 10
#num_threads: 0
#deterministic: FALSE
#force_col_wise: FALSE
#force_row_wise: FALSE
#histogram_pool_size: -1
#max_depth: 4
#min_data_in_leaf: 20
#min_sum_hessian_in_leaf: 0.001
#bagging_fraction: 1
#pos_bagging_fraction: 1
#neg_bagging_fraction: 1
#bagging_freq: 1
#bagging_seed: 3
#feature_fraction: 1
#feature_fraction_bynode: 1
#feature_fraction_seed: 2
#extra_trees: FALSE
#extra_seed: 6
#early_stopping_round: 100
#first_metric_only: FALSE
#max_delta_step: 0
#lambda_l1: 0
#lambda_l2: 0
#linear_lambda: 0
#min_gain_to_split: 0
#drop_rate: 0.1
#max_drop: 50
#skip_drop: 0.5
#xgboost_dart_mode: FALSE
#uniform_drop: FALSE
#drop_seed: 4
#top_rate: 0.2
#other_rate: 0.1
#min_data_per_group: 1
#max_cat_threshold: 32
#cat_l2: 0
#cat_smooth: 10
#max_cat_to_onehot: 4
#top_k: 20
#monotone_constraints:
#monotone_constraints_method: advanced
#monotone_penalty: 0
#feature_contri:
#forcedsplits_filename:
#refit_decay_rate: 0.9
#cegb_tradeoff: 1
#cegb_penalty_split: 0
#cegb_penalty_feature_lazy:
#cegb_penalty_feature_coupled:
#path_smooth: 0
#interaction_constraints:
#verbosity: 1
#saved_feature_importance_type: 0
#linear_tree: FALSE
#max_bin: 255
#max_bin_by_feature:
#min_data_in_bin: 3
#bin_construct_sample_cnt: 200000
#data_random_seed: 1
#is_enable_sparse: TRUE
#enable_bundle: TRUE
#use_missing: TRUE
#zero_as_missing: FALSE
#feature_pre_filter: FALSE
#pre_partition: FALSE
#two_round: FALSE
#header: FALSE
#label_column:
#weight_column:
#group_column:
#ignore_column:
#categorical_feature:
#forcedbins_filename:
#precise_float_parser: FALSE
#objective_seed: 5
#num_class: 1
#is_unbalance: FALSE
#scale_pos_weight: 1
#sigmoid: 1
#boost_from_average: TRUE
#reg_sqrt: FALSE
#alpha: 0.9
#fair_c: 1
#poisson_max_delta_step: 0.7
#tweedie_variance_power: 1.5
#lambdarank_truncation_level: 30
#lambdarank_norm: TRUE
#label_gain:
#eval_at:
#multi_error_top_k: 1
#auc_mu_weights:
#num_machines: 1
#local_listen_port: 12400
#time_out: 120
#machine_list_filename:
#machines:
#gpu_platform_id: -1
#gpu_device_id: -1
#gpu_use_dp: FALSE
#num_gpu: 1',
                label = 'LightGBM all parameters',
                width = '560px',
                height = '600px',
                resize = 'vertical'
              )
            ),
            actionButton(
              inputId = ns("BoostaR_build_model"),
              icon = icon("chevron-right"),
              label = 'Build',
              style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left'
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            fluidRow(
              column(
                width = 7,
                textInput(
                  ns('BoostaR_num_rounds'),
                  'Max rounds',
                  value = 100)
              ),
              column(
                width = 5,
                textInput(
                  ns('BoostaR_early_stopping'),
                  'Stopping',
                  value = 20)
              )
            ),
            fluidRow(
              column(
                width = 7,
                radioGroupButtons(
                  inputId = ns('BoostaR_grid_search'),
                  label = 'Grid search',
                  width = '100%',
                  justified = TRUE,
                  choices = c('Off','On'),
                  selected = 'Off'
                )
              ),
              column(width = 5,
                     textInput(
                       ns('BoostaR_grid_combinations'),
                       'Combinations',
                       value = 10
                     )
              )
            ),
            fluidRow(
              column(
                width = 12,
                selectInput(
                  inputId = ns('BoostaR_objective'),
                  width = '100%',
                  label = 'Objective function',
                  choices = list('identity link' = list('mean_squared_error',
                                                        'mean_absolute_error',
                                                        'mean_absolute_percentage_error',
                                                        'huber',
                                                        'fair'),
                                 'log link' = list('poisson',
                                                   'gamma',
                                                   'tweedie'),
                                 'logit link' = list('binary')
                  )
                ),
                div(style = "margin-top:-6px"),
                selectInput(
                  inputId = ns('BoostaR_initial_score'),
                  width = '100%',
                  label = 'Initial score (response scale offset)',
                  choices = c('no offset')
                ),
                div(style = "margin-top:-6px"),
                radioGroupButtons(
                  inputId = ns('BoostaR_calculate_SHAP_values'),
                  label = 'Calculate SHAP values',
                  width = '100%',
                  justified = TRUE,
                  choices = c('No','10k','All'),
                  selected = 'All',
                )
              ),
            )
          ),
          column(
            width = 6,
            uiOutput(ns('BoostaR_learning_rate_UI')),
            div(style = "margin-top:-10px"),
            uiOutput(ns('BoostaR_num_leaves_UI')),
            div(style = "margin-top:-10px"),
            uiOutput(ns('BoostaR_max_depth_UI')),
            div(style = "margin-top:-10px"),
            uiOutput(ns('BoostaR_column_sample_rate_UI')),
            div(style = "margin-top:-10px"),
            uiOutput(ns('BoostaR_row_sample_rate_UI')),
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(style = "margin-top:-15px; padding-top:0px"),
            h3('Evaluation log'),
            plotlyOutput(ns('BoostaR_evaluation_plot'), height = 'calc(100vh - 600px)'),
          )
        )
      ),
    )
  )
}
    
#' buildBoostaR Server Functions
#'
#' @noRd 
#' 
#' @importFrom rhandsontable renderRHandsontable hot_to_r
#' @importFrom shiny withProgress
#' 
mod_BoostaR_build_model_server <- function(id, d, dt_update, response, weight, feature_spec, BoostaR_models, BoostaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    BoostaR_feature_table <- reactiveVal()
    output$BoostaR_num_features <- renderUI({h3('Features')})
    observeEvent(feature_spec(), {
      feature_specifications <- extract_feature_specifications(feature_spec())
      if(is.null(feature_specifications)){
        updateSelectInput(inputId = 'BoostaR_feature_specification', choices = 'no feature specification')
      } else {
        updateSelectInput(inputId = 'BoostaR_feature_specification', choices = feature_specifications)
      }
    })
    observeEvent(d(), {
      output$BoostaR_features <- renderRHandsontable({
        rhandsontable_formatted(make_BoostaR_feature_grid(d(), feature_spec()), 500)
        })
    })
    observeEvent(BoostaR_idx(), {
      if(!is.null(BoostaR_idx())){
        B <- BoostaR_models()[[BoostaR_idx()]]
        update_GBM_parameters(session, output, B)
        output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(B$feature_table, 500)})
      }

    })
    observeEvent(input$BoostaR_feature_specification, ignoreInit = TRUE, {
      fs <- feature_spec()
      if(!is.null(input$BoostaR_feature_specification) & !is.null(fs)){
        features <- fs[fs[[input$BoostaR_feature_specification]]=='feature', feature]
        features <- remove_lucidum_cols(features)
      } else {
        features <- NULL
      }
      dt <- populate_BoostaR_feature_grid(names(d()), features, fs, BoostaR_feature_table())
      output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, 500)})
    })
    observeEvent(input$BoostaR_features, {
      
      # update BoostaR_feature_table
      BoostaR_feature_table(setDT(hot_to_r(input$BoostaR_features)))
      
      # update number of features ui element
      num_features <- BoostaR_feature_table()[,sum(include)]
      output$BoostaR_num_features <- renderUI({h3(paste(sep = '', 'Features (', num_features, ')'))})
      
      # update feature interaction control
      int_groups <- BoostaR_feature_table()[, sum(include),interaction_grouping][order(interaction_grouping)]
      int_groups <- int_groups[interaction_grouping!='',]
      if(nrow(int_groups)>0){
        choices <- as.list(int_groups[['interaction_grouping']])
        names(choices) <- paste0(choices, ' (', int_groups[['V1']], ')')
      } else {
        choices <- NULL
      }
      updateSelectInput(session, inputId = 'BoostaR_interaction_contraints', choices = choices, selected = input$BoostaR_interaction_contraints)
    })
    observeEvent(input$BoostaR_clear_features, {
      if(!is.null(BoostaR_feature_table())){
        dt <- BoostaR_feature_table()
        dt[, include := FALSE]
        output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, 500)})
        updateSelectInput(session, inputId = 'BoostaR_feature_specification', selected = character(0))
      }
    })
    observeEvent(input$BoostaR_add_features, {
      if(!is.null(BoostaR_feature_table())){
        dt <- BoostaR_feature_table()
        dt[, include := TRUE]
        dt[feature==response(), include := FALSE]
        output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, 500)})
        updateSelectInput(session, inputId = 'BoostaR_feature_specification', selected = character(0))
      }
    })
    observeEvent(input$BoostaR_clear_interaction_groups, {
      if(!is.null(BoostaR_feature_table())){
        dt <- BoostaR_feature_table()
        groups <- input$BoostaR_interaction_contraints
        if(!is.null(groups)){
          interaction_grouping <- NULL
          dt[interaction_grouping %in% groups, include := FALSE]
        }
        output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, 500)})
      }
    })
    observeEvent(input$BoostaR_build_model, {
      # check if the response, weight, features and parameters are compatible
      # and can build a model, e.g. check response is non-negative if objective is gamma
      check <- check_model_features_and_parameters(
        d(),
        response(),
        weight(),
        input$BoostaR_initial_score,
        BoostaR_feature_table(),
        input$BoostaR_objective
       )
      if(check!='ok'){
        confirmSweetAlert(session = session, type = 'error', inputId = ns('BoostaR_error'), title = "Error", text = check,btn_labels = c('OK'))
      } else {
        # no error so good to build model
        # assemble constraints
        monotonicity_constraints <- make_monotonicity_constraints(BoostaR_feature_table(), input$BoostaR_objective)
        feature_interaction_constraints <- make_fics(BoostaR_feature_table(), input$BoostaR_interaction_contraints)
        # assemble model parameters depending on whether it's a grid search or not
        if(input$BoostaR_grid_search=='Off'){
          main_params_combos <- setDT(extract_main_lgbm_parameters(input))
        } else if(input$BoostaR_grid_search=='On') {
          main_params_combos <- setDT(get_main_params_combos(input))
        }
        # get the additional parameters plus monotones and fics if applied
        additional_params <- extract_additional_lgbm_parameters(input$BoostaR_additional_parameters)
        montonicity_possible <- lgbm_objectives[objective==input$BoostaR_objective][['montonicity_possible']]
        if(montonicity_possible){
          additional_params <- c(additional_params, monotone_constraints = list(monotonicity_constraints))
        }
        if(!is.null(feature_interaction_constraints)){
          additional_params <- c(additional_params, interaction_constraints = list(feature_interaction_constraints))
        }
        # prepare the lgb.Dataset and rules (only need to do this just one when running a grid search)
        features <- BoostaR_feature_table()[include==TRUE, feature]
        lgb_dat <- make_lgb_train_test(d(), response(), weight(), input$BoostaR_initial_score, features, input$BoostaR_objective)
        # loop over the combinations of parameters and build models
        for(i in 1:nrow(main_params_combos)){
          withProgress(message = '', detail = 'training', {
            model_name <- make_unique_name(response(), names(BoostaR_models()), 'lgbm')
            if(nrow(main_params_combos)==1){
              message <- 'BoostaR'
            } else {
              message <- paste0('BoostaR (', i,'/',nrow(main_params_combos),')')
            }
            setProgress(value = 0, message = message)
            params <- c(main_params_combos[i], additional_params)
            params$metric <- metric_from_objective(params$objective)
            BoostaR_model <- build_lgbm(lgb_dat, params, lgb_dat$offset, input$BoostaR_calculate_SHAP_values, BoostaR_feature_table())
            BoostaR_model$name <- model_name
            BoostaR_model$additional_params <- additional_params
            if(!is.null(BoostaR_model$lgbm)=='ok'){
              # QUESTION - feels inefficient (copying large object), is there a better way?
              new_list <- BoostaR_models()
              new_list[[model_name]] <- BoostaR_model
              BoostaR_models(new_list)
            } else {
              confirmSweetAlert(session = session, type = 'error', inputId = ns('BoostaR_error'), title = "Error", text = BoostaR_model$message, btn_labels = c('OK'))
            }
          })
        }
        # turn off grid search and select last model
        BoostaR_idx(names(BoostaR_models())[length(names(BoostaR_models()))])
        updateRadioGroupButtons(session, inputId = 'BoostaR_grid_search', selected = 'Off')
      }
    })
    observeEvent(input$BoostaR_grid_search, {
      if(input$BoostaR_grid_search=='Off'){
        learning_rate <- 0.3
        num_leaves <- 5
        max_depth <- 4
        col_sample_rate <- 1
        row_sample_rate <- 1
      } else {
        learning_rate <- c(0.1,0.5)
        num_leaves <- c(2,10)
        max_depth <- c(3,6)
        col_sample_rate <- c(0.5,1.0)
        row_sample_rate <- c(0.5,1.0)
      }
      output$BoostaR_learning_rate_UI <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_learning_rate'),
          label = 'Learning rate',
          min = 0.01,
          max = 1,
          value = learning_rate,
          step = 0.01,
          ticks = FALSE,
          width = '100%'
        )
      })
      output$BoostaR_num_leaves_UI <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_num_leaves'),
          label = 'Number of leaves',
          min = 2,
          max = 32,
          value = num_leaves,
          step = 1,
          ticks = FALSE,
          width = '100%'
        )
      })
      output$BoostaR_max_depth_UI <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_max_depth'),
          label = 'Max depth',
          min = 2,
          max = 10,
          value = max_depth,
          step = 1,
          ticks = FALSE,
          width = '100%'
        )
      })
      output$BoostaR_column_sample_rate_UI <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_column_sample_rate'),
          label = 'Column sample rate',
          min = 0,
          max = 1,
          value = col_sample_rate,
          step = 0.05,
          ticks = FALSE,
          width = '100%'
        )
      })
      output$BoostaR_row_sample_rate_UI <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_row_sample_rate'),
          label = 'Row sample rate',
          min = 0,
          max = 1,
          value = row_sample_rate,
          step = 0.05,
          ticks = FALSE,
          width = '100%'
        )
      })
    })
    output$BoostaR_evaluation_plot <- plotly::renderPlotly({
      # QUESTION - better to use ObserveEvent on BoostaR_models and BoostaR_idx?
      if(!is.null(BoostaR_idx())){
        evaluation_plot(BoostaR_models()[[BoostaR_idx()]]$evaluation_log)
      }
    })
  })
}
    
extract_feature_specifications <- function(d){
  if(is.null(d)){
    f_specs <- c('no feature specification')
  } else {
    if(nrow(d)>0){
      # get column headers, remove special columns used by Toolkit
      f_specs <- names(d)[2:ncol(d)]
      f_specs <- f_specs[!(f_specs %in% c('base_level','min','max','banding','use_mid_point','monotonicity','interaction_grouping'))]
    } else {
      f_specs <- c('no feature specification')
    }
  }
  f_specs
}
make_BoostaR_feature_grid <- function(d, feature_spec){
  features <- remove_lucidum_cols(names(d))
  # make grid
  dt <- data.table(feature=features,
                   gain=0L,
                   include=FALSE)
  # merge on interaction_grouping and monotonicity
  setkey(dt, feature)
  setkey(feature_spec, feature)
  dt <- feature_spec[,c('feature','interaction_grouping','monotonicity')][dt]
  setorder(dt, feature)
  setcolorder(dt, c('feature','gain','include','interaction_grouping','monotonicity'))
  dt
}
remove_lucidum_cols <- function(x){
  l_cols <- c('lgbm_prediction','glm_prediction','lgbm_residual','glm_residual',
              'train_test','user_filter','total_filter')
  l_contains_cols <- c('lgbm_SHAP_','glm_LP_')
  return_cols <- NULL
  if(!is.null(x)){
    if(length(x)>0){
      cols_to_remove <- x[x %like% paste(l_contains_cols, collapse="|")]
      cols_to_remove <- c(cols_to_remove, l_cols)
      return_cols <- setdiff(x, cols_to_remove)
    }
  }
  return_cols
}
#' @importFrom rhandsontable rhandsontable hot_table hot_col hot_cols
rhandsontable_formatted <- function(dt, height){
  rhandsontable(
    dt,
    #outsideClickDeselects = FALSE,
    selectCallback = TRUE,
    readOnly = FALSE,
    rowHeaders = FALSE,
    columnSorting = TRUE,
    colWidths = c(40,8,8,20,15),
    height = height) %>%
    hot_table(stretchH = 'all', highlightRow = TRUE) %>%
    hot_col(c('gain','include'), valign='htCenter')  %>%
    hot_col('gain', format = "0.0000") %>%
    hot_col(c('feature','gain'), readOnly = TRUE) %>%
    hot_cols(manualColumnResize = TRUE)
}
check_model_features_and_parameters <- function(d, response, weight, init_score, feature_table, objective){
  # check that the objective is consistent with the response and weight
  # check monotonicity constraints have only been applied to numerical features
  cols <- names(d)
  features <- feature_table[include==TRUE,feature]
  check <- 'ok'
  # quick checks
  if(response %not_in% names(d)){
    check <- 'Response not found in dataset'
  } else if (weight %not_in% c('N',cols)){
    check <- 'Weight not found in dataset'
  } else if (init_score %not_in% c('no offset', cols)){
    check <- 'Initial score not found in dataset'
  } else if ('train_test' %not_in% cols){
    check <- 'train_test column not found in dataset'
  } else if(length(features)==0){
    check <- 'No features selected'
  } else if (response %in% features){
    check <- 'Response included in features'
  }
  # check that none of the feature columns are dates (LightGBM will exclude)
  if(any(sapply(d[,..features], inherits, 'Date'))){
    check <- 'Date columns in features'
  }
  # check weight if not N
  if(weight!='N'){
    if (anyNA(d[[weight]])){
      check <- 'Weight contains NAs'
    } else if (min(d[[weight]], na.rm = TRUE)<0){
      check <- 'Weight contains negative values'
    }
  }
  # more detailed checks
  if(check=='ok'){
    if(weight=='N'){
      rows_idx <- 1:nrow(d)
    } else {
      rows_idx <- which(d[[weight]]>0)
    }
    if(anyNA(d[rows_idx, ..response])){
      check <- 'Response contains NAs'
    }
  }
  # check response vs objective
  if(check=='ok' & objective %in% c('Poisson','Gamma','Tweedie','Binomial')){
    if(min(d[rows_idx, ..response], na.rm = TRUE)<=0){
      check <- paste0('Non-negative response not allowed for', objective)
    }
  }
  # check monotonicity only specified for numerical features
  if(check=='ok'){
    mono_features <- feature_table[monotonicity!='', feature]
    nums <- numerical_cols(d)
    non_numeric_cols <- setdiff(features, nums)
    if(any(mono_features %in% non_numeric_cols)){
      check <- 'monotonicity applied to non-numeric feature'
    }
  }
  check
}
make_monotonicity_constraints <- function(feature_table, obj){
  montonicity_possible <- lgbm_objectives[objective==obj][['montonicity_possible']]
  if(montonicity_possible){
    monotonicity <- NULL
    monotonicity_constraints <- convert_monotonicity_column(feature_table[include==TRUE, monotonicity])
  }
}
convert_monotonicity_column <- function(x){
  m <- rep(0, length(x))
  m[x %in% c('Increasing','increasing','1')] <- 1
  m[x %in% c('Decreasing','decreasing','-1')] <- -1
  return(m)
}
make_fics <- function(feature_table, groups){
  include <- NULL
  interaction_grouping <- NULL
  if(is.null(groups)){
    fics <- NULL
  } else {
    features <- feature_table[include==TRUE, c('feature','interaction_grouping')]
    features[!(interaction_grouping %in% groups), 'interaction_grouping' := 'non_grouped']
    feature_groups <- split(features, by = 'interaction_grouping')
    c <- function(d){d[[1]]}
    fics <- lapply(feature_groups, c)
  }
  return(fics)
}
extract_main_lgbm_parameters <- function(input){
  list(
    objective = input$BoostaR_objective,
    num_iterations = as.numeric(input$BoostaR_num_rounds),
    early_stopping_round = as.numeric(input$BoostaR_early_stopping),
    learning_rate = input$BoostaR_learning_rate,
    num_leaves = input$BoostaR_num_leaves,
    max_depth = input$BoostaR_max_depth,
    feature_fraction = input$BoostaR_column_sample_rate,
    bagging_fraction = input$BoostaR_row_sample_rate,
    bagging_freq = 1
  )
}
extract_additional_lgbm_parameters <- function(x){
  # lgbm parameters
  x <- unlist(strsplit(x, '\n'))
  x <- gsub(' ','', x)
  if(length(grep('#', x))>0){
    x <- x[-grep('#', x)]
  }
  x <- strsplit(x, ':')
  result <- lapply(x, utils::tail, n = -1)
  names(result) <- lapply(x, utils::head, n = 1)
  result <- lapply(result, convert_numerics)
}
convert_numerics <- function(x){
  converted <- as.numeric(x)
  ifelse(is.na(converted),x,converted)
}

#' @importFrom lightgbm lgb.convert_with_rules lgb.Dataset.create.valid lgb.Dataset set_field
make_lgb_train_test <- function(d, response, weight, init_score, features, obj){
  # only include rows with non-zero weight
  if(weight=='N'){
    rows_idx <- 1:nrow(d)
    rows_include <- rep(TRUE, nrow(d))
  } else {
    rows_idx <- which(d[[weight]]>0)
    rows_include <- d[[weight]]>0
  }
  # split into train and test
  train_test_non_zero_rows <- d[['train_test']][rows_include]
  train_ids <- which(d[['train_test']]==0 & rows_include)
  test_ids <- which(d[['train_test']]==1 & rows_include)
  all_response <- d[[response]][rows_include]
  train_response <- d[[response]][train_ids]
  test_response <- d[[response]][test_ids]
  # make lgbm datasets
  d_convert <- lgb.convert_with_rules(data = d[rows_include, ..features], rules = NULL)
  d_train <- d_convert$data[train_test_non_zero_rows==0]
  d_test <- d_convert$data[train_test_non_zero_rows==1]
  cat_features <- setdiff(features, numerical_cols(d))
  l_train <- lgb.Dataset(as.matrix(d_train), label=train_response, free_raw_data = FALSE, categorical_feature = cat_features)
  l_test <- lgb.Dataset.create.valid(l_train, as.matrix(d_test), label = test_response)
  # set the initial score if there is one
  link <- lgbm_objectives[objective==obj][['link']]
  if(init_score!='none'){
    offset <- d[[init_score]]
  } else if (weight!='N'){
    offset <- d[[weight]]
  }
  if(!is.null(offset)){
    if(link=='log'){
      offset <- log(offset)
    } else if (link=='logit'){
      offset <- log(offset/(1-offset))
    }
    offset[is.infinite(offset)] <- 0
    set_field(l_train, 'init_score', offset[rows_include][train_test_non_zero_rows==0])
    set_field(l_test, 'init_score', offset[rows_include][train_test_non_zero_rows==1])
  }
  return(
    list(
      response = response,
      weight = weight,
      init_score = init_score,
      l_train = l_train,
      l_test = l_test,
      rows_idx = rows_idx,
      rows_include = rows_include,
      data = d_convert$data,
      rules = d_convert$rules,
      offset = offset,
      link = link,
      features = features
    )
  )
}

#' Build a BoostaR model 
#'
#' @description A fct function
#'
#' @param lgb_dat list
#' @param params list
#'
#' @return list
#'
#' @noRd
#' 
#' @importFrom lightgbm lgb.train lgb.importance lgb.model.dt.tree lgb.get.eval.result
#' @importFrom stats predict
build_lgbm <- function(lgb_dat, params, offset, SHAP_sample, feature_table){
  # build the model
  start_time <- Sys.time()
  lgbm <- tryCatch({
    lgb.train(
      params = params,
      data = lgb_dat$l_train,
      verbose = -1,
      valids = list('train'=lgb_dat$l_train,'test'=lgb_dat$l_test), # so we score both train and test data
      callbacks = list(cb.print.period(params$num_iterations)) # callback to enable progressbar to update
    )},
    error = function(e){e}
  )
  run_time <- Sys.time() - start_time
  if(inherits(lgbm,'simpleError')){
    message <- lgbm$error
    lgbm <- NULL
    new_feature_table <- NULL
    importances <- NULL
    evaluation_log <- NULL
    tree_table <- NULL
    gain_summary <- NULL
    SHAP_cols <- NULL
    SHAP_run_time <- NULL
    SHAP_rows <- NULL
    predictions <- NULL
  } else {
    # get predictions (these are pre-offset)
    predictions <- predict(lgbm, as.matrix(lgb_dat$data), rawscore = TRUE)
    # add offset to predictions
    if(!is.null(offset)){
      predictions <- predictions + offset[lgb_dat$rows_include]
    }
    if(lgb_dat$link=='log'){
      predictions <- exp(predictions)
    } else if (lgb_dat$link=='logit'){
      predictions <- 1/(1+exp(-predictions))
    }
    # get SHAP values and append to d
    SHAP_cols <- BoostaR_extract_SHAP_values(lgb_dat$data, lgbm, lgb_dat$features, SHAP_sample, lgb_dat$rows_idx)
    SHAP_run_time <- Sys.time() - start_time
    SHAP_rows <- SHAP_cols[['idx']]
    # extract feature importances and make predictions
    importances <- lgb.importance(lgbm, percentage = TRUE)
    importances[, 2:4] <- 100 * importances[, 2:4]
    # merge the importances onto the feature table
    new_feature_table <- post_model_update_BoostaR_feature_grid(feature_table, importances)
    # extract the evaluation log
    evaluation_log <- make_evaluation_log(lgbm, params)
    # extract the tree table
    tree_table <- lgb.model.dt.tree(lgbm)
    # extract the gain summarised by tree's feature combinations
    gain_summary <- create_gain_summary_from_tree_summary(tree_table)
    gain_summary <- gain_summary[order(-gain_summary$gain),]
  }
  # return list
  return(
    list(
      message = message,
      lgbm = lgbm,
      rules = lgb_dat$rules,
      response = lgb_dat$response,
      weight = lgb_dat$weight,
      init_score = lgb_dat$init_score,
      params = params,
      features = lgb_dat$features,
      offset = lgb_dat$offset,
      link = lgb_dat$link,
      feature_table = new_feature_table,
      run_time = run_time,
      SHAP_run_time = SHAP_run_time,
      predictions = predictions,
      pred_rows = lgb_dat$rows_include,
      SHAP_cols = SHAP_cols,
      SHAP_rows = SHAP_rows,
      importances = importances,
      evaluation_log = evaluation_log,
      tree_table = tree_table,
      gain_summary = gain_summary
    )
  )
}
cb.print.period <- function(n) {
  # callback function to output iteration
  callback <- function(env = parent.frame()) {
    incProgress(0.9/n) # leave a bit for incProgresses below
  }
  attr(callback, 'call') <- match.call()
  attr(callback, 'name') <- 'cb.print.period'
  callback
}
BoostaR_extract_SHAP_values <- function(d, lgbm, features, sample, rows_idx){
  if(sample=='No'){
    SHAP_cols <- NULL
  } else if (sample=='10k'){
    n_sample <- min(10000,nrow(d))
    idx <- sample(rows_idx, n_sample, replace = FALSE)
    SHAP_cols <- stats::predict(lgbm, as.matrix(d[idx,]), predcontrib = TRUE, num_iteration = lgbm$best_iter)
    SHAP_cols <- as.data.table(SHAP_cols)
    names(SHAP_cols) <- paste(sep = '_', 'lgbm_SHAP', c(features, 'base_score'))
    SHAP_cols <- cbind(idx = idx, SHAP_cols)
  } else if (sample=='All') {
    idx <- rows_idx
    SHAP_cols <- stats::predict(lgbm, as.matrix(d), predcontrib = TRUE, num_iteration = lgbm$best_iter)
    SHAP_cols <- as.data.table(SHAP_cols)
    names(SHAP_cols) <- paste(sep = '_', 'lgbm_SHAP', c(features, 'base_score'))
    SHAP_cols <- cbind(idx = idx, SHAP_cols)
  }
  return(SHAP_cols)
}
make_evaluation_log <- function(lgbm, params){
  train_log <- lgb.get.eval.result(lgbm, "train", params$metric)
  test_log <- lgb.get.eval.result(lgbm, "test", params$metric)
  train_err <- train_log[lgbm$best_iter]
  test_err <- test_log[lgbm$best_iter]
  evaluation_log <- list(train_log = train_log,
                         test_log = test_log,
                         train_err = train_err,
                         test_err = test_err,
                         best_iteration = lgbm$best_iter,
                         metric = params$metric)
}
#' @importFrom stringr str_count
create_gain_summary_from_tree_summary <- function(trees){
  split_feature <- NULL
  tree_index <- NULL
  . <- NULL
  split_gain <- NULL
  split_features <- NULL
  gain_proportion <- NULL
  # get number of features in tree - i.e. interaction order
  int_order <- trees[, sum(!is.na(split_feature)), by = tree_index]
  max_int_depth <- max(int_order$V1)
  # split out features
  # sort trees by alphabetical feature
  setorder(trees, tree_index, split_feature)
  features <- trees[, .(split_features = toString(stats::na.omit(unique(split_feature)))), by = list(tree_index)]
  # gain
  gain <- trees[, list(gain = sum(split_gain, na.rm = TRUE)), by = tree_index]
  total_gain <- sum(gain$gain)
  # bind columns together
  summary <- cbind(features, gain = gain[[2]])
  # summarise by feature combinations, sorted by decreasing gain
  summary <- summary[, list(gain = sum(gain)), by = split_features]
  summary[, int_order := 1 + str_count(split_features, ',')]
  summary[, gain_proportion := gain/total_gain]
  summary[, split_features := gsub(', ',' x ', split_features)]
  setorder(summary, -gain)
  setcolorder(summary, c(1,3,2,4))
  names(summary) <- c('tree_features','dim','gain','%')
  return(summary)
}
metric_from_objective <- function(x){
  # define the objective, metric and initial score
  if(x=='mean_squared_error'){
    metric <- 'rmse'
  } else if(x=='binary'){
    metric <- 'binary_logloss'
  } else if(x %in% c('poisson','quasipoisson')){
    metric <- 'poisson'
  } else if(x=='gamma'){
    metric <- 'gamma'
  } else if(x=='tweedie'){
    metric <- 'tweedie'
  } else if(x=='mean_absolute_error'){
    metric <- 'l1'
  } else if(x=='mean_absolute_percentage_error'){
    metric <- 'mape'
  } else if(x=='huber'){
    metric <- 'huber'
  } else if(x=='fair'){
    metric <- 'fair'
  }
}
make_unique_name <- function(response, current_names, suffix){
  # models are called by the response column name and _lgbm_1, _lgbm_2 etc
  suffix1 <- paste0('_',suffix,'_')
  if(is.null(current_names)){
    max_index <- 0
  } else {
    matches <- current_names[grep(paste0(response, suffix1), current_names)]
    end_pattern <- unlist(lapply(gregexpr(suffix1, matches),'[',1))
    if(!is.null(end_pattern)){
      if(suffix=='lgbm'){
        current_indices <- as.numeric(substr(matches,end_pattern+6,nchar(matches)))
      } else if (suffix=='glm'){
        current_indices <- as.numeric(substr(matches,end_pattern+5,nchar(matches)))
      }
      max_index <- max(current_indices)
    } else {
      max_index <- 0
    }
  }
  paste0(response, suffix1, max_index+1)
}
get_main_params_combos <- function(input){
  num_combos <- as.numeric(input$BoostaR_grid_combinations)
  learning_rate <- input$BoostaR_learning_rate
  num_leaves <- input$BoostaR_num_leaves
  max_depth <- input$BoostaR_max_depth
  feature_fraction <- input$BoostaR_column_sample_rate
  bagging_fraction <- input$BoostaR_row_sample_rate
  if((learning_rate[1]*20-floor(learning_rate[1]*20)==0)&learning_rate[2]*20-floor(learning_rate[2]*20)==0){
    lr_inc <- 0.05
  } else {
    lr_inc <- 0.01
  }
  learning_rate <- seq(learning_rate[1], learning_rate[2], lr_inc)
  num_leaves <- seq(num_leaves[1], num_leaves[2], 1)
  max_depth <- seq(max_depth[1], max_depth[2], 1)
  feature_fraction <- seq(feature_fraction[1], feature_fraction[2], 0.05)
  bagging_fraction <- seq(bagging_fraction[1], bagging_fraction[2], 0.05)
  params_grid <- expand.grid(objective = input$BoostaR_objective,
                             num_iterations = as.numeric(input$BoostaR_num_rounds),
                             early_stopping_round = as.numeric(input$BoostaR_early_stopping),
                             learning_rate = learning_rate,
                             num_leaves = num_leaves,
                             max_depth = max_depth,
                             feature_fraction = feature_fraction,
                             bagging_fraction = bagging_fraction,
                             bagging_freq = 1,
                             stringsAsFactors = FALSE
  )
  params_grid <- unique(params_grid)
  if(nrow(params_grid)>num_combos){
    rows_idx <- sample(1:nrow(params_grid), num_combos, replace = FALSE)
    params_grid <- params_grid[rows_idx,]
  }
  setorderv(params_grid, cols = names(params_grid)[4:8])
  return(params_grid)
}

#' @importFrom plotly config add_trace layout
evaluation_plot <- function(evaluation_log){
  if(!is.null(evaluation_log)){
    train <- evaluation_log$train_log
    test <- evaluation_log$test_log
    # get into single table
    eval_results <- data.frame(iter = 1:length(train), model_train_error = train, model_test_error = test)
    # if we plot too many points it can slow down the browser - limit to 100 rows
    # always keep the first and last row
    ex_rows <- 1:2
    if(nrow(eval_results)>1000){
      # make sure first and last row are kept
      rows_to_keep <- c(1, floor(1:1000 * nrow(eval_results)/1000), evaluation_log$best_iteration, nrow(eval_results))
      rows_to_keep <- unique(rows_to_keep)
      eval_results <- eval_results[rows_to_keep,]
      ex_rows <- 1:5
    }
    y_min <- min(eval_results$model_train_error[-ex_rows], eval_results$model_test_error[-ex_rows])
    y_max <- max(eval_results$model_train_error[-ex_rows], eval_results$model_test_error[-ex_rows])
    y_range <- y_max - y_min
    plot_ly(eval_results, hovertemplate = paste('(%{x}, %{y})')) %>%
      add_trace(x = ~iter, y = ~model_train_error, type = 'scatter', mode = 'markers', name = 'train', marker = list(color =  grDevices::rgb(255/255,0/255,0/255))) %>%
      add_trace(x = ~iter, y = ~model_test_error, type = 'scatter', mode = 'markers', name = 'test', marker = list(color =  grDevices::rgb(0/255,0/255,0/255))) %>%
      config(displayModeBar = FALSE) %>%
      layout(legend = list(orientation = 'v', x = 1.05, y = 0.6)) %>%
      layout(hovermode = 'x') %>%
      layout(margin = list(r = 25, l = 10, t = 50),
             title = list(text = paste0('<b>',
                                        'evaluation metric: ',
                                        evaluation_log$metric,
                                        '<br>',
                                        'test metric: ',
                                        signif(evaluation_log$test_err, 6),
                                        ', best iteration: ',
                                        evaluation_log$best_iteration,
                                        '</b>'),
                          y = 0.95,
                          xref = "plot",
                          font = list(size = 12, face='bold')
             ),
             xaxis = list(titlefont = list(size=12)),
             yaxis = list(title = '', range = c(y_min - 0.05*y_range,y_max + 0.05*y_range)))
  }
}

update_GBM_parameters <- function(session, output, BoostaR_model){
  ns <- session$ns
  updateTextInput(session, inputId = ns('BoostaR_num_rounds'), value = BoostaR_model$params$num_iterations)
  updateTextInput(session, inputId = ns('BoostaR_early_stopping'), value = BoostaR_model$params$early_stopping_round)
  updateSelectInput(session, inputId = ns('BoostaR_objective'), selected = BoostaR_model$params$objective)
  updateSelectInput(session, inputId = ns('BoostaR_initial_score'), selected = BoostaR_model$init_score)
  updateRadioGroupButtons(session, inputId = ns('BoostaR_grid_search'), selected = 'Off')
  output$BoostaR_learning_rate_UI <- renderUI({
    sliderInput(
      inputId = ns('BoostaR_learning_rate'),
      label = 'Learning rate',
      min = 0.01,
      max = 1,
      value = BoostaR_model$params$learning_rate,
      step = 0.01,
      ticks = FALSE,
      width = '100%'
    )
  })
  output$BoostaR_num_leaves_UI <- renderUI({
    sliderInput(
      inputId = ns('BoostaR_num_leaves'),
      label = 'Number of leaves',
      min = 2,
      max = 30,
      value = BoostaR_model$params$num_leaves,
      step = 1,
      ticks = FALSE,
      width = '100%'
    )
  })
  output$BoostaR_max_depth_UI <- renderUI({
    sliderInput(
      inputId = ns('BoostaR_max_depth'),
      label = 'Max depth',
      min = 2,
      max = 10,
      value = BoostaR_model$params$max_depth,
      step = 1,
      ticks = FALSE,
      width = '100%'
    )
  })
  output$BoostaR_column_sample_rate_UI <- renderUI({
    sliderInput(
      inputId = ns('BoostaR_column_sample_rate'),
      label = 'Column sample rate',
      min = 0,
      max = 1,
      value = BoostaR_model$params$feature_fraction,
      step = 0.05,
      ticks = FALSE,
      width = '100%'
    )
  })
  output$BoostaR_row_sample_rate_UI <- renderUI({
    sliderInput(
      inputId = ns('BoostaR_row_sample_rate'),
      label = 'Row sample rate',
      min = 0,
      max = 1,
      value = BoostaR_model$params$bagging_fraction,
      step = 0.05,
      ticks = FALSE,
      width = '100%'
    )
  })
}
populate_BoostaR_feature_grid <- function(all_features, selected_features, feature_spec, current_grid){
  feature <- NULL
  include <- NULL
  interaction_grouping <- NULL
  monotonicity <- NULL
  gain <- NULL
  if(!is.null(feature_spec) & !is.null(all_features)){
    all_features <- remove_lucidum_cols(all_features)
    dt <- data.table(feature = all_features, include = FALSE)
    dt[feature %in% selected_features, include := TRUE]
    setkey(dt, feature)
    setkey(feature_spec, feature)
    dt <- feature_spec[, c('feature','monotonicity','interaction_grouping')][dt]
    dt[is.na(dt)] <- ''
    if(!is.null(current_grid)){
      # merge on gains
      setkey(current_grid, feature)
      dt <- current_grid[, c('feature','gain')][dt]
      dt[is.na(gain), gain := 0]
      setorder(dt, -gain, -include, feature)
    } else {
      dt[, gain := 0]
      setorder(dt, -include, feature)
    }
    setcolorder(dt, c('feature','gain','include','interaction_grouping','monotonicity'))
  } else {
    dt <- data.table(feature = all_features, gain = 0, include = TRUE, interaction_grouping = '', monotonicity = '')
  }
  dt
}
post_model_update_BoostaR_feature_grid <- function(original_feature_grid, feature_importances){
  feature_importances <- feature_importances[, c('Feature','Gain')]
  names(feature_importances) <- c('feature','gain')
  setkey(original_feature_grid, feature)
  setkey(feature_importances, feature)
  if('gain' %in% names(original_feature_grid)){
    original_feature_grid[, gain := NULL]
  }
  dt <- feature_importances[original_feature_grid]
  dt[is.na(gain), gain := 0]
  setorder(dt, -include, -gain, feature)
  setcolorder(dt, c('feature','gain','include','interaction_grouping','monotonicity'))
  return(dt)
}