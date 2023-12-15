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
            width = 4,
            htmlOutput(ns('BoostaR_num_features'))
            ),
          column(width = 2,
                 style='margin-top:16px; padding-bottom:0px',
                             actionButton(
              inputId = ns("BoostaR_goto_ChartaR"),
              label = tagList(tags$img(src='www/one_way_line_bar.png', height="26px", width="26px")),
              style = 'padding:3px 5px 3px 5px'
            ),
            tippy_this(
              ns('BoostaR_goto_ChartaR'),
              delay = 2000,
              placement = 'bottom',
              tooltip = tippy_text(
                '<b>Go to ChartaR one-way line and bar </b><br/>
                with the highlighted feature as the x-axis feature',
                12
                )
              )
            ),
          column(
            width = 6,
            align = 'right',
            style='margin-top:16px; padding-bottom:0px',
            actionButton(
              inputId = ns("BoostaR_add_features"),
              label = 'all',
              icon = icon("plus-circle")
            ),
            tippy_this(
              ns('BoostaR_add_features'),
              delay = 2000,
              placement = 'bottom',
              tooltip = tippy_text(
                '<b>Include all features in GBM</b><br/>
                    Includes all dataset columns in the feature table<br/>
                    except for the response, weight and train_test columns',
                12
              )
            ),
            actionButton(
              inputId = ns("BoostaR_clear_features"),
              label = 'all',
              icon = icon("minus-circle")
            ),
            tippy_this(
              ns('BoostaR_clear_features'),
              delay = 2000,
              placement = 'bottom',
              tooltip = tippy_text(
                '<b>Deselect all features</b><br/>',
                12
              )
            ),
            tags$head(tags$style(HTML("#BoostaR-buildBoostaR-feature_scenarios {padding: 3px 5px 3px 5px;}"))),
            dropdownButton(inputId = ns('feature_scenarios'),
                           width = 300,
                           up = FALSE,
                           circle = FALSE,
                           inline = TRUE,
                           size = 'default',
                           label = tags$img(src='www/features.png', height="26px", width="26px"),
                           right = FALSE,
                           margin = '10px',
                           selectInput(
                             ns('BoostaR_feature_specification'),
                             label = 'Choose a feature specification',
                             size = 10,
                             selectize = FALSE,
                             choices = c(''),
                             width = '100%'
                           ),
                           tippy_this(
                             ns('BoostaR_feature_specification'),
                             delay = 2000,
                             placement = 'bottom',
                             tooltip = tippy_text(
                               '<b>Feature specification</b><br/>
                    Select a feature specification<br/>
                    to update the feature table below',
                               12
                             )
                           )
                           
            ),
            # QUESTION - how should I be doing this with a ns?
            tags$head(tags$style(HTML("#BoostaR-buildBoostaR-BoostaR_fics_dropdown {padding: 3px 3px 3px 3px;}"))),
            tippy_this(
              ns('BoostaR_fics_dropdown'),
              delay = 2000,
              placement = 'bottom',
              tooltip = tippy_text(
                '<b>Define custom feature interaction constraints</b><br/>
                    Specify which interactions can be included in the GBM',
                12
              )
            ),
            dropdownButton(
              inputId = ns('BoostaR_fics_dropdown'),
              right = FALSE,
              up = FALSE,
              circle = FALSE,
              label = tags$img(src='www/fics.png', height="26px", width="26px"),
              margin = "20px",
              inline = TRUE,
              selectInput(
                inputId = ns('BoostaR_interaction_contraints'),
                label = 'Apply feature interaction constraints',
                size = 10,
                multiple = TRUE,
                selectize = FALSE,
                choices = NULL,
                width = '100%'
              ),
              tippy_this(
                ns('BoostaR_interaction_contraints'),
                delay = 2000,
                placement = 'bottom',
                tooltip = tippy_text(
                  '<b>Feature interaction constraints (FICS)</b><br/>
                    Applies FICS to the selected interaction grouping<br/>
                    Hold down Ctrl or Command to select multiple rows',
                  12
                )
              ),
              checkboxInput(inputId = ns('BoostaR_use_custom_interaction_constraints'),label = "Apply custom feature interaction constraints", value = FALSE),
              fluidRow(
                column(
                  width = 8,
                  sliderInput(
                    inputId = ns('top_n_interactions'),
                    label = 'Select top n terms by gain from current GBM',
                    min = 1,
                    max = 100,
                    value = 10,
                    step = 1,
                    ticks = FALSE,
                    width = '100%'
                  )
                ),
                column(
                  width = 4,
                  actionButton(
                    inputId = ns('copy_top_n_interactions'),
                    label = 'Copy'
                  )
                )
              ),
              textAreaInput(
                inputId = ns('BoostaR_custom_interaction_constraints'),
                value =
                  '# separate features with "x"
# any features selected for the model
# not included in an interaction constraint
# will be fitted with no interaction terms',
                label = 'Specify interactions',
                width = '520px',
                height = '300px',
                resize = 'vertical'
              )
            )
            # actionButton(
            #   inputId = ns("BoostaR_clear_interaction_groups"),
            #   label = 'int groups',
            #   icon = icon("minus-circle")
            # ),
            # tippy_this(
            #   ns('BoostaR_clear_interaction_groups'),
            #   delay = 2000,
            #   placement = 'bottom',
            #   tooltip = tippy_text(
            #     '<b>Deselect interaction group features</b><br/>
            #     Only features from highlighted interaction groups are removed',
            #     12
            #   )
            # ),
            )
          ),
        div(rHandsontableOutput(ns("BoostaR_features")), style = 'font-size: 12px')
      ),
      column(
        width = 6,
        fluidRow(
          column(
            width = 3,
            h3('Parameters')
          ),
          column(
            width = 5,
            align = 'right',
            div(
              style = 'margin-top:16px',
              radioGroupButtons(
                inputId = ns('ebm_mode'),
                label = NULL,
                choiceValues = c('Normal','EBM mode'),
                choiceNames = c(
                  tagList(tags$img(src='www/normal.png', height="20px", width="26px",'Normal')),
                  tagList(tags$img(src='www/ebm_mode.png', height="20px", width="26px",'EBM'))
                  ),
                selected = 'Normal',
              )
            ),
            tippy_this(
              ns('ebm_mode'),
              delay = 2000,
              placement = 'bottom',
              tooltip = tippy_text(
                '<b>Explainable Boosting Machine</b><br/>
                    Trains 2-leaf trees (1D) terms first,<br/>
                    then 3-leaf trees (2D) terms etc.<br/>
                    up to the selected number of leaves',
                12
              )
            )
          ),
          column(
            width = 4,
            style = 'margin-top:16px; padding-right:16px; padding-bottom:0px',
            align = 'right',

            tippy_this(
              ns('BoostaR_additional_options'),
              delay = 2000,
              placement = 'bottom',
              tooltip = tippy_text(
                '<b>Additional LightGBM parameters</b><br/>
                    Access the full list of LightGBM training parameters',
                12
              )
            ),
            tags$head(tags$style(HTML("#BoostaR-buildBoostaR-BoostaR_additional_options {padding: 6px 3px 5px 3px;}"))),
            dropdownButton(
              inputId = ns('BoostaR_additional_options'),
              right = TRUE,
              up = FALSE,
              circle = FALSE,
              label = tags$img(src='www/parameters.png', height="20px", width="26px"),
              margin = "20px",
              inline = TRUE,
              br(),
              textAreaInput(
                inputId = ns('BoostaR_additional_parameters'),
                value =
'#poisson_max_delta_step: 0.7
#boost_from_average: TRUE
#min_sum_hessian_in_leaf: 0.001
#max_cat_threshold: 32
#cat_l2: 0
#cat_smooth: 10
#max_cat_to_onehot: 4
#objective: gamma
#metric: gamma
#deterministic: FALSE
#force_col_wise: FALSE
#force_row_wise: FALSE
#histogram_pool_size: -1
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
#top_k: 20
#monotone_penalty: 0
#refit_decay_rate: 0.9
#cegb_tradeoff: 1
#cegb_penalty_split: 0
#cegb_penalty_feature_lazy:
#cegb_penalty_feature_coupled:
#path_smooth: 0
#saved_feature_importance_type: 0
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
#objective_seed: 5
#num_class: 1
#is_unbalance: FALSE
#scale_pos_weight: 1
#sigmoid: 1
#reg_sqrt: FALSE
#alpha: 0.9
#fair_c: 1'
                ,
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
            width = 4,
            fluidRow(
              column(
                width = 7,
                style = 'padding-right: 4px',
                textInput(
                  ns('BoostaR_num_rounds'),
                  'Max trees',
                  value = 1000),
                tippy_this(ns('BoostaR_num_rounds'),
                           delay = 2000,
                           placement = 'bottom',
                           tooltip = tippy_text('<b>Max trees</b><br />
                                                Number of boosting iterations'
                                                ,12
                           )
                )
              ),
              column(
                width = 5,
                style = 'padding-left: 4px',
                textInput(
                  ns('BoostaR_early_stopping'),
                  'Stopping',
                  value = 50),
                tippy_this(ns('BoostaR_early_stopping'),
                           delay = 2000,
                           placement = 'bottom',
                           tooltip = tippy_text("<b>Stopping</b><br />
                                                Training stops when test data metric<br />
                                                doesn't improve in this many rounds<br />
                                                0 disables early stopping",
                                                12
                           )
                )
              )
            ),
            fluidRow(
              column(
                width = 7,
                style = 'padding-right: 4px',
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
                     style = 'padding-left: 4px',
                     textInput(
                       ns('BoostaR_grid_combinations'),
                       'Combos',
                       value = 10
                     ),
                     tippy_this(ns('BoostaR_grid_combinations'),
                                delay = 2000,
                                placement = 'bottom',
                                tooltip = tippy_text("<b>Combos</b><br />
                                                Maximum number of<br />
                                                combinations to use for grid search"
                                                     ,12
                                )
                     )
              )
            ),
            fluidRow(
              column(
                width = 12,
                div(style = "margin-top:-4px"),
                div(
                  id = ns('objective_wrapper'),
                  selectInput(
                    inputId = ns('BoostaR_objective'),
                    width = '100%',
                    label = 'Objective',
                    selected = 'gamma',
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
                  )
                ),
                tippy_this(
                  ns('objective_wrapper'),
                  placement = 'bottom',
                  delay = 2000,
                  tooltip = tippy_text(
                    "<b>Objective</b><br/>
                    Objective function minimised during training<br/>
                    Some objective functions use a log link<br/>
                    The binary objective uses a logit link",
                    12
                  )
                ),
                div(style = "margin-top:-6px"),
                div(
                  id = ns('offset_wrapper'),
                  selectInput(
                    inputId = ns('BoostaR_initial_score'),
                    width = '100%',
                    label = 'Offset (initial score)',
                    choices = c('no offset')
                  )
                ),
                tippy_this(
                  ns('offset_wrapper'),
                  placement = 'bottom',
                  delay = 2000,
                  tooltip = tippy_text(
                    "<b>Offset</b><br/>
                    Also called the initial or base score<br/>
                    Training begins from this value<br/>
                    The initial score should be in the space of transformed values",
                    12
                    )
                  ),
                div(style = "margin-top:-6px"),
                radioGroupButtons(
                  inputId = ns('BoostaR_calculate_SHAP_values'),
                  label = 'Calculate SHAP values',
                  width = '100%',
                  justified = TRUE,
                  choices = c('No','10k','All'),
                  selected = 'All',
                ),
                tippy_this(
                  ns('BoostaR_calculate_SHAP_values'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Calculate SHAP values</b><br/>
                    SHAP values can take a long time to calculate depending on model complexity<br/>
                    Choose 10k to calculate SHAP values on a random sample<br/>
                    Choose No to suppress SHAP value calculation',
                    12
                  )
                ),
              ),
            )
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 6,
                radioGroupButtons(
                  inputId = ns('BoostaR_boosting'),
                  label = 'Boosting method',
                  width = '100%',
                  justified = TRUE,
                  choices = c('gbdt','goss'),
                  selected = 'gbdt'
                ),
                tippy_this(ns('BoostaR_boosting'),
                           delay = 2000,
                           placement = 'bottom',
                           tooltip = tippy_text('<b>Boosting method</b><br />
                                                gbdt = traditional gradient boosted decision trees<br />
                                                goss = gradient-based one-side sampling<br />
                                                goss requires row sample rate = 1.0 (no bagging)'
                                                ,12
                                                )
                           ),
                uiOutput(ns('BoostaR_learning_rate_UI')),
                tippy_this(
                  ns('BoostaR_learning_rate_UI'),
                  delay = 2000, placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Learning rate</b><br/>
                    Lower learning rates are usually more accurate<br/>
                    but take longer to train',
                    12
                    )
                  ),
                div(style = "margin-top:-10px"),
                uiOutput(ns('BoostaR_num_leaves_UI')),
                tippy_this(
                  ns('BoostaR_num_leaves_UI'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Number of leaves</b><br/>
                    The maximum number of leaves in a single tree<br/>
                    The maximum model interaction order equals<br/>
                    the number of leaves minus 1',
                    12
                    )
                  ),
                div(style = "margin-top:-10px"),
                uiOutput(ns('BoostaR_max_depth_UI')),
                tippy_this(
                  ns('BoostaR_max_depth_UI'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Max depth</b><br/>
                    The maximum number of edges traversed from<br/>
                    the root to a terminal leaf in a single tree',
                    12
                    )
                ),
                div(style = "margin-top:-10px"),
                uiOutput(ns('BoostaR_row_sample_rate_UI')),
                tippy_this(
                  ns('BoostaR_row_sample_rate_UI'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Row sample rate</b><br/>
                    Also called the bagging fraction<br/>
                    Randomly select part of data without resampling<br/>
                    Can reduce over-fitting and speed up training',
                    12
                  )
                ),
              ),
              column(
                width = 6,
                textInput(
                  ns('BoostaR_tweedie_variance_power'),
                  'Tweedie var power',
                  value = 1.5),
                tippy_this(
                  ns('BoostaR_tweedie_variance_power'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Tweedie variance power</b><br/>
                    Only used with the Tweedie objective<br/>
                    Set this closer to 2 to shift towards a Gamma distribution<br/>
                    Set this closer to 1 to shift towards a Poisson distribution<br/>
                    Constraints: 1.0 <= tweedie_variance_power < 2.0',
                    12
                  )
                ),
                uiOutput(ns('BoostaR_min_data_in_leaf')),
                tippy_this(
                  ns('BoostaR_min_data_in_leaf'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Minimum data in leaf</b><br/>
                    The minimum number of rows in any leaf of any tree.<br/>
                    Try increasing this value to reduce overfitting',
                    12
                  )
                ),
                div(style = "margin-top:-10px"),
                uiOutput(ns('BoostaR_lambda_l1')),
                tippy_this(
                  ns('BoostaR_lambda_l1'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>L1 normalisation</b><br/>
                    L1 normalisation tends to reduce the number of leaf splits.',
                    12
                  )
                ),
                div(style = "margin-top:-10px"),
                uiOutput(ns('BoostaR_lambda_l2')),
                tippy_this(
                  ns('BoostaR_lambda_l2'),
                  delay = 2000,
                  placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>L2 normalisation</b><br/>
                    L2 normalisation tends to reduce the<br/>
                    number of leaves with large predictions',
                    12
                  )
                ),
                div(style = "margin-top:-10px"),
                uiOutput(ns('BoostaR_column_sample_rate_UI')),
                tippy_this(
                  ns('BoostaR_column_sample_rate_UI'),
                  delay = 2000, placement = 'bottom',
                  tooltip = tippy_text(
                    '<b>Column sample rate</b><br/>
                    Randomly selects a subset of features on each tree<br/>
                    if feature_fraction is smaller than 1.0',
                    12
                  )
                ),
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(style = "margin-top:-15px; padding-top:0px"),
            fluidRow(
              column(
                width = 6,
                h3('Evaluation log')
              ),
              column(
                width = 6,
                align = 'right',
                div(
                  style = 'margin-top:20px; margin-bottom:-10px',
                  radioGroupButtons(
                    inputId = ns('eval_log_view'),
                    label = NULL,
                    choices = c('All','Tail'),
                    selected = 'All'
                  )
                )
              )
            ),
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
mod_BoostaR_build_model_server <- function(id, d, dt_update, response, weight, feature_spec, BoostaR_models, BoostaR_idx, dimensions, crosstab_selector){
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
        rhandsontable_formatted(make_BoostaR_feature_grid(d(), feature_spec()), dimensions()[2] - 200)
        })
    })
    observeEvent(c(d(), dt_update()), {
      # get the numerical columns in d
      if(!is.null(d())){
        num_cols <- numerical_cols(d())
        choices <- remove_lucidum_cols(num_cols)
        # put just lgbm_prediction and glm_prediction back in if present and at the top
        fav <- 'glm_prediction'
        if(fav %in% num_cols){
          choices <- c(fav, choices)
        }
        fav <- 'lgbm_prediction'
        if(fav %in% num_cols){
          choices <- c(fav, choices)
        }
        if(is.null(choices)){
          choices <- 'no offset'
        } else {
          choices <- c('no offset', choices)
        }
        updateSelectInput(session, inputId = 'BoostaR_initial_score', choices = choices)
      }
    })
    observeEvent(BoostaR_idx(), {
      if(!is.null(BoostaR_idx())){
        B <- BoostaR_models()[[BoostaR_idx()]]
        if(!is.null(B)){
          update_GBM_parameters(session, output, B)
          output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(B$feature_table, dimensions()[2] - 200)})
        }
      }
    })
    observeEvent(input$copy_top_n_interactions, {
      if(!is.null(BoostaR_models()) & !is.null(BoostaR_idx())){
        n <- input$top_n_interactions
        gain_summary <- BoostaR_models()[[BoostaR_idx()]]$gain_summary
        n <- min(n, nrow(gain_summary))
        top_n <- gain_summary[1:n, tree_features]
        updateTextAreaInput(inputId = 'BoostaR_custom_interaction_constraints', value = paste(top_n, collapse = '\n'))
      } else {
        updateTextAreaInput(inputId = 'BoostaR_custom_interaction_constraints', value = 'No active GBM')
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
      output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, dimensions()[2] - 200)})
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
        output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, dimensions()[2] - 200)})
        updateSelectInput(session, inputId = 'BoostaR_feature_specification', selected = character(0))
      }
    })
    observeEvent(input$BoostaR_add_features, {
      if(!is.null(BoostaR_feature_table())){
        dt <- BoostaR_feature_table()
        dt[, include := TRUE]
        dt[feature==response(), include := FALSE]
        output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, dimensions()[2] - 200)})
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
        output$BoostaR_features <- renderRHandsontable({rhandsontable_formatted(dt, dimensions()[2] - 200)})
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
        features <- BoostaR_feature_table()[include==TRUE, feature]
        monotonicity_constraints <- make_monotonicity_constraints(BoostaR_feature_table(), input$BoostaR_objective)
        #feature_interaction_constraints <- make_fics(BoostaR_feature_table(), input$BoostaR_interaction_contraints)
        
        # NEW BIT
        if(input$BoostaR_use_custom_interaction_constraints==TRUE){
          # extract the fics from the textAreaInput BoostaR_custom_interaction_constraints
          feature_interaction_constraints <- make_custom_fics(input$BoostaR_custom_interaction_constraints, features)
        } else {
          groups_to_constrain <- input$BoostaR_interaction_contraints
          if(!is.null(groups_to_constrain)){
            feature_interaction_constraints <- make_fics(BoostaR_feature_table(), input$BoostaR_interaction_contraints)
          } else {
            feature_interaction_constraints <- NULL
          }
        }
        
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
        lgb_dat <- make_lgb_train_test(d(), response(), weight(), input$BoostaR_initial_score, features, input$BoostaR_objective)
        # loop over the combinations of parameters and build models
        for(i in 1:nrow(main_params_combos)){
          threads <- getDTthreads()
          if(threads==1){
            detail_message <- paste0('training (', threads, ' thread)')
          } else {
            detail_message <- paste0('training (', threads, ' threads)')
          }
          withProgress(message = '', detail = detail_message, {
            model_name <- make_unique_name(response(), names(BoostaR_models()), 'lgbm')
            if(nrow(main_params_combos)==1){
              message <- 'BoostaR'
            } else {
              message <- paste0('BoostaR (', i,'/',nrow(main_params_combos),')')
            }
            setProgress(value = 0, message = message)
            params <- c(main_params_combos[i], additional_params)
            params$metric <- metric_from_objective(params$objective)
            BoostaR_model <- build_lgbm(lgb_dat, params, lgb_dat$offset, input$BoostaR_calculate_SHAP_values, input$ebm_mode, BoostaR_feature_table())
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
        updateRadioGroupButtons(session, inputId = 'BoostaR_grid_search', selected = 'Off')
        BoostaR_idx(names(BoostaR_models())[length(names(BoostaR_models()))])
      }
    })
    observeEvent(input$BoostaR_grid_search, {
      if(is.null(BoostaR_models()) | is.null(BoostaR_idx())){
        B <- NULL
      } else {
        B <- BoostaR_models()[[BoostaR_idx()]]
      }
      if(input$BoostaR_grid_search=='Off'){
        if(is.null(B)){
          learning_rate <- 0.3
          num_leaves <- 5
          max_depth <- 4
          col_sample_rate <- 1
          row_sample_rate <- 1
          min_data_in_leaf <- 20
          lambda_l1 <- 0
          lambda_l2 <- 0
        } else {
          learning_rate <- B$params$learning_rate
          num_leaves <- B$params$num_leaves
          max_depth <- B$params$max_depth
          col_sample_rate <- B$params$feature_fraction
          row_sample_rate <- B$params$bagging_fraction
          min_data_in_leaf <- B$params$min_data_in_leaf
          lambda_l1 <- B$params$lambda_l1
          lambda_l2 <- B$params$lambda_l2
        }
      } else {
        learning_rate <- c(0.1,0.3)
        num_leaves <- c(2,10)
        max_depth <- c(32,32)
        col_sample_rate <- c(0.5,1.0)
        row_sample_rate <- c(0.5,1.0)
        min_data_in_leaf <- c(0,200)
        lambda_l1 <- c(0,200)
        lambda_l2 <- c(0,200)
      }
      output$BoostaR_learning_rate_UI <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_learning_rate'),
          label = 'Learning rate',
          min = 0.05,
          max = 1,
          value = learning_rate,
          step = 0.05,
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
          max = 32,
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
      output$BoostaR_min_data_in_leaf <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_min_data_in_leaf'),
          label = 'Min data in leaf',
          min = 0,
          max = 200,
          value = min_data_in_leaf,
          step = 10,
          ticks = FALSE,
          width = '100%'
        )
      })
      output$BoostaR_lambda_l1 <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_lambda_l1'),
          label = 'L1 normalisation',
          min = 0,
          max = 200,
          value = lambda_l1,
          step = 10,
          ticks = FALSE,
          width = '100%'
        )
      })
      output$BoostaR_lambda_l2 <- renderUI({
        sliderInput(
          inputId = ns('BoostaR_lambda_l2'),
          label = 'L2 normalisation',
          min = 0,
          max = 200,
          value = lambda_l2,
          step = 10,
          ticks = FALSE,
          width = '100%'
        )
      })
    })
    output$BoostaR_min_data_in_leaf <- renderUI({
      sliderInput(
        inputId = ns('BoostaR_min_data_in_leaf'),
        label = 'Min data in leaf',
        min = 0,
        max = 200,
        value = 0,
        step = 10,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_lambda_l1 <- renderUI({
      sliderInput(
        inputId = ns('BoostaR_lambda_l1'),
        label = 'L1 normalisation',
        min = 0,
        max = 200,
        value = 0,
        step = 10,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_lambda_l2 <- renderUI({
      sliderInput(
        inputId = ns('BoostaR_lambda_l2'),
        label = 'L2 normalisation',
        min = 0,
        max = 200,
        value = 0,
        step = 10,
        ticks = FALSE,
        width = '100%'
      )
    })
    observeEvent(input$BoostaR_goto_ChartaR, {
      # get the selected row in the table
      r <- input$BoostaR_features_select$select$rAll
      last_clicked <- ''
      if(!is.null(r)){
        if(length(r)==1){
          # only proceed if one row is selected
          last_clicked <- input$BoostaR_features$data[[r]][[1]]
        }
      }
      # forces update when last_clicked hasn't changed
      val <- crosstab_selector()$val
      if(is.null(val)) val <- 1 else val <- val + 1
      info_list <- list(
        originator = 'BoostaR feature table',
        last_clicked = last_clicked,
        val = val
      )
      crosstab_selector(info_list)
    })
    output$BoostaR_evaluation_plot <- plotly::renderPlotly({
      # QUESTION - better to use ObserveEvent on BoostaR_models and BoostaR_idx?
      if(!is.null(BoostaR_idx()) & !is.null(BoostaR_models())){
        evaluation_plot(BoostaR_models()[[BoostaR_idx()]], input$eval_log_view)
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
  l_cols <- c('lgbm_prediction','lgbm_tabulated_prediction','glm_prediction','glm_tabulated_prediction','lgbm_residual','glm_residual',
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
  if(check=='ok' & objective %in% c('poisson','tweedie','binary')){
    if(min(d[rows_idx, ..response], na.rm = TRUE)<0){
      check <- paste0('Negative response not allowed for ', objective)
    }
  }
  if(check=='ok' & objective %in% c('gamma')){
    if(min(d[rows_idx, ..response], na.rm = TRUE)<=0){
      check <- paste0('Non-negative response not allowed for ', objective)
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
make_custom_fics <- function(x, features){
  x <- unlist(strsplit(x, '\n'))
  x <- gsub(' ','', x)
  if(length(grep('#', x))>0){
    x <- x[-grep('#', x)]
  }
  fics <- strsplit(x, 'x')
  # include all the features as one-way terms
  # so these will still be included in the model
  fics <- c(fics, features)
  # only keep fics that involve features
  keep <- sapply(fics, function(x){all(x %in% features)})
  fics <- fics[keep]
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
    bagging_freq = 1,
    boosting = input$BoostaR_boosting,
    tweedie_variance_power = as.numeric(input$BoostaR_tweedie_variance_power),
    lambda_l1 = input$BoostaR_lambda_l1,
    lambda_l2 = input$BoostaR_lambda_l2,
    min_data_in_leaf = input$BoostaR_min_data_in_leaf
  )
}
extract_additional_lgbm_parameters <- function(x){
  # lgbm parameters
  x <- unlist(strsplit(x, '\n'))
  x <- gsub(' ','', x)
  if(length(grep('#', x))>0){
    x <- x[-grep('#', x)]
    x <- x[x!='']
  }
  if(length(x)==0){
    # no additional parameters
    result <- NULL
  } else {
    x <- strsplit(x, ':')
    result <- lapply(x, utils::tail, n = -1)
    names(result) <- lapply(x, utils::head, n = 1)
    result <- lapply(result, convert_numerics)
  }
  return(result)
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
  l_train <- lgb.Dataset(as.matrix(d_train), label=train_response, free_raw_data = FALSE, categorical_feature = cat_features, params = list(feature_pre_filter = FALSE))
  l_test <- lgb.Dataset.create.valid(l_train, as.matrix(d_test), label = test_response)
  # set the initial score if there is one
  link <- lgbm_objectives[objective==obj][['link']]
  if(init_score!='no offset'){
    offset <- d[[init_score]]
  } else if (weight!='N'){
    offset <- d[[weight]]
  } else {
    offset <- NULL
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
build_lgbm <- function(lgb_dat, params, offset, SHAP_sample, ebm_mode, feature_table){
  # build the model
  start_time <- Sys.time()
  params$num_threads <- getDTthreads()
  
  # callbacks
  callbacks <- list(cb.print.period(params$num_threads, params$num_iterations))
  if(ebm_mode=='EBM mode'){
    es <- params$early_stopping_round
    max_leaves <- params$num_leaves
    lr <- params$learning_rate
    params$early_stopping_round <- 0 # override the "normal" early stopping with ebm stopping
    params$num_leaves <- 2 # start build with 1D terms, then 2D etc
    #params$learning_rate <- 0.3 # for 1D terms use this learning rate, then revert to what user specified
    callbacks <- add.cb(
      cb_list = callbacks,
      cb = cb_early_stop_ebm(
        max_leaves = max_leaves,
        learning_rate = lr,
        stopping_rounds = es,
        first_metric_only = isTRUE(params[["first_metric_only"]]),
        verbose = FALSE
      )
    )
  }
  
  lgbm <- tryCatch({
    lgb.train(
      params = params,
      data = lgb_dat$l_train,
      verbose = -1,
      valids = list('train'=lgb_dat$l_train,'test'=lgb_dat$l_test), # so we score both train and test data
      callbacks = callbacks # callback to enable progressbar to update
    )},
    error = function(e){e}
  )
  run_time <- Sys.time() - start_time
  if(ebm_mode=='EBM mode'){
    # reinstate the original values
    params$early_stopping_round <- es
    params$num_leaves <- max_leaves
    #params$learning_rate <- lr
  }
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
    setProgress(value = 0.9, detail = paste0('best iteration: ', lgbm$best_iter, ', predict...'))
    predictions <- predict(lgbm, as.matrix(lgb_dat$data), type = 'raw')
    # add offset to predictions
    if(!is.null(offset)){
      predictions <- predictions + offset[lgb_dat$rows_include]
    }
    if(lgb_dat$link=='log'){
      predictions <- exp(predictions)
    } else if (lgb_dat$link=='logit'){
      predictions <- 1/(1+exp(-predictions))
    }
    setProgress(value = 0.92, detail = paste0('best iteration: ', lgbm$best_iter, ', SHAP values...'))
    SHAP_cols <- BoostaR_extract_SHAP_values(lgb_dat$data, lgbm, lgb_dat$features, SHAP_sample, lgb_dat$rows_idx)
    SHAP_run_time <- Sys.time() - start_time
    SHAP_rows <- SHAP_cols[['idx']]
    # extract feature importances and make predictions
    setProgress(value = 0.94, detail = paste0('best iteration: ', lgbm$best_iter, ', feature importances...'))
    importances <- lgb.importance(lgbm, percentage = TRUE)
    importances[, 2:4] <- 100 * importances[, 2:4]
    # merge the importances onto the feature table
    new_feature_table <- post_model_update_BoostaR_feature_grid(feature_table, importances)
    # extract the evaluation log
    evaluation_log <- make_evaluation_log(lgbm, params)
    # extract the tree table
    setProgress(value = 0.96, detail = paste0('best iteration: ', lgbm$best_iter, ', tree table...'))
    tree_table <- lgb.model.dt.tree(lgbm, num_iteration = length(evaluation_log$train_log))
    # extract the gain summarised by tree's feature combinations
    setProgress(value = 0.98, detail = paste0('best iteration: ', lgbm$best_iter, ', gain summary...'))
    gain_summary <- create_gain_summary_from_tree_summary(tree_table, lgbm$best_iter)
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
      ebm_mode = ebm_mode,
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
cb.print.period <- function(num_threads, n) {
  # callback function to output iteration
  if(num_threads==1){
    detail_message <- paste0('training (', num_threads, ' thread)')
  } else {
    detail_message <- paste0('training (', num_threads, ' threads)')
  }
  callback <- function(env = parent.frame()) {
    incProgress(0.9/n) # leave a bit for incProgresses below
    i <- env$iteration
    setProgress(detail = paste0(detail_message, ', tree ', env$iteration))
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
    SHAP_cols <- stats::predict(lgbm, as.matrix(d[idx,]), type='contrib', num_iteration = lgbm$best_iter)
    SHAP_cols <- as.data.table(SHAP_cols)
    names(SHAP_cols) <- paste(sep = '_', 'lgbm_SHAP', c(features, 'base_score'))
    SHAP_cols <- cbind(idx = idx, SHAP_cols)
  } else if (sample=='All') {
    idx <- rows_idx
    SHAP_cols <- stats::predict(lgbm, as.matrix(d), type='contrib', num_iteration = lgbm$best_iter)
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
create_gain_summary_from_tree_summary <- function(trees, best_iter){
  trees <- trees[tree_index<best_iter] # less than as trees start at zero
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
  summary <- summary[, list(trees = .N, gain = sum(gain)), by = split_features]
  summary[, int_order := 1 + str_count(split_features, ',')]
  summary[, gain_proportion := gain/total_gain]
  summary[, split_features := gsub(', ',' x ', split_features)]
  setorder(summary, -gain)
  setcolorder(summary, c(1,4,2,3,5))
  names(summary) <- c('tree_features','dim','trees','gain','%')
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
  min_data_in_leaf <- input$BoostaR_min_data_in_leaf
  lambda_l1 <- input$BoostaR_lambda_l1
  lambda_l2 <- input$BoostaR_lambda_l2
  tweedie_variance_power <- input$BoostaR_tweedie_variance_power
  boosting <- input$BoostaR_boosting
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
  min_data_in_leaf <- seq(min_data_in_leaf[1], min_data_in_leaf[2], 10)
  lambda_l1 <- seq(lambda_l1[1], lambda_l1[2], 10)
  lambda_l2 <- seq(lambda_l2[1], lambda_l2[2], 10)
  combos <-
    length(learning_rate) *
    length(max_depth) *
    length(feature_fraction) *
    length(bagging_fraction) *
    length(min_data_in_leaf) *
    length(lambda_l1) *
    length(lambda_l2)
  if(combos > 1000){
    # sample down to avoid params_grid being huge
    learning_rate <- sample(learning_rate, size = num_combos, replace = TRUE)
    #num_leaves <- sample(num_leaves, size = num_combos, replace = TRUE)
    num_leaves <- sample_down(num_leaves, num_combos)
    max_depth <- sample(max_depth, size = num_combos, replace = TRUE)
    feature_fraction <- sample(feature_fraction, size = num_combos, replace = TRUE)
    bagging_fraction <- sample(bagging_fraction, size = num_combos, replace = TRUE)
    min_data_in_leaf <- sample_down(min_data_in_leaf, num_combos)
    lambda_l1 <- sample_down(lambda_l1, num_combos)
    lambda_l2 <- sample_down(lambda_l2, num_combos)
    params_grid <- data.table(
      objective = input$BoostaR_objective,
      boosting = boosting,
      num_iterations = as.numeric(input$BoostaR_num_rounds),
      early_stopping_round = as.numeric(input$BoostaR_early_stopping),
      learning_rate = learning_rate,
      num_leaves = num_leaves,
      max_depth = max_depth,
      feature_fraction = feature_fraction,
      bagging_fraction = bagging_fraction,
      min_data_in_leaf = min_data_in_leaf,
      bagging_freq = 1,
      lambda_l1 = lambda_l1,
      lambda_l2 = lambda_l2,
      tweedie_variance_power = tweedie_variance_power
    )
  } else {
    params_grid <- expand.grid(objective = input$BoostaR_objective,
                               boosting = boosting,
                               num_iterations = as.numeric(input$BoostaR_num_rounds),
                               early_stopping_round = as.numeric(input$BoostaR_early_stopping),
                               learning_rate = learning_rate,
                               num_leaves = num_leaves,
                               max_depth = max_depth,
                               feature_fraction = feature_fraction,
                               bagging_fraction = bagging_fraction,
                               min_data_in_leaf = min_data_in_leaf,
                               bagging_freq = 1,
                               lambda_l1 = lambda_l1,
                               lambda_l2 = lambda_l2,
                               tweedie_variance_power = tweedie_variance_power,
                               stringsAsFactors = FALSE
    )
  }
  # make unique in case of duplicates
  params_grid <- unique(params_grid)
  if(nrow(params_grid)>num_combos){
    rows_idx <- sample(1:nrow(params_grid), num_combos, replace = FALSE)
    params_grid <- params_grid[rows_idx,]
  }
  setorderv(params_grid, cols = names(params_grid)[4:8])
  return(params_grid)
}

#' @importFrom plotly config add_trace layout
#' @importFrom utils tail
evaluation_plot <- function(BoostaR_model, view){
  evaluation_log <- BoostaR_model$evaluation_log
  if(!is.null(evaluation_log)){
    train <- evaluation_log$train_log
    test <- evaluation_log$test_log
    # get into single table depending on view
    eval_results <- data.frame(iter = 1:length(train), model_train_error = train, model_test_error = test)
    if(view=='All'){
      # if we plot too many points it can slow down the browser
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
    } else if (view=='Tail'){
      # just show the last 3 x early stopping rounds of test metric
      es <- BoostaR_model$params$early_stopping_round
      if(es<0) es <- 50
      eval_results <- tail(eval_results, 3*es)
      y_min <- min(eval_results$model_test_error)
      y_max <- max(eval_results$model_test_error)
      y_range <- y_max - y_min
    }
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
  if(!is.null(BoostaR_model)){
    ns <- session$ns
    updateRadioGroupButtons(session, inputId = 'ebm_mode', selected = BoostaR_model$ebm_mode)
    updateTextInput(session, inputId = 'BoostaR_num_rounds', value = BoostaR_model$params$num_iterations)
    updateTextInput(session, inputId = 'BoostaR_early_stopping', value = BoostaR_model$params$early_stopping_round)
    updateTextInput(session, inputId = 'BoostaR_tweedie_variance_power', value = BoostaR_model$params$tweedie_variance_power)
    updateRadioGroupButtons(session, inputId = 'BoostaR_boosting', selected = BoostaR_model$params$boosting)
    updateSelectInput(session, inputId = 'BoostaR_objective', selected = BoostaR_model$params$objective)
    updateSelectInput(session, inputId = 'BoostaR_initial_score', selected = BoostaR_model$init_score)
    output$BoostaR_learning_rate_UI <- renderUI({
      sliderInput(
        inputId = ns('BoostaR_learning_rate'),
        label = 'Learning rate',
        min = 0.05,
        max = 1,
        value = BoostaR_model$params$learning_rate,
        step = 0.05,
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
        max = 32,
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
    output$BoostaR_min_data_in_leaf <- renderUI({
      sliderInput(
        inputId = ns('BoostaR_min_data_in_leaf'),
        label = 'Min data in leaf',
        min = 0,
        max = 200,
        value = BoostaR_model$params$min_data_in_leaf,
        step = 10,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_lambda_l1 <- renderUI({
      sliderInput(
        inputId = ns('BoostaR_lambda_l1'),
        label = 'L1 normalisation',
        min = 0,
        max = 200,
        value = BoostaR_model$params$lambda_l1,
        step = 10,
        ticks = FALSE,
        width = '100%'
      )
    })
    output$BoostaR_lambda_l2 <- renderUI({
      sliderInput(
        inputId = ns('BoostaR_lambda_l2'),
        label = 'L2 normalisation',
        min = 0,
        max = 200,
        value = BoostaR_model$params$lambda_l2,
        step = 10,
        ticks = FALSE,
        width = '100%'
      )
    })
  }
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
      if('gain' %not_in% names(current_grid)){
        current_grid[, gain := 0]
      }
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
cb_early_stop_ebm <- function(max_leaves, learning_rate, stopping_rounds, first_metric_only, verbose) {
  
  factor_to_bigger_better <- NULL
  best_iter <- NULL
  best_score <- NULL
  best_msg <- NULL
  eval_len <- NULL
  restart_stopping <- NULL
  
  # Initialization function
  init <- function(env) {
    # Early stopping cannot work without metrics
    if (length(env$eval_list) == 0L) {
      stop("For early stopping, valids must have at least one element")
    }
    
    # Store evaluation length
    eval_len <<- length(env$eval_list)
    
    # Check if verbose or not
    if (isTRUE(verbose)) {
      msg <- paste0(
        "Will train until there is no improvement in "
        , stopping_rounds
        , " rounds."
      )
      print(msg)
    }
    
    # Internally treat everything as a maximization task
    factor_to_bigger_better <<- rep.int(1.0, eval_len)
    best_iter <<- rep.int(-1L, eval_len)
    best_score <<- rep.int(-Inf, eval_len)
    best_msg <<- list()
    
    # Loop through evaluation elements
    for (i in seq_len(eval_len)) {
      
      # Prepend message
      best_msg <<- c(best_msg, "")
      
      # Internally treat everything as a maximization task
      if (!isTRUE(env$eval_list[[i]]$higher_better)) {
        factor_to_bigger_better[i] <<- -1.0
      }
      
    }
    
    return(invisible(NULL))
    
  }
  
  # Create callback
  callback <- function(env) {
    # Check for empty evaluation
    if (is.null(eval_len)) {
      init(env = env)
    }
    
    # Store iteration
    cur_iter <- env$iteration
    
    # By default, any metric can trigger early stopping. This can be disabled
    # with 'first_metric_only = TRUE'
    if (isTRUE(first_metric_only)) {
      evals_to_check <- 1L
    } else {
      evals_to_check <- seq_len(eval_len)
    }
    
    # Loop through evaluation
    for (i in evals_to_check) {
      
      # Store score
      score <- env$eval_list[[i]]$value * factor_to_bigger_better[i]
      
      # Check if score is better
      if (score > best_score[i]) {

        # let stopping continue as normal
        if(env$eval_list[[i]]$data_name=='test'){
          restart_stopping <<- FALSE
        }

        # Store new scores
        best_score[i] <<- score
        best_iter[i] <<- cur_iter
        
      } else {
        
        # Check if early stopping is required
        extra_rounds <- 0
        if(isTRUE(restart_stopping)){
          extra_rounds <- stopping_rounds
        }
        if (cur_iter - best_iter[i] >= stopping_rounds+extra_rounds) {
          
          # restart stopping
          restart_stopping <<- TRUE
          
          # increase the number of leaves by one
          new_params <- copy(env$model$params)
          new_params$num_leaves <- new_params$num_leaves + 1
          #new_params$learning_rate <- learning_rate
          env$model$reset_parameter(params = new_params)
          
          if(env$model$params$num_leaves==max_leaves+1){
            
            if (!is.null(env$model)) {
              env$model$best_score <- best_score[i]
              env$model$best_iter <- best_iter[i]
            }
            
            if (isTRUE(verbose)) {
              print(paste0("Early stopping, best iteration is: ", best_msg[[i]]))
            }
            
            # Store best iteration and stop
            env$best_iter <- best_iter[i]
            env$met_early_stop <- TRUE
            
          }
          
        }
        
      }
      
      if (!isTRUE(env$met_early_stop) && cur_iter == env$end_iteration) {
        
        if (!is.null(env$model)) {
          env$model$best_score <- best_score[i]
          env$model$best_iter <- best_iter[i]
        }
        
        if (isTRUE(verbose)) {
          print(paste0("Did not meet early stopping, best iteration is: ", best_msg[[i]]))
        }
        
        # Store best iteration and stop
        env$best_iter <- best_iter[i]
        env$met_early_stop <- TRUE
      }
    }
    
    return(invisible(NULL))
    
  }
  
  attr(callback, "call") <- match.call()
  attr(callback, "name") <- "cb_early_stop_ebm"
  
  return(callback)
  
}

add.cb <- function(cb_list, cb) {
  
  # Combine two elements
  cb_list <- c(cb_list, cb)
  
  # Set names of elements
  names(cb_list) <- callback.names(cb_list = cb_list)
  
  if ("cb.early.stop" %in% names(cb_list)) {
    
    # Concatenate existing elements
    cb_list <- c(cb_list, cb_list["cb.early.stop"])
    
    # Remove only the first one
    cb_list["cb.early.stop"] <- NULL
    
  }
  
  return(cb_list)
  
}

# Extract callback names from the list of callbacks
callback.names <- function(cb_list) {
  return(unlist(lapply(cb_list, attr, "name")))
}

sample_down <- function(x,n){
  if(length(x)==1){
    x
  } else {
    sample(x, size = n, replace = TRUE)
  }
}