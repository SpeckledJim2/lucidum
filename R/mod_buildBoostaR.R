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
mod_buildBoostaR_ui <- function(id){
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
                       value = 100
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
            plotlyOutput(ns('BoostaR_evaluation_plot')),
            tags$head(tags$script('
                                            // Define function to set height of "BoostaR_evaluation_plot"
                                            setHeight_BoostaR_evaluation_plot = function() {
                                              var window_height = $(window).height();
                                              var header_height = $(".main-header").height();
                                              var boxHeight = (window_height - header_height) - 580;
                                              $("#BoostaR_evaluation_plot").height(boxHeight);
                                            };
                                            // Set input$box_height when the connection is established
                                            $(document).on("shiny:connected", function(event) {
                                              setHeight_BoostaR_evaluation_plot();
                                            });
                                            // Refresh the box height on every window resize event
                                            $(window).on("resize", function(){
                                              setHeight_BoostaR_evaluation_plot();
                                            });
                                          '))
          )
        )
      ),
    )
  )
}
    
#' buildBoostaR Server Functions
#'
#' @noRd 
mod_buildBoostaR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
