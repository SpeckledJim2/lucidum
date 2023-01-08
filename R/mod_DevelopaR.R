#' DevelopaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyAce aceEditor
mod_DevelopaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns('tabsetPanel'),
                tabPanel(value = 'KPI specification', title = span(tagList(tags$img(src='www/kpi.png', height="30px", width="30px"), 'KPI specification')),
                         br(),
                         fluidRow(
                           column(
                             width = 4,
                             div(
                               p("Use the KPI specification to apply formatting to KPI's
                                       in summary charts and tables")
                             ),
                             style = 'font-size: 20px; font-weight: 400'
                           ),
                           column(
                             width = 8,
                             div(
                               p('kpi_name: the name of the KPI', style = 'margin: 0 0 0 0'),
                               p('kpi_numerator: numerical column name', style = 'margin: 0 0 0 0'),
                               p('kpi_denominator: numerical column name, N (equal weights per row) or "no weights" (view totals instead of averages) ', style = 'margin: 0 0 0 0'),
                               p('kpi_dp: number of decimal places to display (over-rides kpi_signif)', style = 'margin: 0 0 0 0'),
                               p('kpi_signif: number of significant digits to display', style = 'margin: 0 0 0 0'),
                               p('kpi_divisor: e.g. 0.01 for percentages, 1000 for thousands', style = 'margin: 0 0 0 0'),
                               p('kpi_prefix: character text to appear in front of KPI, e.g. $', style = 'margin: 0 0 0 0'),
                               p('kpi_suffix: character text to appear after KPI, e.g. %', style = 'margin: 0 0 0 0')
                             ),
                             style = 'font-size: 12px; font-weight: 400;'
                           )
                         ),
                         tags$hr(style="border-color: black; margin-bottom: 6px"),
                         mod_editSpecification_ui(ns('kpi'))
                         ),
                tabPanel(value = 'Feature specification', title = span(tagList(tags$img(src='www/features.png', height="30px", width="30px"), 'Feature specification')),
                         br(),
                         fluidRow(
                           column(
                             width = 4,
                             div(
                               p("Use the feature specification to define feature groupings
                                       and set GlimmaR model export base levels and bandings")
                             ),
                             style = 'font-size: 20px; font-weight: 400'
                           ),
                           column(
                             width = 8,
                             div(
                               p('feature: the name of the KPI', style = 'margin: 0 0 0 0'),
                               p('base_level: level set to 1.000 in GlimmaR table export', style = 'margin: 0 0 0 0'),
                               p('min: minimum value for continuous features', style = 'margin: 0 0 0 0'),
                               p('max: maximum value for continuous features', style = 'margin: 0 0 0 0'),
                               p('banding: banding for continuous features', style = 'margin: 0 0 0 0'),
                               p('monotonicity: for BoostaR models', style = 'margin: 0 0 0 0'),
                               p('interaction_grouping: for feature selection', style = 'margin: 0 0 0 0'),
                               p('use subsequent columns to define feature scenarios for BoostaR models with the word "feature"', style = 'margin: 0 0 0 0')
                             ),
                             style = 'font-size: 12px; font-weight: 400;'
                           )
                         ),
                         tags$hr(style="border-color: black; margin-bottom: 6px"),
                         mod_editSpecification_ui(ns('feature'))
                         ),
                tabPanel(value = 'Filter specification', title = span(tagList(tags$img(src='www/filter.png', height="30px", width="30px"), 'Filter specification')),
                         br(),
                         fluidRow(
                           column(
                             width = 4,
                             div(
                               p("Use the filter specification to define filters to apply to charts and tables")
                             ),
                             style = 'font-size: 20px; font-weight: 400'
                           ),
                           column(
                             width = 8,
                             div(
                               p('the filter expression is an R statement', style = 'margin: 0 0 0 0'),
                               p('that evaluates to TRUE/FALSE or 1/0', style = 'margin: 0 0 0 0'),
                               p('using the dataset column names', style = 'margin: 0 0 0 0'),
                               p('e.g. my_column>5 or my_column=="my_text"', style = 'margin: 0 0 0 0'),
                               p('use == for equality', style = 'margin: 0 0 0 0'),
                               p('use & for logical AND', style = 'margin: 0 0 0 0'),
                               p('use | for logical OR', style = 'margin: 0 0 0 0'),
                               p(' ', style = 'margin: 0 0 0 0'),
                               p(' ', style = 'margin: 0 0 0 0')
                             ),
                             style = 'font-size: 12px; font-weight: 400;'
                           )
                         ),
                         tags$hr(style="border-color: black; margin-bottom: 6px"),
                         mod_editSpecification_ui(ns('filter'))
                         ),
                tabPanel(value = 'shinyAce', title = span(tagList(tags$img(src='www/shinyAce.png', height="30px", width="30px"), 'shinyAce')),
                         fluidRow(
                           column(
                             width = 12,
                             fluidRow(
                               column(
                                 width = 6,
                                 h3('shinyAce')
                               ),
                               column(
                                 width = 6,
                                 align = 'right',
                                 br(),
                                 actionButton(
                                   inputId = ns('shinyAce_textsize_minus'),
                                   label = "A-"
                                 ),
                                 actionButton(
                                   inputId = ns('shinyAce_textsize_plus'),
                                   label = "A+"
                                 ),
                                 actionButton(
                                   ns('shinyAce_evaluate'),
                                   label = 'Evaluate',
                                   icon = icon("chevron-right"),
                                   style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left"
                                 )
                               )
                             ),
                             aceEditor(
                               ns('shinyAce_code'),
                               mode = "r",
                               fontSize = 16,
                               wordWrap = FALSE,
                               height = 'calc(40vh)',
                               autoScrollEditorIntoView = TRUE,
                               value =
"# evaluate glm tabulation error
# d()[, sd(glm_prediction/glm_tabulated_prediction, na.rm=TRUE)]

# calculate the ratio of GBM to GLM prediction
# d()[, model_ratio := lgbm_prediction/glm_prediction]
# d()[, sd(model_ratio), by = train_test]

# copy all GLM prediction to dataset
# for(g in GlimmaR_models()){d()[, (g$name):=g$predictions]}

# copy all GBM prediction to dataset
# for(b in BoostaR_models()){d()[, (b$name):=b$predictions]}
"
                             )
                           )
                         ),
                         fluidRow(
                           column(
                             width = 12,
                             # needs namespace
                             tags$head(tags$style(paste0('#',ns('shinyAce_output'),'{font-size:14px; overflow-y:scroll; max-height: 360px; background: ghostwhite; white-space: pre-wrap}'))),
                             verbatimTextOutput(ns('shinyAce_output'))
                             )
                           )
                         )
                )
    )
}
    
#' DevelopaR Server Functions
#'
#' @noRd 
#' 
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom shinyAce updateAceEditor
#' 
mod_DevelopaR_server <- function(id, d, dt_update, kpi_spec, filter_spec, feature_spec, BoostaR_models, GlimmaR_models, BoostaR_idx, GlimmaR_idx, dimensions){
  moduleServer( id, function(input, output, session){
    updated_kpi_spec <- reactiveVal()
    updated_filter_spec <- reactiveVal()
    updated_feature_spec <- reactiveVal()
    ns <- session$ns
    shinyAce_text_size <- reactiveVal(16)
    observeEvent(input$shinyAce_evaluate, {
      result <- tryCatch({eval(parse(text = input$shinyAce_code))}, error = function(e){e})
      if(class(result)[1]=='simpleError'){
        # something went wrong
        confirmSweetAlert(session = session,
                          type = 'error',
                          inputId = "DataR_error",
                          title = 'Evaluation error',
                          text = result$message,
                          btn_labels = c('OK'))
      } else {
        output$shinyAce_output <- renderPrint(result, width = 1000)
        dt_update(dt_update()+1)
      }
    })
    observeEvent(input$shinyAce_textsize_minus, {
      shinyAce_text_size(pmax(8,shinyAce_text_size()-1))
      updateAceEditor(session, editorId = 'shinyAce_code', fontSize = shinyAce_text_size())
    })
    observeEvent(input$shinyAce_textsize_plus, {
      shinyAce_text_size(min(30,shinyAce_text_size()+1))
      updateAceEditor(session, editorId = 'shinyAce_code', fontSize = shinyAce_text_size())
    })
    updated_kpi_spec <- mod_editSpecification_server('kpi', kpi_spec, type = 'kpi', dimensions)
    updated_filter_spec <- mod_editSpecification_server('filter', filter_spec, type = 'filter', dimensions)
    updated_feature_spec <- mod_editSpecification_server('feature', feature_spec, type = 'feature', dimensions)
    observeEvent(updated_kpi_spec(), {
      if(!identical(kpi_spec(), updated_kpi_spec())){
        kpi_spec(updated_kpi_spec())
      }
    })
    observeEvent(updated_filter_spec(), {
      if(!identical(filter_spec(), updated_filter_spec())){
        filter_spec(updated_filter_spec())
      }
    })
    observeEvent(updated_feature_spec(), {
      if(!identical(feature_spec(), updated_feature_spec())){
        feature_spec(updated_feature_spec())
      }
    })
  })
}
