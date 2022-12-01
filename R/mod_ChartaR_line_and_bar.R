#' ChartaR_line_and_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ChartaR_line_and_bar_ui <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        selectInput_ui(id = ns('x_axis_feature'), label = 'x-axis feature', height_divisor = 50, height_adj = 2, multiple = FALSE),
        selectInput_ui(id = ns('add_columns'), label = 'Additional y-axis columns', height_divisor = 55, height_adj = 3, multiple = TRUE)
      ),
      column(
        width = 9,
        fluidRow(
          column(3,
                 radioGroupButtons(
                   inputId = "ChartaR_1W_sort_order",
                   label = "x-axis sort order",
                   choices = c('A-Z', 'Wt', 'Act','Add','PD'),
                   individual = FALSE,
                   size = 'xs',
                   selected = 'A-Z')
          ),
          column(3,
                 radioGroupButtons(
                   inputId = "ChartaR_1W_group_low_exposure",
                   label = "Group low weights",
                   choices = c(0,5,10,20,50,'1%'),
                   individual = FALSE,
                   size = 'xs',
                   selected = 0)
          ),
          column(2,
                 align = 'left',
                 radioGroupButtons(
                   inputId = "ChartaR_A_vs_E_show_labels",
                   label = "Labels",
                   choices = c('-', 'Weight','All'),
                   individual = FALSE,
                   size = 'xs',
                   selected = '-'
                 )
          ),
          column(4,
                 align = 'right',
                 radioGroupButtons(
                   inputId = "ChartaR_1W_banding",
                   label = "Banding",
                   choices = c('<','0.01','0.1','1','5','10','100','>',`<i class='fa fa-lock'></i>` = 'lock'),
                   individual = FALSE,
                   size = 'xs',
                   selected = -1)
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioGroupButtons(
              inputId = "ChartaR_1W_error_bars",
              label = "Error bars",
              choices = c('-'='-', '90%'=0.9, '95%'=0.95, '99%'=0.99),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = "ChartaR_A_vs_E_show_partial_dependencies",
              label = "Partial dependencies",
              choices = c('-','GLM','GBM','Both'),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = "ChartaR_show_response",
              label = "Response",
              choices = c('Hide','Show'),
              individual = FALSE,
              size = 'xs',
              selected = 'Show')
          ),
          column(
            width = 3,
            align = 'right',
            radioGroupButtons(
              inputId = "ChartaR_1W_y_transform",
              label = "Response transform",
              choices = c('-', 'Log','Exp','Logit','0','1'),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          )
        ),
        tabsetPanel(
          id = 'ChartaR_one_way_tabs',
          type = 'tabs',
          tabPanel('Chart',
                   h2('Chart')
          ),
          tabPanel('Table',
                   br(),
                   h2('Table')
          )
        )
      )
    )
  )
}
    
#' ChartaR_line_and_bar Server Functions
#'
#' @noRd 
mod_ChartaR_line_and_bar_server <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec, BoostaR_models, BoostaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    one_way_x_axis_feature <- selectInput_server(
      id = 'x_axis_feature',
      d,
      dt_update,
      feature_spec,
      BoostaR_models,
      BoostaR_idx,
      FALSE
    )
    # one_way_add_columns <-  selectInput_server(
    #   id = '1W_add_columns',
    #   all_cols = reactive(names(d())),
    #   feature_spec = feature_spec,
    #   initial_selected = FALSE,
    #   update = dt_update
    # )
  })
}
    
## To be copied in the UI
# mod_ChartaR_line_and_bar_ui("ChartaR_line_and_bar_1")
    
## To be copied in the server
# mod_ChartaR_line_and_bar_server("ChartaR_line_and_bar_1")
