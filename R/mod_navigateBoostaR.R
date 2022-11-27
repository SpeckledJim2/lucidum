#' navigateBoostaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DiagrammeR grVizOutput
mod_navigateBoostaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        DT::DTOutput('BoostaR_model_summary')
      )
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        fluidRow(
          column(
            width = 12,
            sliderInput("BoostaR_tree_selector",
                        width = '100%',
                        label = NULL,
                        min = 0,
                        max = 2000,
                        step = 1,
                        value = 0,
                        ticks = FALSE,
                        animate = TRUE
            )
          )
        ),
        grVizOutput("BoostaR_tree_diagram", width = '100%', height = '400px')
      ),
      column(
        width = 6,
        fluidRow(
          column(
            width = 2,
            actionButton(
              inputId = "BoostaR_gain_table_goto_ChartaR",
              icon = icon('chart-line'),
              label = ''
            )
          ),
          column(
            width = 6,
            textInput(
              'BoostaR_search_gain_table',
              label = NULL,
              width = '100%',
              placeholder = 'select feature'
            )
          ),
          column(
            width = 4,
            align = 'right',
            shinyFiles::shinySaveButton(
              id = 'BoostaR_save_model',
              label = 'Save LGBM',
              title = 'Save LightGBM model',
              filename = "",
              filetype = list(txt="txt"),
              icon = icon('upload'),
              style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              viewtype = "detail"
            )
          )
        ),
        DT::DTOutput('BoostaR_gain_summary')
      )
    )
  )
}
    
#' navigateBoostaR Server Functions
#'
#' @noRd 
mod_navigateBoostaR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_navigateBoostaR_ui("navigateBoostaR_1")
    
## To be copied in the server
# mod_navigateBoostaR_server("navigateBoostaR_1")
