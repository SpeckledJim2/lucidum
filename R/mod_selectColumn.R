#' selectColumn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param d data.frame from which you want to select a column.
#' @param numerical_cols Boolean (default = FALSE), set to TRUE to only show numerical columns.
#' @param subset Character (default = NULL), only show columns also contained in subset.
#' @param special_options Character (default = NULL), always show these options.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selectColumn_ui <- function(id, label = 'label', ...){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css',
               ".selectize-input {font-size: 13px; line-height: 16px; min-height: 20px}
                .selectize-dropdown-content {max-height: 400px; }
                .selectize-dropdown {font-size: 13px; line-height: 16px}"),
    div(
      style='margin-bottom:-15px;',
      selectInput(
        inputId = ns('col'),
        label = label,
        choices = NULL,
        ...
      )
    )
  )
}
    
#' selectColumn Server Functions
#'
#' @noRd 
mod_selectColumn_server <- function(id, d, dt_update, numerical_cols, subset, special_options){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(dt_update(), {
      if(nrow(d())>0){
        current_selection <- input$col
        choices <- getColumnChoices(d(), numerical_cols, subset, special_options)
        selected <- input$col
        if(selected %not_in% choices){
          selected <- choices[[1]]
        }
        updateSelectInput(
          inputId = 'col',
          choices = choices,
          selected = selected
        )
      }
    })
    return(reactive({input$col}))
  })
}
