#' tabulatedGlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_GlimmaR_tabulated_models_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        h3('Tabulated models'),
        selectInput(inputId = ns('GlimmaR_model_chooser'), label = 'Select tabulated model', choices = NULL, size = 10, selectize = FALSE),
        selectInput(inputId = ns('GlimmaR_table_chooser'), label = 'Select table', choices = NULL, size = 30, selectize = FALSE),
      ),
      column(
        width = 9,
        h3('Tables'),
        fluidRow(
          column(
            width = 6,
            radioGroupButtons(
              inputId = ns('GlimmaR_transpose_table'),
              label = NULL,
              choices = c('Normal','Transpose'),
              selected = 'Normal'
            )
          ),
          column(
            width = 6,
            align = 'right',
            shinySaveButton(
              id = ns('GlimmaR_export_tables'),
              label = 'Export to Excel',
              title = 'Choose location to save tables',
              filename = "",
              filetype=list(txt="xlsx"),
              icon = icon('upload'),
              style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              viewtype = "detail"
            )
          )
        ),
        br(),
        DTOutput(ns('GlimmaR_tabulated_model'))
      )
    )
  )
}
    
#' tabulatedGlimmaR Server Functions
#'
#' @noRd 
mod_GlimmaR_tabulated_models_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tabulatedGlimmaR_ui("tabulatedGlimmaR_1")
    
## To be copied in the server
# mod_tabulatedGlimmaR_server("tabulatedGlimmaR_1")
