#' datasetViewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_datasetViewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(width = 12,
                    align = 'right',
                    checkboxInput(inputId=ns('transpose'), label='Transpose', value = FALSE))
             ),
    fluidRow(column(width = 12,
                    DT::DTOutput(ns('data'))
                    )
             )
  )
}
    
#' datasetViewer Server Functions
#'
#' @noRd 
mod_datasetViewer_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$data <- DT::renderDT({
      dt_update()
      format_dataset_for_DT(d(), input$transpose)
    })
  })
}

