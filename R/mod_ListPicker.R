#' ListPicker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param list list
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ListPicker_ui <- function(id, icon, prefix){
  ns <- NS(id)
  tagList(
    div(
      style='display: flex; margin-top: 10px',
      actionButton(inputId = ns('previous_item'), label = '<', style="text-align: left; margin-right: 0px; padding:4px"),
      actionButton(inputId = ns('navigate'), label = paste(prefix, '0/0'), icon = icon, style = "margin-left: 0px; margin-right: 0px; padding:4px"),
      actionButton(inputId = ns('next_item'), label = '>', style="text-align: right; margin-left: 0px; padding:4px")
    )
  )
}
    
#' ListPicker Server Functions
#'
#' @noRd 
mod_ListPicker_server <- function(id, input_list, prefix){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    pick <- reactiveVal(0)
    observeEvent(input_list, {
      n_list <- length(input_list())
      if(n_list>0){
        pick(1)
        updateActionButton(inputId = 'navigate', label = paste0(prefix, ' ', pick(),'/',n_list))
      }
    })
    observeEvent(input$previous_item, {
      n_list <- length(input_list())
      if(n_list>0){
        pick(max(1, pick()-1))
        updateActionButton(inputId = 'navigate', label = paste0(prefix, ' ', pick(),'/',n_list))
      }
    })
    observeEvent(input$next_item, {
      n_list <- length(input_list())
      if(n_list>0){
        pick(min(n_list, pick()+1))
        updateActionButton(inputId = 'navigate', label = paste0(prefix, ' ', pick(),'/',n_list))
      }
    })
  return(pick)
  })
}
    
## To be copied in the UI
# mod_ListPicker_ui("ListPicker_1")
    
## To be copied in the server
# mod_ListPicker_server("ListPicker_1")
