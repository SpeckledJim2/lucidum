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

format_dataset_for_DT <- function(d, transpose){
  max_rows_to_display <- 100
  max_cols <- 100
  if('total_filter' %in% names(d)){
    d_filter <- d[which(total_filter==1)]
  } else {
    d_filter <- d
  }
  if(transpose==FALSE){
    pg_length <- min(max_rows_to_display, nrow(d_filter))
  } else {
    if(nrow(d_filter)>max_cols){
      idx <- 1:max_cols
      d_filter <- utils::head(d_filter, max_cols)
    } else {
      idx <- 1:nrow(d_filter)
    }
    d_filter <- cbind(data.table(col = names(d_filter), t(d_filter)))
    names(d_filter) <- c('dataset_column', as.character(idx))
    pg_length <- min(1000, nrow(d_filter))
  }
  dt <- DT::datatable(d_filter,
                      rownames= FALSE,
                      extensions = 'Buttons',
                      #class = 'white-space: nowrap',
                      options = list(pageLength = pg_length,
                                     #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                     dom = 'Brtip',
                                     scrollX = T,
                                     scrollY = 'calc(90vh - 220px)',
                                     searchHighlight=TRUE
                      )
  ) |>
    DT::formatStyle(1:ncol(d_filter), lineHeight='0%', fontSize = '12px')
}