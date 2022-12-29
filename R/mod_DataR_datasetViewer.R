#' datasetViewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DataR_datasetViewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6
      ),
      column(
        width = 3,
        align = 'right',
        div(
          style = 'margin-top:20px',
          checkboxInput(
            inputId = ns('transpose'),
            label='Transpose',
            value = FALSE
            )
        )
      ),
      column(
        width = 3,
        align = 'right',
        div(
          style = 'margin-top:15px',
          radioGroupButtons(
            inputId = ns('columns_to_display'),
            label = NULL,
            choices = c('Dataset','Lucidum','All'),
            selected = 'Dataset'
            )
          )
        )
      ),
    fluidRow(
      column(
        width = 12,
        DT::DTOutput(ns('data'))
        )
      )
  )
}
    
#' datasetViewer Server Functions
#' 
#' @param id Internal parameter for {shiny}.
#' @param d reactiveVal containing the data.frame or data.table to be displayed
#' @param dt_update reactiveVal to trigger an update (as data.table is not reactive)
#' 
#' @noRd 
#' 
#' @importFrom DT renderDT
mod_DataR_datasetViewer_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(c(dt_update(), input$transpose, input$columns_to_display), {
      output$data <- renderDT({
        create_DT_from_dataframe(d(), input$transpose, input$columns_to_display)
        })
    })
  })
}

#' Create dataset viewer using DataTable
#'
#' @param d data.frame to be displayed
#' @param transpose boolean, TRUE to transpose the table view
#' @param columns_to_display character column names to display
#'
#' @return DataTable HTML
#' 
#' @importFrom utils head
#' @importFrom DT datatable formatStyle
create_DT_from_dataframe <- function(d, transpose, columns_to_display){
  if(!is.null(d)){
    max_rows_to_display <- 100
    max_cols <- 100
    # apply filter
    if('total_filter' %in% names(d)){
      d_filter <- d[which(total_filter==1)]
    } else {
      d_filter <- d
    }
    # only include selected cols
    all_cols <- names(d_filter)
    non_lucidum_cols <- remove_lucidum_cols(all_cols)
    lucidum_cols <- setdiff(all_cols, non_lucidum_cols)
    if(columns_to_display=='Dataset'){
      d_filter <- d_filter[, ..non_lucidum_cols]
    } else if(columns_to_display=='Lucidum'){
      d_filter <- d_filter[, ..lucidum_cols]
    } else if(columns_to_display=='All'){
      
    }
    # transpose if selected
    if(transpose==FALSE){
      pg_length <- min(max_rows_to_display, nrow(d_filter))
    } else {
      if(nrow(d_filter)>max_cols){
        idx <- 1:max_cols
        d_filter <- head(d_filter, max_cols)
      } else {
        idx <- 1:nrow(d_filter)
      }
      d_filter <- cbind(data.table(col = names(d_filter), t(d_filter)))
      names(d_filter) <- c('dataset_column', as.character(idx))
      pg_length <- min(1000, nrow(d_filter))
    }
    # create the datatable
    datatable(d_filter,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(
                # change font size of header row
                headerCallback = JS(
                  "function(thead) {",
                  "  $(thead).css('font-size', '12px');",
                  "}"
                ),
                pageLength = pg_length,
                dom = 'Bfrtip',
                scrollX = T,
                scrollY = 'calc(90vh - 220px)',
                searchHighlight=TRUE
              )
    ) |>
      formatStyle(1:ncol(d_filter), lineHeight = '0%', fontSize = '12px') |>
      formatStyle(1:ncol(d_filter), 'white-space' = 'nowrap')
  }
}
