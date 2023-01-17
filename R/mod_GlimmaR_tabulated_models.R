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
        fluidRow(
          column(
            width = 6,
            h3('Tabulations')
          ),
          column(
            width = 6,
            align = 'right',
            div(
              style = 'margin-top:20px',
              shinySaveButton(
                id = ns('export_tables'),
                label = 'Excel',
                title = 'Choose location to save tables',
                filename = "",
                filetype=list(txt="xlsx"),
                icon = icon('upload'),
                style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
                viewtype = "detail"
              )
            )
          )
        ),
        selectInput(inputId = ns('model_chooser'), label = 'Select tabulated model', choices = NULL, size = 10, selectize = FALSE),
        selectInput(inputId = ns('table_chooser'), label = 'Select table', choices = NULL, size = 25, selectize = FALSE),
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 3,
            h3('Tables')
          ),
          column(
            width = 3,
            
          ),
          column(
            width = 3,
            div(
              style = 'margin-top:20px',
              radioGroupButtons(
                inputId = ns('transform'),
                choices = c('-','exp'),
                selected = '-'
              )
            )
          ),
          column(
            width = 3,
            align = 'right',
            div(
              style = 'margin-top:20px',
              checkboxInput(
                inputId = ns('show_terms'),
                label = 'Show terms',
                value = FALSE
              )
            )
          )
        ),
        br(),
        DTOutput(ns('tabulated_model'))
      )
    )
  )
}
    
#' tabulatedGlimmaR Server Functions
#'
#' @noRd 
mod_GlimmaR_tabulated_models_server <- function(id, GlimmaR_models){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(GlimmaR_models(), {
      if(length(GlimmaR_models())>0){
        # identify which models have a tabulations slot present and their names
        tabulated <- !sapply(lapply(GlimmaR_models(), '[[','tabulations'), is.null)
        tabulated_models <- names(GlimmaR_models())[tabulated]
        selected <- tabulated_models[1]
        # don't change the selection
        if(!is.null(input$model_chooser)){
          if(input$model_chooser %in% tabulated_models){
            selected <- input$model_chooser
          }
        }
        updateSelectInput(session, inputId = 'model_chooser', choices = tabulated_models, selected = selected)
      }
    })
    
    output$tabulated_model <- renderDT({
      if(!is.null(input$model_chooser)){
        tabulation <- GlimmaR_models()[[input$model_chooser]]$tabulations[[input$table_chooser]]
        GlimmaR_format_table_DT(tabulation, input$table_chooser, input$transform, input$show_terms)
      }
    })
    observeEvent(input$model_chooser, ignoreInit = TRUE, {
      curr_selection <- input$table_chooser
      choices <- model_table_list(GlimmaR_models()[[input$model_chooser]]$tabulations)
      if(length(curr_selection)==0){
        selected <- choices[1]
      } else {
        if(curr_selection %in% choices){
          selected <- curr_selection
        } else {
          selected <- choices[1]
        }
      }
      updateSelectInput(session, inputId = 'table_chooser', choices = choices, selected = selected)
    })
  })
}
    
## To be copied in the UI
# mod_tabulatedGlimmaR_ui("tabulatedGlimmaR_1")
    
## To be copied in the server
# mod_tabulatedGlimmaR_server("tabulatedGlimmaR_1")

GlimmaR_format_table_DT <- function(tabulation, vars, transform, show_terms){
  if(!is.null(tabulation)){
    # split out the individual vars in the table
    vars <- unlist(strsplit(vars, '|', fixed = TRUE))
    dt <- copy(tabulation)
    if(show_terms){
      # leave alone
    } else {
      if(vars[1]=='base'){
        # leave alone
      } else {
        # keep the cols containing vars and the last column
        # i.e. leave out the terms columns
        keep_idx <- c(1:length(vars), ncol(dt))
        keep_cols <- names(dt)[keep_idx]
        dt <- dt[, ..keep_cols]
      }
    }
    if(vars[1]=='base'){
      transform_idx <- 1
    } else {
      transform_idx <- setdiff(1:ncol(dt), 1:length(vars))
    }
    if(transform=='exp'){
      dt[, (transform_idx) := exp(.SD),.SDcols=transform_idx]
    }
    if(nrow(dt)>1000){
      dt <- head(dt,1000)
    }
    if(!is.null(dt)){
      t <- dt |> datatable(
        rownames= TRUE,
        options = list(pageLength = nrow(dt),
                       #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                       dom = 'Bfrti',
                       scrollX = T,
                       scrollY = 'calc(100vh - 380px)',
                       columnDefs = list(list(visible = F, targets = 0))
                       )
        ) |>
        formatStyle(columns = 1:ncol(dt), lineHeight='0%', fontSize = '14px') |>
        formatRound(columns = transform_idx, digits = 6)
    } else {
      t <- data.table(V1 = 'no model tabulated') %>% DT::datatable()
    }
    return(t)
  }
}

model_table_list <- function(tabulations){
  if(is.null(tabulations)){
    table_list <- NULL
  } else {
    table_list <- names(tabulations)
    table_list <- as.list(table_list)
    names(table_list) <- paste0(1:length(table_list),' - ',table_list)
  }
  return(table_list)
}