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
        selectInput(inputId = ns('model_chooser'), label = 'Select tabulated model', choices = NULL, size = 10, selectize = FALSE),
        selectInput(inputId = ns('table_chooser'), label = 'Select table', choices = NULL, size = 25, selectize = FALSE),
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 4,
            h3('Tables')
          ),
          column(
            width = 4,
            align = 'center',
            div(
              style = 'margin-top:20px',
              radioGroupButtons(
                inputId = ns('transpose_table'),
                label = NULL,
                choices = c('Normal','Transpose'),
                selected = 'Normal'
              )
            )
          ),
          column(
            width = 4,
            align = 'right',
            div(
              style = 'margin-top:20px',
              shinySaveButton(
                id = ns('export_tables'),
                label = 'Export to Excel',
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
        br(),
        DTOutput(ns('tabulated_model'))
      )
    )
  )
}
    
#' tabulatedGlimmaR Server Functions
#'
#' @noRd 
mod_GlimmaR_tabulated_models_server <- function(id, tabulated_models){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(tabulated_models(), {
      if(length(tabulated_models())>0){
        nmes <- names(tabulated_models())
        selected <- nmes[1]
        if(!is.null(input$model_chooser)){
          if(input$model_chooser %in% nmes){
            selected <- input$model_chooser
          }
        }
        updateSelectInput(session, inputId = 'model_chooser', choices = names(tabulated_models()), selected = selected)
      }
    })
    output$tabulated_model <- renderDT({
      if(!is.null(input$model_chooser)){
        tabulated_model <- tabulated_models()[[input$model_chooser]]
        GlimmaR_format_table_DT(tabulated_model, input$table_chooser, input$transpose_table)
      }
    })
    observeEvent(input$model_chooser, ignoreInit = TRUE, {
      curr_selection <- input$table_chooser
      choices <- model_table_list(tabulated_models()[[input$model_chooser]])
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

GlimmaR_format_table_DT <- function(tabulated_model, table_name, transpose){
  if(length(tabulated_model)>0 & !is.null(table_name)){
    dt <- tabulated_model[[table_name]]$table
    if(transpose=='Transpose' & tabulated_model$format=='solo'){
      dt <- transpose(dt, keep.names = names(dt)[1], make.names = names(dt)[1])
    }
    if(!is.null(dt)){
      t <- dt %>% DT::datatable(rownames= TRUE,
                                options = list(pageLength = nrow(dt),
                                               dom = 'Bfrti',
                                               scrollX = T,
                                               scrollY = 'calc(100vh - 380px)',
                                               columnDefs = list(list(visible = F, targets = 0)
                                               )
                                )
      ) %>%
        DT::formatStyle(1:ncol(dt), lineHeight='0%', fontSize = '80%')
      if(tabulated_model$format %in% c('long','long norm')){
        # make first row bold as total row
        t <- t %>% DT::formatStyle(0, target = "row", fontWeight = DT::styleEqual(1, "bold"))
        t <- t %>% DT::formatRound(c("observed", "fitted"), 4, mark = ',')
        t <- t %>% DT::formatRound(c('weight'), digits=0, mark = ',')
        t <- t %>% DT::formatRound(c('model_relativity'), digits=4, mark = ',')
      }
    } else {
      t <- data.table(V1 = 'no model tabulated') %>% DT::datatable()
    }
    return(t)
  }
}

model_table_list <- function(tabulated_model){
  if(is.null(tabulated_model)){
    table_list <- NULL
  } else {
    table_list <- names(tabulated_model)[1:(length(tabulated_model)-1)]
    table_list <- as.list(table_list)
    names(table_list) <- paste0(1:length(table_list),' - ',table_list)
  }
  return(table_list)
}