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
            width = 12,
            h3('Tabulations')
          )
        ),
        selectInput(inputId = ns('model_chooser'), label = 'Select tabulated model', choices = NULL, size = 10, selectize = FALSE),
        selectInput(inputId = ns('table_chooser'), label = 'Select table', choices = NULL, size = 25, selectize = FALSE),
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 2,
            h3('Tables')
          ),
          column(
            width = 2,
            div(
              style = 'margin-top:20px',
              radioGroupButtons(
                inputId = ns('transform'),
                choices = c('-','exp'),
                selected = '-',
                size = 'sm'
              )
            )
          ),
          column(
            width = 2,
            style = 'padding-left:0px; margin-left:0px; padding-right:0px, margin-right:0px',
            div(
              style = 'margin-top:20px',
              radioGroupButtons(
                inputId = ns('show_terms'),
                choices = c('-','terms'),
                selected = '-',
                size = 'sm'
              )
            )
          ),
          column(
            width = 2,
            div(
              style = 'margin-top:20px',
              radioGroupButtons(
                inputId = ns('colour_table'),
                choiceValues = c('-','colours'),
                choiceNames = c(
                  '-',
                  tagList(tags$img(src='www/divergent.png', height="18px", width="18px"))
                ),
                selected = '-',
                size = 'sm'
              )
            )
          ),
          column(
            width = 3,
            div(
              style = 'margin-top:20px',
              selectInput(
                inputId = ns('crosstab'),
                label = NULL,
                choices = 'no crosstab',
                width = '100%'
              )
            )
          ),
          column(
            width = 1,
            align = 'right',
            div(
              style = 'margin-top:18px',
              shinySaveButton(
                id = ns('export_tables'),
                label = NULL,
                title = 'Choose location to save tables',
                filename = "",
                filetype=list(txt="xlsx"),
                icon = icon('upload'),
                style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
                viewtype = "detail"
              )
            ),
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
    observeEvent(input$model_chooser, ignoreInit = TRUE, {
      # update the table_chooser selectInput
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
    observeEvent(input$table_chooser, ignoreInit = TRUE, {
      # update the crosstab selectInput
      if(is.null(input$table_chooser)){
        choices <- c('no crosstab')
      } else {
        vars <- unlist(strsplit(input$table_chooser, '|', fixed = TRUE))
        if(length(vars)>1){
          choices <- c('no crosstab', setdiff(vars, 'base'))
        } else {
          choices <- c('no crosstab')
        }
      }
      curr_selection_crosstab <- input$crosstab
      if(length(curr_selection_crosstab)==0){
        selected_crosstab <- curr_selection_crosstab
      } else {
        if(curr_selection_crosstab %in% choices){
          selected_crosstab <- curr_selection_crosstab
        } else {
          selected_crosstab <- 'no crosstab'
        }
      }
      updateSelectInput(session, inputId = 'crosstab', choices = choices, selected = selected_crosstab)
    })
    output$tabulated_model <- renderDT({
      if(!is.null(input$model_chooser)){
        vars <- unlist(strsplit(input$table_chooser, '|', fixed = TRUE))
        tabulation <- GlimmaR_models()[[input$model_chooser]]$tabulations[[input$table_chooser]]
        if(input$crosstab %in% c('no crosstab', vars)){
          GlimmaR_format_table_DT(tabulation, input$table_chooser, input$transform, input$show_terms, input$crosstab, input$colour_table)
        }
      }
    })
  })
}
    
## To be copied in the UI
# mod_tabulatedGlimmaR_ui("tabulatedGlimmaR_1")
    
## To be copied in the server
# mod_tabulatedGlimmaR_server("tabulatedGlimmaR_1")

#' @importFrom DT styleInterval
GlimmaR_format_table_DT <- function(tabulation, vars, transform, show_terms, crosstab, colour_table){
  if(!is.null(tabulation)){
    # split out the individual vars in the table
    vars <- unlist(strsplit(vars, '|', fixed = TRUE))
    dt <- copy(tabulation)
    if(show_terms=='terms'){
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
    if(crosstab!='no crosstab'){
      # dcast the table
      lhs_vars <- setdiff(vars, crosstab)
      dcast_form <- paste0(paste0(lhs_vars, collapse = '+'),'~',crosstab)
      dt <- dcast(dt, dcast_form, value.var = 'total')
    } else {
      lhs_vars <- vars
    }
    # value transform
    if(vars[1]=='base'){
      transform_idx <- 1
    } else {
      transform_idx <- setdiff(1:ncol(dt), 1:length(lhs_vars))
    }
    if(transform=='exp'){
      dt[, (transform_idx) := exp(.SD),.SDcols=transform_idx]
    }
    pg_length <- min(1000, nrow(dt))
    # cell colours
    if(colour_table=='colours'){
      values <- dt[, .SD, .SDcols=transform_idx]
      values <- as.matrix(values)
      if(transform=='-'){
        max_abs_value <- max(0.1,max(abs(values)))
        step <- max_abs_value/20
        brks_down <- seq(-max_abs_value,0,step)
        brks_up <- seq(step,max_abs_value,step)
      } else {
        max_abs_value <- max(abs(log(values)))
        step <- max_abs_value/20
        brks_down <- exp(seq(-max_abs_value,0,step))
        brks_up <- exp(seq(step,max_abs_value,step))
      }
      clrs_down <- round(seq(100, 255, length.out = length(brks_down)), 0) %>% {paste0("rgb(",.,",255,", ., ")")}
      clrs_up <- round(seq(255, 100, length.out = length(brks_up) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
      brks <- c(brks_down, brks_up)
      clrs <- c(clrs_down, clrs_up)
    }
    if(!is.null(dt)){
      t <- dt |> datatable(
        rownames= TRUE,
        options = list(pageLength = nrow(dt),
                       #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                       dom = 'Bfrti',
                       scrollX = T,
                       scrollY = 'calc(100vh - 400px)',
                       pageLength = pg_length,
                       columnDefs = list(list(visible = F, targets = 0))
                       )
        ) |>
        formatStyle(columns = 1:ncol(dt), lineHeight='0%', fontSize = '14px') |>
        formatRound(columns = transform_idx, digits = 6)
      if(vars[1]!='base' & colour_table=='colours'){
        t <- t |> formatStyle(columns = transform_idx, backgroundColor = styleInterval(brks, clrs))
      }
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