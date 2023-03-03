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
mod_GlimmaR_tabulated_models_server <- function(id, GlimmaR_models, BoostaR_models){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    tabulated_glms <- c()
    tabulated_lgbms <- c()
    observeEvent(c(GlimmaR_models(), BoostaR_models()), {
      if(length(GlimmaR_models())>0){
        # identify which models have a tabulations slot present and their names
        tabulated <- !sapply(lapply(GlimmaR_models(), '[[','tabulations'), is.null)
        tabulated_glms <- names(GlimmaR_models())[tabulated]
      }
      if(length(BoostaR_models())>0){
        # identify which models have a tabulations slot present and their names
        tabulated <- !sapply(lapply(BoostaR_models(), '[[','tabulations'), is.null)
        tabulated_lgbms <- names(BoostaR_models())[tabulated]
      }
      tabulated_models <- c(tabulated_glms, tabulated_lgbms)
      # don't change the selection
      if(length(tabulated_models)>0){
        selected <- tabulated_models[1]
        if(!is.null(input$model_chooser)){
          if(input$model_chooser %in% tabulated_models){
            selected <- input$model_chooser
          }
        }
        updateSelectInput(session, inputId = 'model_chooser', choices = tabulated_models, selected = selected)
      }
    })
    observeEvent(input$model_chooser, ignoreInit = TRUE, {
      # update the table_chooser selectInput depending on which model is selected
      # get the selectInput choices
      curr_selection <- input$table_chooser
      if(input$model_chooser %in% names(GlimmaR_models())){
        choices <- model_table_list(GlimmaR_models()[[input$model_chooser]]$tabulations)
      } else if (input$model_chooser %in% names(BoostaR_models())){
        choices <- model_table_list(BoostaR_models()[[input$model_chooser]]$tabulations)
      }
      # decide what is selected
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
    observeEvent(input$export_tables, {
      volumes <- c('working directory' = getwd(), 'home' = path_home())
      shinyFileSave(input, "export_tables", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$export_tables)
      if(!is.null(input$model_chooser)){
        if(length(fileinfo$datapath)>0){
          write_tables_to_excel(GlimmaR_models()[[input$model_chooser]]$tabulations, input$transform, fileinfo$datapath)
          showNotification(paste0(fileinfo$datapath, ' created'), duration = 5, type = 'message')
        }
      }
    })
    output$tabulated_model <- renderDT({
      if(!is.null(input$model_chooser)){
        vars <- unlist(strsplit(input$table_chooser, '|', fixed = TRUE))
        if(input$model_chooser %in% names(GlimmaR_models())){
          tabulation <- GlimmaR_models()[[input$model_chooser]]$tabulations[[input$table_chooser]]
          type <- 'glm'
        } else if (input$model_chooser %in% names(BoostaR_models())){
          tabulation <- BoostaR_models()[[input$model_chooser]]$tabulations[[input$table_chooser]]
          type <- 'lgbm'
        }
        if(input$crosstab %in% c('no crosstab', vars)){
          GlimmaR_format_table_DT(tabulation, input$table_chooser, input$transform, input$show_terms, input$crosstab, input$colour_table, type)
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
GlimmaR_format_table_DT <- function(tabulation, vars, transform, show_terms, crosstab, colour_table, type){
  if(!is.null(tabulation)){
    # split out the individual vars in the table
    vars <- unlist(strsplit(vars, '|', fixed = TRUE))
    dt <- copy(tabulation)
    if(type=='glm'){
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
    }
    if(crosstab!='no crosstab'){
      # dcast the table
      lhs_vars <- setdiff(vars, crosstab)
      dcast_form <- paste0(paste0(lhs_vars, collapse = '+'),'~',crosstab)
      if(type=='glm'){
        dt <- dcast(dt, dcast_form, value.var = 'tabulated_glm')
      } else if (type=='lgbm'){
        dt <- dcast(dt, dcast_form, value.var = 'tabulated_lgbm')
      }
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

#' @importFrom openxlsx createWorkbook addWorksheet addStyle createStyle writeData saveWorkbook setColWidths
write_tables_to_excel <- function(tables, transform, filename){
  # takes a set of exported tables and the model coefficients and writes them to Excel
  # define a white text on blue background style to use for all header rows
  headerStyle_center <- createStyle(bgFill = "#222222", fontColour = "#FFFFFF", halign = 'center')
  headerStyle_left <- createStyle(bgFill = "#222222", fontColour = "#FFFFFF", halign = 'left')
  # write the coefficients table to Excel
  wb <- createWorkbook()
  # format the index table
  n_tables <- length(tables)
  index_table <- data.table(index = integer(),
                            table = character(),
                            dimensions = integer(),
                            num_rows = integer(),
                            min_value = numeric(),
                            max_value = numeric(),
                            span =numeric())[1:n_tables]
  # fill in the base table
  index_table[1,index := 1]
  index_table[1,table := 'base']
  index_table[1,dimensions := 0]
  index_table[1,num_rows := 1]
  if(transform=='exp'){
    index_table[1,min_value := exp(tables[[1]]$base)]
    index_table[1,max_value := exp(tables[[1]]$base)]
  } else {
    index_table[1,min_value := tables[[1]]$base]
    index_table[1,max_value := tables[[1]]$base]
  }
  for (i in 2:n_tables){
    vars <- unlist(strsplit(names(tables)[i], '|', fixed = TRUE))
    index_table[i,index := i]
    index_table[i,table := names(tables)[i]]
    index_table[i,dimensions := length(vars)]
    index_table[i,num_rows := nrow(tables[[i]])]
    index_table[i,min_value := min(tables[[i]][['tabulated_glm']])]
    index_table[i,max_value := max(tables[[i]][['tabulated_glm']])]
    if(transform=='exp'){
      index_table[i,min_value := exp(min_value)]
      index_table[i,max_value := exp(max_value)]
      index_table[i,span := max_value/min_value]
    } else {
      index_table[i,span := max_value-min_value]
    }
  }
  # write the index worksheet to Excel
  addWorksheet(wb, "index")
  writeData(wb, "index", index_table)
  setColWidths(wb, "index", cols = 1, widths = 10)
  setColWidths(wb, "index", cols = 2, widths = 40)
  setColWidths(wb, "index", cols = 3:7, widths = 15)
  addStyle(wb, sheet = "index", cols = 1:2, rows = 1:200, style = createStyle(halign = 'left'), gridExpand = TRUE)
  addStyle(wb, sheet = "index", cols = 3:7, rows = 1:200, style = createStyle(halign = 'center'), gridExpand = TRUE)
  addStyle(wb, sheet = "index", style=headerStyle_left, cols=1:2, rows=1)
  addStyle(wb, sheet = "index", style=headerStyle_center, cols=3:7, rows=1)
  # write the tables to Excel
  for (i in 1:n_tables){
    vars <- unlist(strsplit(names(tables)[i], '|', fixed = TRUE))
    if (vars[1]=='base'){
      # special format for the base level
      addWorksheet(wb,as.character(i))
      setColWidths(wb, as.character(i), cols = 1:2, widths = 30)
      addStyle(wb = wb, sheet = as.character(i), cols = 1L, rows = 1:2, style = createStyle(halign = 'left'))
      addStyle(wb, sheet = as.character(i), cols = 2, rows = 1:2, style = createStyle(halign = 'center'))
      addStyle(wb, sheet = as.character(i), style=headerStyle_left, cols=1, rows=1)
      addStyle(wb, sheet = as.character(i), style=headerStyle_center, cols=2, rows=1)
      table_to_write <- data.table(base = character(), tabulated_glm = double())[1]
      table_to_write[1, base := 'base']
      table_to_write[1, tabulated_glm := tables[[i]]$base]
      if(transform=='exp'){
        table_to_write[, tabulated_glm:=exp(tabulated_glm)]
      }
      writeData(wb, as.character(i), table_to_write)
    } else {
      # add worksheet and format
      n_var <- length(vars)
      n_row <- nrow(tables[[i]])
      addWorksheet(wb,as.character(i))
      setColWidths(wb, as.character(i), cols = 1:(n_var+1), widths = 30)
      addStyle(wb = wb, sheet = as.character(i), cols = 1:n_var, rows = 1:(n_row+1), style = createStyle(halign = 'left'), gridExpand = TRUE)
      addStyle(wb, sheet = as.character(i), cols = n_var+1, rows = 1:(n_row+1), style = createStyle(halign = 'center'), gridExpand = TRUE)
      addStyle(wb, sheet = as.character(i), style=headerStyle_left, cols=1:n_var, rows=1)
      addStyle(wb, sheet = as.character(i), style=headerStyle_center, cols=n_var+1, rows=1)
      # create table in correct format
      cols <- names(tables[[i]])[c(1:length(vars), ncol(tables[[i]]))] # leaves out the terms
      table_to_write <- tables[[i]][, ..cols]
      if(transform=='exp'){
        table_to_write[, tabulated_glm:=exp(tabulated_glm)]
      }
      # write table
      writeData(wb, as.character(i), table_to_write)
    }
  }
  saveWorkbook(wb, filename, overwrite = TRUE)
}
