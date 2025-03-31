#' columnSummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList htmlOutput
mod_DataR_columnSummary_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 8,
        fluidRow(
          column(
            width = 6,
            p('Dataset column summary', style = 'font-size: 20px; margin-top: 18px; margin-bottom: 0px'),
          ),
          column(
            width = 6,
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
        br(),
        DTOutput(ns('column_summary'))
      ),
      column(
        width = 4,
        div(htmlOutput(ns('feature')), style = 'margin-bottom:39px'),
        DTOutput(ns('feature_summary'))
        )
    )
  )
}
    
#' columnSummary Server Functions
#'
#' @noRd 
mod_DataR_columnSummary_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    selected_row <- reactiveVal(NULL)
    selected_feature <- reactiveVal(NULL)
    column_summary <- reactiveVal(NULL)
    output$column_summary <- renderDT({
      dt_update()
      input$columns_to_display
      col_sum <- get_column_summary(d(), sample = FALSE, columns_to_display = input$columns_to_display)
      column_summary(col_sum)
      format_column_summary_DT(col_sum, isolate(selected_row()))
      })
    observeEvent(input$column_summary_rows_selected, {
      selected_row(input$column_summary_rows_selected)
      selected_feature(column_summary()[[1]][selected_row()])
    })
    observeEvent(c(selected_feature(), dt_update()), {
      output$feature <- renderUI({
        p(selected_feature(), style = 'font-size: 20px; margin-top: 18px; margin-bottom:-15px')
      })
      output$feature_summary <- renderDT({
        if(!is.null(selected_feature())){
          format_feature_summary_DT(
            get_feature_summary(d(), selected_feature())
          )
        }
      })
    })
  })
}

get_column_summary <- function(d, sample, columns_to_display){
  if(is.null(d)){
    result <- data.table(class = 'select dataset from top right',
                         type = '',
                         mean = '',
                         max = '',
                         min = '',
                         countNAs = '')
  } else {
    names_col <- names(d)
    if('total_filter' %in% names_col){
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
    result <- d_filter[, sapply(.SD,
                         function(x) c(class = class(x)[1],
                                       type = typeof(x)[1],
                                       mean = mean2(x),
                                       min = min2(x),
                                       max = max2(x),
                                       NAs = countNAs(x)
                         )
    )
    ]
    result <- rbind(name = names(d_filter), result)
    result <- as.data.table(t(result))
  }
  result
}
mean2 <- function(x){
  if(class(x)[1] %in% c('numeric','logical','integer')){
    as.character(signif(mean(x, na.rm = TRUE), 6))
  } else if (class(x)[1] %in% c('IDate','POSIXct','Date')) {
    as.character(as.Date(mean(x, na.rm = TRUE)))
  } else {
    NA
  }
}
min2 <- function(x){
  if(class(x)[1] %in% c('numeric','logical','integer')){
    as.character(signif(min(x, na.rm = TRUE), 6))
  } else if (class(x)[1] %in% c('IDate','POSIXct','Date')) {
    as.character(as.Date(min(x, na.rm = TRUE)))
  } else {
    NA
  }
}
max2 <- function(x){
  if(class(x)[1] %in% c('numeric','logical','integer')){
    as.character(signif(max(x, na.rm = TRUE), 6))
  } else if (class(x)[1] %in% c('IDate','POSIXct','Date')) {
    as.character(as.Date(max(x, na.rm = TRUE)))
  } else {
    NA
  }
}
countNAs <- function(x){
  sum(is.na(x))
}
get_feature_summary <- function(dt, col) {
  if (is.null(dt) || is.null(col) || !col %in% names(dt)) return(NULL)
  x <- dt[total_filter == 1L, get(col)]
  if (is.null(x)) return(NULL)
  if (inherits(x, c('numeric', 'logical', 'integer', 'POSIXct', 'Date', 'IDate'))) {
    type <- if (inherits(x, c('Date', 'POSIXct'))) {
      x <- as.IDate(x)
      1
    } else {
      8
    }
    summary <- data.table(
      metrics = c('Min', '1st percentile', '5th percentile', '25th percentile',
                  'Median', 'Mean', '75th percentile', '95th percentile',
                  '99th percentile', 'Max', 'Standard deviation'),
      value = c(
        min(x, na.rm = TRUE),
        stats::quantile(x, prob = 0.01, na.rm = TRUE, type = type),
        stats::quantile(x, prob = 0.05, na.rm = TRUE, type = type),
        stats::quantile(x, prob = 0.25, na.rm = TRUE, type = type),
        stats::quantile(x, prob = 0.50, na.rm = TRUE, type = type),
        mean(x, na.rm = TRUE),
        stats::quantile(x, prob = 0.75, na.rm = TRUE, type = type),
        stats::quantile(x, prob = 0.95, na.rm = TRUE, type = type),
        stats::quantile(x, prob = 0.99, na.rm = TRUE, type = type),
        max(x, na.rm = TRUE),
        stats::sd(x, na.rm = TRUE)
      )
    )
    if (inherits(x, c('integer', 'numeric', 'logical'))) {
      summary[, value := signif(value, 6)]
    }
  } else {
    summary <- dt[total_filter == 1L, .N, by = col][order(-N)]
    setnames(summary, c(col, 'count'))
    if (nrow(summary) > 10000) {
      other_total <- summary[10001:.N, sum(count)]
      summary <- summary[1:10000]
      summary <- rbindlist(list(data.table(Level = 'Levels outside top 10k', count = other_total), summary))
    }
  }
  if (inherits(x, c('IDate', 'POSIXct', 'Date'))) {
    summary[, value := c(as.character(as.Date(summary$value[-.N], origin = '1970-01-01')), as.character(summary$value[.N]))]
  }
  return(summary)
}

#' @importFrom DT datatable formatStyle formatSignif
format_column_summary_DT <- function(d, selected_row){
  if(!is.null(selected_row)){
    if(selected_row>nrow(d)){
      selected_row <- 1
    }
  }
  pg_length <- min(1000, nrow(d))
  dt <- datatable(d,
                  rownames= FALSE,
                  selection=list(mode="single", selected=selected_row),
                  extensions = 'Buttons',
                  options = list(pageLength = pg_length,
                                 dom = 'Bfrtip',
                                 scrollX = T,
                                 scrollY = 'calc(100vh - 330px)',
                                 searchHighlight=TRUE
                  )
  ) |>
    formatStyle(1:ncol(d), lineHeight='0%', fontSize = '14px') |>
    formatStyle(c('mean','min','max'))
}

#' @importFrom DT datatable formatStyle
format_feature_summary_DT <- function(d){
  DT <- d |>
    datatable(
    rownames= FALSE,
    extensions = 'Buttons',
    selection = list(mode="single", target="row"),
    options = list(
      pageLength = min(100, nrow(d)),
      dom = 'Brti',
      scrollX = T,
      scrollY = 'calc(100vh - 330px)',
      searchHighlight=TRUE
      )
    ) |>
    formatStyle(1:ncol(d), lineHeight='0%', fontSize = '14px')
}