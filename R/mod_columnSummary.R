#' columnSummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList htmlOutput
mod_columnSummary_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 8,
        p('Dataset column summary', style = 'font-size: 20px; margin-top: 18px; margin-bottom: 0px'),
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
mod_columnSummary_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    selected_row <- reactiveVal(NULL)
    output$column_summary <- renderDT({
      dt_update()
      format_column_summary_DT(
        get_column_summary(d(), FALSE),
        isolate(selected_row())
      )
    })
    output$feature <- renderUI({
      if(!is.null(input$column_summary_rows_selected)){
        selected_row(input$column_summary_rows_selected)
        col <- names(d())[input$column_summary_rows_selected]
      } else {
        selected_row(NULL)
        col <- 'select feature'
      }
      p(col, style = 'font-size: 20px; margin-top: 18px; margin-bottom:-15px')
    })
    output$feature_summary <- renderDT({
      if(!is.null(input$column_summary_rows_selected)){
        col <- names(d())[input$column_summary_rows_selected]
        format_feature_summary_DT(
          get_feature_summary(d(), col)
        )
      }
    })
  })
}

get_column_summary <- function(d, sample){
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
get_feature_summary <- function(d, col){
  if(col %in% names(d)){
    x <- d[which(total_filter==1)][[col]]
    if(is.null(x)){
      # do nothing
    } else if(class(x)[1] %in% c('numeric','logical','integer','POSIXct','Date','IDate')){
      metrics <- c('Min',
                   '1st percentile',
                   '5th percentile',
                   '25th percentile',
                   'Median',
                   'Mean',
                   '75th percentile',
                   '95th percentile',
                   '99th percentile',
                   'Max',
                   'Standard deviation'
      )
      summary <- data.table(
        metrics=c('Min',
                  '1st percentile',
                  '5th percentile',
                  '25th percentile',
                  'Median',
                  'Mean',
                  '75th percentile',
                  '95th percentile',
                  '99th percentile',
                  'Max',
                  'Standard deviation'
                  ),
        value=rep(0,11)
        )
      summary[1, value := min(x,na.rm=TRUE)]
      summary[2, value := stats::quantile(x,prob=0.01,na.rm=TRUE, type = 8)]
      summary[3, value := stats::quantile(x,prob=0.05,na.rm=TRUE, type = 8)]
      summary[4, value := stats::quantile(x,prob=0.25,na.rm=TRUE, type = 8)]
      summary[5, value := stats::quantile(x,prob=0.50,na.rm=TRUE, type = 8)]
      summary[6, value := mean(x, na.rm = TRUE)]
      summary[7, value := stats::quantile(x,prob=0.75,na.rm=TRUE, type = 8)]
      summary[8, value := stats::quantile(x,prob=0.95,na.rm=TRUE, type = 8)]
      summary[9, value := stats::quantile(x,prob=0.99,na.rm=TRUE, type = 8)]
      summary[10, value := max(x,na.rm=TRUE)]
      summary[11, value := stats::sd(x,na.rm=TRUE)]
      if(class(x)[1] %in% c('numeric')){
        summary[, value := signif(value, 6)]
      }
    } else {
      frequencies <- sort(table(x, useNA = 'ifany'),decreasing=TRUE)
      summary <- as.data.table(frequencies)
      names(summary) <- c('value','count')
      num_levels <- nrow(summary)
      if(num_levels>10000){
        other_total <- summary[10001:.N, sum(count)]
        summary <- summary[1:10000,]
        summary <- rbind(data.table(value='Levels outside top 10k', count = other_total), summary)
      }
    }
    if(class(x)[1] %in% c('IDate','POSIXct','Date')){
      sd <- as.character(as.numeric(summary[nrow(summary), value]))
      summary[nrow(summary), value := as.numeric(as.Date(value))]
      summary[, value := as.character(as.Date(value))]
      summary[nrow(summary), value := sd]
    }
    summary
  }
}

#' @importFrom DT datatable formatStyle
format_column_summary_DT <- function(d, selected_row){
  pg_length <- min(100, nrow(d))
  dt <- datatable(d,
                  rownames= FALSE,
                  #selection=list(mode="single", target="row"),
                  selection=list(mode="single", selected=selected_row),
                  extensions = 'Buttons',
                  class = 'white-space: nowrap',
                  options = list(pageLength = pg_length,
                                 #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                 dom = 'Bfrtip',
                                 scrollX = T,
                                 scrollY = 'calc(100vh - 330px)',
                                 searchHighlight=TRUE
                  )
  ) |>
    formatStyle(1:ncol(d), lineHeight='0%', fontSize = '12px') |>
    formatStyle(c('mean','min','max'), 'white-space'='nowrap')
}

#' @importFrom DT datatable formatStyle
format_feature_summary_DT <- function(d){
  d |>
    datatable(
    rownames= FALSE,
    extensions = 'Buttons',
    class = 'white-space: nowrap',
    selection = list(mode="single", target="row"),
    options = list(
      pageLength = min(100, nrow(d)),
      dom = 'Brti',
      scrollX = T,
      scrollY = 'calc(100vh - 330px)',
      searchHighlight=TRUE
      )
    ) |>
    formatStyle(1:ncol(d), lineHeight='0%', fontSize = '12px')
}