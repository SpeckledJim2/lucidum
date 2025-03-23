#' histogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
mod_histogram_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        textInput(
          ns('num_bins'),
          label = 'Histogram bins (max 10,000)',
          width = '100%',
          placeholder = 'auto'
        ),
        DTOutput(ns('summary_table')),
        #withSpinner(DT::DTOutput(ns('summary_table'))),
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 3,
            radioGroupButtons(
              label = 'Distribution',
              inputId = ns('inc_cum'),
              choices = c('Incremental'=0,'Cumulative'=1),
              individual = FALSE,
              size = 'xs',
              selected = 0
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              label = 'y axis type',
              inputId = ns('normalise'),
              choices = c('Sum'='','Probability'='probability'),
              individual = FALSE,
              size = 'xs',
              selected = ''
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              label = 'log scale',
              inputId = ns('log_scale'),
              choices = c('-','X axis','Y axis','Both'),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          ),
          column(
            width = 3,
            align = 'right',
            radioGroupButtons(
              label = 'Sample data for chart',
              inputId = ns('use_sample'),
              choices = c('Use 100k','Use all'),
              individual = FALSE,
              size = 'xs',
              selected = 'Use 100k'
            )
          )
        ),
        br(),
        plotlyOutput(ns('histogram'), height = 'calc(85vh - 100px)')
      )
    )
  )
}
    
#' histogram Server Functions
#'
#' @noRd 
#' 
#' 
#' @importFrom plotly renderPlotly layout
mod_histogram_server <- function(id, d, dt_update, response, weight, kpi_spec, filters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$summary_table <- DT::renderDataTable({
      dt_update()
      histogram_DT(histogram_table(d(), response(), weight(), kpi_spec()))
    })
    output$histogram <- renderPlotly({
      dt_update()
      histogram_chart(d(), response(), weight(), input$num_bins, input$log_scale, input$normalise, input$inc_cum, input$use_sample, filters())
    })
  })
}

#' @importFrom stats sd
histogram_table <- function(d, response, weight, kpi_spec){
  Value_temp <- NULL
  Value <- NULL
  Statistic <- NULL
  if(!is.null(response) & !is.null(d)){
    if(response %in% names(d) & weight %in% c(names(d),'N')){
      if(weight=='N'){
        if('total_filter' %in% names(d)){
          value <- d[which(total_filter==1), ..response][[1]]
        } else {
          value <- d[, ..response][[1]]
        }
        weighted_mean <- mean(value, na.rm = TRUE)
      } else {
        if('total_filter' %in% names(d)){
          num <- d[which(total_filter==1), ..response][[1]]
          den <- d[which(total_filter==1), ..weight][[1]]
        } else {
          num <- d[, ..response][[1]]
          den <- d[, ..weight][[1]]
        }
        weighted_mean <- sum(num, na.rm = TRUE)/sum(den, na.rm = TRUE)
        value <- num/den
      }
      # empty data frame to hold the data summary
      row_names <- c('Numeric count', 'NA count', 'Zero count', 'Mean',
                     'Weighted mean', 'Std deviation', 'Minimum', '0.1st percentile', 
                     '0.5th percentile','1st percentile', '5th percentile',
                     '10th percentile', '25th percentile', 'Median', '75th percentile',
                     '90th percentile', '95th percentile', '99th percentile',
                     '99.5th percentile', '99.9th percentile', 'Maximum'
                     )
      x <- data.table(Statistic=row_names, Value = rep(0,length(row_names)))
      # statistics
      x[1, Value := sum(ifelse(!is.na(value),1,0))]
      x[2, Value := sum(ifelse(is.na(value),1,0))]
      x[3, Value := sum(ifelse(value==0,1,0), na.rm = TRUE)]
      x[4, Value := mean(value, na.rm = TRUE)]
      x[5, Value := weighted_mean]
      x[6, Value := sd(value, na.rm = TRUE)]
      x[7, Value := min(value, na.rm = TRUE)]
      x[8, Value := quantile(value, prob = 0.001, na.rm = TRUE)[[1]]]
      x[9, Value := quantile(value, prob = 0.005, na.rm = TRUE)[[1]]]
      x[10, Value := quantile(value, prob = 0.01, na.rm = TRUE)[[1]]]
      x[11, Value := quantile(value, prob = 0.05, na.rm = TRUE)[[1]]]
      x[12, Value := quantile(value, prob = 0.1, na.rm = TRUE)[[1]]]
      x[13, Value := quantile(value, prob = 0.25, na.rm = TRUE)[[1]]]
      x[14, Value := quantile(value, prob = 0.5, na.rm = TRUE)[[1]]]
      x[15, Value := quantile(value, prob = 0.75, na.rm = TRUE)[[1]]]
      x[16, Value := quantile(value, prob = 0.90, na.rm = TRUE)[[1]]]
      x[17, Value := quantile(value, prob = 0.95, na.rm = TRUE)[[1]]]
      x[18, Value := quantile(value, prob = 0.99, na.rm = TRUE)[[1]]]
      x[19, Value := quantile(value, prob = 0.995, na.rm = TRUE)[[1]]]
      x[20, Value := quantile(value, prob = 0.999, na.rm = TRUE)[[1]]]
      x[21, Value := max(value, na.rm = TRUE)]
      x[, Value_formatted := '']
      x[1:3, Value_formatted := as.character(round(Value,0))]
      x[4:21, Value_formatted := apply_kpi_format(Value, response, weight, kpi_spec)]
      x[, Value := NULL]
      setnames(x, old = 'Value_formatted', new = 'Value')
      x
    }
  }
}
histogram_DT <- function(d){
  if(!is.null(d)){
    d |>
      datatable(
        extensions = 'Buttons',
        editable = TRUE,
        rownames= FALSE,
        options = list(
          pageLength = nrow(d),
          dom = 'Brt',
          scrollX = T,
          searchHighlight=TRUE,
          columnDefs=list(
            list(width="100px",targets="_all"),
            list(className = 'dt-right', targets = 1)
          ),
          buttons =
            list('copy', list(
              extend = 'collection',
              buttons = list(
                list(extend='csv',filename = ''),
                list(extend='excel',filename = ''),
                list(extend='pdf',filename= '')
              ),
              text = 'Download')
            )
        )
      ) |>
      formatStyle(1:2, fontSize = '100%', lineHeight='20%')
  }
}

#' @importFrom plotly plot_ly layout
histogram_chart <- function(d, response, weight, num_bins, log_scale, normalise, inc_cum, use_sample, filters){
  if(!is.null(response)){
    # only proceed if d and the filters have the same length
    if(response %in% names(d) &
       weight %in% c(names(d), 'N')
    ){
      if(weight=='N'){
        values <- d[total_filter==1,..response][[1]]
      } else {
        values <- d[total_filter==1,..response][[1]]/d[total_filter==1,..weight][[1]]
      }
      # sample down if selected
      if(use_sample=='Use 100k'){
        num_rows <- length(values)
        if(num_rows>100000){
          values <- values[sample(1:num_rows, 100000, replace = FALSE)]
        }
      }
      # divide by weight
      if(weight=='N'){
        title <- response
      } else {
        title <- paste(response, '/', weight)
      }
      # filter text
      train_test_filter <- filters$train_test_filter
      user_filter <- filters$user_filter
      if(train_test_filter=='All'){train_test_filter <- ''}
      if(train_test_filter=='Train'){train_test_filter <- '  training data  '}
      if(train_test_filter=='Test'){train_test_filter <- '  test data  '}
      # make title bold
      title <- paste0(boldify(title), train_test_filter, ' ', user_filter)
      # set the number of bins, if NULL then plotly will automatically pick the number of bins
      num_bins <- as.numeric(num_bins)
      num_bins <- min(10000,num_bins)
      # parse the log scale choice
      x_scale <- ifelse(log_scale %in% c('X axis','Both'), 'log','')
      y_scale <- ifelse(log_scale %in% c('Y axis','Both'), 'log','')
      # render the plot
      p <- plot_ly(x = ~values,
                   type = "histogram",
                   nbinsx = num_bins,
                   histnorm = normalise,
                   cumulative = list(enabled=as.logical(as.numeric(inc_cum)))) |>
        layout(hovermode = 'x') |>
        layout(margin = list(l = 50, r = 50, b = 10, t = 70, pad = 4)) |>
        layout(xaxis = list(title = '', type = x_scale)) |>
        layout(yaxis = list(title = NULL, type = y_scale)) |>
        layout(title = list(text = title, x = 0.03, y = 0.97, font = list(size = 16))) |>
        layout(font=list(family = 'Helvetica Neue')) |>
        layout(bargap=0.1)
    }
  }
}
