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
              inputId = ns('hist_normalise'),
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
        shinycssloaders::withSpinner(plotlyOutput(ns('histogram'))),
        # tags$head(tags$script('
        #                                     // Define function to set height of "histogram"
        #                                     setHeightChartaR_histogram = function() {
        #                                       var window_height = $(window).height();
        #                                       var header_height = $(".main-header").height();
        #                                       var boxHeight = (window_height - header_height) - 220;
        #                                       $("#histogram").height(boxHeight);
        #                                     };
        #                                     // Set input$box_height when the connection is established
        #                                     $(document).on("shiny:connected", function(event) {
        #                                       setHeightChartaR_histogram();
        #                                     });
        #                                     // Refresh the box height on every window resize event
        #                                     $(window).on("resize", function(){
        #                                       setHeightChartaR_histogram();
        #                                     });
        #                                   '))
      )
    )
  )
}
    
#' histogram Server Functions
#'
#' @noRd 
mod_histogram_server <- function(id, d, dt_update, response, weight, kpi_spec){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$summary_table <- DT::renderDataTable({
      dt_update()
      histogram_DT(histogram_table(d(), response(), weight(), kpi_spec()))
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
        weighted_mean <- num/den
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
      x[, Value := as.character(format(Value, big.mark = ',', digits = 2))]
      x
    }
  }
}
histogram_DT <- function(d){
  d |>
    datatable(extensions = 'Buttons',
                  editable = TRUE,
                  rownames= FALSE,
                  options = list(pageLength = nrow(d),
                                 dom = 'Brt',
                                 scrollX = T,
                                 searchHighlight=TRUE,
                                 columnDefs=list(list(width="100px",targets="_all"),
                                                 list(className = 'dt-right', targets = 1)),
                                 buttons =
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = list(list(extend='csv',filename = ''),
                                                    list(extend='excel',filename = ''),
                                                    list(extend='pdf',filename= '')),
                                     text = 'Download')
                                   )
                  )) |>
    formatStyle(1:2, fontSize = '85%', lineHeight='20%')
}

