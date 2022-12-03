#' ChartaR_line_and_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ChartaR_line_and_bar_ui <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        selectInput_ui(id = ns('x_axis_feature'), label = 'x-axis feature', height_divisor = 50, height_adj = 2, multiple = FALSE),
        selectInput_ui(id = ns('add_columns'), label = 'Additional y-axis columns', height_divisor = 55, height_adj = 3, multiple = TRUE)
      ),
      column(
        width = 9,
        fluidRow(
          column(3,
                 radioGroupButtons(
                   inputId = "ChartaR_1W_sort_order",
                   label = "x-axis sort order",
                   choices = c('A-Z', 'Wt', 'Act','Add','PD'),
                   individual = FALSE,
                   size = 'xs',
                   selected = 'A-Z')
          ),
          column(3,
                 radioGroupButtons(
                   inputId = ns('group_low_exposure'),
                   label = "Group low weights",
                   choices = c(0,5,10,20,50,'1%'),
                   individual = FALSE,
                   size = 'xs',
                   selected = 0)
          ),
          column(2,
                 align = 'left',
                 radioGroupButtons(
                   inputId = "ChartaR_A_vs_E_show_labels",
                   label = "Labels",
                   choices = c('-', 'Weight','All'),
                   individual = FALSE,
                   size = 'xs',
                   selected = '-'
                 )
          ),
          column(4,
                 align = 'right',
                 mod_bandingChooser_ui(ns('x_banding'))
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioGroupButtons(
              inputId = "ChartaR_1W_error_bars",
              label = "Error bars",
              choices = c('-'='-', '90%'=0.9, '95%'=0.95, '99%'=0.99),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = "ChartaR_A_vs_E_show_partial_dependencies",
              label = "Partial dependencies",
              choices = c('-','GLM','GBM','Both'),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = "ChartaR_show_response",
              label = "Response",
              choices = c('Hide','Show'),
              individual = FALSE,
              size = 'xs',
              selected = 'Show')
          ),
          column(
            width = 3,
            align = 'right',
            radioGroupButtons(
              inputId = "ChartaR_1W_y_transform",
              label = "Response transform",
              choices = c('-', 'Log','Exp','Logit','0','1'),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          )
        ),
        tabsetPanel(
          id = 'ChartaR_one_way_tabs',
          type = 'tabs',
          tabPanel('Chart',
                   htmlOutput(ns('test')),
                   DTOutput(ns('one_way_table')),
          ),
          tabPanel('Table',
                   h2('Table'),
                   br()
          )
        )
      )
    )
  )
}
    
#' ChartaR_line_and_bar Server Functions
#'
#' @noRd 
mod_ChartaR_line_and_bar_server <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec, BoostaR_models, BoostaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    data_summary <- reactiveVal(NULL)
    initial_banding <- reactiveVal(NULL)
    banding <- reactiveVal(NULL)
    x_col <- selectInput_server(id = 'x_axis_feature', d, dt_update, feature_spec, BoostaR_models, BoostaR_idx, FALSE)
    add_cols <- NULL
    observeEvent(x_col(), {
      banding_guess <- banding_guesser_numeric_date(d(), x_col())
      initial_banding(banding_guess)
      banding(banding_guess)
    })
    banding_new <- mod_bandingChooser_server('x_banding', d, x_col, initial_banding)
    observeEvent(banding_new(), {
      banding(banding_new())
    })
    observeEvent(c(dt_update(), response(), weight(), x_col(), banding(), kpi_spec(), input$group_low_exposure), {
      data_summary(line_and_bar_summary(d(), response(), weight(), x_col(), banding(), input$group_low_exposure, kpi_spec()))
    })
    observeEvent(data_summary(), {
      output$one_way_table <- DT::renderDT({format_table_DT(data_summary())})
    })



  })
}
    
## To be copied in the UI
# mod_ChartaR_line_and_bar_ui("ChartaR_line_and_bar_1")
    
## To be copied in the server
# mod_ChartaR_line_and_bar_server("ChartaR_line_and_bar_1")

line_and_bar_summary <- function(d, response, weight, group_by_col, banding, group_low_exposure, kpi_spec){
  if(!is.null(d) & !is.null(response) & !is.null(weight) & !is.null(group_by_col)){
    if(response!='' & weight !=''){
      d_cols <- names(d)
      if(response %in% d_cols & weight %in% c('N',d_cols)){
        g <- d[[group_by_col]]
        rows_idx <- which(d[['total_filter']]==1)
        # band the variable if numeric or date
        if(is.numeric(g)){
          
          # band the numerical variable for plotting
          banding <- as.numeric(banding)
          banded <- floor(g/banding) * banding
          # if percentage hide_low_exposure selected, group the low exposure rows
          if (group_low_exposure=='1%'){
            q_low <- quantile(g[rows_idx], prob = 0.01, na.rm = TRUE)[[1]]
            q_high <- quantile(g[rows_idx], prob = 0.99, na.rm = TRUE)[[1]]
            q_low_banded <- floor(q_low/banding) * banding
            q_high_banded <- (1+floor(q_high/banding)) * banding
            banded <- pmax(q_low_banded, pmin(q_high_banded, banded))
          }

          banded_col <- banded[rows_idx]
          new_colname <- paste0(group_by_col, '_banded')
        } else if (inherits(g,'Date')){
          if(banding=='Day'){
            # day
            banded <- g
            new_colname <- paste0(group_by_col, '_day')
          } else if (banding=='Week'){
            # week
            banded <- 100*year(g) + week(g)
            new_colname <- paste0(group_by_col, '_week')
          } else if (banding=='Mnth'){
            # month
            banded <- 100*year(g) + month(g)
            new_colname <- paste0(group_by_col, '_month')
          } else if (banding=='Qtr'){
            # quarter
            banded <- 100*year(g) + floor((month(g)-1)/3)+1
            new_colname <- paste0(group_by_col, '_quarter')
          } else if (banding=='Year'){
            # year
            banded <- year(g)
            new_colname <- paste0(group_by_col, '_year')
          }
          banded_col <- banded[rows_idx]
        } else {
          banded_col <- group_by_col
          new_colname <- group_by_col
        }
        # assemble the columns we need in the summary
        if(weight %in% c('N','no weights')){
          cols_to_summarise <- c(response)
        } else {
          cols_to_summarise <- c(weight, response)
        }
        # summarise
        if(length(rows_idx)==nrow(d)){
          d_summary <- d[, c(count = .N, lapply(.SD, sum, na.rm = TRUE)), banded_col, .SDcols = cols_to_summarise]
        } else {
          d_summary <- d[rows_idx, c(count = .N, lapply(.SD, sum, na.rm = TRUE)), banded_col, .SDcols = cols_to_summarise]
        }
        # apply denominator
        # divide by weight if specified
        if(weight == 'N'){
          first_col <- 3
          # divide all summary columns (3rd onwards) by the weight column (2nd)
          d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[2]]
        } else if (weight != 'no weights'){
          first_col <- 4
          # divide all summary columns (4rd onwards) by the weight column (3rd)
          d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[3]]
        }

        # change first column name
        first_col <- names(d_summary)[[1]]
        setnames(d_summary, old = first_col, new = new_colname)
        setorderv(d_summary, new_colname)
      }
    }
  }
}

banding_guesser_numeric_date <- function(d, col){
  type <- 'NULL'
  # get type of column
  if(!is.null(col) & !is.null(d)){
    if(col %in% names(d)){
      if(inherits(d[[col]],c('factor','character'))){
        type <- 'character'
      } else if (inherits(d[[col]],c('numeric','integer'))){
        type <- 'numeric'
      } else if (inherits(d[[col]],'Date')){
        type <- 'date'
      } else if (col=='none'){
        type <- 'NULL'
      }
    }
  }
  # calculate what banding should be chosen initially
  # and what should be shown on the widget
  if(type=='numeric'){
    b <- banding_guesser(d[[col]])
  } else if(type=='date'){
    b <- banding_guesser_date(d[[col]])
  } else {
    b <- 0
  }
}

format_table_DT <- function(dt){
  # format table prior to rendering with DT
  dt[,3] <- round(dt[,3],2)
  datatable(
    dt,
    rownames= FALSE,
    options = list(pageLength = min(1000, nrow(dt)),
                   scrollX = T,
                   dom = 'tp',
                   scrollY = 'calc(90vh - 300px)'
    )
  ) |>
    formatStyle(1:ncol(dt), lineHeight='0%', fontSize = '12px')
}