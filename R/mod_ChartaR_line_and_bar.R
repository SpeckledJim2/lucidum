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
                   inputId = "ChartaR_1W_group_low_exposure",
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
    observeEvent(c(dt_update(), response(), weight(), x_col(), banding(), kpi_spec()), {
      data_summary(line_and_bar_summary(d(), response(), weight(), x_col(), banding(), kpi_spec()))
    })
    observeEvent(data_summary(), {
      output$one_way_table <- DT::renderDT({
        DT::datatable(
          data_summary(),
          rownames= FALSE,
          options = list(pageLength = nrow(data_summary()),
                         scrollX = T,
                         scrollY = 'calc(60vh - 100px)'
                         )
          ) |>
          DT::formatStyle(1:ncol(data_summary()), lineHeight='0%', fontSize = '12px')
        })
    })



  })
}
    
## To be copied in the UI
# mod_ChartaR_line_and_bar_ui("ChartaR_line_and_bar_1")
    
## To be copied in the server
# mod_ChartaR_line_and_bar_server("ChartaR_line_and_bar_1")

line_and_bar_summary <- function(d, response, weight, group_by_col, banding, kpi_spec){
  if(!is.null(d) & !is.null(response) & !is.null(weight) & !is.null(group_by_col)){
    if(response!='' & weight !=''){
      d_cols <- names(d)
      if(response %in% d_cols & weight %in% c('N',d_cols)){
        g <- d[[group_by_col]]
        rows_idx <- which(d[['total_filter']]==1)
        numeric_group_by_col <- FALSE
        # band the variable if numeric or date
        if(is.numeric(g)){
          # band the numerical variable for plotting
          banding <- as.numeric(banding)
          numeric_group_by_col <- TRUE
          new_colname <- paste0(group_by_col, '_banded')
          banded <- floor(g/banding) * banding
          group_by_col <- banded[rows_idx]
        } else if (inherits(g,'Date')){
          
        } else {
          
        }
        # assemble the columns we need in the summary
        if(weight %in% c('N','no weights')){
          cols_to_summarise <- c(response)
        } else {
          cols_to_summarise <- c(weight, response)
        }
        # summarise
        if(length(rows_idx)==nrow(d)){
          d_summary <- d[, c(count = .N, lapply(.SD, sum, na.rm = TRUE)), group_by_col, .SDcols = cols_to_summarise]
        } else {
          d_summary <- d[rows_idx, c(count = .N,lapply(.SD, sum, na.rm = TRUE)), group_by_col, .SDcols = cols_to_summarise]
        }
        d_summary
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

