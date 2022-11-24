#' selectColumn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param d data.frame from which you want to select a column.
#' @param numerical_cols Boolean (default = FALSE), set to TRUE to only show numerical columns.
#' @param subset Character (default = NULL), only show columns also contained in subset.
#' @param special_options Character (default = NULL), always show these options.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selectResponseColumn_ui <- function(id, label = 'label', ...){
  ns <- NS(id)
  tagList(
    tags$style(type='text/css',
               ".selectize-input {font-size: 13px; line-height: 16px; min-height: 20px}
                .selectize-dropdown-content {max-height: 400px; }
                .selectize-dropdown {font-size: 13px; line-height: 16px}"),
    div(
      style='margin-bottom:-15px;',
      selectInput(
        inputId = ns('col'),
        label = label,
        choices = NULL,
        ...
      )
    )
  )
}
    
#' selectColumn Server Functions
#'
#' @noRd 
mod_selectResponseColumn_server <- function(id, d, dt_update, numerical_cols, subset, special_options, kpi, kpi_spec, weight){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(dt_update(), {
      if(nrow(d())>0){
        current_selection <- input$col
        choices <- getColumnChoices(d(), numerical_cols, subset, special_options)
        selected <- input$col
        if(selected %not_in% choices){
          selected <- choices[[1]]
        }
        updateSelectInput(
          inputId = 'col',
          choices = choices,
          selected = selected
        )
      }
    })
    observeEvent(kpi(), {
      kpi_components <- kpi_numerator_denominator(kpi_spec()[as.numeric(kpi())][[1]], kpi_spec())
      selected <- kpi_components$numerator
      updateSelectInput(inputId = 'col', selected = selected)
    })
    observeEvent(c(input$col, weight(), dt_update()), ignoreInit = TRUE, {
      if('user_filter' %in% names(d())){
        if(weight()=='N'){
          num <- d()[which(user_filter==1), sum(.SD), .SDcols=input$col]
          den <- d()[, sum(user_filter)]
          val <- num/den
        } else {
          num <- d()[which(user_filter==1), sum(.SD), .SDcols=input$col]
          den <- d()[which(user_filter==1), sum(.SD), .SDcols=weight()]
          val <- num/den
        }
      } else {
        val <- d()[, mean(.SD), .SDcols=input$col]
      }
      updateSelectInput(inputId = 'col', label = paste0('Response ', response_text(d(), input$col, weight())))
    })
    return(reactive({input$col}))
  })
}

kpi_numerator_denominator <- function(kpi, kpi_spec){
  kpi_name <- NULL
  kpi_numerator <- NULL
  kpi_denominator <- NULL
  if(is.null(kpi)){
    components <- NULL
  } else {
    if(nrow(kpi_spec)>0){
      numerator <- kpi_spec[kpi_name==kpi, kpi_numerator]
      denominator <- kpi_spec[kpi_name==kpi, kpi_denominator]
      if(length(numerator)>1){ # multiple matches
        components <- NULL
      } else {
        components <- list(numerator=numerator, denominator=denominator)
      }
    } else {
      components <- NULL
    }
  }
  return(components)
}

response_text <- function(d, response, weight){
  if(nrow(d)>0 & weight!='' & response !=''){
    # get numerator
    if('user_filter' %in% names(d)){
      num <- d[which(user_filter==1), sum(.SD, na.rm = TRUE), .SDcols = response]
    } else {
      num <- d[, sum(.SD, na.rm = TRUE), .SDcols = response]
    }
    # get denominator
    if(weight=='N'){
      if('user_filter' %in% names(d)){
        den <- sum(d[['user_filter']])
      } else {
        den <- nrow(d)
      }
    } else {
      if('user_filter' %in% names(d)){
        den <- d[which(user_filter==1), sum(.SD, na.rm = TRUE), .SDcols = weight]
      } else {
        den <- d[, sum(.SD, na.rm = TRUE), .SDcols = weight]
      }
    }
    paste0('= ', format(num/den, big.mark = ',', digits = 4))
  }
}