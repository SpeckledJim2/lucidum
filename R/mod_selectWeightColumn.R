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
mod_selectWeightColumn_ui <- function(id, label = 'label', ...){
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
mod_selectWeightColumn_server <- function(
    id,
    d,
    dt_update,
    numerical_cols,
    subset,
    special_options,
    kpi,
    kpi_spec,
    new_selection
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$col, {
      new_selection(input$col)
    })
    observeEvent(c(d(), dt_update()), {
      if(!is.null(d())){
        if(nrow(d())>0){
          choices <- getColumnChoices(d(), numerical_cols, subset, special_options, 'Weights')
          if(is.null(new_selection())){
            selected <- input$col
          } else if(new_selection() %in% unlist(choices)){
            # use the new selection
            selected <- new_selection()
          } else if(input$col %not_in% unlist(choices)){
            selected <- unlist(choices)[[1]]
          } else {
            selected <- input$col
          }
          updateSelectInput(
            inputId = 'col',
            choices = choices,
            selected = selected
          )
        }
      }
    })
    observeEvent(kpi(), {
      kpi_components <- kpi_numerator_denominator(kpi(), kpi_spec())
      selected <- kpi_components$denominator
      updateSelectInput(inputId = 'col', selected = selected)
    })
    observeEvent(c(input$col, dt_update()), {
      if(!is.null(d())){
        updateSelectInput(inputId = 'col', label = paste0('Weight ', weight_text(d(), input$col)))
      }
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

weight_text <- function(d, weight){
  if(nrow(d)>0 & weight!=''){
    if(weight=='N'){
      if('total_filter' %in% names(d)){
        val <- sum(d[['total_filter']], na.rm = TRUE)
      } else {
        val <- nrow(d)
      }
    } else {
      if('total_filter' %in% names(d)){
        val <- d[which(total_filter==1), sum(.SD, na.rm = TRUE), .SDcols = weight]
      } else {
        val <- d[, sum(.SD, na.rm = TRUE), .SDcols = weight]
      }
    }
    paste0('= ', format(val, big.mark = ',', scientific = FALSE))
  }
}