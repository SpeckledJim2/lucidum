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
mod_selectResponseColumn_server <- function(
    id,
    d,
    dt_update,
    numerical_cols,
    subset,
    special_options,
    kpi,
    kpi_spec,
    weight,
    startup,
    new_selection
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$col, {
      #new_selection(input$col)
    })
    observeEvent(c(d(), dt_update()), {
      if(!is.null(d())){
        if(nrow(d())>0){
          choices <- getColumnChoices(d(), numerical_cols)
          updateSelectInput(
            inputId = 'col',
            choices = choices,
            selected = input$col
          )
        }
      }
    })
    observeEvent(new_selection(), {
      if(!is.null(d())){
        if(nrow(d())>0){
          choices <- getColumnChoices(d(), numerical_cols)
          if(is.null(new_selection())){
            selected <- input$col
          } else if(new_selection() %in% unlist(choices)){
            # use the new_selection
            selected <- new_selection()
          } else if(input$col %not_in% unlist(choices)){
            selected <- unlist(choices)[[1]]
          } else {
            selected <- input$col
          }
          updateSelectInput(
            inputId = 'col',
            selected = selected
          )
        }
      }
    })
    observeEvent(kpi(), {
      kpi_components <- kpi_numerator_denominator(kpi(), kpi_spec())
      selected <- kpi_components$numerator
      updateSelectInput(inputId = 'col', selected = selected)
    })
    observeEvent(c(input$col, weight(), dt_update(), kpi_spec()), ignoreInit = TRUE, {
      if(input$col %in% names(d()) &
         weight() %in% c('N', names(d()))){
        if('total_filter' %in% names(d())){
          if(weight()=='N'){
            num <- d()[which(total_filter==1L), sum(.SD), .SDcols=input$col]
            den <- d()[, sum(total_filter)]
            val <- num/den
          } else {
            num <- d()[which(total_filter==1L), sum(.SD), .SDcols=input$col]
            den <- d()[which(total_filter==1L), sum(.SD), .SDcols=weight()]
            val <- num/den
          }
        } else {
          val <- d()[, mean(.SD), .SDcols=input$col]
        }
        response_label <- 0
        updateSelectInput(inputId = 'col', label = paste0('Response ', response_text(d(), input$col, weight(), kpi_spec())))
      }
    })
    observeEvent(startup(), once = TRUE, {
      if(!is.null(startup())){
        if(startup() %in% numerical_cols(d())){
          updateSelectInput(inputId = 'col', selected = startup())
        } else {
          # set to zero from app_server.R
          # means nothing selected
          # pick the first numerical column
          first_numerical_col <- numerical_cols(d())[1]
          updateSelectInput(inputId = 'col', selected = first_numerical_col)
        }
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
response_text <- function(d, response, weight, kpi_spec){
  if(nrow(d)>0 & weight!='' & response !=''){
    # get numerator
    if('total_filter' %in% names(d)){
      num <- d[which(total_filter==1), sum(.SD, na.rm = TRUE), .SDcols = response]
    } else {
      num <- d[, sum(.SD, na.rm = TRUE), .SDcols = response]
    }
    # get denominator
    if(weight=='N'){
      if('total_filter' %in% names(d)){
        den <- sum(d[['total_filter']], na.rm = TRUE)
      } else {
        den <- nrow(d)
      }
    } else {
      if('total_filter' %in% names(d)){
        den <- d[which(total_filter==1), sum(.SD, na.rm = TRUE), .SDcols = weight]
      } else {
        den <- d[, sum(.SD, na.rm = TRUE), .SDcols = weight]
      }
    }
    response_label <- apply_kpi_format(num/den, response, weight, kpi_spec)
    paste0('= ', response_label)
  }
}
getColumnChoices <- function(d, numerical_cols = FALSE, subset = NULL, special_options = NULL, special_options_group_name = NULL){
  cols <- NULL
  if(!is.null(d)){
    if(ncol(d)>0){
      if(numerical_cols){
        cols <- names(d)[which(sapply(d,is.numeric))]
      } else {
        cols <- names(d)
      }
      if(!is.null(subset)){
        cols <- setdiff(subset, cols)
      }
      non_lucidum_cols <- remove_lucidum_cols(cols)
      lucidum_cols <- setdiff(cols, non_lucidum_cols)
      non_lucidum_cols <- sort(non_lucidum_cols)
      lucidum_cols <- sort(lucidum_cols)
      # fix if length 1 to ensure selectInput displays correctly
      special_options <- list_if_length_one(special_options)
      lucidum_cols <- list_if_length_one(lucidum_cols)
      non_lucidum_cols <- list_if_length_one(non_lucidum_cols)
      column_choices <- list()
      group_name <- paste0('Dataset columns (', length(non_lucidum_cols),')')
      if(!is.null(special_options)){
        column_choices[[special_options_group_name]] <- special_options
        }
      column_choices[[group_name]] <- non_lucidum_cols
      if(!is.null(lucidum_cols)){
        lucidum_group_name <- paste0('Lucidum columns (', length(lucidum_cols),')')
        column_choices[[lucidum_group_name]] <- lucidum_cols
        }
    }
  }
  return(column_choices)
}