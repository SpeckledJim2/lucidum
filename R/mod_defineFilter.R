#' defineFilter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_defineFilter_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          radioGroupButtons(
            inputId = ns('train_test_filter'),
            label = 'Filter (0/0)',
            choices = c('All','Train','Test'),
            selected = 'All',
            justified = TRUE,
            size = 's',
            width = '100%'
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 10,
        style = 'margin-top: 0px; margin-right:0px; padding-right: 0px',
        div(
          textInput(inputId = ns('free_filter'), placeholder = 'enter filter...', label = NULL, width = '100%'),
        )
      ),
      column(
        width = 2,
        style = 'margin-left:-30px; margin-top:0px; padding-top: 0px; padding-left:0px;',
        div(
          actionButton(
            style = 'height:34px; border-radius:0px; margin-top: 0px; margin-bottom: 0px; padding-left:0px; padding-right:0px',
            width = '100%',
            inputId = ns('apply_filter'),
            label = NULL,
            icon = icon("filter")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          #style = 'margin-top:0px',
          selectInput(inputId = ns('filter_list'),label = NULL, width = '100%', size = 6, selectize = FALSE, multiple = TRUE, choices = NULL)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        #style = 'margin-top: 0px',
        div(
          radioGroupButtons(
            inputId = ns('filter_operation'),
            label = NULL,
            choices = c('AND','OR','NAND','NOR'),
            individual = FALSE,
            size = 'xs',
            justified = TRUE
          ),
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        style = 'margin-left:18px; margin-top: 2px; font-size: 8px',
          textOutput(ns('message'))
      )
    )
  )
}
    
#' defineFilter Server Functions
#'
#' @noRd 
mod_defineFilter_server <- function(id, d, dt_update, filter_spec){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    user_filter <- reactiveVal(FALSE)
    free_filter <- reactiveVal(FALSE)
    stop_update <- reactiveVal(FALSE)

    observeEvent(dt_update(), {
      if(!stop_update()){
        if(free_filter()){
          flt <- input$free_filter
        } else {
          flt <- combine_filters(filters = input$filter_list, input$filter_operation)
        }
        message <- apply_filter(d(), flt, input$train_test_filter)
        output$message <- renderText({message})
        updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
        user_filter(flt)
        free_filter(FALSE)
      }
      stop_update(FALSE)
    })
    observeEvent(input$apply_filter, {
      free_filter(TRUE)
      dt_update(dt_update()+1)
      message <- apply_filter(d(), input$free_filter, input$train_test_filter)
      output$message <- renderText({message})
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
      user_filter(input$free_filter)
    })
    observeEvent(c(input$filter_list, input$filter_operation, input$train_test_filter), ignoreInit = TRUE, {
      dt_update(dt_update()+1)
      filter_formula <- combine_filters(filters = input$filter_list, input$filter_operation)
      message <- apply_filter(d(), filter_formula, input$train_test_filter)
      output$message <- renderText({message})
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
      if(!is.null(filter_formula)){
        if(filter_formula=='no_filter'){
          value <- ''
        } else {
          value <- filter_formula
        }
        updateTextInput(inputId = 'free_filter', value = value)
      } else {
        updateTextInput(inputId = 'free_filter', value = '')
      }
      user_filter(filter_formula)
      stop_update(TRUE)
    })
    observeEvent(filter_spec(), {
      updateSelectInput(inputId = 'filter_list', choices = no_filter(filter_spec()[[1]]))
    })
    return(
      reactive(
        list(
          train_test_filter = input$train_test_filter,
          user_filter = user_filter()
        )
      )
    )
  })
}

filter_text <- function(d){
  na_count <- sum(is.na(d[['total_filter']]))
  rows_in_filter <- format(sum(d[['total_filter']], na.rm = TRUE), big.mark = ',', scientific = FALSE)
  rows <- format(nrow(d), big.mark = ',', scientific = FALSE)
  if(na_count>0){
    na_count <- format(na_count, big.mark = ',')
    paste0('Filter (', rows_in_filter,'/', rows, ', ', 'NAs)')
  } else {
    paste0('Filter (', rows_in_filter,'/', rows, ')')
  }

}

apply_filter <- function(d, filter, train_test_filter){
  user_filter <- NULL
  total_filter <- NULL
  if(is.null(train_test_filter) | 'train_test' %not_in% names(d)){
    train_test_filter <- 'All'
  }
  if(!is.null(d)){
    message <- 'no filter'
    if(nrow(d)>0){
      if(is.null(filter)){
        # no filter
        d[, user_filter := 1L]
        message <- 'no filter'
      } else {
        if(filter[[1]] %in% c('','no filter')){
          # no filter
          d[, user_filter := 1L]
          message <- 'no filter'
        } else {
          f <- tryCatch({d[, eval(parse(text=filter))]}, error = function(e){e})
          if('logical' %in% class(f)){
            d[, user_filter := as.integer(f)]
          } else {
            d[, user_filter := 1L]
          }
          if('error' %in% class(f)){
            message <- f$message
            loc_start <- gregexpr('is a single symbol but column name',message)
            loc_end <- gregexpr('is not found',message)
            if(loc_start>0){
              message <- paste0(substring(message, loc_start[[1]]+35, loc_end[[1]]-1), 'not found')
            }
          } else {
            message <- filter
          }
        }
      }
      # make the total filter from the user_filter and train_test
      if(train_test_filter=='All'){
        d[, total_filter := user_filter]
      } else if (train_test_filter=='Train'){
        d[, total_filter := user_filter*(1-train_test)]
      } else if (train_test_filter=='Test'){
        d[, total_filter := user_filter*train_test]
      }
      message
    }
  }
}

combine_filters <- function(filters, operation){
  if(operation %in% c('AND','NAND')){
    op <- ' & '
  } else if (operation %in% c('OR','NOR')){
    op <- ' | '
  }
  if(is.null(filters)){
    filter_expression <- filters
  } else if(length(filters)==1){
    filter_expression <- filters
  } else {
    filter_expression <- paste(filters, collapse = op)
  }
  if(operation %in% c('NAND','NOR')){
    filter_expression <- paste0('!(',filter_expression,')')
  }
  filter_expression
}

no_filter <- function(x){
  if(x[[1]]!='no filter'){
    x <- c('no filter', x)
  }
  x
}

filter_idx <- function(d, train_test){
  if(nrow(d)>0){
    if(is.null(train_test)){
      if('user_filter' %in% names(d)){
        idx <- d[user_filter==1L, which = TRUE]
      } else {
        idx <- 1:nrow(d)
      }
    } else if (!('train_test' %in% names(d))){
      if('user_filter' %in% names(d)){
        idx <- d[user_filter==1L, which = TRUE]
      } else {
        idx <- 1:nrow(d)
      }
    } else if (train_test == 'All'){
      if('user_filter' %in% names(d)){
        idx <- d[user_filter==1L, which = TRUE]
      } else {
        idx <- 1:nrow(d)
      }
    } else if (train_test == 'Train'){
      if('user_filter' %in% names(d)){
        idx <- d[user_filter==1L & train_test==0L, which = TRUE]
      } else {
        idx <- d[train_test==0L, which = TRUE]
      }
    } else if (train_test == 'Test'){
      if('user_filter' %in% names(d)){
        idx <- d[user_filter==1L & train_test==1L, which = TRUE]
      } else {
        idx <- d[train_test==1L, which = TRUE]
      }
    }
    idx
  }
}