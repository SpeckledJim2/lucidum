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
    div(
      style = 'margin-top:0px; margin-bottom:-28px; padding-top:0px ; padding-bottom:0px;',
      radioGroupButtons(
        inputId = ns('train_test_filter'),
        label = 'Filter (0/0)',
        choices = c('All','Train','Test'),
        selected = 'All',
        justified = TRUE,
        size = 'xs',
        width = '100%',
      )
    ),
    #tags$style(paste0("#",ns('free_filter')," {margin-bottom:-30px;}")),
    #tags$style(paste0("#",ns('filter_list')," {margin-bottom:0px; margin-top:0px;}")),
    fluidRow(
      style = 'margin-bottom: 0px; margin-left:0px; margin-right:0px;padding-left:0px',
      column(
        width = 10,
        style = 'margin-left:0px; margin-right:0px;padding-left:0px; padding-right:0px',
        div(
          style = 'margin-left:0px; margin-right:0px;padding-left:0px; padding-right:0px',
          textInput(inputId = ns('free_filter'), placeholder = 'enter filter...', label = NULL, width = '100%')
        )
      ),
      column(
        width = 2,
        style = 'display: inline; margin-left:0px; margin-right:0px;padding-left:0px; padding-right:20px',
        div(
          style = 'margin-top:12px; margin-left:-30px; margin-right:10px;padding-left:0px; padding-right:0px',
          actionButton(
            width = '100%',
            inputId = ns('apply_filter'),
            label = NULL,
            icon = icon("filter")
          )
        )

      )
    ),
    div(
      style = 'margin-top:-27px; margin-bottom:-30px; position:relative',
      selectInput(inputId = ns('filter_list'),label = NULL, width = '100%', size = 6, selectize = FALSE, multiple = TRUE,
                  choices = c('no filter',
                              "make=='PORSCHE'",
                              "make=='BMW'",
                              "make=='FORD'",
                              'age<25',
                              'mpg>20',
                              'value>30000'
                              )
                  )
    ),
    div(
      radioGroupButtons(
        inputId = ns('filter_operation'),
        label = NULL,
        choices = c('AND','OR','NAND','NOR'),
        individual = FALSE,
        size = 'xs',
        justified = TRUE
      ),
      style = 'padding-top:0px ; padding-bottom:0px'
    ),
    div(
      style = 'margin-top:-10px; margin-bottom:0px; margin-left:20px; font-size: 10px',
      textOutput(ns('message'))
    )
  )
}
    
#' defineFilter Server Functions
#'
#' @noRd 
mod_defineFilter_server <- function(id, d, dt_update, feature_spec){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    free_filter <- reactiveVal(FALSE)
    stop_update <- reactiveVal(FALSE)
    observeEvent(dt_update(), {
      if(!stop_update()){
        if(free_filter()){
          flt <- input$free_filter
        } else {
          flt <- combine_filters(filters = input$filter_list, input$filter_operation)
        }
        message <- apply_filter(d(), flt)
        output$message <- renderText({message})
        updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
        free_filter(FALSE)
      }
      stop_update(FALSE)
    })
    observeEvent(input$apply_filter, {
      free_filter(TRUE)
      dt_update(dt_update()+1)
      message <- apply_filter(d(), input$free_filter)
      output$message <- renderText({message})
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
    })
    observeEvent(c(input$filter_list, input$filter_operation), ignoreInit = TRUE, {
      dt_update(dt_update()+1)
      if(length(input$filter_list)==0){
        # nothing selected
        d()[, user_filter:=1]
        message <- 'no filter'
      } else {
        if(input$filter_list[1]=='no filter'){
        # no filter
          d()[, user_filter:=1]
          message <- 'no filter'
        } else {
          filter_formula <- combine_filters(filters = input$filter_list, input$filter_operation)
          updateTextInput(inputId = 'free_filter', value = filter_formula)
          message <- apply_filter(d(), filter_formula)
        }
      }
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
      output$message <- renderText({message})
      stop_update(TRUE)
    })
    observeEvent(feature_spec(), {
      updateSelectInput(inputId = 'filter_list', choices = no_filter(feature_spec()[[1]]))
    })
  })
}

filter_text <- function(d){
  rows_in_filter <- format(sum(d[['user_filter']]), big.mark = ',')
  rows <- format(nrow(d), big.mark = ',')
  paste0('Filter (', rows_in_filter,'/', rows, ')')
}

apply_filter <- function(d, filter){
  user_filter <- NULL
  if(nrow(d)>0){
    if(is.null(filter)){
      # no filter
      d[, user_filter := TRUE]
      message <- 'no filter'
    } else {
      if(filter[[1]]==''){
        # no filter
        d[, user_filter := TRUE]
        message <- 'no filter'
      } else {
        f <- tryCatch({d[, eval(parse(text=filter))]}, error = function(e){e})
        if('logical' %in% class(f)){
          d[, user_filter := f]
        } else {
          d[, user_filter := TRUE]
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

  message
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
}