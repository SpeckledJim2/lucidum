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
      style = 'margin-top:0px; margin-bottom:-30px; padding-top:0px ; padding-bottom:0px;',
      radioGroupButtons(
        inputId = ns('train_test_filter'),
        label = 'Filter (0/0)',
        choices = c('All','Train','Test'),
        selected = 'All',
        justified = TRUE,
        size = 'sm',
        width = '100%',
      )
    ),
    tags$style(paste0("#",ns('free_filter')," {margin-bottom:-27px;}")),
    tags$style(paste0("#",ns('filter_list')," {margin-bottom:0px; margin-top:0px;}")),
    div(
      #style = 'margin-bottom:-30px',
      textInput(inputId = ns('free_filter'), placeholder = 'enter filter...', label = NULL, width = '100%'),
    ),
    div(
      #style = 'margin-top:0px; margin-bottom:0px',
      selectInput(inputId = ns('filter_list'),label = NULL, width = '100%', size = 6, selectize = FALSE,
                  choices = c('no filter',
                              "make=='PORSCHE'",
                              'age<25',
                              'mpg>20',
                              'value>30000'
                              )
                  )
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
mod_defineFilter_server <- function(id, d, dt_update){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    filter_slow <- debounce(reactive({input$free_filter}), 500)
    observeEvent(filter_slow(), {
      dt_update(dt_update()+1)
      message <- apply_filter(d(), filter_slow())
      output$message <- renderText({message})
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
    })
    observeEvent(dt_update(), {
      message <- apply_filter(d(), filter_slow())
      output$message <- renderText({message})
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
    })
    observeEvent(input$filter_list, ignoreInit = TRUE, {
      if(input$filter_list=='no filter'){
        updateTextInput(inputId = 'free_filter', value = '')
      } else {
        updateTextInput(inputId = 'free_filter', value = input$filter_list)
      }
    })
  })
}

filter_text <- function(d){
  paste0('Filter (', sum(d[['user_filter']]),'/', nrow(d), ')')
}

apply_filter <- function(d, filter){
  user_filter <- NULL
  if(nrow(d)>0){
    if(filter==''){
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
  message
  }
}