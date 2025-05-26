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
    tags$head(
      tags$style(HTML("
      .btn-custom-black {
        color: black !important;
      }
      .btn-custom-red {
        color: red !important;
      }
      .btn-custom-blue {
        color: blue !important;
      }
    "))
    ),
    tags$style(HTML(paste0("#", ns('train_test_filter'), " .btn {border-radius: 0 !important;}"))),
    fluidRow(
      column(
        width = 12,
        div(
          radioGroupButtons(
            inputId = ns('train_test_filter'),
            label = 'Filter (0/0)',
            choiceValues = c('All','Train','Test'),
            choiceNames = list(
              tags$span(class = "btn-custom-black", "All"),
              tags$span(class = "btn-custom-red", "Train"),
              tags$span(class = "btn-custom-blue", "Test")
            ),
            selected = 'All',
            justified = TRUE,
            size = 's',
            width = '100%'
          )
        )
      )
    ),
    div(
      style = "display: flex; width: 100%; padding-right: 15px",
        textInput(
          inputId = ns('free_filter'),
          label = NULL,
          placeholder = 'enter filter...',
          width = '100%'
        ),
      actionButton(
        inputId = ns('apply_filter'),
        label = NULL,
        icon = icon("filter"),
        style = "height: 34px; width: 34px; padding: 0; margin: 0; border-radius: 0px; margin-left: -15px"
      ),
      actionButton(
        inputId = ns('clear_filter'),
        label = NULL,
        icon = icon("xmark"),
        style = "height: 34px; width: 34px; padding: 0; margin: 0; border-radius: 0px;"
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          #style = 'margin-top:0px',
          selectInput(inputId = ns('filter_list'),label = NULL, width = '100%', size = 12, selectize = FALSE, multiple = TRUE, choices = NULL)
        )
      )
    ),
    tags$style(HTML(paste0("#", ns('filter_operation'), " .btn {border-radius: 0 !important;}"))),
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
        #free_filter(FALSE)
      }
      stop_update(FALSE)
    })
    observeEvent(input$apply_filter, {
      free_filter(TRUE)
      message <- apply_filter(d(), input$free_filter, input$train_test_filter)
      output$message <- renderText({message})
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
      updateSelectInput(inputId = 'filter_list', selected = character(0))
      dt_update(dt_update()+1)
      user_filter(input$free_filter)
    })
    observeEvent(input$clear_filter, {
      updateTextInput(inputId = 'free_filter', value = '')
      message <- apply_filter(d(), '', input$train_test_filter)
      output$message <- renderText({message})
      updateRadioButtons(inputId = 'train_test_filter', label = filter_text(d()))
      updateSelectInput(inputId = 'filter_list', selected = character(0))
      if(isTRUE(input$free_filter != input$filter_list) | free_filter()){
        dt_update(dt_update()+1)
      }
      free_filter(FALSE)
      user_filter('')
    })
    observeEvent(c(input$filter_list, input$filter_operation), ignoreInit = TRUE, {
      if(!is.null(input$filter_list)){
        free_filter(FALSE)
      }
      if(!is.null(input$filter_list) | (is.null(input$filter_list) & !free_filter())){
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
      }
    })
    observeEvent(input$train_test_filter, ignoreInit = TRUE, {
      dt_update(dt_update()+1)
      if(free_filter()){
        filter_formula <- input$free_filter
      } else {
        filter_formula <- combine_filters(filters = input$filter_list, input$filter_operation)
      }
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
      updateSelectInput(inputId = 'filter_list', choices = split_list(filter_spec()[[1]]))
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
            # turn NAs to FALSE
            f[is.na(f)] <- FALSE
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
        d[, total_filter := user_filter*(1L-train_test)]
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
filter_idx <- function(d, train_test) {
  if (nrow(d) == 0) return(integer())
  has_user_filter <- "user_filter" %in% names(d)
  has_train_test  <- "train_test" %in% names(d)
  if (is.null(train_test) || !has_train_test) {
    if (has_user_filter) {
      return(d[user_filter == 1L, which = TRUE])
    } else {
      return(seq_len(nrow(d)))
    }
  }
  # combine filters if needed
  if (has_user_filter) {
    return(d[user_filter == 1L & train_test == train_test, which = TRUE])
  } else {
    return(d[train_test == train_test, which = TRUE])
  }
}
split_list <- function(input_choices) {
  if (!any(startsWith(input_choices, "---"))) {
    return(input_choices)  # flat character vector
  }
  
  res <- list()
  current_group <- NULL
  buffer <- character(0)
  
  for (item in input_choices) {
    if (startsWith(item, "---")) {
      # skip empty or malformed group headers
      group_label <- trimws(sub("^---\\s*", "", item))
      if (group_label == "") {
        next
      }
      if (!is.null(current_group)) {
        res[[current_group]] <- buffer
      } else if (length(buffer) > 0) {
        res[["no grouping"]] <- buffer
      }
      current_group <- sub("^---\\s*", "", item)
      buffer <- character(0)
    } else {
      buffer <- c(buffer, item)
    }
  }
  
  if (!is.null(current_group)) {
    res[[current_group]] <- buffer
  } else if (length(buffer) > 0) {
    res[["no grouping"]] <- buffer
  }
  
  # ensure each group is a character vector (even if length 1)
  res <- lapply(res, list_if_length_one)
  
  return(res)
}