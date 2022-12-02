#' @import shiny
#' @importFrom data.table data.table setDT setkey
#' @importFrom plotly style
#' @importFrom utils head
selectInput_ui <- function(id, label = 'your_label', height_divisor, height_adj, multiple = FALSE){
  ns <- NS(id)
  id_name <- ns("selectInput")
  # can't use - in js function name, replace with underscore
  function_name <- paste0(gsub('-','_',id_name), '_function')
  # QUESTION - how to put in .js file when it relies on sprintf to work in module?
  js_code <- sprintf('// Define function to set height of "xaxis-selectInput"
                %s = function() {
                  var window_height = $(window).height();
                  var header_height = $(".main-header").height();
                  window_height = window.innerHeight;
                  var num_rows = Math.min(64,Math.floor(window_height/%s-%s));
                  var preview = document.getElementById("%s");
                  preview.setAttribute("size", num_rows);
                };
                // Set input$height when the connection is established
                $(document).on("shiny:connected", function(event) {
                  %s();
                });

                // Refresh the box height on every window resize event
                $(window).on("resize", function(){
                  %s();
                });
              ', function_name, height_divisor, height_adj, id_name,function_name,function_name)
  tagList(
    div(
      radioGroupButtons(
        width = '100%',
        inputId = ns('selectChooser'),
        label = label,
        choices = c('Original', 'A-Z','lucidum'),
        individual = FALSE,
        size = 'xs',
        justified = TRUE,
        selected = 'Original'),
      style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
    ),
    tags$head(tags$script(js_code)),
    div(
      radioGroupButtons(
        width = '100%',
        inputId = ns('selectChooserGroup'),
        label = NULL,
        choices = c('No groups', 'Use groups'),
        individual = FALSE,
        size = 'xs',
        justified = TRUE,
        selected = 'No groups'),
      style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
    ),
    fluidRow(
      column(
        width = 10,
        style = 'margin-left:0px; margin-right:0px;padding-right:0px',
        div(
          textInput(
            inputId = ns('search'),
            width = '100%',
            label = NULL,
            placeholder = 'search (regex)'
          ),
          style = 'margin-top:0px; margin-bottom:-15px;'
        )
      ),
      column(
        width = 2,
        style = 'margin-left:0px; margin-right:0px;padding-left:0px',
        actionButton(
          width = '100%',
          style = 'padding: 6px 0px',
          inputId = ns('clear_selection'),
          label = NULL,
          icon = icon("circle-minus")
        )
      )
    ),
    selectInput(
      inputId = ns("selectInput"),
      width = '100%',
      label = NULL,
      choices=c('none'),
      size = 20,
      multiple = multiple,
      selectize = FALSE
      )
    )
}
selectInput_server <- function(id, d, dt_update, feature_spec, BoostaR_models, BoostaR_idx, numeric_only) {
  moduleServer(id, function(input, output, session) {
    # QUESTION - when something depends on so many things it feels odd
    # is this a case where a plan observe would be neater?
    observeEvent(c(d(), dt_update(), feature_spec(), BoostaR_models(), BoostaR_idx(), input$selectChooser, input$search, input$selectChooserGroup), {
      if(!is.null(d())){
        choices <- selectInput_choices(
          d(),
          feature_spec(),
          BoostaR_models(),
          BoostaR_idx(),
          input$selectChooser,
          input$selectChooserGroup,
          input$search,
          numeric_only
          )
      }
      if(is.null(input$selectInput)){
        selected <- character(0)
      } else if(input$selectInput %not_in% unlist(choices, use.names = FALSE)){
        selected <- character(0)
      } else {
        selected <- input$selectInput
      }
      updateSelectInput(
        session,
        inputId = 'selectInput',
        choices = choices,
        selected = selected
      )
    })
    
    # observeEvent(input$clear_selection, {
    #   updateTextInput(session, inputId = 'search', value = '')
    #   updateSelectInput(session, inputId = 'selectInput', selected = character(0))
    # })
    return(reactive({input$selectInput}))
  })
}

selectInput_choices <- function(
    d,
    feature_spec,
    BoostaR_models,
    BoostaR_idx,
    selectChooser,
    selectChooserGroup,
    search,
    numeric_only
    ){
  if(!is.null(d) & !is.null(selectChooser) & !is.null(selectChooserGroup)){
    if(numeric_only){
      cols <- numerical_cols(d)
    } else {
      cols <- names(d)
    }
    if (selectChooser=='Original'){
      # whatever was supplied
      choices <- remove_lucidum_cols(cols)
    } else if(selectChooser=='A-Z'){
      # alphabetical
      choices <- sort(remove_lucidum_cols(cols))
    } else if(selectChooser=='lucidum'){
      # get features
      if(!is.null(BoostaR_models) & !is.null(BoostaR_idx)){
        current_model_prediction <- intersect(cols, 'lgbm_prediction')
        importance_cols <- BoostaR_models[[BoostaR_idx]]$importances$Feature
        lgbm_cols <- cols[grep('lgbm', cols)]
        SHAP_cols <- cols[grep('lgbm_SHAP', cols)]
        lgbm_cols <- setdiff(lgbm_cols, c(SHAP_cols, 'lgbm_prediction'))
        all_cols <- c(current_model_prediction, importance_cols, lgbm_cols, SHAP_cols)
        lucidum_choices <- rbindlist(
          list(
            data.table(feature = current_model_prediction, interaction_grouping = 'Current model'),
            data.table(feature = importance_cols, interaction_grouping = 'LGBM feature importance'),
            data.table(feature = lgbm_cols, interaction_grouping = 'LGBM predictions'),
            data.table(feature = SHAP_cols, interaction_grouping = 'SHAP values')
            )
          )
        # create the choices list
        lucidum_choices <- split(lucidum_choices, by = 'interaction_grouping', sorted = TRUE, keep.by = FALSE)
        lucidum_choices <- lapply(lucidum_choices, function(d){d[[1]]}) # convert to character list
        if(selectChooser=='A-Z'){
          lucidum_choices <- lapply(lucidum_choices, sort)
        }
        choices <- lapply(lucidum_choices, list_if_length_one) # so selectInput choices look right
      } else {
        choices <- 'No lucidum columns'
      }
    }
    # get search choices
    search_choices <- NULL
    if(!is.null(search) & search!=''){
      search_choices <- tryCatch({choices[grepl(search, choices)]}, error = function(e){e})
      if(inherits(search_choices,'simpleError')){
        search_choices <- NULL
      } else if (length(search_choices)==0){
        search_choices <- 'no match'
      }
    }
    # split out search terms if search selected
    if(selectChooserGroup=='No groups' & !is.null(search_choices)){
      if(length(search_choices)>0){
        search_choices <- data.table(feature = search_choices, interaction_grouping = '--- matching search ---')
        choices_dt <- data.table(feature = choices, interaction_grouping = 'features')
        choices_dt <- rbindlist(list(search_choices,choices_dt))
        choices <- split(choices_dt, by = 'interaction_grouping', sorted = TRUE, keep.by = FALSE)
        choices <- lapply(choices, function(d){d[[1]]}) # convert to character list
        choices <- lapply(choices, list_if_length_one) # so selectInput choices look right
        if(names(choices)[1]==''){
          # selectInput lists must be named
          names(choices)[1] <- 'No grouping'
        }
      }
    }
    # apply grouping if selected
    if(selectChooserGroup=='Use groups' & selectChooser != 'lucidum'){
      if(is.null(feature_spec)){
        choices <- 'No feature specification'
      } else {
        if(nrow(feature_spec)==0){
          choices <- 'Empty feature specification'
        } else {
          choices_dt <- data.table(idx = 1:length(choices), feature = choices)
          setkey(choices_dt, feature)
          setkey(feature_spec, feature)
          choices_dt <- feature_spec[, c('feature','interaction_grouping')][choices_dt][order(idx)]
          choices_dt[is.na(interaction_grouping), interaction_grouping := 'No grouping']
          choices_dt[, idx := NULL]
          if(!is.null(search_choices)){
            if(length(search_choices)>0){
              search_choices <- data.table(feature = search_choices, interaction_grouping = '--- matching search ---')
              choices_dt <- rbindlist(list(search_choices,choices_dt))
            }
          }
          choices <- split(choices_dt, by = 'interaction_grouping', sorted = TRUE, keep.by = FALSE)
          choices <- lapply(choices, function(d){d[[1]]}) # convert to character list
          choices <- lapply(choices, list_if_length_one) # so selectInput choices look right
          if(names(choices)[1]==''){
            # selectInput lists must be named
            names(choices)[1] <- 'No grouping'
          }
        }
      }
    }
  }
  choices
}

list_if_length_one <- function(x){if(length(x)==1){x<-list(x)}else{x}}

