#' @import shiny
#' @importFrom data.table data.table setDT setkey
#' @importFrom plotly style
#' @importFrom utils head
selectInput_ui <- function(id, label = 'your_label', height_divisor, height_adj, multiple = FALSE, initial = 'Original'){
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
        selected = initial),
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
      choices=c('start_up_XXX'), # used below on startup
      size = 20,
      multiple = multiple,
      selectize = FALSE
      )
    )
}
selectInput_server <- function(id, d, dt_update, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx, numeric_only) {
  moduleServer(id, function(input, output, session) {
    # QUESTION - when something depends on so many things it feels odd
    # is this a case where a plan observe would be neater?
    observeEvent(c(d(), dt_update(), feature_spec(), BoostaR_models(), BoostaR_idx(), GlimmaR_models(), GlimmaR_idx(), input$selectChooser, input$search, input$selectChooserGroup), {
      if(!is.null(d())){
        choices <- selectInput_choices(
          d(),
          feature_spec(),
          BoostaR_models(),
          BoostaR_idx(),
          GlimmaR_models(),
          GlimmaR_idx(),
          input$selectChooser,
          input$selectChooserGroup,
          input$search,
          numeric_only,
          input$selectInput
          )
        selected <- character(0) # default
        if(!is.null(input$selectInput)){
          if(input$selectInput[1]=='start_up_XXX'){
            selected <- choices[1]
          } else if (all(input$selectInput %in% unlist(choices, use.names = FALSE))){
            selected <- input$selectInput
          }
        }
        updateSelectInput(
          session,
          inputId = 'selectInput',
          choices = choices,
          selected = selected
        )
      }
    })
    
    observeEvent(input$clear_selection, {
       updateTextInput(session, inputId = 'search', value = '')
       updateSelectInput(session, inputId = 'selectInput', selected = character(0))
     })
    return(reactive({input$selectInput}))
  })
}

selectInput_choices <- function(
    d,
    feature_spec,
    BoostaR_models,
    BoostaR_idx,
    GlimmaR_models,
    GlimmaR_idx,
    selectChooser,
    selectChooserGroup,
    search,
    numeric_only,
    current_selection
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
      if(length(BoostaR_models)>0 | length(GlimmaR_models)>0){
        current_model_prediction <- intersect(cols, c('glm_prediction','lgbm_prediction','glm_tabulated_prediction','lgbm_tabulated_prediction'))
        importance_cols <- NULL
        importance_cols_glm <- NULL
        if(!is.null(BoostaR_idx)){
          if(BoostaR_idx %in% names(BoostaR_models)){
            importance_cols <- intersect(BoostaR_models[[BoostaR_idx]]$importances$Feature, cols)
          }
        }
        if(!is.null(GlimmaR_idx)){
          if(GlimmaR_idx %in% names(GlimmaR_models)){
            # check there are some vars (a mean only model won't contain any vars)
            if(isTruthy(GlimmaR_models[[GlimmaR_idx]]$importances$vars)){
              importance_cols_glm <- intersect(GlimmaR_models[[GlimmaR_idx]]$importances$vars$names, cols)
            } else {
              importance_cols_glm <- NULL
            }
          }
        }
        lgbm_cols <- cols[grep('lgbm', cols)]
        SHAP_cols <- cols[grep('lgbm_SHAP', cols)]
        lgbm_cols <- setdiff(lgbm_cols, c(SHAP_cols, 'lgbm_prediction','lgbm_tabulated_prediction'))
        glm_cols <- cols[grep('glm', cols)]
        LP_cols <- cols[grep('glm_LP', cols)]
        glm_cols <- setdiff(glm_cols, c(LP_cols, 'glm_prediction','glm_tabulated_prediction'))
        all_cols <- c(current_model_prediction, importance_cols, importance_cols_glm, lgbm_cols, glm_cols, SHAP_cols, LP_cols)
        remaining_cols <- setdiff(numerical_cols(d), all_cols)
        # replace blanks
        if(length(current_model_prediction)==0) current_model_prediction <- 'none'
        if(length(importance_cols)==0) importance_cols <- 'none'
        if(length(importance_cols_glm)==0) importance_cols_glm <- 'none'
        if(length(lgbm_cols)==0) lgbm_cols <- 'none'
        if(length(glm_cols)==0) glm_cols <- 'none'
        if(length(SHAP_cols)==0) SHAP_cols <- 'none'
        if(length(LP_cols)==0) LP_cols <- 'none'
        lucidum_choices <- rbindlist(
          list(
            data.table(feature = current_model_prediction, interaction_grouping = 'Current model'),
            data.table(feature = importance_cols, interaction_grouping = 'GBM feature importance'),
            data.table(feature = importance_cols_glm, interaction_grouping = 'GLM feature importance'),
            data.table(feature = lgbm_cols, interaction_grouping = 'GBM predictions'),
            data.table(feature = glm_cols, interaction_grouping = 'GLM predictions'),
            data.table(feature = SHAP_cols, interaction_grouping = 'GBM SHAP values'),
            data.table(feature = LP_cols, interaction_grouping = 'GLM LP values'),
            data.table(feature = remaining_cols, interaction_grouping = 'Other columns')
            )
          )
        # create the choices list
        if (selectChooser %in% c('Original','A-Z')){
          lucidum_choices <- split(lucidum_choices, by = 'interaction_grouping', sorted = TRUE, keep.by = FALSE)
        } else if (selectChooser=='lucidum'){
          lucidum_choices <- split(lucidum_choices, by = 'interaction_grouping', sorted = FALSE, keep.by = FALSE)
        }
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
      nl_cols <- remove_lucidum_cols(cols)
      search_choices <- tryCatch({nl_cols[grepl(search, nl_cols)]}, error = function(e){e})
      search_choices <- unname(search_choices)
      if(inherits(search_choices,'simpleError')){
        choices <- NULL
      } else if (length(search_choices)==0){
        choices <- 'no match'
      } else {
        choices <- sort(search_choices)
      }
    }
  }
  # split by group if selected
  if(selectChooserGroup=='Use groups'){
    # add on interaction groupings if not already created
    if(selectChooser != 'lucidum'){
      choices <- data.table(idx = 1:length(choices), feature = choices)
      setkey(choices, feature)
      setkey(feature_spec, feature)
      choices <- feature_spec[, c('feature','interaction_grouping')][choices][order(idx)]
      choices[is.na(interaction_grouping), interaction_grouping := 'No grouping']
      choices[, idx := NULL]
      choices <- split(choices, by = 'interaction_grouping', sorted = TRUE, keep.by = FALSE)
      choices <- lapply(choices, function(d){d[[1]]}) # convert to character list
      choices <- lapply(choices, list_if_length_one) # so selectInput choices look right
      if(names(choices)[1]==''){
        # selectInput lists must be named
        names(choices)[1] <- 'No match'
      }
    }
  }
  choices
}

list_if_length_one <- function(x){if(length(x)==1){x<-list(x)}else{x}}

