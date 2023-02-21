#' buildGlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom readr read_file 
mod_GlimmaR_build_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 5,
        fluidRow(
          column(
            width = 3,
            h3("Formula")
          ),
          column(
            width = 4,
            div(
              id = ns('wrapper'),
              style = 'margin-top: 22px',
              selectInput(
                inputId = ns('objective'),
                width = '100%',
                label = NULL,
                choices = list('identity link' = list('gaussian'),
                               'log link' = list('poisson',
                                                 'quasipoisson',
                                                 'gaussian (log link)',
                                                 'gamma',
                                                 'tweedie'),
                               'logit link' = list('binomial')
                ),
                selected = 'gamma'
              )
            ),
            tippy_this(ns('wrapper'), delay = 1000, placement = 'bottom', tooltip = tippy_text('Choose GLM error and link function',12))
          ),
          column(
            width = 2,
            align = 'right',
            br(),
            dropdownButton(inputId = ns('helper_dropdown'),
                           width = 700,
                           up = FALSE,
                           circle = FALSE,
                           size = 'default',
                           label = tags$img(src='www/formula_helper.png', height="18px", width="26px"),
                           right = FALSE,
                           margin = '10px',
                           fluidRow(
                             column(
                               width = 6,
                               div(
                                 radioGroupButtons(
                                   inputId = ns('helper_feature_choice'),
                                   choices = c('Original','A-Z','GBM'),
                                   size = 's',
                                   label = 'Feature',
                                   justified = TRUE
                                 ),
                                 style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
                               ),
                               div(
                                 textInput(
                                   inputId = ns('helper_search'),
                                   width = '100%',
                                   label = NULL,
                                   placeholder = 'filter'
                                 ),
                                 style = 'margin-top:0px; margin-bottom:-15px;'
                               ),
                               selectInput(
                                 inputId = ns('helper_feature'),
                                 label = NULL,
                                 choices = NULL,
                                 multiple = FALSE,
                                 selectize = FALSE,
                                 size = 15
                               )
                             ),
                             column(
                               width = 6,
                               div(
                                 radioGroupButtons(
                                   inputId = ns('helper_levels_choice'),
                                   choices = c('Single','Group'),
                                   size = 's',
                                   label = 'Factor grouping/function selection',
                                   justified = TRUE,
                                 ),
                                 style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
                               ),
                               div(
                                 textInput(
                                   inputId = ns('helper_level_text'),
                                   width = '100%',
                                   label = NULL,
                                   placeholder = 'function arguments seperated by commas'
                                 ),
                                 style = 'margin-top:0px; margin-bottom:-15px;'
                               ),
                               selectInput(
                                 inputId = ns('helper_levels'),
                                 label = NULL,
                                 choices = NULL,
                                 multiple = TRUE,
                                 selectize = FALSE,
                                 size = 15
                               )
                             )
                           ),
                           fluidRow(
                             column(
                               width = 12,
                               textAreaInput(
                                 inputId = ns('formula_suggestion'),
                                 label = NULL,
                                 width = '100%',
                                 height = '200px',
                                 resize = 'none'
                               )
                             )
                           )
            ),
            tippy_this(ns('helper_dropdown'), delay = 1000, placement = 'bottom', tooltip = tippy_text('Create GLM formula',12))
          ),
          column(
            width = 3,
            align = 'right',
            style = 'padding-left: 0px;',
            br(),
            actionButton(
              inputId = ns('build_GLM'),
              label = "Build",
              icon = icon("chevron-right"),
              style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left"
            ),
            tippy_this(ns('build_GLM'), delay = 1000, placement = 'right', tooltip = tippy_text('Build the GLM and create coefficient table',12))
          )
        ),
        fluidRow(
          column(
            width = 4,
            style = 'padding-right:0px',
            align = 'right',
            radioGroupButtons(
              inputId = ns('data_to_use'),
              justified =  TRUE,
              label = NULL,
              choices = c('All', 'Training'),
              selected = 'Training'
            ),
            tippy_this(ns('data_to_use'), delay = 1000, placement = 'right', tooltip = tippy_text('Choose rows supplied to GLM',12))
          ),
          column(
            width = 5,
            align = 'right',
            actionButton(
              inputId = ns('clear_formula'),
              label = 'clear',
              icon = icon("minus-circle")
            ),
            tippy_this(ns('clear_formula'), delay = 1000, placement = 'right', tooltip = tippy_text('clear GLM formula above',12)),
            actionButton(
              inputId = ns('textsize_minus'),
              label = "A-",
              style = 'padding-left: 8px; padding-right:8px'
            ),
            actionButton(
              inputId = ns('textsize_plus'),
              label = "A+",
              style = 'padding-left: 6px; padding-right:6px'
            )
          ),
          column(
            width = 3,
            align = 'right',
            shinyFilesButton(
              id = ns('formula_load'),
              label = '',
              filetype=list(txt="txt"),
              icon = icon('download'),
              title = 'Choose formula',
              #style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              multiple = FALSE
            ),
            tippy_this(ns('formula_load'), delay = 1000, placement = 'bottom', tooltip = tippy_text('Load GLM formula from .txt file',12)),
            shinySaveButton(
              id = ns('formula_save'),
              label = '',
              title = 'Choose location to save formula',
              filename = "",
              filetype=list(txt="txt"),
              icon = icon('upload'),
              #style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              viewtype = "detail"
            ),
            tippy_this(ns('formula_save'), delay = 1000, placement = 'bottom', tooltip = tippy_text('Save GLM formula to .txt file',12))
          )
        ),
        # QUESTION - need to move
        tags$style(".form-group.shiny-input-container { width: 100%; }"),
        aceEditor(
          ns('glm_formula'),
          showPrintMargin = FALSE,
          mode = "r",
          fontSize = 12,
          wordWrap = FALSE,
          height = 'calc(80vh - 100px)',
          autoScrollEditorIntoView = TRUE,
          autoComplete = 'live',
          selectionId = 'selection',
          debounce = 10,
          value = ''
        )
      ),
      column(
        width = 7,
        fluidRow(
          column(
            width = 3,
            h3("Coefficients")
          ),
          column(
            width = 4,
            div(
              style = 'margin-left: 30px',
              htmlOutput(ns('model_dispersion'))
            )
          ),
          column(
            width = 3,
            htmlOutput(ns('model_NAs'))
          ),
          column(
            width = 2,
            align = 'right',
            br(),
            actionButton(
              inputId = ns('goto_ChartaR'),
              label = tagList(tags$img(src='www/one_way_line_bar.png', height="26px", width="26px")),
              style = 'padding:3px 5px 3px 5px'
            ),
            tippy_this(ns('goto_ChartaR'), delay = 1000, placement = 'bottom', tooltip = tippy_text('Show selected feature in ChartaR',12))
          )
        ),
        br(),
        DTOutput(ns('glm_coefficients'))
      )
    )
  )
}



#' buildGlimmaR Server Functions
#'
#' @import splines
#' @importFrom readr read_file
#'
#' @noRd 
mod_GlimmaR_build_model_server <- function(id, d, dt_update, response, weight, GlimmaR_models, GlimmaR_idx, BoostaR_models, BoostaR_idx, crosstab_selector){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    demo <- reactiveVal(TRUE)
    observeEvent(demo(), once = TRUE, {
      if(demo()){
        fpath <- system.file("glm_formula.csv", package="lucidum")
        txt <- read_file(file = fpath)
      }
      updateAceEditor(session, editorId = 'glm_formula', value = txt)
    })
    text_size <- reactiveVal(12)
    observeEvent(input$build_GLM, {
      GlimmaR_model <- GlimmaR_build_GLM(
        session,
        d(),
        response(),
        weight(),
        input$data_to_use,
        input$glm_formula,
        input$objective
        )
      if(GlimmaR_model$message=='ok'){
        # QUESTION - feels inefficient (copying large object), is there a better way?}
        model_name <- make_unique_name(response(), names(GlimmaR_models()), 'glm')
        GlimmaR_model$name <- model_name
        new_list <- GlimmaR_models()
        new_list[[model_name]] <- GlimmaR_model
        GlimmaR_models(new_list)
        # select model
        GlimmaR_idx(names(GlimmaR_models())[length(names(GlimmaR_models()))])
      } else {
        confirmSweetAlert(session = session, type = 'error', inputId = ns('GlimmaR_error'), title = "Error", text = GlimmaR_model$message, btn_labels = c('OK'))
      }
    })
    observeEvent(input$clear_formula, {
      updateAceEditor(session, editorId = 'glm_formula', value = '')
    })
    observeEvent(input$textsize_minus, {
      text_size(pmax(8,text_size()-1))
      updateAceEditor(session, editorId = 'glm_formula', fontSize = text_size())
    })
    observeEvent(input$textsize_plus, {
      text_size(pmin(30,text_size()+1))
      updateAceEditor(session, editorId = 'glm_formula', fontSize = text_size())
    })
    observeEvent(GlimmaR_idx(), {
      # update the coefficient table
      if(!is.null(GlimmaR_models()) & !is.null(GlimmaR_idx())){
        g <- GlimmaR_models()[[GlimmaR_idx()]]
        # update coefficient table
        output$glm_coefficients <- DT::renderDataTable({
          GlimmaR_coefficient_DT(g$coefficients)
          })
        # update the formula
        updateAceEditor(session, editorId = 'glm_formula', value = g$formula)
        # update the objective
        updateSelectInput(session, inputId = 'objective', selected = g$objective)
        # update the model dispersion
        output$model_dispersion <- renderUI({
          if(GlimmaR_idx() %in% names(GlimmaR_models())){
            g <- GlimmaR_models()[[GlimmaR_idx()]]
            if(is.null(g)){
              text <- ''
            } else {
              text <- paste0('<b>Dispersion: </b>', signif(g$dispersion,4))
            }
            p(HTML(text), style = 'font-size: 12px; margin-top: 26px')
          }
        })
        # update the NA count
        output$model_NAs <- renderUI({
          if(GlimmaR_idx() %in% names(GlimmaR_models())){
            g <- GlimmaR_models()[[GlimmaR_idx()]]
            if(g$count_NAs>0){
              text <- paste0('<b><span style=\"color:red\"><b>NAs in fitted: ', g$count_NAs, '</span>')
            } else {
              text <- paste0('<b>NAs in fitted</b>: 0')
            }
            p(HTML(text), style = 'font-size: 12px; margin-top: 26px')
          }
        })
      }
    })
    observeEvent(c(d(), dt_update()), {
      # define the autocomplete options for shinyAce glm_formula editor
      comps <- list()
      if(!is.null(golem::get_golem_options('dataset_name'))){
        group_name <- golem::get_golem_options('dataset_name')
      } else {
        group_name <- 'columns'
      }
      comps[[group_name]] <- names(d())
      updateAceEditor(session, 'glm_formula', autoCompleteList = comps, autoCompleters = c('static'))
    })
    observeEvent(input$formula_save, {
      volumes <- c('working directory' = getwd(), 'home' = fs::path_home())
      shinyFileSave(input, "formula_save", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$formula_save)
      if (nrow(fileinfo) > 0) {
        fileConn<-file(fileinfo$datapath)
        writeLines(isolate(input$glm_formula), fileConn)
        close(fileConn)
      }
    })
    observeEvent(input$formula_load, {
      volumes <- c('working directory' = getwd(), 'home' = fs::path_home())
      shinyFileChoose(input, "formula_load", roots=volumes, session=session)
      fileinfo <- parseFilePaths(volumes, input$formula_load)
      if (nrow(fileinfo) > 0) {
        glm_formula <- readr::read_file(file = fileinfo$datapath)
        updateAceEditor(session, editorId = 'glm_formula', value = glm_formula)
      }
    })
    observeEvent(input$goto_ChartaR, {
      last_clicked <- input$glm_coefficients_cell_clicked$value
      # extracts first feature in the supplied GLM term
      if(!is.null(last_clicked)){
        n <- length(GlimmaR_models())
        all_model_variables <- all.vars(GlimmaR_models()[[GlimmaR_idx()]]$glm$terms)
        feature <- NULL
        for (i in 1:length(all_model_variables)){
          present <- grep(all_model_variables[i], last_clicked, fixed = TRUE)
          if(length(present)>0){
            feature <- all_model_variables[i]
            break
          }
        }
        # forces update when last_clicked hasn't changed
        val <- crosstab_selector()$val
        if(is.null(val)) val <- 1 else val <- val + 1
        info_list <- list(
          originator = 'GlimmaR coefficient table',
          last_clicked = feature,
          val = val
        )
        crosstab_selector(info_list)
      }
    })

    observe({
      if(!is.null(BoostaR_idx())){
        b <- BoostaR_models()[[BoostaR_idx()]]
      } else {
        b <- NULL
      }
      updateSelectInput(
        session,
        inputId = 'helper_feature',
        choices = make_GlimmaR_helper_features(d(), b, input$helper_feature_choice, input$helper_search)
        )
        
    })
    observe({
      updateSelectInput(session,
                        inputId = 'helper_levels',
                        choices = make_GlimmaR_helper_levels(d(), input$helper_feature)
                        )
    })
    observe({
      updateTextAreaInput(session,
                          inputId = 'formula_suggestion',
                          value = make_GlimmaR_formula_suggestion(
                            d = d(),
                            feature = input$helper_feature,
                            options = input$helper_levels,
                            level_grouping = input$helper_levels_choice,
                            inputs = input$helper_level_text
                            )
                          )
    })
  })
}

#' @importFrom broom tidy
#' @importFrom statmod tweedie
GlimmaR_build_GLM <- function(session, d, response, weight, data_to_use, glm_formula, glm_objective){
  l <- list()
  if(!(response %in% names(d))){
  } else if (!(weight %in% c('N',names(d)))){
  } else if (is.null(d)){
  } else if (data_to_use=='Training' & !('train_test' %in% names(d))){
    # training data selected but no train test column
    l$message <- 'no train_test column'
    confirmSweetAlert(session = session,
                      type = 'error',
                      inputId = "build_error",
                      title = l$message,
                      text = 'Training selected but there is no train_test column in the dataset',
                      btn_labels = c('OK')
    )
  } else {
    # attempt to turn text input into a formula
    original_glm_formula <- glm_formula
    glm_formula <- paste(response, ' ~ ', glm_formula)
    glm_formula <- tryCatch({stats::as.formula(glm_formula)},error = function(e){NULL})
    if(is.null(glm_formula)){
      l$message <- 'check model formula'
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "build_error",
                        title = l$message,
                        text = '',
                        btn_labels = c('OK')
      )
    } else if (all.vars(glm_formula)[1] %in% labels(stats::terms(glm_formula))){
      # response (left hand side) is also in formula (right hand side)
      l$message <- 'response in formula'
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "build_error",
                        title = l$message,
                        text = '',
                        btn_labels = c('OK')
      )
    } else if (any(all.vars(glm_formula) %in% names(d)[sapply(d, is.character)])){
      # formula contains character columns
      # R glm uses factor columns, not character
      l$message <- 'Character columns in formula'
      confirmSweetAlert(session = session,
                        type = 'error',
                        inputId = "build_error",
                        title = l$message,
                        text = 'GLM requires factors: convert character columns to factors',
                        btn_labels = c('OK')
      )
    } else {
      # set the GLM family
      if (glm_objective=='gaussian'){
        family <- stats::gaussian(link='identity')
        link <- 'identity'
      } else if (glm_objective=='binomial'){
        family <- stats::binomial(link = 'logit')
        link <-  'logit'
      } else if (glm_objective=='poisson'){
        family <- stats::poisson(link = 'log')
        link <-  'log'
      } else if (glm_objective=='gaussian (log link)'){
        family <- stats::gaussian(link = 'log')
        link <-  'log'
      } else if (glm_objective=='gamma'){
        family <- stats::Gamma(link = 'log')
        link <-  'log'
      } else if (glm_objective=='tweedie'){
        # link.power = 0 means log link which is usually what you want
        family <- statmod::tweedie(var.power = 1.1, link.power = 0)
        link <-  'log'
      } else if (glm_objective=='quasipoisson'){
        family <- stats::quasipoisson(link = 'log')
        link <-  'log'
      }
      # use whole dataset or just training
      if(data_to_use=='All'){
        include <- 1:nrow(d)
      } else {
        include <- which(d[['train_test']]==0)
      }
      # get rid of rows with non-zero weights for the model fit
      if(weight!='N'){
        non_zero_weight_rows <- which(d[[weight]]>0)
        include <- intersect(include, non_zero_weight_rows)
      } else {
        non_zero_weight_rows <- 1:nrow(d)
      }
      # build model
      withProgress(message = 'GlimmaR', detail = 'building model', value = 0.5,{
        start_time <- Sys.time()
        message <- 'ok'
        glm_model <- tryCatch({stats::glm(formula = glm_formula,
                                          model = FALSE,
                                          data = d[include],
                                          family = family)},
                              error = function(e){e})
        time <- as.numeric(difftime(Sys.time(), start_time, units = 's'))
        # check if something went wrong
        if(!(class(glm_model)[[1]]=='glm')){
          # something went wrong
          l <- list()
          l$message <- glm_model$message
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "build_error",
                            title = 'GLM build error',
                            text = glm_model$message,
                            btn_labels = c('OK')
          )
        } else {
          # get coefficient summary table in nice format
          c <- broom::tidy(glm_model)
          c$statistic <- NULL
          c[, 2:4] <- signif(c[,2:4])
          # predict on dataset
          incProgress(0.1, detail = 'predicting')
          fitted_glm <- tryCatch({stats::predict(glm_model, d, type = 'response')}, error = function(e){e})
          if(inherits(fitted_glm,'simpleError')){
            confirmSweetAlert(session = session,
                              type = 'error',
                              inputId = "build_error",
                              title = 'GLM prediction error',
                              text = fitted_glm$message,
                              btn_labels = c('OK'))
          } else {
            # append predictions to main dataset
            fitted_glm[-non_zero_weight_rows] <- 0 # set predictions to zero for rows not used to build model
            count_NAs <- sum(is.na(fitted_glm[non_zero_weight_rows])) # in case we supplied a factor with NAs to the model
            # calculate linear predictor split by feature
            incProgress(0.1, detail = 'LP terms')
            LP_contributions <- gather_glm_terms(d[non_zero_weight_rows], glm_model)
            # calculate the term and feature importances
            importances <- calc_terms_importances(glm_model)
            # return GlimmaR_model
            l <- list(time = time,
                      predictions = fitted_glm[non_zero_weight_rows],
                      pred_rows = non_zero_weight_rows,
                      glm = strip_glm(glm_model),
                      training_data = data_to_use,
                      formula = original_glm_formula,
                      coefficients = c,
                      num_terms = nrow(c),
                      objective = glm_objective,
                      link = link,
                      response = response,
                      weight = weight,
                      rows_used = include,
                      non_zero_weight_rows = non_zero_weight_rows,
                      deviance = glm_model$deviance,
                      AIC = glm_model$aic,
                      dispersion = dispersion_estimate(glm_model),
                      count_NAs = count_NAs,
                      LP_contributions = LP_contributions,
                      importances = importances,
                      message = message
            )
          }
        }
      })
    }
  }
  return(l)
}

gather_glm_terms <- function(d, model){
  # takes input glm model
  # outputs data table of each feature's contribution to the terms outputs
  # get list of all features used in the model
  # loop through each feature
  # identify terms containing feature
  # sum terms
  # get the model features and model terms
  features <- all.vars(model$formula)
  model_terms <- attr(stats::terms(model), 'term.labels')
  if(length(features)>1 & length(model_terms) >0){
    features <- features[2:length(features)] # remove first element as is response
    model_terms_formulae <- paste('y ~', model_terms)
    model_terms_formulae <- lapply(model_terms_formulae, stats::as.formula)
    model_terms_features <- lapply(model_terms_formulae, all.vars)
    # create matrix to hold results
    glm_feature_contributions <- matrix(0, nrow = nrow(d), ncol = length(features))
    # get the linear prediction broken down into individual terms
    terms_predictions <- stats::predict(model, d, type = 'terms')
    # loop through each feature and create feature contributions to linear predictor
    for (i in 1:length(features)){
      cols <- rep(FALSE, length(features))
      # identify GLM term columns containing the feature
      feature <- features[i]
      term_contains_feature <- lapply(model_terms_features, is.element, feature)
      term_contains_feature <- lapply(term_contains_feature, sum)
      term_contains_feature <- unlist(term_contains_feature)
      cols <- ifelse(term_contains_feature>0,TRUE,FALSE)
      glm_feature_contributions[,i] <- rowSums(terms_predictions[, cols, drop = FALSE])
    }
    glm_feature_contributions <- as.data.table(glm_feature_contributions)
    names(glm_feature_contributions) <- paste(sep = '_', 'glm_LP', features)
  } else {
    glm_feature_contributions <- NULL
  }
  glm_feature_contributions
}
dispersion_estimate <- function(model){
  if(model$family$family %in% c('poisson','binomial')){
    1
  } else {
    sum((model$weights * model$residuals^2)[model$weights > 0])/model$df.residual
  }
}
strip_glm <- function(cm) {
  
  # strips out stuff we don't need from GLM model to make it smaller when saving
  cm$y <- c()
  cm$model <- c()
  
  cm$residuals <- c()
  cm$fitted.values <- c()
  cm$effects <- c()
  cm$qr$qr <- c()
  cm$linear.predictors <- c()
  cm$weights <- c()
  cm$prior.weights <- c()
  cm$data <- c()
  cm$offset <- c()
  
  # following means glm unable to predict using type = 'terms'
  cm$family$variance <- c()
  cm$family$dev.resids <- c()
  cm$family$aic <- c()
  cm$family$validmu <- c()
  cm$family$simulate <- c()
  attr(cm$terms,".Environment") <- globalenv() # otherwise the object is huge
  attr(cm$formula,".Environment") <- c()
  
  cm
}
make_GlimmaR_helper_features <- function(d, BoostaR_model, choice, search){
  features <- NULL
  if(!is.null(d)){
    if(choice=='Original'){
      features <- remove_lucidum_cols(names(d))
    } else if (choice=='A-Z'){
      features <- sort(remove_lucidum_cols(names(d)))
    } else if (choice=='GBM'){
      if(!is.null(BoostaR_model)){
        features <- BoostaR_model$importances$Feature
      } else {
        features <- '-- no GBMs built --'
      }
    }
  }
  # filter on search
  search_choices <- NULL
  if(!is.null(search) & search!=''){
    features <- tryCatch({features[grepl(search, features)]}, error = function(e){e})
    if(inherits(features,'simpleError')){
      features <- '-- no result --'
    } else if(length(features)==0){
      features <- '-- no result --'
    }
  }
  return(features)
}
make_GlimmaR_helper_levels <- function(d, feature){
  result <- '-- no feature selected --'
  if(!is.null(d) & !is.null(feature)){
    if(feature %in% names(d)){
      if(class(d[[feature]])[1]=='factor'){
        result <- levels(d[[feature]])
        if(length(result)>1000){
          result <- '-- too many levels (>1,000) --'
        }
      } else if (class(d[[feature]])[1] %in% c('integer','numeric')){
        result <- c('Identity',
                    'pmin(x, feature)',
                    'pmax(x, feature)',
                    'pmax(x, pmin(y, feature))',
                    'Polynomial (order)',
                    'log(feature)',
                    'log(1+feature)',
                    'sqrt(feature)',
                    'Piecewise linear (breaks)',
                    'if(feature=x,1,0)',
                    'if(feature<x,1,0)',
                    'if(feature>x,1,0)',
                    'between(feature,x,y)',
                    'Spline (df)',
                    'Spline (knots)'
        )
      }
    }
  }
  return(result)
}
make_GlimmaR_formula_suggestion <- function(d, feature, options, level_grouping, inputs){
  if(!is.null(d) & !is.null(feature)){
    comment_line <- paste0('# ', feature)
    formula_lines <- NULL
    if(class(d[[feature]])[1]=='factor'){
      if(level_grouping=='Single'){
        if(!is.null(options)){
          if(length(options)>0){
            formula_lines <- paste0("ifelse(", feature, "=='", options, "',1,0)")
            formula_lines <- paste(formula_lines, collapse = ' + \n')
            formula_lines <- paste(formula_lines, sep = '\n')
          }
        }
      } else if (level_grouping=='Group'){
        formula_lines <- paste(options, collapse = "','")
        formula_lines <- paste0("c('", formula_lines, "')")
        formula_lines <- paste0("ifelse(", feature, " %in% ", formula_lines, ",1,0)")
      }
    } else if(class(d[[feature]])[1] %in% c('integer','numeric')){
      if(length(options)==1){
        formula_lines <- make_numerical_feature_formula(feature, options, inputs)
      }
    }
    result <- paste(comment_line, formula_lines, sep = '\n')
  }
}
make_numerical_feature_formula <- function(feature, formula_type, inputs){
  # inputs is a sequence of numbers separated by commas
  inputs <- as.numeric(unlist(strsplit(inputs, ',')))
  n <- length(inputs)
  if(length(inputs)==0){
    inputs <- c('x','y')
  } else if (length(inputs)==1){
    inputs <- c(inputs[1], 'y')
  }
  if(formula_type=='Identity'){
    feature
  } else if (formula_type=='pmin(x, feature)'){
    paste0('pmin(', inputs[1], ', ', feature, ')')
  } else if (formula_type=='pmax(x, feature)'){
    paste0('pmax(', inputs[1], ', ', feature, ')')
  } else if (formula_type=='pmax(x, pmin(y, feature))'){
    paste0('pmax(', inputs[1], ', pmin(', inputs[2], ', ',feature, '))')
  } else if (formula_type=='Polynomial (order)'){
    paste0('poly(', feature, ', ', inputs[1], ')')
  } else if (formula_type=='log(feature)'){
    paste0('log(', feature, ')')
  } else if (formula_type=='log(1+feature)'){
    paste0('log(1+', feature, ')')
  } else if (formula_type=='sqrt(feature)'){
    paste0('sqrt(', feature, ')')
  } else if (formula_type=='Piecewise linear (breaks)'){
    if(n<=1){
      paste0('pmin(', inputs[1], ', ', feature, ') +\n',
             'pmax(', inputs[1], ', ', feature, ')')
    } else if (n==2){
      paste0('pmin(', inputs[1], ', ', feature, ') +\n',
             'pmax(', inputs[1], ', pmin(', inputs[2], ', ',feature, ')) +\n',
             'pmax(', inputs[2], ', ', feature, ')')
    } else {
      tstart <- paste0('pmin(', inputs[1], ', ', feature, ') +\n')
      tmiddle <- ''
      for(i in 2:n-1){
        tmiddle <- paste0(tmiddle, 'pmax(', inputs[i], ', pmin(', inputs[i+1], ', ',feature, ')) +\n')
      }
      tend <- paste0('pmax(', inputs[n], ', ', feature, ')')
      paste0(tstart, tmiddle, tend)
    }
    
  } else if (formula_type=='if(feature=x,1,0)'){
    paste0('ifelse(', feature, "==", inputs[1], ",1,0)")
  } else if (formula_type=='if(feature<x,1,0)'){
    paste0('ifelse(', feature, "<", inputs[1], ",1,0)")
  } else if (formula_type=='if(feature>x,1,0)'){
    paste0('ifelse(', feature, ">", inputs[1], ",1,0)")
  } else if (formula_type=='between(feature,x,y)'){
    paste0('between(', feature, ',', inputs[1], ',', inputs[2],')')
  } else if (formula_type=='Spline (df)'){
    paste0('ns(', feature, ', ', inputs[1], ')')
  } else if (formula_type=='Spline (knots)'){
    inputs <- paste(inputs, collapse = ',')
    paste0('ns(', feature, ', knots = c(', inputs, '))')
  }
  
  
  
  
}
GlimmaR_coefficient_DT <- function(coefficients_dt){
  if(!is.null(coefficients_dt)){
    setDT(coefficients_dt)
    num_rows <- nrow(coefficients_dt)
    # shorten very long terms for display
    long_terms <- nchar(coefficients_dt$term)>55
    coefficients_dt[long_terms, term := paste0(substr(term,1,55), '...')]
    coefficients_dt %>%
      DT::datatable(rownames= TRUE,
                    extensions = 'Buttons',
                    selection = 'single',#HERE
                    options = list(pageLength = num_rows,
                                   dom = 'Bfrt',
                                   buttons =
                                     list('copy', list(
                                       extend = 'collection',
                                       buttons = list(list(extend='csv',filename = ''),
                                                      list(extend='excel',filename = ''),
                                                      list(extend='pdf',filename= '')),
                                       text = 'Download')
                                     ),
                                   scrollX = T,
                                   scrollY = 'calc(100vh - 338px)',
                                   searchHighlight=TRUE,
                                   columnDefs = list(list(width = '500px', targets =c(1))
                                   )
                                   
                    )) %>%
      DT::formatStyle(columns = 0:4, fontSize = '85%', lineHeight='10%') %>%
      DT::formatPercentage(c("p.value"), 1) %>%
      DT::formatSignif(c("estimate","std.error"), 4) %>%
      DT::formatStyle('p.value',
                      target = 'row',
                      backgroundColor = DT::styleInterval(c(0.01,0.05,0.1),
                                                          c(grDevices::rgb(210/255,255/255,210/255)
                                                            ,grDevices::rgb(240/255,255/255,220/255)
                                                            ,grDevices::rgb(255/255,255/255,220/255)
                                                            ,grDevices::rgb(255/255,220/255,220/255)
                                                          )
                      )
      ) |>
      formatStyle(1:ncol(coefficients_dt),"white-space"="nowrap")
    
  }
}
