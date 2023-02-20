#' navigateGlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput
mod_GlimmaR_navigate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        h3('GlimmaR model summary')
      ),
      # column(width = 3,
      #        fluidRow(
      #          column(
      #            width = 6,
      #            style = 'margin-right:0px; padding-right:0px',
      #            div(
      #              style = 'margin-bottom:-15px; margin-top:10px',
      #              radioGroupButtons(
      #                inputId = ns('tabulate_format'),
      #                label = NULL,
      #                choices = c('solo','long'),
      #                selected = 'solo',
      #                size = 'xs',
      #                justified = TRUE,
      #                )
      #              ),
      #            div(
      #              style = 'margin-bottom:0px; margin-top:0px',
      #              radioGroupButtons(
      #                inputId = ns('tabulate_scale'),
      #                label = NULL,
      #                choices = c('link','response'),
      #                selected = 'response',
      #                size = 'xs',
      #                justified = TRUE
      #                )
      #              )
      #            ),
      #          column(
      #            width = 6,
      # 
      #            )
      #          )
      #        ),
      column(width = 8,
             align = 'right',
             style = 'margin-top:16px; padding-right:16px; padding-bottom:0px',
             actionButton(
               inputId = ns('delete_model'),
               label = 'Delete',
               icon = icon("minus-circle"),
               class = 'btn-danger'
             ),
             actionButton(
               inputId = ns('make_active'),
               label = 'Make active',
               icon = icon("chevron-right")
             ),
             actionButton(
               inputId = ns('tabulate'),
               label = 'Tabulate',
               icon = icon("table")
             ),
             # actionButton(
             #   inputId = ns('generate_predictions'),
             #   label = 'Predict',
             #   icon = icon("chevron-right")
             # ),
             shinySaveButton(
               id = ns('save_model'),
               label = 'Save GLM',
               title = 'Save GLM model as .RDS',
               filename = "",
               filetype = list(txt="RDS"),
               icon = icon('upload'),
               style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
               viewtype = "detail"
             )
      )
    ),
    fluidRow(
      column(width = 12,
             align = 'right',
             DTOutput(ns('model_summary')
             )
      )
    ),
    fluidRow(
      column(
        width = 12,
        fluidRow(
          column(
            width = 5,
            h3('Importances')
          ),
          column(
            width = 7,
            align = 'right',
            div(
              style = 'margin-top:16px; margin-bottom:-16px',
              radioGroupButtons(
                inputId = ns('importance_type'),
                label = NULL,
                choices = c('Features','Terms'),
                selected = 'Features'
              )
            )
          )
        ),
        DTOutput(ns('importance_summary'))
      )
    )
  )
}
    
#' navigateGlimmaR Server Functions
#'
#' @importFrom DT styleEqual
#'
#' @noRd 
mod_GlimmaR_navigate_server <- function(id, d, response, weight, feature_spec, GlimmaR_models, GlimmaR_idx, tabulated_models){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(GlimmaR_models(), {
      # QUESTION - this still triggers when GlimmaR_idx() is changed
      # which is what I want to happen
      # but why? We are inside an observeEvent
      # or by creating output$model_summary does it remain "reactive" once created?
      output$model_summary <- renderDT({
        # model summary table
        dt <- GlimmaR_model_summary(GlimmaR_models())
        dt |>
          DT::datatable(rownames= FALSE,
                        extensions = 'Buttons',
                        selection=list(mode="multiple", target="row"),
                        options = list(pageLength = nrow(dt),
                                       #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                       dom = 'Bfrt',
                                       scrollX = T,
                                       scrollY = 'calc(20vh)',
                                       searchHighlight=TRUE,
                                       buttons =
                                         list('colvis', 'copy', list(
                                           extend = 'collection',
                                           buttons = list(list(extend='csv',filename = ''),
                                                          list(extend='excel',filename = ''),
                                                          list(extend='pdf',filename= '')),
                                           text = 'Download')
                                         )
                        )
          ) |>
          formatStyle(columns = colnames(dt), lineHeight='0%', fontSize = '14px') |>
          formatStyle(columns = 'name', target='row', backgroundColor = styleEqual(GlimmaR_idx(), rgb(100/255,180/255,220/255)))
      })
    })
    observeEvent(c(GlimmaR_models(), GlimmaR_idx()), {
      output$importance_summary <- renderDT({
        # model summary table
        if(!is.null(GlimmaR_models()) & !is.null(GlimmaR_idx())){
          if(input$importance_type=='Terms'){
            dt <- GlimmaR_models()[[GlimmaR_idx()]]$importances[[1]]
          } else {
            dt <- GlimmaR_models()[[GlimmaR_idx()]]$importances[[2]]
          }
        } else {
          dt <- data.table('V1'='No GLMs')
        }
        dt |>
          DT::datatable(rownames= FALSE,
                        extensions = 'Buttons',
                        selection='single',
                        options = list(pageLength = nrow(dt),
                                       #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                       dom = 'rt',
                                       scrollX = T,
                                       scrollY = 'calc(80vh - 380px)',
                                       searchHighlight=TRUE
                                       )
          ) |>
          formatRound('importance', 4) |>
          formatStyle(columns = colnames(dt), lineHeight='0%', fontSize = '14px')
      })
    })
    observeEvent(input$delete_model, {
      rows_selected <- input$model_summary_rows_selected
      new_list <- GlimmaR_models()[-rows_selected]
      GlimmaR_models(new_list)
    })
    observeEvent(input$make_active, {
      rows_selected <- input$model_summary_rows_selected
      if(!is.null(rows_selected)){
        if(length(rows_selected)>1){
          rows_selected <- rows_selected[1]
        }
      }
      GlimmaR_idx(names(GlimmaR_models())[rows_selected])
    })
    observeEvent(input$tabulate, {
      rows_selected <- input$model_summary_rows_selected
      if(!is.null(rows_selected)){
        if(length(rows_selected)>1){
          rows_selected <- rows_selected[1]
        }
        rows_selected
        if(length(GlimmaR_models())>0){
          # QUESTION - need to check for multiple selections
          model_name <- names(GlimmaR_models())[rows_selected]
          g <- GlimmaR_models()[[model_name]]
          if(!is.null(g)){
            base_risk <- get_base_risk(d(), feature_spec(), g$weight)
            #withProgress(message = 'GlimmaR', detail = 'tabulate', value = 0.5, {
              # OLD WAY
              # tabulated_model <- export_model(d()[g$pred_rows],
              #                                 g$glm,
              #                                 base_risk,
              #                                 FALSE,
              #                                 input$tabulate_scale,
              #                                 feature_spec(),
              #                                 input$tabulate_format,
              #                                 weight(),
              #                                 response(),
              #                                 g$predictions,
              #                                 model_name = g$name
              # )
              tabulated_model <- NULL
              # NEW LINES
              glm_term_summary <- summarise_glm_vars(g$glm)
              if(!is.null(glm_term_summary)){
                var_terms <- split(glm_term_summary, by ='vars', keep.by = FALSE)
              } else {
                var_terms <- NULL
              }
              tabulations <- prepare_glm_tabulations(d()[g$pred_rows], var_terms, feature_spec())
              tabulations <- predict_on_tabulations(tabulations, g$glm, var_terms)
              tabulations_adj <- adjust_base_levels(tabulations, feature_spec())
              predictions <- predict_tabulations(d()[g$pred_rows], tabulations_adj, feature_spec())
              predictions <- cbind(predictions, glm_fitted = g$predictions)
              # calculate the sd between glm prediction and tabulated prediction
              sd_error <- sd(link_function(predictions$tabulated_glm, g$link)-g$predictions)
              # save the tabulations into the GlimmaR model
              temp <- GlimmaR_models()
              temp[[model_name]][['tabulations']] <- tabulations_adj
              temp[[model_name]][['tabulated_predictions']] <- predictions
              temp[[model_name]][['tabulated_error']] <- sd_error
              GlimmaR_models(temp)
              # save into list
              temp <- tabulated_models()
              temp[[g$name]] <- tabulated_model
              tabulated_models(temp)
          }
        }
      }
    })
    observe({
      volumes <- c('working directory' = getwd(), 'home' = fs::path_home())
      shinyFileSave(input, 'save_model', roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$save_model)
      isolate({
        if (nrow(fileinfo) > 0) {
          # leave only part of glm_model object needed for prediction
          # otherwise file will be huge (it retains data used to fit model)
          stripped_model <- GlimmaR_models()[[GlimmaR_idx()]]$glm
          saveRDS(stripped_model, file = fileinfo$datapath, compress = TRUE)
          confirmSweetAlert(session = session,
                            type = 'success',
                            inputId = "temp",
                            title = 'GLM saved',
                            btn_labels = c('OK')
          )
        }
      })
    })
  })
}

GlimmaR_model_summary <- function(GlimmaR_models){
  # takes the key info from the BoostaR_models
  # and makes a summary data table
  rows <- lapply(GlimmaR_models, GlimmaR_model_summary_row)
  rbindlist(rows)
}
GlimmaR_model_summary_row <- function(GlimmaR_model){
  x <- data.table(name =  GlimmaR_model$name,
                  time = round(GlimmaR_model$time, 1),
                  data = GlimmaR_model$dataset_name,
                  train = GlimmaR_model$training_data,
                  response = GlimmaR_model$response,
                  weight = GlimmaR_model$weight,
                  obj = GlimmaR_model$objective,
                  terms = GlimmaR_model$num_terms,
                  dev = signif(GlimmaR_model$deviance, 6),
                  AIC = signif(GlimmaR_model$AIC, 6),
                  dispersion = signif(GlimmaR_model$dispersion, 6),
                  NAs = GlimmaR_model$count_NAs
  )
  return(x)
}

get_base_risk <- function(d, feature_spec, weight){
  base_risk <- d[1,]
  if(weight!='N'){
    base_risk[[weight]] <- 1
  }
  for (col in names(base_risk)){
    base_level <- feature_spec$base_level[feature_spec$feature==col]
    if(length(base_level)>0){
      if(is.na(base_level)){
        # do nothing
      } else {
        if(base_level!=''){
          if(inherits(base_risk[[col]],'factor')){
            base_risk[[col]] <- base_level
          } else {
            base_risk[[col]] <- as.numeric(base_level)
          }
        }
      }
    }
  }
  return(base_risk)
}
export_model <- function(dat, model, base_risk, collapse, type, feature_spec = NULL, format, weight, response, fitted = NULL, model_name = NULL){
  # function converts a glm model into rectangular tables
  # get a list of the factor in the base_risk and whether they are present in the model
  grouping_var <- NULL
  grouping_var_1 <- NULL
  grouping_var_2 <- NULL
  factors_in_base_risk <- as.data.frame(all.vars(model$formula))
  factors_in_base_risk[,1] <- as.character(factors_in_base_risk[,1])
  factors_in_base_risk$id <- 1:nrow(factors_in_base_risk)
  names(factors_in_base_risk)[1] <- 'var1'
  # get model terms and append on any offset terms (which can be found in variables)
  model_terms <- attr(stats::terms(model), 'term.labels')
  # get any terms with an offset, removing the weight term if present
  model_terms_offset <- as.character(attr(stats::terms(model), 'variables'))
  model_terms_offset <- model_terms_offset[grep('offset\\(', model_terms_offset)]
  model_terms <- c(model_terms, model_terms_offset)
  model_terms_formulae <- paste('y ~', model_terms)
  variable_list <- data.frame('var1'=character(0), 'var2'=character (0))
  if(length(model_terms)>0){
    for (i in 1:length(model_terms_formulae)){
      variables <- all.vars(stats::as.formula(model_terms_formulae[i]))
      if (length(variables)==2){
        # simple factor
        variable_list <- rbind(variable_list, data.frame('var1'=variables[2],'var2'=''))
      } else if (length(variables)==3) {
        # two way interaction
        variable_list <- rbind(variable_list, data.frame('var1'=variables[2],'var2'=variables[3]))
      }
    }
  }
  # make the list unique and remove the weight if present
  variable_list <- unique(variable_list)
  variable_list <- variable_list[variable_list$var1!=weight,]
  # create dataframes containing the factors and interactions present in the model
  factors <- variable_list[variable_list$var2=='',]
  interactions <- variable_list[variable_list$var2!='',]
  factors[,1] <- as.character(factors[,1])
  interactions[,1] <- as.character(interactions[,1])
  interactions[,2] <- as.character(interactions[,2])
  # shuffle order in factors to make closer to original formula
  factors  <- merge(factors, factors_in_base_risk, by = 'var1', all.x = TRUE)
  factors <- factors[order(factors$id),]
  factors$id <- NULL
  # create a list to contain the model tables, extra 1 for the base level
  number_of_tables <- 1 + nrow(factors) + nrow(interactions)
  table_list <- vector('list', number_of_tables)
  # get the base level for the model and put it into the first element of the list
  if(model$family$link == 'logit'){type = 'link'} # can't export response for binomial
  base_level <- stats::predict(model, newdata = base_risk, type = type)
  # if long format, need extra information for the base level row
  if(format %in% c('long')){
    if(weight=='N'){
      wts <- rep(1,nrow(model$data))
    } else {
      wts <- model$data[[weight]]
    }
    # dat_subset <- data.frame(weight = wts, observed = model$y, fitted = model$fitted.values)
    # summary_tot <- dat_subset %>%
    #   dplyr::summarise_at(c('observed','fitted','weight'), sum, na.rm = TRUE) %>%
    #   dplyr::mutate_at(c('observed','fitted'), ~./ weight)
    
    # DATA.TABLE replacement
    cols <- c('observed','fitted')
    summary_tot <- data.table(weight = wts, observed = model$y, fitted = model$fitted.values)
    summary_tot <- summary_tot[, lapply(.SD, sum), .SDcols = c(cols,'weight')]
    summary_tot[, observed :=  observed/weight]
    summary_tot[, fitted :=  fitted/weight]
    if(is.null(model_name)) model_name <- paste(sep='_', response, weight)
    # include an operation signifier in the factor1 column for the base level table
    # strip out the base level
    if(type == 'link' | model$family$link == 'identity'){
      operation <- 'sum'
    } else if (model$family$link == 'log'){
      operation <- 'product'
    } else {
      # any other link function won't have a sensible response so revert to link function
      # no matter what the user asked for
      operation <- 'sum'
      type <- 'link'
    }
    base_table <- cbind(model_name=model_name,
                        factor1='ratebook_operation',
                        factor1_levels=operation,
                        factor2='',
                        factor2_levels='',
                        summary_tot,
                        model_relativity = base_level)
  } else {
    base_table <- data.frame(model_relativity = base_level)
  }
  table_list[[1]] <- list(table = base_table, name = 'base level')
  # create one way tables
  num_1D_tables <- number_of_tables - nrow(interactions) - 1
  # cumulative base adjustment for long format tables
  # adjust relativities so they average to 1.000
  if(type == 'link' | model$family$link == 'identity'){
    long_format_adj <- 0.0
  } else if (model$family$link == 'log'){
    long_format_adj <- 1.0
  } else {
    long_format_adj <- 0.0
  }
  for (i in 1:num_1D_tables){
    if(num_1D_tables==0) next
    factor_name <- factors[i,1]
    expanded_factor <- return_banded_levels(factor_name, feature_spec, FALSE, dat)
    # could change next line to only involve features that are present in the model, rather than the whole row
    dummy_risks <- base_risk[rep(1,each=nrow(expanded_factor$expanded_factor)),]
    dummy_risks[,factor_name] <- expanded_factor$expanded_factor_for_prediction
    predictions <- stats::predict(model, newdata = dummy_risks, type = type)
    # strip out the base level
    if(type == 'link' | model$family$link == 'identity'){
      predictions <- predictions - base_level
    } else if (model$family$link == 'log'){
      predictions <- predictions / base_level
    } else {
      # any other link function won't have a sensible response so revert to link function
      predictions <- predictions - base_level
    }
    # rounding
    predictions <- signif(predictions, 6)
    predictions[abs(predictions)<0.0000001] <- 0
    export_table <- setDT(cbind(expanded_factor$expanded_factor, predictions))
    colnames(export_table)[1] <- factor_name
    colnames(export_table)[2] <- 'model_relativity'
    if(format %in% c('long')){
      # append on KPI columns for
      # number of observations, weight, fitted, observed, model relativity
      # first band the feature if it is numeric
      print(factor_name)
      if(is.numeric(dat[[factor_name]])){
        banding <- feature_spec$banding[feature_spec$feature==factor_name]
        banding_min <- feature_spec$min[feature_spec$feature==factor_name]
        banding_max <- feature_spec$max[feature_spec$feature==factor_name]
        if(!is.na(banding)){
          if(length(banding)>0){
            if(banding=='') banding <- numeric(0)
          }
        } else {
          banding <- numeric(0)
        }
        if(length(banding)==0){
          # guess the bandings
          banding <- banding_guesser(dat[[factor_name]])
          banding_min <- min(dat[[factor_name]], na.rm = TRUE)
          banding_max <- max(dat[[factor_name]], na.rm = TRUE)
          # round down or up as needed
          banding_min <- floor(banding_min/banding)*banding
          banding_max <- ceiling(banding_max/banding)*banding
        }
        # apply same rounding as used in table creation to ensure merge can proceed
        grouped <- floor(model$data[[factor_name]]/banding)*banding
        grouped <- round(pmax(banding_min, pmin(banding_max, grouped)),6)
      } else {
        # categorical feature - just use raw data column
        grouped <- model$data[[factor_name]]
      }
      # extract actual and fitted from glm_model
      #dat_subset <- data.frame(grouping_var = dat[['grouping_var']], weight = dat[[weight]], observed = glm_model()$y, fitted = glm_model()$fitted.values)
      if(weight=='N'){
        wts <- rep(1,nrow(model$data))
      } else {
        wts <- model$data[[weight]]
      }
      
      # data.table replacement
      dat_subset <- data.table(grouping_var = grouped, weight = wts, observed = model$y, fitted = model$fitted.values)
      summary <- dat_subset[, lapply(.SD, sum), by = grouping_var, .SDcols = c('observed', 'fitted', 'weight')]
      summary[, observed := observed/weight]
      summary[, fitted := fitted/weight]
      
      # merge summary onto the rating table
      export_table <- merge(export_table, summary, by.x = factor_name, by.y = 'grouping_var', all.x = TRUE)
      if(format=='long norm'){
        # adjust relativities so they average to 1.000
        if(type == 'link' | model$family$link == 'identity'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        } else if (model$family$link == 'log'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity / average_relativity
          long_format_adj <- long_format_adj * average_relativity
        } else {
          # any other link function won't have a sensible response so revert to link function
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        }
      }
      # swap around the position of the value column (2nd column) and rename
      export_table <- export_table[, c(1,3,4,5,2)]
      names(export_table)[5] <- 'model_relativity'
      # create a summary row at the bottom
      # this is a bit long-winded, but a useful check that the relativities average to 1.000
      export_table$weighted_relativity <- export_table$model_relativity * export_table$weight
      export_table$weighted_observed <- export_table$observed * export_table$weight
      export_table$weighted_fitted <- export_table$fitted * export_table$weight
      
      # data.table replacement
      cols <- c('weighted_observed','weighted_fitted','weight','weighted_relativity')
      summary_tot <- export_table[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols]
      cols <- c('weighted_observed','weighted_fitted','weighted_relativity')
      summary_tot[, (cols) := lapply(.SD, '/', weight), .SDcols = cols]
      summary_tot <- cbind('', summary_tot)
      export_table$weighted_relativity <- NULL
      export_table$weighted_observed <- NULL
      export_table$weighted_fitted <- NULL
      # bind the summary row to the overall table
      names(summary_tot) <- names(export_table)
      export_table <- rbind(summary_tot, export_table)
      # rename columns and create a blank column for factor 2
      names(export_table)[1] <- 'factor1_levels'
      export_table$factor2_levels <- ''
      # bind the factor name
      export_table <- cbind('factor1'=rep(factor_name, nrow(export_table)),
                            'factor2'=rep('',nrow(export_table)),
                            export_table)
      export_table <- export_table[, c(1,3,2,8,4,5,6,7)]
      # bind a model name row if one has been supplied
      if(!is.null(model_name)){
        export_table <- cbind('model_name'=rep(model_name, nrow(export_table)), export_table)
      }
    }
    table_list[[i+1]] <- list(table = export_table, name = factor_name)
  }
  # create two way tables
  for (i in 1:nrow(interactions)){
    if (nrow(interactions)==0) next
    factor1_name <- interactions[i,1]
    factor2_name <- interactions[i,2]
    expanded_factor_1 <- return_banded_levels(factor1_name, feature_spec, FALSE, dat)
    expanded_factor_2 <- return_banded_levels(factor2_name, feature_spec, FALSE, dat)
    expanded_factor_1$expanded_factor <- as.vector(expanded_factor_1$expanded_factor[[1]]) # expand.grid didn't like data frames, so make vector
    expanded_factor_2$expanded_factor <- as.vector(expanded_factor_2$expanded_factor[[1]])
    expanded_factor_1$expanded_factor_for_prediction <- as.vector(expanded_factor_1$expanded_factor_for_prediction[[1]])
    expanded_factor_2$expanded_factor_for_prediction <- as.vector(expanded_factor_2$expanded_factor_for_prediction[[1]])
    expanded_factor <- expand.grid(expanded_factor_1$expanded_factor, expanded_factor_2$expanded_factor)
    expanded_factor_for_prediction <- expand.grid(expanded_factor_1$expanded_factor_for_prediction, expanded_factor_2$expanded_factor_for_prediction)
    dummy_risks <- base_risk[rep(1,each=nrow(expanded_factor_for_prediction)),]
    dummy_risks1 <- dummy_risks
    dummy_risks2 <- dummy_risks
    dummy_risks[,factor1_name] <- expanded_factor[,1]
    dummy_risks[,factor2_name] <- expanded_factor[,2]
    dummy_risks1[,factor1_name] <- expanded_factor[,1]
    dummy_risks2[,factor2_name] <- expanded_factor[,2]
    predictions <- stats::predict(model, newdata = dummy_risks, type = type)
    predictions1 <- stats::predict(model, newdata = dummy_risks1, type = type)
    predictions2 <- stats::predict(model, newdata = dummy_risks2, type = type)
    if (collapse){
      if(type == 'link' | model$family$link == 'identity'){
        predictions <- predictions - base_level
      } else if (model$family$link == 'log'){
        predictions <- predictions / base_level
      } else {
        # any other link function won't have a sensible response so revert to link function
        predictions <- predictions - base_level
      }
    } else {
      if(type == 'link'| model$family$link == 'identity'){
        predictions <- predictions + base_level - predictions1 - predictions2
      } else if (model$family$link == 'log'){
        predictions <- (base_level * predictions) / (predictions1 * predictions2)
      } else {
        predictions <- predictions + base_level - predictions1 - predictions2
      }
    }
    # rounding and set numbers v close to zero to zero
    predictions <- signif(predictions, 6)
    predictions[abs(predictions)<0.0000001] <- 0
    export_table <- as.data.table(cbind(expanded_factor, predictions))
    colnames(export_table)[1] <- factor1_name
    colnames(export_table)[2] <- factor2_name
    colnames(export_table)[3] <- 'model_relativity'
    if(format=='solo'){
      # 2 dimensional table
      export_table <- dcast(export_table, export_table[[factor1_name]] ~ export_table[[factor2_name]], value.var = c('model_relativity'), fun.aggreagte=sum)
      colnames(export_table)[1] <- paste(factor1_name, '__X__', factor2_name, sep = '')
    } else if (format %in% c('long')){
      # long table with two columns for features
      # export_table is in correct format, we need to append KPI columns for
      # number of observations, weight, fitted, observed, model relativity
      # band feature 1 if numeric
      if(is.numeric(dat[[factor1_name]])){
        banding_min_1 <- feature_spec$min[feature_spec$feature==factor1_name]
        banding_max_1 <- feature_spec$max[feature_spec$feature==factor1_name]
        banding_1 <- feature_spec$banding[feature_spec$feature==factor1_name]
        if(length(banding_1)==0 | banding_1=='') banding_1 <- NA
        if(is.na(banding_1)){
          # guess the bandings
          banding_1 <- banding_guesser(dat[[factor1_name]])
          banding_min_1 <- min(dat[[factor1_name]], na.rm = TRUE)
          banding_max_1 <- max(dat[[factor1_name]], na.rm = TRUE)
        }
        dat$grouping_var_1 <- floor(model$data[[factor1_name]]/banding_1)*banding_1
        dat$grouping_var_1 <- pmax(banding_min_1, pmin(banding_max_1, dat$grouping_var_1))
      } else {
        # categorical feature - just use raw data column
        dat$grouping_var_1 <- model$data[[factor1_name]]
      }
      # band feature 2 if numeric
      if(is.numeric(dat[[factor2_name]])){
        banding_min_2 <- feature_spec$min[feature_spec$feature==factor2_name]
        banding_max_2 <- feature_spec$max[feature_spec$feature==factor2_name]
        banding_2 <- feature_spec$banding[feature_spec$feature==factor2_name]
        if(length(banding_2)==0 | banding_2=='') banding_2 <- NA
        if(is.na(banding_2)){
          # guess the bandings
          banding_2 <- banding_guesser(dat[[factor2_name]])
          banding_min_2 <- min(dat[[factor2_name]], na.rm = TRUE)
          banding_max_2 <- max(dat[[factor2_name]], na.rm = TRUE)
        }
        dat$grouping_var_2 <- floor(model$data[[factor2_name]]/banding_2)*banding_2
        dat$grouping_var_2 <- pmax(banding_min_2, pmin(banding_max_2, dat$grouping_var_2))
      } else {
        # categorical feature - just use raw data column
        dat$grouping_var_2 <- model$data[[factor2_name]]
      }
      # extract actual and fitted from glm_model
      if(weight=='N'){
        wts <- rep(1, nrow(dat))
      } else {
        wts <- dat[[weight]]
      }
      
      # data.table version
      observed <- NULL
      dat_subset <- data.table(grouping_var_1 = dat[['grouping_var_1']], grouping_var_2 = dat[['grouping_var_2']], weight = wts, observed = model$y, fitted = model$fitted.values)
      summary <- dat_subset[, lapply(.SD, sum), by = c('grouping_var_1','grouping_var_2'), .SDcols = c('observed', 'fitted', 'weight')]
      cols <- c('observed', 'fitted')
      summary <- summary[, (cols) := lapply(.SD, '/', weight), .SDcols = cols]
      
      # apply some rounding
      # this is probably not ideal
      if(is.numeric(summary$grouping_var_1)){
        summary$grouping_var_1 <- signif(summary$grouping_var_1,6)
      }
      if(is.numeric(summary$grouping_var_2)){
        summary$grouping_var_2 <- signif(summary$grouping_var_2,6)
      }
      # merge summary onto the rating table
      export_table <- merge(export_table, summary, by.x = c(factor1_name, factor2_name), by.y = c('grouping_var_1', 'grouping_var_2'), all.x = TRUE)
      # adjust relativities so they average to 1.000
      if(format=='long norm'){
        if(type == 'link' | model$family$link == 'identity'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        } else if (model$family$link == 'log'){
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity / average_relativity
          long_format_adj <- long_format_adj * average_relativity
        } else {
          # any other link function won't have a sensible response so revert to link function
          average_relativity <- sum(export_table$model_relativity*export_table$weight, na.rm =TRUE) / sum(export_table$weight, na.rm = TRUE)
          export_table$model_relativity <- export_table$model_relativity - average_relativity
          long_format_adj <- long_format_adj + average_relativity
        }
      }
      # swap around the position of the value column (2nd column) and rename
      export_table <- export_table[, c(1,2,4,5,6,3)]
      names(export_table)[1] <- 'factor1_levels'
      names(export_table)[2] <- 'factor2_levels'
      names(export_table)[6] <- 'model_relativity'
      # create a summary row at the bottom
      # this is a bit long-winded, but a useful check that the relativities average to 1.000
      export_table$weighted_relativity <- export_table$model_relativity * export_table$weight
      export_table$weighted_observed <- export_table$observed * export_table$weight
      export_table$weighted_fitted <- export_table$fitted * export_table$weight
      # summary_tot <- export_table %>%
      #   dplyr::summarise_at(c('weighted_observed','weighted_fitted','weight','weighted_relativity'), sum, na.rm = TRUE) %>%
      #   dplyr::mutate_at(c('weighted_observed','weighted_fitted','weighted_relativity'), ~./ weight)
      
      # data.table version
      cols <- c('weighted_observed','weighted_fitted','weighted_relativity')
      summary_tot <- export_table[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(cols, 'weight')]
      summary_tot[, (cols) := lapply(.SD, '/', weight), .SDcols = cols]
      setcolorder(summary_tot, c('weighted_observed','weighted_fitted','weight','weighted_relativity'))
      summary_tot <- cbind('','', summary_tot)
      export_table$weighted_relativity <- NULL
      export_table$weighted_observed <- NULL
      export_table$weighted_fitted <- NULL
      # bind the summary row to the overall table
      names(summary_tot) <- names(export_table)
      export_table <- rbind(summary_tot, export_table)
      # bind the factor name
      export_table <- cbind('factor1'=rep(factor1_name, nrow(export_table)),
                            'factor2'=rep(factor2_name, nrow(export_table)),
                            export_table)
      # reorder columns yet again
      export_table <- export_table[, c(1,3,2,4,5,6,7,8)]
      # bind a model name row if one has been supplied
      if(!is.null(model_name)){
        export_table <- cbind('model_name'=rep(model_name, nrow(export_table)), export_table)
      }
    }
    table_list[[1 + nrow(factors) + i]] <- list(table = export_table, name = paste(sep = '__X__', factor1_name, factor2_name))
  }
  # adjust the long format base level if neccesary
  # adjust relativities so they average to 1.000
  if(format %in% c('long')){
    if(type == 'link' | model$family$link == 'identity'){
      table_list[[1]]$table$model_relativity <- table_list[[1]]$table$model_relativity + long_format_adj
    } else if (model$family$link == 'log'){
      table_list[[1]]$table$model_relativity <- table_list[[1]]$table$model_relativity * long_format_adj
    } else {
      # any other link function won't have a sensible response so revert to link function
      table_list[[1]]$table$model_relativity <-table_list[[1]]$table$model_relativity + long_format_adj
    }
  }
  # return list from function, include slot for the format of the tables
  table_names <- sapply(table_list, '[[', 'name')
  names(table_list) <- table_names
  table_list$format <- format
  return(table_list)
}
return_banded_levels <- function(factor_name, feature_spec, use_mid_point_column, dat){
  
  # if the feature spec contains banding/min/max then use those
  # otherwise make a guess as to a good banding level
  banding_min <- NA
  banding_max <- NA
  banding <- NA
  use_mid_point <- FALSE
  if(!is.null(feature_spec) & is.numeric(dat[[factor_name]]) & ('banding' %in% names(feature_spec))){
    banding_min <- feature_spec$min[feature_spec$feature==factor_name]
    banding_max <- feature_spec$max[feature_spec$feature==factor_name]
    banding <- feature_spec$banding[feature_spec$feature==factor_name]
    use_mid_point <- feature_spec$use_mid_point[feature_spec$feature==factor_name]
    if(length(banding_min)==0) banding_min <- NA
    if(length(banding_max)==0) banding_max <- NA
    if(length(banding)==0) banding <- NA
    if(length(use_mid_point)==0){
      use_mid_point <- FALSE
    } else if(is.na(use_mid_point)){
      use_mid_point <- FALSE
    } else if(inherits(use_mid_point,'character')){
      if(use_mid_point=='TRUE') use_mid_point <- TRUE
    }
    
    # override use_mid_point if
    if(use_mid_point_column=='No'){
      use_mid_point <- FALSE
    }
    
  }
  
  # set the banding level if not using feature spec or banding level not found
  if(is.null(feature_spec) | is.na(banding) | banding==''){
    # guess the banding level if numeric
    if(is.numeric(dat[[factor_name]])){
      banding <- banding_guesser(dat[[factor_name]])
    }
  }
  
  # set the min if not using feature spec or banding level not found
  if(is.null(feature_spec) | is.na(banding_min) | banding_min==''){
    if(is.numeric(dat[[factor_name]])){
      banding_min <- min(dat[[factor_name]], na.rm = TRUE)
    }
  }
  
  # set the max if not using feature spec or banding level not found
  if(is.null(feature_spec) | is.na(banding_max) | banding_max==''){
    if(is.numeric(dat[[factor_name]])){
      banding_max <- max(dat[[factor_name]], na.rm = TRUE)
    }
  }
  
  # create levels we want to predict on
  if(is.numeric(dat[[factor_name]])){
    # in case feature spec saved as characters
    banding <- as.numeric(banding)
    banding_min <- as.numeric(banding_min)
    banding_max <- as.numeric(banding_max)
    # simple sequence - code below makes sure end points are correct
    banding_min <- floor(banding_min/banding)*banding
    banding_max_temp <- floor(banding_max/banding)*banding
    if(banding_max_temp==banding_max){
      banding_max <- banding_max_temp
    } else {
      banding_max <- banding_max_temp + banding
    }
    
    # expand out the feature
    # apply some rounding to make merging on data easier later
    expanded_factor <- round(seq(banding_min, banding_max, banding),6)
    expanded_factor <- as.data.frame(expanded_factor)
    
    # if use_mid_point is TRUE then predict on mid-point not start of band
    if(use_mid_point){
      expanded_factor_for_prediction <- expanded_factor + banding/2
    } else {
      expanded_factor_for_prediction <- expanded_factor
    }
    
  } else {
    # every level in the dataset for non-numeric features
    expanded_factor <- as.data.frame(levels(dat[[factor_name]]))
    expanded_factor_for_prediction <- expanded_factor
  }
  
  list(expanded_factor = expanded_factor, expanded_factor_for_prediction = expanded_factor_for_prediction)
  
}

#' @import stats
summarise_glm_vars <- function(glm){
  # extract the terms
  term.labels <- attr(terms.formula(glm$formula), "term.labels")
  # extract the offsets
  all_vars <- as.character(attr(stats::terms(glm), 'variables'))
  offsets <- all_vars[grep('offset\\(', all_vars)]
  # add them to the term.labels
  term.labels <- c(term.labels, offsets)
  if(length(term.labels)>0){
    term.labels.form <- paste0('~', term.labels)
    # extract the vars in each term
    vars <- term.labels.form |>
      lapply(formula) |>              # make into a formula
      lapply(all.vars) |>             # extract vars from formula
      lapply(sort) |>                 # alphabetical sort
      lapply(paste0, collapse = '|')  # collapse into string
    # return data.table
    data.table(
      terms = term.labels,
      vars = unlist(vars)
    )
  }
}
uncentered_terms_new <- function(object, newdata, terms, na.action = na.pass, ...) {
  offset_term <- ifelse(length(grep('offset(', terms, fixed = TRUE))>0,T,F)
  if(offset_term){
    # evaluate the offset directly
    # QUESTION FOR GHH - need to split out offset terms
    begin <- min(unlist(gregexpr('(', terms, fixed = TRUE))) + 1
    end <- max(unlist(gregexpr(')', terms, fixed = TRUE))) - 1
    form <- substr(terms,begin,end)
    predictor <- newdata[, eval(parse(text = form))]
    result <- data.table(predictor)
    setnames(result, terms)
  } else {
    # use model matrix
    # get all the terms
    tt <- terms(object)
    n_terms <- length(attr(tt, 'term.labels'))
    
    # keep only the terms we are going to use
    component_terms <- terms[grep(':', terms)]
    if(length(component_terms)>0){
      component_terms <- unique(unlist(strsplit(component_terms, ':')))
    }
    component_terms <- c(component_terms, terms)
    component_terms <- intersect(component_terms, attr(tt, 'term.labels'))
    keep <- which(attr(tt, 'term.labels') %in% component_terms)
    drop_terms <- setdiff(1:n_terms, keep)
    if(length(drop_terms)>0){
      tt <- drop.terms2(tt, drop_terms)
    }
    tt <- delete.response(tt)
    predvars <- attr(tt, "predvars")
    vars <- attr(tt, 'variables')
    if (!is.null(predvars)) {
      predvars_char <- as.character(predvars)
      vars_char <- as.character(vars)
      offset_loc <- grep('offset(', predvars_char, fixed = TRUE)
      if(length(offset_loc)>0){
        attr(tt, "predvars") <- predvars[-offset_loc]
      }
      offset_loc <- grep('offset(', vars_char, fixed = TRUE)
      if(length(offset_loc)>0){
        attr(tt, "variables") <- vars[-offset_loc]
      }
    }
    
    n_terms <- length(terms)
    attr(tt, '.Environment') <- environment() # makes splines package available as loaded by lucidum
    m <- model.frame(tt, newdata, na.action = na.action, xlev = object$xlevels, drop.unused.levels = TRUE)
    if (!is.null(cl <- attr(tt, "dataClasses"))) .checkMFClasses(cl, m)
    new_form <- as.formula(paste0('~',paste0(terms, collapse = '+')))
    X <- model.matrix(new_form, m, contrasts.arg = object$contrasts)
    aa <- attr(X, "assign")
    
    hasintercept <- attr(tt, "intercept") > 0L
    if (hasintercept){
      ll <- c("(Intercept)", terms)
      keep <- c(1,keep+1)
      n_terms <- n_terms + 1
    }
    aaa <- factor(aa, labels = ll)
    asgn <- split(order(aa), aaa)
    
    # keep the coefficients we need
    coeff_names <- dimnames(X)[[2]]
    # NEW- populate beta
    beta <- vector('list', length = length(coeff_names))
    names(beta) <- coeff_names
    beta[1:length(beta)] <- 0
    for(i in 1:length(beta)){
      if(names(beta)[i] %in% names(object$coefficients)){
        beta[i] <- object$coefficients[names(beta)[i]]
      }
    }
    beta <- unlist(beta)
    beta[is.na(beta)] <- 0 # glm returns NA for coefficients it drops from the model - zero ensures no contribution
    predictor <- matrix(ncol = n_terms, nrow = NROW(X))
    for(i in seq.int(1L, n_terms, length.out = n_terms)) {
      ii <- asgn[[i]]
      predictor[, i] <- X[, ii, drop = FALSE] %*% beta[ii]
    }
    result <- data.table(predictor)
    setnames(result, levels(aaa))
  }
  return(result)
}
uncentered_vars <- function(uncentered_terms, var_terms){
  uc_vars <- setNames(data.table(matrix(nrow = nrow(uncentered_terms), ncol = length(var_terms))), names(var_terms))
  for(i in 1:length(var_terms)){
    cols <- var_terms[[i]]$terms
    if(all(cols %in% names(uncentered_terms))){
      uc_vars[,i] <- uncentered_terms[,rowSums(.SD),.SDcols=cols]
    }
  }
  uc_vars
}
feature_banding <- function(d, feature_col, feature_spec){
  if(inherits(d[[feature_col]], 'factor')){
    levels(d[[feature_col]])
  } else if (inherits(d[[feature_col]], c('integer','numeric'))){
    f_min <- feature_spec[feature==feature_col,'min'][[1]]
    f_max <- feature_spec[feature==feature_col,'max'][[1]]
    f_banding <- feature_spec[feature==feature_col,'banding'][[1]]
    # in case the feature spec is character cols
    f_min <- as.numeric(f_min)
    f_max <- as.numeric(f_max)
    f_banding <- as.numeric(f_banding)
    # check for NAs or empties
    # and derive banding from raw data if so
    got_banding <- T
    if(any(length(f_min)==0,length(f_max)==0,length(f_banding)==0)) got_banding <- F
    if(any(is.na(f_min),is.na(f_max),is.na(f_banding))) got_banding <- F
    if(!got_banding){
      f_min <- min(d[[feature_col]])
      f_max <- max(d[[feature_col]])
      f_banding <- banding_guesser(d[[feature_col]])
      f_min <- floor(f_min/f_banding)*f_banding
      f_max <- floor(f_max/f_banding)*f_banding + f_banding
    }
    seq(from = f_min, to = f_max, by = f_banding)
  }
}
position <- function(input_attr, test_attr){
  # calculate the positions of each element of input_attr within test_attr
  input_char <- as.character(input_attr)
  test_attr <- as.character(test_attr)
  which(test_attr %in% input_char)
}
drop.terms2 <- function (termobj, dropx = NULL, keep.response = FALSE){
  # copy of drop.terms from glm to correctly handle predvars and dataClasses
  if (is.null(dropx)) 
    termobj
  else {
    if (!inherits(termobj, "terms")) 
      stop(gettextf("'termobj' must be a object of class %s", dQuote("terms")), domain = NA)
    newformula <- reformulate(attr(termobj, "term.labels")[-dropx], 
                              response = if (keep.response) 
                                termobj[[2L]], intercept = attr(termobj, "intercept"), 
                              env = environment(termobj))
    result <- terms(newformula, specials = names(attr(termobj, "specials")))
    response <- attr(termobj, "response")
    dropOpt <- if (response && !keep.response) 
      c(response, dropx + length(response))
    else dropx + max(response)
    loc <- position(attr(termobj, 'term.labels')[dropx], attr(termobj, 'var'))
    # extract the individual term labels in termobj
    # stripping down interaction terms (that use ":")
    tl <- attr(termobj, 'term.labels')[-dropx]
    int_tl <- tl[grep(':', tl)]
    if(length(int_tl)>0){
      int_tl <- unique(unlist(strsplit(int_tl, ':')))
    }
    tl <- unique(c(tl,int_tl))
    
    # find where the term labels are in attr(termobj, 'var')
    # and keep only those in predvars and dataClasses
    keep_loc <- position(tl, attr(termobj, 'var'))
    if (!is.null(predvars <- attr(termobj, "predvars"))) {
      attr(result, "predvars") <- predvars[c(1,keep_loc)]
    }
    if (!is.null(dataClasses <- attr(termobj, "dataClasses"))) {
      attr(result, "dataClasses") <- dataClasses[(keep_loc-1)] 
    }
    result
  }
}
prepare_glm_tabulations <- function(dt, var_terms, feature_spec){
  tabulations <- list()
  tabulations[['base']] <- data.table(base=0L)
  if(!is.null(var_terms)){
    for(i in 1:length(var_terms)){
      # get the vars and terms
      terms <- var_terms[[i]][[1]]
      vars <- unlist(strsplit(names(var_terms)[[i]], '|', fixed = TRUE))
      banded_vars <- list()
      for(j in 1:length(vars)){
        banded_vars[[vars[j]]] <- feature_banding(dt, vars[j], feature_spec)
      }
      # create data.table with all combinations of banded_vars
      expanded_vars <- expand.grid(banded_vars)
      tabulations[[names(var_terms)[i]]] <- setDT(expanded_vars)
    }
  }
  return(tabulations)
}
predict_on_tabulations <- function(tabulations, glm, var_terms){
  # first table is the base level
  # pick up the glm intercept
  if('(Intercept)' %in% names(glm$coefficients)){
    tabulations[['base']][, base:= glm$coefficients[['(Intercept)']]]
  } else {
    tabulations[['base']][, base := 0]
  }
  if(!is.null(var_terms)){
    for(i in 2:length(tabulations)){
      predictions <- uncentered_terms_new(glm, newdata = tabulations[[i]], terms = var_terms[[i-1]]$terms)
      if('(Intercept)' %in% names(predictions)){
        predictions[, `(Intercept)`:=NULL]
      }
      total <- rowSums(predictions)
      tabulations[[i]] <- cbind(tabulations[[i]], predictions, tabulated_glm = total)
    }
  }
  return(tabulations)
}
adjust_base_levels <- function(tabulations, feature_spec){
  cumulative_adjustment <- 0
  if(length(tabulations)>1){
    for(i in 2:length(tabulations)){
      # get the base levels for the table
      vars <- unlist(strsplit(names(tabulations)[i], '|', fixed = TRUE))
      base_levels <- tabulations[[i]][1,1:length(vars)]
      all_present <- TRUE
      for(j in 1:length(vars)){
        b <- feature_spec[feature==vars[j], base_level]
        if(!inherits(tabulations[[i]][[j]], c('character','factor'))){
          b <- as.numeric(b)
        }
        if(shiny::isTruthy(b)){
          base_levels[[j]][1] <- b
        } else {
          base_levels[[j]][1] <- NA
          all_present <- FALSE
        }
      }
      # identify the base level row in tabulations
      if(all_present){
        setkeyv(tabulations[[i]], vars)
        setkeyv(base_levels, vars)
        adjustment <- tabulations[[i]][base_levels]$tabulated_glm
      } else {
        adjustment <- 0
      }
      tabulations[[i]][['tabulated_glm']] <- tabulations[[i]][['tabulated_glm']] - adjustment
      cumulative_adjustment <- cumulative_adjustment + adjustment
    }
  }
  # adjust the base level
  tabulations[['base']] <- tabulations[['base']] + cumulative_adjustment
  return(tabulations)
}
band_var_with_feature_spec <- function(x, var_name, feature_spec){
  if(inherits(x, c('character','factor'))){
    x
  } else {
    got_banding <- T
    if(is.null(feature_spec)){
      got_banding <- F
    } else {
      f_min <- feature_spec$min[feature_spec$feature==var_name]
      f_max <- feature_spec$max[feature_spec$feature==var_name]
      f_banding <- feature_spec$banding[feature_spec$feature==var_name]
      f_min <- as.numeric(f_min)
      f_max <- as.numeric(f_max)
      f_banding <- as.numeric(f_banding)
      if(any(length(f_min)==0,length(f_max)==0,length(f_banding)==0)) got_banding <- F
      if(any(is.na(f_min),is.na(f_max),is.na(f_banding))) got_banding <- F
    }
    if(!got_banding){
      # derive banding from raw data
      f_min <- min(x)
      f_max <- max(x)
      f_banding <- banding_guesser(x)
      f_min <- floor(f_min/f_banding)*f_banding
      f_max <- floor(f_max/f_banding)*f_banding + f_banding
    }
    # band and apply min/max
    x <- floor(x/f_banding)*f_banding
    x <- pmax(f_min, pmin(f_max, x))
  }
  return(x)
}
predict_tabulations <- function(dt, tabulations, feature_spec){
  # matrix to hold the predictions for each table in tablulations
  # and each row in dt
  predictions <- matrix(data=NA, nrow=nrow(dt),ncol=length(tabulations))
  # base level is the same for every row
  predictions[,1] <- tabulations[[1]][['base']]
  if(length(tabulations)>1){
    # loop over the remaining tables
    withProgress(message = 'GlimmaR', detail = 'tabulating', {
      for(i in 2:length(tabulations)){
        setProgress(
          value = i/length(tabulations),
          detail = paste0('tabulating ',
                          names(tabulations)[[i]], # name of the table
                          ' (',
                          prod(dim(tabulations[[i]])), # number of cells in the table
                          ' cells)')
        )
        vars <- unlist(strsplit(names(tabulations)[[i]], '|', fixed = TRUE))
        dt_var_cols <- dt[, ..vars]
        # band numerical columns
        for(v in vars){
          x_banded <- band_var_with_feature_spec(dt_var_cols[[v]],v,feature_spec)
          dt_var_cols[, (v):= x_banded]
        }
        # create index col so can reorder after merge
        dt_var_cols[, row_idx_temp := 1:.N]
        # now values are banded, we can
        # merge tabulation onto dt_var_cols
        setkeyv(dt_var_cols, vars)
        setkeyv(tabulations[[i]], vars)
        merged <- tabulations[[i]][dt_var_cols]
        setorder(merged, 'row_idx_temp')
        predictions[,i] <- merged$tabulated_glm
      }
    })
  }
  predictions_dt <- data.table(predictions)
  setnames(predictions_dt, names(tabulations))
  predictions_dt[, tabulated_glm := rowSums(predictions_dt)]
  return(predictions_dt)
}
extract_offsets <- function(glm){
  tt <- glm$terms
  offset_idx <- attr(tt, 'offset')
  as.character(attr(glm$terms, 'predvar')[offset_idx+1])
}
predict_offsets <- function(dt, offsets){
  results <- matrix(data=NA, nrow=nrow(dt), ncol = length(offsets))
  for(i in 1:length(offsets)){
    results[,i] <- dt[, eval(parse(text = offsets[i]))]
  }
  results <- data.table(results)
  setnames(results, offsets)
  return(results)
}
offset_importances <- function(dt, offsets){
  results <- data.table(names = offsets, importance = 0)
  for(i in 1:length(offsets)){
    x <- dt[, eval(parse(text = offsets[i]))]
    results[i,importance := sd(x)]
  }
  return(results)
}
calc_terms_importances <- function(glm){
  offsets <- extract_offsets(glm)
  terms_predictions <- predict(glm, type = 'terms')
  glm_term_summary <- summarise_glm_vars(glm)
  if(!is.null(glm_term_summary)){
    var_terms <- split(glm_term_summary, by ='vars', keep.by = FALSE)
  } else {
    var_terms <- NULL
  }
  if(length(offsets)>0){
    offsets_predictions <- predict_offsets(glm$data, offsets)
    terms_offset_predictions <- cbind(terms_predictions, offsets_predictions)
  } else {
    terms_offset_predictions <- data.table(terms_predictions)
  }
  if(ncol(terms_offset_predictions)>0){
    terms_importances <- apply(terms_offset_predictions, 2, sd)
    terms_importances <- data.table(names = names(terms_importances), importance = terms_importances)[order(-importance)]
    var_importances <- uncentered_vars(terms_offset_predictions, var_terms)
    var_importances <- apply(var_importances, 2, sd)
    var_importances <- data.table(names = names(var_importances), importance = var_importances)[order(-importance)]
  } else {
    terms_importances <- NA
    var_importances <- NA
  }
  return(list(terms=terms_importances, vars = var_importances))
}