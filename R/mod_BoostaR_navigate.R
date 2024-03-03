#' navigateBoostaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DiagrammeR grVizOutput
#' @importFrom DT DTOutput
mod_BoostaR_navigate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        h3('BoostaR model summary')
      ),
      column(width = 6,
             align = 'right',
             style = 'margin-top:16px; padding-right:16px; padding-bottom:0px',
             actionButton(
               inputId = ns('BoostaR_delete_model'),
               label = 'Delete',
               icon = icon("minus-circle"),
               class = 'btn-danger'
             ),
             actionButton(
               inputId = ns('BoostaR_make_active'),
               label = 'Make active',
               icon = icon("chevron-right")
               ),
             actionButton(
               inputId = ns('tabulate'),
               label = 'Tabulate',
               icon = icon("table")
             ),
             # actionButton(
             #   inputId = ns('BoostaR_generate_predictions'),
             #   label = 'Predict',
             #   icon = icon("chevron-right")
             #   ),
             shinySaveButton(
               id = ns('BoostaR_save_model'),
               label = 'Save GBM',
               title = 'Save GBM model as .txt',
               filename = "",
               filetype = list(txt="txt"),
               icon = icon('upload'),
               style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
               viewtype = "detail"
               ),
             shinySaveButton(
               id = ns('BoostaR_save_BoostaR_models'),
               label = 'Save all BoostaR models',
               title = 'Save all BoostaR models as .rds',
               filename = "",
               filetype = list(txt="rds"),
               icon = icon('upload'),
               style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
               viewtype = "detail"
               )
             )
    ),
    fluidRow(
      column(width = 12,
             align = 'right',
             DTOutput(ns('BoostaR_model_summary')
             )
      )
    ),
    fluidRow(
      column(
        width = 4,
        fluidRow(
          column(
            width = 12,
            h3('Model details'),
          )
        ),
        DTOutput(ns('BoostaR_detailed_model_summary'))
      ),
      column(
        width = 8,
        fluidRow(
          column(
            width = 5,
            h3('Gain summary')
          ),
          column(
            width = 5,
            align = 'right',
            div(
              style = 'margin-top:16px; margin-bottom:-16px',
              textInput(
                ns('BoostaR_search_gain_table'),
                label = NULL,
                width = '100%',
                placeholder = 'highlight feature'
              )
            )
          ),
          column(
            width = 2,
            align = 'right',
            div(
              style = 'margin-top:16px; margin-bottom:-16px',
              actionButton(
                inputId = ns('BoostaR_gain_table_goto_ChartaR'),
                label = tags$img(src='www/SHAP.png', height='26px', width='26px'),
                style = 'padding:3px 5px 3px 5px'
              ),
              tippy_this(ns('BoostaR_gain_table_goto_ChartaR'), placement = 'bottom', tooltip = tippy_text('Show selected feature(s) in ChartaR',12))
            )
          )
        ),
        DTOutput(ns('BoostaR_gain_summary'))
      )
    )
  )
}
    
#' navigateBoostaR Server Functions
#'
#' @noRd 
#' 
#' 
#' @importFrom shiny updateSliderInput
#' @importFrom DiagrammeR renderGrViz render_graph
#' @importFrom DT formatRound formatPercentage formatStyle
#' @importFrom stringr str_count
#' @importFrom lightgbm lgb.save
#' 
#' 
mod_BoostaR_navigate_server <- function(id, d, BoostaR_models, BoostaR_idx, feature_spec, crosstab_selector, tabulated_models){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(BoostaR_models(), {
      if(length(BoostaR_models())==0){
        model_summary <- NULL
      } else {
        model_summary <- make_BoostaR_model_summary(BoostaR_models())
      }
      if(is.null(model_summary)){
        model_summary <- data.table(name='No GBMs built')
      }
      if(!is.null(model_summary)){
        output$BoostaR_model_summary <- DT::renderDT({
          # model summary table
          model_summary |>
            DT::datatable(rownames= FALSE,
                          extensions = 'Buttons',
                          selection=list(mode="multiple", target="row"),
                          options = list(pageLength = nrow(model_summary),
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
            formatStyle(columns = colnames(model_summary), lineHeight='0%', fontSize = '14px') |>
            formatStyle(columns = 'name', target='row', backgroundColor = styleEqual(BoostaR_idx(), rgb(100/255,180/255,220/255)))
        })
      }
    })
    observeEvent(BoostaR_idx(), {
      if(BoostaR_idx()=='No GBMs'){
        detailed_summary <- NULL
      } else {
        detailed_summary <- make_BoostaR_detailed_summary(BoostaR_models()[[BoostaR_idx()]])
      }
      if(is.null(detailed_summary)){
        detailed_summary <- data.table(`No GBMs`='')
      }
      output$BoostaR_detailed_model_summary <- DT::renderDT({
        # model summary table
        detailed_summary |>
          DT::datatable(rownames= FALSE,
                        selection=list(mode="multiple", target="row"),
                        options = list(pageLength = nrow(detailed_summary),
                                       #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                       dom = 'rt',
                                       scrollX = T,
                                       scrollY = 'calc(80vh - 380px)',
                                       searchHighlight=TRUE
                        )
          ) |>
          DT::formatStyle(columns = colnames(detailed_summary), lineHeight='0%', fontSize = '14px')
      })
    })
    # QUESTION - this seems to work
    # but should I wrap in an observeEvent?
    output$BoostaR_gain_summary <- DT::renderDataTable({
      # gain summary
      if(length(BoostaR_models())>0){
        model_index <- BoostaR_idx()
        if(!is.null(model_index)){
          gain_summary <- BoostaR_models()[[model_index]]$gain_summary
          original_n_rows <- nrow(gain_summary)
          # filter rows
          if(input$BoostaR_search_gain_table!=''){
            keep_rows <- grepl(input$BoostaR_search_gain_table, gain_summary$tree_features)
            gain_summary <- gain_summary[keep_rows]
            # update the tree selector
            tree_table <- BoostaR_models()[[model_index]]$tree_table
            match_rows <- grepl(input$BoostaR_search_gain_table,tree_table$split_feature)
            if(all(!match_rows)){
              first_tree <- 0
            } else {
              first_tree <- tree_table[which.max(match_rows)][['tree_index']]
            }
          }
          # limit number of rows
          n_rows <- pmin(2000, nrow(gain_summary))
          # display how many rows
          col_text <- paste0('tree_features (',n_rows,' of ',original_n_rows,')')
          names(gain_summary)[1] <- col_text
          gain_summary[1:n_rows,] |>
            datatable(rownames= FALSE,
                      selection = 'single',
                      options = list(pageLength = n_rows,
                                     #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                     dom = 'rt',
                                     scrollX = T,
                                     scrollY = 'calc(80vh - 380px)'
                      )
            ) |>
            formatRound('gain', 0) |>
            formatPercentage('%', 1) |>
            formatStyle(columns = colnames(gain_summary), fontSize = '14px', lineHeight='0%')
        }
      }
    })
    observeEvent(input$BoostaR_delete_model, {
      rows_selected <- input$BoostaR_model_summary_rows_selected
      if(!is.null(rows_selected)){
        new_list <- BoostaR_models()[-rows_selected]
        BoostaR_models(new_list)
      } else {
        confirmSweetAlert(session = session,
                          type = 'error',
                          inputId = "temp",
                          title = 'No GBMs selected',
                          btn_labels = c('OK')
        )
      }
    })
    observeEvent(input$BoostaR_make_active, {
      rows_selected <- input$BoostaR_model_summary_rows_selected
      if(!is.null(rows_selected)){
        if(length(rows_selected)>1){
          rows_selected <- rows_selected[1]
        }
        BoostaR_idx(names(BoostaR_models())[rows_selected])
      } else {
        confirmSweetAlert(session = session,
                          type = 'error',
                          inputId = "temp",
                          title = 'No GBM selected',
                          btn_labels = c('OK')
        )
      }

    })
    observeEvent(input$tabulate, {
      rows_selected <- input$BoostaR_model_summary_rows_selected
      if(!is.null(rows_selected)){
        if(length(rows_selected)>1){
          rows_selected <- rows_selected[1]
        }
      }
      if(length(BoostaR_models())>0){
        model_name <- names(BoostaR_models())[rows_selected]
        B <- BoostaR_models()[[model_name]]
        base_risk <- get_base_risk(d(), feature_spec(), B$weight)
        tabulations <- lgbm.convert.to.tables(d(), B, base_risk, feature_spec())
        if(inherits(tabulations, 'character')){
          # return warning message that there are too many rows in one of the tables
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "build_error",
                            title = 'Error',
                            text = tabulations,
                            btn_labels = c('OK'))
        } else {
          # include the tabulations in the BoostaR_model list
          tabulations_adj <- adjust_base_levels_lgbm(tabulations, feature_spec())
          predictions <- predict_tabulations_lgbm(d()[B$pred_rows], tabulations_adj, feature_spec())
          predictions <- cbind(predictions, lgbm_fitted = B$predictions)
          # calculate the sd between glm prediction and tabulated prediction
          sd_error <- sd(link_function(predictions$tabulated_lgbm, B$link)-B$predictions)
          # save the tabulations into the BoostaR model
          temp <- BoostaR_models()
          temp[[model_name]][['tabulations']] <- tabulations_adj
          temp[[model_name]][['tabulated_predictions']] <- predictions
          temp[[model_name]][['tabulated_error']] <- sd_error
          BoostaR_models(temp)
        }

      }
    })
    observeEvent(input$BoostaR_gain_table_goto_ChartaR, {
      if(length(BoostaR_models())>0 & !is.null(BoostaR_idx())){
        b <- BoostaR_models()[[BoostaR_idx()]]
        rows_selected <- input$BoostaR_gain_summary_cell_clicked$value
        if(length(rows_selected)==1){
          int_order <- str_count(rows_selected, ' x ') + 1
        }
        if(is.null(rows_selected)){
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "build_error",
                            title = 'Error',
                            text = 'Please select a 1D or 2D interaction row from the gain summary table',
                            btn_labels = c('OK'))
        } else if (int_order>2) {
          confirmSweetAlert(session = session,
                            type = 'error',
                            inputId = "build_error",
                            title = 'Error',
                            text = 'Please select a 1D or 2D interaction row from the gain summary table',
                            btn_labels = c('OK'))
        } else {
          if(int_order==1){
            f1 <- rows_selected
            f2 <- NULL
          } else if(int_order==2){
            # extract features from table
            char_pos <- as.numeric(gregexpr(' x ', rows_selected))
            f1 <- substr(rows_selected, char_pos+3, nchar(rows_selected))
            f2 <- substr(rows_selected, 1,char_pos-1)
          }
        }
        info_list <- list(
          originator = 'BoostaR gain summary',
          int_order=int_order,
          f1=f1,
          f2=f2
        )
        crosstab_selector(info_list)
      }
    })
    observe({
      model_index <- isolate(BoostaR_idx())
      volumes <- c('working directory' = getwd(), 'home' = fs::path_home())
      shinyFileSave(input, 'BoostaR_save_model', roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$BoostaR_save_model)
      isolate({
        if (nrow(fileinfo) > 0) {
          if(!is.null(model_index)){
            lgb.save(BoostaR_models()[[model_index]]$lgbm, fileinfo$datapath)
            confirmSweetAlert(session = session,
                              type = 'success',
                              inputId = "temp",
                              title = 'LightGBM saved',
                              btn_labels = c('OK')
            )
          } else {
            confirmSweetAlert(session = session,
                              type = 'error',
                              inputId = "temp",
                              title = 'No model selected',
                              btn_labels = c('OK')
            )
          }
        }
      })
    })
    observe({
      volumes <- c('working directory' = getwd(), 'home' = fs::path_home())
      shinyFileSave(input, 'BoostaR_save_BoostaR_models', roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$BoostaR_save_BoostaR_models)
      isolate({
        if (nrow(fileinfo) > 0) {
          # leave only part of glm_model object needed for prediction
          # otherwise file will be huge (it retains data used to fit model)
          if(length(BoostaR_models())>0){
            saveRDS(BoostaR_models(), file = fileinfo$datapath, compress = TRUE)
            confirmSweetAlert(session = session,
                              type = 'success',
                              inputId = "temp",
                              title = 'BoostaR models saved',
                              btn_labels = c('OK')
            )
          } else {
            confirmSweetAlert(session = session,
                              type = 'error',
                              inputId = "temp",
                              title = 'No GBMs built',
                              btn_labels = c('OK')
            )
          }
        }
      })
    })
  })
}

make_BoostaR_model_summary <- function(Bs){
  # takes the key info from the BoostaR_models
  # and makes a summary data table
  if(!is.null(Bs)){
    if(length(Bs)>0){
      rows <- lapply(Bs, BoostaR_model_summary_row)
      summary <- rbindlist(rows, fill = TRUE)
      summary[, `row%` := format(round(`row%`,2), nsmall = 2)]
      summary[, `col%` := format(round(`col%`,2), nsmall = 2)]
      summary[, `lr` := format(round(lr,2), nsmall = 2)]
      summary
    } else {
      NULL
    }
  } else {
    NULL
  }
}
BoostaR_model_summary_row <- function(BoostaR_model){
  if(!is.null(BoostaR_model)){
    num_ICs <- ifelse(is.null(BoostaR_model$params$interaction_constraints),
                      0,
                      length(BoostaR_model$params$interaction_constraints)-1)
    # format train and test errors
    test_err <- BoostaR_model$evaluation_log$test_err
    train_err <- BoostaR_model$evaluation_log$train_err
    if(!is.null(test_err)){
      test_err <- signif(test_err, 6)
    } else {
      test_err <- NA
    }
    if(!is.null(train_err)){
      train_err <- signif(train_err, 6)
    } else {
      train_err <- NA
    }
    x <- data.table(name = BoostaR_model$name,
                    test = test_err,
                    train = train_err,
                    best_iter = BoostaR_model$evaluation_log$best_iteration,
                    method = BoostaR_model$params$boosting,
                    lr = BoostaR_model$params$learning_rate,
                    ebm = BoostaR_model$ebm_mode,
                    leaves = BoostaR_model$params$num_leaves,
                    depth = BoostaR_model$params$max_depth,
                    `row%` = BoostaR_model$params$bagging_fraction,
                    `col%` = BoostaR_model$params$feature_fraction,
                    min_wt = BoostaR_model$params$min_data_in_leaf,
                    L1 = BoostaR_model$params$lambda_l1,
                    L2 = BoostaR_model$params$lambda_l2,
                    n_feat = length(BoostaR_model$features)
    )
  }
  return(x)
}
make_BoostaR_detailed_summary <- function(BoostaR_model){
  if(!is.null(BoostaR_model)){
    # calculate the number of feature interaction constraints
    if(is.null(BoostaR_model$params$interaction_constraints)){
      num_interaction_constraints <- 0
    } else {
      num_interaction_constraints <- length(BoostaR_model$params$interaction_constraints)
    }
    # calculate the number of monotone constraints
    if(is.null(BoostaR_model$params$monotone_constraints)){
      num_monotone_constraints <- 0
    } else {
      num_monotone_constraints <- sum(abs(BoostaR_model$params$monotone_constraints)>0)
    }
    x <- data.table(name = BoostaR_model$name,
                    response = BoostaR_model$response,
                    weight = BoostaR_model$weight,
                    objective = BoostaR_model$params$objective,
                    metric = BoostaR_model$params$metric,
                    tweedie_var_power = BoostaR_model$params$tweedie_variance_power,
                    boosting_method = BoostaR_model$params$boosting,
                    offset = BoostaR_model$init_score,
                    test = signif(BoostaR_model$evaluation_log$test_err, 6),
                    train = signif(BoostaR_model$evaluation_log$train_err, 6),
                    number_of_features = length(BoostaR_model$features),
                    interaction_constraints = num_interaction_constraints,
                    monotone_constraints = num_monotone_constraints,
                    model_build_time = round(as.numeric(BoostaR_model$run_time, units = 'secs'), 1),
                    SHAP_build_time = round(as.numeric(BoostaR_model$SHAP_run_time, units = 'secs'), 1),
                    num_iterations = BoostaR_model$params$num_iterations,
                    early_stopping = BoostaR_model$params$early_stopping_round,
                    best_iter = BoostaR_model$evaluation_log$best_iteration,
                    ebm_mode = BoostaR_model$ebm_mode,
                    lr = BoostaR_model$params$learning_rate,
                    leaves = BoostaR_model$params$num_leaves,
                    depth = BoostaR_model$params$max_depth,
                    bagging_fraction = BoostaR_model$params$bagging_fraction,
                    bagging_frequency = BoostaR_model$params$bagging_freq,
                    feature_fraction = BoostaR_model$params$feature_fraction,
                    min_data_in_leaf = BoostaR_model$params$min_data_in_leaf,
                    L1_normalisation = BoostaR_model$params$lambda_l1,
                    L2_normalisation = BoostaR_model$params$lambda_l2
    )
    x <- data.table(parameter=names(x), value = t(x[1]))
    setnames(x, c('parameter','value'))
    if(!is.null(BoostaR_model$additional_params)){
      add_params <- BoostaR_model$additional_params
      add_params$interaction_constraints <- NULL
      add_params$monotone_constraints <- NULL
      if(length(add_params)>0){
        extra_rows <- data.table(parameter = names(add_params), value = add_params)
        x <- rbindlist(list(x, extra_rows))
      }
    }
    x
  } else {
    NULL
  }
}
lgbm.convert.to.tables <- function(d, BoostaR_model, base_risk = NULL, feature_specification = NULL){
  # prepare the tabulations - all combinations of features that appear at least once in the GBM
  var_terms <- lgbm.extract.feature.combinations(BoostaR_model$tree_table, BoostaR_model$lgbm$best_iter)
  tabulations <- prepare_lgbm_tabulations(d[BoostaR_model$pred_rows], var_terms$split_features, feature_specification)
  # check if tabulations is a character - if so then don't go further as this is a warning message
  if(!inherits(tabulations, 'character')){
    # predict on the base level
    cols <- BoostaR_model$features
    base_risk <- base_risk[,..cols] # we only want to keep the columns used in the model or lgb.predict will error
    base_risk_converted <- lgb.convert_with_rules(base_risk, rules = BoostaR_model$rules)
    base_risk_converted <- as.matrix(base_risk_converted$data)
    base_level <- predict(BoostaR_model$lgbm, base_risk_converted, type = 'raw')
    tabulations[['base']]$base <- base_level
    # predict on the tabulations
    for(i in 2:length(tabulations)){
      # replace base risk features for this table with the columns in tabulation
      # convert for format needed for lgbm to predict
      table_name <- names(tabulations)[[i]]
      vars <- unlist(strsplit(table_name, '|', fixed = TRUE))
      dummy_risks <- base_risk[rep(1,each=nrow(tabulations[[i]]))]
      new_cols <- vars # otherwise will get warning on next line
      dummy_risks[, (new_cols):=tabulations[[i]][,..vars]]
      # make factor columns character so that the rules can be correctly applied by lgb.convert_with_rules
      cols <- names(dummy_risks)[sapply(dummy_risks, inherits, 'factor')]
      dummy_risks[, (cols) := lapply(.SD, as.character), .SDcols = cols]
      # convert with rules to lgb dataset
      dummy_risks_converted <- lgb.convert_with_rules(dummy_risks, rules = BoostaR_model$rules)
      dummy_risks_converted <- as.matrix(dummy_risks_converted$data)
      # get the trees used by this table
      trees_idx <- var_terms[i-1][['trees']] # as var_terms do not contain the base level need the minus 1
      trees_idx <- as.numeric(trimws(unlist(strsplit(trees_idx, ",")))) # split into vector, trim leading whitespace
      # predict on tables
      predictions <- lgbm.extract.tree.predictions(dummy_risks_converted, BoostaR_model$lgbm, trees_idx)
      tabulations[[table_name]]$tabulated_lgbm <- predictions
    }
  }
  return(tabulations)
}
# functions for new bit
lgbm.extract.feature.combinations <- function(lgbm_tree_table, best_iter){
  # returns a data.table containing all combinations of features that appear
  # together with the gain from that combination of features
  # only retain the trees up to best_iter
  lgbm_tree_table <- lgbm_tree_table[tree_index<=best_iter]
  setorder(lgbm_tree_table, tree_index, split_feature)
  tree_summary <- lgbm_tree_table[, .(split_features = toString(na.omit(unique(split_feature))),
                                      gain = sum(split_gain, na.rm = TRUE)),
                                  by = list(tree_index)]
  feature_combination_summary <- tree_summary[, .(trees = toString(na.omit(unique(tree_index))),
                                                  gain = sum(gain, na.rm = TRUE)),
                                              by = list(split_features)]
  feature_combination_summary[, split_features := gsub(', ', '|', split_features)]
  setorderv(feature_combination_summary, cols = 'gain', order = -1)
}
lgbm.extract.tree.predictions <- function(d, model, trees_idx){
  # d is the data frame you want to predict on
  # model is the LightGBM BOOSTER? object
  # tree_idx is vector of integers indicating which trees should be extracted
  if(length(trees_idx)>0){
    prediction_matrix <- matrix(data=0, nrow=nrow(d), ncol = length(trees_idx))
    for (i in 1:length(trees_idx)){
      idx <- trees_idx[i]
      prediction_matrix[,i] <- predict(model, d, type='raw', start_iteration = idx, num_iteration = 1)
    }
    # sum rows to get the total prediction from the specified trees
    rowSums(prediction_matrix)
  } else {
    # return column of zeroes - as no trees supplied
    rep(0, nrow(d))
  }
}
prepare_lgbm_tabulations <- function(dt, var_terms, feature_spec){
  tabulations <- list()
  tabulations[['base']] <- data.table(base=0L)
  if(!is.null(var_terms)){
    for(i in 1:length(var_terms)){
      # get the variables in the table
      vars <- unlist(strsplit(var_terms[[i]], '|', fixed = TRUE))
      banded_vars <- list()
      for(j in 1:length(vars)){
        banded_vars[[vars[j]]] <- feature_banding(dt, vars[j], feature_spec)
      }
      # calculate the number of rows
      combos <- prod(sapply(banded_vars, length))
      if(combos>100000){
        tabulations <- paste('More than 100,000 rows in table', var_terms[[i]])
        break
      } else {
        # create data.table with all combinations of banded_vars
        expanded_vars <- expand.grid(banded_vars)
        tabulations[[var_terms[i]]] <- setDT(expanded_vars)
      }
    }
  }
  return(tabulations)
}
adjust_base_levels_lgbm <- function(tabulations, feature_spec){
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
        adjustment <- tabulations[[i]][base_levels]$tabulated_lgbm
      } else {
        adjustment <- 0
      }
      tabulations[[i]][['tabulated_lgbm']] <- tabulations[[i]][['tabulated_lgbm']] - adjustment
      #cumulative_adjustment <- cumulative_adjustment + adjustment
    }
  }
  # adjust the base level
  #tabulations[['base']] <- tabulations[['base']] + cumulative_adjustment
  return(tabulations)
}
predict_tabulations_lgbm <- function(dt, tabulations, feature_spec){
  # matrix to hold the predictions for each table in tablulations
  # and each row in dt
  predictions <- matrix(data=NA, nrow=nrow(dt),ncol=length(tabulations))
  # base level is the same for every row
  predictions[,1] <- tabulations[[1]][['base']]
  if(length(tabulations)>1){
    # loop over the remaining tables
    withProgress(message = 'BoostaR', detail = 'tabulating', {
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
        predictions[,i] <- merged$tabulated_lgbm
      }
    })
  }
  predictions_dt <- data.table(predictions)
  setnames(predictions_dt, names(tabulations))
  predictions_dt[, tabulated_lgbm := rowSums(predictions_dt)]
  return(predictions_dt)
}