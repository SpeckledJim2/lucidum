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
             # actionButton(
             #   inputId = ns('BoostaR_generate_predictions'),
             #   label = 'Predict',
             #   icon = icon("chevron-right")
             #   ),
             shinySaveButton(
               id = ns('BoostaR_save_model'),
               label = 'Save model',
               title = 'Save model',
               filename = "",
               filetype = list(txt="txt"),
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
#' 
#' 
mod_BoostaR_navigate_server <- function(id, BoostaR_models, BoostaR_idx, crosstab_selector){
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
          n_rows <- pmin(1000, nrow(gain_summary))
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
      new_list <- BoostaR_models()[-rows_selected]
      BoostaR_models(new_list)
    })
    observeEvent(input$BoostaR_make_active, {
      rows_selected <- input$BoostaR_model_summary_rows_selected
      if(!is.null(rows_selected)){
        if(length(rows_selected)>1){
          rows_selected <- rows_selected[1]
        }
      }
      BoostaR_idx(names(BoostaR_models())[rows_selected])
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
  })
}

make_BoostaR_model_summary <- function(Bs){
  # takes the key info from the BoostaR_models
  # and makes a summary data table
  if(!is.null(Bs)){
    if(length(Bs)>0){
      rows <- lapply(Bs, BoostaR_model_summary_row)
      rbindlist(rows)
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
    x <- data.table(name = BoostaR_model$name,
                    test = signif(BoostaR_model$evaluation_log$test_err, 6),
                    train = signif(BoostaR_model$evaluation_log$train_err, 6),
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
                    n_feat = length(BoostaR_model$features),
                    time = round(as.numeric(BoostaR_model$run_time), 1)
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
                    tweedie_variance_power = BoostaR_model$params$tweedie_variance_power,
                    boosting_method = BoostaR_model$params$boosting,
                    offset = BoostaR_model$init_score,
                    test = signif(BoostaR_model$evaluation_log$test_err, 6),
                    train = signif(BoostaR_model$evaluation_log$train_err, 6),
                    number_of_features = length(BoostaR_model$features),
                    interaction_constraints = num_interaction_constraints,
                    monotone_constraints = num_monotone_constraints,
                    model_build_time = round(as.numeric(BoostaR_model$run_time), 1),
                    SHAP_build_time = round(as.numeric(BoostaR_model$SHAP_run_time), 1),
                    num_iterations = BoostaR_model$params$num_iterations,
                    early_stopping_round = BoostaR_model$params$early_stopping_round,
                    best_iter = BoostaR_model$evaluation_log$best_iteration,
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
        extra_rows <- data.table(name = names(add_params), value = add_params)
        x <- rbindlist(list(x, extra_rows))
      }
    }
    x
  } else {
    NULL
  }
}

