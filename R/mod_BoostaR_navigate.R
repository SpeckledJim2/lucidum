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
               inputId = ns('BoostaR_generate_predictions'),
               label = 'Predict',
               icon = icon("chevron-right")
               ),
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
            width = 8,
            h3('Gain summary')
          ),
          column(
            width = 4,
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
#' 
#' 
mod_BoostaR_navigate_server <- function(id, BoostaR_models, BoostaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(BoostaR_models(), {
      output$BoostaR_model_summary <- DT::renderDT({
        # model summary table
        dt <- BoostaR_model_summary(BoostaR_models())
        dt |>
          DT::datatable(rownames= FALSE,
                        extensions = 'Buttons',
                        selection=list(mode="multiple", target="row"),
                        options = list(pageLength = nrow(dt),
                                       initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
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
          DT::formatStyle(columns = colnames(dt), lineHeight='0%', fontSize = '12px')
      })
      output$BoostaR_detailed_model_summary <- DT::renderDT({
        # model summary table
        dt <- BoostaR_detailed_summary(BoostaR_models()[[BoostaR_idx()]])
        dt |>
          DT::datatable(rownames= FALSE,
                        selection=list(mode="multiple", target="row"),
                        options = list(pageLength = nrow(dt),
                                       initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                       dom = 'rt',
                                       scrollX = T,
                                       scrollY = 'calc(80vh - 380px)',
                                       searchHighlight=TRUE
                        )
          ) |>
          DT::formatStyle(columns = colnames(dt), lineHeight='0%', fontSize = '12px')
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
                                     initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                     dom = 'rt',
                                     scrollX = T,
                                     scrollY = 'calc(80vh - 380px)'
                      )
            ) |>
            formatRound('gain', 0) |>
            formatPercentage('%', 1) |>
            formatStyle(columns = colnames(gain_summary), fontSize = '12px', lineHeight='0%')
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
  })
}

BoostaR_model_summary <- function(Bs){
  # takes the key info from the BoostaR_models
  # and makes a summary data table
  if(!is.null(Bs)){
    rows <- lapply(Bs, BoostaR_model_summary_row)
    rbindlist(rows)
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
                    lr = BoostaR_model$params$learning_rate,
                    leaves = BoostaR_model$params$num_leaves,
                    depth = BoostaR_model$params$max_depth,
                    `row%` = BoostaR_model$params$bagging_fraction,
                    `col%` = BoostaR_model$params$feature_fraction,
                    n_feat = length(BoostaR_model$features),
                    time = round(as.numeric(BoostaR_model$run_time), 1)
    )
  }
  return(x)
}
BoostaR_detailed_summary <- function(BoostaR_model){

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
                  objective = BoostaR_model$params$objective,
                  metric = BoostaR_model$params$metric,
                  response = BoostaR_model$response,
                  weight = BoostaR_model$weight,
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
                  feature_fraction = BoostaR_model$params$feature_fraction
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
}
