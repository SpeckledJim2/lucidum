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
    br(),
    fluidRow(
      column(width = 6,
             actionButton(
               inputId = ns("BoostaR_make_active"),
               label = 'Make active model',
               icon = icon("chevron-right")
               )
             ),
      column(width = 6,
             align = 'right',
             actionButton(
               inputId = ns("BoostaR_delete_model"),
               label = 'Delete selected model(s)',
               icon = icon("minus-circle")
             ),
             shinySaveButton(
               id = ns('BoostaR_save_model'),
               label = 'Save selected model',
               title = 'Save selected model',
               filename = "",
               filetype = list(txt="txt"),
               icon = icon('upload'),
               style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
               viewtype = "detail"
               )
             )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        DTOutput(ns('BoostaR_model_summary'))
      )
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        fluidRow(
          column(
            width = 12,
            sliderInput(ns("BoostaR_tree_selector"),
                        width = '100%',
                        label = NULL,
                        min = 0,
                        max = 2000,
                        step = 1,
                        value = 0,
                        ticks = FALSE,
                        animate = TRUE
            )
          )
        ),
        grVizOutput(ns("BoostaR_tree_diagram"), width = '100%', height = '400px')
      ),
      column(
        width = 6,
        fluidRow(
          column(
            width = 2,
            actionButton(
              inputId = ns("BoostaR_gain_table_goto_ChartaR"),
              icon = icon('chart-line'),
              label = ''
            )
          ),
          column(
            width = 6,
            textInput(
              ns('BoostaR_search_gain_table'),
              label = NULL,
              width = '100%',
              placeholder = 'select feature'
            )
          ),
          column(
            width = 4,
            align = 'right'
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
    output$BoostaR_model_summary <- DT::renderDT({
      # model summary table
      dt <- BoostaR_model_summary(BoostaR_models())
      dt %>%
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
        ) %>%
        DT::formatStyle(columns = colnames(dt), lineHeight='0%', fontSize = '12px')
    })
    output$BoostaR_tree_diagram <-   renderGrViz({
      # tree diagram
      if(length(BoostaR_models())>0){
        model_index <- BoostaR_idx()
        # QUESTION - why doesn't this line trigger (or return an error)?
        # the max on the SliderInput never gets updated
        # even though I have namespaced the inputId
        # works fine in old project without namespacing
        updateSliderInput(session, inputId = ns('BoostaR_tree_selector'), max = BoostaR_models()[[model_index]]$lgbm$best_iter-1)
        if(!is.na(model_index)){
          if(model_index %in% names(BoostaR_models())){
            tree_index <- NULL
            tree_table <- BoostaR_models()[[model_index]]$tree_table
            rules <- BoostaR_models()[[model_index]]$rules
            tree <- tree_table[tree_index==input$BoostaR_tree_selector,]
            my_graph <- BoostaR_render_tree_graph(tree, rules)
            render_graph(my_graph)
          }
        }
      }
    })
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
            updateSliderInput(session, ns('BoostaR_tree_selector'), value = first_tree)
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
                    run_time = round(BoostaR_model$run_time, 1),
                    SHAP_run_time = round(BoostaR_model$SHAP_run_time, 1),
                    #data = BoostaR_model$dataset,
                    response = BoostaR_model$response,
                    weight = BoostaR_model$weight,
                    #offset = BoostaR_model$offset,
                    obj = BoostaR_model$params$objective,
                    best_iter = BoostaR_model$evaluation_log$best_iteration,
                    test_err = signif(BoostaR_model$evaluation_log$test_err, 6),
                    train_err = signif(BoostaR_model$evaluation_log$train_err, 6),
                    lr = BoostaR_model$params$learning_rate,
                    leaves = BoostaR_model$params$num_leaves,
                    depth = BoostaR_model$params$max_depth,
                    row_smp = BoostaR_model$params$bagging_fraction,
                    col_smp = BoostaR_model$params$feature_fraction,
                    n_feat = length(BoostaR_model$features),
                    ICs = num_ICs
    )
  }
  return(x)
}

replace_lgbm_levels_with_names <- function(x, feature_name, rules){
  lvls <- rules[[feature_name]]
  result <- strsplit(x,'||', fixed = TRUE)
  result <- lapply(result, as.numeric)
  levels_to_names <- function(x){names(lvls)[as.numeric(x)]}
  result <- lapply(result, levels_to_names)
  result <- lapply(result, paste, collapse = '\n')
}

#' @importFrom DiagrammeR create_edge_df create_graph add_global_graph_attrs
BoostaR_render_tree_graph <- function(dt, rules = NULL){
  Value <- NULL
  leaf_value <-  NULL
  internal_value <- NULL
  Quality <- NULL
  Feature <- NULL
  Cover <- NULL
  internal_count <- NULL
  leaf_count <- NULL
  Node <- NULL
  split_index <- NULL
  leaf_index <- NULL
  ID <- NULL
  Tree <- NULL
  parent <- NULL
  node_parent <- NULL
  leaf_parent <- NULL
  Yes <- NULL
  No <- NULL
  default_left <- NULL
  Missing <- NULL
  label <- NULL
  shape <- NULL
  filledcolor <- NULL
  Split <- NULL
  decision_type <- NULL
  setnames(dt, old = c('tree_index','split_feature','threshold','split_gain'), new = c('Tree','Feature','Split','Quality'))
  dt[, Value := leaf_value]
  dt[is.na(Value), Value := internal_value]
  dt[is.na(Quality), Quality := leaf_value]
  dt[is.na(Feature), Feature := 'Leaf']
  dt[, Cover := internal_count][Feature=='Leaf', Cover := leaf_count]
  dt[, c('leaf_count', 'internal_count','leaf_value','internal_value'):= NULL]
  dt[, Node := split_index]
  max_node <- max(dt[['Node']], na.rm = TRUE)
  dt[is.na(Node), Node := max_node + leaf_index +1]
  dt[, ID := paste(Tree, Node, sep = '-')]
  dt[, c('depth','leaf_index') := NULL]
  dt[, parent := node_parent][is.na(parent), parent := leaf_parent]
  dt[, c('node_parent', 'leaf_parent','split_index') := NULL]
  dt[, Yes := dt$ID[match(dt$Node, dt$parent)]]
  dt <- dt[nrow(dt):1,]
  dt[, No := dt$ID[match(dt$Node, dt$parent)]]
  dt[default_left==TRUE, Missing := Yes]
  dt[default_left==FALSE, Missing := No]
  dt[, c('parent', 'default_left') := NULL]
  setcolorder(dt, c('Tree','Node','ID','Feature','decision_type','Split','Yes','No','Missing','Quality','Cover','Value'))
  
  dt[, label:= paste0(Feature,
                      "\nCover: ", Cover,
                      ifelse(Feature == "Leaf", "", "\nGain: "), ifelse(Feature == "Leaf", "", round(Quality, 4)),
                      "\nValue: ", round(Value, 4)
  )]
  dt[Node == 0, label := paste0("Tree ", Tree, "\n", label)]
  dt[, shape:= "rectangle"][Feature == "Leaf", shape:= "oval"]
  dt[, filledcolor:= "Beige"][Feature == "Leaf", filledcolor:= "Khaki"]
  # in order to draw the first tree on top:
  dt <- dt[order(-Tree)]
  
  nodes <- DiagrammeR::create_node_df(
    n         = nrow(dt),
    ID        = dt$ID,
    label     = dt$label,
    fillcolor = dt$filledcolor,
    shape     = dt$shape,
    data      = dt$Feature,
    fontcolor = "black")
  
  # format the edge labels
  numeric_idx <- !is.na(as.numeric(dt[['Split']]))
  dt[numeric_idx, Split := round(as.numeric(Split),4)]
  
  # replace indices with feature levels if rules supplied
  if(!is.null(rules)){
    for (f in names(rules)){
      dt[Feature==f & decision_type == '==',
         Split := replace_lgbm_levels_with_names(Split, f, rules)]
    }
  }
  
  # replace long split names
  dt[nchar(Split)>500, Split := 'Split too long to render']
  
  edges <- DiagrammeR::create_edge_df(
    from  = match(dt[Feature != "Leaf", c(ID)] %>% rep(2), dt$ID),
    to    = match(dt[Feature != "Leaf", c(Yes, No)], dt$ID),
    label = dt[Feature != "Leaf", paste(decision_type, Split)] %>%
      c(rep("", nrow(dt[Feature != "Leaf"]))),
    style = dt[Feature != "Leaf", ifelse(Missing == Yes, "bold", "solid")] %>%
      c(dt[Feature != "Leaf", ifelse(Missing == No, "bold", "solid")]),
    rel   = "leading_to")
  
  graph <- DiagrammeR::create_graph(
    nodes_df = nodes,
    edges_df = edges,
    attr_theme = NULL
  ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "graph",
      attr  = c("layout", "rankdir"),
      value = c("dot", "LR")
    ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "node",
      attr  = c("color", "style", "fontname"),
      value = c("DimGray", "filled", "Helvetica")
    ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr_type = "edge",
      attr  = c("color", "arrowsize", "arrowhead", "fontname"),
      value = c("DimGray", "1.5", "vee", "Helvetica"))
  
  
  
}
    

