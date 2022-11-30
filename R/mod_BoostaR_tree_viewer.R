#' BoostaR_tree_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_BoostaR_tree_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 8,
        fluidRow(
          column(
            width = 6,
            h3('Tree viewer')
          ),
          column(
            width = 6,
            div(
              style='margin-top:18px;',
              radioGroupButtons(
                inputId = ns('BoostaR_tree_colours'),
                label = NULL,
                choices = c('Plain','Divergent','Spectral','Viridis'),
                selected = 'Plain'
              )
            )
          )
        ),
        grVizOutput(ns("BoostaR_tree_diagram"), width = '100%', height = '75vh')
        ),
        column(
          width = 4,
          h3('Select tree'),
          sliderInput(ns("BoostaR_tree_selector"),
                      width = '100%',
                      label = NULL,
                      min = 0,
                      max = 2000,
                      step = 1,
                      value = 0,
                      ticks = FALSE,
                      animate = TRUE
                      ),
          br(),
          DTOutput(ns('BoostaR_tree_summary'))
          )
      )
    )
}
    
#' BoostaR_tree_viewer Server Functions
#'
#' @noRd 
mod_BoostaR_tree_viewer_server <- function(id, BoostaR_models, BoostaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$BoostaR_tree_diagram <-   renderGrViz({
      # tree diagram
      if(length(BoostaR_models())>0){
        model_index <- BoostaR_idx()
        updateSliderInput(
          session,
          inputId = 'BoostaR_tree_selector',
          max = BoostaR_models()[[model_index]]$lgbm$best_iter-1
          )
        if(!is.na(model_index)){
          if(model_index %in% names(BoostaR_models())){
            tree_index <- NULL
            tree_table <- BoostaR_models()[[model_index]]$tree_table
            rules <- BoostaR_models()[[model_index]]$rules
            tree <- tree_table[tree_index==input$BoostaR_tree_selector,]
            my_graph <- BoostaR_render_tree_graph(tree, input$BoostaR_tree_colours, rules)
            render_graph(my_graph)
          }
        }
      }
    })
    # QUESTION - same again, better to put below in observeEvent?
    observeEvent(c(BoostaR_models(), BoostaR_idx(), input$BoostaR_tree_selector), {
      output$BoostaR_tree_summary <- DT::renderDT({
        # model summary table
        dt <- tree_statistics(BoostaR_models()[[BoostaR_idx()]]$tree_table, input$BoostaR_tree_selector)
        dt |>
          DT::datatable(rownames= FALSE,
                        selection=list(mode="multiple", target="row"),
                        options = list(pageLength = nrow(dt),
                                       initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                       dom = 'rt',
                                       scrollX = T,
                                       #scrollY = 'calc(80vh - 380px)',
                                       searchHighlight=TRUE
                        )
          ) |>
          DT::formatStyle(columns = colnames(dt), lineHeight='0%', fontSize = '12px')
      })
    })
  })
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
BoostaR_render_tree_graph <- function(dt, colours, rules = NULL){
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
  
  bins <- unique(stats::quantile(round(dt$Value,6), na.rm = TRUE, probs = 0:20/20))
  bins[1] <- bins[1] - 0.000001
  bins[length(bins)] <- bins[length(bins)] + 0.000001
  if(colours=='Plain'){
    clrs <- NULL
  } else if (colours=='Divergent'){
    clrs <- c('green','white','red')
  } else if (colours=='Spectral'){
    clrs <- c('blue','yellow','red')
  } else if (colours=='Viridis'){
    clrs <- c('purple','green','yellow')
  }
  if(!is.null(clrs)){
    pal <- colorBin(palette = colorRamp(clrs, interpolate="linear"), domain = NULL, bins = bins)
    dt[, filledcolor := pal(Value)]
    
  }
  
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

tree_statistics <- function(dt, t_idx){
  starting_value <- dt[tree_index==t_idx & split_index==0, internal_value]
  gain <- dt[tree_index==t_idx, sum(split_gain, na.rm = TRUE)]
  cover <- dt[tree_index==t_idx & split_index==0, internal_count]
  min_leaf <- dt[tree_index==t_idx & !is.na(leaf_index), min(leaf_value, na.rm = TRUE)]
  max_leaf <- dt[tree_index==t_idx & !is.na(leaf_index), max(leaf_value, na.rm = TRUE)]
  w_avg <- dt[tree_index==t_idx & !is.na(leaf_index), sum(leaf_count*leaf_value)/sum(leaf_count)]
  w_sd <- dt[tree_index==t_idx & !is.na(leaf_index), sqrt(sum(leaf_count*(leaf_value-w_avg)^2)/sum(leaf_count))]
  x <- data.table(tree_index = t_idx,
                  gain = gain,
                  starting_value = starting_value,
                  min_leaf = min_leaf,
                  max_leaf = max_leaf,
                  leaf_average = w_avg,
                  leaf_sd = w_sd
  )
  x <- data.table(parameter=names(x), value = t(signif(x[1], 6)))
  setnames(x, c('parameter','value'))
}