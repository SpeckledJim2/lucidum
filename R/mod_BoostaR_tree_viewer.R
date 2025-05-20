#' BoostaR_tree_viewer UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
mod_BoostaR_tree_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 5,
        fluidRow(
          column(
            width = 12,
            h3('Select tree')
            )
        ),
        DTOutput(ns('BoostaR_tree_summary'))
      ),
      column(
        width = 7,
        fluidRow(
          column(
            width = 6,
            h3('Tree viewer')
          ),
          column(
            width = 6,
            div(
              style = 'margin-top:18px;',
              align = 'right',
              radioGroupButtons(
                inputId = ns('BoostaR_tree_colours'),
                label = NULL,
                choiceValues = c('Plain', 'Divergent', 'Spectral', 'Viridis'),
                choiceNames = c(
                  'Plain',
                  tagList(tags$img(src = 'www/divergent.png', height = "16px", width = "16px", ' Divergent')),
                  tagList(tags$img(src = 'www/spectral.png', height = "16px", width = "16px", ' Spectral')),
                  tagList(tags$img(src = 'www/viridis.png', height = "16px", width = "16px", ' Viridis'))
                ),
                selected = 'Plain'
              )
            )
          )
        ),
        grVizOutput(ns("BoostaR_tree_diagram"), width = '100%', height = '75vh')
      )
    )
  )
}

#' BoostaR_tree_viewer Server Functions
#'
#' @noRd 
mod_BoostaR_tree_viewer_server <- function(id, BoostaR_models, BoostaR_idx){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # reactiveVal to hold the selected tree
    selected_tree <- reactiveVal(0)
    
    # tree diagram render
    output$BoostaR_tree_diagram <- renderGrViz({
      req(BoostaR_models(), BoostaR_idx(), selected_tree())
      model_index <- BoostaR_idx()
      
      # Ensure valid model index
      if(model_index %in% names(BoostaR_models())){
        tree_table <- BoostaR_models()[[model_index]]$tree_table
        rules <- BoostaR_models()[[model_index]]$rules
        tree <- tree_table[tree_index == selected_tree(), ]
        
        if(nrow(tree) > 0){
          my_graph <- BoostaR_render_tree_graph(tree, input$BoostaR_tree_colours, rules)
          render_graph(my_graph)
        }
      }
    })
    
    # new tree summary table
    output$BoostaR_tree_summary <- DT::renderDT({
      req(BoostaR_models(), BoostaR_idx())
      model_index <- BoostaR_idx()
      tree_summary <- summarise_trees(BoostaR_models()[[model_index]]$tree_table)
      long_terms <- nchar(tree_summary$features)>60
      tree_summary[long_terms, features := paste0(substr(features, 1, 60), '...')]
      DT::datatable(
        tree_summary, 
        rownames = FALSE,
        selection = list(mode = "single", target = "row"),
        options = list(
          pageLength = nrow(tree_summary),
          dom = 'frt',
          scrollX = TRUE,
          scrollY = 'calc(90vh - 200px)',
          searchHighlight = TRUE,
          columnDefs = list(
            list(width = '15px', targets = 0, className = 'dt-left'),  # Width of first column
            list(width = '15px', targets = 1, className = 'dt-left'),  # Width of second column
            list(width = '30px', targets = 3, className = 'dt-left')   # Width of second column
          )
          )
      ) %>%
        DT::formatStyle(columns = colnames(tree_summary), lineHeight = '0%', fontSize = '12px')
    })
    
    # get the selected tree
    observeEvent(input$BoostaR_tree_summary_rows_selected, {
      # minus one as trees start at zero
      selected_tree(input$BoostaR_tree_summary_rows_selected[1]-1)
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
  unlist(result)
}

#' @importFrom DiagrammeR create_edge_df create_graph add_global_graph_attrs
BoostaR_render_tree_graph <- function(dt, colours, rules = NULL){
  if(!is.null(dt)){
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
    
    # get rid of 1e-35's, i.e zeroes
    if(inherits(dt[['Split']], 'character')){
      dt[grep('e-35', Split), Split := '0']
    }
    
    # replace indices with feature levels if rules supplied
    if(!is.null(rules)){
      for (f in names(rules)){
        dt[Feature==f & decision_type == '==', Split := replace_lgbm_levels_with_names(Split, f, rules)]
      }
    }
    
    # replace long split names
    dt[nchar(Split)>500, Split := 'Split too long to render']
    
    # make the chart
    edges <- DiagrammeR::create_edge_df(
      from  = match(dt[Feature != "Leaf", c(ID)] |> rep(2), dt$ID),
      to    = match(dt[Feature != "Leaf", c(Yes, No)], dt$ID),
      label = dt[Feature != "Leaf", paste(decision_type, Split)] |>
        c(rep("", nrow(dt[Feature != "Leaf"]))),
      style = dt[Feature != "Leaf", ifelse(Missing == Yes, "bold", "solid")] |>
        c(dt[Feature != "Leaf", ifelse(Missing == No, "bold", "solid")]),
      rel   = "leading_to")
    graph <- DiagrammeR::create_graph(
      nodes_df = nodes,
      edges_df = edges,
      attr_theme = NULL
    ) |>
      DiagrammeR::add_global_graph_attrs(
        attr_type = "graph",
        attr  = c("layout", "rankdir"),
        value = c("dot", "LR")
      ) |>
      DiagrammeR::add_global_graph_attrs(
        attr_type = "node",
        attr  = c("color", "style", "fontname"),
        value = c("DimGray", "filled", "Helvetica")
      ) |>
      DiagrammeR::add_global_graph_attrs(
        attr_type = "edge",
        attr  = c("color", "arrowsize", "arrowhead", "fontname"),
        value = c("DimGray", "1.5", "vee", "Helvetica"))
  }
}

tree_statistics <- function(dt, t_idx){
  if(!is.null(dt)){
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
}

# function to summarise each tree in a GBM
summarise_trees <- function(dt) {
  
  # Calculate summary statistics by tree_index
  result <- dt[, .(
    # dimensionality: count unique non-NA split_features
    dim = sum(!is.na(unique(split_feature)) & 
                           unique(split_feature) != "NA" & 
                           unique(split_feature) != ""),
    
    # concatenated features with " x " separator (sorted alphabetically)
    features = {
      unique_features <- unique(split_feature[!is.na(split_feature) & 
                                                split_feature != "NA" & 
                                                split_feature != ""])
      if(length(unique_features) > 0) {
        paste(unique_features, collapse = " x ")
      } else {
        ""
      }
    },
    
    # Sum of split_gain for the tree
    gain = round(sum(split_gain, na.rm = TRUE), 0)
    
  ), by = tree_index]
  
  # make sure the columns are in the correct order
  setnames(result, old = 'tree_index', new = 'tree')
  
  return(result)
}