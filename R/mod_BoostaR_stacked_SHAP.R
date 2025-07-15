#' BoostaR_stacked_SHAP UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
mod_BoostaR_stacked_SHAP_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = ns('feature'),
          width = '100%',
          label = 'Model feature',
          choices=c('none'),
          size = 50,
          selectize = FALSE
        )
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 3,
            radioGroupButtons(
              inputId = ns('sort'),
              label = 'x-axis sort order',
              choices = c('A-Z', 'Descending'),
              individual = FALSE,
              size = 'xs',
              selected = 'A-Z')
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = ns('tail'),
              label = 'Tail grouping',
              choiceNames = c('-','0.1%','0.5%','1%','5%'),
              choiceValues = c(0, 0.001, 0.005, 0.01, 0.05),
              individual = FALSE,
              size = 'xs',
              selected = 0)
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = ns('top_n'),
              label = 'Num features to display',
              choiceNames = c('1','2','3','5','10','All'),
              choiceValues = c(1,2,3,5,10,0),
              individual = FALSE,
              size = 'xs',
              selected = 0)
          ),
          column(
            width = 3,
            align = 'right',
            mod_bandingChooser_ui(ns('feature_banding')),
          )
        ),
        plotlyOutput(ns('stacked_SHAP_plot'), height = 'calc(90vh - 150px)')
      )
    )
  )
}

#' BoostaR_tree_viewer Server Functions
#'
#' @noRd 
mod_BoostaR_stacked_SHAP_server <- function(id, d, dt_update, BoostaR_models, BoostaR_idx){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    x_col_and_banding <- reactiveVal(NULL)
    banding_new <- mod_bandingChooser_server('feature_banding', d, x_col_and_banding)
    observeEvent(banding_new(), {
      x_col_and_banding(list(x_col = x_col_and_banding()$x_col, banding = banding_new()))
    })
    observeEvent(input$feature, {
      req(d(), input$feature)
      banding_guess <- banding_guesser_numeric_date(d(), input$feature)
      x_col_and_banding(list(x_col = input$feature, banding = banding_guess))
    })
    observeEvent(c(BoostaR_models(), BoostaR_idx()), {
      # require following variables to be not NULL
      req(BoostaR_models(), BoostaR_idx())
      # get the GBM features
      features <- BoostaR_models()[[BoostaR_idx()]]$importances$Feature
      # validate sel1
      sel1 <- input$feature
      if (sel1 %not_in% features) {
        sel1 <- features[1]
      }
      # update selectInput
      updateSelectInput(inputId = 'feature', choices = features, selected = sel1)
    })
    # render plot
    output$stacked_SHAP_plot <- renderPlotly({
      req(dt_update(), BoostaR_models(), BoostaR_idx())
      req(d(), x_col_and_banding(), x_col_and_banding()$x_col, input$sort)
      if(x_col_and_banding()$x_col!='none'){
        stacked_SHAP(
          dat = d()[total_filter==1],
          feature_col = x_col_and_banding()$x_col,
          sort = input$sort,
          banding = x_col_and_banding()$banding,
          p_tail = as.numeric(input$tail),
          top_n = as.integer(input$top_n)
        )
      }
    })
  })
}

# render stacked SHAP plot
#' @importFrom grDevices colorRampPalette
stacked_SHAP <- function(dat, feature_col, sort, banding = NULL, p_tail = 0, top_n = 5) {
  shap_cols <- grep("^lgbm_SHAP_", names(dat), value = TRUE)
  shap_cols <- shap_cols[!grepl("base_score", shap_cols)]
  
  # banding with optional tail grouping
  if (!is.null(banding) &&
      is.numeric(dat[[feature_col]]) &&
      length(banding) == 1 &&
      uniqueN(na.omit(dat[[feature_col]])) > 2) {
    
    x <- dat[[feature_col]]
    rng <- range(x, na.rm = TRUE)
    
    if (p_tail > 0 && p_tail < 0.5) {
      lower_cut <- quantile(x, probs = p_tail, na.rm = TRUE)
      upper_cut <- quantile(x, probs = 1 - p_tail, na.rm = TRUE)
      breaks_main <- seq(floor(lower_cut), ceiling(upper_cut), by = banding)
      breaks <- c(-Inf, breaks_main, Inf)
      dat[, feature_group := cut(x, breaks = breaks, include.lowest = TRUE, dig.lab = 10)]
    } else {
      breaks <- seq(floor(rng[1]), ceiling(rng[2]) + banding, by = banding)
      dat[, feature_group := cut(x, breaks = breaks, include.lowest = TRUE)]
    }
    
  } else {
    dat[, feature_group := as.factor(dat[[feature_col]])]
  }
  
  shap_summary <- dat[, lapply(.SD, mean, na.rm = TRUE), by = feature_group, .SDcols = shap_cols]
  shap_summary[, net_feature_contribution := rowSums(.SD), .SDcols = shap_cols]
  
  # reorder feature_group if sorting requested
  if (!is.numeric(dat[[feature_col]]) && sort == "Descending") {
    shap_summary <- shap_summary[order(-net_feature_contribution)]
    shap_summary[, feature_group := factor(feature_group, levels = feature_group)]
  } else {
    shap_summary[, feature_group := factor(feature_group)]  # reset factor levels
  }
  
  shap_long <- melt(
    shap_summary, 
    id.vars = c("feature_group", "net_feature_contribution"),
    measure.vars = shap_cols,
    variable.name = "shap_feature", 
    value.name = "shap_value"
  )
  
  shap_long[, shap_feature := gsub("lgbm_SHAP_", "", shap_feature)]
  shap_long[, `:=`(
    shap_pos = pmax(shap_value, 0),
    shap_neg = pmin(shap_value, 0)
  )]
  
  # generate distinct colours
  n_feats <- uniqueN(shap_long$shap_feature)
  Set3_colors <- c(
    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
    "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"
  )
  get_colors <- colorRampPalette(Set3_colors)
  feature_colors <- setNames(get_colors(n_feats), unique(shap_long$shap_feature))
  
  # sort by descending mean absolute SHAP
  sd_SHAP <- data.table(
    feature = names(shap_summary)[-1],
    mean_abs_SHAP = sapply(shap_summary[, -1, with = FALSE], function(x) mean(abs(x)))
  )
  setorder(sd_SHAP, -mean_abs_SHAP)
  sd_SHAP <- sd_SHAP[feature != 'net_feature_contribution']
  sd_SHAP[, feature := gsub("lgbm_SHAP_", "", feature)]
  
  # build plot
  p <- plot_ly()
  
  # add net prediction points
  net_contrib <- unique(shap_long[, .(feature_group, net_feature_contribution)])
  p <- add_trace(p,
                 data = net_contrib,
                 x = ~feature_group,
                 y = ~net_feature_contribution,
                 type = "scatter",
                 mode = "markers",
                 marker = list(color = "red", size = 8),
                 name = "Sum of SHAP values",
                 yaxis = "y"
  )
  
  # group features
  features <- sd_SHAP$feature
  if (top_n > 0 && top_n < nrow(sd_SHAP)) {
    top_feats <- head(sd_SHAP$feature, top_n)
    all_feats <- unique(shap_long$shap_feature)
    
    # Group remaining as "Other"
    other_data <- shap_long[!shap_feature %in% top_feats,
                            .(shap_value = sum(shap_value, na.rm = TRUE)),
                            by = .(feature_group)]
    other_data[, shap_feature := "Other"]
    shap_long <- rbind(shap_long[shap_feature %in% top_feats], other_data, fill = TRUE)
    features <- c(top_feats, 'Other')
  }
  for (feature in features) {
    sub_data <- shap_long[shap_feature == feature]
    color <- if (feature == "Other") "#A9A9A9" else feature_colors[feature]
    
    p <- add_trace(p,
                   data = sub_data,
                   x = ~feature_group,
                   y = ~shap_value,
                   type = "bar",
                   name = feature,
                   marker = list(color = color)
    )
  }
  
  p <- layout(p,
              barmode = "relative",
              title = list(
                text = paste("SHAP Values by", feature_col),
                y = 0.98,           # move title down (1 = very top)
                x = 0.5,            # center the title horizontally
                xanchor = "center",
                yanchor = "top"
              ),
              xaxis = list(title = feature_col, tickangle = -45, tickfont = list(size = 10)),
              yaxis = list(title = "SHAP Contribution (Linear Predictor Scale)"),
              legend = list(
                title = list(text = "Feature"),
                y = 0.97  # move legend down; 1 is top, 0 is bottom
              )
  )
  
  dat[, c('feature_group') := NULL]
  return(p)
}
