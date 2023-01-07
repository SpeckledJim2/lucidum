#' ChartaR_SHAP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ChartaR_SHAP_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = ns('feature_1'),
          width = '100%',
          label = 'SHAP feature one',
          choices=c('none'),
          size = 20,
          selectize = FALSE
        ),
        selectInput(
          inputId = ns('feature_2'),
          width = '100%',
          label = 'SHAP feature two',
          choices=c('none'),
          size = 20,
          selectize = FALSE
        )
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 4,
            mod_bandingChooser_ui(ns('feature_1_banding'))
          ),
          column(
            width = 4,
            align = 'center',
            radioGroupButtons(
              inputId = ns('quantile'),
              label = "Tail % to group",
              choices = c('-','0.1%','0.5%','1%','2%','5%'),
              individual = FALSE,
              size = 'xs',
              selected = '1%')
          ),
          column(
            width = 4,
            align = 'right',
            mod_bandingChooser_ui(ns('feature_2_banding'))
          )
        ),
        fluidRow(
          column(
            width = 3,
            style = 'margin-top:-10px',
            checkboxInput(
              inputId = ns('feature_1_factor'),
              label = 'Treat as factor',
              value = FALSE
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = ns('rebase'),
              label = NULL,
              choices = c('-','0','1'),
              individual = FALSE,
              size = 'xs',
              selected = '-')
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = ns('SHAP_ribbons'),
              label = NULL,
              choices = c('Mean','25_75','5_95','All'),
              individual = FALSE,
              size = 'xs',
              selected = 'All')
          ),
          column(
            width = 3,
            style = 'margin-top:-10px',
            align = 'right',
            checkboxInput(
              inputId = ns('feature_2_factor'),
              label = 'Treat as factor',
              value = FALSE
            )
          )
        ),
        plotlyOutput(ns('SHAP_plot'), height = 'calc(85vh - 150px)')
      )
    )
  )
}
    
#' ChartaR_SHAP Server Functions
#'
#' @noRd 
#' 
mod_ChartaR_SHAP_server <- function(id, d, dt_update, weight, BoostaR_models, BoostaR_idx, feature_spec){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    initial_banding_1 <- reactiveVal(NULL)
    initial_banding_2 <- reactiveVal(NULL)
    banding_1 <- reactiveVal(1)
    banding_2 <- reactiveVal(1)
    observeEvent(input$feature_1, {
      banding_guess <- banding_guesser_numeric_date(d(), input$feature_1)
      initial_banding_1(banding_guess)
    })
    observeEvent(input$feature_2, {
      banding_guess <- banding_guesser_numeric_date(d(), input$feature_2)
      initial_banding_2(banding_guess)
    })
    banding_1_new <- mod_bandingChooser_server('feature_1_banding', d, reactive({input$feature_1}), initial_banding_1)
    banding_2_new <- mod_bandingChooser_server('feature_2_banding', d, reactive({input$feature_2}), initial_banding_2)
    observeEvent(banding_1_new(), {
      banding_1(banding_1_new())
    })
    observeEvent(banding_2_new(), {
      banding_2(banding_2_new())
    })
    observeEvent(c(BoostaR_models(), BoostaR_idx()), {
      if(!is.null(BoostaR_models()) & !is.null(BoostaR_idx())){
        features <- BoostaR_models()[[BoostaR_idx()]]$importances$Feature
        sel1 <- input$feature_1
        sel2 <- input$feature_2
        if(is.null(sel1)){
          sel1 <- character(0)
        } else if(sel1 %not_in% features){
          sel1 <- character(0)
        }
        if(is.null(sel2)){
          sel2 <- 'none'
        } else if(sel2 %not_in% features){
          sel2 <- 'none'
        }
        updateSelectInput(inputId = 'feature_1', choices = features, selected = sel1)
        updateSelectInput(inputId = 'feature_2', choices = c('none', features), selected = sel2)
      }
    })
    observeEvent(
      c(
        d(),
        dt_update(),
        banding_1(),
        banding_2(),
        input$feature_1,
        input$feature_2,
        input$feature_1_factor,
        input$feature_2_factor,
        input$quantile,
        input$rebase,
        input$SHAP_ribbons,
        feature_spec()
      ),
      {
        output$SHAP_plot <- renderPlotly({
          viz_SHAP_chart(
            d = d(),
            weight = weight(),
            feature_1 = input$feature_1,
            feature_2 = input$feature_2,
            banding_1 = banding_1(),
            banding_2 = banding_2(),
            factor_1 = input$feature_1_factor,
            factor_2 = input$feature_2_factor,
            SHAP_quantile = input$quantile,
            rebase = input$rebase,
            SHAP_ribbons = input$SHAP_ribbons,
            feature_spec = feature_spec()
            )
      })
    })
  })
}

#' @importFrom plotly add_surface
viz_SHAP_chart <- function(d, weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, SHAP_quantile, rebase, SHAP_ribbons, feature_spec){
  if(!is.null(d) & !is.null(weight)){
    if(SHAP_quantile=='-'){
      q <- 0
    } else {
      q <- as.numeric(substr(SHAP_quantile,1,nchar(SHAP_quantile)-1))/100
    }
    p <- plotly_empty(type = "scatter", mode = "markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = 'No plot to show',yref = "paper", y = 0.5)
      )
    if(!is.null(feature_1)){
      if(feature_1=='none') feature_1 <- NULL
    }
    if(!is.null(d) & !is.null(weight) & !is.null(feature_1)){
      c1 <- class(d[[feature_1]])
      if(factor_1){
        c1 <- 'factor'
      }
      if(is.null(feature_2)){
        feature_2 <- 'none'
      } else {
        if(feature_2!='none'){
          c2 <- class(d[[feature_2]])
          if(factor_2){
            c2 <- 'factor'
          }
        }
      }
      if(weight=='N'){
        idx <- 1:nrow(d)
      } else {
        idx <- which(d[[weight]]>0)
      }
      if(feature_2=='none'){
        # 1D chart
        if(c1 %in% c('integer','numeric') & !factor_1){
          # flame chart by bands
          p <- SHAP_flame(d[idx], weight, feature_1, banding_1, q, rebase, SHAP_ribbons, feature_spec)
        } else {
          # box and whisker in descending order by mean SHAP
          p <- SHAP_box_and_whisker(d[idx], weight, feature_1, banding_1, factor_1, q, rebase, feature_spec)
        }
      } else {
        # 2D chart
        if(c1 %in% c('integer','numeric') & c2 %in% c('integer','numeric')){
          # surface plot
          p <- SHAP_surface(d[idx], weight, feature_1, feature_2, banding_1, banding_2, q)
        } else if(!(c1 %in% c('integer','numeric')) & !(c2 %in% c('integer','numeric'))){
          # heat map, sorted on c1 something
          p <- SHAP_heatmap(d[idx], weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q)
        } else {
          # average SHAP value in bands for numerical feature cut by non-numerical feature
          p <- SHAP_lines(d[idx], weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q)
        }
      }
    }
    return(p)
  }
}

SHAP_flame <- function(d, weight, feature_1, banding_1, q, rebase, SHAP_ribbons, feature_spec){
  col1 <- paste0('lgbm_SHAP_', feature_1)
  banded <- band_var(d[[feature_1]], q, banding_1)
  SHAP_summary <- d[,c(min = lapply(.SD, min, na.rm = TRUE),
                       perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                       perc_25 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.25),
                       mean = lapply(.SD, mean, na.rm = TRUE),
                       perc_75 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.75),
                       perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                       max = lapply(.SD, max, na.rm = TRUE)
  ),
  banded,
  .SDcols = col1]
  cols <- c('min','perc_5','perc_25','mean','perc_75','perc_95','max')
  names(SHAP_summary)[2:8] <- cols
  setorderv(SHAP_summary, names(SHAP_summary)[1])
  # apply response transform
  base_level <- feature_spec$base_level[feature_spec$feature==feature_1]
  if(!shiny::isTruthy(base_level)) base_level <- character(0)
  base_adj <- NA
  if(length(base_level)>0){
    base_level <- as.numeric(base_level)
    base_level_banded <- floor(base_level/banding_1) * banding_1
    idx <- which(SHAP_summary[[1]]==base_level_banded)
    base_adj <- SHAP_summary[['mean']][idx]
  }
  if(rebase=='0'){
    if(!is.na(base_adj)){
      SHAP_summary[,(cols):=.SD-base_adj,.SDcols=cols]
    }
  } else if(rebase=='1'){
    if(is.na(base_adj)){
      SHAP_summary[,(cols):=exp(.SD),.SDcols=cols]
    } else {
      SHAP_summary[,(cols):=exp(.SD-base_adj),.SDcols=cols]
    }
  }
  p <- plot_ly()
  p <- p %>%
    add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['mean']], type = 'scatter', mode = 'lines', yaxis = "y1",
              line = list(color = 'rgba(200, 50, 50, 1.0)', dash = 'dot'),
              showlegend = TRUE, name = 'SHAP_mean')
  # 5th-95th percentiles
  if(SHAP_ribbons %in% c('All','5_95')){
    p <- p %>%
      add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_5']], type = 'scatter', mode = 'lines', yaxis = "y1",
                fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                showlegend = FALSE, name = 'SHAP_5') %>%
      add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_95']], type = 'scatter', mode = 'lines', yaxis = "y1",
                fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                showlegend = TRUE, name = 'SHAP_5_95')
  }
  # 25th-75th percentiles
  if(SHAP_ribbons %in% c('All','25_75','5_95')){
    p <- p %>%
      add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_25']], type = 'scatter', mode = 'lines', yaxis = "y1",
                fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                showlegend = FALSE, name = 'SHAP_25') %>%
      add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['perc_75']], type = 'scatter', mode = 'lines', yaxis = "y1",
                fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.2)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                showlegend = TRUE, name = 'SHAP_25_75')
  }
  # min to max SHAP
  if(SHAP_ribbons %in% c('All')){
    p <- p %>%
      add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['min']], type = 'scatter', mode = 'lines', yaxis = "y1",
                fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                showlegend = FALSE, name = 'SHAP_min') %>%
      add_trace(x = SHAP_summary[[1]], y = SHAP_summary[['max']], type = 'scatter', mode = 'lines', yaxis = "y1",
                fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                showlegend = TRUE, name = 'SHAP_min_max')
  }
  # formatting
  p <- p %>% layout(title = list(y= 0.98, text = boldify(paste0('SHAP flame plot: ',feature_1)), font = list(size = 16, face='bold')))
  return(p)
}
SHAP_box_and_whisker <- function(d, weight, feature_1, banding_1, factor_1, q, rebase, feature_spec){
  banded_1 <- NULL
  col1 <- paste0('lgbm_SHAP_', feature_1)
  cols <- c(feature_1, col1)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','SHAP'))
  # get base level
  base_level <- feature_spec$base_level[feature_spec$feature==feature_1]
  if(factor_1 & (class(d[[feature_1]]) %in% c('integer','numeric'))){
    base_level <- as.numeric(base_level)
    d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
    base_level <- band_var(base_level, q, banding_1)
  } else {
    d_cols[, banded_1 := feature_1]
  }
  # mean SHAP
  mean_SHAP <- d_cols[, lapply(.SD, mean, na.rm = TRUE), by = banded_1, .SDcols = 'SHAP']
  # base adjustment
  if(!shiny::isTruthy(base_level)) base_level <- character(0)
  base_adj <- NA
  if(length(base_level)>0){
    idx <- which(mean_SHAP[[1]]==base_level)
    base_adj <- mean_SHAP[['SHAP']][idx]
  } else {
    base_adj <- 0
  }
  # rebase
  if(rebase=='0'){
    d_cols[,2] <- d_cols[,2] - base_adj
  } else if(rebase=='1'){
    d_cols[,2] <- exp(d_cols[,2]-base_adj)
  }
  # reorder plot by mean_SHAP
  setorderv(mean_SHAP, 'SHAP', order = -1)
  xform <- list(autotick = TRUE,
                categoryorder = 'array',
                categoryarray = mean_SHAP[[1]])
  if(nrow(mean_SHAP)>500){
    p <- plotly_empty(type = "scatter", mode = "markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(title = list(text = 'Too many levels to display (>500)',yref = "paper", y = 0.5)
      )
  } else {
    if(nrow(d_cols)>50000){
      set.seed(42)
      d_cols <- d_cols[sample(1:.N, 50000, replace = FALSE),]
    }
    p <- plotly::plot_ly(
      y = d_cols[[2]],
      color = if(factor_1){as.factor(d_cols[[3]])} else {d_cols[[3]]},
      boxmean = TRUE,
      boxpoints = FALSE,
      type = 'box',
    ) %>%
      layout(legend = list(title=list(text='
<b>Box and whisker plot</b>
Each box spans Q1 to Q3
Median is the solid line within box
Mean is the dashed line
Whiskers extend to min/max <br>'),
                           x = 1.05,
                           y = 1.05,
                           size = 50,
                           font = list(size = 10)
      )
      ) %>%
      layout(xaxis = xform,
             title = list(text = boldify(paste0('SHAP box plot: ',feature_1)), font = list(size = 16, face='bold'))
      )
  }
  
  return(p)
}
SHAP_surface <- function(d, weight, feature_1, feature_2, banding_1, banding_2, q){
  SHAP <- NULL
  banded_1 <- NULL
  banded_2 <- NULL
  # two way summary
  col1 <- paste0('lgbm_SHAP_', feature_1)
  col2 <- paste0('lgbm_SHAP_', feature_2)
  cols <- c(feature_1, feature_2, col1, col2)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','feature_2','col1','col2'))
  d_cols[, SHAP := col1 + col2]
  d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
  d_cols[, banded_2 := band_var(feature_2, q, banding_2)]
  #d_cols[, banded_1 := floor(feature_1/banding_1) * banding_1]
  #d_cols[, banded_2 := floor(feature_2/banding_2) * banding_2]
  d_summary <- d_cols[, list(SHAP = mean(SHAP)), by = c('banded_1', 'banded_2')]
  d_summary <- dcast(d_summary, stats::as.formula('banded_1 ~ banded_2'), value.var = 'SHAP')
  p <- plot_ly(x = names(d_summary)[-1],
               y = d_summary[[1]],
               z = ~as.matrix(d_summary[,-1])) %>%
    add_surface(color = 'RdYlBlu') %>%
    layout(scene = list(xaxis = list(title = feature_2),
                        yaxis = list(title = feature_1),
                        zaxis = list(title = 'SHAP'),
                        camera = list(eye = list(x=0.8, y=-0.3, z=2))
    )
    )
  return(p)
}
SHAP_heatmap <- function(d, weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q){
  # two way summary
  SHAP <- NULL
  banded_1 <- NULL
  banded_2 <- NULL
  col1 <- paste0('lgbm_SHAP_', feature_1)
  col2 <- paste0('lgbm_SHAP_', feature_2)
  cols <- c(feature_1, feature_2, col1, col2)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','feature_2','col1','col2'))
  d_cols[, SHAP := col1 + col2]
  if(factor_1 & is.numeric(d[[feature_1]])){
    d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
  } else {
    d_cols[, banded_1 := feature_1]
  }
  if(factor_2 & is.numeric(d[[feature_2]])){
    d_cols[, banded_2 := band_var(feature_2, q, banding_2)]
  } else {
    d_cols[, banded_2 := feature_2]
  }
  d_summary <- d_cols[, list(SHAP = mean(SHAP)), by = c('banded_1', 'banded_2')]
  d_summary <- dcast(d_summary, stats::as.formula('banded_1 ~ banded_2'), value.var = 'SHAP')
  
  p <- plotly::plot_ly(
    # x = d_summary[[1]],
    # y = d_summary[[2]],
    # z = d_summary[['SHAP']],
    x = names(d_summary)[-1],
    y = d_summary[[1]],
    z = as.matrix(d_summary[,-1]),
    colors = grDevices::colorRamp(c('green', 'white', 'red')),
    type = "heatmap") %>%
    # add_annotations(
    #   x = names(d_summary)[-1],
    #   y = d_summary[[1]],
    #   text =  as.matrix(d_summary[,-1]),
    #   showarrow = FALSE) %>%
    layout(plot_bgcolor='rgb(200, 200, 200)') %>%
    layout(xaxis = list(showgrid = FALSE, title = feature_2), yaxis = list(showgrid = FALSE, title = feature_1))
  return(p)
}
SHAP_lines <- function(d, weight, feature_1, feature_2, banding_1, banding_2, factor_1, factor_2, q){
  SHAP <- NULL
  banded_1 <- NULL
  banded_2 <- NULL
  col1 <- paste0('lgbm_SHAP_', feature_1)
  col2 <- paste0('lgbm_SHAP_', feature_2)
  if(factor_1 | (!factor_2 & is.numeric(d[[feature_2]]))){
    # swap around and make feature_1 the numeric feature
    temp <- feature_1
    feature_1 <- feature_2
    feature_2 <- temp
    temp <- banding_1
    banding_1 <- banding_2
    banding_2 <- temp
    temp <- factor_1
    factor_1 <- factor_2
    factor_2 <- temp
  }
  cols <- c(feature_1, feature_2, col1, col2)
  d_cols <- d[,.SD,.SDcols=cols]
  setnames(d_cols, c('feature_1','feature_2','col1','col2'))
  d_cols[, SHAP := col1 + col2]
  d_cols[, banded_1 := band_var(feature_1, q, banding_1)]
  if(is.numeric(d[[feature_2]])){
    d_cols[, banded_2 := band_var(feature_2, q, banding_2)]
  } else {
    d_cols[, banded_2 := feature_2]
  }
  d_summary <- d_cols[, list(SHAP = mean(SHAP)), by = c('banded_1', 'banded_2')]
  setorderv(d_summary, names(d_summary)[1])
  p <- plot_ly(d_summary,
               x = d_summary[[1]],
               y = d_summary[['SHAP']],
               color = as.factor(d_summary[[2]]),
               type = 'scatter',
               mode = 'lines',
               hovertemplate = paste('(%{x}, %{y:.3f})')
  ) %>%
    plotly::layout(
      title = list(text = boldify(paste0('SHAP lines plot: ',feature_1, ' x ', feature_2)), font = list(size = 14), y= 0.98),
      xaxis = list(title = feature_1),
      yaxis = list(title = 'SHAP'),
      legend = list(title=list(text=boldify(feature_2))),
      hovermode = 'x'
    )
  return(p)
}
band_var <- function(x, q, b){
  lower_cutoff <- stats::quantile(x, prob = q, na.rm = TRUE)
  upper_cutoff <- stats::quantile(x, prob = 1-q, na.rm = TRUE)
  pmax(lower_cutoff, pmin(upper_cutoff, floor(x/b) * b))
}