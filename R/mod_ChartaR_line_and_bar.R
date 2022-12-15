#' ChartaR_line_and_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ChartaR_line_and_bar_ui <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        selectInput_ui(id = ns('x_axis_feature'), label = 'x-axis feature', height_divisor = 50, height_adj = 2, multiple = FALSE, initial = 'Original'),
        selectInput_ui(id = ns('add_columns'), label = 'Additional y-axis columns', height_divisor = 55, height_adj = 3, multiple = TRUE, initial = 'lucidum')
      ),
      column(
        width = 9,
        fluidRow(
          column(3,
                 radioGroupButtons(
                   inputId = ns('sort'),
                   label = 'x-axis sort order',
                   choices = c('A-Z', 'Wt', 'Act','Add','PD'),
                   individual = FALSE,
                   size = 'xs',
                   selected = 'A-Z')
          ),
          column(3,
                 radioGroupButtons(
                   inputId = ns('group_low_exposure'),
                   label = "Group low weights",
                   choices = c(0,5,10,20,50,'1%'),
                   individual = FALSE,
                   size = 'xs',
                   selected = 0)
          ),
          column(2,
                 align = 'left',
                 radioGroupButtons(
                   inputId = ns('show_labels'),
                   label = "Labels",
                   choices = c('-', 'Weight','All'),
                   individual = FALSE,
                   size = 'xs',
                   selected = '-'
                 )
          ),
          column(4,
                 align = 'right',
                 mod_bandingChooser_ui(ns('x_banding'))
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioGroupButtons(
              inputId = "ChartaR_1W_error_bars",
              label = "Error bars",
              choices = c('-'='-', '90%'=0.9, '95%'=0.95, '99%'=0.99),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = ns('show_partial_dependencies'),
              label = "Partial dependencies",
              choices = c('-','GLM','GBM','Both'),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          ),
          column(
            width = 3,
            radioGroupButtons(
              inputId = ns('show_response'),
              label = "Response",
              choices = c('Hide','Show'),
              individual = FALSE,
              size = 'xs',
              selected = 'Show')
          ),
          column(
            width = 3,
            align = 'right',
            radioGroupButtons(
              inputId = ns('response_transform'),
              label = "Response transform",
              choices = c('-', 'Log','Exp','Logit','0','1'),
              individual = FALSE,
              size = 'xs',
              selected = '-'
            )
          )
        ),
        tabsetPanel(
          id = 'ChartaR_one_way_tabs',
          type = 'tabs',
          tabPanel('Chart',
                   plotlyOutput(ns('chart'), height = 'calc(92vh - 280px)')
                   
          ),
          tabPanel('Table',
                   DTOutput(ns('one_way_table'))
          )
        )
      )
    )
  )
}
    
#' ChartaR_line_and_bar Server Functions
#'
#' @noRd 
mod_ChartaR_line_and_bar_server <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    data_summary <- reactiveVal(NULL)
    initial_banding <- reactiveVal(NULL)
    banding <- reactiveVal(NULL)
    x_col <- selectInput_server(id = 'x_axis_feature', d, dt_update, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx, FALSE)
    add_cols <- selectInput_server(id = 'add_columns', d, dt_update, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx, TRUE)
    observeEvent(x_col(), {
      banding_guess <- banding_guesser_numeric_date(d(), x_col())
      initial_banding(banding_guess)
      banding(banding_guess)
    })
    banding_new <- mod_bandingChooser_server('x_banding', d, x_col, initial_banding)
    observeEvent(banding_new(), {
      banding(banding_new())
    })
    observeEvent(c(dt_update(), response(), weight(), x_col(), add_cols(), banding(), kpi_spec(), feature_spec(), input$group_low_exposure, input$show_partial_dependencies, input$response_transform, input$sort), {
      # QUESTION - how to stop this triggering twice on first call
      if(!is.null(BoostaR_idx())){
        gbm_link <- BoostaR_models()[[BoostaR_idx()]]$link
      }
      if(!is.null(GlimmaR_models())){
        glm_link <- GlimmaR_models()[[GlimmaR_idx()]]$link
      }
      data_summary(
        line_and_bar_summary(
          d(),
          response(),
          weight(),
          x_col(),
          add_cols(),
          banding(),
          input$group_low_exposure,
          input$sort,
          input$show_partial_dependencies,
          input$response_transform,
          kpi_spec(),
          feature_spec(),
          gbm_link,
          glm_link)
        )
    })
    observeEvent(data_summary(), {
      output$one_way_table <- DT::renderDT({format_table_DT(data_summary(), response(), weight(), kpi_spec(), input$response_transform)})
      output$chart <- renderPlotly({
        format_plotly(data_summary(),
                      response(),
                      weight(),
                      input$show_labels,
                      input$show_response,
                      kpi_spec()
                      )
        })
    })
  })
}

line_and_bar_summary <- function(d, response, weight, group_by_col, add_cols, banding, group_low_exposure, sort, show_partial_dependencies, response_transform, kpi_spec, feature_spec, gbm_link, glm_link){
  if(!is.null(d) & !is.null(response) & !is.null(weight) & !is.null(group_by_col) & !is.null(banding)){
    if(response!='' & weight !=''){
      d_cols <- names(d)
      if(response %in% d_cols &
         group_by_col %in% d_cols &
         all(add_cols %in% d_cols) &
         weight %in% c('N',d_cols)){
        g <- d[[group_by_col]]
        if(!(is.numeric(g) & banding=='0')){
          rows_idx <- which(d[['total_filter']]==1)
          # band the variable if numeric or date
          if(is.numeric(g) & banding!='0'){
            # band the numerical variable for plotting
            banding <- as.numeric(banding)
            banded <- floor(g/banding) * banding
            # if percentage hide_low_exposure selected, group the low exposure rows
            if (group_low_exposure=='1%'){
              q_low <- quantile(g[rows_idx], prob = 0.01, na.rm = TRUE)[[1]]
              q_high <- quantile(g[rows_idx], prob = 0.99, na.rm = TRUE)[[1]]
              q_low_banded <- floor(q_low/banding) * banding
              q_high_banded <- (1+floor(q_high/banding)) * banding
              banded <- pmax(q_low_banded, pmin(q_high_banded, banded))
            }
            banded_col <- banded[rows_idx]
            new_colname <- paste0(group_by_col, '_banded')
          } else if (inherits(g,'Date') & banding!='0'){
            if(banding=='Day'){
              # day
              banded <- g
              new_colname <- paste0(group_by_col, '_day')
            } else if (banding=='Week'){
              # week
              banded <- 100*year(g) + week(g)
              new_colname <- paste0(group_by_col, '_week')
            } else if (banding=='Mnth'){
              # month
              banded <- 100*year(g) + month(g)
              new_colname <- paste0(group_by_col, '_month')
            } else if (banding=='Qtr'){
              # quarter
              banded <- 100*year(g) + floor((month(g)-1)/3)+1
              new_colname <- paste0(group_by_col, '_quarter')
            } else if (banding=='Year'){
              # year
              banded <- year(g)
              new_colname <- paste0(group_by_col, '_year')
            }
            banded_col <- banded[rows_idx]
          } else {
            banded_col <- group_by_col
            new_colname <- group_by_col
          }
          # assemble the columns we need in the summary
          if(weight %in% c('N','no weights')){
            cols_to_summarise <- c(response, add_cols)
          } else {
            cols_to_summarise <- c(weight, response, add_cols)
          }
          # summarise and order
          if(length(rows_idx)==nrow(d)){
            d_summary <- d[, c(count = .N, lapply(.SD, sum, na.rm = TRUE)), banded_col, .SDcols = cols_to_summarise]
          } else {
            d_summary <- d[rows_idx, c(count = .N, lapply(.SD, sum, na.rm = TRUE)), banded_col, .SDcols = cols_to_summarise]
          }
          setorderv(d_summary, names(d_summary)[1])
          # extract weighted mean as needed later on to calibrate SHAP values
          if(weight %in% c('N','no weights')){
            wtd_mean <- sum(d_summary[,3], na.rm = TRUE)/sum(d_summary[,2], na.rm = TRUE)
          } else {
            wtd_mean <- sum(d_summary[,4], na.rm = TRUE)/sum(d_summary[,3], na.rm = TRUE)
          }
          # SHAP summary
          SHAP_col <- NULL
          if (show_partial_dependencies %in% c('GBM','Both')){
            SHAP_col <- paste0('lgbm_SHAP_', group_by_col)
            if(!(SHAP_col %in% names(d))){
              SHAP_col <- NULL
            }
          }
          if(!is.null(SHAP_col)){
            if(length(rows_idx)==nrow(d)){
              SHAP_summary <- d[,c(min = lapply(.SD, min, na.rm = TRUE),
                                   perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                                   mean = lapply(.SD, mean, na.rm = TRUE),
                                   perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                                   max = lapply(.SD, max, na.rm = TRUE)
              ),
              banded_col,
              .SDcols = SHAP_col]
            } else {
              SHAP_summary <- d[rows_idx,
                                c(min = lapply(.SD, min, na.rm = TRUE),
                                  perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                                  mean = lapply(.SD, mean, na.rm = TRUE),
                                  perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                                  max = lapply(.SD, max, na.rm = TRUE)
                                ),
                                banded_col,
                                .SDcols = SHAP_col]
            }
            setorderv(SHAP_summary, names(SHAP_summary)[1])
            names(SHAP_summary)[2:6] <- c('min','perc_5','mean','perc_95','max')
            # scale SHAP values to mean
            # how to do this depends on the choice of objective
            if(!is.null(gbm_link)){
              if(gbm_link=='identity'){
                SHAP_summary[, 2:6] <- wtd_mean + SHAP_summary[, 2:6]
              } else if (gbm_link=='log'){
                SHAP_summary[, 2:6] <- exp(SHAP_summary[, 2:6]) * wtd_mean
              } else if (gbm_link=='binary'){
                # following is rough for now - won't always tie up due to logit
                SHAP_summary[, 2:6] <- exp(SHAP_summary[, 2:6])/(1+exp(SHAP_summary[, 2:6])) * wtd_mean * 2
              }
            }
            # multiply SHAP_summary by row weights
            # needed for later weighted average removal of rows
            if(weight=='N'){
              SHAP_summary[, 2:6] <- SHAP_summary[, 2:6] * d_summary[[2]]
            } else {
              SHAP_summary[, 2:6] <- SHAP_summary[, 2:6] * d_summary[[3]]
            }
            d_summary <- cbind(d_summary, SHAP_summary[, 2:6])
          }
          # LP summary
          LP_col <- NULL
          if (show_partial_dependencies %in% c('GLM','Both')){
            LP_col <- paste0('glm_LP_', group_by_col)
            if(!(LP_col %in% names(d))){
              LP_col <- NULL
            }
          }
          if(!is.null(LP_col)){
            if(length(rows_idx)==nrow(d)){
              LP_summary <- d[, lapply(.SD, mean, na.rm = TRUE), banded_col, .SDcols = LP_col]
            } else {
              LP_summary <- d[rows_idx, lapply(.SD, mean, na.rm = TRUE), banded_col, .SDcols = LP_col]
            }
            setorderv(LP_summary, names(LP_summary)[1])
            names(LP_summary)[2] <- c('LP_mean')
            # scale SHAP values to mean
            # how to do this depends on the choice of objective
            if(!is.null(glm_link)){
              if(glm_link=='identity'){
                LP_summary[, 2] <- wtd_mean + LP_summary[, 2]
              } else if (glm_link=='log'){
                LP_summary[, 2] <- exp(LP_summary[, 2]) * wtd_mean
              } else if (glm_link=='logit'){
                # following is rough for now - won't always tie up due to logit
                LP_summary[, 2] <- exp(LP_summary[, 2])/(1+exp(LP_summary[, 2])) * wtd_mean * 2
              }
            }
            # multiply LP_summary by row weights
            # needed for later weighted average removal of rows
            if(weight=='N'){
              LP_summary[, 2] <- LP_summary[, 2] * d_summary[[2]]
            } else {
              LP_summary[, 2] <- LP_summary[, 2] * d_summary[[3]]
            }
            d_summary <- cbind(d_summary, LP_summary[, 2])
          }
          # group low exposure rows and remove from summary
          if(!is.numeric(g)){
            min_exposure <- 0
            if(group_low_exposure!='0' & weight != 'no weights'){
              if(group_low_exposure=='1%'){
                if(weight=='N'){
                  min_exposure <- 0.01 * sum(d_summary[,2])
                } else {
                  min_exposure <- 0.01 * sum(d_summary[,3])
                }
              } else {
                min_exposure <- as.numeric(group_low_exposure)
              }
              if(weight=='N'){
                low_exposure_rows <- which(d_summary[[2]]<min_exposure)
              } else {
                low_exposure_rows <- which(d_summary[[3]]<min_exposure)
              }
              low_exposure_summary <- d_summary[low_exposure_rows,
                                                lapply(.SD, sum, na.rm = TRUE),
                                                .SDcols = 2:ncol(d_summary)]
              if(nrow(low_exposure_summary)>0){
                col <- names(d_summary)[1]
                low_exposure_summary[, (col) := 'Small weights']
                d_summary <- d_summary[-low_exposure_rows,]
                d_summary <- rbind(d_summary, low_exposure_summary)
              }
            }
          }
          # divide by weight if specified
          if(weight == 'N'){
            first_col <- 3
            # divide all summary columns (3rd onwards) by the weight column (2nd)
            d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[2]]
          } else if (weight != 'no weights'){
            first_col <- 4
            # divide all summary columns (4rd onwards) by the weight column (3rd)
            d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[3]]
          }
          # apply response transformation
          if(response_transform=='Log'){
            d_summary[, first_col:ncol(d_summary)] <- log(d_summary[, first_col:ncol(d_summary)])
          } else if (response_transform=='Exp'){
            d_summary[, first_col:ncol(d_summary)] <- exp(d_summary[, first_col:ncol(d_summary)])
          } else if (response_transform=='Logit'){
            d_summary[, first_col:ncol(d_summary)] <- log(d_summary[, first_col:ncol(d_summary)]/(1-d_summary[, first_col:ncol(d_summary)]))
          } else if (response_transform=='0'){
            base_level <- feature_spec$base_level[feature_spec$feature==group_by_col]
            if(!shiny::isTruthy(base_level)) base_level <- character(0)
            if(length(base_level)>0){
              if(is.numeric(d[[group_by_col]])){
                base_level <- as.numeric(base_level)
                base_level_banded <- floor(base_level/banding) * banding
              }
              idx <- which(d_summary[[1]]==base_level)
              if(length(idx)==1){
                cols <- names(d_summary)[first_col:ncol(d_summary)]
                denominator <- d_summary[idx, .SD, .SDcols = cols]
                if(!is.null(SHAP_col)){
                  denominator[, (c('min','perc_5','perc_95','max')) := mean] # what to adjust to the mean, not the percentiles
                }
                rebased_values <- as.data.table(mapply('-',d_summary[, .SD, .SDcols=cols], denominator))
                d_summary[, (cols) := rebased_values]
              }
            }
          } else if (response_transform=='1'){
            base_level <- feature_spec$base_level[feature_spec$feature==group_by_col]
            if(!shiny::isTruthy(base_level)) base_level <- character(0)
            if(length(base_level)>0){
              if(is.numeric(d[[group_by_col]])){
                base_level <- as.numeric(base_level)
                base_level_banded <- floor(base_level/banding) * banding
              }
              idx <- which(d_summary[[1]]==base_level)
              if(length(idx)==1){
                cols <- names(d_summary)[first_col:ncol(d_summary)]
                denominator <- d_summary[idx, .SD, .SDcols = cols]
                if(!is.null(SHAP_col)){
                  denominator[, (c('min','perc_5','perc_95','max')) := mean] # want to adjust to the mean, not the percentiles
                }
                rebased_values <- as.data.table(mapply('/',d_summary[, .SD, .SDcols=cols], denominator))
                d_summary[, (cols) := rebased_values]
              }
            }
          }
          # sort table
          first_col_name <- names(d_summary)[1]
          setnames(d_summary, old = first_col_name, new = new_colname)
          if(sort=='A-Z'){
            setorderv(d_summary, new_colname)
          } else if(sort=='Wt'){
            setorderv(d_summary, weight, -1)
          } else if(sort=='Act'){
            setorderv(d_summary, response, +1)
          } else if (sort=='Add'){
            if(length(add_cols)>0){
              first_add_col <- add_cols[1]
              setorderv(d_summary, first_add_col, +1)
            } else {
              setorderv(d_summary, response, +1)
            }
          } else if (sort=='PD'){
            if(show_partial_dependencies %in% c('Both','GBM')){
              # can't sort by both so sort by SHAP
              if('mean' %in% names(d_summary)){
                setorderv(d_summary, 'mean')
              } else if ('LP_mean' %in% names(d_summary)) {
                setorderv(d_summary, 'LP_mean')
              } else {
                setorderv(d_summary, new_colname)
              }
            } else if (show_partial_dependencies=='GLM'){
              if('LP_mean' %in% names(d_summary)){
                setorderv(d_summary, 'LP_mean')
              } else {
                setorderv(d_summary, new_colname)
              }
            } else {
              setorderv(d_summary, new_colname, +1)
            }
          }
        }
      }
    }
  }
}
banding_guesser_numeric_date <- function(d, col){
  type <- 'NULL'
  # get type of column
  if(!is.null(col) & !is.null(d)){
    if(col %in% names(d)){
      if(inherits(d[[col]],c('factor','character'))){
        type <- 'character'
      } else if (inherits(d[[col]],c('numeric','integer'))){
        type <- 'numeric'
      } else if (inherits(d[[col]],'Date')){
        type <- 'date'
      } else if (col=='none'){
        type <- 'NULL'
      }
    }
  }
  # calculate what banding should be chosen initially
  # and what should be shown on the widget
  if(type=='numeric'){
    b <- banding_guesser(d[[col]])
  } else if(type=='date'){
    b <- banding_guesser_date(d[[col]])
  } else {
    b <- 0
  }
}

add_total_row <- function(dt, weight){
  if(all(c('min','perc_5','mean','perc_95','max') %in% names(dt))) SHAP_cols <- TRUE else SHAP_cols <- FALSE
  if(weight=='N'){
    weight_cols <- 'N'
    weight_extra <- 0
  } else {
    weight_cols <- c('N',weight)
    weight_extra <- 1
  }
  wt <- dt[[weight]]
  response_cols <- names(dt)[(3+weight_extra):ncol(dt)]
  first_col_name <- names(dt)[1]
  total_cell <- dt[1,1]
  total_cell[,1] <- as.character(total_cell[,1])
  total_cell[1,1] <- 'Total'
  weight_totals <- dt[,lapply(.SD,sum,na.rm=TRUE),.SDcols=weight_cols]
  sumprod <- function(x,wt){sum(x*wt, na.rm=TRUE)}
  response_totals <- dt[,lapply(.SD,sumprod,wt=wt),.SDcols=response_cols]
  response_totals <- response_totals / weight_totals[[ncol(weight_totals)]]
  total_row <- cbind(total_cell, weight_totals, response_totals)
  dt[, (first_col_name) := lapply(.SD, as.character), .SDcols=first_col_name]
  rbind(dt, total_row)
}

format_table_DT <- function(dt, response, weight, kpi_spec, response_transform){
  # add a total row at the bottom
  dt <- add_total_row(dt, weight)
  dt[,3:ncol(dt)] <- round(dt[,3:ncol(dt)],2)
  if(weight=='N'){
    weight_cols <- 'N'
    first_response_col <- 3
  } else {
    weight_cols <- c('N', weight)
    first_response_col <- 4
  }
  # apply the kpi format
  if(response_transform == '-'){
    formatted_kpis <- apply_kpi_format(as.matrix(dt[,first_response_col:ncol(dt)]), response, weight, kpi_spec)
  } else {
    formatted_kpis <- apply_kpi_format(as.matrix(dt[,first_response_col:ncol(dt)]), 'dummy', 'dummy', kpi_spec)
  }
  formatted_kpis <- as.data.table(formatted_kpis)
  cols <- names(dt)[first_response_col:ncol(dt)]
  setnames(formatted_kpis, cols)
  dt[, (cols):= formatted_kpis]
  datatable(
    dt,
    rownames= FALSE,
    options = list(pageLength = min(1000, nrow(dt)),
                   scrollX = T,
                   dom = 'tp',
                   columnDefs = 
                   list(list(className = 'dt-right', targets = "_all")),
                   scrollY = 'calc(90vh - 300px)'
    )
  ) |>
    formatStyle(1:ncol(dt), lineHeight='0%', fontSize = '12px') |>
    #formatStyle(first_response_col:ncol(dt), `text-align` = 'right') |>
    formatStyle(names(dt), target = "row", fontWeight = DT::styleEqual('Total', 'bold')) |>
    formatRound(weight_cols, digits = 0)
}

#' @importFrom plotly plotly_empty add_text
format_plotly <- function(dt, response, weight, show_labels, show_response, kpi_spec){
  if(is.null(dt)){
    # nothing to display - return message to user
    p <- plotly_empty(type = "scatter", mode = "markers") |>
      config(displayModeBar = FALSE) |>
      layout(title = list(text = 'Select x-axis feature',yref = "paper", y = 0.5))
  } else if (nrow(dt)==0){
    # nothing to display - return message to user
    p <- plotly_empty(type = "scatter", mode = "markers") |>
      config(displayModeBar = FALSE) |>
      layout(title = list(text = 'Select x-axis feature',yref = "paper", y = 0.5))
  } else {
    p <- plot_ly()
    # make the first column character
    dt[[1]] <- as.character(dt[[1]])
    # work out first_line_col
    if(weight=='N'){
      first_line_col <- 3
    } else if (weight=='no weights'){
      first_line_col <- 3
    } else {
      first_line_col <- 4
    }
    # setup plot parameters
    # check for SHAP cols
    if(all(c('min','perc_5','mean','perc_95','max') %in% names(dt))) SHAP_cols <- TRUE else SHAP_cols <- FALSE
    # remove rows with zero weight for plot (they make SHAP values look funny)
    include <- dt[[first_line_col-1]] > 0
    dt <- dt[include]
    # check for LP col
    if('LP_mean' %in% names(dt)) LP_col <- TRUE else LP_col <- FALSE
    # last non SHAP or LP line
    last_line_col <- ncol(dt) - ifelse(SHAP_cols,5,0) - ifelse(LP_col,1,0)
    # setup plot
    xform <- list()
    yform <- list()
    yform2 <- list()
    # xform
    xform$xaxis_type <- 'category'
    xform$categoryorder <- 'array'
    xform$categoryarray <- dt[[1]]
    xform$autotick <- FALSE
    xform$title <- boldify(ifelse(nrow(dt)>200, paste0(names(dt)[1], ' (', nrow(dt), ' levels)'), names(dt)[1]))
    xform$range <- NULL
    xform$showgrid <- FALSE
    xform$tickfont <- list(size = min(12,max(6,500/nrow(dt))))
    xform$showticklabels <- ifelse(nrow(dt)>200,FALSE,TRUE)
    # yform
    yform$showgrid <- FALSE
    yform$side <- 'right'
    yform$title <- boldify(names(dt)[first_line_col-1])
    yform$range <- return_bar_limits(dt[[first_line_col-1]])
    # yform2
    yform2$xaxis_type <- 'category'
    yform2$overlaying <- 'y'
    yform2$side <- 'left'
    if(show_response=='Show'){
      yform2$range <- return_y_axis_limits(as.matrix(dt[,first_line_col:ncol(dt)]))
    } else if (show_response=='Hide'){
      if(ncol(dt)==last_line_col){
        col <- first_line_col
      } else {
        col <- last_line_col+1
      }
      yform2$range <- return_y_axis_limits(as.matrix(dt[,col:ncol(dt)]))
    }
    yform2$showgrid <- TRUE
    yform2$title <- boldify(names(dt)[first_line_col])
    # add the bars, with distinct colours for NA and X
    colours <- rep('rgba(200, 240, 250,1.0)', nrow(dt))
    na_col <- which(dt[[1]]=='NA')
    X_col <- which(dt[[1]]=='Small weights')
    colours[na_col] <- 'rgba(255,150,150,1.0)'
    colours[X_col] <- 'rgba(255,150,150,1.0)'
    # choose weight labels
    if(show_labels=='-' | nrow(dt)>200){
      weight_text_template <- NULL
    } else if (show_labels %in% c('Weight','All')){
      weight_text_template <- '%{y:.3s}'
    }
    # add weight bars
    p <- add_trace(p,
                   x = dt[[1]],
                   y = dt[[first_line_col-1]],
                   name = names(dt)[first_line_col-1],
                   type = 'bar',
                   marker = list(color = colours),
                   yaxis = 'y',
                   texttemplate = weight_text_template,
                   textposition = 'outside',
                   textfont = list(color = 'rgba(100, 120, 125,1.0)')
    )
    # add on the response lines
    last_line_col <- ncol(dt) - ifelse(SHAP_cols,5,0) - ifelse(LP_col,1,0)
    if(show_response=='Show'){
      # add the lines
      for(i in first_line_col:last_line_col){
        pc <- plot_colour(i-first_line_col+1)
        p <- add_trace(p,
                       x = dt[[1]],
                       y = dt[[i]],
                       type = 'scatter',
                       mode = 'lines+markers',
                       yaxis = 'y2',
                       name = names(dt)[i],
                       marker = list(color = pc, size = 5),
                       line = list(color = pc, width = 2)
        )
      }
    }
    # format the chart
    filter_text <- ''
    train_test_filter_text <- ''
    p <- p |> layout(xaxis = xform,
                     yaxis = yform,
                     yaxis2 = yform2,
                     margin = list(r = 100, l = 50, t = 50),
                     title = list(text = paste0(boldify(yform2$title), filter_text, ' ', train_test_filter_text), font = list(size = 16, face='bold'))
    ) |>
      layout(legend = list(traceorder = 'normal',
                           orientation = 'v',
                           title=list(text='<b> Click to show/hide</b>'),
                           x = 1.05,
                           y = 1.05,
                           font = list(size = 10)
                           )
             )
    if(SHAP_cols){
      # add SHAP ribbons
      # mean
      # remove rows from d with NAs for SHAP values
      p <- p %>%
        add_trace(x = dt[[1]], y = dt[['mean']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  line = list(color = 'rgba(200, 50, 50, 1.0)', dash = 'dot'),
                  showlegend = TRUE, name = 'SHAP_mean')
      # 5th-95th percentiles
      p <- p %>%
        add_trace(x = dt[[1]], y = dt[['perc_5']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = FALSE, name = 'SHAP_5') %>%
        add_trace(x = dt[[1]], y = dt[['perc_95']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = TRUE, name = 'SHAP_5_95')
      # min to max SHAP
      p <- p %>%
        add_trace(x = dt[[1]], y = dt[['min']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = FALSE, name = 'SHAP_min') %>%
        add_trace(x = dt[[1]], y = dt[['max']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = TRUE, name = 'SHAP_min_max')
    }
    if(LP_col){
      p <- p %>%
        add_trace(x = dt[[1]], y = dt[['LP_mean']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  line = list(color = 'rgba(50, 50, 200, 1.0)', dash = 'dot'),
                  showlegend = TRUE, name = 'LP')
    }
    if(show_labels=='All'){
      range <- max(dt[[first_line_col]], na.rm = TRUE) - min(dt[[first_line_col]], na.rm = TRUE)
      p <- p %>%
        add_text(x = dt[[1]],
                 y = dt[[first_line_col]] + range * 0.03,
                 text = apply_kpi_format(dt[[first_line_col]], response, weight, kpi_spec),
                 textfont = list(size = 10, color = 'black'),
                 textposition = "top",
                 yaxis = 'y2',
                 bgcolor = 'white',
                 showlegend = FALSE)
    }
  }
  p
}

return_y_axis_limits <- function(y){
  # return nicer limits than plotly defaults
  ymin <- min(y[is.finite(y)], na.rm = TRUE)
  ymax <- max(y[is.finite(y)], na.rm = TRUE)
  range <- ymax - ymin
  ymin_plot <- ymin - range * 0.05
  ymax_plot <- ymax + range * 0.1
  # if ymin_plot is close to zero vs the range then make zero
  if(abs(ymin_plot)/range<0.3 & ymin>=0){
    ymin_plot <- 0
  } else {
    # adjustment to get line out of way of bars
    ymin_plot <- ymin_plot - range * 0.3
  }
  return(c(ymin_plot,ymax_plot))
}
return_bar_limits <- function(y){
  # return nicer limits than plotly defaults
  ymin <- min(y, na.rm = TRUE)
  ymax <- max(y, na.rm = TRUE)
  ymin_plot <- 0
  ymax_plot <- ymax * 5
  return(c(ymin_plot,ymax_plot))
}
boldify <- function(x){
  paste('<b>', x, '</b>')
}
plot_colour <- function(x){
  # set up custom colours for up to three plots
  # after that it's all grey
  if(x==1){
    # dark grey
    pc <- 'rgb(40, 40, 40)'
  } else if (x==2){
    # nice red
    pc <- 'rgb(240, 50, 50)'
  } else if (x==3){
    # nice blue
    pc <- 'rgb(50, 60, 240)'
  } else if (x>3){
    # grey
    pc <- 'rgb(100, 100, 100)'
  }
  pc
}