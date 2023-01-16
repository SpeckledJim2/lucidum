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
                 style = 'padding-right: 0px',
                 radioGroupButtons(
                   inputId = ns('group_low_exposure'),
                   label = "Group low weights",
                   choices = c(0,5,10,20,50,'0.1%','1%'),
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
              inputId = ns('sigma_bars'),
              label = "Sigma bars",
              choices = c('-'=0,'2'=2,'3'=3,'4'=4,'5'=5),
              individual = FALSE,
              size = 'xs',
              selected = 0
            )
          ),
          column(
            width = 3,
            style = 'padding-right: 0px',
            radioGroupButtons(
              inputId = ns('show_partial_dependencies'),
              label = "Partial dependencies",
              choices = c('-','GLM','GBM','GBM-','Both'),
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
                   br(),
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
mod_ChartaR_line_and_bar_server <- function(id, d, dt_update, response, weight, kpi_spec, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx, filters){
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
      if(banding_new()!=banding()){
        banding(banding_new())
      }
    })
    observeEvent(c(dt_update(), response(), weight(), x_col(), add_cols(), banding(), kpi_spec(), feature_spec(), input$group_low_exposure, input$show_partial_dependencies, input$sigma_bars, input$response_transform, input$sort), {
      # QUESTION - how to stop this triggering twice on first call
      if(!is.null(BoostaR_idx())){
        gbm_link <- BoostaR_models()[[BoostaR_idx()]]$link
      }
      if(!is.null(GlimmaR_idx())){
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
          input$sigma_bars,
          input$response_transform,
          kpi_spec(),
          feature_spec(),
          gbm_link,
          glm_link)
        )
    })
    observeEvent(data_summary(), {
      output$one_way_table <- DT::renderDT({format_table_DT(data_summary(), response(), weight(), kpi_spec(), feature_spec(), input$response_transform)})
      output$chart <- renderPlotly({
        format_plotly(data_summary(),
                      response(),
                      weight(),
                      input$show_labels,
                      input$show_response,
                      input$sigma_bars,
                      kpi_spec(),
                      feature_spec(),
                      input$response_transform,
                      filters()
                      )
        })
    })
  })
}

line_and_bar_summary <- function(d, response, weight, group_by_col, add_cols, banding, group_low_exposure, sort, show_partial_dependencies, sigma_bars, response_transform, kpi_spec, feature_spec, gbm_link, glm_link){
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
            } else if (group_low_exposure=='0.1%'){
              q_low <- quantile(g[rows_idx], prob = 0.001, na.rm = TRUE)[[1]]
              q_high <- quantile(g[rows_idx], prob = 0.999, na.rm = TRUE)[[1]]
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
            # group low weight groups into a single group
            if(group_low_exposure!='0' & weight != 'no weights'){
              if(weight %in% c('N','no weights')){
                d_summary <- d[rows_idx, .(weight = .N), group_by_col]
              } else {
                d_summary <- d[rows_idx, .(weight = lapply(.SD, sum, na.rm = TRUE)), group_by_col, .SDcols = weight]
              }
              if(group_low_exposure=='1%'){
                min_exposure <- 0.01 * sum(d_summary[,2])
              } else if(group_low_exposure=='0.1%'){
                min_exposure <- 0.001 * sum(d_summary[,2])
              } else {
                min_exposure <- as.numeric(group_low_exposure)
              }
              low_weight_groups <- as.character(d_summary[weight<min_exposure, 1][[1]])
              # replace low exposure groups with "Small weights"
              banded_col <- as.character(g[rows_idx])
              banded_col[banded_col %in% low_weight_groups] <- 'Small weights'
              new_colname <- group_by_col
            } else {
              banded_col <- group_by_col
              new_colname <- group_by_col
            }
          }
          # assemble the columns we need in the summary
          if(weight %in% c('N','no weights')){
            cols_to_summarise <- c(response, add_cols)
          } else {
            cols_to_summarise <- c(weight, response, add_cols)
          }
          # make banded_col a factor if a character
          if(length(banded_col)>1 & inherits(banded_col, 'character')){
            banded_col <- as.factor(banded_col)
          }
          # summarise data
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
          if (show_partial_dependencies %in% c('GBM','GBM-','Both')){
            SHAP_col <- paste0('lgbm_SHAP_', group_by_col)
            if(!(SHAP_col %in% names(d))){
              SHAP_col <- NULL
            }
          }
          if(!is.null(SHAP_col)){
            if(length(rows_idx)==nrow(d)){
              SHAP_summary <- d[,c(min = lapply(.SD, min, na.rm = TRUE),
                                   perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                                   perc_25 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.25),
                                   mean = lapply(.SD, mean, na.rm = TRUE),
                                   perc_75 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.75),
                                   perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                                   max = lapply(.SD, max, na.rm = TRUE)
              ),
              banded_col,
              .SDcols = SHAP_col]
            } else {
              SHAP_summary <- d[rows_idx,
                                c(min = lapply(.SD, min, na.rm = TRUE),
                                  perc_5 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.05),
                                  perc_25 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.25),
                                  mean = lapply(.SD, mean, na.rm = TRUE),
                                  perc_75 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.75),
                                  perc_95 = lapply(.SD, stats::quantile, na.rm = TRUE, probs = 0.95),
                                  max = lapply(.SD, max, na.rm = TRUE)
                                ),
                                banded_col,
                                .SDcols = SHAP_col]
            }
            setorderv(SHAP_summary, names(SHAP_summary)[1])
            names(SHAP_summary)[2:8] <- c('min','perc_5','perc_25','mean','perc_75','perc_95','max')
            # remove min and max if selected
            n_SHAP_cols <- 8
            if(show_partial_dependencies=='GBM-'){
              SHAP_summary[, c('min','max'):=NULL]
              n_SHAP_cols <- 6
            }
            # scale SHAP values to mean
            # how to do this depends on the choice of objective
            if(!is.null(gbm_link)){
              if(gbm_link=='identity'){
                if(weight %in% c('N','no weights')){
                  wtd_SHAP_mean <- sum(d_summary[,2]*SHAP_summary[['mean']], na.rm = TRUE)/sum(d_summary[,2], na.rm = TRUE)
                } else {
                  wtd_SHAP_mean <- sum(d_summary[,3]*SHAP_summary[['mean']], na.rm = TRUE)/sum(d_summary[,3], na.rm = TRUE)
                }
                SHAP_summary[, 2:n_SHAP_cols] <-  SHAP_summary[, 2:n_SHAP_cols] + wtd_mean - wtd_SHAP_mean
              } else if (gbm_link=='log'){
                SHAP_summary[, 2:n_SHAP_cols] <- exp(SHAP_summary[, 2:n_SHAP_cols]) # exponentiate for log link
                if(weight %in% c('N','no weights')){
                  wtd_SHAP_mean <- sum(d_summary[,2]*SHAP_summary[['mean']], na.rm = TRUE)/sum(d_summary[,2], na.rm = TRUE)
                } else {
                  wtd_SHAP_mean <- sum(d_summary[,3]*SHAP_summary[['mean']], na.rm = TRUE)/sum(d_summary[,3], na.rm = TRUE)
                }
                SHAP_summary[, 2:n_SHAP_cols] <- SHAP_summary[, 2:n_SHAP_cols] * wtd_mean / wtd_SHAP_mean # scale to mean
              } else if (gbm_link=='logit'){
                # won't tie up due to logit, following leads to sensible chart
                SHAP_summary[, 2:n_SHAP_cols] <- exp(SHAP_summary[, 2:n_SHAP_cols])/(1+exp(SHAP_summary[, 2:n_SHAP_cols])) * wtd_mean * 2
              }
            }
            # multiply SHAP_summary by row weights
            # needed for later weighted average removal of rows
            if(weight=='N'){
              SHAP_summary[, 2:n_SHAP_cols] <- SHAP_summary[, 2:n_SHAP_cols] * d_summary[[2]]
            } else {
              SHAP_summary[, 2:n_SHAP_cols] <- SHAP_summary[, 2:n_SHAP_cols] * d_summary[[3]]
            }
            d_summary <- cbind(d_summary, SHAP_summary[, 2:n_SHAP_cols])
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
            # scale LP values to mean
            # how to do this depends on the choice of objective
            if(!is.null(glm_link)){
              if(glm_link=='identity'){
                if(weight %in% c('N','no weights')){
                  wtd_LP_mean <- sum(d_summary[,2]*LP_summary[['LP_mean']], na.rm = TRUE)/sum(d_summary[,2], na.rm = TRUE)
                } else {
                  wtd_LP_mean <- sum(d_summary[,3]*LP_summary[['LP_mean']], na.rm = TRUE)/sum(d_summary[,3], na.rm = TRUE)
                }
                LP_summary[, 2] <- LP_summary[, 2] + wtd_mean - wtd_LP_mean
              } else if (glm_link=='log'){
                
                LP_summary[, 2] <- exp(LP_summary[, 2]) # exponentiate for log link
                if(weight %in% c('N','no weights')){
                  wtd_LP_mean <- sum(d_summary[,2]*LP_summary[['LP_mean']], na.rm = TRUE)/sum(d_summary[,2], na.rm = TRUE)
                } else {
                  wtd_LP_mean <- sum(d_summary[,3]*LP_summary[['LP_mean']], na.rm = TRUE)/sum(d_summary[,3], na.rm = TRUE)
                }
                LP_summary[, 2] <- LP_summary[, 2] * wtd_mean / wtd_LP_mean # scale to mean
              } else if (glm_link=='logit'){
                # won't tie up due to logit, following leads to sensible chart
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
              } else {
                base_level_banded <- base_level
              }
              idx <- which(d_summary[[1]]==base_level)
              if(length(idx)==1){
                cols <- names(d_summary)[first_col:ncol(d_summary)]
                denominator <- d_summary[idx, .SD, .SDcols = cols]
                if(!is.null(SHAP_col)){
                  if(n_SHAP_cols==6){
                    denominator[, (c('perc_5','perc_25','perc_75','perc_95')) := mean] # what to adjust to the mean, not the percentiles
                  } else if (n_SHAP_cols==8){
                    denominator[, (c('min','perc_5','perc_25','perc_75','perc_95','max')) := mean] # what to adjust to the mean, not the percentiles
                  }
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
              } else {
                base_level_banded <- base_level
              }
              idx <- which(d_summary[[1]]==base_level_banded)
              if(length(idx)==1){
                cols <- names(d_summary)[first_col:ncol(d_summary)]
                denominator <- d_summary[idx, .SD, .SDcols = cols]
                if(!is.null(SHAP_col)){
                  if(n_SHAP_cols==6){
                    denominator[, (c('perc_5','perc_25','perc_75','perc_95')) := mean] # what to adjust to the mean, not the percentiles
                  } else if (n_SHAP_cols==8){
                    denominator[, (c('min','perc_5','perc_25','perc_75','perc_95','max')) := mean] # what to adjust to the mean, not the percentiles
                  }
                }
                rebased_values <- mapply('/',d_summary[, .SD, .SDcols=cols], denominator)
                if(inherits(rebased_values, 'matrix')){
                  rebased_values <- data.table(rebased_values)
                } else {
                  # only one row of numbers, data.table won't keep column names so need next line
                  rebased_values <- setDT(data.frame(as.list(rebased_values)))
                }
                d_summary[, (cols) := rebased_values]
              }
            }
          }
          # sigma bars - only permit if not transforming the response
          sigma_bars <- as.numeric(sigma_bars)
          if(response_transform != '-'){
            sigma_bars <- 0
          }
          if(length(add_cols)==1 & sigma_bars!=0){
            fitted <- add_cols[1]
            if(inherits(banded_col, 'character')){
              # low exposure rows
              bands <- d[[banded_col]][rows_idx]
            } else {
              bands <- banded_col
            }
            sigmas_by_group <- get_sd_estimate_by_group(d, rows_idx, response, weight, fitted, add_cols, bands, 10, sigma_bars)
            d_summary[, sigma_bar:= sigmas_by_group[,2]]
          }
          # sort table
          first_col_name <- names(d_summary)[1]
          setnames(d_summary, old = first_col_name, new = new_colname)
          if(sort=='A-Z'){
            setorderv(d_summary, new_colname)
          } else if(sort=='Wt'){
            setorderv(d_summary, weight, -1)
          } else if(sort=='Act'){
            setorderv(d_summary, response, -1)
          } else if (sort=='Add'){
            if(length(add_cols)>0){
              first_add_col <- add_cols[1]
              setorderv(d_summary, first_add_col, -1)
            } else {
              setorderv(d_summary, response, -1)
            }
          } else if (sort=='PD'){
            if(show_partial_dependencies %in% c('Both','GBM','GBM-')){
              # can't sort by both so sort by SHAP
              if('mean' %in% names(d_summary)){
                setorderv(d_summary, 'mean')
              } else if ('LP_mean' %in% names(d_summary)) {
                setorderv(d_summary, 'LP_mean' -1)
              } else {
                setorderv(d_summary, new_colname, -1)
              }
            } else if (show_partial_dependencies=='GLM'){
              if('LP_mean' %in% names(d_summary)){
                setorderv(d_summary, 'LP_mean', -1)
              } else {
                setorderv(d_summary, new_colname, -1)
              }
            } else {
              setorderv(d_summary, new_colname, -1)
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
  #if(all(c('min','perc_5','mean','perc_95','max') %in% names(dt))) SHAP_cols <- TRUE else SHAP_cols <- FALSE
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

format_table_DT <- function(dt, response, weight, kpi_spec, feature_spec, response_transform){
  if(!is.null(dt) & !is.null(response) & !is.null(weight)){
    # add a total row at the bottom
    dt <- add_total_row(dt, weight)
    # highlight the base level row if there is one
    # get the variable name
    col <- names(dt)[1]
    if(substr(col,nchar(col)-6,nchar(col))=='_banded'){
      # strip of the suffix _banded if it is present
      col <- substr(col,1,nchar(col)-7)
    }
    # get the base level and row
    base_level <- NULL
    if(!is.null(feature_spec)){
      base_level <- feature_spec[feature==col,base_level]
      base_level <- as.character(base_level) # first column of dt is character
      if(length(base_level)==0){
        base_level <- NULL
      } else if (base_level %not_in% dt[[1]]){
        base_level <- NULL
      }
    }
    #dt[,3:ncol(dt)] <- round(dt[,3:ncol(dt)],4)
    # apply the kpi format
    if(weight=='N'){
      weight_cols <- 'N'
      first_response_col <- 3
    } else {
      weight_cols <- c('N', weight)
      first_response_col <- 4
    }
    if(response_transform == '-'){
      formatted_kpis <- apply_kpi_format(as.matrix(dt[,first_response_col:ncol(dt)]), response, weight, kpi_spec)
    } else {
      formatted_kpis <- apply_kpi_format(as.matrix(dt[,first_response_col:ncol(dt)]), 'dummy', 'dummy', kpi_spec)
    }
    formatted_kpis <- as.data.table(formatted_kpis)
    cols <- names(dt)[first_response_col:ncol(dt)]
    setnames(formatted_kpis, cols)
    dt[, (cols):= formatted_kpis]
    
    # to format NAs
    rowCallback <- c(
      "function(row, data){",
      "  for(var i=0; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('NA')",
      "        .css({'color': 'rgb(150,150,150)', 'font-style': 'italic'});",
      "    }",
      "  }",
      "}"  
    )
    
    DT <- datatable(
      dt,
      rownames= FALSE,
      extensions = 'Buttons',
      options = list(pageLength = min(5000, nrow(dt)),
                     rowCallback = JS(rowCallback),
                     scrollX = T,
                     dom = 'Bfrtip',
                     columnDefs = list(list(className = 'dt-right', targets = "_all")),
                     scrollY = 'calc(90vh - 370px)'
      )
    ) |>
      formatStyle(1:ncol(dt), lineHeight='0%', fontSize = '14px') |>
      formatStyle(names(dt), target = "row", fontWeight = DT::styleEqual('Total', 'bold')) |>
      formatRound(weight_cols, digits = 0)
    if(!is.null(base_level)){
      DT <- DT |>
        formatStyle(columns = 1, target='row', backgroundColor = styleEqual(base_level, rgb(100/255,180/255,220/255)))
    }
    return(DT)
  }
}

#' @importFrom plotly plotly_empty add_text
format_plotly <- function(dt, response, weight, show_labels, show_response, sigma_bars, kpi_spec, feature_spec, response_transform, filters){
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
  } else if (nrow(dt)>10000){
    # nothing to display - return message to user
    p <- plotly_empty(type = "scatter", mode = "markers") |>
      config(displayModeBar = FALSE) |>
      layout(title = list(text = 'Too many rows (>10,000) to display - view table instead',yref = "paper", y = 0.5))
  } else {
    p <- plot_ly()
    p <- p |> layout(font=list(family = 'Helvetica Neue'))
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
    SHAP_cols <- 0
    if(all(c('perc_5','perc_25','mean','perc_75','perc_95') %in% names(dt))) SHAP_cols <- 5
    if(all(c('min','perc_5','perc_25','mean','perc_75','perc_95','max') %in% names(dt))) SHAP_cols <- 7
    # remove rows with zero weight for plot (they make SHAP values look funny)
    include <- dt[[first_line_col-1]] > 0
    dt <- dt[include]
    # check for LP col
    if('LP_mean' %in% names(dt)) LP_col <- TRUE else LP_col <- FALSE
    # check for sigma_bar col - don't show if response_transform selected
    if('sigma_bar' %in% names(dt) & response_transform == '-') sigma_col <- TRUE else sigma_col <- FALSE
    # last non SHAP or LP line
    last_line_col <- ncol(dt) - SHAP_cols - ifelse(LP_col,1,0) - ifelse(sigma_col,1,0)
    last_ex_sigma <- ncol(dt) - ifelse(sigma_col,1,0)
    # setup plot
    xform <- list()
    yform <- list()
    yform2 <- list()
    # xform
    xform$xaxis_type <- 'category'
    xform$categoryorder <- 'array'
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
      yform2$range <- return_y_axis_limits(as.matrix(dt[, first_line_col:last_ex_sigma]))
    } else if (show_response=='Hide'){
      if(ncol(dt)==last_line_col){
        col <- first_line_col
      } else {
        col <- last_line_col+1
      }
      yform2$range <- return_y_axis_limits(as.matrix(dt[, col:ncol(dt)]))
    }
    yform2$showgrid <- TRUE
    yform2$title <- boldify(names(dt)[first_line_col])
    # plotly won't plot NAs
    # so replace with "NA"
    col <- names(dt)[[1]]
    dt[,(col):=lapply(.SD, \(x){x[is.na(x)] <- 'NA';x}), .SDcols = col]
    #dt[,(col):=lapply(.SD, as.factor), .SDcols = col]
    xform$categoryarray <- dt[[1]]
    # get the base level and row
    if(substr(col,nchar(col)-6,nchar(col))=='_banded'){
      # strip of the suffix _banded if it is present
      col <- substr(col,1,nchar(col)-7)
    }
    base_level <- NULL
    if(!is.null(feature_spec)){
      base_level <- feature_spec[feature==col,base_level]
      base_level <- as.character(base_level) # first column of dt is character
      if(length(base_level)==0){
        base_level <- NULL
      } else if (base_level %not_in% dt[[1]]){
        base_level <- NULL
      }
    }
    # add the bars, with distinct colours for NA, Small weights and the base level
    colours <- rep('rgba(200, 240, 250,1.0)', nrow(dt))
    na_col <- which(dt[[1]]=='NA')
    X_col <- which(dt[[1]]=='Small weights')
    base_col <- which(dt[[1]]==base_level)
    colours[base_col] <- 'rgba(100,180,220,1.0)'
    colours[na_col] <- 'rgba(150,150,150,1.0)'
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
    if(show_response=='Show'){
      # add the lines
      for(i in first_line_col:last_line_col){
        pc <- plot_colour(i-first_line_col+1)
        if(i==first_line_col+1 & sigma_col==TRUE){
          errors_plot <- list(
            color = '#888888',
            thickness = 0.5,
            type = "data",
            symmetric = TRUE,
            array = dt[['sigma_bar']]
          )
        } else {
          errors_plot <- NULL
        }
        p <- add_trace(p,
                       x = dt[[1]],
                       y = dt[[i]],
                       type = 'scatter',
                       mode = 'lines+markers',
                       yaxis = 'y2',
                       name = names(dt)[i],
                       marker = list(color = pc, size = 5),
                       line = list(color = pc, width = 2),
                       error_y = errors_plot
        )
      }
    }
    # filter text for title
    filter_text <- filters$train_test_filter
    train_test_filter_text <- filters$user_filter
    if(filter_text=='All'){filter_text <- ''}
    if(filter_text=='Train'){filter_text <- 'training data'}
    if(filter_text=='Test'){filter_text <- 'test data'}
    # rescale text for title
    rescale_text <- ''
    if(response_transform %in% c('0','1')){
      if(is.null(base_level)){
        rescale_text <- 'rebased'
      } else {
        rescale_text <- paste0('rebased, base level: ', base_level)
      }
    }
    # make the chart
    p <- p |> layout(xaxis = xform,
                     yaxis = yform,
                     yaxis2 = yform2,
                     margin = list(r = 100, l = 50, t = 50),
                     title = list(
                       x = 0.03,
                       y = 0.97,
                       text = paste0(boldify(yform2$title), filter_text, ' ', train_test_filter_text, ' ', rescale_text), font = list(size = 16)
                       )
    ) |>
      layout(legend = list(traceorder = 'normal',
                           orientation = 'v',
                           title=list(text='<b> Click to show/hide</b>'),
                           x = 1.05,
                           y = 1.05,
                           font = list(size = 10)
                           )
             )
    if(SHAP_cols>0){
      # add SHAP ribbons
      # mean
      # remove rows from d with NAs for SHAP values
      p <- p %>%
        add_trace(x = dt[[1]], y = dt[['mean']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  line = list(color = 'rgba(200, 50, 50, 1.0)', dash = 'dot'),
                  showlegend = TRUE, name = 'SHAP_mean')
      # 25th-75th percentiles
      p <- p %>%
        add_trace(x = dt[[1]], y = dt[['perc_25']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = FALSE, name = 'SHAP_25') %>%
        add_trace(x = dt[[1]], y = dt[['perc_75']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = TRUE, name = 'SHAP_25_75')
      # 5th-95th percentiles
      p <- p %>%
        add_trace(x = dt[[1]], y = dt[['perc_5']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fillcolor='rgba(200, 50, 50, 0.3)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = FALSE, name = 'SHAP_5') %>%
        add_trace(x = dt[[1]], y = dt[['perc_95']], type = 'scatter', mode = 'lines', yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.2)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                  showlegend = TRUE, name = 'SHAP_5_95')
      # min to max SHAP
      if(SHAP_cols==7){
        p <- p %>%
          add_trace(x = dt[[1]], y = dt[['min']], type = 'scatter', mode = 'lines', yaxis = "y2",
                    fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                    showlegend = FALSE, name = 'SHAP_min') %>%
          add_trace(x = dt[[1]], y = dt[['max']], type = 'scatter', mode = 'lines', yaxis = "y2",
                    fill = 'tonexty', fillcolor='rgba(200, 50, 50, 0.1)', line = list(color = 'rgba(200, 50, 50, 0.0)'),
                    showlegend = TRUE, name = 'SHAP_min_max')
      }
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
get_sd_estimate_by_group <- function(d, rows_idx, response, weight, fitted, add_cols, group_by_col, n_samples, sigma_bars){
  n_groups <- 20
  n_rows <- length(rows_idx)
  # create n random columns with numbers from 1 to n_groups
  set.seed(42)
  rand_cols <- matrix(sample.int(n_groups,size=n_samples*n_rows,replace=TRUE), nrow=length(rows_idx), ncol=n_samples)
  # for each sample, create response-fitted cut by group_by_col and samples
  for(i in 1:n_samples){
    random_col <- rand_cols[,i]
    # assemble the columns we need in the summary
    if(weight %in% c('N','no weights')){
      cols_to_summarise <- c(response, add_cols[1])
    } else {
      cols_to_summarise <- c(weight, response, add_cols[1])
    }
    # grab just the columns we need
    d_subset <- cbind(group_by_col, random_col, d[rows_idx, ..cols_to_summarise])
    d_summary <- d_subset[, c(count = .N, lapply(.SD, sum, na.rm = TRUE)), c('group_by_col', 'random_col'), .SDcols = cols_to_summarise]
    # divide by weight if specified
    if(weight == 'N'){
      first_col <- 4
      # divide all summary columns (4th onwards) by the weight (number of rows) column (3rd)
      d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[3]]
    } else if (weight != 'no weights'){
      first_col <- 5
      # divide all summary columns (5th onwards) by the weight column (4th)
      d_summary[, first_col:ncol(d_summary)] <- d_summary[, first_col:ncol(d_summary)] / d_summary[[4]]
    }
    # calculate the sd for each group
    d_summary[, difference:= d_summary[[first_col]] - d_summary[[first_col+1]] ]
    sigma <- d_summary[, sd(difference), by = 'group_by_col']
    setorder(sigma, 'group_by_col')
    if(!exists('sigmas')){
      sigmas <- matrix(0,nrow=nrow(sigma),ncol=n_samples)
    }
    sigmas[,i] <- sigma[[2]]
  }
  # average over samples, normalise with sqrt and multiply by chosen sigma_bars
  sigmas <- cbind(sigma[,1], sigma = rowMeans(sigmas) * sigma_bars / sqrt(n_groups))
  return(sigmas)
}