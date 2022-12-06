#' navigateGlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput
mod_GlimmaR_navigate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        h3('GlimmaR model summary')
      ),
      column(width = 6,
             align = 'right',
             style = 'margin-top:16px; padding-right:16px; padding-bottom:0px',
             actionButton(
               inputId = ns('delete_model'),
               label = 'Delete',
               icon = icon("minus-circle"),
               class = 'btn-danger'
             ),
             actionButton(
               inputId = ns('make_active'),
               label = 'Make active',
               icon = icon("chevron-right")
             ),
             actionButton(
               inputId = ns('generate_predictions'),
               label = 'Predict',
               icon = icon("chevron-right")
             ),
             shinySaveButton(
               id = ns('save_model'),
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
             DTOutput(ns('model_summary')
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
        DTOutput(ns('detailed_model_summary'))
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
                ns('search_gain_table'),
                label = NULL,
                width = '100%',
                placeholder = 'highlight feature'
              )
            )
          )
        ),
        DTOutput(ns('gain_summary'))
      )
    )
  )
}
    
#' navigateGlimmaR Server Functions
#'
#' @noRd 
mod_GlimmaR_navigate_server <- function(id, GlimmaR_models, GlimmaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(GlimmaR_models(), {
      output$model_summary <- DT::renderDT({
        # model summary table
        dt <- GlimmaR_model_summary(GlimmaR_models())
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
    })
  })
}

GlimmaR_model_summary <- function(GlimmaR_models){
  # takes the key info from the BoostaR_models
  # and makes a summary data table
  rows <- lapply(GlimmaR_models, GlimmaR_model_summary_row)
  rbindlist(rows)
}
GlimmaR_model_summary_row <- function(GlimmaR_model){
  x <- data.table(name =  GlimmaR_model$name,
                  time = round(GlimmaR_model$time, 1),
                  data = GlimmaR_model$dataset_name,
                  train = GlimmaR_model$training_data,
                  response = GlimmaR_model$response,
                  weight = GlimmaR_model$weight,
                  obj = GlimmaR_model$objective,
                  terms = GlimmaR_model$num_terms,
                  dev = signif(GlimmaR_model$deviance, 6),
                  AIC = signif(GlimmaR_model$AIC, 6),
                  dispersion = signif(GlimmaR_model$dispersion, 6),
                  NAs = GlimmaR_model$count_NAs
  )
  return(x)
}