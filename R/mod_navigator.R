#' navigator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets radioGroupButtons
mod_navigator_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      style = 'margin-top:0px; margin-bottom:-30px; padding-top:0px ; padding-bottom:0px;  border-radius: 3px 3px 0px 0px;',
      radioGroupButtons(
        inputId = ns('type'),
        label = 'KPIs (0/0)',
        choices = c(`<i class='fa fa-gears'></i>` = 'KPI',
                    `<i class='fa fa-rocket'></i>` = 'GBM', 
                    `<i class='fa fa-star'></i>` = 'GLM'),
        selected = 'KPI',
        size = 'sm',
        justified = TRUE,
        width = '100%',
      )
    ),
    div(
      style = 'margin-bottom:-15px;',
      conditionalPanel(sprintf("input['%s'] == 'KPI'", ns("type")),
                       selectInput(inputId=ns('kpi_chooser'),
                                   label = NULL,
                                   selectize = FALSE,
                                   size = 8,
                                   width = '100%',
                                   choices = NULL
                       )
      ),
      conditionalPanel(sprintf("input['%s'] == 'GLM'", ns("type")),
                       selectInput(inputId=ns('glm_chooser'),
                                   label = NULL,
                                   selectize = FALSE,
                                   size = 8,
                                   width = '100%',
                                   choices = NULL
                       )
      ),
      conditionalPanel(sprintf("input['%s'] == 'GBM'", ns("type")),
                       selectInput(inputId=ns('gbm_chooser'),
                                   label = NULL,
                                   selectize = FALSE,
                                   size = 8,
                                   width = '100%',
                                   choices = NULL
                       )
      )
    )
  )
}
    
#' navigator Server Functions
#'
#' @importFrom stats setNames
#'
#' @noRd 
mod_navigator_server <- function(id, kpi_spec, GlimmaR_models, BoostaR_models, GlimmaR_idx, BoostaR_idx){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(kpi_spec(), {
      updateSelectInput(inputId = 'kpi_chooser', choices = setNames(1:nrow(kpi_spec()), kpi_spec()[[1]]))
    })
    observeEvent(GlimmaR_models(), {
      if(!is.null(GlimmaR_models())){
        current_selection <- input$glm_chooser
        model_names <- names(GlimmaR_models())
        if(is.null(current_selection)){
          selected <- model_names[length(model_names)]
        }
        else if(current_selection %in% model_names){
          selected <- current_selection
        } else {
          selected <- model_names[length(model_names)]
        }
        updateSelectInput(inputId = 'glm_chooser', choices = names(GlimmaR_models()), selected = selected)
      }
    })
    observeEvent(BoostaR_models(), {
      if(!is.null(BoostaR_models())){
        current_selection <- input$gbm_chooser
        model_names <- names(BoostaR_models())
        if(is.null(current_selection)){
          selected <- model_names[length(model_names)]
        }
        else if(current_selection %in% model_names){
          selected <- current_selection
        } else {
          selected <- model_names[length(model_names)]
        }
        updateSelectInput(inputId = 'gbm_chooser', choices = names(BoostaR_models()), selected = selected)
      }
    })
    observeEvent(BoostaR_idx(), {
      if(!is.null(BoostaR_idx())){
        if(is.null(input$gbm_chooser)){
          updateSelectInput(inputId = 'gbm_chooser', choices = names(BoostaR_models()), selected = BoostaR_idx())
          # QUESTION - next line stops circular reactivity between BoostaR_idx and input$gbm_chooser. Why does it work?
        } else if(input$gbm_chooser != BoostaR_idx()){
          updateSelectInput(inputId = 'gbm_chooser', choices = names(BoostaR_models()), selected = BoostaR_idx())
        }
      }
    })
    observeEvent(GlimmaR_idx(), {
      if(!is.null(GlimmaR_idx())){
        if(is.null(input$glm_chooser)){
          updateSelectInput(inputId = 'glm_chooser', choices = names(GlimmaR_models()), selected = GlimmaR_idx())
          # QUESTION - next line stops circular reactivity between BoostaR_idx and input$gbm_chooser. Why does it work?
        } else if(input$glm_chooser != GlimmaR_idx()){
          updateSelectInput(inputId = 'glm_chooser', choices = names(GlimmaR_models()), selected = GlimmaR_idx())
        }
      }
    })
    observeEvent(c(input$type, input$kpi_chooser, input$gbm_chooser, input$glm_chooser), {
      lbl_icn <- nav_label(input$type, kpi_spec(), BoostaR_models(), GlimmaR_models())
      updateRadioButtons(inputId = 'type', label = lbl_icn$label)
    })
    return(reactive({list(kpi=input$kpi_chooser, glm=input$glm_chooser, gbm=input$gbm_chooser)}))
  })
}

nav_label <- function(type, kpi_spec, BoostaR_models, GlimmaR_models){
  if(type=='KPI'){
    n <- nrow(kpi_spec)
    icon <- icon('gears')
  } else if (type=='GLM'){
    n <- length(GlimmaR_models)
    icon <- icon('star')
  } else if (type=='GBM'){
    n <- length(BoostaR_models)
    icon <- icon('rocket')
  }
  return(list(label=paste0(type,'s (',n,')'), icon=icon))
}


