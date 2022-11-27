#' buildGlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_buildGlimmaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    absolutePanel(id = "GlimmaR_helper_panel",
                  class = "panel panel-default",
                  top = '60px',
                  right = '14px',
                  width = '160px',
                  fixed=TRUE,
                  draggable = TRUE,
                  height = "auto",
                  style = "opacity: 1.0; z-index: 10;",
                  fluidRow(
                    column(width = 6,
                           dropdownButton(inputId = 'GlimmaR_helper_dropdown',
                                          width = 700,
                                          up = FALSE,
                                          circle = FALSE,
                                          size = 'default',
                                          label = 'Formula helper',
                                          right = TRUE,
                                          margin = '10px',
                                          fluidRow(
                                            column(
                                              width = 6,
                                              div(
                                                radioGroupButtons(
                                                  inputId = 'GlimmaR_helper_feature_choice',
                                                  choices = c('Original','A-Z','GBM'),
                                                  size = 's',
                                                  label = 'Feature',
                                                  justified = TRUE
                                                ),
                                                style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
                                              ),
                                              div(
                                                textInput(
                                                  inputId = 'GlimmaR_helper_search',
                                                  width = '100%',
                                                  label = NULL,
                                                  placeholder = 'filter'
                                                ),
                                                style = 'margin-top:0px; margin-bottom:-15px;'
                                              ),
                                              selectInput(
                                                inputId = 'GlimmaR_helper_feature',
                                                label = NULL,
                                                choices = NULL,
                                                multiple = FALSE,
                                                selectize = FALSE,
                                                size = 15
                                              )
                                            ),
                                            column(
                                              width = 6,
                                              div(
                                                radioGroupButtons(
                                                  inputId = 'GlimmaR_helper_levels_choice',
                                                  choices = c('Single','Group'),
                                                  size = 's',
                                                  label = 'Factor grouping/function selection',
                                                  justified = TRUE,
                                                ),
                                                style = 'margin-top:0px; margin-bottom:-15px; padding-top:0px ; padding-bottom:0px'
                                              ),
                                              div(
                                                textInput(
                                                  inputId = 'GlimmaR_helper_level_text',
                                                  width = '100%',
                                                  label = NULL,
                                                  placeholder = 'function arguments seperated by commas'
                                                ),
                                                style = 'margin-top:0px; margin-bottom:-15px;'
                                              ),
                                              selectInput(
                                                inputId = 'GlimmaR_helper_levels',
                                                label = NULL,
                                                choices = NULL,
                                                multiple = TRUE,
                                                selectize = FALSE,
                                                size = 15
                                              )
                                            )
                                          ),
                                          fluidRow(
                                            column(
                                              width = 12,
                                              textAreaInput(
                                                inputId = 'GlimmaR_formula_suggestion',
                                                label = NULL,
                                                width = '100%',
                                                height = '200px',
                                                resize = 'none'
                                              )
                                            )
                                          )
                           )
                    ),
                    column(width = 3,
                           style = 'padding-left:0px; padding-right:0px',
                           # actionButton(
                           #   inputId = "GlimmaR_prev_model",
                           #   label = "<",
                           #   style = 'padding-left: 12px; padding-right:12px'
                           #   ),
                           # actionButton(
                           #   inputId = "GlimmaR_next_model",
                           #   label = ">",
                           #   style = 'padding-left: 12px; padding-right:12px'
                           #   )
                    ),
                    column(width = 3,
                           align = 'center',
                           style = 'padding-left:0px; padding-right:0px'
                           # htmlOutput('GlimmaR_model_index')
                    )
                  )
    ),
    fluidRow(
      column(
        width = 5,
        fluidRow(
          column(
            width = 4,
            h3("Formula")
          ),
          column(
            width = 8,
            align = 'right',
            br(),
            actionButton(
              inputId = "GlimmaR_tabulate",
              label = 'Tabulate',
              icon = icon("table"),
            ),
            shinyFilesButton(
              id = 'GlimmaR_formula_load',
              label = '',
              filetype=list(txt="txt"),
              icon = icon('download'),
              title = 'Choose formula',
              style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              multiple = FALSE
            ),
            shinySaveButton(
              id = 'GlimmaR_formula_save',
              label = '',
              title = 'Choose location to save formula',
              filename = "",
              filetype=list(txt="txt"),
              icon = icon('upload'),
              style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              viewtype = "detail"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = 'GlimmaR_objective',
              width = '100%',
              label = 'Family',
              choices = list('identity link' = list('gaussian'),
                             'log link' = list('poisson',
                                               'quasipoisson',
                                               'gamma',
                                               'tweedie'),
                             'logit link' = list('binomial')
              )
            )
          ),
          column(
            width = 4,
            align = 'right',
            radioGroupButtons(
              inputId = 'GlimmaR_tabulate_format',
              label = 'Tabulate format',
              choices = c('solo','long'),
              selected = 'solo'
            )
          ),
          column(
            width = 5,
            align = 'right',
            radioGroupButtons(
              inputId = 'GlimmaR_tabulate_scale',
              label = 'Tabulate function',
              choices = c('link','response'),
              selected = 'response'
            )
          )
        ),
        tags$style(".form-group.shiny-input-container { width: 100%; }"),
        tags$style("#GlimmaR_glm_formula {font-size:14px;}"),
        tags$script("
                         Shiny.addCustomMessageHandler('glm_formula_text_size', function(size) {
                          var box = document.getElementById('GlimmaR_glm_formula');
                          box.style.fontSize = size;
                         });"),
        # $(document).on("shiny:connected", function(e) {
        # $(document).on("shiny:inputchanged", function(e) {
        # tags$script(
        # 'var selection = [0,0];
        #  $("#GlimmaR_glm_formula").on("input selectionchange propertychange", function(e) {
        #    var ctl = document.getElementById("GlimmaR_glm_formula");
        #    var startPos = ctl.selectionStart;
        #    var endPos = ctl.selectionEnd;
        #    selection[0] = startPos;
        #    selection[1] = endPos;
        #    Shiny.onInputChange("formula_selected_text", selection);
        #  });'
        # ),
        tags$head(tags$script('
                          // Define function to set height of "GlimmaR_glm_formula"
                          setHeight_GlimmaR_formula_box = function() {
                            var window_height = $(window).height();
                            var header_height = $(".main-header").height();
                            var h = (window_height - header_height - 335)+"px" ;
                            var box = document.getElementById("GlimmaR_glm_formula");
                            box.style.height = h;
                          };
                          // Set input$box_height when the connection is established
                          $(document).on("shiny:connected", function(event) {
                            setHeight_GlimmaR_formula_box();
                          });
                          // Refresh the box height on every window resize event
                          $(window).on("resize", function(){
                            setHeight_GlimmaR_formula_box();
                          });')),
        textAreaInput(
          inputId = "GlimmaR_glm_formula",
          value = 'Edit the GLM formula...',
          label = NULL,
          height = '480px',
          resize = 'none'
        ),
        fluidRow(
          column(
            width = 6,
            align = 'right',
            radioGroupButtons(
              inputId = "GlimmaR_data_to_use",
              justified =  TRUE,
              label = NULL,
              choices = c('All rows', 'Training only'),
              selected = 'Training only'
            )
          ),
          column(
            width = 3,
            align = 'right',
            actionButton(
              inputId = "GlimmaR_textsize_minus",
              label = "A-",
              style = 'padding-left: 8px; padding-right:8px'
            ),
            actionButton(
              inputId = "GlimmaR_textsize_plus",
              label = "A+",
              style = 'padding-left: 6px; padding-right:6px'
            )
          ),
          column(
            width = 3,
            align = 'right',
            actionButton(
              inputId = "GlimmaR_build_GLM",
              label = "Build",
              icon = icon("chevron-right"),
              style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left"
            )
          )
        )
      ),
      column(
        width = 7,
        fluidRow(
          column(
            width = 2,
            h3("Coefficients")
          ),
          column(
            width = 3,
            div(
              style = 'margin-left: 30px',
              htmlOutput('GlimmaR_model_dispersion')
            )
          ),
          column(
            width = 3,
            htmlOutput('GlimmaR_model_NAs')
          ),
          column(
            width = 4,
            align = 'right',
            br(),
            actionButton(
              inputId = "GlimmaR_goto_ChartaR",
              label = "",
              icon = icon("chart-line")
            ),
            shinyFiles::shinySaveButton(
              id = 'GlimmaR_save_model',
              label = 'Save GLM',
              title = 'Save GLM model as .RDS',
              filename = "",
              filetype = list(txt="RDS"),
              icon = icon('upload'),
              style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              viewtype = "detail"
            )
          )
        ),
        br(),
        DT::DTOutput('GlimmaR_glm_coefficients')
      )
    )
  )
}
    
#' buildGlimmaR Server Functions
#'
#' @noRd 
mod_buildGlimmaR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_buildGlimmaR_ui("buildGlimmaR_1")
    
## To be copied in the server
# mod_buildGlimmaR_server("buildGlimmaR_1")
