#' tabulatedGlimmaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabulatedGlimmaR_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        selectInput(inputId = 'GlimmaR_model_chooser', label = 'Select tabulated model', choices = NULL, size = 10, selectize = FALSE),
        selectInput(inputId = 'GlimmaR_table_chooser', label = 'Select table', choices = NULL, size = 30, selectize = FALSE),
        tags$head(tags$script('
                          // Define function to set height of "GlimmaR_table_chooser"
                          setHeight_GlimmaR_table_chooser = function() {
                            var window_height = $(window).height();
                            var header_height = $(".main-header").height();
                            var num_rows = (window_height - header_height)/20 - 15 ;
                            var preview = document.getElementById("GlimmaR_table_chooser")
                            preview.setAttribute("size", num_rows);
                          };
                          // Set input$box_height when the connection is established
                          $(document).on("shiny:connected", function(event) {
                            setHeight_GlimmaR_table_chooser();
                          });
                          // Refresh the box height on every window resize event
                          $(window).on("resize", function(){
                            setHeight_GlimmaR_table_chooser();
                          });
                        ')),
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 6,
            radioGroupButtons(
              inputId = 'GlimmaR_transpose_table',
              label = NULL,
              choices = c('Normal','Transpose'),
              selected = 'Normal'
            )
          ),
          column(
            width = 6,
            align = 'right',
            shinySaveButton(
              id = 'GlimmaR_export_tables',
              label = 'Export to Excel',
              title = 'Choose location to save tables',
              filename = "",
              filetype=list(txt="xlsx"),
              icon = icon('upload'),
              style = 'color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left',
              viewtype = "detail"
            )
          )
        ),
        br(),
        DT::DTOutput('GlimmaR_tabulated_model')
      )
    )
  )
}
    
#' tabulatedGlimmaR Server Functions
#'
#' @noRd 
mod_tabulatedGlimmaR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tabulatedGlimmaR_ui("tabulatedGlimmaR_1")
    
## To be copied in the server
# mod_tabulatedGlimmaR_server("tabulatedGlimmaR_1")
