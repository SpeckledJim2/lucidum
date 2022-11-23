#' MappaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets searchInput spectrumInput
#' @importFrom leaflet leaflet leafletOptions addTiles addProviderTiles addLayersControl hideGroup 
#' @importFrom leaflet addEasyButton easyButton setView JS layersControlOptions providers
#' 
#' @noRd
#' 
mod_MappaR_ui <- function(id){
  button_style_move_map <- 'padding:3px; font-size:80%; margin-left:0px; margin-right:0px;color: #000; border-color: #3e6e37'
  button_style_update <- 'padding:3px; font-size:80%; margin-left:0px; margin-right:0px;color: #fff; background-color: #4bb03c; border-color: #3e6e37'
  ns <- NS(id)
  tagList(
    # height argument ensures map resizes well with browser
    leaflet::leafletOutput(ns('map'), height = 'calc(99vh - 69px)'),
    absolutePanel(id = ns('controls'),
                  class = 'panel panel-default',
                  top = '25%',
                  right = '2%',
                  width = 260,
                  fixed=TRUE,
                  draggable = TRUE,
                  height = "auto",
                  fluidRow(
                    column(width = 12,
                           align = 'center',
                           textOutput(ns('panel_title')),
                           htmlOutput(ns('panel_location')),
                           textOutput(ns('filters'))
                    )
                  ),
                  br(),
                  fluidRow(
                    column(width = 12,
                           searchInput(
                             inputId = ns('postcode'),
                             label = NULL,
                             placeholder = "enter postcode area",
                             btnSearch = icon("magnifying-glass")
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           sliderInput(
                             inputId = ns('line_thickness'),
                             label = 'Line thickness',
                             min = 0,
                             max = 5,
                             value = 1,
                             step = 0.1,
                             ticks = FALSE,
                             width = '100%'
                           ),
                           sliderInput(
                             inputId = ns('opacity'),
                             label = 'Opacity',
                             min = 0,
                             max = 1,
                             value = 1.00,
                             step = 0.05,
                             ticks = FALSE,
                             width = '100%'
                           ),
                    ),
                    column(width = 6,
                           sliderInput(
                             inputId = ns('hotspots'),
                             label = 'Hot/not-spots',
                             min = -10,
                             max = 10,
                             value = 0,
                             step = 1,
                             ticks = FALSE,
                             width = '100%'
                           ),
                           sliderInput(
                             inputId = ns('label_size'),
                             label = 'Label size',
                             min = 0,
                             max = 20,
                             value = 0,
                             step = 1,
                             ticks = FALSE,
                             width = '100%'
                           )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      align = 'left',
                      fluidRow(
                        column(width = 4,
                               spectrumInput(
                                 inputId = ns('colour1'),
                                 label = 'Low',
                                 choices = NULL,
                                 selected = 'green',
                                 flat = TRUE,
                                 options = list(flat = 'false'),
                                 update_on = c('dragstop'),
                                 width = '100%'
                               )
                        ),
                        column(width = 4,
                               spectrumInput(
                                 inputId = ns('colour2'),
                                 label = 'Middle',
                                 choices = NULL,
                                 selected = 'white',
                                 flat = TRUE,
                                 options = list(flat = 'false'),
                                 update_on = c('dragstop'),
                                 width = '100%'
                               )
                        ),
                        column(width = 4,
                               spectrumInput(
                                 inputId = ns('colour3'),
                                 label = 'High',
                                 choices = NULL,
                                 selected = 'red',
                                 flat = TRUE,
                                 options = list(flat = 'false'),
                                 update_on = c('dragstop'),
                                 width = '100%'
                               )
                        )
                      ),
                      fluidRow(
                        style = 'margin-top: -10px; padding-top: -10px; margin-bottom: -10px; padding-bottom: -10px',
                        column(
                          width = 12,
                          radioGroupButtons(
                            inputId = ns('palettes'),
                            label = NULL,
                            justified = TRUE,
                            size = 'xs',
                            choices = c('Christmas','Spectral','Greys'),
                            selected = 'Christmas'
                          )
                        )
                      ),
                      fluidRow(
                        style = 'margin-top: -10px; padding-top: -10px; margin-bottom: -10px; padding-bottom: -10px',
                        column(
                          width = 4,
                          align = 'right',
                          div(
                            checkboxInput(inputId = ns('dark_mode'), label = "Dark", value = TRUE),
                            style = 'margin-top: -15px; padding-top: -10px;'
                          )
                        ),
                        column(
                          width = 8,
                          align = 'center',
                          div(
                            checkboxInput(inputId = ns('sectors'),label = "Sectors & units", value = FALSE),
                            style = 'margin-top: -15px; padding-top: -10px;'
                          )
                        )
                      )
                    )
                  )
    )
  )
}
    
#' MappaR Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param d data.frame or data.table
#' @param dt_update update
#' @param response character name of numerator
#' @param weight character name of denominator
#' @param kpi_spec data.table containing the kpi_specification
#' 
#' @importFrom htmlwidgets onRender
#' @importFrom leaflet leafletProxy
#' 
#' @noRd
#' 
mod_MappaR_server <- function(id, d, dt_update, response, weight, kpi_spec){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # setup map
    # QUESTION how can I make the map only update when it is visible or first selected?
    # suspendWhenHidden doesn't seem to work, i.e. map is updated when it's not visible
    output$map <- leaflet::renderLeaflet({base_map()})
    outputOptions(output, "map", suspendWhenHidden = FALSE) # TRUE doesn't do anything
    
    # assemble the map options
    # QUESTION - must be an easier way of doing this - this works but feels clunky
    map_options <- reactiveVal()
    observeEvent(c(input$line_thickness,
                   input$opacity,
                   input$hotspots,
                   input$label_size,
                   input$colour1,
                   input$colour2,
                   input$colour3,
                   input$sectors), {
      map_options(
        list(
          line_thickness = input$line_thickness,
          opacity = input$opacity,
          hotspots = input$hotspots,
          label_size = input$label_size,
          colour1 = input$colour1,
          colour2 = input$colour2,
          colour3 = input$colour3,
          sectors = input$sectors
        )
      )
    })
    
    # update map when one of the inputs up
    observe({
      dt_update()
      viz_create_map(leafletProxy('map'), d(), response(), weight(), kpi_spec(), map_options())
    })
    
    observeEvent(input$tabs, {
      # QUESTION - why does this never trigger (linked to question above)
      # input$tabs is always NULL - is that because I am in a module?
      if(input$tabs=='MappaR'){
        viz_create_map(leafletProxy('map'), d(), response(), weight(), kpi_spec(), map_options())
      }
    })
  })
}
