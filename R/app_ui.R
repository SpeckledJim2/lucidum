#' ui
#' 
#' @param request needed for bookmarking
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom DT DTOutput
#' @importFrom shinyAce aceEditor
#' 
#' @noRd
#' 
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(tags$style(HTML('select {resize: vertical; overflow: auto;}'))),
    # List the first level UI elements here 
    dashboardPage(
      mod_dashboardHeader_ui('header_nav_buttons'),
      dashboardSidebar(
        width = golem::get_golem_options('sidebar_width'),
        tags$div(class = "draggable", id = "draggable"),  # Draggable element
        sidebarMenu(
          id = 'tabs',
          br(),
          tags$head(tags$style(".sidebar-menu li a {padding-top: 3px; padding-bottom: 3px; font-size: 14px}")),
          
          # draggable sidebar
          tags$head(
            tags$style(
            HTML(
            ".draggable {
              width: 5px; 
              background: rgba(0,0,0,0); 
              cursor: ew-resize; 
              height: 100%; 
              position: absolute; 
              right: 0; 
              top: 0; 
              z-index: 1000;}"
                 )
              )
            ),
          
          menuItemOutput('Specs'),
          menuItemOutput('DataR'),
          menuItemOutput('ChartaR'),
          menuItemOutput('MappaR'),
          menuItemOutput('BoostaR'),
          menuItemOutput('GlimmaR'),

          # response, weight, navigator and filter
          mod_selectResponseColumn_ui('response', label = 'Response', width = '100%'),
          mod_selectWeightColumn_ui('weight', label = 'Weight', width = '100%'),
          mod_navigator_ui("navigator"),
          mod_defineFilter_ui("filter")
          
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = 'Specs', mod_DevelopaR_ui('DevelopaR')),
          tabItem(tabName = 'DataR', mod_DataR_ui('DataR')),
          tabItem(tabName = 'ChartaR', mod_ChartaR_ui('ChartaR')),
          tabItem(tabName = 'MappaR', mod_MappaR_ui('MappaR')),
          tabItem(tabName = 'BoostaR', mod_BoostaR_ui('BoostaR')),
          tabItem(tabName = 'GlimmaR', mod_GlimmaR_ui('GlimmaR'))
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = "lucidum")
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$title("lucidum"),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    tags$script(src="www/sidebar_drag.js"),
    tags$script(src="www/window_dimensions.js")
  )
}
