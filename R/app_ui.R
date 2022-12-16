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
        width = 250,
        sidebarMenu(
          id = 'tabs',
          br(),
          tags$head(tags$style(".sidebar-menu li a {padding-top: 3px; padding-bottom: 3px; font-size: 14px}")),
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
          mod_defineFilter_ui("filter"),

          # QUESTION where should I put this so it only applies to THIS control and no others?
          # this is still applying to EVERY sliderInput
          # tags$style(".irs-from, .irs-to, .irs-min, .irs-max, .irs-single{display:none}"),
          div(
            #style="margin-top:-10px; margin-bottom:0px;padding-top:0px;",
            #sliderInput("sidebarWidth", label = NULL, value = 250, min = 200, max = 400, step = 50, width = '80px', ticks = FALSE)
          )
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
    'www', system.file('app/www', package = "glucidum")
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$title("glucidum"),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    tags$script(src="www/sidebar_resize.js"),
    tags$script(src="www/window_dimensions.js")
  )
}
