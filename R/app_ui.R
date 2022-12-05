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
      dashboardHeader(
        title = span(tagList(tags$img(src='www/dashboard_title.png', height='40px', width='220px'),'')),
        titleWidth = 250,
        # controls placed in the header
        insertDashboardHeader(div('Models', style = 'color: white; font-size: 14px; margin-top: 16px; margin-bottom:-20px;margin-left: 20px;margin-right: 10px')),
        insertDashboardHeader(actionButton(inputId = 'GoTo_BoostaR', label = 'GBMs', icon = icon('rocket'), style = ' font-size:80%')),
        insertDashboardHeader(actionButton(inputId = 'GoTo_GlimmaR', label = 'GLMs', icon = icon('star'), style = ' font-size:80%')),
        insertDashboardHeader(div('Specs', style = 'color: white; font-size: 14px; margin-top: 16px; margin-bottom:-20px;margin-left: 20px;margin-right: 10px')),
        insertDashboardHeader(actionButton(inputId = 'GoTo_kpi_spec', label = NULL, icon = icon('gears'), style = ' font-size:100%')),
        insertDashboardHeader(actionButton(inputId = 'GoTo_feature_spec', label = NULL, icon = icon('list'), style = ' font-size:100%')),
        insertDashboardHeader(actionButton(inputId = 'GoTo_filter_spec', label = NULL, icon = icon('filter'), style = ' font-size:100%')),
        insertDashboardHeader(actionButton(inputId = 'GoTo_shinyAce', label = NULL, icon = icon('chevron-right'), style = ' font-size:100%')),
        insertDashboardHeader(div('Dataset', style = 'color: white; font-size: 14px; margin-top: 16px; margin-bottom:-20px;margin-left: 20px')),
        insertDashboardHeader(
          div(
            style="margin-left: 6px; margin-right: 14px; margin-top:10px; margin-bottom:-20px;",
            div(
              selectInput(inputId = "dataset", width = 320, label = NULL, choices = NULL), style = 'font-weight: 600')
            )
          )
        ),
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
          
          # developer text
          div(textOutput('selection_text'), style = 'margin-top:0px; margin-bottom:0px; margin-left:20px; font-size: 10px'),
          
          # QUESTION where should I put this so it only applies to THIS control and no others?
          # this is still applying to EVERY sliderInput
          # tags$style(".irs-from, .irs-to, .irs-min, .irs-max, .irs-single{display:none}"),
          div(
            style="margin-top:-10px; margin-bottom:0px;padding-top:0px;",
            sliderInput("sidebarWidth", label = NULL, value = 250, min = 200, max = 400, step = 50, width = '80px', ticks = FALSE)
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
    tags$script(src="www/sidebar_resize.js")
  )
}
