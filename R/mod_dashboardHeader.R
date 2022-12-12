#' dashboardHeader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dashboardHeader_ui <- function(id){
  ns <- NS(id)
    dashboardHeader(
      title = span(tagList(tags$img(src='www/dashboard_title.png', height='40px', width='220px'),'')),
      titleWidth = 250,
      # controls placed in the header
      # Specs
      insertDashboardHeader(actionButton(inputId = ns('GoTo_kpi_spec'), label = NULL, icon = icon('gears'), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_DevelopaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_feature_spec'), label = NULL, icon = icon('list'), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_DevelopaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_filter_spec'), label = NULL, icon = icon('filter'), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_DevelopaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_shinyAce'), label = NULL, icon = icon('chevron-right'), style = 'padding:4px 6px 4px 6px; margin-right: 12px'), get_golem_options('show_DevelopaR')),
      # DataR
      insertDashboardHeader(actionButton(inputId = ns('GoTo_dataset_viewer'), label = NULL, icon = icon('bars'), style = 'padding:4px 6px 4px 6px;'), get_golem_options('show_DataR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_column_summary'), label = NULL, icon = icon('table-columns'), style = 'padding:4px 6px 4px 6px; margin-right: 12px'), get_golem_options('show_DataR')),
      # ChartaR
      insertDashboardHeader(actionButton(inputId = ns('GoTo_one_way'), label = tagList(tags$img(src='www/one_way_line_bar.png', height="20px", width="20px")), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_ChartaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_histogram'), label = tagList(tags$img(src='www/histogram.png', height="20px", width="20px")), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_ChartaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_SHAP'), label = tagList(tags$img(src='www/SHAP.png', height="20px", width="20px")), style = 'padding:4px 6px 4px 6px; margin-right: 12px'), get_golem_options('show_ChartaR')),
      # MappaR
      insertDashboardHeader(actionButton(inputId = ns('GoTo_MappaR'), label = tagList(tags$img(src='www/UK.png', height="22px", width="22px")), style = 'padding:3px 4px 4px 3px; margin-right: 12px'), get_golem_options('show_MappaR')),
      # BoostaR
      insertDashboardHeader(actionButton(inputId = ns('GoTo_BoostaR_build'), label = NULL, icon = icon('bars'), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_BoostaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_BoostaR_navigate'), label = NULL, icon = icon('table-columns'), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_BoostaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_BoostaR_tree'), label = tagList(tags$img(src='www/tree.png', height="20px", width="20px")), style = 'padding:4px 6px 4px 6px; margin-right: 12px'), get_golem_options('show_BoostaR')),
      # GlimmaR
      insertDashboardHeader(actionButton(inputId = ns('GoTo_GlimmaR_build'), label = tagList(tags$img(src='www/beta.png', height="20px", width="20px")), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_MappaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_GlimmaR_navigate'), label = NULL, icon = icon('star'), style = 'padding:4px 6px 4px 6px'), get_golem_options('show_MappaR')),
      insertDashboardHeader(actionButton(inputId = ns('GoTo_GlimmaR_tabulate'), label = NULL, icon = icon('table-columns'), style = 'padding:4px 6px 4px 6px; margin-right: 12px'), get_golem_options('show_MappaR')),
      # dataset
      insertDashboardHeader(
        div(
          style="margin-left: 0px; margin-right: 12px; margin-top:10px; margin-bottom:-20px;",
          div(
            selectInput(inputId = "dataset", width = 360, label = NULL, choices = NULL), style = 'font-weight: 600')
        ),
        get_golem_options('show_dataset_chooser')
      )
    )
}
    
#' dashboardHeader Server Functions
#'
#' @noRd 
mod_dashboardHeader_server <- function(id, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # DevelopaR
    observeEvent(input$GoTo_kpi_spec, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'Specs')
      updateNavbarPage(session = parent, inputId = "DevelopaR-tabsetPanel", selected = 'KPI specification')
    })
    observeEvent(input$GoTo_feature_spec, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'Specs')
      updateNavbarPage(session = parent, inputId = "DevelopaR-tabsetPanel", selected = 'Feature specification')
    })
    observeEvent(input$GoTo_filter_spec, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'Specs')
      updateNavbarPage(session = parent, inputId = "DevelopaR-tabsetPanel", selected = 'Filter specification')
    })
    observeEvent(input$GoTo_shinyAce, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'Specs')
      updateNavbarPage(session = parent, inputId = "DevelopaR-tabsetPanel", selected = 'shinyAce')
    })
    # DataR
    observeEvent(input$GoTo_dataset_viewer, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'DataR')
      updateNavbarPage(session = parent, inputId = "DataR-tabsetPanel", selected = 'Dataset viewer')
    })
    observeEvent(input$GoTo_column_summary, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'DataR')
      updateNavbarPage(session = parent, inputId = "DataR-tabsetPanel", selected = 'Column summary')
    })
    # ChartaR
    observeEvent(input$GoTo_one_way, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'ChartaR')
      updateNavbarPage(session = parent, inputId = "ChartaR-tabsetPanel", selected = '1-way line and bar')
    })
    observeEvent(input$GoTo_histogram, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'ChartaR')
      updateNavbarPage(session = parent, inputId = "ChartaR-tabsetPanel", selected = 'Histogram')
    })
    observeEvent(input$GoTo_SHAP, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'ChartaR')
      updateNavbarPage(session = parent, inputId = "ChartaR-tabsetPanel", selected = 'SHAP')
    })
    # MappaR
    observeEvent(input$GoTo_MappaR, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'MappaR')
    })
    # BoostaR
    observeEvent(input$GoTo_BoostaR_build, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'BoostaR')
      updateNavbarPage(session = parent, inputId = "BoostaR-tabsetPanel", selected = 'Features and parameters')
    })
    observeEvent(input$GoTo_BoostaR_navigate, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'BoostaR')
      updateNavbarPage(session = parent, inputId = "BoostaR-tabsetPanel", selected = 'Model navigator')
    })
    observeEvent(input$GoTo_BoostaR_tree, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'BoostaR')
      updateNavbarPage(session = parent, inputId = "BoostaR-tabsetPanel", selected = 'Tree viewer')
    })
    # GlimmaR
    observeEvent(input$GoTo_GlimmaR_build, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'GlimmaR')
      updateNavbarPage(session = parent, inputId = "GlimmaR-tabsetPanel", selected = 'Model formula')
    })
    observeEvent(input$GoTo_GlimmaR_navigate, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'GlimmaR')
      updateNavbarPage(session = parent, inputId = "GlimmaR-tabsetPanel", selected = 'Model navigator')
    })
    observeEvent(input$GoTo_GlimmaR_tabulate, {
      updateTabItems(session = parent, inputId = 'tabs', selected = 'GlimmaR')
      updateNavbarPage(session = parent, inputId = "GlimmaR-tabsetPanel", selected = 'Tabulated models')
    })
  })
}
    
#' Wrap ui elements in dropdown tag
#' so they can be placed in the shiny dashboard header
#'
#' @param x ui element to put into dropdown
#' @param show boolean TRUE shows item, FALSE hides item
#'
#' @return HTML tag
#'
insertDashboardHeader <- function(x, show){
  if(show){
    tags$li(
      class = "dropdown",
      div(
        style='margin-top:10px; padding-right:0;',
        x
      )
    )
  } else {
    tags$li(
      class = "dropdown",
      div(
        style='margin:0px; padding:0px;'
      )
    )
  }
}
