#' dashboardHeader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom tippy tippy_this
mod_dashboardHeader_ui <- function(id){
  bs <- '30px'
  pd <- 'padding:3px 5px 3px 5px'
  pd_marg <- 'padding:3px 5px 3px 5px; margin-right: 12px'
  ns <- NS(id)
    dashboardHeader(
      title = span(tagList(tags$img(src='www/dashboard_title.png', height='40px', width='220px'),'')),
      titleWidth = 250,
      # controls placed in the header
      # Specs
      dashboardButton(ns, 'GoTo_kpi_spec', 'kpi', 'KPI specification', 'show_DevelopaR', bs, pd),
      dashboardButton(ns, 'GoTo_feature_spec', 'features', 'Feature specification', 'show_DevelopaR', bs, pd),
      dashboardButton(ns, 'GoTo_filter_spec', 'filter', 'Filter specification', 'show_DevelopaR', bs, pd),
      dashboardButton(ns, 'GoTo_shinyAce', 'shinyAce', 'shinyAce console', 'show_DevelopaR', bs, pd_marg),
      # DataR
      dashboardButton(ns, 'GoTo_dataset_viewer', 'dataset_viewer', 'Dataset viewer', 'show_DataR', bs, pd),
      dashboardButton(ns, 'GoTo_column_summary', 'column_summary', 'Dataset column summary', 'show_DataR', bs, pd_marg),
      # ChartaR
      dashboardButton(ns, 'GoTo_one_way', 'one_way_line_bar', 'One way line and bar chart', 'show_ChartaR', bs, pd),
      dashboardButton(ns, 'GoTo_histogram', 'histogram', 'Histogram', 'show_ChartaR', bs, pd),
      dashboardButton(ns, 'GoTo_SHAP', 'SHAP', 'SHAP viewer', 'show_ChartaR', bs, pd_marg),
      # MappaR
      dashboardButton(ns, 'GoTo_MappaR', 'UK', 'UK mapping', 'show_MappaR', bs, pd_marg),
      # BoostaR
      dashboardButton(ns, 'GoTo_BoostaR_build', 'BoostaR_features', 'GBM features and parameters', 'show_BoostaR', bs, pd),
      dashboardButton(ns, 'GoTo_BoostaR_navigate', 'BoostaR_navigate', 'GBM navigator', 'show_BoostaR', bs, pd),
      dashboardButton(ns, 'GoTo_BoostaR_tree', 'tree', 'GBM tree diagram', 'show_BoostaR', bs, pd_marg),
      # GlimmaR
      dashboardButton(ns, 'GoTo_GlimmaR_build', 'beta', 'GLM formula and coefficient table', 'show_GlimmaR', bs, pd),
      dashboardButton(ns, 'GoTo_GlimmaR_navigate', 'GlimmaR_navigate', 'GLM navigator', 'show_GlimmaR', bs, pd),
      dashboardButton(ns, 'GoTo_GlimmaR_tabulate', 'tabulate', 'Tabulated GLMs', 'show_GlimmaR', bs, pd_marg),
      # dataset
      insertDashboardHeader(
        div(
          style="margin-left: 0px; margin-right: 12px; margin-top:10px; margin-bottom:-20px;",
          div(
            selectInput(inputId = "dataset", width = 360, label = NULL, choices = NULL), style = 'font-weight: 600')
        ),
        # if no data supplied then always show dataset chooser
        ifelse(is.null(get_golem_options('data')), TRUE, FALSE)
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
        style='margin-top:6px; padding-right:0;',
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

dashboardButton <- function(ns, button_name, icon_file, tooltip_text, golem_option, bs, pd){
  icon_file <- paste0('www/', icon_file, '.png')
  tooltip_text <- paste0("<span style='font-size:12px;'>",tooltip_text,"<span>")
  insertDashboardHeader(
    tagList(
      actionButton(inputId = ns(button_name), label = tagList(tags$img(src=icon_file, height=bs, width=bs)), style = pd),
      tippy_this(ns(button_name), placement = 'bottom', tooltip = tooltip_text)
    ),
    get_golem_options(golem_option)
  )
}