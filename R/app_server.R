#' @import shiny
#' @import data.table
#' @import pkgload
#' @importFrom golem get_golem_options
#' @importFrom DT datatable renderDT
app_server <- function(input, output,session) {

  # d is the dataset being analysed by lucidum
  # the golem option 'data' specifies the dataset
  # dt_update is used to trigger reactivity when d is changed
  # required because d is a data.table and can be changed by reference
  d <- reactiveVal(data.table::data.table())
  dt_update <- reactiveVal(0)
  d(setDT(golem::get_golem_options('data')))
  
  # load specifications from the golem options
  feature_spec <- reactiveVal()
  filter_spec <- reactiveVal()
  kpi_spec <- reactiveVal()
  kpi_spec(load_specification(golem::get_golem_options('kpi_spec'), 'kpi'))
  filter_spec(load_specification(golem::get_golem_options('filter_spec'), 'filter'))
  feature_spec(load_specification(golem::get_golem_options('feature_spec'), 'feature'))

  # load models from the golem options
  GlimmaR_models <- reactiveVal(list('a','b','c'))
  BoostaR_models <- reactiveVal(list('when','will','I','see','you','again'))
  
  # model indices
  GlimmaR_model_index <- reactiveVal(0)
  BoostaR_model_index <- reactiveVal(0)
  
  # menuItems
  showModule(output, 'Specs', 'chevron-right', golem::get_golem_options('show_DevelopaR'))
  showModule(output, 'DataR', 'bars', golem::get_golem_options('show_DataR'))
  showModule(output, 'ChartaR', 'chart-line', golem::get_golem_options('show_ChartaR'))
  showModule(output, 'MappaR', 'map', golem::get_golem_options('show_MappaR'))
  showModule(output, 'BoostaR', 'rocket', golem::get_golem_options('show_BoostaR'))
  showModule(output, 'GlimmaR', 'star', golem::get_golem_options('show_GlimmaR'))
  updateTabItems(session, 'tabs', 'DataR')
  
  # sidebar servers
  response <- mod_selectColumn_server('response', d, dt_update, TRUE, NULL, NULL)
  weight <- mod_selectColumn_server('weight', d, dt_update, TRUE, NULL, 'N')
  nav_options <- mod_navigator_server("navigator", kpi_spec, GlimmaR_models, BoostaR_models, GlimmaR_model_index, BoostaR_model_index)
  
  # header servers
  observeEvent(input$dataset, ignoreInit = TRUE, {
    d(setDT(get(input$dataset)))
    dt_update(dt_update()+1)
  })
  observeEvent(nav_options, {
    output$selection_text <- renderText({
      paste0('dt_update: ', dt_update())
    })
  })
  observeEvent(input$GoTo_kpi_spec, {
    updateTabItems(session, inputId = 'tabs', selected = 'Specs')
    updateNavbarPage(session = session, inputId = "DevelopaR-tabsetPanel", selected = 'KPI specification')
  })
  observeEvent(input$GoTo_feature_spec, {
    updateTabItems(session, inputId = 'tabs', selected = 'Specs')
    updateNavbarPage(session = session, inputId = "DevelopaR-tabsetPanel", selected = 'Feature specification')
  })
  observeEvent(input$GoTo_filter_spec, {
    updateTabItems(session, inputId = 'tabs', selected = 'Specs')
    updateNavbarPage(session = session, inputId = "DevelopaR-tabsetPanel", selected = 'Filter specification')
  })
  observeEvent(input$GoTo_shinyAce, {
    updateTabItems(session, inputId = 'tabs', selected = 'Specs')
    updateNavbarPage(session = session, inputId = "DevelopaR-tabsetPanel", selected = 'shinyAce')
  })
  
  # filter server
  mod_defineFilter_server("filter", d, dt_update, filter_spec)
  
  # module servers
  mod_DevelopaR_server('DevelopaR')
  mod_DataR_server('DataR', d, dt_update)
  mod_ChartaR_server('ChartaR')
  mod_MappaR_server('MappaR', d, dt_update, response, weight, kpi_spec)
  mod_BoostaR_server('BoostaR', d, dt_update)

  # run on close browser - stops server
  session$onSessionEnded(function() {stopApp()})
  
}

