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
  
  # d() contains a data.table
  # when columns in d are updated by reference, this does not trigger any reactivity
  # only when d is assigned to a new value (e.g. selecting a new dataset)
  observeEvent(d(), {
    if(!is.null(d())){
      d()[, user_filter := 1]
      d()[, total_filter := 1]
    }
  })
  
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
  kpi <- reactiveVal(NULL)
  GlimmaR_idx <- reactiveVal(0)
  BoostaR_idx <- reactiveVal(0)
  
  # menuItems
  showModule(output, 'Specs', 'chevron-right', golem::get_golem_options('show_DevelopaR'))
  showModule(output, 'DataR', 'bars', golem::get_golem_options('show_DataR'))
  showModule(output, 'ChartaR', 'chart-line', golem::get_golem_options('show_ChartaR'))
  showModule(output, 'MappaR', 'map', golem::get_golem_options('show_MappaR'))
  showModule(output, 'BoostaR', 'rocket', golem::get_golem_options('show_BoostaR'))
  showModule(output, 'GlimmaR', 'star', golem::get_golem_options('show_GlimmaR'))
  updateTabItems(session, 'tabs', 'DataR')
  
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
  
  # sidebar servers
  weight <- mod_selectWeightColumn_server('weight', d, dt_update, TRUE, NULL, 'N', kpi, kpi_spec)
  response <- mod_selectResponseColumn_server('response', d, dt_update, TRUE, NULL, NULL, kpi, kpi_spec, weight)
  nav_options <- mod_navigator_server("navigator", kpi_spec, GlimmaR_models, BoostaR_models, GlimmaR_idx, BoostaR_idx)
  
  # read out the kpi and model indices from the sidebar navigator
  observeEvent(nav_options(), {
    kpi(nav_options()$kpi)
  })
  
  # filter server
  mod_defineFilter_server("filter", d, dt_update, filter_spec)
  
  # tab servers
  mod_DevelopaR_server('DevelopaR')
  mod_DataR_server('DataR', d, dt_update)
  mod_ChartaR_server('ChartaR', d, dt_update, response, weight, kpi_spec)
  mod_MappaR_server('MappaR', d, dt_update, response, weight, kpi_spec)
  mod_BoostaR_server('BoostaR', d, dt_update)

  # run on close browser - stops server
  session$onSessionEnded(function() {stopApp()})
  
}

