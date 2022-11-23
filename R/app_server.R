#' @import shiny
#' @import data.table
#' @import pkgload
#' @importFrom golem get_golem_options
#' @importFrom DT datatable renderDT
app_server <- function(input, output,session) {

  # d is the dataset being analysed by lucidum
  # dt_update is used to trigger reactivity when d is changed
  # required because d is a data.table and can be changed by reference
  d <- reactiveVal(data.table::data.table())
  dt_update <- reactiveVal(0)
  d(setDT(golem::get_golem_options('data')))
  
  # specifications
  feature_spec <- reactiveVal(1)
  filter_spec <- reactiveVal(2)
  kpi_spec <- reactiveVal(data.table(kpi=c('first','second')))

  # models
  GlimmaR_models <- reactiveVal(list('a','b','c'))
  BoostaR_models <- reactiveVal(list('when','will','I','see','you','again'))
  
  # model indices
  GlimmaR_model_index <- reactiveVal(0)
  BoostaR_model_index <- reactiveVal(0)
  
  # menuItems
  showModule(output, 'Specs', 'chevron-right', golem::get_golem_options('show_DevelopaR'))
  showModule(output, 'DataR', 'bars', TRUE)
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
  
  # filter server
  mod_defineFilter_server("filter", d, dt_update)
  
  # module servers
  mod_DataR_server('DataR', d, dt_update)
  mod_ChartaR_server('ChartaR')
  mod_MappaR_server('MappaR', d, dt_update, response, weight, kpi_spec)
  mod_BoostaR_server('BoostaR', d, dt_update)

  # run on close browser - stops server
  session$onSessionEnded(function() {stopApp()})
  
}

