#' @import shiny
#' @import data.table
#' @import pkgload
#' @importFrom golem get_golem_options
#' @importFrom DT datatable renderDT
app_server <- function(input, output, session) {

  # set threads for data.table and lightgbm
  set_threads()
  
  # reactiveVals
  d <- reactiveVal(NULL)
  dataset_name <- reactiveVal(NULL)
  GlimmaR_models <- reactiveVal(NULL)
  BoostaR_models <- reactiveVal(NULL)
  kpi <- reactiveVal(NULL)
  GlimmaR_idx <- reactiveVal(0)
  BoostaR_idx <- reactiveVal(0)
  dimensions <- reactiveVal()
  crosstab_selector <- reactiveVal()
  new_response <- reactiveVal(NULL)
  new_weight <- reactiveVal(NULL)
  selected_tab <- reactiveVal(golem::get_golem_options('starting_tab'))
  
  # window dimensions to resize tables and ui elements
  observeEvent(input$dimensions, {
    # input$dimensions is defined in the .js file window_dimensions.js
    dimensions(input$dimensions)
  })
  
  # lucidum startup
  init_lucidum(session, golem::get_golem_options('data'), golem::get_golem_options('dataset_name'))
  
  # d is the dataset being analysed by lucidum
  # the golem option 'data' specifies the dataset
  # dt_update is used to trigger reactivity when d is changed
  # required because d is a data.table and can be changed by reference
  dt_update <- reactiveVal(0)
  d(load_dataset(golem::get_golem_options('data')))
  dataset_name(golem::get_golem_options('dataset_name'))
  
  # models
  BoostaR_models(golem::get_golem_options('BoostaR_models'))
  GlimmaR_models(golem::get_golem_options('GlimmaR_models'))

  # specification files
  kpi_spec <- reactiveVal()
  feature_spec <- reactiveVal()
  filter_spec <- reactiveVal()
  
  #
  demo <- reactiveVal(get_golem_options('dataset_name')=='insurance')
  starting_response <- reactiveVal()
  observeEvent(demo(), once = TRUE, {
    if(demo() & is.null(golem::get_golem_options('starting_response'))){
      starting_response('price')
    } else {
      starting_response(golem::get_golem_options('starting_response'))
    }
  })
  
  # d() contains a data.table
  # when columns in d are updated by reference, this does not trigger any reactivity
  # only when d is assigned to a new value (e.g. selecting a new dataset)
  observeEvent(d(), {
    if(!is.null(d())){
      d()[, user_filter := 1L]
      d()[, total_filter := 1L]
      # load specification files
      kpi_spec_path <- get_spec_filepath('kpi', dataset_name())
      filter_spec_path <- get_spec_filepath('filter', dataset_name())
      feature_spec_path <- get_spec_filepath('feature', dataset_name())
      kpi_spec(load_specification(d(), kpi_spec_path, 'kpi'))
      filter_spec(load_specification(d(), filter_spec_path, 'filter'))
      feature_spec(load_specification(d(), feature_spec_path, 'feature'))
    }
  })

  observeEvent(crosstab_selector(), {
    # QUESTION - what I do below isn't modular, but it feels OK and all in one place
    # any comments whether a better or preferred way to this?
    # purpose of this section is to enable quick access to ChartaR from BoostaR/GlimmaR with a pre-selected feature
    if(!is.null(crosstab_selector)){
      c <- crosstab_selector()
      if(c$originator=='BoostaR feature table'){
        # navigate to ChartaR one way line and bar with pre-selected inputs
        if(c$last_clicked %in% names(d())){
          updateSelectInput(session, inputId = 'ChartaR-line_and_bar-x_axis_feature-selectInput', selected = c$last_clicked)
          if('lgbm_prediction' %in% names(d())){
            updateRadioGroupButtons(session, inputId = 'ChartaR-line_and_bar-x_axis_feature-selectChooser', selected = 'lucidum')
            updateSelectInput(session, inputId = 'ChartaR-line_and_bar-add_columns-selectInput', selected = 'lgbm_prediction')
            updateRadioGroupButtons(session, inputId = 'ChartaR-line_and_bar-show_partial_dependencies', selected = 'GBM-')
          }
          updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
          updateNavbarPage(session = session, inputId = "ChartaR-tabsetPanel", selected = "1-way line and bar")
        }
      } else if(c$originator=='BoostaR gain summary'){
        # navigate to ChartaR SHAP plot with pre-selected inputs
        if(c$int_order==1){
          updateSelectInput(session, inputId = 'ChartaR-SHAP-feature_1', selected = c$f1)
          updateSelectInput(session, inputId = 'ChartaR-SHAP-feature_2', selected = 'none')
          updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
          updateNavbarPage(session, inputId = "ChartaR-tabsetPanel", selected = "SHAP")
        } else if(c$int_order==2){
          updateSelectInput(session, inputId = 'ChartaR-SHAP-feature_1', selected = c$f1)
          updateSelectInput(session, inputId = 'ChartaR-SHAP-feature_2', selected = c$f2)
          updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
          updateNavbarPage(session, inputId = "ChartaR-tabsetPanel", selected = "SHAP")
        }
      } else if(c$originator=='GlimmaR coefficient table'){
        # navigate to ChartaR one way line and bar with pre-selected inputs
        if(!is.null(c$last_clicked)){
          if(c$last_clicked %in% names(d()) & 'glm_prediction' %in% names(d())){
            updateSelectInput(session, inputId = 'ChartaR-line_and_bar-x_axis_feature-selectInput', selected = c$last_clicked)
            updateSelectInput(session, inputId = 'ChartaR-line_and_bar-add_columns-selectInput', selected = 'glm_prediction')
            updateRadioGroupButtons(session, inputId = 'ChartaR-line_and_bar-show_partial_dependencies', selected = 'GLM')
            updateTabItems(session, inputId = 'tabs', selected = 'ChartaR')
            updateNavbarPage(session = session, inputId = "ChartaR-tabsetPanel", selected = "1-way line and bar")
          }
        }
      }
    }
  })

  # selected tab
  observeEvent(input$tabs, {
    selected_tab(input$tabs)
  })
  
  # menuItems
  showModule(output, 'Specs', 'chevron-right', golem::get_golem_options('show_DevelopaR'))
  showModule(output, 'DataR', 'bars', golem::get_golem_options('show_DataR'))
  showModule(output, 'ChartaR', 'chart-line', golem::get_golem_options('show_ChartaR'))
  showModule(output, 'MappaR', 'map', golem::get_golem_options('show_MappaR'))
  showModule(output, 'BoostaR', 'rocket', golem::get_golem_options('show_BoostaR'))
  showModule(output, 'GlimmaR', 'star', golem::get_golem_options('show_GlimmaR'))
  updateTabItems(session, 'tabs', golem::get_golem_options('starting_tab'))
  
  # header server
  mod_dashboardHeader_server('header_nav_buttons', session, dt_update)
  observeEvent(input$dataset, ignoreInit = TRUE, {
    if(input$dataset %not_in% c('loaded from .csv file', 'choose dataset','user supplied dataset')){
      d(setDT(get(input$dataset)))
      dataset_name(input$dataset)
      dt_update(dt_update()+1)
    }
  })
  observeEvent(nav_options(), {
    kpi(nav_options()$kpi)
    BoostaR_idx(nav_options()$gbm)
    GlimmaR_idx(nav_options()$glm)
  })

  # sidebar servers
  weight <- mod_selectWeightColumn_server('weight', d, dt_update, TRUE, NULL, 'N', kpi, kpi_spec, new_weight)
  response <- mod_selectResponseColumn_server('response', d, dt_update, TRUE, NULL, NULL, kpi, kpi_spec,
    weight, starting_response, new_response)
  nav_options <- mod_navigator_server('navigator', kpi_spec, GlimmaR_models, BoostaR_models, GlimmaR_idx, BoostaR_idx)
  
  # filter server
  filters <- mod_defineFilter_server("filter", d, dt_update, filter_spec)
  
  # tab servers
  mod_DevelopaR_server('DevelopaR', d, dt_update, response, weight, kpi_spec, filter_spec, feature_spec, BoostaR_models, GlimmaR_models, BoostaR_idx, GlimmaR_idx, dimensions)
  if(golem::get_golem_options('show_DataR')){
    mod_DataR_server('DataR', d, dt_update)
  }
  mod_ChartaR_server('ChartaR', d, dt_update, response, weight, kpi_spec, feature_spec, BoostaR_models, BoostaR_idx, GlimmaR_models, GlimmaR_idx, filters)
  mod_MappaR_server('MappaR', d, dt_update, response, weight, kpi_spec, selected_tab, golem::get_golem_options('show_MappaR'), filters)
  mod_BoostaR_server('BoostaR', d, dt_update, response, weight, feature_spec, BoostaR_models, BoostaR_idx, dimensions, crosstab_selector)
  mod_GlimmaR_server('GlimmaR', d, dt_update, response, weight, feature_spec, GlimmaR_models, GlimmaR_idx, BoostaR_models, BoostaR_idx, crosstab_selector)
  
  # update response and weight when BoostaR model is changed
  observeEvent(BoostaR_idx(), {
    if(!is.null(BoostaR_models()) & !is.null(BoostaR_idx())){
      b <- BoostaR_models()[[BoostaR_idx()]]
      # following lines with dt_update
      # used within ChartaR to ensure chart only updates
      # once when model is changed
      if(!is.null(new_response()) & !is.null(b)){
        if(new_response()!=b$response){dt_update(-1)}
      }
      if(!is.null(new_weight()) & !is.null(b)){
        if(new_weight()!=b$weight){dt_update(-1)}
      }
      new_response(b$response)
      new_weight(b$weight)
    }
  })
  # update response and weight when GlimmaR model is changed
  observeEvent(GlimmaR_idx(), {
    if(!is.null(GlimmaR_models()) & !is.null(GlimmaR_idx())){
      g <- GlimmaR_models()[[GlimmaR_idx()]]
      # following lines with dt_update
      # used within ChartaR to ensure chart only updates
      # once when model is changed
      if(!is.null(new_response()) & !is.null(g)){
        if(new_response()!=g$response){dt_update(-1)}
      }
      if(!is.null(new_weight()) & !is.null(g)){
        if(new_weight()!=g$weight){dt_update(-1)}
      }
      new_response(g$response)
      new_weight(g$weight)
    }
  })
  
  # run on close browser - stops server
  session$onSessionEnded(function() {stopApp()})
  
}

select_if_present <- function(new_val, current_val, choices){
  # returned new_val is it is present in choices, else the current_val
  if(new_val %in% unlist(choices)){
    new_val
  } else {
    current_val
  }
}
get_spec_filepath <- function(type, dataset_name){
  # if no specification path provided use working directory
  if(is.null(golem::get_golem_options('specification_path'))){
    spec_folder <- getwd()
  } else {
    spec_folder <- golem::get_golem_options('specification_path')
  }
  # get the golem option for the specification file
  explicit_spec_file <- golem::get_golem_options(paste0(type, '_spec'))
  if(!is.null(explicit_spec_file)){
    # use the explicit spec file
    explicit_spec_file
  } else {
    if(dataset_name=='insurance'){
      # this is the included demo dataset
        spec_name <- paste0('insurance_',type,'_spec.csv')
        system.file(spec_name, package="lucidum")
    } else if(dataset_name!='NULL'){
      # search for the spec file in the specification folder supplied as a golem option
      paste0(spec_folder, '/', dataset_name, '_', type, '_spec.csv')
    }
  }
}

#' @importFrom lightgbm setLGBMthreads
set_threads <- function(){
  n_threads <- golem::get_golem_options('num_threads')
  if(n_threads>0){
    setDTthreads(n_threads)
    setLGBMthreads(n_threads)
  } 
}