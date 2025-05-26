#' Launch lucidum
#'
#' @param data data.frame or data.table to analyse in lucidum.
#' @param BoostaR_models character, path to .rds file containing BoostaR_models.
#' @param GlimmaR_models character, path to .rds file containing GlimmaR_models.
#' @param kpi_spec character, path to kpi specification file.
#' @param filter_spec character, path to filter specification file.
#' @param feature_spec character, path to feature specification file.
#' @param specification_path character, path to feature specification folder
#' @param show_DataR logical, TRUE (not default) will show the DataR menu item.
#' @param show_ChartaR logical, TRUE (default) will show the ChartaR menu item.
#' @param show_MappaR logical, TRUE (default) will show the MappaR menu item.
#' @param show_BoostaR logical, TRUE (default) will show the BoostaR menu item.
#' @param show_GlimmaR logical, TRUE (default) will show the GlimmaR menu item.
#' @param show_DevelopaR logical, TRUE (not default) will show the ShinyAce console.
#' @param starting_tab character, name of tab to show on startup.
#' @param starting_response character, name of response column to show on startup.
#' @param sidebar_width integer, width of the sidebar.
#' @param num_threads integer, number of threads for data.table and lightgbm (default -1 means max threads)
#' @param title character, text to show in the app header.
#'
#' @export
#'
#' @examples
#' # Launch lucidum with the supplied demo dataset called "insurance"
#' 
#' lucidum(insurance, starting_response = 'price')
#' 
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
lucidum <- function(data=NULL,
                     BoostaR_models = NULL,
                     GlimmaR_models = NULL,
                     kpi_spec = NULL,
                     filter_spec = NULL,
                     feature_spec = NULL,
                     specification_path = NULL,
                     show_DataR = TRUE,
                     show_ChartaR = TRUE,
                     show_MappaR = TRUE,
                     show_BoostaR = TRUE,
                     show_GlimmaR = TRUE,
                     show_DevelopaR = FALSE,
                     starting_tab = 'ChartaR',
                     starting_response = NULL,
                     sidebar_width = 280,
                     num_threads = -1,
                     title = NULL
                     ) {
  
  # validate data
  if (is.null(data)) {
    stop("Error: 'data' must be supplied.")
  }
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    stop("Error: 'data' must be a data.frame or data.table.")
  }
  
  # validate BoostaR_models
  if (!is.null(BoostaR_models) && !is.list(BoostaR_models)) {
    stop("'BoostaR_models' must be a list.")
  }
  
  # validate GlimmaR_models
  if (!is.null(GlimmaR_models) && !is.list(GlimmaR_models)) {
    stop("'GlimmaR_models' must be a list.")
  }
  
  # validate specification files
  check_spec_path <- function(path, name) {
    if (!is.null(path) && (!is.character(path) || !file.exists(path))) {
      stop(sprintf("'%s' must be a valid file path.", name))
    }
  }
  check_spec_path(kpi_spec, "kpi_spec")
  check_spec_path(filter_spec, "filter_spec")
  check_spec_path(feature_spec, "feature_spec")
  
  # validate show_ flags
  check_logical <- function(x, name) {
    if (!is.logical(x) || length(x) != 1) {
      stop(sprintf("'%s' must be a single logical value (TRUE or FALSE).", name))
    }
  }
  check_logical(show_DataR, "show_DataR")
  check_logical(show_ChartaR, "show_ChartaR")
  check_logical(show_MappaR, "show_MappaR")
  check_logical(show_BoostaR, "show_BoostaR")
  check_logical(show_GlimmaR, "show_GlimmaR")
  check_logical(show_DevelopaR, "show_DevelopaR")
  
  # validate starting_tab
  if (!is.null(starting_tab) && (!is.character(starting_tab) || length(starting_tab) != 1)) {
    stop("'starting_tab' must be a single character string.")
  }
  
  # validate starting_response
  if (!is.null(starting_response) && (!is.character(starting_response) || length(starting_response) != 1)) {
    stop("'starting_response' must be a single character string.")
  }
  
  # validate title
  if (!is.null(title) && (!is.character(title) || length(title) != 1)) {
    stop("'title' must be a single character string.")
  }
  
  # validate sidebar_width
  if (!is.numeric(sidebar_width) || length(sidebar_width) != 1 || sidebar_width <= 0 || sidebar_width != as.integer(sidebar_width)) {
    stop("'sidebar_width' must be a single positive integer.")
  }
  
  # validate num_threads
  if (!is.numeric(num_threads) || length(num_threads) != 1 || num_threads != as.integer(num_threads)) {
    stop("'num_threads' must be a single integer value.")
  }
  
  # extract dataset name
  dataset_name <- deparse(substitute(data))
  
  # launch app with golem options
  # this is the entry point to the app
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list(data = data,
                      BoostaR_models = BoostaR_models,
                      GlimmaR_models = GlimmaR_models,
                      dataset_name = dataset_name,
                      kpi_spec = kpi_spec,
                      filter_spec = filter_spec,
                      feature_spec = feature_spec,
                      specification_path = specification_path,
                      show_DataR = show_DataR,                      
                      show_ChartaR = show_ChartaR,
                      show_MappaR = show_MappaR,
                      show_BoostaR = show_BoostaR,
                      show_GlimmaR = show_GlimmaR,
                      show_DevelopaR = show_DevelopaR,
                      starting_tab = starting_tab,
                      starting_response = starting_response,
                      sidebar_width = sidebar_width,
                      num_threads = num_threads,
                      title = title
                      )
  )
}
