getColumnChoices <- function(d, numerical_cols = FALSE, subset = NULL, special_options = NULL){
  cols <- NULL
  if(!is.null(d)){
    if(ncol(d)>0){
      if(numerical_cols){
        cols <- names(d)[which(sapply(d,is.numeric))]
      } else {
        cols <- names(d)
      }
      if(!is.null(subset)){
        cols <- setdiff(subset, cols)
      }
      if(!is.null(special_options)){
        cols <- c(special_options, cols)
      }
    }

  }
  return(cols)
}

nav_label <- function(type,
                      kpi,
                      gbm,
                      glm,
                      kpi_spec, BoostaR_models, GlimmaR_models, GlimmaR_model_index, BoostaR_model_index){
  if(type=='KPI'){
    n <- nrow(kpi_spec)
    icon <- icon('gears')
    idx <- kpi
  } else if (type=='GLM'){
    n <- length(GlimmaR_models)
    icon <- icon('star')
    idx <- glm
  } else if (type=='GBM'){
    n <- length(BoostaR_models)
    icon <- icon('rocket')
    idx <- gbm
  }
  return(list(label=paste0(type,'s (',idx,'/',n,')'), icon=icon))
}

insertDashboardHeader <- function(x){
  tags$li(
    class = "dropdown",
    div(
      style='margin-top:10px; padding-right:0;',
      x
    )
  )
}

load_specification <- function(specification, specification_type){
  # NULL - will check the working directory for folders called feature_spec etc
  # function's operation depends on class of spec
  # character - loads the csv
  # data.frame 
  x <- NULL
  if(is.null(specification)){
    # check the working directory for folders containing specifications
    
  } else if (inherits(specification, 'character')){
    # check it is a .csv file
    x <- fread(specification)
  } else if (inherits(specification, 'data.table')){
    x <- setDT(specification)
  }
  if(!is.null(x)){
    # check specification is in correct format
    #x <- check_specification_format(x, type)
  }
  x
}



