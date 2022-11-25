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



