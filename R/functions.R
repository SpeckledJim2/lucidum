insertDashboardHeader <- function(x){
  tags$li(
    class = "dropdown",
    div(
      style='margin-top:10px; padding-right:0;',
      x
    )
  )
}

#' @importFrom data.table fread
load_dataset <- function(data){
  if(is.null(data)){
    NULL
  } else if(inherits(data, 'character')){
    # check whether file exists
    if(substr(data,nchar(data)-3,nchar(data))=='.csv'){
      if(file.exists(data)){
        fread(file = data, stringsAsFactors = TRUE)
      } else {
        NULL
      }
    } else {
      NULL
    }
  } else if (inherits(data, 'data.frame')){
    setDT(data)
  } else if (inherits(data, 'data.table')){
    data
  }
}

init_lucidum <- function(session, data){

  # if a data.frame is supplied as data then get name
  if(inherits(data, 'data.frame')){
    dataset_name <- deparse(substitute(data))
  } else {
    dataset_name <- NULL
  }
  
  # get the names of the tables in the global environment
  table_lists <- return_global_env_tables()

  # get the dataset name
  if(is.null(data)){
    dataset_name <- 'choose dataset'
    table_lists[[2]] <- c(table_lists[[2]], 'choose dataset')
  } else if(inherits(data, 'character')){
    # the user specified a .csv file
    # so the object has no file name
    dataset_name <- 'loaded from .csv file'
    table_lists[[2]] <- c(table_lists[[2]], 'loaded from .csv file')
  } else if(inherits(data, 'data.frame')){
    dataset_name <- 'user supplied dataset'
    table_lists[[2]] <- c(table_lists[[2]], 'user supplied dataset')
  }
  
  # update the selectInput
  updateSelectInput(session, inputId = 'dataset', choices = table_lists, selected = dataset_name)
}

contains_postcode_cols <- function(d){
  # TRUE if d contains one of PostcodeUnit, PostcodeSector or PostcodeArea
  # 'PostcodeUnit' %in% names(d) | 'PostcodeSector' %in% names(d) | 'PostcodeArea' %in% names(d)
  'PostcodeArea' %in% names(d)
}
valid_lucidum_dt <- function(d){
  valid <- TRUE
  if(nrow(d)<=1) {valid<-FALSE}
  if(ncol(d)<=1) {valid<-FALSE}
  if(length(numerical_cols(d))==0) {valid<-FALSE}
  return(valid)
}
return_global_env_tables <- function(){
  # function returns list of tables present in Global Environment
  # split by whether they contain postcode columns or not
  # with special tables used by Toolkit removed from list
  names_not_allowed <- c('d')

  # get the tables
  starting_tables <- Filter(function(x) is.data.frame(get(x)), ls(envir=.GlobalEnv))
  starting_tables <- Filter(function(x) valid_lucidum_dt(get(x)), starting_tables)
  starting_tables <- c(setdiff(starting_tables, names_not_allowed))
  postcode_tables <- Filter(function(x) contains_postcode_cols(get(x)), starting_tables)
  postcode_tables <- c(setdiff(postcode_tables, names_not_allowed))
  non_postcode_tables <- setdiff(starting_tables, postcode_tables)
  if(length(postcode_tables)==1) postcode_tables <- list(postcode_tables)
  if(length(non_postcode_tables)==1) non_postcode_tables <- list(non_postcode_tables)
  # include number of rows and columns
  n_rows <- function(x){paste0('(',format(nrow(get(x)), big.mark = ','),
                               ' x ',
                               format(ncol(get(x)), big.mark = ',')
                               ,', ',
                               format(utils::object.size(get(x)), units = 'auto'),
                               ')'
  )}
  if(length(postcode_tables)>0){
    names(postcode_tables) <- paste(postcode_tables, lapply(postcode_tables, n_rows))
  }
  if(length(non_postcode_tables)>0){
    names(non_postcode_tables) <- paste(non_postcode_tables, lapply(non_postcode_tables, n_rows))
  }
  list('Datasets with PostcodeArea column for MappaR' = postcode_tables,
       'Other datasets' = non_postcode_tables
  )
}

numerical_cols <- function(d){
  names(d)[as.numeric(which(sapply(d, is.numeric)==TRUE))]
}





