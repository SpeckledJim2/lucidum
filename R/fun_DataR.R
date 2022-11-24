add_col <- function(d, dt_update){
  new_col_name <- paste0('V',ncol(d)+1)
  d[,(new_col_name):=dt_update]
}
format_dataset_for_DT <- function(d, transpose){
  max_rows_to_display <- 100
  max_cols <- 100
  if('user_filter' %in% names(d)){
    d_filter <- d[which(user_filter==1)]
  } else {
    d_filter <- d 
  }
  if(transpose==FALSE){
    pg_length <- min(max_rows_to_display, nrow(d_filter))
  } else {
    if(nrow(d_filter)>max_cols){
      idx <- 1:max_cols
      d_filter <- utils::head(d_filter, max_cols)
    } else {
      idx <- 1:nrow(d_filter)
    }
    d_filter <- cbind(data.table(col = names(d_filter), t(d_filter)))
    names(d_filter) <- c('dataset_column', as.character(idx))
    pg_length <- min(1000, nrow(d_filter))
  }
  dt <- DT::datatable(d_filter,
                      rownames= FALSE,
                      extensions = 'Buttons',
                      class = 'white-space: nowrap',
                      options = list(pageLength = pg_length,
                                     #initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                     dom = 'Brtip',
                                     scrollX = T,
                                     scrollY = 'calc(90vh - 220px)',
                                     searchHighlight=TRUE
                                     )
                      ) |>
    DT::formatStyle(1:ncol(d_filter), lineHeight='0%', fontSize = '12px')
}