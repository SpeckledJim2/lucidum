add_col <- function(d, dt_update){
  new_col_name <- paste0('V',ncol(d)+1)
  d[,(new_col_name):=dt_update]
}
format_table <- function(d){
  user_filter <- NULL
  if('user_filter' %in% names(d)){
    d_subset <- d[which(user_filter==1)]
  } else {
    d_subset <- d 
  }
  dt <- DT::datatable(d_subset,
                      rownames= FALSE,
                      extensions = 'Buttons',
                      class = 'white-space: nowrap',
                      options = list(pageLength = 100,
                                     dom = 'Brtip',
                                     scrollX = T,
                                     scrollY = 'calc(90vh - 100px)',
                                     searchHighlight=TRUE
                                     )
                      ) |>
    DT::formatStyle(1:ncol(d), lineHeight='30%', fontSize = '85%')
}