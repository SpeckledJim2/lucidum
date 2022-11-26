#' @import shiny
#' @importFrom data.table fread fwrite setDT
#' @importFrom fs path_home
#' @importFrom rhandsontable hot_to_r renderRHandsontable rhandsontable rHandsontableOutput
#' @importFrom shinyFiles getVolumes parseFilePaths parseSavePath shinyFileChoose shinyFileSave shinyFilesButton shinySaveButton
mod_editSpecification_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        actionButton(inputId = ns('push_specification'), label = 'Use specification', icon = icon("chevron-right"))
      ),
      column(width = 9,
             align = 'right',
             style = 'margin-top: 0px; margin-bottom: -10px',
             shinyFilesButton(
               id = ns('load_specification'),
               label = 'Load',
               filetype=list(txt="csv"),
               icon = icon('download'),
               title = 'Load specification file',
               style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left; padding: 10px 12px",
               multiple = FALSE
             ),
             shinySaveButton(
               id = ns('save_specification'),
               label = 'Save',
               title = 'Save specification file',
               filename = "",
               filetype=list(txt="csv"),
               icon = icon('upload'),
               style="color: #fff; background-color: #4bb03c; border-color: #3e6e37; text-align: left; padding: 10px 12px",
               viewtype = "detail"
             )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        align = 'right',
        p('right click in table to add/remove rows', style = 'font-size: 12px; margin: 0 0 0 0')
      )
    ),
    div(rHandsontableOutput(ns('specification')), style = 'font-size: 12px')
  )

}
mod_editSpecification_server <- function(id, input_spec, type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output_spec <- reactiveVal()
    observeEvent(input_spec, {
      # if the input and output specs are identical then no need to update
      # and trigger reactivity
      if(!identical(input_spec(), output_spec())){
        output_spec(input_spec())
        if(!is.null(input_spec())){
          output$specification <- renderRHandsontable({
            rhandsontable(
              input_spec(),
              selectCallback = TRUE, rowHeaders = FALSE, columnSorting = TRUE, stretchH = "all", height = 500
            )
          })
        }
      }

    })
    # handles loading a new specification
    observe({
      volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
      shinyFileChoose(input, "load_specification", roots=volumes, session=session)
      fileinfo <- parseFilePaths(volumes, input$load_specification)
      isolate({
        if (nrow(fileinfo) > 0) {
          dt <- fread(fileinfo$datapath, header = TRUE, sep = ',')
          # check file for appropriateness
          valid_spec <- check_specification(dt, type)
          if(valid_spec){
            # ensure correct column formats
            # valid specification
            # make logical columns character - otherwise rhandontable will render as logical
            logical_cols <- names(dt)[which(as.vector(dt[,lapply(.SD, class)]) == "logical")]
            if(length(logical_cols)>0){
              dt[, (logical_cols):= lapply(.SD, as.character), .SDcols = logical_cols]
            }
            output$specification <- rhandsontable::renderRHandsontable({
              rhandsontable::rhandsontable(
                dt,
                selectCallback = TRUE,
                rowHeaders = FALSE,
                columnSorting = TRUE,
                stretchH = "all",
                height = 'calc(50vh - 50px)'
              )
            })
            confirmSweetAlert(session = session, type = 'success', inputId = "spec_load_OK",
                              title = paste0(type, ' specification loaded'),
                              btn_labels = c('OK')
            )
          } else {
            confirmSweetAlert(session = session, type = 'error', inputId = "spec_load_error",
                              title = paste0('Error loading ',type, ' specification'),
                              text = 'Check file headers',
                              btn_labels = c('OK')
                              )
          }
        }
      })
    })
    # handles saving the specification
    observe({
      volumes <- c('Home' = fs::path_home(), shinyFiles::getVolumes()())
      shinyFileSave(input, "save_specification", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$save_specification)
      if(nrow(fileinfo)>0){
        dt <- setDT(rhandsontable::hot_to_r(isolate({input$specification})))
        fwrite(dt, fileinfo$datapath)
      }
    })
    # handles returning the specification
    observeEvent(input$push_specification, {
      output_spec(setDT(rhandsontable::hot_to_r(input$specification)))
    })
    return(output_spec)
  })
}

check_specification <- function(d, spec_type){
  required_cols <- list(feature = c('feature','base_level','min','max','interaction_grouping'),
                        kpi = c('kpi_name','kpi_numerator','kpi_denominator','kpi_dp','kpi_signif','kpi_divisor','kpi_prefix','kpi_suffix'),
                        filter = c('filter'))
  all(required_cols[[spec_type]] %in% names(d))
}