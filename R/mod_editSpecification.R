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
mod_editSpecification_server <- function(id, input_spec, type, dimensions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output_spec <- reactiveVal()
    observeEvent(input_spec(), {
      # if the input and output specs are identical then no need to update
      # and trigger reactivity
      if(!identical(input_spec(), output_spec())){
        output_spec(input_spec())
        if(!is.null(input_spec())){
          output$specification <- renderRHandsontable({
            rhandsontable(
              input_spec(),
              selectCallback = TRUE, rowHeaders = FALSE, columnSorting = TRUE, stretchH = "all", height = dimensions()[2] - 400
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

load_specification <- function(d, specification, specification_type){
  # NULL - will check the working directory for folders called feature_spec etc
  # function's operation depends on class of spec
  # character - loads the csv
  # data.frame 
  x <- NULL
  if(is.null(specification)){
    # check the working directory for folders containing specifications
    x <- specification_template(d, specification_type)
  } else if (inherits(specification, 'character')){
    # check it is a .csv file and it exists
    if(file.exists(specification)){
      if(specification_type=='filter'){
        x <- fread(specification, sep = NULL)
      } else {
        x <- fread(specification)
      }
      x[is.na(x)] <- ''
      #showNotification(paste0(specification, ' loaded'), duration = 5, type = 'message')
    } else {
      x <- specification_template(d, specification_type)
      if(!is.null(get_golem_options('specification_path')) | !is.null(get_golem_options(paste0(specification_type, '_spec')))){
        showNotification(paste0(specification_type, '_spec: "', specification, '" not found'), duration = NULL, type = 'error')
      }
    }
  } else if (inherits(specification, 'data.table')){
    x <- setDT(specification)
  }
  if(!is.null(x)){
    # check specification is in correct format
    valid_spec <- check_specification(x, specification_type)
  }
  if(valid_spec){
    # valid specification
    # make logical columns character - otherwise rhandontable will render as logical
    logical_cols <- names(x)[which(as.vector(x[,lapply(.SD, class)]) == "logical")]
    if(length(logical_cols)>0){
      x[, (logical_cols):= lapply(.SD, as.character), .SDcols = logical_cols]
    }
  } else {
    # invalid specification - use default
    showNotification(paste0('Invalid ', specification_type, ' specification'), duration = NULL, type = 'error')
    x <- specification_template(d, specification_type)
  }
  x
}

specification_template <- function(d, spec_type){
  if(spec_type=='feature'){
    spec <- data.table(feature=names(d),
                       base_level='',
                       min='',
                       max='',
                       banding='',
                       monotonicity='',
                       interaction_grouping='',
                       scenario1='',
                       scenario2='',
                       scenario3='')
  } else if (spec_type=='kpi'){
    spec <- data.table(kpi_name='User defined',
                       kpi_numerator='Numerator',
                       kpi_denominator='Denominator',
                       kpi_dp=0,
                       kpi_signif=0,
                       kpi_divisor=1,
                       kpi_prefix='',
                       kpi_suffix='')
  } else if (spec_type=='filter'){
    spec <- data.table(filter='no filter')
  } else {
    spec <- NULL
  }
  return(spec)
}