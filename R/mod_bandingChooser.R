#' bandingChooser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bandingChooser_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('banding_container')),
  )
}
    
#' bandingChooser Server Functions
#'
#' @noRd 
mod_bandingChooser_server <- function(id, d, user_col, initial_banding){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    col_type <- reactiveVal('NULL')
    banding <- reactiveVal()
    observeEvent(user_col(), {
      col <- user_col()
      type <- 'NULL'
      # get type of column
      if(!is.null(col) & !is.null(d())){
        if(col %in% names(d())){
          if(inherits(d()[[col]],c('factor','character'))){
            type <- 'character'
          } else if (inherits(d()[[col]],c('numeric','integer'))){
            type <- 'numeric'
          } else if (inherits(d()[[col]],'Date')){
            type <- 'date'
          } else if (col=='none'){
            type <- 'NULL'
          }
        }
      }
      col_type(type)
      # calculate what banding should be chosen initially
      # and what should be shown on the widget
      if(type=='numeric'){
        b_display <- banding_displayed(initial_banding())
      } else if(type=='date'){
        b_display <- banding_displayed_date(initial_banding())
      } else {
        b_display <- 'Factor'
      }
      # render widget
      if(type=='date'){
        output$banding_container <- renderUI({
          radioGroupButtons(
            inputId = ns('banding'),
            label = b_display,
            choices = c('<','Day','Week','Mnth','Qtr','Year','>',`<i class='fa fa-lock'></i>` = 'lock'),
            individual = FALSE,
            selected = character(0),
            size = 'xs')
        })
      } else {
        output$banding_container <- renderUI({
          radioGroupButtons(
            inputId = ns('banding'),
            label = b_display,
            choices = c('<','0.01','0.1','1','5','10','100','>',`<i class='fa fa-lock'></i>` = 'lock'),
            individual = FALSE,
            selected = character(0),
            size = 'xs')
        })
      }
      banding(initial_banding())
    })
    observeEvent(input$banding, {
      b <- banding()
      if(length(input$banding>0)){
        if(col_type()=='date'){
          if(input$banding=='<'){
            new_banding <- modify_banding_level_date(b, -1)
          } else if(input$banding=='>'){
            new_banding <- modify_banding_level_date(b, +1)
          } else {
            new_banding <- input$banding
          }
          updateRadioGroupButtons(inputId='banding', label = banding_displayed_date(new_banding), selected = character(0))
        } else if (col_type()=='numeric'){
          if(input$banding=='<'){
            new_banding <- modify_banding_level(b, -1)
          } else if(input$banding=='>'){
            new_banding <- modify_banding_level(b, +1)
          } else {
            new_banding <- as.numeric(input$banding)
          }
          updateRadioGroupButtons(inputId='banding', label = banding_displayed(new_banding), selected = character(0))
        } else {
          new_banding <- NULL
        }
        banding(new_banding)
      }

    })
    return({banding})
  })
}

banding_guesser <- function(x){
  # speed up - just use first 10000 rows
  if(length(x)>10000){x <- x[1:10000]}
  s <- stats::sd(x, na.rm = TRUE)/20
  if(is.na(s) | is.nan(s)) {s <- 1}
  if (s==0){s <- 1}
  exponent <- floor(log10(s))
  mantissa <- s / 10^exponent
  if (mantissa<2){
    m <- 1
  } else if (mantissa<5) {
    m <- 2
  } else {
    m <- 5
  }
  banding <- m * 10^(exponent+1)
  # some special cases to modify banding
  if(length(table(x))<=5 & min(x, na.rm = TRUE)==0 & max(x, na.rm = TRUE)==1) banding <- 1
  if(length(table(x))<=100 & min(x, na.rm = TRUE)<=20 & max(x, na.rm = TRUE)<=100 & max(x, na.rm = TRUE)>=1) banding <- 1
  if(class(x)[1] %in% c('IDate','Date', 'POSIXct')){
    banding <- pmax(1,pmin(10,banding))
  }
  banding
}
banding_guesser_date <- function(x){
  'Month'
}
banding_displayed <- function(b){
  paste0('Banding (',as.character(format(b, big.mark=',', scientific = FALSE)),')')
}
banding_displayed_date <- function(b){
  if(b=='Mnth'){
    b_display <- 'Month'
  } else if(b=='Qtr'){
    b_display <- 'Quarter'
  } else {
    b_display <- b
  }
  paste0('Banding (',b_display,')')
}
modify_banding_level <- function (current_banding_level, modifier){
  # the banding levels are
  # 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50 etc
  # plus some special levels like 4 and 7 and 12
  # function below lets us move through these levels
  if (modifier==0){
    1
  } else {
    current_banding_level <- as.numeric(current_banding_level)
    exponent <- floor(log10(current_banding_level))
    mantissa <- current_banding_level / 10^exponent
    overrule <- NA
    if(modifier==-1){
      # couple of special rules to get 4,7 and 12 bandings in there (good for days/months/quarters)
      if(current_banding_level==20){
        overrule <- 12
      } else if (current_banding_level==12){
        overrule <- 10
      } else if (current_banding_level==10){
        overrule <- 7
      } else if (current_banding_level==7){
        overrule <- 5
      } else if (current_banding_level==5){
        overrule <- 4
      } else if (current_banding_level==4){
        overrule <- 2
      }
      if(mantissa==1){
        mantissa <- 5
        exponent <- exponent - 1
      } else if (mantissa==2) {
        mantissa <- 1
      } else if (mantissa==5){
        mantissa <- 2
      }
    } else if (modifier==1){
      # couple of special rules to get 4 and 12 bandings in there (good for months/quarters)
      if(current_banding_level==2){
        overrule <- 4
      } else if (current_banding_level==4){
        overrule <- 5
      } else if (current_banding_level==5){
        overrule <- 7
      } else if (current_banding_level==7){
        overrule <- 10
      } else if (current_banding_level==10){
        overrule <- 12
      } else if (current_banding_level==12){
        overrule <- 20
      }
      if(mantissa==1){
        mantissa <- 2
      } else if (mantissa==2) {
        mantissa <- 5
      } else if (mantissa==5){
        mantissa <- 1
        exponent <- exponent + 1
      }
    }
    if(is.na(overrule)){
      mantissa * 10^exponent
    } else {
      overrule
    }
  }
}
modify_banding_level_date <- function(current_banding_level, modifier){
  # the banding levels are
  # Day, Week, Month, Qtr (Quarter), Year
  if(modifier==0){
    'Mnth'
  } else if (modifier==-1){
    if(current_banding_level=='Day'){
      'Day'
    } else if (current_banding_level=='Week'){
      'Day'
    } else if (current_banding_level=='Mnth'){
      'Week'
    } else if (current_banding_level=='Qtr'){
      'Mnth'
    } else if (current_banding_level=='Year'){
      'Qtr'
    }
  } else if (modifier==1){
    if(current_banding_level=='Day'){
      'Week'
    } else if (current_banding_level=='Week'){
      'Mnth'
    } else if (current_banding_level=='Mnth'){
      'Qtr'
    } else if (current_banding_level=='Qtr'){
      'Year'
    } else if (current_banding_level=='Year'){
      'Year'
    }
  }
}