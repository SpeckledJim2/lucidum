#' MappaR UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets searchInput spectrumInput
#' @importFrom leaflet leaflet leafletOptions addTiles addProviderTiles addLayersControl hideGroup 
#' @importFrom leaflet addEasyButton easyButton setView JS layersControlOptions providers
#' 
#' @noRd
#' 
mod_MappaR_ui <- function(id){
  button_style_move_map <- 'padding:3px; font-size:80%; margin-left:0px; margin-right:0px;color: #000; border-color: #3e6e37'
  button_style_update <- 'padding:3px; font-size:80%; margin-left:0px; margin-right:0px;color: #fff; background-color: #4bb03c; border-color: #3e6e37'
  ns <- NS(id)
  tagList(
    # height argument ensures map resizes well with browser
    leaflet::leafletOutput(ns('map'), height = 'calc(99vh - 69px)'),
    
    # QUESTION - better way to do the following?
    # not sure how I could move to an external file because of the ns
    tags$head(tags$style(HTML(paste0('#', ns('map'), '{background-color: rgb(36,45,49)}')))),
    tags$head(tags$style(HTML(paste0('#', ns('controls'), '{background-color: rgba(255,255,255,0.9)}')))),
    tags$head(tags$style(HTML(paste0('#', ns('controls'), '{border-width: 2px; border-color: rgb(255,255,255)}')))),
    tags$head(tags$style(HTML(paste0('#', ns('panel_title'), ' {font-size: 48px; font-weight: 300; text-align:center}')))),
    tags$head(tags$style(HTML(paste0('#', ns('panel_location'), '{font-size: 20px; text-align:center}')))),
    tags$head(tags$style(HTML(paste0('#', ns('filters'), '{margin-top:5px; font-size: 14px; text-align:center; font-weight: 600}')))),
    tags$script(paste0("Shiny.addCustomMessageHandler('background-color', function(color) {var map = document.getElementById('" , ns('map') , "') ;map.style.backgroundColor = color;});")),
    absolutePanel(id = ns('controls'),
                  class = 'panel panel-default',
                  top = '25%',
                  right = '2%',
                  width = 250,
                  fixed=TRUE,
                  draggable = TRUE,
                  height = "auto",
                  fluidRow(
                    column(width = 12,
                           align = 'center',
                           textOutput(ns('panel_title')),
                           htmlOutput(ns('panel_location')),
                           textOutput(ns('panel_value')),
                           textOutput(ns('filters'))
                    )
                  ),
                  br(),
                  fluidRow(
                    column(width = 12,
                           searchInput(
                             inputId = ns('postcode'),
                             label = NULL,
                             placeholder = "enter postcode area",
                             btnReset = icon("xmark"),
                             btnSearch = icon("magnifying-glass")
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           sliderInput(
                             inputId = ns('line_thickness'),
                             label = 'Line thickness',
                             min = 0,
                             max = 5,
                             value = 1,
                             step = 0.5,
                             ticks = FALSE,
                             width = '100%'
                           ),
                           sliderInput(
                             inputId = ns('opacity'),
                             label = 'Opacity',
                             min = 0,
                             max = 1,
                             value = 1.00,
                             step = 0.2,
                             ticks = FALSE,
                             width = '100%'
                           ),
                    ),
                    column(width = 6,
                           sliderInput(
                             inputId = ns('hotspots'),
                             label = 'Hot/not-spots',
                             min = -20,
                             max = 20,
                             value = 0,
                             step = 5,
                             ticks = FALSE,
                             width = '100%'
                           ),
                           sliderInput(
                             inputId = ns('label_size'),
                             label = 'Label size',
                             min = 0,
                             max = 20,
                             value = 0,
                             step = 1,
                             ticks = FALSE,
                             width = '100%'
                           )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 5,
                      align = 'left',
                      radioGroupButtons(
                        inputId = ns('palettes'),
                        label = NULL,
                        justified = TRUE,
                        size = 'xs',
                        choiceValues = c('Divergent','Spectral','Viridis'),
                        choiceNames = c(
                          tagList(tags$img(src='www/divergent.png', height="18px", width="18px",'')),
                          tagList(tags$img(src='www/spectral.png', height="18px", width="18px",'')),
                          tagList(tags$img(src='www/viridis.png', height="18px", width="18px",''))
                          ),
                        selected = 'Divergent'
                        )
                      ),
                    column(
                      width = 7,
                      align = 'right',
                      radioGroupButtons(
                        inputId = ns('resolution'),
                        label = NULL,
                        justified = TRUE,
                        size = 'xs',
                        choices = c('Area','Sector','Unit'),
                        selected = 'Area'
                        )
                      )
                    ),
                      fluidRow(
                        column(
                          width = 5,
                          align = 'left',
                          radioGroupButtons(
                            inputId = ns('dark_mode'),
                            label = NULL,
                            justified = TRUE,
                            size = 'xs',
                            choices = c('Light','Dark'),
                            selected = 'Dark'
                            )
                          ),
                        column(
                          width = 7,
                          align = 'center',
                          radioGroupButtons(
                            inputId = ns('max_units'),
                            label = NULL,
                            justified = TRUE,
                            size = 'xs',
                            choiceValues = c(50000,250000),
                            choiceNames = c('50k','250k'),
                            selected = 50000
                            )
                          )
                        )
                  )
    )
}
    
#' MappaR Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param d data.frame or data.table
#' @param dt_update update
#' @param response character name of numerator
#' @param weight character name of denominator
#' @param kpi_spec data.table containing the kpi_specification
#' 
#' @importFrom htmlwidgets onRender
#' @importFrom shinyWidgets updateSpectrumInput updateRadioGroupButtons
#' @importFrom leaflet leafletProxy
#' 
#' @noRd
#' 
mod_MappaR_server <- function(id, d, dt_update, response, weight, kpi_spec, selected_tab, show_MappaR, filters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    plot_postcode_area <- reactiveVal('PO')
    trigger_update <- reactiveVal(FALSE)
    output$map <- leaflet::renderLeaflet({base_map()})
    outputOptions(output, "map", suspendWhenHidden = FALSE) # ensures base map is drawn even when not visible
    # map options from input
    map_options <- reactiveVal()
    observeEvent(c(input$line_thickness,
                   input$opacity,
                   input$hotspots,
                   input$label_size,
                   input$palettes,
                   input$resolution,
                   input$max_units,
                   input$sectors), {
      map_options(
        list(
          line_thickness = input$line_thickness,
          opacity = input$opacity,
          hotspots = input$hotspots,
          label_size = input$label_size,
          palette = input$palettes,
          resolution = input$resolution,
          max_units = input$max_units,
          label_size = input$label_size
        )
      )
    })
    observeEvent(c(dt_update(), d(), response(), weight(), kpi_spec(), map_options(), plot_postcode_area()), {
      trigger_update(TRUE)
    })
    observeEvent(c(trigger_update(), selected_tab()), {
      if(trigger_update()){
        # only update when MappaR tab is selected (as otherwise will redraw in background and slow up app)
        if(show_MappaR & selected_tab()=='MappaR'){
          viz_create_map(leafletProxy('map'), d(), response(), weight(), kpi_spec(), map_options(), plot_postcode_area())
          trigger_update(FALSE)
        }
      }
    })
    observeEvent(input$dark_mode, {
      if(input$dark_mode=='Dark'){
        session$sendCustomMessage("background-color", "#242d31")
      } else {
        session$sendCustomMessage("background-color", "#FFFFFF")
      }
    })
    observeEvent(input$postcode, ignoreInit = TRUE,{
      if(input$postcode!=''){
        # find and zoom
        coords_and_zoom <- coords(input$postcode)
        postcode_centroid <- coords_and_zoom[[1]]
        zoom_level <- coords_and_zoom[[2]]
        if(!is.null(postcode_centroid)){
          leafletProxy("map", session) %>% setView(lng=postcode_centroid[[1]],lat=postcode_centroid[[2]],zoom=zoom_level)
          if(nchar(input$postcode)>2){
            postcode_area <- substr(input$postcode,1,regexpr('[0-9]', input$postcode)-1)
            plot_postcode_area(postcode_area)
          } else {
            plot_postcode_area(input$postcode)
          }
        } else {
          showNotification('Postcode not found', type = 'error')
        }
      }
    })
    observeEvent(filters(), {
      # filter text
      train_test_filter <- filters()$train_test_filter
      user_filter <- filters()$user_filter
      if(train_test_filter=='All'){train_test_filter <- ''}
      if(train_test_filter=='Train'){train_test_filter <- 'Training data'}
      if(train_test_filter=='Test'){train_test_filter <- 'Test data'}
      output$filters <- renderText({
        paste0(train_test_filter, ' ', user_filter)
      })
    })
  })
}

#' Create HTML to pass to leaflet
#'
#' @param d data.table containing columns to plot on map
#' @param response numerator of the value plotted on the map
#' @param weight denominator of the value plotted on the map
#' @param map_options list of map styling options
#'
#' @noRd
#' 
#' @import sf
#' @importFrom leaflet leafletProxy clearShapes clearMarkers clearControls addMapPane colorBin
#' @importFrom leaflet addPolygons labelOptions highlightOptions pathOptions addLabelOnlyMarkers addCircles
#' @importFrom grDevices colorRamp rgb
#' @importFrom stats quantile
#'
viz_create_map <- function(map, d, response, weight, kpi_spec, map_options, plot_postcode_area){
  # check inputs are valid
  if(!is.null(d) &
     !is.null(response) &
     !is.null(weight) &
     response %in% names(d) &
     weight %in% c('N', 'no weights', names(d)) &
     response != 'select feature'){
    
    # summarise data by PostcodeArea and merge onto area shapefile
    area_summary <- NULL
    sector_summary <- NULL
    unit_summary <- NULL
    if('PostcodeArea' %in% names(d)){
      area_summary <- postcode_summary(d, response, weight, 'PostcodeArea')
      if(!is.null(area_summary)){
        setDF(area_summary)
        if(weight=='no weights'){
          area_summary$area_plot <- area_summary[,3]
        } else {
          area_summary$area_plot <- area_summary[,3]/area_summary[,2]
        }
        areas_sf <- merge(x=uk_areas, y=area_summary, by = 'PostcodeArea', all.x = TRUE)
      }
    }
    # summarise data by PostcodeSector and merge onto sector shapefile
    if(map_options$resolution %in% c('Sector','Unit') & 'PostcodeSector' %in% names(d)){
      sector_summary <- postcode_summary(d, response, weight, 'PostcodeSector')
      if(!is.null(sector_summary)){
        setDF(sector_summary)
        if(weight=='no weights'){
          sector_summary$sector_plot <- sector_summary[,3]
        } else {
          sector_summary$sector_plot <- sector_summary[,3]/sector_summary[,2]
        }
        sectors_sf <- merge(x=uk_sectors, y=sector_summary, by = 'PostcodeSector', all.x = TRUE)
      }
    }
    # summarise data by PostcodeUnit if lat and long present
    if(map_options$resolution == 'Unit'  & 'PostcodeUnit' %in% names(d) & 'lat' %in% names(d) & 'long' %in% names(d)){
      unit_summary <- postcode_summary(d, response, weight, 'PostcodeUnit')
      if(!is.null(unit_summary)){
        if(weight=='no weights'){
          unit_summary$unit_plot <- unit_summary[,3]
        } else {
          unit_summary$unit_plot <- unit_summary[,3]/unit_summary[,2]
        }
      }
      # remove rows with NA or NaN
      unit_summary <- unit_summary[!is.nan(lat)]
      unit_summary <- unit_summary[!is.na(lat)]
      # filter if too many rows
      if(nrow(unit_summary)>as.numeric(map_options$max_units)){
        # retain the postcode area and it's neighbours
        areas_to_plot <- uk_areas$PostcodeArea[unlist(uk_areas$neighbours[uk_areas$PostcodeArea==plot_postcode_area])]
        areas_to_plot <- c(plot_postcode_area, areas_to_plot)
        unit_summary[, PostcodeArea := substr(PostcodeUnit,1,regexpr('[0-9]', PostcodeUnit)-1)]
        unit_summary <- unit_summary[PostcodeArea %in% areas_to_plot]
        if(nrow(unit_summary)>as.numeric(map_options$max_units)){
          # still too many points
          # retain just the postcode area
          unit_summary <- unit_summary[PostcodeArea==plot_postcode_area]
        }
        unit_summary[, PostcodeArea := NULL]
      }
    }
    # clear the map
    m <- map |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::clearControls()
    # show labels and label size
    if(map_options$label_size==0){
      show_area_labels <- FALSE
    } else {
      show_area_labels <- TRUE
    }
    label_size <- ifelse(map_options$label_size==0,0,map_options$label_size+5)
    # set colours
    if(map_options$palette=='Spectral'){
      map_options$colour1 = 'blue'
      map_options$colour2 = 'yellow'
      map_options$colour3 = 'red'
    } else if(map_options$palette=='Divergent') {
      map_options$colour1 = 'darkgreen'
      map_options$colour2 = 'white'
      map_options$colour3 = 'red'
    } else if(map_options$palette=='Viridis'){
      map_options$colour1 = 'purple'
      map_options$colour2 = 'green'
      map_options$colour3 = 'yellow'
    }
    # area bins, labels and opacity
    if(!is.null(area_summary)){
      bins_area <- unique(stats::quantile(round(area_summary$area_plot,6), na.rm = TRUE, probs = 0:20/20))
      bins_area[1] <- bins_area[1] - 0.000001
      bins_area[length(bins_area)] <- bins_area[length(bins_area)] + 0.000001
      pal_area <- leaflet::colorBin(palette = grDevices::colorRamp(c(map_options$colour1,map_options$colour2,map_options$colour3), interpolate="linear"), domain = NULL, bins = bins_area)
      if(length(bins_area)>1){area_fillColor <- pal_area(areas_sf$area_plot)} else {area_fillColor <- 0}
      area_labels <- apply_kpi_format(areas_sf$area_plot, response, weight, kpi_spec)
      opacity_area_modifier <- hot_spotted_opacity(areas_sf$area_plot, map_options$hotspots)
    }
    # sector bins, labels and opacity
    if(!is.null(sector_summary)){
      bins_sector <- unique(stats::quantile(round(sector_summary$sector_plot,6), na.rm = TRUE, probs = 0:20/20))
      bins_sector[1] <- bins_sector[1] - 0.000001
      bins_sector[length(bins_sector)] <- bins_sector[length(bins_sector)] + 0.000001
      pal_sector <- leaflet::colorBin(palette = grDevices::colorRamp(c(map_options$colour1,map_options$colour2,map_options$colour3), interpolate="linear"), domain = NULL, bins = bins_sector)
      if(length(bins_sector)>1){sector_fillColor <- pal_sector(sectors_sf$sector_plot)} else {sector_fillColor <- 0}
      sector_labels <- apply_kpi_format(sectors_sf$sector_plot, response, weight, kpi_spec)
      opacity_sector_modifier <- hot_spotted_opacity(sectors_sf$sector_plot, map_options$hotspots)
    }
    # unit bins, labels and opacity
    if(!is.null(unit_summary)){
      bins_unit <- unique(stats::quantile(round(unit_summary$unit_plot,6), na.rm = TRUE, probs = 0:20/20))
      bins_unit[1] <- bins_unit[1] - 0.000001
      bins_unit[length(bins_unit)] <- bins_unit[length(bins_unit)] + 0.000001
      pal_unit <- leaflet::colorBin(palette = grDevices::colorRamp(c(map_options$colour1,map_options$colour2,map_options$colour3), interpolate="linear"), domain = NULL, bins = bins_unit)
      if(length(bins_unit)>1){unit_fillColor <- pal_unit(unit_summary$unit_plot)} else {unit_fillColor <- 0}
      unit_labels <- apply_kpi_format(unit_summary$unit_plot, response, weight, kpi_spec)
    }
    # add on sectors if available - sectors before areas to get polygon order correct
    label_style <- list('box-shadow' = '3px 3px rgba(0,0,0,0.25)','font-size' = '16px','border-color' = 'rgba(0,0,0,0.5)')
    if(!is.null(sector_summary)){
      if(!is.null(unit_summary)){
        notification_message <- paste0('Redrawing map (', format(nrow(unit_summary), big.mark = ','), ' postcode units)...')
      } else {
        notification_message <- 'Redrawing postcode sectors...'
      }
      showNotification(
        notification_message, duration = 10, type = 'warning'
      )
      m |>
        leaflet::addMapPane('sector_polygons', zIndex = 405) %>%
        leaflet::addPolygons(data = sectors_sf,
                             layerId = sectors_sf$PostcodeSector,
                             group = 'Sector',
                             weight = map_options$line_thickness * 0.1,
                             opacity = 1,
                             color = "black",
                             smoothFactor = 0,
                             fillColor = sector_fillColor,
                             fillOpacity = map_options$opacity * opacity_sector_modifier,
                             label = postcode_hover_labels(sectors_sf, sector_labels, response, weight),
                             labelOptions = labelOptions(textOnly = FALSE, style=label_style),
                             highlightOptions = highlightOptions(color='white', weight = 2*map_options$line_thickness, bringToFront = TRUE, sendToBack = TRUE),
                             options = pathOptions(pane = "sector_polygons")
        )
    }
    # add on units if available
    if(!is.null(unit_summary)){
      m %>%
        addMapPane('points', zIndex = 420) %>%
        addCircles(data = unit_summary,
                   layerId = unit_summary$PostcodeUnit,
                   lng=unit_summary$long,
                   lat=unit_summary$lat,
                   label = postcode_hover_labels(unit_summary, unit_labels, response, weight),
                   labelOptions = labelOptions(textOnly = FALSE,style=label_style),
                   radius = 50 * map_options$line_thickness^2,
                   weight = 0,
                   stroke = FALSE,
                   fill = TRUE,
                   fillColor = unit_fillColor,
                   fillOpacity = ifelse(is.na(unit_summary$unit_plot),0.5,1.0),
                   highlightOptions = highlightOptions(color='white', opacity = 1, weight = 1, fillOpacity = 1, bringToFront = TRUE, sendToBack = TRUE),
                   group = "Unit",
                   options = pathOptions(pane = "points")
        )
    }
    # add on the area polygons
    if(!is.null(area_summary)){
      m |>
        leaflet::addMapPane('area_polygons', zIndex = 405) |>
        leaflet::addPolygons(
          data = areas_sf,
          layerId = areas_sf$PostcodeArea,
          group = 'Area',
          weight = map_options$line_thickness,
          opacity = map_options$opacity,
          color = "black",
          smoothFactor = 0,
          fillColor = area_fillColor,
          fillOpacity = map_options$opacity * opacity_area_modifier,
          label = postcode_hover_labels(areas_sf, area_labels, response, weight),
          labelOptions = labelOptions(textOnly = FALSE, style=label_style),
          highlightOptions = highlightOptions(color='white', weight = 2*map_options$line_thickness, bringToFront = TRUE, sendToBack = TRUE),
          options = pathOptions(pane = "area_polygons")) |>
        addLabelOnlyMarkers(
          lng = areas_sf$X,
           lat = areas_sf$Y,
           label = lapply(paste(sep = "", '<b>',areas_sf$PostcodeArea,'</b><br/>',area_labels), HTML),
           labelOptions = labelOptions(
             style = list('color' = "black", 'font-size' = paste0(label_size, 'px')),
             noHide = show_area_labels,
             direction = 'center',
             textOnly = TRUE)
          )
    }
  }
}

postcode_hover_labels <- function(postcode_summary, labels, response, weight){
  resolution <- names(postcode_summary)[1]
  weights <- postcode_summary[[weight]]
  if(resolution=='PostcodeArea'){
    nmes <- postcode_area_name_mapping[order(PostcodeArea),PostcodeArea_name]
    postcode_area_name_mapping <- postcode_area_name_mapping[order(PostcodeArea)]
    lapply(
      paste(
        sep = "",
        "<span style='font-size:48px; font-weight:200'>",
        postcode_summary$PostcodeArea,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400; color: rgb(60,141,188)'>",
        nmes,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400'>",
        response,
        ': ',
        "</span>",
        "<span style='font-size:16px; font-weight:400'>",
        labels,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400; color: grey'>",
        weight,
        ': ',
        if(weight=='N'){format(weights, digits = 1, big.mark = ',')} else {format(weights, nsmall = 2, big.mark = ',')},
        "</span>"
      ),
      HTML
    )
  } else if (resolution=='PostcodeSector'){
    postcode_summary <- merge(postcode_summary, postcode_area_name_mapping, by = 'PostcodeArea', all.x = TRUE)
    postcode_summary <- postcode_summary[order(postcode_summary$PostcodeSector),]
    lapply(
      paste(
        sep = "",
        "<span style='font-size:48px; font-weight:200'>",
        postcode_summary$PostcodeSector,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400; color: rgb(60,141,188)'>",
        postcode_summary$PostcodeArea_name,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400'>",
        response,
        ': ',
        "</span>",
        "<span style='font-size:16px; font-weight:400'>",
        labels,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400; color: grey'>",
        weight,
        ': ',
        if(weight=='N'){format(weights, digits = 1, big.mark = ',')} else {format(weights, nsmall = 2, big.mark = ',')},
        "</span>"
      ),
      HTML
    )
  } else if (resolution=='PostcodeUnit'){
    postcode_summary[, PostcodeArea := substr(PostcodeUnit,1,regexpr('[0-9]', PostcodeUnit)-1)]
    postcode_summary <- merge(postcode_summary, postcode_area_name_mapping, by = 'PostcodeArea', all.x = TRUE)
    setorder(postcode_summary, 'PostcodeUnit')
    lapply(
      paste(
        sep = "",
        "<span style='font-size:48px; font-weight:200'>",
        postcode_summary$PostcodeUnit,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400; color: rgb(60,141,188)'>",
        postcode_summary$PostcodeArea_name,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400'>",
        response,
        ': ',
        "</span>",
        "<span style='font-size:16px; font-weight:400'>",
        labels,
        "</span>",
        '<br/>',
        "<span style='font-size:16px; font-weight:400; color: grey'>",
        weight,
        ': ',
        if(weight=='N'){format(weights, digits = 1, big.mark = ',')} else {format(weights, nsmall = 2, big.mark = ',')},
        "</span>"
      ),
      HTML
    )
  }
}

#' Summarise dataset columns by postcode
#'
#' @param d data.table
#' @param rows_to_summarise vector row indices to include in the summary
#' @param response character numerator column to sum across groups
#' @param weight character denominator column to sum across groups (use N for equal weights per row and "no weights" to ignore denominator)
#' @param resolution character one of PostcodeArea, PostcodeSector or PostcodeUnit
#'
#' @noRd
#'
postcode_summary <- function(d, response, weight, resolution){
  # extract the columns we need to make the summary
  if(resolution %in% c('PostcodeArea','PostcodeSector')){
    cols <- c(resolution, response)
  } else if (resolution=='PostcodeUnit'){
    cols <- c(resolution, 'lat', 'long', response)
  }
  cols_w <- c(cols, weight)
  if(weight %in% c('N','no weights')){
    d_cols <- d[total_filter==1L, .SD, .SDcols = cols]
    d_cols[, weight := 1]
  } else {
    d_cols <- d[total_filter==1L, .SD, .SDcols = cols_w]
  }
  # summarise columns
  if(resolution %in% c('PostcodeArea','PostcodeSector')){
    setnames(d_cols, c('resolution','response','weight'))
    summary <- d_cols[, list(V1 = sum(weight, na.rm = TRUE), V2 = sum(response, na.rm = TRUE)), by = 'resolution']
    setnames(summary, c(resolution, weight, response))
  } else if (resolution=='PostcodeUnit'){
    setnames(d_cols, c('resolution','lat','long','response','weight'))
    summary <- d_cols[, list(
      V1 = sum(weight, na.rm = TRUE),
      V2 = sum(response, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE),
      long = mean(long, na.rm = TRUE)
      ), by = 'resolution']
    setnames(summary, c(resolution, weight, response, 'lat', 'long'))
    setorder(summary, 'PostcodeUnit')
  }
  return(summary)
}
base_map <- function(){
  leaflet(options = leafletOptions(preferCanvas = TRUE, zoomControl = FALSE, attributionControl=TRUE)) |>
    addTiles(group = "OSM") |>
    addProviderTiles("Esri.WorldStreetMap", group = 'Esri') |>
    addProviderTiles('Esri.WorldGrayCanvas', group = "Grey") |>
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    addLayersControl(
      baseGroups = c('Blank','Esri','Grey','OSM','Satellite'),
      overlayGroups = c('Area','Sector','Unit'),
      options = layersControlOptions(position = "topleft", collapsed = FALSE, autoZIndex = TRUE)) |>
    hideGroup(c('Sector','Unit')) |>
    htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topleft' }).addTo(this)}")  |>
    addEasyButton(easyButton(icon="fa-globe", title="Reset", onClick=JS("function(btn, map){map.setView([54.81,-1],6);}"))) |>
    setView(lng=-1,lat=54.81,zoom=6)
}
hot_spotted_opacity <- function(p, hotspots){
  if(hotspots==0){
    opacity_modifier <- 1
  } else if (hotspots>0){
    opacity_modifier <- ifelse(!is.na(p) & p > maxN(p-1e-06, hotspots),1,0)
  } else if (hotspots<0){
    opacity_modifier <- ifelse(!is.na(p) & p < -maxN(-p-1e-06, -hotspots),1,0)
  }
}
maxN <- function(x, N=2){
  len <- length(x)
  # replace NAs with smallest value in x
  x[is.na(x)] <- min(x, na.rm = TRUE)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}
return_mouse_hover_postcode <- function(pointId){
  if(nchar(pointId)>2){
    pointId_area <- substr(pointId,1,regexpr('[0-9]', pointId)-1)
  } else {
    pointId_area <- pointId
  }
  postcode_area_name_mapping[PostcodeArea==pointId_area, PostcodeArea_name]
}
apply_kpi_format <- function(x, response, weight, kpi_spec){
  kpi_numerator <- NULL
  kpi_denominator <- NULL
  # function to format the number x according to whatever format has been defined in the kpi_spec
  if(is.numeric(x) & !is.null(response) & !is.null(weight)){
    if(inherits(x, c('data.frame','matrix'))){
      n_row <- nrow(x)
      n_col <- ncol(x)
    } else {
      n_row <- length(x)
      n_col <- 1
    }
    format_row <- kpi_spec[kpi_numerator==response & kpi_denominator==weight,]
    if(nrow(format_row)>1){
      # duplicate rows int he kpi specification
      # take just the first row
      format_row <- format_row[1,]
    }
    if(nrow(format_row)>0){
      significant_digits <- as.numeric(format_row$kpi_signif)
      divisor <- as.numeric(format_row$kpi_divisor)
      decimal_places <- as.numeric(format_row$kpi_dp)
      prefix <- format_row$kpi_prefix
      suffix <- format_row$kpi_suffix
      if(is.na(significant_digits)) significant_digits <- 6
      if(is.na(decimal_places)) decimal_places <- 3
      if(is.na(divisor)) divisor <- 1
      if(is.na(prefix)) prefix <- ''
      if(is.na(suffix)) suffix <- ''
      # format number
      x_MappaR <- x / divisor
      if(is.null(n_row)){n_row <- 1}
      if(is.null(n_col)){n_col <- 1}
      if(!is.na(decimal_places) & is.numeric(decimal_places)){
        x_MappaR <- format(round(x_MappaR,decimal_places), nsmall = decimal_places, big.mark = ',')
      } else {
        x_MappaR <- format(x_MappaR, digits = significant_digits, big.mark = ',')
      }
      # make matrix and remove any white space introduced by format
      x_MappaR <- trimws(x_MappaR)
      x_MappaR <- paste(sep = '', prefix, x_MappaR, suffix)
      x_MappaR <- matrix(x_MappaR, nrow = n_row, ncol = n_col)
    } else {
      # simple format depending on magnitude of number
      m <- mean(x, na.rm = TRUE)
      if(!is.na(m)){
        if(log10(abs(m)+1)<0){
          x_MappaR <- format(round(x,3), nsmall = 3, big.mark = ',')
        } else if (log10(abs(m)+1)<2){
          x_MappaR <- format(round(x,3), nsmall = 2, big.mark = ',')
        } else {
          x_MappaR <- format(round(x,3), nsmall = 0, big.mark = ',')
        }
        x_MappaR <- matrix(x_MappaR, nrow = n_row, ncol = n_col)
      } else {
        x_MappaR <- NA
      }
    }
  } else {
    x_MappaR <- NA
  }
  x_MappaR
}

coords <- function(postcode){
  centroid <- NULL
  zoom <- NULL
  if(nchar(postcode)<3){
    # postcode area
    if(postcode %in% uk_areas$PostcodeArea){
      centroid <- list(uk_areas$X[uk_areas$PostcodeArea==postcode], uk_areas$Y[uk_areas$PostcodeArea==postcode])
      zoom <- 10
    }
  } else if (nchar(postcode)<=6){
    # most likely a postcode sector
    if(postcode %in% uk_sectors$PostcodeSector){
       centroid <- list(uk_sectors$X[uk_sectors$PostcodeSector==postcode], uk_sectors$Y[uk_sectors$PostcodeSector==postcode])
       zoom <- 13
     }
  } else {
    # postcode unit
    # if(postcode %in% uk_units[['PostcodeUnit']]){
    #   centroid <- list(uk_units$X[uk_units$PostcodeUnit==postcode], uk_units$Y[uk_units$PostcodeUnit==postcode])
    #   zoom <- 15
    # }
  }
  return(list(centroid,zoom))
}