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
#' @importFrom leaflet addPolygons labelOptions highlightOptions pathOptions
#' @importFrom grDevices colorRamp rgb
#' @importFrom stats quantile
#'
viz_create_map <- function(map, d, response, weight, kpi_spec, map_options){
  # check inputs are valid
  if(!is.null(d) &
     !is.null(response) &
     !is.null(weight) &
     response %in% names(d) &
     weight %in% c('N', 'no weights', names(d)) &
     response != 'select feature'){
    
    # summarise data and merge the area_summary onto the shapefile
    area_summary <- NULL
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

    # clear the map
    m <- map |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::clearControls()
    
    # area bins, labels and opacity
    if(!is.null(area_summary)){
      bins_area <- unique(stats::quantile(round(area_summary$area_plot,6), na.rm = TRUE, probs = 0:20/20))
      bins_area[1] <- bins_area[1] - 0.000001
      bins_area[length(bins_area)] <- bins_area[length(bins_area)] + 0.000001
      pal_area <- leaflet::colorBin(palette = grDevices::colorRamp(c(map_options$colour1,map_options$colour2,map_options$colour3), interpolate="linear"), domain = NULL, bins = bins_area)
      if(length(bins_area)>1){area_fillColor <- pal_area(areas_sf$area_plot)} else {area_fillColor <- 0}
      #area_labels <- apply_kpi_format(areas_sf$area_plot, response, weight, kpi_spec)
      opacity_area_modifier <- hot_spotted_opacity(areas_sf$area_plot, map_options$hotspots)
    }
    
    # add on the area polygons
    label_style <- list('box-shadow' = '3px 3px rgba(0,0,0,0.25)','font-size' = '16px','border-color' = 'rgba(0,0,0,0.5)')
    if(!is.null(area_summary)){
      m |>
        leaflet::addMapPane('area_polygons', zIndex = 405) |>
        leaflet::addPolygons(data = areas_sf,
                             layerId = areas_sf$PostcodeArea,
                             group = 'Area',
                             weight = map_options$line_thickness,
                             opacity = map_options$opacity,
                             color = "black",
                             smoothFactor = 0,
                             fillColor = area_fillColor,
                             fillOpacity = map_options$opacity * opacity_area_modifier,
                             label = lapply(paste(sep = "", '<b>',areas_sf$PostcodeArea,'</b><br/>',signif(areas_sf$area_plot,6)), HTML),
                             labelOptions = labelOptions(textOnly = FALSE, style=label_style),
                             highlightOptions = highlightOptions(color='white', weight = 2*map_options$line_thickness, bringToFront = TRUE, sendToBack = TRUE),
                             options = pathOptions(pane = "area_polygons")
        )
    }
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
  rows_to_summarise <- d[total_filter==1,which=TRUE]
  if(length(rows_to_summarise)==nrow(d)){
    if(weight %in% c('N','no weights')){
      d_cols <- d[, .SD, .SDcols = c(resolution, response)]
      d_cols[, weight := 1]
    } else {
      d_cols <- d[, .SD, .SDcols = c(resolution, response, weight)]
    }
  } else {
    if(weight %in% c('N','no weights')){
      d_cols <- d[rows_to_summarise, .SD, .SDcols = c(resolution, response)]
      d_cols[, weight := 1]
    } else {
      d_cols <- d[rows_to_summarise, .SD, .SDcols = c(resolution, response, weight)]
    }
  }
  setnames(d_cols, c('resolution','response','weight'))
  summary <- d_cols[, list(V1 = sum(weight, na.rm = TRUE), V2 = sum(response, na.rm = TRUE)), by = 'resolution']
  setnames(summary, c(resolution, response, weight))
  return(summary)
}

base_map <- function(){
  leaflet(options = leafletOptions(preferCanvas = TRUE, zoomControl = FALSE, attributionControl=TRUE)) |>
    addTiles(group = "OSM") |>
    addProviderTiles("Esri.WorldStreetMap", group = 'Esri') |>
    addProviderTiles(providers$Stamen.TonerLite, group = "Stamen") |>
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    addLayersControl(
      baseGroups = c('Blank','Esri','OSM','Stamen','Satellite'),
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