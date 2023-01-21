## code to prepare `insurance` dataset goes here
insurance <- data.table::fread('data-raw/insurance.csv', stringsAsFactors = TRUE)

## code to prepare `uk_areas` dataset goes here
uk_sf_area <- sf::st_read('data-raw/areas_MappaR.gpkg', quiet = TRUE)
uk_sf_area$area <- as.numeric(sf::st_area(uk_sf_area))/1000000 # km^2
area_coords <- as.data.frame(sf::st_coordinates(sf::st_centroid(uk_sf_area$geom)))
uk_areas <- cbind(uk_sf_area, area_coords)
uk_areas$population <- NULL; uk_areas$area <- NULL

## code to prepare `uk_sectors` dataset goes here
uk_sf_sector <- sf::st_read('data-raw/sectors_MappaR.gpkg', quiet = TRUE)
sector_coords <- as.data.frame(sf::st_coordinates(sf::st_centroid(uk_sf_sector$geom)))
uk_sectors <- cbind(uk_sf_sector, sector_coords)
uk_sectors$PostcodeDistrict <- NULL; uk_sectors$population <- NULL
uk_sectors$x <- NULL; uk_sectors$y <- NULL; uk_sectors$SectID <- NULL

# internal datasets
postcode_area_name_mapping <- data.table::fread('data-raw/postcode_area_name_mapping.csv')
lgbm_objectives <- data.table::fread('data-raw/lgbm_objectives.csv')
glm_objectives <- data.table::fread('data-raw/glm_objectives.csv')

# usethis for package datasets available to user
usethis::use_data(insurance, overwrite = TRUE)

# usethis internal datasets
usethis::use_data(uk_areas,
                  uk_sectors,
                  postcode_area_name_mapping,
                  lgbm_objectives,
                  glm_objectives,
                  overwrite = TRUE,
                  internal = TRUE)