## code to prepare `insurance` dataset goes here
insurance <- data.table::fread('data-raw/insurance.csv', stringsAsFactors = TRUE)

## code to prepare `uk_areas` dataset goes here
uk_sf_area <- sf::st_read('data-raw/areas_MappaR.gpkg', quiet = TRUE)
uk_sf_area$area <- as.numeric(sf::st_area(uk_sf_area))/1000000 # km^2
area_coords <- as.data.frame(sf::st_coordinates(sf::st_centroid(uk_sf_area$geom)))
uk_areas <- cbind(uk_sf_area, area_coords)
postcode_area_name_mapping <- data.table::fread('data-raw/postcode_area_name_mapping.csv')
lgbm_objectives <- data.table::fread('data-raw//lgbm_objectives.csv')
glm_objectives <- data.table::fread('data-raw//glm_objectives.csv')

# usethis for package datasets available to user
usethis::use_data(insurance, overwrite = TRUE)

# usethis internal datasets
usethis::use_data(uk_areas,
                  postcode_area_name_mapping,
                  lgbm_objectives,
                  glm_objectives,
                  overwrite = TRUE,
                  internal = TRUE)