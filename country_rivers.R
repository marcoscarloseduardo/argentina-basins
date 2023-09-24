# Ríos asociados por cuenca de un país

# Medimos el tiempo que lleva ejecutar el script
start_time <- Sys.time()



# 0. Creamos estructura de archivos y cargamos librerías necesarias
#------------------------------------------------------------------

libs <- c("tidyverse", "sf", "giscoR", "RColorBrewer")    # librerías a utilizar

installed_libraries <- libs %in% 
  rownames(installed.packages())

if (any(installed_libraries == F)) {install.packages(libs[!installed_libraries])}
invisible(lapply(libs, library, character.only = T))

if (!file.exists("data")) {dir.create("data")}            # contiene archivos de datos entrantes o salientes

if (!file.exists("data/geo")) {dir.create("data/geo")}    # contiene archivos importados con datos de geolocalización

if (!file.exists("plots")) {dir.create("plots")}          # contiene gráficos generados por el script


# 1. Obtenemos información geográfica del país
#---------------------------------------------

country_code <- "AR"                                       # código país seleccionado
region_code <- "sa"                                        # código región seleccionada
level_code <- "03"                                         # código nivel resolución seleccionado

path_geo <- "data/geo"

get_country_borders <- function() {
  country_borders <- giscoR::gisco_get_countries(
    resolution = level_code,                               # nivel de resolución de los datos geoespaciales "03": 1:3million
    country = country_code                                       
  )
  return(country_borders)
}

country_borders <- get_country_borders()



# 2. Obtener información sobre las cuencas de los ríos
#-----------------------------------------------------

get_basins <- function() {
  url <- paste0("https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_",region_code,"_lev",level_code,"_v1c.zip")
  file_name <- paste0("hybas_",region_code,"_lev",level_code,"_v1c")
  dest_file <- file.path(path_geo, paste0(file_name, ".zip"))
  
  download.file(
    url = url,
    destfile = dest_file,
    mode = "wb"                                           # archivo binario
  )
  
  unzip(zipfile = dest_file, exdir = path_geo)
  
  basins <- sf::st_read(file.path(path_geo, paste0(file_name, ".shp")))
}

country_basin <- get_basins()

sf::sf_use_s2(F)                                          # debido a limitaciones de compatibilidad son sf

country_basin <- country_basin |>                         # eliminamos todo lo que esté fuera del país objetivo
  sf::st_intersection(country_borders) |>
  dplyr::select(HYBAS_ID)



# 3. Obtener información sobre los ríos
#--------------------------------------

get_rivers <- function() {
  url <- paste0("https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_",region_code,"_shp.zip")
  file_name <- paste0(region_code,"-rivers")
  dest_file <- file.path(path_geo, paste0(file_name, ".zip"))
  
  download.file(
    url = url,
    destfile = dest_file,
    mode = "wb"
  )
  
  unzip(zipfile = dest_file, exdir = path_geo)
  
  rivers <- sf::st_read(file.path(path_geo, paste0("HydroRIVERS_v10_",region_code,"_shp/HydroRIVERS_V10_",region_code,".shp")))
}

country_rivers <- get_rivers()

country_rivers <- country_rivers |>                   # eliminamos todo lo que esté fuera del país objetivo
  dplyr::select(ORD_FLOW) |>
  sf::st_intersection(country_borders)



# 4. Asociar cuencas con los ríos
#-----------------------------------

country_river_basin <- sf::st_intersection(
  country_rivers,
  country_basin
)



# 5. Definir ancho con que se representarán los ríos
#---------------------------------------------------

country_river_basin_width <- country_river_basin |>
  dplyr::mutate(width = as.numeric(ORD_FLOW),         # Indicator of river order using river flow to distinguish logarithmic size classes: 
    width = dplyr::case_when(                         
      width ==  1 ~ .90,                              # order 1 represents river reaches with a long-term average discharge ≥ 100,000 m3/s
      width ==  2 ~ .80,                              # order 2 represents river reaches with a long-term average discharge ≥  10,000 m3/s
      width ==  3 ~ .70,
      width ==  4 ~ .60,
      width ==  5 ~ .45,
      width ==  6 ~ .35,
      width ==  7 ~ .25,
      width ==  8 ~ .10,
      width ==  9 ~ .05,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf()



# 6. Gráfico
#-----------

color_n <- unique(country_river_basin_width$HYBAS_ID) %>%             # número de cuencas
  length()

p <- ggplot() +
  geom_sf(
    data = country_river_basin_width,
    aes(
      color = factor(HYBAS_ID),
      size = width,
      alpha = width
    )
  ) +
  geom_sf(data = country_borders, aes(alpha = 0.1)) +
  scale_color_manual(
    name = "",
    values = brewer.pal(n = color_n, name = 'Set3')
  ) +
  scale_size(range = c(.1, .7)) +
  scale_alpha(range = c(.01, .7)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 9, color = "grey90", hjust = 0.5, family = "Optima", face = "bold"),
    plot.caption = element_text(size = 6, color = "grey60",hjust = .1, vjust = 10),
    plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA)
  ) +
  labs(
    title = "",
    x = "",
    y = "",
    caption = "Database: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS"
  )

ggsave(
  filename = file.path("plots", paste0(tolower(country_code),"-river-basins.png")),
  width = 2.5, height = 5.9, dpi = 600,
  bg = "black", device = "png", p
)

st_write(country_river_basin_width, paste0(country_code,"-river-basins.geojson"), layer = NULL, driver = "GeoJson")

end_time <- Sys.time()
total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
print(paste("Total time:", total_time, "segundos"))