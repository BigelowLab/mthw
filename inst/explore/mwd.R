suppressPackageStartupMessages({
  library(andreas)
  library(mthw)
  library(stars)
  library(dplyr)
  library(leaflet)
  library(leafem)
  library(terra)
  library(rnaturalearth)
  library(ggplot2)
})

region = 'chfc'
depths = "sur"
vars = c("temp", "sal")
andpath = andreas::copernicus_path(region)
mwpath = andreas::copernicus_path("mthw")

DB = mthw::read_database(file.path(mwpath, region)) |>
  dplyr::filter(.data$depth %in% depths, 
                .data$name %in% vars)
CDB = andreas::read_database(andpath, multi = TRUE) |>
  dplyr::filter(.data$depth %in% depths, 
                .data$name %in% c("temp", "sal"), 
                .data$period == "day")


tempe = mthw::generate_wave(
  db = DB |> dplyr::filter(name == "temp"),
  region = region,
  cDB = CDB)
sale = mthw::generate_wave(
  db = DB |> dplyr::filter(name == "sal"),
  region = region,
  cDB = CDB)

if(FALSE){
  x = tempe
  event_window = 5 
  gap_width = 2
  mask = TRUE
}


p = tribble(
    ~lon, ~lat,
    -68.926, 38.6593) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = st_crs(tempe))
tempd = mthw::encode_wave(tempe) 
sald = mthw::encode_wave(sale) 

coast = rnaturalearth::ne_coastline(returnclass = "sf") |>
  st_geometry() |>
  st_crop(tempd)



if (FALSE){
  dat = slice(tempd,"time", 1) 
  leaflet() |>
    addTiles() |>
    addStarsImage(dat, project = TRUE, method = "near") |>
    addPolylines(data = coast) |>
    addMouseCoordinates()
}

