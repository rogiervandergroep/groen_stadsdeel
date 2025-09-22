library(tidyverse)
library(sf)

source(
  "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/OS_get_geoms.R"
)


ggw_gebieden <- os_get_geom("stadsdelen")


# Let's read the downloaded geoJson file with the sf library:
library(sf)
parken_sf <- read_sf("references/groengebieden_ggo_wgs84.geojson")

parken_geb <- st_join(parken_sf, ggw_gebieden, left = T) |>
  st_drop_geometry() |>
  ungroup() |>
  select("gebied_naam", "naam", "code") |>
  set_names(c(
    "parknaam",
    "stadsdeel_name",
    "stadsdeel_code"
  ))


parken_sd <- stadsdelen |>
  map(\(x) filter(parken_geb, stadsdeel_name == x)) |>
  map(\(x) select(x, parknaam)) |>
  map(\(x) pull(x)) |>
  set_names(stadsdelen)
