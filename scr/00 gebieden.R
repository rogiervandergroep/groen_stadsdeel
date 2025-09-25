source(
  "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/OS_get_geoms.R"
)


gemeente <- c("Amsterdam")

stadsdelen <- c(
  "Centrum",
  "West",
  "Nieuw-West",
  "Zuid",
  "Oost",
  "Noord",
  "Weesp",
  "Zuidoost"
)

ggw_gebieden <- c(
  "Bijlmer-Centrum",
  "Bijlmer-Oost",
  "Bijlmer-West",
  "Bos en Lommer",
  "Buitenveldert, Zuidas",
  "Centrum-Oost",
  "Centrum-West",
  "De Aker, Sloten, Nieuw-Sloten",
  "De Pijp, Rivierenbuurt",
  "Gaasperdam",
  "Geuzenveld, Slotermeer",
  "IJburg, Zeeburgereiland",
  "Indische Buurt, Oostelijk Havengebied",
  "Noord-Oost",
  "Noord-West",
  "Osdorp",
  "Oud-Noord",
  "Oud-Oost",
  "Oud-West, De Baarsjes",
  "Oud-Zuid",
  "Sloterdijk Nieuw-West",
  "Slotervaart",
  "Watergraafsmeer",
  "Weesp, Driemond",
  "Westerpark"
)

geb <- list()

geb[['Centrum']] <- c(
  "Amsterdam",
  stadsdelen,
  "Centrum-Oost",
  "Centrum-West"
)

geb[['West']] <- c(
  "Amsterdam",
  stadsdelen,
  "Bos en Lommer",
  "Oud-West, De Baarsjes",
  "Westerpark"
)

geb[["Nieuw-West"]] <- c(
  "Amsterdam",
  stadsdelen,
  "Osdorp",
  "Sloterdijk Nieuw-West",
  "Slotervaart",
  "De Aker, Sloten, Nieuw-Sloten",
  "Geuzenveld, Slotermeer"
)

geb[['Zuid']] <- c(
  "Amsterdam",
  stadsdelen,
  "Oud-Zuid",
  "De Pijp, Rivierenbuurt",
  "Buitenveldert, Zuidas"
)

geb[['Oost']] <- c(
  "Amsterdam",
  stadsdelen,
  "Watergraafsmeer",
  "Oud-Oost",
  "IJburg, Zeeburgereiland",
  "Indische Buurt, Oostelijk Havengebied"
)

geb[['Noord']] <- c(
  "Amsterdam",
  stadsdelen,
  "Oud-Noord",
  "Noord-Oost",
  "Noord-West"
)

geb[['Weesp']] <- c(
  "Amsterdam",
  stadsdelen,
  "Weesp, Driemond"
)

geb[['Zuidoost']] <- c(
  "Amsterdam",
  stadsdelen,
  "Bijlmer-Centrum",
  "Bijlmer-Oost",
  "Bijlmer-West",
  "Gaasperdam"
)


### geb_kort gebruiken voor achtergrondvragen
geb_kort <- list()

geb_kort[['Centrum']] <- c(
  "Amsterdam",
  "Centrum",
  "Centrum-Oost",
  "Centrum-West"
)

geb_kort[['West']] <- c(
  "Amsterdam",
  "West",
  "Bos en Lommer",
  "Oud-West, De Baarsjes",
  "Westerpark"
)

geb_kort[["Nieuw-West"]] <- c(
  "Amsterdam",
  "Nieuw-West",
  "Osdorp",
  "Sloterdijk Nieuw-West",
  "Slotervaart",
  "De Aker, Sloten, Nieuw-Sloten",
  "Geuzenveld, Slotermeer"
)

geb_kort[['Zuid']] <- c(
  "Amsterdam",
  "Zuid",
  "Oud-Zuid",
  "De Pijp, Rivierenbuurt",
  "Buitenveldert, Zuidas"
)

geb_kort[['Oost']] <- c(
  "Amsterdam",
  "Oost",
  "Watergraafsmeer",
  "Oud-Oost",
  "IJburg, Zeeburgereiland",
  "Indische Buurt, Oostelijk Havengebied"
)

geb_kort[['Noord']] <- c(
  "Amsterdam",
  "Noord",
  "Oud-Noord",
  "Noord-Oost",
  "Noord-West"
)

geb_kort[['Weesp']] <- c(
  "Amsterdam",
  "Weesp",
  "Weesp, Driemond"
)

geb_kort[['Zuidoost']] <- c(
  "Amsterdam",
  "Zuidoost",
  "Bijlmer-Centrum",
  "Bijlmer-Oost",
  "Bijlmer-West",
  "Gaasperdam"
)


parken <- c(
  'Vondelpark',
  'Amsterdamse Bos',
  'Oosterpark',
  'Westerpark',
  'Rembrandtpark',
  'Museumplein',
  'Sloterplas/Sloterpark',
  'Flevopark',
  'Beatrixpark',
  'Sarphatipark',
  'Amstelpark',
  'Park Frankendael',
  'Noorderpark',
  'Oeverlanden/Nieuwe Meer',
  'Erasmuspark',
  'W.H. Vliegenbos',
  'Diemerpark/Diemerzeedijk',
  'De Bretten',
  'Gaasperplas/Gaasperpark/Gaasperzoom',
  'Martin Luther Kingpark',
  'Park Overhoeks/IJ-oever',
  'Diemerbos',
  'Rondom Sint Barbara',
  'Nelson Mandelapark/Bijlmerpark',
  'Volgermeerpolder',
  'Wertheimpark',
  'Gijsbrecht van Aemstelpark',
  'Bijlmerweide',
  'Bilderdijkpark',
  'Park Somerlust',
  'Baanakkerspark',
  'Park Schinkeleilanden/Schinkelpark',
  't Kleine Loopveld',
  'Buikslotermeerpark',
  'Hoge Dijk',
  'Darwinplantsoen/Prins Bernardpark',
  'Stadspark Osdorp',
  'Tuinen van West/Osdorper Binnenpolder',
  'Gerbrandypark',
  'Noorder-IJplas',
  'Geuzenbos',
  'Piet Wiedijkpark',
  'Eendrachtspark',
  'Theo van Goghpark',
  'Siegerpark',
  'Brasapark',
  'Kasterleepark',
  'Vrije Geer',
  'Keerkringpark',
  'Houtrakpolder'
)


library(tidyverse)
library(sf)


ggw_gebieden <- os_get_geom("stadsdelen")


# Let's read the downloaded geoJson file with the sf library:
library(sf)
parken_geb <- read_sf("references/groengebieden_ggo_wgs84.geojson") |>
  st_join(ggw_gebieden, left = T) |>
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
