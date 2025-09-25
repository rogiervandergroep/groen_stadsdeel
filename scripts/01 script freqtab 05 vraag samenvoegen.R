## inlezen freq tabellen

source("scripts/01 script freqtab 01 vraag basis.R") # in deze tabel worden ook de functies ingelezen
source("scripts/01 script freqtab 02 vraag naar groen O10.R")
source("scripts/01 script freqtab 03 vraag 07 08 09.R")
source("scripts/01 script freqtab 04 vraag open.R")


# voeg de gewogen en ongewogen totale N toe
freq_list <- freq_list |>
  map(\(x) my_total_n(x))

freq_list_park <- freq_list_park |>
  map(\(x) my_total_n(x))


# NB: bij parken is de totale n eerder toegevoegd in script 7
# overschrijf de oude tabel van 32 met de juiste versie
# totale N is al toegevoegd aan vraag 32

freq_list[['O32']] <- O32


my_clean <- function(x) {
  x |>
    mutate(value_clean = str_replace_all(value, "[[:punct:]]", " "))
}
### nette volgorde voor publicatiedoeleinden
### verwijderen van interpunctie bij value
freq_list_slim <- freq_list |>
  map(\(x) {
    select(
      x,
      any_of(c(
        "vraag",
        "park_naam",
        "name",
        "labels",
        "spatial_type",
        "spatial_name",
        "value",
        "totaal_ong",
        "totaal_gew",
        "aandeel_gew",
        "gem_rapportcijfer"
      ))
    )
  }) |>
  map(\(x) my_clean(x))

# idem voor parken
freq_list_park_slim <- freq_list_park |>
  map(\(x) {
    select(
      x,
      any_of(c(
        "vraag",
        "park_naam",
        "name",
        "labels",
        "spatial_type",
        "spatial_name",
        "value",
        "totaal_ong",
        "totaal_gew",
        "aandeel_gew"
      ))
    )
  }) |>
  map(\(x) my_clean(x))


### opsplitsen freq_list zonder achtergrondvragen en achtergrondvragen appart

vragen_onl_tot <- c(
  "T4",
  "T26",
  "T34",
  "T46",
  "O1",
  "O2a",
  "O2b",
  "O2c",
  "O2d",
  "O2e",
  "O31",
  "O32",
  "O41",
  "O47",
  "O48t",
  "O6",
  "T30",
  "Tbomen",
  "T45",
  "T33",
  "T35",
  "T36",
  "T37",
  "O24",
  "O29",
  "O38",
  "O39",
  "O40",
  "O42",
  "O43",
  "O48",
  "O49_Codes"
)

vragen_achtergr <- c(
  "O10_alt",
  "geslacht",
  "oplcat",
  "inkcat",
  "mig8",
  "hh",
  "gebied"
)


# opsplitsen met en zonder achtergrondvragen
freq_list_za <- freq_list[vragen_onl_tot] # zonder achtegrondvragen
freq_list_av <- freq_list[vragen_achtergr] # alleen achtegrondvragen

freq_list_slim_za <- freq_list_slim[vragen_onl_tot] # zonder achtegrondvragen
freq_list_slim_av <- freq_list_slim[vragen_achtergr] # alleen achtegrondvragen

#### om zetten respons naar wide format

# functie voor omzetting naar wide format data
my_end_table_w <- function(x) {
  x |>
    ungroup() |>
    #filter(value != 'No') |>
    #filter(totaal_gew > 49) |>
    select(any_of(c(
      "vraag",
      "labels",
      "value",
      "aandeel_gew",
      "spatial_name"
    ))) |>
    mutate(aandeel_gew = str_glue("{round(aandeel_gew*100)}%")) |>
    pivot_wider(
      names_from = c(spatial_name),
      values_from = c(aandeel_gew)
      #values_fill = "0%"
    )
}

# functie om totale n naar wide format om te zetten
my_end_table_n <- function(x) {
  x |>
    ungroup() |>
    filter(totaal_gew > 49) |>
    select(vraag, spatial_name, totaal_gew) |>

    mutate(totaal_gew = as.character(round(totaal_gew))) |>
    distinct(vraag, spatial_name, .keep_all = T) |>
    pivot_wider(
      names_from = c(spatial_name),
      values_from = c(totaal_gew)
      #values_fill = "-"
    ) |>
    add_column(value = "(n)", labels = "totaal aantal respondenten")
  #  mutate(x, name = str_remove(vraag, ":.*$")) |>
  #  mutate(x, name = str_sub(name, 2))|>
  #  mutate(x, name = str_glue("0{name}")
}

#
freq_list_slim_av <- list(
  geslacht = bind_rows(
    freq_list_slim_av[["geslacht"]] |>
      my_end_table_n(),
    freq_list_slim_av[["geslacht"]] |>
      my_end_table_w()
  ),

  oplcat = bind_rows(
    freq_list_slim_av[["oplcat"]] |>
      my_end_table_n(),
    freq_list_slim_av[["oplcat"]] |>
      my_end_table_w()
  ),

  inkcat = bind_rows(
    freq_list_slim_av[["inkcat"]] |>
      my_end_table_n(),
    freq_list_slim_av[["inkcat"]] |>
      my_end_table_w()
  ),

  mig8 = bind_rows(
    freq_list_slim_av[["mig8"]] |>
      my_end_table_n(),
    freq_list_slim_av[["mig8"]] |>
      my_end_table_w()
  ),

  hh = bind_rows(
    freq_list_slim_av[["hh"]] |>
      my_end_table_n(),
    freq_list_slim_av[["hh"]] |>
      my_end_table_w()
  ),

  gebied = bind_rows(
    freq_list_slim_av[["gebied"]] |>
      my_end_table_n(),
    freq_list_slim_av[["gebied"]] |>
      my_end_table_w()
  )
)
