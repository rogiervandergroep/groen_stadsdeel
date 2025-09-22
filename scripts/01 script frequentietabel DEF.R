# inlezen ruwe data en namen van variabelen
source("scripts/00 inlezen ruwe data vraag_codes.R")
source("scripts/00 inlezen ruwe data vraag_naam.R")

# inlezen functies en libraries
source("scr/01 functies summaries DEF.R")

### check variabelen
# names(vraag[['mr']])
# names(vraag[['sr']])

# voor het tabblad 'rol stedelijk groen in OR' met weegfactor
# de extensie geeft de weegfactor aan
# vraag 32 wordt op andere manier berekend

mr_vragen_onl <- c(
  "O1",
  "O2a",
  "O2b",
  "O2c",
  "O2d",
  "O2e",
  "O31",
  #"O32",
  "O41",
  "O47",
  "O48t"
)
mr_vragen_tot <- c("T4", "T26", "T34", "T46")
mr_vragen_geb <- c("O6") # i.e. weeg_gebied


# de achtergrondvragen leeftijd worden ook berekend obv
sr_vragen_onl <- c(
  "lft4cat",
  "geslacht",
  "oplcat",
  "inkcat",
  "mig8",
  "hh",
  "gebied",
  "O10_alt",
  "O24",
  "O29",
  "O38",
  "O39",
  "O40",
  "O42",
  "O43",
  "O48"
)

sr_open_vragen <- c(
  "O5",
  "O22",
  "O25",
  "O32_Other5",
  "O48_toelichting_nw_Other6",
  "O44",
  "O49_toelichting"
)

sr_vragen_tot <- c(
  "T30",
  "Tbomen",
  "T45",
  "T33",
  "T35",
  "T36",
  "T37"
)


# vragen per eerder genoemde markt v10 : allen weeg_ONLINE
sr_vragen_O10_alt <- c("O111", "O141", "O18", "O21")
mr_vragen_O10_alt <- c("O12", "O13", "O15", "O16", "O17", "O19")

freq_list <- list(
  mr_vragen_tot |>
    map(\(v) my_bind_rows(i = v, typevraag = 'mr', weeg = weeg_TOT)) |>
    set_names(mr_vragen_tot),

  mr_vragen_onl |>
    map(\(v) my_bind_rows(i = v, typevraag = 'mr', weeg = weeg_ONLINE)) |>
    set_names(mr_vragen_onl),

  mr_vragen_geb |>
    map(\(v) my_bind_rows(i = v, typevraag = 'mr', weeg = weeg_gebied)) |>
    set_names(mr_vragen_geb),

  sr_vragen_tot |>
    map(\(v) my_bind_rows(i = v, typevraag = 'sr', weeg = weeg_TOT)) |>
    set_names(sr_vragen_tot),

  sr_vragen_onl |>
    map(\(v) my_bind_rows(i = v, typevraag = 'sr', weeg = weeg_ONLINE)) |>
    set_names(sr_vragen_onl)
) |>
  list_flatten()

# 032 twee keuzen

# open vragen ---

freq_openvragen <- list(
  # naar stadsdelen
  openvragen_sd = sr_open_vragen |>
    map(\(v) {
      my_summarise(
        data_groen,
        geb_var = sd,
        i = vraag[['sr']][[v]],
        wf = weeg_ONLINE
      ) |>
        add_column(v, vraag = vraag_naam[['sr']][[v]])
    }) |>
    map(\(v) rename(v, spatial_name = sd)) |>
    map(\(v) add_column(v, spatial_type = 'stadsdelen')) |>
    set_names(sr_open_vragen),

  # naar gebied
  openvragen_geb = sr_open_vragen |>
    map(\(v) {
      my_summarise(
        data_groen,
        geb_var = geb,
        i = vraag[['sr']][[v]],
        wf = weeg_ONLINE
      ) |>
        add_column(v, vraag = vraag_naam[['sr']][[v]])
    }) |>
    map(\(v) rename(v, spatial_name = geb)) |>
    map(\(v) add_column(v, spatial_type = 'ggw_gebieden')) |>
    set_names(sr_open_vragen),

  # naar park
  openvragen_park = sr_open_vragen |>
    map(\(v) {
      my_summarise(
        data_groen,
        geb_var = O10_alt,
        i = vraag[['sr']][[v]],
        wf = weeg_ONLINE
      ) |>
        add_column(v, vraag = vraag_naam[['sr']][[v]])
    }) |>
    map(\(v) rename(v, spatial_name = O10_alt)) |>
    map(\(v) add_column(v, spatial_type = 'parken')) |>
    set_names(sr_open_vragen)
)

freq_list_park <- list(
  sr_vragen_O10_alt |>
    map(\(v) {
      my_summarise(
        data_groen,
        geb_var = O10_alt,
        i = vraag[['sr']][[v]],
        wf = weeg_ONLINE
      ) |>
        add_column(v, vraag = vraag_naam[['sr']][[v]])
    }) |>
    map(\(v) rename(v, spatial_name = O10_alt)) |>
    map(\(v) add_column(v, spatial_type = 'parken')) |>
    set_names(sr_vragen_O10_alt),

  mr_vragen_O10_alt |>
    map(\(v) {
      my_summarise(
        data_groen,
        geb_var = O10_alt,
        i = vraag[['mr']][[v]],
        wf = weeg_ONLINE
      ) |>
        add_column(v, vraag = vraag_naam[['mr']][[v]])
    }) |>
    map(\(v) rename(v, spatial_name = O10_alt)) |>
    map(\(v) add_column(v, spatial_type = 'parken')) |>
    set_names(mr_vragen_O10_alt)
) |>
  list_flatten()


# dit is een lijst met namen van parken die gekoppeld worden aan de vragen 07 tm 09 11?
park_labels <- data_groen |>
  select(all_of(contains("_rapportcijfer1"))) |>
  names() |>
  map_df(
    \(i) {
      tibble(
        name = i,
        labels = attr(data_groen[[i]], "label")
      )
    }
  ) |>
  mutate(
    park_naam = str_remove_all(name, "O9_"),
    park_naam = str_remove_all(park_naam, "_rapportcijfer1"),
    spatial_name = str_remove_all(labels, " : rapportcijfer") # nodig om later alleen parken binnen stadsdeel te selecteren
  )


# v7 waarom gaan mensen naar parken
freq_list_park[['O7']] <- parken |>
  map_df(\(p) my_bind_rows(i = p, typevraag = 'O7', weeg = weeg_ONLINE)) |>
  rename(park_naam = vraag) |>
  add_column(
    vraag = 'V7: Redenen van bezoek van parkachtig groengebieden in Noord'
  ) |>
  filter(spatial_type == 'gemeente') |>
  select(-c(spatial_name, spatial_type)) |>
  add_column(spatial_type = 'parken') |>
  left_join(park_labels[, c("park_naam", "spatial_name")], by = "park_naam")


# v8 hoe vaak komt u in park
freq_list_park[['O8']] <- parken |>
  map_df(\(p) my_bind_rows(i = p, typevraag = 'O8', weeg = weeg_ONLINE)) |>
  rename(park_naam = vraag) |>
  add_column(
    vraag = 'V8: Frequentie van bezoek van parkachtig groengebieden in Noord'
  ) |>
  filter(spatial_type == 'gemeente') |>
  select(-c(spatial_name, spatial_type)) |>
  add_column(spatial_type = 'parken') |>
  left_join(park_labels[, c("park_naam", "spatial_name")], by = "park_naam")


# v9 gemiddelde rapportcijfer per park
freq_list_park[['O9']] <- data_groen |>
  select(all_of(contains("_rapportcijfer1")), weeg_ONLINE) |>
  pivot_longer(cols = contains("_rapportcijfer1")) |>
  group_by(name) |>
  filter(!is.na(value)) |>
  summarise(
    aantal_ong = n(),
    aantal_gew = sum(weeg_ONLINE),
    gem_rapportcijfer = weighted.mean(value, weeg_ONLINE, na.rm = T)
  ) |>
  left_join(park_labels[, c("name", "spatial_name")], by = 'name') |>
  add_column(
    vraag = 'O9: Kunt u een rapportcijfer geven voor het park ...?',
    value = 'rapportcijfer',
    spatial_type = 'parken'
  )


# voeg de gewogen en ongewogen totale N toe
freq_list <- freq_list |>
  map(\(x) my_total_n(x))

freq_list_park <- freq_list_park |>
  map(\(x) my_total_n(x))


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
        "aandeel_gew",
        "gem_rapportcijfer"
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
  "O48"
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
