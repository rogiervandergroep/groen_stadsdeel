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


# aparte dataframes voor v7 v8 en v9

# v7 waarom gaan mensen naar parken
freq_list_park_O7 <- parken |>
  map_df(\(p) my_bind_rows(i = p, typevraag = 'O7', weeg = weeg_ONLINE)) |>
  rename(park_naam = vraag) |>
  add_column(
    vraag = 'V7: Redenen van bezoek van parkachtig groengebieden in Noord'
  ) |>
  filter(spatial_type == 'gemeente') |>
  select(-c(spatial_name, spatial_type)) |>
  add_column(spatial_type = 'parken') |>
  left_join(park_labels[, c("park_naam", "spatial_name")], by = "park_naam") |>
  my_total_n()


# v8 hoe vaak komt u in park
freq_list_park_O8 <- parken |>
  map_df(\(p) my_bind_rows(i = p, typevraag = 'O8', weeg = weeg_ONLINE)) |>
  rename(park_naam = vraag) |>
  add_column(
    vraag = 'V8: Frequentie van bezoek van parkachtig groengebieden in Noord'
  ) |>
  filter(spatial_type == 'gemeente') |>
  select(-c(spatial_name, spatial_type)) |>
  add_column(spatial_type = 'parken') |>
  left_join(park_labels[, c("park_naam", "spatial_name")], by = "park_naam") |>
  my_total_n()


# v9 gemiddelde rapportcijfer per park
freq_list_park_O9 <- data_groen |>
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
  ) |>
  my_total_n()

# functie voor omzetting naar wide format data
my_end_table_w <- function(x) {
  x |>
    ungroup() |>
    filter(value != 'No') |>
    filter(totaal_gew > 49) |>
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


freq_park_789_wide <- bind_rows(
  # vraag7
  freq_list_park_O7 |>
    my_end_table_n(),
  freq_list_park_O7 |>
    my_end_table_w(),

  # vraag 8
  freq_list_park_O8 |>
    my_end_table_n(),

  freq_list_park_O8 |>
    select(-labels) |>
    my_end_table_w() |>
    mutate(labels = str_remove_all(vraag, "V8: ")),

  # vraag 9
  freq_list_park_O9 |>
    my_end_table_n(),

  freq_list_park_O9 |>
    rename(aandeel_gew = gem_rapportcijfer) |>
    my_end_table_w()
)
