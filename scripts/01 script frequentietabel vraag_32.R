### 032 ---

# bestaat uit O321  O322

source("scripts/01 script frequentietabel DEF.R")


my_sum_32_function <- function(var_32, suf) {
  tabel <- bind_rows(
    data_groen |>
      group_by(O32 = {{ var_32 }}) |> # totaal amsterdam
      summarise(
        "aantal_ong_{suf}" := n(),
        "aantal_gew_{suf}" := sum(weeg_ONLINE, na.rm = T)
      ) |>
      add_column(
        spatial_type = 'gemeente',
        spatial_name = 'Amsterdam'
      ),

    data_groen |>
      group_by(O32 = {{ var_32 }}, spatial_name = sd) |> # per stadsdeel
      summarise(
        "aantal_ong_{suf}" := n(),
        "aantal_gew_{suf}" := sum(weeg_ONLINE, na.rm = T)
      ) |>
      add_column(
        spatial_type = 'stadsdelen'
      ),

    data_groen |>
      group_by(O32 = {{ var_32 }}, spatial_name = geb) |> # per gebied
      summarise(
        "aantal_ong_{suf}" := n(),
        "aantal_gew_{suf}" := sum(weeg_ONLINE, na.rm = T)
      ) |>
      add_column(
        spatial_type = 'gebieden'
      )
  )
}

a <- my_sum_32_function(O321, "O321")
b <- my_sum_32_function(O321, "O322")

##### totaal ---

O32 <- a |>
  left_join(b, by = c("O32", "spatial_name", "spatial_type")) |>
  filter(!is.na(O32)) |>
  mutate(
    across(c(aantal_ong_O322, aantal_gew_O322), ~ replace_na(.x, 0))
  ) |>
  mutate(
    aantal_ong = aantal_ong_O321 + aantal_ong_O322,
    aantal_gew = aantal_gew_O321 + aantal_gew_O322
  ) |>
  group_by(spatial_name, spatial_type) |>
  mutate(
    totaal_ong = sum(aantal_ong_O321),
    totaal_gew = sum(aantal_gew_O321),
    aandeel_ong = aantal_ong / sum(aantal_ong_O321),
    aandeel_gew = aantal_gew / sum(aantal_gew_O321)
  ) |>
  select(
    O32,
    spatial_name,
    spatial_type,
    totaal_ong,
    totaal_gew,
    aandeel_ong,
    aandeel_gew
  ) |>
  rename(value = O32) |>
  add_column(
    vraag = 'V32: Wat is volgens u het belangrijkst bij het verbeteren van het groen in uw woonomgeving?',
    name = 'O32',
    label = 'Wat is volgens u het belangrijkst bij het verbeteren van het groen in uw woonomgeving?'
  )
