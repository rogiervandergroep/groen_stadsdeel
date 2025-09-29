# functie voor omzetting naar wide format data
my_end_table_w <- function(x) {
  # selecteer de relevant kolommen
  y <- x |>
    ungroup() |>
    filter(totaal_gew > 49) |>
    select(any_of(c(
      "vraag",
      "labels",
      "value",
      "aandeel_gew",
      "gem_rapportcijfer",
      "spatial_name"
    )))

  colnames <- names(y)

  if ("aandeel_gew" %in% colnames) {
    y |>
      filter(value != 'No') |>

      mutate(
        aandeel_gew = str_glue("{round(aandeel_gew * 100)}%")
      ) |>
      pivot_wider(
        names_from = c(spatial_name),
        values_from = c(aandeel_gew)
      )
  } else if ("gem_rapportcijfer" %in% colnames) {
    y |>
      mutate(
        gem_rapportcijfer = as.character(round(gem_rapportcijfer, 2))
      ) |>
      pivot_wider(
        names_from = c(spatial_name),
        values_from = c(gem_rapportcijfer)
      )
  }
}

# # functie om totale n naar wide format om te zetten
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


# tabel met data en sign*
my_end_table_w_sig <- function(x) {
  x |>

    map(\(x) ungroup(x)) |>
    map(\(x) filter(x, value != 'No')) |>
    map(\(x) {
      mutate(
        x,
        waarde = case_when(
          totaal_gew < 50 ~ "-",
          sig_verschil == 'significant hoger dan stedelijk gemiddelde' ~
            glue::glue("{ round(aandeel_gew*100) }%\u207A"),
          sig_verschil == 'significant lager dan stedelijk gemiddelde' ~
            glue::glue("{ round(aandeel_gew*100) }%\u207B"),
          TRUE ~ glue::glue("{ round(aandeel_gew*100) }%")
        )
      )
    }) |>
    map(\(x) {
      select(
        x,
        -(c(
          'spatial_type',
          'stdres',
          'totaal_ong',
          'totaal_gew',
          'aandeel_gew',
          'sig_verschil'
        ))
      )
    }) |>
    map(\(x) {
      pivot_wider(
        x,
        names_from = c(spatial_name),
        values_from = c(waarde),
        values_fill = "-"
      )
    }) |>
    map(\(x) mutate(x, name = str_sub(name, 2)))
}

# tabel met totale respons per vraag en spatial_name
my_end_table_n_sig <- function(x) {
  x |>
    map(\(x) ungroup(x)) |>
    map(\(x) select(x, vraag, spatial_name, totaal_gew)) |>

    map(\(x) mutate(x, totaal_gew = as.character(round(totaal_gew)))) |>
    map(\(x) distinct(x, vraag, spatial_name, .keep_all = T)) |>
    map(\(x) {
      pivot_wider(
        x,
        names_from = c(spatial_name),
        values_from = c(totaal_gew),
        values_fill = "-"
      )
    }) |>
    map(\(x) {
      add_column(
        x,
        value = "(n)",
        labels = "totaal aantal respondenten"
      )
    }) |>
    map(\(x) mutate(x, name = str_remove(vraag, ":.*$"))) |>
    map(\(x) mutate(x, name = str_sub(name, 2))) |>
    map(\(x) mutate(x, name = str_glue("0{name}")))
}

### functie om rij met totale n te plakken op tabel met data
my_row_bind <- function(x) {
  map2(
    pub_tabel_zonder_O7_O8_O9_resp[[x]] |>
      my_end_table_w_sig(),

    pub_tabel_zonder_O7_O8_O9_resp[[x]] |>
      my_end_table_n_sig(),

    bind_rows
  ) |>
    map(\(x) arrange(x, vraag, name))
}
