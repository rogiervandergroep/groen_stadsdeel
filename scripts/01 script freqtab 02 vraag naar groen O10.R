# dit zijn vragen die betrekking hebben op de eerder genoemde groengebieden

# vragen per eerder genoemde markt v10 : allen weeg_ONLINE
sr_vragen_O10_alt <- c("O111", "O141", "O18", "O21")
mr_vragen_O10_alt <- c("O12", "O13", "O15", "O16", "O17", "O19")

freq_list_park <- list(
  # single respons
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

  # multi respons
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
