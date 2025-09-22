# toevoegen open vragen ---

sr_open_vragen <- c(
  "O5",
  "O22",
  "O25",
  "O32_Other5",
  "O48_toelichting_nw_Other6",
  "O44",
  "O49_toelichting"
)

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
