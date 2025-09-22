library(tidyverse)


pub_tabel <- read_rds(
  "data/processed/result_list_publiek.rds"
)

# tabel met data en sign*
my_end_table_w <- function(x) {
  x |>

    map(\(x) ungroup(x)) |>
    map(\(x) filter(x, value != 'No')) |>
    map(\(x) {
      mutate(
        x,
        waarde = case_when(
          totaal_gew < 50 ~ "-",
          sig_verschil %in%
            c(
              'significant hoger dan stedelijk gemiddelde',
              'significant lager dan stedelijk gemiddelde'
            ) ~
            glue::glue("{ round(aandeel_gew*100) }%*"),

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
        values_fill = "0%"
      )
    }) |>
    map(\(x) mutate(x, name = str_sub(name, 2)))
}

# tabel met totale respons per vraag en spatial_name
my_end_table_n <- function(x) {
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

### respons, open  vragen O7 en O8 en O9 hebben een andere structuur
pub_tabel_zonder_O9 <- pub_tabel |>
  map(\(x) x[names(x) != c("respons")]) |>
  map(\(x) x[names(x) != c("spec_groen_O7_O8")]) |>
  map(\(x) x[names(x) != c("spec_groen_O9")]) |>
  map(\(x) x[names(x) != c("priv_groen_open")]) |>
  map(\(x) x[names(x) != c("gebruik_groen_open")]) |>
  map(\(x) x[names(x) != c("stadsbreed_sd_open")]) |>
  map(\(x) x[names(x) != c("groen_in_woon_open")])

### alleen respons en O9 en open vragen
pub_tabel_alleen_O9 <- pub_tabel |>
  map(\(x) {
    x[
      names(x) %in%
        c(
          "spec_groen_O7_O8",
          "respons",
          "spec_groen_O9"
        )
    ]
  })


pub_tabel_open <- pub_tabel |>
  map(\(x) {
    x[
      names(x) %in%
        c(
          "priv_groen_open",
          "gebruik_groen_open",
          "stadsbreed_sd_open",
          "groen_in_woon_open"
        )
    ]
  })

kol_juist <- c(
  "name",
  "vraag",
  "spatial_name",
  "value"
)

pub_tabel_open_klein <- pub_tabel_open |>
  map(\(sublist) {
    map(
      sublist,
      \(df) {
        df |>
          filter(value != '') |>
          filter(spatial_type == 'ggw_gebieden') |>
          select(kol_juist)
      }
    )
  })


my_row_bind <- function(x) {
  map2(
    pub_tabel_zonder_O9[[x]] |>
      my_end_table_w(),

    pub_tabel_zonder_O9[[x]] |>
      my_end_table_n(),

    bind_rows
  ) |>
    map(\(x) arrange(x, vraag, name))
}

stadsdelen <- names(pub_tabel)


final_table <- list()


final_table <- stadsdelen |>
  map(\(x) my_row_bind(x)) |>
  set_names(stadsdelen)

## toevoegen respons open vragen O7 O8 O9 aan final table
final_table <- pmap(
  list(final_table, pub_tabel_alleen_O9, pub_tabel_open_klein),
  \(x, y, z) c(x, y, z)
)


# volgorde ondeliggende items goed zetten

tab_volgorde <- c(
  "respons",
  "rol_sted_groen",
  "gebruik_groen",
  "gebruik_groen_open",
  "stadsbreed_sd",
  "stadsbreed_sd_open",
  "groen_in_woon",
  "groen_in_woon_open",
  "priv_groen",
  "priv_groen_open",
  "spec_groen_O7_O8",
  "spec_groen_O9",
  "spec_groen_O11"
)


final_table2 <- names(final_table) |>
  map(\(x) final_table[[x]][tab_volgorde]) |>
  set_names(names(final_table))


names(final_table2) |>
  map(\(x) {
    openxlsx::write.xlsx(
      final_table[[x]],
      glue::glue("reports/tabel_v5_{ x }.xlsx"),
      withFilter = T
    )
  })
