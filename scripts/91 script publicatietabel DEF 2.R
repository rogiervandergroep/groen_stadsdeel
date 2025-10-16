library(tidyverse)


pub_tabel <- read_rds(
  "data/processed/result_list_publiek.rds"
)


### respons, open  vragen O7 en O8 en O9 hebben een andere structuur
pub_tabel_zonder_O7_O8_O9_resp <- pub_tabel |>
  map(\(x) x[names(x) != c("respons")]) |>
  map(\(x) x[names(x) != c("spec_groen_O7_O8_O9")]) |>
  map(\(x) x[names(x) != c("priv_groen_open")]) |>
  map(\(x) x[names(x) != c("gebruik_groen_open")]) |>
  map(\(x) x[names(x) != c("stadsbreed_sd_open")]) |>
  map(\(x) x[names(x) != c("groen_in_woon_open")]) |>
  map(\(x) x[names(x) != c("spec_groen_O22_open")])


### alleen respons en O7 O8 O9 en respons
pub_tabel_met_O7_O8_O9_resp <- pub_tabel |>
  map(\(x) {
    x[
      names(x) %in%
        c("spec_groen_O7_O8_O9", "respons")
    ]
  })

# open vragen selectie van kolommen
pub_tabel_open <- pub_tabel |>
  map(\(x) {
    x[
      names(x) %in%
        c(
          "priv_groen_open",
          "gebruik_groen_open",
          "stadsbreed_sd_open",
          "groen_in_woon_open",
          "spec_groen_O22_open"
        )
    ]
  })
# selectie van kolommen voor open vragen
kol_juist <- c(
  "name",
  "vraag",
  "spatial_name",
  "value"
)

#
pub_tabel_open_klein <- pub_tabel_open |>
  map(\(sublist) {
    map(
      sublist,
      \(df) {
        df |>
          filter(value != '') |>
          filter(spatial_type %in% c('ggw_gebieden', 'parken')) |>
          select(all_of(kol_juist))
      }
    )
  })

# vector met stadsdelen
stadsdelen <- names(pub_tabel)

# my_row_bind staat in 01 functies wide format
source("scr/01 functies wide format.R")

# maak een list met stadsdelen
final_table <- stadsdelen |>
  map(\(x) my_row_bind(x)) |>
  set_names(stadsdelen)


# voor 011 tm 21 bewaren we alleen de parken met n > 50
final_table_1121 <- final_table |>
  map(\(x) {
    x[
      names(x) %in%
        c(
          "spec_groen_O11_O21"
        )
    ]
  }) |>
  map(\(sublist) {
    map(
      sublist,
      \(df) {
        df |>
          select(1:4, where(~ .[2] != "-"))
      }
    )
  })

final_table_overig <- final_table |>
  map(\(x) {
    x[
      names(x) !=
        c(
          "spec_groen_O11_O21"
        )
    ]
  })


## toevoegen respons open vragen O7 O8 O9 aan final table
final_table <- pmap(
  list(
    final_table_overig,
    pub_tabel_met_O7_O8_O9_resp,
    final_table_1121,
    pub_tabel_open_klein
  ),
  \(w, x, y, z) c(w, x, y, z)
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
  "spec_groen_gebr",
  "spec_groen_O6",
  "spec_groen_O7_O8_O9",
  "spec_groen_O11_O21",
  "spec_groen_O22_open"
)


final_table <- names(final_table) |>
  map(\(x) final_table[[x]][tab_volgorde]) |>
  set_names(names(final_table))

# speciale opmaak voor spec_groen_O11_O21
# bewaar alleen de parken met N > 49

names(final_table) |>
  map(\(x) {
    openxlsx::write.xlsx(
      final_table[[x]],
      glue::glue("reports/tabel_v5_{ x }.xlsx"),
      withFilter = T
    )
  })
