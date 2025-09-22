### publicatie tabel ---
library(tidyverse)
source("scr/00 gebieden.R")

pub_tabel <- read_rds(
  "data/processed/result_list.rds"
)

## filter waarbij n < 50 per vraag verdwijnt

## data blad 1: rol stedelijk groen in openbar3e ruimte
## v1, v2

my_bind_rows <- function(x) {
  bind_rows(
    pub_tabel[["ams"]][[x]],
    pub_tabel[["sd"]][[x]],
    pub_tabel[["geb"]][[x]],
    pub_tabel[["park"]][[x]],
    pub_tabel[["park_O7_O8_O9"]][[x]],
    pub_tabel[["achtergrond"]][[x]],
  )
}

excel_list = list()

#data blad 1: rol stedelijk groen

excel_list[["respons"]] <- c(
  "lft4cat",
  "geslacht",
  "oplcat",
  "inkcat",
  "mig8",
  "hh",
  "gebied",
  "O10_alt"
) |>
  map_df(\(v) my_bind_rows(v))


excel_list[["rol_sted_groen"]] <- c("O1", "O2a", "O2b", "O2c", "O2d", "O2e") |>
  map_df(\(v) my_bind_rows(v))

#data blad 2: gebruik groen naar type
## v4, v5 en v25
## nb: v5 en v25: open vragen
excel_list[["gebruik_groen"]] <- c("T4") |>
  map_df(\(v) my_bind_rows(v))

excel_list[["gebruik_groen_open"]] <- c("O5", "O25") |>
  map_df(\(v) my_bind_rows(v))

## data blad 3: stadsdbrede resultaten naar sd
## v24, v26 v45 v46 v47 v48 v48t v49
## T49 is open vraag
excel_list[["stadsbreed_sd"]] <- c(
  "O24",
  "T26",
  "T45",
  "T46",
  "O47",
  "O48",
  "O48t"
) |>
  map_df(\(v) my_bind_rows(v))


excel_list[["stadsbreed_sd_open"]] <- c(
  "O48_toelichting_nw_Other6",
  "O49_toelichting"
) |>
  map_df(\(v) my_bind_rows(v))

#data blad 4: bezoek specifieke groengebieden
## v6 v7 v8 v9 v11 v12 v13 v14 v15 v16 v17 v18 v21 v22

excel_list[["spec_groen_O6"]] <- c(
  "O6"
) |>
  map_df(\(v) my_bind_rows(v))

# deze cijfers zijn niet naar stadsdeel maar naar park gevraagd
excel_list[["spec_groen_O7_O8"]] <- c(
  "O7",
  "O8"
) |>
  map_df(\(v) my_bind_rows(v))

# rapportcijfers in een apart tabblad
excel_list[["spec_groen_O9"]] <- pub_tabel[["park_O7_O8_O9"]][["O9"]]


# deze cijfers zijn niet naar stadsdeel maar naar park gevraagd
excel_list[["spec_groen_O11_21"]] <- c(
  "O111",
  "O12",
  "O13",
  "O141",
  "O15",
  "O16",
  "O17",
  "O18",
  "O19",
  "O21"
) |>
  map_df(\(v) my_bind_rows(v))


# basis_list5: groen in de woonomgeving
# v28 v29 v30 tbomen v31 v32 v33
# v28 is een kaartje
excel_list[["groen_in_woon"]] <- c(
  "O29",
  "T30",
  "Tbomen",
  "O31",
  "O32",
  "T33"
) |>
  map_df(\(v) my_bind_rows(v))

excel_list[["groen_in_woon_open"]] <- c("O32_Other5") |>
  map_df(\(v) my_bind_rows(v))


# basislist6: privaatgroen en medebeheer
# v34 en v35 v36 v37 v39  v40 v41 v42 v43 v44
excel_list[["priv_groen"]] <- c(
  "T34",
  "T35",
  "T36",
  "T37",
  "O38",
  "O39",
  "O40",
  "O41",
  "O42",
  "O43",
  "T45",
  "T46",
  "O47",
  "O48",
  "O48t"
) |>
  map_df(\(v) my_bind_rows(v))

excel_list[["priv_groen_open"]] <- c("O44") |>
  map_df(\(v) my_bind_rows(v))


# hierbij worden de juiste gebieden, stadsdelen aan juiste stadsdeellist toegekend
my_stadsdeel_split_av <- function(sheet, gebied) {
  excel_list[[sheet]] |>
    filter(spatial_name %in% gebied)
}


# hier wordt een list gemaakt met stadsdeel als eerste dimensie
eindlist <- stadsdelen |>
  map(\(x) {
    #x = "Centrum"

    list(
      # alle stadsdelen
      rol_sted_groen = my_stadsdeel_split_av("rol_sted_groen", geb[[x]]),
      gebruik_groen = my_stadsdeel_split_av("gebruik_groen", geb[[x]]),
      stadsbreed_sd = my_stadsdeel_split_av("stadsbreed_sd", geb[[x]]),
      groen_in_woon = my_stadsdeel_split_av("groen_in_woon", geb[[x]]),
      priv_groen = my_stadsdeel_split_av("priv_groen", geb[[x]]),

      # niet alle stadsdelen
      respons = my_stadsdeel_split_av("respons", geb_kort[[x]]),
      gebruik_groen_open = my_stadsdeel_split_av(
        "gebruik_groen_open",
        geb_kort[[x]]
      ),
      stadsbreed_sd_open = my_stadsdeel_split_av(
        "stadsbreed_sd_open",
        geb_kort[[x]]
      ),
      groen_in_woon_open = my_stadsdeel_split_av(
        "groen_in_woon_open",
        geb_kort[[x]]
      ),
      priv_groen_open = my_stadsdeel_split_av("priv_groen_open", geb_kort[[x]]),

      # alles
      spec_groen_O6 = excel_list[["spec_groen_O6"]],

      # allen parken van het stadsdeel
      spec_groen_O7_O8 = my_stadsdeel_split_av(
        "spec_groen_O7_O8",
        parken_sd[[x]]
      ),
      spec_groen_O9 = my_stadsdeel_split_av("spec_groen_O9", parken_sd[[x]]),
      spec_groen_O11 = my_stadsdeel_split_av(
        "spec_groen_O11_21",
        parken_sd[[x]]
      )
    )
  }) |>

  set_names(stadsdelen)


names(eindlist) |>
  map(\(x) {
    openxlsx::write.xlsx(
      eindlist[[x]],
      glue::glue("reports/tabel_v4_{ x }.xlsx"),
      withFilter = T
    )
  })

write_rds(eindlist, "data/processed/result_list_publiek.rds")
