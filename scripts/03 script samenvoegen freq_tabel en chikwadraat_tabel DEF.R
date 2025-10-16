# met dit schript worden de frequentietabellen en de chi-tabellen samengevoegd

# eerst worden met dit script de frequentietabellen en de chi-tabellen ingelezen
source("scripts/02 script chikwadraattabel DEF.R")

### tweede stap inlezen functie toets samen te voegen met freq_tabel---
source("scr/02 functies chi kwadraat toets 2.R")
# stap 1: maak een mooie tabel met gebieden en sd_residuen per antwoord

mr_vragen <- c(mr_vragen_onl, mr_vragen_tot, mr_vragen_geb)
sr_vragen <- c(sr_vragen_onl_za, sr_vragen_geb, sr_vragen_tot)

# nb : geen chitoets voor mr_vragen_O10_alt en sr_vragen_O10_alt

my_pivot_longer <- function(x) {
  x |>
    pivot_longer(
      cols = where(is.numeric),
      names_to = 'value',
      values_to = 'stdres'
    ) |>
    mutate(
      value_clean = str_replace_all(
        value,
        "\\.",
        " "
      )
    )
}


# naar stadsdelen
chi_publiekstabel_sd <- list(
  mr_vragen |>
    map(\(j) {
      my_chi_tibble(
        type_freq = freq_list_za,
        type_vraag = 'mr',
        i = j,
        geb_var = 'stadsdelen'
      )
    }) |>
    set_names(mr_vragen) |>
    map(\(x) my_pivot_longer(x)),

  sr_vragen |>
    map(\(j) {
      my_chi_tibble(
        type_freq = freq_list_za,
        type_vraag = 'sr',
        i = j,
        geb_var = 'stadsdelen'
      )
    }) |>
    set_names(sr_vragen) |>
    map(\(x) my_pivot_longer(x))
) |>
  list_flatten()

# naar gebieden
chi_publiekstabel_geb <- list(
  mr_vragen |>
    map(\(j) {
      my_chi_tibble(
        type_freq = freq_list_za,
        type_vraag = 'mr',
        i = j,
        geb_var = 'ggw_gebieden'
      )
    }) |>
    set_names(mr_vragen) |>
    map(\(x) my_pivot_longer(x)),

  sr_vragen |>
    map(\(j) {
      my_chi_tibble(
        type_freq = freq_list_za,
        type_vraag = 'sr',
        i = j,
        geb_var = 'ggw_gebieden'
      )
    }) |>
    set_names(sr_vragen) |>
    map(\(x) my_pivot_longer(x))
) |>
  list_flatten()

# naar parken
chi_publiekstabel_park <- list(
  mr_vragen_O10_alt |>
    map(\(j) {
      my_chi_tibble(
        type_freq = freq_list_park,
        type_vraag = 'mr',
        i = j,
        geb_var = 'parken'
      )
    }) |>
    set_names(mr_vragen_O10_alt) |>
    map(\(x) my_pivot_longer(x)),

  sr_vragen_O10_alt |>
    map(\(j) {
      my_chi_tibble(
        type_freq = freq_list_park,
        type_vraag = 'sr',
        i = j,
        geb_var = 'parken'
      )
    }) |>
    set_names(sr_vragen_O10_alt) |>
    map(\(x) my_pivot_longer(x))
) |>
  list_flatten()


# stap 2 : merge de mooie chi-tabel met de frequentietabel

my_signif <- function(x) {
  x |>
    map(\(x) {
      mutate(
        x,
        sig_verschil = case_when(
          stdres >= 1.9 ~ 'significant hoger dan stedelijk gemiddelde',
          stdres <= -1.9 ~ 'significant lager dan stedelijk gemiddelde',
          is.na(stdres) ~ 'stedelijk gemiddelde',
          TRUE ~ 'niet significant'
        )
      )
    })
}


# zorg voor de juiste volgorde van de items in de frequentielijsten
sorted_names <- gtools::mixedsort(names(freq_list_slim_za))
sorted_names_parken <- gtools::mixedsort(names(freq_list_park_slim))

# zet de onderliggende items in de freq_lijsten op de juiste volgorde
sorted_freq_list <- freq_list_slim_za[sorted_names]
sorted_freq_list_parken <- freq_list_park_slim[sorted_names_parken]

# zet de onderliggende items in de chikwadraat lijsten op de juisten volgorde
sorted_chi_pub_sd <- chi_publiekstabel_sd[sorted_names] |>
  map(\(x) select(x, -value))
sorted_chi_pub_geb <- chi_publiekstabel_geb[sorted_names] |>
  map(\(x) select(x, -value))
sorted_chi_pub_park <- chi_publiekstabel_park[sorted_names_parken] |>
  map(\(x) select(x, -value))

result_list = list()


# voor amsterdam geen chikwadraattoets
result_list$ams <- sorted_freq_list |>
  map(\(x) filter(x, spatial_type == 'gemeente')) |>
  map(\(x) select(x, -(value_clean)))

result_list$sd <- sorted_freq_list |>
  map(\(x) filter(x, spatial_type == 'stadsdelen')) |>
  map2(sorted_chi_pub_sd, \(x, y) {
    left_join(x, y, by = c("name", "value_clean", "spatial_name"))
  }) |>
  my_signif() |>
  map(\(x) select(x, -(value_clean)))

result_list$geb <- sorted_freq_list |>
  map(\(x) filter(x, spatial_type == 'ggw_gebieden')) |>
  map2(sorted_chi_pub_geb, \(x, y) {
    left_join(x, y, by = c("name", "value_clean", "spatial_name"))
  }) |>
  my_signif() |>
  map(\(x) select(x, -(value_clean)))

result_list$park <- sorted_freq_list_parken |>
  map2(sorted_chi_pub_park, \(x, y) {
    left_join(x, y, by = c("name", "value_clean", "spatial_name"))
  }) |>
  my_signif() |>
  map(\(x) select(x, -(value_clean)))

# data zonder chikwadraat
result_list$park_O7_O8_O9 <- freq_park_789_wide

# achtergrond ook zonder chikwadraat
result_list$achtergrond <- freq_list_slim_av

# toevoegen open vragen
result_list[["sd"]] <- c(
  result_list[["sd"]],
  freq_openvragen[["openvragen_sd"]]
)
result_list[["geb"]] <- c(
  result_list[["geb"]],
  freq_openvragen[["openvragen_geb"]]
)
result_list[["park"]] <- c(
  result_list[["park"]],
  freq_openvragen[["openvragen_park"]]
)
write_rds(result_list, "data/processed/result_list1.rds")
