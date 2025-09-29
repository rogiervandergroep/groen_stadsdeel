# inlezen ruwe data en namen van variabelen
source("scripts/00 inlezen ruwe data vraag_codes.R")
source("scripts/00 inlezen ruwe data vraag_naam.R")

# inlezen functies en libraries
source("scr/01 functies summaries DEF.R")


### eventueel check variabelen
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
  # "O32",
  "O38",
  "O39",
  "O40",
  "O42",
  "O43",
  "O48",
  'O49_Codes'
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

# labels zijn verloren gegaan: opnieuw plakken
freq_list[["O6"]] <- freq_list[["O6"]] |>
  select(-labels) |>
  left_join(p_labels, by = ("name"))


# 032 toevoegen
a <- my_sum_32_function(O321, "O321") # eerst genoemde antwoord
b <- my_sum_32_function(O322, "O322") # tweede antwoord

##### totaal 32: antwoorden worden bij elkaar opgeteld

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
    aantal_ong,
    aantal_gew,
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
    labels = 'Wat is volgens u het belangrijkst bij het verbeteren van het groen in uw woonomgeving?'
  ) |>
  filter(!is.na(spatial_name))
