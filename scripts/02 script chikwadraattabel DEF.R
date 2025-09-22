# met dit script worden de basis chi-tabellen gemaakt

# eerst wordt met dit script  de frequentietabellen gemaakt
source("scripts/01 script freqtab 05 vraag samenvoegen.R")

# nb: van O7 en O8 worden geen chi-toetsen berekend

### berekening chi-kwadraadtoetsen per antwoordoptie ###
source("scr/02 functies chi kwadraat toets 1.R")


chi2 <- list()


## dit zijn alle multiple response vragen per stadsdeel en gebied
chi2[['mr']][['stadsdelen']] <- c(
  mr_vragen_tot,
  mr_vragen_onl,
  mr_vragen_geb
) |>
  map(\(j) {
    my_chi_map(
      type_freq = freq_list_za,
      i = j,
      geb_var = 'stadsdelen',
      type_vraag = 'mr'
    )
  }) |>
  set_names(c(mr_vragen_tot, mr_vragen_onl, mr_vragen_geb))


chi2[['mr']][['ggw_gebieden']] <- c(
  mr_vragen_tot,
  mr_vragen_onl,
  mr_vragen_geb
) |>
  map(\(j) {
    my_chi_map(
      type_freq = freq_list_za,
      i = j,
      geb_var = 'ggw_gebieden',
      type_vraag = 'mr'
    )
  }) |>
  set_names(c(mr_vragen_tot, mr_vragen_onl, mr_vragen_geb))

# dit zijn de sr vragen zonder achtergrondvragen
# geen chikwadraat toets voor de achtergrondvragen
sr_vragen_onl_za <- c(
  "O24",
  "O29",
  "O38",
  "O39",
  "O40",
  "O42",
  "O43",
  "O48"
)

chi2[['sr']][['stadsdelen']] <- c(sr_vragen_tot, sr_vragen_onl_za) |>
  map(\(j) {
    my_chi_map(
      type_freq = freq_list_za,
      i = j,
      geb_var = 'stadsdelen',
      type_vraag = 'sr'
    )
  }) |>
  set_names(c(sr_vragen_tot, sr_vragen_onl_za))

chi2[['sr']][['ggw_gebieden']] <- c(sr_vragen_tot, sr_vragen_onl_za) |>
  map(\(j) {
    my_chi_map(
      type_freq = freq_list_za,
      i = j,
      geb_var = 'ggw_gebieden',
      type_vraag = 'sr'
    )
  }) |>
  set_names(c(sr_vragen_tot, sr_vragen_onl_za))

# dit zijn de chitoetsen voor de parken
chi2[['sr']][['parken']] <- sr_vragen_O10_alt |>
  map(\(j) {
    my_chi_map(
      type_freq = freq_list_park,
      i = j,
      geb_var = 'parken',
      type_vraag = 'sr'
    )
  }) |>
  set_names(sr_vragen_O10_alt)


chi2[['mr']][['parken']] <- mr_vragen_O10_alt |>
  map(\(j) {
    my_chi_map(
      type_freq = freq_list_park,
      i = j,
      geb_var = 'parken',
      type_vraag = 'mr'
    )
  }) |>
  set_names(mr_vragen_O10_alt)
