## inlezen freq tabellen

source("scripts/01 script freqtab 01 vraag basis.R") # in deze tabel worden ook de functies ingelezen
source("scripts/01 script freqtab 02 vraag naar groen O10.R")
source("scripts/01 script freqtab 03 vraag 07 08 09.R")
source("scripts/01 script freqtab 04 vraag open.R")


# voeg de gewogen en ongewogen totale N toe
freq_list <- freq_list |>
  map(\(x) my_total_n(x))

# overschrijf de oude tabel van 32 met de juiste versie
freq_list[['O32']] <- O32

freq_list_park <- freq_list_park |>
  map(\(x) my_total_n(x))


my_clean <- function(x) {
  x |>
    mutate(value_clean = str_replace_all(value, "[[:punct:]]", " "))
}
### nette volgorde voor publicatiedoeleinden
### verwijderen van interpunctie bij value
freq_list_slim <- freq_list |>
  map(\(x) {
    select(
      x,
      any_of(c(
        "vraag",
        "park_naam",
        "name",
        "labels",
        "spatial_type",
        "spatial_name",
        "value",
        "totaal_ong",
        "totaal_gew",
        "aandeel_gew",
        "gem_rapportcijfer"
      ))
    )
  }) |>
  map(\(x) my_clean(x))

freq_list_park_slim <- freq_list_park |>
  map(\(x) {
    select(
      x,
      any_of(c(
        "vraag",
        "park_naam",
        "name",
        "labels",
        "spatial_type",
        "spatial_name",
        "value",
        "totaal_ong",
        "totaal_gew",
        "aandeel_gew",
        "gem_rapportcijfer"
      ))
    )
  }) |>
  map(\(x) my_clean(x))


### opsplitsen freq_list zonder achtergrondvragen en achtergrondvragen appart

vragen_onl_tot <- c(
  "T4",
  "T26",
  "T34",
  "T46",
  "O1",
  "O2a",
  "O2b",
  "O2c",
  "O2d",
  "O2e",
  "O31",
  "O32",
  "O41",
  "O47",
  "O48t",
  "O6",
  "T30",
  "Tbomen",
  "T45",
  "T33",
  "T35",
  "T36",
  "T37",
  "O24",
  "O29",
  "O38",
  "O39",
  "O40",
  "O42",
  "O43",
  "O48"
)

vragen_achtergr <- c(
  "O10_alt",
  "geslacht",
  "oplcat",
  "inkcat",
  "mig8",
  "hh",
  "gebied"
)


# opsplitsen met en zonder achtergrondvragen
freq_list_za <- freq_list[vragen_onl_tot] # zonder achtegrondvragen
freq_list_av <- freq_list[vragen_achtergr] # alleen achtegrondvragen

freq_list_slim_za <- freq_list_slim[vragen_onl_tot] # zonder achtegrondvragen
freq_list_slim_av <- freq_list_slim[vragen_achtergr] # alleen achtegrondvragen
