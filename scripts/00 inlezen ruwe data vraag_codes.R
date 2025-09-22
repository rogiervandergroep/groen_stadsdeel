library(haven)
library(tidyverse)
### bewerking data groen onderzoek voor tabellenrapportage

# weegvariabele = weeg_TOT
# weegvariabele = weeg_ONLINE

# inlezen gebieden en parken
# inlezen algemene scripts
source(
  "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R"
)

source("scr/00 gebieden.R")

data_groen <- haven::read_sav(
  "data/raw/240021_GGO_2024_opgeschoond databestand.sav"
) |>
  haven::as_factor()
# filter(stadsdeel != 'Westpoort')
# nb: er wordt niet gerapporteerd over westpoort, maar de respondenten uit westpoort worden wel meegenomen

vraag <- list()

# achtergrondvragen

vraag[['sr']][['lft4cat']] <- data_groen |>
  select(lft4cat) |>
  names()

vraag[['sr']][['geslacht']] <- data_groen |>
  select(GESLACHT) |>
  names()

vraag[['sr']][['oplcat']] <- data_groen |>
  select(oplcat) |>
  names()

vraag[['sr']][['inkcat']] <- data_groen |>
  select(inkcat) |>
  names()

vraag[['sr']][['gebied']] <- data_groen |>
  select(gebied) |>
  names()

vraag[['sr']][['hh']] <- data_groen |>
  select(hh) |>
  names()

vraag[['sr']][['mig8']] <- data_groen |>
  select(mig8) |>
  names()

#####

# een list met de multiresponse vragen die opgesplitst worden naar stadsdeel en gebied
vraag[['mr']][['O1']] <- data_groen |>
  select(O11:O17) |>
  names()

vraag[['mr']][['O2a']] <- data_groen |>
  select(O2a01:O2a12) |>
  names()

vraag[['mr']][['O2b']] <- data_groen |>
  select(O2b01:O2b12) |>
  names()

# "O2c10" : uit de data verwijders omdat alleen No is aangegeven
vraag[['mr']][['O2c']] <- data_groen |>
  select(O2c01:O2c09) |>
  names()

vraag[['mr']][['O2d']] <- data_groen |>
  select(O2d01:O2d10) |>
  names()

vraag[['mr']][['O2e']] <- data_groen |>
  select(O2e1:O2e9) |>
  names()

# T41_buiten_ams verwijderd
vraag[['mr']][['T4']] <- data_groen |>
  select(T401:T413, T414, T415) |>
  names()

# open vraag
vraag[['sr']][['O5']] <- 'O5'


# let op weeg op gebieden
vraag[['mr']][['O6']] <- data_groen |>
  select(O6_alt01:O6_alt54) |>
  names()


### 07, 08 en 09 zijn per park gesteld ---

parken <- data_groen |>
  select(starts_with("O7_")) |>
  names() |>
  str_remove_all("O7_") |>
  str_remove_all("_GV11") |>
  str_remove_all("_GV12") |>
  str_remove_all("_GV13") |>
  str_remove_all("_GV14") |>
  str_remove_all("_GV15") |>
  str_remove_all("_GV16") |>
  unique()

parken_list <- list()

# v7 bezoekmotief
vraag[["O7"]] <- parken |>
  map(\(x) names(select(data_groen, contains(glue::glue("O7_{x}"))))) |>
  set_names(parken)

# v8 bezoekfrequentie
vraag[["O8"]] <- parken |>
  map(\(x) names(select(data_groen, contains(glue::glue("O8_{x}"))))) |>
  set_names(parken)

# v9 rapportcijfer
parken_list[["O9"]] <- parken |>
  map(\(x) {
    names(select(data_groen, contains(glue::glue("O9_{x}_rapportcijfer1"))))
  }) |>
  set_names(parken)


### vragen 11 tm 16 worden berekend per eerder park (O10) ###

vraag[['sr']][[
  'O10_alt'
]] <- 'O10_alt'


# V11 hoeveel tijd bracht u doot
vraag[['sr']][['O111']] <- 'O111'

# V12: Met wie ging u meestal naar [uw favoriete groengebied]? Meerdere antwoorden mogelijk.
vraag[['mr']][['O12']] <- data_groen |>
  select(O1201:O1210) |>
  names()

# V13: Hoe ging u meestal naar [uw favoriete groengebied]? Meerdere antwoorden mogelijk.
vraag[['mr']][['O13']] <- data_groen |>
  select(O131:O139) |>
  names()

# V14: Wat vindt u van de bereikbaarheid van [uw favoriete groengebied]?
vraag[['sr']][['O141']] <- 'O141'

# V15: Wat deed u meestal in [uw favoriete groengebied]? Meerdere antwoorden mogelijk.
vraag[['mr']][['O15']] <- data_groen |>
  select(O1501:O1529) |>
  names()

# V16: Waarom koos u voor die activiteit(en) juist [uw favoriete groengebied]? Meerdere antwoorden mogelijk.
vraag[['mr']][['O16']] <- data_groen |>
  select(O1601:O1618) |>
  names()

# V17: Hoe tevreden bent u over de volgende onderdelen van de inrichting van [uw favoriete groengebied]?
vraag[['mr']][['O17']] <- data_groen |>
  select(O17_mogelijkheden_om_GV1:O17_hondenveld_GV1) |>
  names()

# V18: Wat vindt u over het algemeen van de drukte in [uw favoriete groengebied]?
vraag[['sr']][['O18']] <- 'O18'

# V19: wat vindt u van de drukte en van de rust
vraag[['mr']][['O19']] <- c('O19_drukte', 'O19_rust')

# V21: Hoe beoordeelt u [uw favoriete groengebied] ten opzichte van een jaar geleden?
vraag[['sr']][['O21']] <- c('O21')

# v22: open vraag: wat zou er verbeterd kunnen worden aan dit winkelgebied?
vraag[['sr']][['O22']] <- c('O22')


## vragen 24 tm 49 gaan over personen uitgesplitst per stadsdeel

# V24: kiest u dan weleens voor een groene route?
vraag[['sr']][['O24']] <- c('O24')

# v22: open vraag: V25: De Amsterdamse parken worden voor veel verschillende redenen bezocht.
# Wat zou het voor u logischer, makkelijker of leuker maken om zulke groene gebieden te bezoeken?
vraag[['sr']][['O25']] <- c('O25')


# groen aan de rand van de stad
vraag[['mr']][['T26']] <- data_groen |>
  select(T26_nw_201:T26_nw_219) |>
  names()

# tevredenheid groen
vraag[['sr']][['T45']] <- c('T45')

# hoe belangrijk is het groen in amsterdam
vraag[['mr']][['T46']] <- data_groen |>
  select(T46_beweging_en_of_s_GV1:T46_werkplezier_GV1) |>
  names()

# stellingen groen
vraag[['mr']][['O47']] <- data_groen |>
  select(O47_Genoeg_groen_is_GV1:O47_In_Amsterdam_moe_GV1) |>
  names()

# wat vind u van festivals?
vraag[['sr']][['O48']] <- c('O48')


# toelichting
vraag[['mr']][['O48t']] <- data_groen |>
  select(O48_toelichting_nw01:O48_toelichting_nw10) |>
  names()

# open vraag bij 'anders'
vraag[['sr']][['O48_toelichting_nw_Other6']] <- c('O48_toelichting_nw_Other6')

# v49 is open vraag: Vermijdt u in Amsterdam of Weesp weleens plekken in het groen omdat u zich daar onveilig voelt?
vraag[['sr']][['O49_toelichting']] <- c('O49_toelichting')


# basis_list5: groen in de woonomgeving
# v28 v29 v30 tbomen v31 v32 v33

# Wat vindt u van de afstand van uw woning tot grotere groene gebieden, zoals parken?
vraag[['sr']][['O29']] <- c('O29')

#Hoe tevreden bent u met de <u>hoeveelheid</u> groen in uw woonomgeving?  <h6>Denk aan de omgeving binnen ongeveer 5 minuten lopen van uw woning.</h6>
vraag[['sr']][['T30']] <- c('T30')

# tevredenheid bomen
vraag[['sr']][['Tbomen']] <- c('Tbomen')

vraag[['mr']][['O31']] <- data_groen |>
  select(
    O31_afwisselende_soo_GV1,
    O31_hoe_schoon_en_ve_GV1,
    O31_hoe_mooi_het_gro_GV1
  ) |>
  names()


# Wat is volgens u het belangrijkst bij het verbeteren van het groen in uw woonomgeving 2 keuzes
vraag[['mr']][['O32']] <- data_groen |>
  select(O321, O322) |>
  names()

# open vraag: toelichting op bovenstaande
vraag[['sr']][['O32_Other5']] <- c('O32_Other5')


#Als u vanuit uw huis naar de straat kijkt, ziet u dan groen (zoals bomen, planten, struiken, grasveldjes)? <h6>Het gaat hierbij <u>niet</u> om een (binnen)tuin achter uw huis.</h6>
vraag[['sr']][['T33']] <- c('T33')


# basislist6: privaatgroen en medebeheer
# v34 en v35 v36 v37 v38 v39 v40 v41 v42 v43 v44

# soorten groen
vraag[['mr']][['T34']] <- data_groen |>
  select(T341:T348) |>
  names()

# Hoe ziet uw tuin, balkon of dakterras eruit?
vraag[['sr']][['T35']] <- c('T35')

# Zou u uw tuin, balkon of dakterras (verder) willen vergroenen
vraag[['sr']][['T36']] <- c('T36')

# Wist u dat de gemeente hulp biedt bij het vergroenen van uw buitenruimte?
vraag[['sr']][['T37']] <- c('T37')

# Wist u dat u bij de gemeente gratis een geveltuin aan kunt vragen?
vraag[['sr']][['O38']] <- c('O38')

# Heeft u interesse in een geveltuin?
vraag[['sr']][['O39']] <- c('O39')

vraag[['sr']][['O40']] <- c('O40')

# V41: Wat zijn voor u redenen om een stukje groen of een tuin te verzorgen? Meerdere antwoorden mogelijk.
vraag[['mr']][['O41']] <- data_groen |>
  select(O4111:O4117) |>
  names()

# open vraag: toelichting op bovenstaande
vraag[['sr']][['O41_Other5']] <- c('O41_Other5')


vraag[['sr']][['O42']] <- c('O42')

vraag[['sr']][['O43']] <- c('O43')

# V44: U geeft aan dat u (misschien) wel een stukje groen of een tuin zou willen verzorgen, maar dit nu (nog) niet doet. Waarom niet?
vraag[['sr']][['O44']] <- c('O44')
