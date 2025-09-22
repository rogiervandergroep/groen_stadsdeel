vraag_naam <- list()

## achtergrondvragen

vraag_naam[['sr']][['lft4cat']] <- "leeftijdscategorie"

vraag_naam[['sr']][['geslacht']] <- "geslacht"

vraag_naam[['sr']][['oplcat']] <- "opleidingsniveau"

vraag_naam[['sr']][['inkcat']] <- "inkomenscategorie"

vraag_naam[['sr']][['gebied']] <- "ggw_gebieden"

vraag_naam[['sr']][['hh']] <- "huishoudsamenstelling"

vraag_naam[['sr']][['mig8']] <- "migratieachtergrond"


vraag_naam[['mr']][[
  'O1'
]] <- "V1: elke van de volgende activiteiten doet u weleens buitenshuis (in Amsterdam of Weesp)?"
vraag_naam[['mr']][[
  'O2a'
]] <- "V2a: Als u buitenshuis afspreekt met vrienden, familie of collega’s, waar spreekt u dan meestal af?"
vraag_naam[['mr']][[
  'O2b'
]] <- "V2b: Als u buitenshuis een plek opzoekt om onder de mensen te zijn of nieuwe mensen te ontmoeten, waar gaat u dan meestal heen?"
vraag_naam[['mr']][[
  'O2c'
]] <- "V2c: Als u buitenshuis een plek opzoekt om tot rust te komen, waar gaat u dan meestal heen?"
vraag_naam[['mr']][[
  'O2d'
]] <- "V2d: Als u naar buiten gaat om te sporten, waar gaat u dan meestal naartoe?"
vraag_naam[['mr']][[
  'O2e'
]] <- "V2e: Als u een rondje gaat wandelen of fietsen, waar gaat u dan meestal heen?"
vraag_naam[['mr']][[
  'T4'
]] <- 'V4: In welke soorten openbare groene gebieden in Amsterdam of in Weesp bent u in het afgelopen jaar geweest?'

# open vraag
vraag_naam[['sr']][[
  'O5'
]] <- 'V5: Kunt u aangeven waarom u geen openbaar groen gebied in Amsterdam bezocht?'


vraag_naam[['mr']][[
  'O6'
]] <- 'V6: Amsterdamse parkachtige groengebieden waar bewoners in het afgelopen jaar zijn geweest.'


# V7: Redenen van bezoek van parkachtig groengebieden in Noord
vraag_naam[["O7"]] <- set_names(parken)

# V8: Frequentie van bezoek van parkachtig groengebieden in Noord
vraag_naam[["O8"]] <- set_names(parken)

# V9: Gemiddeld rapportcijfer waarmee de parkachtige groengebieden in Noord worden beoordeeld
vraag_naam[["O9"]] <- set_names(parken)


#####  namen van vragen per park ####

vraag_naam[['sr']][[
  'O10_alt'
]] <- 'V10: Kunt u het groene gebied waar u in het afgelopen jaar het vaakst bent geweest in onderstaande lijst aanvinken?'


vraag_naam[['sr']][['O111']] <- 'V11: hoeveel tijd bracht u door in het park'

vraag_naam[['mr']][[
  'O12'
]] <- 'V12: Met wie ging u meestal naar [uw favoriete groengebied]?'

vraag_naam[['mr']][[
  'O13'
]] <- 'V13: Hoe ging u meestal naar [uw favoriete groengebied]?'

vraag_naam[['sr']][[
  'O141'
]] <- 'V14: Wat vindt u van de bereikbaarheid van [uw favoriete groengebied]?'

vraag_naam[['mr']][[
  'O15'
]] <- 'V15: Wat deed u meestal in [uw favoriete groengebied]?'

vraag_naam[['mr']][[
  'O16'
]] <- 'V16: Waarom koos u voor die activiteit(en) juist [uw favoriete groengebied]?'

vraag_naam[['mr']][[
  'O17'
]] <- 'V17: Hoe tevreden bent u over de volgende onderdelen van de inrichting van [uw favoriete groengebied]?'

vraag_naam[['sr']][[
  'O18'
]] <- 'V18: Wat vindt u over het algemeen van de drukte in [uw favoriete groengebied]?'

vraag_naam[['mr']][[
  'O19'
]] <- 'V19: wat vindt u van de drukte en van de rust?'

vraag_naam[['sr']][[
  'O21'
]] <- 'V21: Hoe beoordeelt u [uw favoriete groengebied] ten opzichte van een jaar geleden?'

# open vraag
vraag_naam[['sr']][[
  'O22'
]] <- 'V22: wat zou er verbeterd kunnen worden aan dit winkelgebied?'


### vragen per stadsdeel ---

vraag_naam[['sr']][[
  'O24'
]] <- 'V24: Als u lopend of met de fiets onderweg bent, kiest u dan weleens voor een groene route (bijvoorbeeld door een park of langs bomen) in plaats van de snelste route?'

vraag_naam[['sr']][[
  'O25'
]] <- 'V25: Wat zou het voor u logischer, makkelijker of leuker maken om zulke groene gebieden te bezoeken?'


vraag_naam[['mr']][[
  'T26'
]] <- 'V26: Kwam u in het afgelopen jaar weleens in een groen gebied aan de rand van Amsterdam?'


# O27 : kaart
# O28 : kaart

vraag_naam[['sr']][[
  'O29'
]] <- 'V29: Wat vindt u van de afstand van uw woning tot grotere groene gebieden, zoals parken?'


vraag_naam[['sr']][[
  'T30'
]] <- 'V30a: Hoe tevreden bent u met de hoeveelheid groen in uw woonomgeving?'

vraag_naam[['sr']][[
  'Tbomen'
]] <- 'V30b: Hoe tevreden bent u met de hoeveelheid bomen in uw woonomgeving?'

vraag_naam[['mr']][[
  'O31'
]] <- 'V31: Hoe tevreden bent u over de kwaliteit van het groen in uw woonomgeving?'

vraag_naam[['mr']][[
  'O32'
]] <- 'V32: Wat is volgens u het belangrijkst bij het verbeteren van het groen in uw woonomgeving?'

# open vraag
vraag_naam[['sr']][[
  'O32_Other5'
]] <- 'O32_Other5: Wat is volgens u het belangrijkst bij het verbeteren van het groen in uw woonomgeving?'


vraag_naam[['sr']][[
  'T33'
]] <- 'V33: Als u vanuit uw huis naar de straat kijkt, ziet u dan groen (zoals bomen, planten, struiken, grasveldjes)?'

vraag_naam[['mr']][[
  'T34'
]] <- 'V34: Als u vanuit uw huis naar de straat kijkt, ziet u dan groen?'

vraag_naam[['sr']][[
  'T35'
]] <- 'V35: Hoe ziet uw tuin, balkon of dakterras eruit??'


vraag_naam[['sr']][[
  'T36'
]] <- 'V36: Zou u uw tuin, balkon of dakterras (verder) willen vergroenen?'


vraag_naam[['sr']][[
  'T37'
]] <- 'V37: Wist u dat de gemeente hulp biedt bij het vergroenen van uw buitenruimte?'


vraag_naam[['sr']][[
  'O38'
]] <- 'V38: Wist u dat u bij de gemeente gratis een geveltuin aan kunt vragen?'

vraag_naam[['sr']][[
  'O39'
]] <- 'V39: Heeft u interesse in een geveltuin?'


vraag_naam[['sr']][[
  'O40'
]] <- 'V40: Verzorgt u groen in de openbare ruimte?'

vraag_naam[['mr']][[
  'O41'
]] <- 'v41: Waarom verzorgt u groen in de openbare ruimte?'

vraag_naam[['sr']][[
  'O42'
]] <- 'v42: Zou u dat willen doen?'


vraag_naam[['sr']][[
  'O43'
]] <- 'v43: Weet u waar u informatie kunt vinden over de mogelijkheid om zelf voor een stukje groen in Amsterdam of Weesp te zorgen?'


# v44 is open vraag
vraag_naam[['sr']][[
  'O44'
]] <- 'v44: Waarom niet??'


vraag_naam[['sr']][[
  'T45'
]] <- 'V45: Hoe tevreden bent u met de hoeveelheid groen in Amsterdam?'

vraag_naam[['mr']][['T46']] <- c(
  'V46: Hoe belangrijk is het groen in Amsterdam'
)

vraag_naam[['mr']][['O47']] <- c(
  'V47: In hoeverre bent u het eens met onderstaande stellingen'
)

vraag_naam[['sr']][[
  'O48'
]] <- 'V48: Wat vindt u van de organisatie van festivals en evenementen in groene gebieden in Amsterdam?'

vraag_naam[['mr']][['O48t']] <- c(
  'V48t: Onder welke voorwaarde(n) vindt u dat festivals en evenementen wel in het groen  georganiseerd kunnen worden?'
)

vraag_naam[['sr']][[
  'O48_toelichting_nw_Other6'
]] <- 'V48 toelichting bij anders.'


vraag_naam[['sr']][[
  'O49'
]] <- 'V49: Vermijdt u  weleens plekken in het groen in Amsterdam of Weesp omdat u zich daar onveilig voelt?'


vraag_naam[['sr']][[
  'O49_toelichting'
]] <- 'V49 toelichting: Als u dat wilt, kunt u uw antwoord hieronder toelichten.'
