library(haven)
library(tidyverse)

# inlezen algemene scripts
source(
  "http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R"
)


# functie om de totale ongewogen respons per indicator per gebied weer te geven
# labels verwijderd, want zelfde  aan name
my_total_n <- function(x) {
  x |>
    group_by(name, spatial_type, spatial_name) |>
    mutate(
      totaal_ong = sum(aantal_ong),
      totaal_gew = sum(aantal_gew)
    )
}


### deze functie maakt de frequentietabellen voor de multirespone vragen
my_summarise <- function(x, i, geb_var, wf) {
  # dit is een lijstje met labels van de multiresonse vragen
  labels <- x |>
    select(all_of(i)) |>
    names() |>
    map_df(
      \(i) {
        tibble(
          name = i,
          labels = attr(x[[i]], "label")
        )
      }
    )

  # dit is de uiteindelijke frequentietabel
  y <- x |>
    select(all_of(i), {{ geb_var }}, {{ wf }}) |>
    pivot_longer(cols = i) |>
    filter(!is.na(value)) |>
    left_join(labels, by = c('name')) |>
    group_by(name, labels, {{ geb_var }}, value) |>
    summarise(
      aantal_ong = n(),
      aantal_gew = sum({{ wf }})
    ) |>
    group_by({{ geb_var }}, name) |>
    mutate(
      aandeel_ong = aantal_ong / sum(aantal_ong),
      aandeel_gew = aantal_gew / sum(aantal_gew)
    )

  return(y)
}

# sd       : stadsdelen zonder westpoort factor
# stadsdeel: stadsdelen met westpoort (incl. westpoort) character

# i is de looping vector met daarin de antwoordopties
my_bind_rows <- function(x = data_groen, i, typevraag = 'mr', weeg) {
  # frequentietabel van amsterdam
  y <- bind_rows(
    x |>
      my_summarise(
        i = vraag[[typevraag]][[i]],
        geb_var = NULL,
        wf = {{ weeg }}
      ) |>
      add_column(
        spatial_type = 'gemeente',
        spatial_name = 'Amsterdam'
      ),

    # frequentietabel van de stadelen
    x |>
      my_summarise(
        i = vraag[[typevraag]][[i]],
        geb_var = sd,
        wf = {{ weeg }}
      ) |>
      rename(spatial_name = sd) |>
      add_column(spatial_type = 'stadsdelen'),

    # frequentietabel van de gebieden
    x |>
      my_summarise(
        i = vraag[[typevraag]][[i]],
        geb_var = geb,
        wf = {{ weeg }}
      ) |>
      rename(spatial_name = geb) |>
      add_column(spatial_type = 'ggw_gebieden')
  ) |>
    # voeg een kolom met de vraag toe voor de duidelijkheid
    add_column(vraag = vraag_naam[[typevraag]][[i]]) |>
    select(vraag, name, labels, spatial_type, spatial_name, everything())

  return(y)
}


# nieuwe functie voor 32: iets ingewikkelder: ook met totale n op antwoord 1
my_sum_32_function <- function(var_32, suf) {
  tabel <- bind_rows(
    data_groen |>
      group_by(O32 = {{ var_32 }}) |> # totaal amsterdam
      summarise(
        "aantal_ong_{suf}" := n(),
        "aantal_gew_{suf}" := sum(weeg_ONLINE, na.rm = T)
      ) |>
      add_column(
        spatial_type = 'gemeente',
        spatial_name = 'Amsterdam'
      ),

    data_groen |>
      group_by(O32 = {{ var_32 }}, spatial_name = sd) |> # per stadsdeel
      summarise(
        "aantal_ong_{suf}" := n(),
        "aantal_gew_{suf}" := sum(weeg_ONLINE, na.rm = T)
      ) |>
      add_column(
        spatial_type = 'stadsdelen'
      ),

    data_groen |>
      group_by(O32 = {{ var_32 }}, spatial_name = geb) |> # per gebied
      summarise(
        "aantal_ong_{suf}" := n(),
        "aantal_gew_{suf}" := sum(weeg_ONLINE, na.rm = T)
      ) |>
      add_column(
        spatial_type = 'gebieden'
      )
  )
}
