# onderlegger voor chi functie
# filter op antwoordcategorieën a en op spatial_type: ggw_gebieden of stadsdelen

# onderlegger voor chi publiekstabel:
# levert kolommen op: spatial_name, en antwoordopties
my_fre_basis <- function(i, type_freq, type_vraag, geb_var) {
  vraag[[type_vraag]][[i]] |>
    map(\(j) my_fre(x = type_freq[[i]], a = j, geb_var = geb_var)) |>
    set_names(vraag[[type_vraag]][[i]])
}


# chi publiekstabel
my_chi_tibble <- function(i, type_freq, type_vraag, geb_var) {
  ## dit is de basistabel waar de gebiednamen uit worden gehaald

  spatial_name <- my_fre_basis(
    i = i,
    type_freq = type_freq,
    type_vraag = type_vraag,
    geb_var = geb_var
  ) |>
    map(\(x) select(x, spatial_name)) |>
    list_flatten()

  # maak een kolom met de stand.residuen uit de chi2_list met de antwoorden
  stdres <- vraag[[type_vraag]][[i]] |>
    map(\(a) chi2[[type_vraag]][[geb_var]][[i]][[a]][["stdres"]]) |>
    set_names(vraag[[type_vraag]][[i]]) |>
    list_flatten()

  # # voeg beide kolommen samen in een tibble met de naam van het antwoord
  eindtabel <- vraag[[type_vraag]][[i]] |>
    map_df(\(a) {
      data.frame(
        name = a,
        spatial_name = spatial_name[[a]],
        stdres[[a]]
      )
    })
}
