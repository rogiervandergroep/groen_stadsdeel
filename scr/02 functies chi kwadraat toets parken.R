### dit zijn de chikwadraattoetsen voor de vragen over parken

my_fre_p <- function(x, a) {
  x |>
    ungroup() |>
    filter(
      name == a
    ) |>
    select(value, aantal_gew, O10_alt) |>
    mutate(aantal_gew = round(aantal_gew)) |>
    pivot_wider(
      names_from = value,
      values_from = aantal_gew,
      values_fill = 0
    )
}
my_fre_basis_p <- function(i, typevraag) {
  vraag_p[[typevraag]][[i]] |>
    map(\(j) my_fre_p(x = freq_list[[typevraag]][[i]], a = j)) |>
    set_names(vraag_p[[typevraag]][[i]])
}

### chitoets voor parken O10_alt ###
my_chi_map_parken <- function(i, typevraag) {
  y <- vraag_p[[typevraag]][[i]] |>
    map(\(j) my_fre_p(x = freq_list[[typevraag]][[i]], a = j))

  z <- 1:length(y) |>
    map(\(l) chisq.test(y[[l]][-1])) |>
    set_names(vraag_p[[typevraag]][[i]])

  return(z)
}


# chi publiekstabel parken
my_chi_tibble_p <- function(i, typevraag) {
  freq_tabel <- my_fre_basis_p(i, typevraag)

  # maak een kolom met parken
  spatial_name <- vraag_p[[typevraag]][[i]] |>
    map(\(i) freq_tabel[[i]][["O10_alt"]]) |>
    set_names(vraag_p[[typevraag]][[i]])

  # maak een kolom met de stand.residuen uit de chi2_list met alle antwoorden
  stdres <- vraag_p[[typevraag]][[i]] |>
    map(\(a) chi2[[typevraag]][[i]][[a]][["stdres"]]) |>
    set_names(vraag_p[[typevraag]][[i]])

  # voeg beide kolommen samen in een tibble met de naam van het antwoord
  eindtabel <- vraag_p[[typevraag]][[i]] |>
    map_df(\(a) {
      data.frame(
        name = a,
        spatial_name = spatial_name[[a]],
        stdres[[a]]
      )
    })
}
