# onderlegger voor chi functie
# filter op antwoordcategorieën van name = a
# filter op spatial_type (geb_var): ggw_gebieden  stadsdelen, parken

my_fre <- function(x, a, geb_var) {
  x |>
    ungroup() |>
    filter(
      spatial_type == geb_var,
      name == a
    ) |>
    select(value, aantal_gew, spatial_name) |>
    mutate(aantal_gew = round(aantal_gew)) |>
    pivot_wider(
      names_from = value,
      values_from = aantal_gew,
      values_fill = 0
    )
}


### chitoets om stadsdelen te vergelijken

# type_freq: keuze uit freq_list of freq_list_park
# type_vraag: keuze uit 'sr', 'mr', '07', '08'
# nb: freq_list werkt alleen bij sr en mr
# nb: freq_list_park werkt alleen bij 07 en 08
# i:  zijn de vragen waar doorheen gemapt wordt
# geb_var: 'parken', 'stadsdelen of 'ggw_gebieden'

my_chi_map <- function(i, type_freq, type_vraag, geb_var) {
  y <- vraag[[type_vraag]][[i]] |>
    map(\(j) my_fre(x = type_freq[[i]], a = j, geb_var = geb_var))

  z <- 1:length(y) |>
    map(\(l) chisq.test(y[[l]][-1])) |>
    set_names(vraag[[type_vraag]][[i]])

  return(z)
}

# test <- my_fre(freq_list_za[["O1"]], "O11", "stadsdelen")

#   z <- 1:length(test) |>
#     map(\(l) chisq.test(test[[]][-1]))
