library(openxlsx)
library(purrr)


# Dit wordt de heading van elke tabel
heading <- createStyle(
  textDecoration = "bold",
  fontColour = "white",
  fgFill = "#004699"
)

# elk kolom met amsterdam er in wordt deze kleur
ams_style <- createStyle(
  textDecoration = "bold",
  fontColour = "white",
  fgFill = "#7f8ac2"
)


# elke rij met totaal aantal respondenten krijgt deze kleur
total_style <- createStyle(
  textDecoration = 'bold',
  fontColour = "black",
  fgFill = "#dcddee"
)

# significatiekleur positief
sign_style_p <- createStyle(
  textDecoration = 'bold',
  fontColour = "#55b462"
)

# significatiekleur negatief
sign_style_n <- createStyle(
  textDecoration = 'bold',
  fontColour = "#ec0000"
)

my_style_sheet <- function(tabel, stadsdeel) {
  x = tabel[[stadsdeel]]

  sheet_nr <- c(1:length(x))

  # maak een leeg workbook aan
  wb <- createWorkbook()

  # Loop through the list and add each element to a new sheet
  for (i in sheet_nr) {
    total_row <- data.frame(which(
      x[[i]] == "totaal aantal respondenten",
      arr.ind = TRUE
    ))

    sheet_name <- names(x)[i]
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, x[[i]])

    # heading stijl
    addStyle(
      wb,
      sheet_name,
      cols = 1:ncol(x[[i]]),
      rows = 1,
      style = heading
    )

    # kleur rijen met totalen
    addStyle(
      wb,
      sheet_name,
      cols = 1:ncol(x[[i]]),
      rows = total_row[, 1] + 1,
      style = total_style,
      gridExpand = TRUE
    )

    conditionalFormatting(
      wb,
      sheet_name,
      cols = 1:ncol(x[[i]]),
      rows = 2:(nrow(x[[i]]) + 1),
      "contains",
      rule = "\U207A",
      style = sign_style_p
    )

    conditionalFormatting(
      wb,
      sheet_name,
      cols = 1:ncol(x[[i]]),
      rows = 2:(nrow(x[[i]]) + 1),
      "contains",
      rule = "\u207B",
      style = sign_style_n
    )
  }

  saveWorkbook(
    wb,
    glue::glue("reports/tabel_v7_{stadsdeel}.xlsx", overwrite = TRUE)
  )
}


names(final_table) |>
  walk(\(x) {
    my_style_sheet(
      tabel = final_table,
      stadsdeel = x
    )
  })
