test_that("right (number of) species", {
  expect_identical(
    get_species(camtrapdp),
    tibble(
      taxon_id = c(
        "DGP6",
        "3F6VX",
        "6RRQT",
        "49JSC",
        "4RM67"
      ),
      scientific_name = c(
        "Anas platyrhynchos",
        "Gallinula chloropus",
        "Myocastor coypus",
        "Ondatra zibethicus",
        "Rattus norvegicus"
      ),
      vernacular_names.en = c(
        "mallard",
        "common moorhen",
        "coypu",
        "muskrat",
        "brown rat"
      ),
      vernacular_names.nl = c(
        "wilde eend",
        "waterhoen",
        "beverrat",
        "muskusrat",
        "bruine rat"
      )
    )
  )
})
