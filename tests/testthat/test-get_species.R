test_that("right (number of) species", {
  expect_identical(
    get_species(camtrapdp),
    tibble(
      scientific_name = c(
        "Rattus norvegicus",
        "Anas platyrhynchos",
        "Myocastor coypus",
        "Ondatra zibethicus",
        "Gallinula chloropus"
      ),
      vernacular_names.en = c(
        "brown rat",
        "mallard",
        "coypu",
        "muskrat",
        "common moorhen"
      ),
      vernacular_names.nl = c(
        "bruine rat",
        "wilde eend",
        "beverrat",
        "muskusrat",
        "waterhoen"
      )
    )
  )
})
