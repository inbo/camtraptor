test_that("right (number of) species", {
  expect_equal(
    get_species(camtrapdp),
    tibble(
      scientific_name = c(
        "Anas platyrhynchos",
        "Rattus norvegicus",
        "Ondatra zibethicus",
        "Myocastor coypus",
        "Gallinula chloropus"
      ),
      vernacular_name = c(
        "Mallard",
        "Norway Rat",
        "Muskrat",
        "Coypu",
        "Common Moorhen"
      )
    )
  )
})
