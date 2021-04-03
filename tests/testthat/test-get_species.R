test_that("right (number of) species", {
  expect_equal(get_species(camtrapdp),
               c("Anas platyrhynchos",
                 "Rattus norvegicus",
                 "Ondatra zibethicus",
                 "Myocastor coypus",
                 "Gallinula chloropus"))
})
