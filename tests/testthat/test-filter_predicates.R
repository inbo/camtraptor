test_that("right class for all filter predicates", {
  expect_s3_class(pred(arg = "a", value = "b"), class = "filter_predicate")
  expect_s3_class(pred_not(arg = "a", value = "b"), class = "filter_predicate")
  expect_s3_class(pred_gt(arg = "a", value = 1), class = "filter_predicate")
  expect_s3_class(pred_gte(arg = "a", value = 1), class = "filter_predicate")
  expect_s3_class(pred_lt(arg = "a", value = 1), class = "filter_predicate")
  expect_s3_class(pred_lte(arg = "a", value = 1), class = "filter_predicate")
  expect_s3_class(pred_in(arg = "a",
                          value = c(1, 2)),
                  class = "filter_predicate")
  expect_s3_class(pred_notin(arg = "a",
                             value = c(1, 2)),
                  class = "filter_predicate")
  expect_s3_class(pred_na(arg = "a"), class = "filter_predicate")
})

test_that("right predicate slots for all filter predicates", {
  # expected slots
  pred_slots <- c("arg", "value", "type", "expr")
  expect_equal(names(pred(arg = "a", value = "b")), pred_slots)
  expect_equal(names(pred_not(arg = "a", value = "b")), pred_slots)
  expect_equal(names(pred_gt(arg = "a", value = 1)), pred_slots)
  expect_equal(names(pred_gte(arg = "a", value = 1)), pred_slots)
  expect_equal(names(pred_lt(arg = "a", value = 1)), pred_slots)
  expect_equal(names(pred_lte(arg = "a", value = 1)), pred_slots)
  expect_equal(names(pred_in(arg = "a", value = c(1, 2))), pred_slots)
  expect_equal(names(pred_notin(arg = "a", value = c(1, 2))), pred_slots)
  expect_equal(names(pred_notna(arg = "a")), pred_slots)
})

test_that("specific tests: pred_na", {
  expect_error(pred_na(arg = "a", value = "b"))
  expect_equal(pred_na(arg = "a")$arg, "a")
  expect_true(is.na(pred_na(arg = "a")$value))
  expect_equal(pred_na(arg = "a")$type, "na")
  expect_equal(pred_na(arg = "a")$expr, glue("(is.na(a))"))
})
