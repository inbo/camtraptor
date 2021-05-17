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

test_that("right length predicate slots for all filter predicates", {
  # check length of all slots (arg, value, type, expr) for basic pred()
  basic_pred <- pred(arg = "a", value = "b")
  expect_equal(length(basic_pred$arg), 1)
  expect_equal(length(basic_pred$value), 1)
  expect_equal(length(basic_pred$type), 1)
  expect_equal(length(basic_pred$expr), 1)

  # check length of all slots (arg, value, type, expr) for negation not_pred()
  not_pred <- pred(arg = "a", value = "b")
  expect_equal(length(not_pred$arg), 1)
  expect_equal(length(not_pred$value), 1)
  expect_equal(length(not_pred$type), 1)
  expect_equal(length(not_pred$expr), 1)

  # check length of all slots (arg, value, type, expr) for pred_in()
  in_pred <- pred_in(arg = "a", value = c(1,2))
  expect_equal(length(in_pred$arg), 1)
  expect_equal(length(in_pred$value), 2)
  expect_equal(length(in_pred$type), 1)
  expect_equal(length(in_pred$expr), 1)

  # check length of all slots (arg, value, type, expr) for combination
  # predicates by pred_and
  and_pred <- pred_and(basic_pred,
                       not_pred,
                       in_pred)
  expect_equal(length(and_pred$arg), 3)
  expect_equal(length(and_pred$value), 3)
  expect_equal(length(and_pred$type), 3)
  expect_equal(length(and_pred$expr), 1) # always one returned expression

  expect_equal(length(and_pred$arg[[1]]), 1)
  expect_equal(length(and_pred$arg[[2]]), 1)
  expect_equal(length(and_pred$arg[[3]]), 1)
  expect_equal(length(and_pred$value[[1]]), 1) # == "b"
  expect_equal(length(and_pred$value[[2]]), 1) # != "b"
  expect_equal(length(and_pred$value[[3]]), 2) # in c(1,2)
  expect_equal(length(and_pred$type[[1]]), 1) # "=="
  expect_equal(length(and_pred$type[[2]]), 1) # "!="
  expect_equal(length(and_pred$type[[3]]), 1) # "in"

  # check length of all slots (arg, value, type, expr) for combination
  # predicates by pred_or
  or_pred <- pred_or(basic_pred,
                       not_pred,
                       in_pred)
  expect_equal(length(or_pred$arg), 3)
  expect_equal(length(or_pred$value), 3)
  expect_equal(length(or_pred$type), 3)
  expect_equal(length(or_pred$expr), 1) # always one returned expression
})

test_that("specific tests: pred_na and pred_notna", {

  # pred_na
  expect_error(pred_na(arg = "a", value = "b"))
  na_pred <- pred_na(arg = "a")
  expect_equal(na_pred$arg, "a")
  expect_true(is.na(na_pred$value))
  expect_equal(na_pred$type, "na")
  expect_equal(na_pred$expr, glue("(is.na(a))"))

  #pred_notna
  expect_error(pred_notna(arg = "a", value = "b"))
  notna_pred <- pred_notna(arg = "a")
  expect_equal(notna_pred$arg, "a")
  expect_true(is.na(notna_pred$value))
  expect_equal(notna_pred$type, "notNa")
  expect_equal(notna_pred$expr, glue("(!is.na(a))"))

})

test_that("specific tests: pred and pred_not", {
  # pred
  basic_pred <- pred(arg = "a", value = "b")
  expect_equal(basic_pred$arg, "a")
  expect_equal(basic_pred$value, "b")
  expect_equal(basic_pred$type, "equals")
  expect_equal(basic_pred$expr, glue("(a == \"b\")"))
  # not_pred
  not_pred <- pred_not(arg = "a", value = "b")
  expect_equal(not_pred$arg, "a")
  expect_equal(not_pred$value, "b")
  expect_equal(not_pred$type, "notEquals")
  expect_equal(not_pred$expr, glue("(a != \"b\")"))
})

test_that("specific tests: pred_gt and pred_gte", {
  gt_pred <- pred_gt(arg = "a", value = 3)
  expect_equal(gt_pred$arg, "a")
  expect_equal(gt_pred$value, 3)
  expect_equal(gt_pred$type, "greaterThan")
  expect_equal(gt_pred$expr, glue("(a > 3)"))

  gte_pred <- pred_gte(arg = "a", value = 3)
  expect_equal(gte_pred$arg, "a")
  expect_equal(gte_pred$value, 3)
  expect_equal(gte_pred$type, "greaterThanOrEquals")
  expect_equal(gte_pred$expr, glue("(a >= 3)"))
})

test_that("specific tests: pred_lt and pred_lte", {
  lt_pred <- pred_lt(arg = "a", value = 3)
  expect_equal(lt_pred$arg, "a")
  expect_equal(lt_pred$value, 3)
  expect_equal(lt_pred$type, "lessThan")
  expect_equal(lt_pred$expr, glue("(a < 3)"))

  lte_pred <- pred_lte(arg = "a", value = 3)
  expect_equal(lte_pred$arg, "a")
  expect_equal(lte_pred$value, 3)
  expect_equal(lte_pred$type, "lessThanOrEquals")
  expect_equal(lte_pred$expr, glue("(a <= 3)"))
})


