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
  # pred_not
  not_pred <- pred_not(arg = "a", value = "b")
  expect_equal(not_pred$arg, "a")
  expect_equal(not_pred$value, "b")
  expect_equal(not_pred$type, "notEquals")
  expect_equal(not_pred$expr, glue("(a != \"b\")"))
})

test_that("specific tests: pred_gt and pred_gte", {
  # pred_gt
  gt_pred <- pred_gt(arg = "a", value = 3)
  expect_equal(gt_pred$arg, "a")
  expect_equal(gt_pred$value, 3)
  expect_equal(gt_pred$type, "greaterThan")
  expect_equal(gt_pred$expr, glue("(a > 3)"))
  # pred_gte
  gte_pred <- pred_gte(arg = "a", value = 3)
  expect_equal(gte_pred$arg, "a")
  expect_equal(gte_pred$value, 3)
  expect_equal(gte_pred$type, "greaterThanOrEquals")
  expect_equal(gte_pred$expr, glue("(a >= 3)"))
})

test_that("specific tests: pred_lt and pred_lte", {
  # pred_lt
  lt_pred <- pred_lt(arg = "a", value = 3)
  expect_equal(lt_pred$arg, "a")
  expect_equal(lt_pred$value, 3)
  expect_equal(lt_pred$type, "lessThan")
  expect_equal(lt_pred$expr, glue("(a < 3)"))
  # pred_lte
  lte_pred <- pred_lte(arg = "a", value = 3)
  expect_equal(lte_pred$arg, "a")
  expect_equal(lte_pred$value, 3)
  expect_equal(lte_pred$type, "lessThanOrEquals")
  expect_equal(lte_pred$expr, glue("(a <= 3)"))
})

test_that("specific tests: pred_in and pred_notin", {
  # pred_in
  in_pred <- pred_in(arg = "a", value = c("b", "c"))
  expect_equal(in_pred$arg, "a")
  expect_equal(in_pred$value, c("b", "c"))
  expect_equal(in_pred$type, "in")
  expect_equal(in_pred$expr, glue("(a %in% c(\"b\",\"c\"))"))
  # pred_notin
  notin_pred <- pred_notin(arg = "a", value = c("b", "c"))
  expect_equal(notin_pred$arg, "a")
  expect_equal(notin_pred$value, c("b", "c"))
  expect_equal(notin_pred$type, "notIn")
  expect_equal(notin_pred$expr, glue("(!(a %in% c(\"b\",\"c\")))"))
})

test_that("specific tests: pred_and and pred_or", {
  basic_pred <- pred(arg = "col1", value = "b")
  in_pred <- pred_in(arg = "col2", value = c("b", "c"))
  lt_pred <- pred_lt(arg = "col3", value = 3)
  notna_pred <- pred_notna(arg = "col4")
  # pred_and
  and_pred <- pred_and(basic_pred, in_pred, lt_pred, notna_pred)
  expect_equal(and_pred$arg, list(basic_pred$arg,
                                  in_pred$arg,
                                  lt_pred$arg,
                                  notna_pred$arg))
  expect_equal(and_pred$value, list(basic_pred$value,
                                  in_pred$value,
                                  lt_pred$value,
                                  notna_pred$value))
  expect_equal(and_pred$type, list(basic_pred$type,
                                    in_pred$type,
                                    lt_pred$type,
                                    notna_pred$type))
  expect_equal(and_pred$expr, glue("(",
                                   glue_collapse(c(basic_pred$expr,
                                                   in_pred$expr,
                                                   lt_pred$expr,
                                                   notna_pred$expr),
                                                 sep = " & "),
                                   ")"))
  # pred_or
  or_pred <- pred_or(basic_pred, in_pred, lt_pred, notna_pred)
  expect_equal(or_pred$arg, list(basic_pred$arg,
                                 in_pred$arg,
                                 lt_pred$arg,
                                 notna_pred$arg))
  expect_equal(or_pred$value, list(basic_pred$value,
                                   in_pred$value,
                                   lt_pred$value,
                                   notna_pred$value))
  expect_equal(or_pred$type, list(basic_pred$type,
                                   in_pred$type,
                                   lt_pred$type,
                                   notna_pred$type))
  expect_equal(or_pred$expr, glue("(",
                                  glue_collapse(c(basic_pred$expr,
                                                  in_pred$expr,
                                                  lt_pred$expr,
                                                  notna_pred$expr),
                                                sep = " | "),
                                  ")"))
})

test_that("specific tests: nesting pred_and and pred_or", {
  basic_pred <- pred(arg = "col1", value = "b")
  in_pred <- pred_in(arg = "col2", value = c("b", "c"))
  lt_pred <- pred_lt(arg = "col3", value = 3)
  notna_pred <- pred_notna(arg = "col4")
  # nested pred_and and pred_or
  nested_pred <- pred_and(pred_and(basic_pred, in_pred),
                       pred_or(lt_pred, notna_pred))
  expect_equal(length(nested_pred$arg), 2)
  expect_equal(nested_pred$arg[[1]], pred_and(basic_pred, in_pred)$arg)
  expect_equal(nested_pred$arg[[2]], pred_or(lt_pred, notna_pred)$arg)
  expect_equal(nested_pred$value[[1]], pred_and(basic_pred, in_pred)$value)
  expect_equal(nested_pred$value[[2]], pred_or(lt_pred, notna_pred)$value)
  expect_equal(nested_pred$type[[1]], pred_and(basic_pred, in_pred)$type)
  expect_equal(nested_pred$type[[2]], pred_or(lt_pred, notna_pred)$type)
  expect_equal(nested_pred$expr,
               glue("(",
                 glue_collapse(c(pred_and(basic_pred, in_pred)$expr,
                                 pred_or(lt_pred, notna_pred)$expr),
                               sep = " & "),
                 ")"))
})
