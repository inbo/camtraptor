test_that("map_summary() returns error when `df` is not a grouped data frame", {
  skip_if_offline()
  x <- example_dataset()
  expect_error(
    map_summary(x, feature = "n_scientificName"),
    "The summary must be a data frame.",
    fixed = TRUE
  )
  df <- data.frame(
    deploymentID = c("dep1", "dep2"),
    latitude = c(45.0, 46.0),
    longitude = c(7.0, 8.0),
    n_scientificName = c(5, 10)
  )
  expect_error(
    map_summary(df, feature = "n_scientificName"),
    "The summary must be a grouped data frame.",
    fixed = TRUE
  )
  expect_no_error(
    map_summary(
      df %>% dplyr::group_by(deploymentID, latitude, longitude),
      feature = "n_scientificName"
    )
  )
})

test_that("map_summary() returns error when feature is missing", {
  skip_if_offline()
  df_dep <- example_dataset() %>%
    summarize_deployments()
  expect_error(map_summary(df_dep),
    'argument "feature" is missing, with no default',
    fixed = TRUE
  )
  df_obs <- example_dataset() %>%
    summarize_observations()
  expect_error(map_summary(df_obs),
    'argument "feature" is missing, with no default',
    fixed = TRUE
  )
})

test_that("map_summary() returns error for invalid or more than one feature", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  # More than one feature passed
  expect_error(
    map_summary(df, feature = c("n_events", "sum_count")),
    "`feature` must be a character of length 1.",
    fixed = TRUE
  )
  # Rename effort_duration to `no_feature` to simulate invalid feature
  df <- df %>%
    dplyr::rename(no_feature = n_observations)
  expect_error(
    map_summary(df, feature = "no_feature"),
    paste0(
      "Invalid features in the summary: `no_feature`.\n",
      "Valid features from `summarize_observations()`: `n_scientificName`, ",
      "`n_events`, `n_observations`, `sum_count`, `rai_observations` and ",
      "`rai_count`.\n",
      "Valid features from `summarize_deployments()`: `effort_duration`."
    ),
    fixed = TRUE
  )
  # Feature is correct but not present in the summary
  df <- example_dataset() %>%
    summarize_observations()
  expect_error(
    map_summary(df, feature = "effort_duration"),
    paste0(
      "`feature` 'effort_duration' not found in `df`. ",
      "Based on your `df`, possible values for `feature` are: ",
      "`n_scientificName`, `n_events`, `n_observations`, `sum_count`, ",
      "`rai_observations` and `rai_count`"
    )
  )
})

test_that("map_summary() warns if effort_unit is used with wrong feature", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  expect_warning(
    map_summary(df, feature = "n_observations", effort_unit = "month"),
    "`effort_unit` ignored for `feature = n_observations`.",
    fixed = TRUE
  )
})

test_that("map_summary() returns a leaflet", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  expect_s3_class(
    map_summary(df, feature = "n_observations"),
    c("leaflet", "htmlwidget")
  )
})

test_that("map_summary() returns a leaflet with the right title", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  map <- map_summary(df, feature = "n_observations")
  expect_equal(
    map$x$calls[[4]]$args[[1]]$title,
      "Number of observations"
  )
  map <- map_summary(df, feature = "n_scientificName")
  expect_equal(map$x$calls[[5]]$args[[1]]$title, "Number of detected species")
  map <- map_summary(df, feature = "sum_count")
  expect_equal(map$x$calls[[5]]$args[[1]]$title, "Sum of individual counts")
  map <- map_summary(df, feature = "rai_observations")
  expect_equal(map$x$calls[[4]]$args[[1]]$title, "RAI")
  map <- map_summary(df, feature = "rai_count")
  expect_equal(map$x$calls[[5]]$args[[1]]$title, "RAI (individual counts)")
  df <- example_dataset() %>%
    summarize_deployments()
  df
  map <- map_summary(df, feature = "effort_duration")
  expect_equal(
    map$x$calls[[4]]$args[[1]]$title,
    "Effort (hour)"
  )
  map <- map_summary(df, feature = "effort_duration", effort_unit = "day")
  expect_equal(
    map$x$calls[[4]]$args[[1]]$title,
    "Effort (day)"
  )
  map <- map_summary(df, feature = "effort_duration", effort_unit = "week")
  expect_equal(
    map$x$calls[[4]]$args[[1]]$title,
    "Effort (week)"
  )
  map <- map_summary(df, feature = "effort_duration", effort_unit = "month")
  expect_equal(
    map$x$calls[[4]]$args[[1]]$title,
    "Effort (month)"
  )
  map <- map_summary(df, feature = "effort_duration", effort_unit = "year")
  expect_equal(
    map$x$calls[[4]]$args[[1]]$title,
    "Effort (year)"
  )
})

test_that("map_summary() can toggle showing deployments with zero values", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  # expect an error when the toggle has length > 1
  expect_error(
    map_summary(
      df,
      feature = "n_observations",
      zero_values_show = c(TRUE, TRUE)),
    "zero_values_show must be a logical of length 1: TRUE or FALSE.",
    fixed = TRUE
  )
  # expect an error when the toggle is not TRUE or FALSE
  expect_error(
    map_summary(df, feature = "n_observations",
                       zero_values_show = "dax"),
               "zero_values_show must be a logical of length 1: TRUE or FALSE.",
               fixed = TRUE)
  expect_error(
    map_summary(df, feature = "n_observations", zero_values_show = NA),
    "zero_values_show must be a logical of length 1: TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    map_summary(df, feature = "n_observations", zero_values_show = NULL),
    "zero_values_show must be a logical of length 1: TRUE or FALSE.",
    fixed = TRUE
  )
  # expect a message when an url/size is provided but the toggle is off
  suppressMessages(expect_message(
    map_summary(df, feature = "n_observations", zero_values_show = FALSE),
    "`zero_values_icon_url` is ignored because `zero_values_show` is FALSE.",
    fixed = TRUE
  ))
  
  suppressMessages(expect_message(
    map_summary(df, feature = "n_observations", zero_values_show = FALSE),
    "`zero_values_icon_size` is ignored because `zero_values_show` is FALSE.",
    fixed = TRUE
  ))
  
  expect_no_message(
    map_summary(df, feature = "n_observations", zero_values_show = TRUE)
  )
})

test_that("map_summary() can toggle showing deployments with NA values", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  # expect an error when the toggle has length > 1
  expect_error(
    map_summary(df, feature = "n_observations", na_values_show = c(TRUE, TRUE)),
    "na_values_show must be a logical of length 1: TRUE or FALSE.",
    fixed = TRUE
  )
  # expect an error when the toggle is not TRUE or FALSE
  expect_error(
    map_summary(df, feature = "n_observations", na_values_show = "dax"),
    "na_values_show must be a logical of length 1: TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    map_summary(df, feature = "n_observations", na_values_show = NA),
    "na_values_show must be a logical of length 1: TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    map_summary(df, feature = "n_observations", na_values_show = NULL),
    "na_values_show must be a logical of length 1: TRUE or FALSE.",
    fixed = TRUE
  )
  # expect a message when an url/size is provided but the toggle is off
  suppressMessages(expect_message(
    map_summary(df, feature = "n_observations", na_values_show = FALSE),
    "`na_values_icon_url` is ignored because `na_values_show` is FALSE.",
    fixed = TRUE
  ))
  suppressMessages(expect_message(
    map_summary(df, feature = "n_observations", na_values_show = FALSE),
    "`na_values_icon_size` is ignored because `na_values_show` is FALSE.",
    fixed = TRUE
  ))
  
  expect_no_message(
    map_summary(df,feature = "n_observations", na_values_show = TRUE)
  )
  
})

test_that("map_summary() allows for scale modifications", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  expect_no_warning(
    map_summary(
      df,
      feature = "n_observations",
      max_scale = 0,
      relative_scale = FALSE
    )
  )
  expect_warning(
    map_summary(df, feature = "n_observations", max_scale = 1),
    "Relative scale used: `max_scale` value ignored.",
    fixed = TRUE
  )
  expect_error(
    map_summary(df, feature = "n_observations", relative_scale = FALSE),
    "If you use an absolute scale, `max_scale` must be a number, not `NULL`.",
    fixed = TRUE
  )
})

test_that("map_summary() allows disabling of hover columns", {
  skip_if_offline()
  df <- example_dataset() %>%
    summarize_observations()
  map_no_hover <- map_summary(
    df,
    feature = "n_observations",
    hover_columns = NULL
  )

  expect_true(
    all(
      is.na(map_no_hover$x$calls[[3]]$args[[11]])
    )
  )
  map_hover <- map_summary(
    df,
    feature = "sum_count",
    hover_columns = "scientificName"
  )
  expect_true(
    all(
        stringr::str_detect(
          map_hover$x$calls[[4]]$args[[11]],
          "<p>Scientific name: (.*?)</p>"
        )
    )
  )
})

test_that("map_dep() is deprecated", {
  skip_if_offline()
  x <- example_dataset()
  lifecycle::expect_deprecated(map_dep(x, "n_observations"))
})

test_that("output of map_dep() is the same as map_summary()", {
  skip_if_offline()
  x <- example_dataset()
  df <- x %>%
    summarize_observations(extend = TRUE)
  expect_identical(
    suppressWarnings(
      map_dep(x,
              "n_observations",
              hover_columns = c("deploymentID",
                                "latitude",
                                "longitude",
                                "scientificName",
                                "n_observations")
      )
    ),
    map_summary(df, "n_observations")
  )
  # The leaflet returned by `map_dep()` without showing zero/NA values is the
  # same as the one returned by `map_summary()` without showing zero/NA values
  df <- x %>%
    filter_observations(
      scientificName %in% c("Anas platyrhynchos", "Anas strepera")
    ) %>%
    summarize_observations()
  expect_identical(
    suppressWarnings(
      map_dep(x,
              "n_obs", # the old name of the feature `n_observations`
              species = c("Anas platyrhynchos", "Anas strepera"),
              hover_columns = c("deploymentID",
                                "latitude",
                                "longitude",
                                "scientificName",
                                "n"),
              zero_values_show = FALSE,
              zero_values_icon_url = NULL,
              zero_values_icon_size = NULL
      )
    ),
    map_summary(df,
                "n_observations",
                zero_values_show = FALSE,
                zero_values_icon_url = NULL,
                zero_values_icon_size = NULL
    )
  )
  df <- x %>%
    filter_observations(
      scientificName %in% c("Anas platyrhynchos", "Anas strepera")
    ) %>%
    summarize_observations(
      group_by = c("deploymentID", "latitude", "longitude")
    )
  expect_identical(
    suppressWarnings(
      map_dep(x,
              "n_species", # the old name of the feature `n_scientificName`
              species = c("Anas platyrhynchos", "Anas strepera"),
              hover_columns = c("deploymentID",
                                "latitude",
                                "longitude",
                                "n"),
              na_values_show = FALSE,
              na_values_icon_url = NULL,
              na_values_icon_size = NULL,
              zero_values_show = FALSE,
              zero_values_icon_url = NULL,
              zero_values_icon_size = NULL
      )
    ),
    map_summary(df,
                "n_scientificName",
                na_values_show = FALSE,
                na_values_icon_url = NULL,
                na_values_icon_size = NULL,
                zero_values_show = FALSE,
                zero_values_icon_url = NULL,
                zero_values_icon_size = NULL
    )
  )
})

test_that(
  "Check all three deprecations: function and args `sex` and `life_stage`", {
    skip_if_offline()
    x <- example_dataset()
    warnings <- list()
    # Using the (superseded) `capture_warnings()` doesn't retain the
    # class of the warnings. Still, not worth to go more complex
    warnings <- capture_warnings(
      rlang::with_options(
        lifecycle_verbosity = "warning",
        suppressMessages(
          map_dep(x, "n_observations", sex = "female", life_stage = "adult")
        )
      )
    )
    
    # Test length of the warnings
    expect_length(warnings, 3)
    
    # Test the content of the warnings (partial match is enough)
    expect_match(
      warnings[[1]],
      "`map_dep\\(\\)` was deprecated in camtraptor 1.0.0.",
      all = FALSE
    )
    expect_match(
      warnings[[2]],
      paste0("The `sex` argument of `map_dep\\(\\)` ", 
             "is deprecated as of camtraptor 1.0.0."),
      all = FALSE
    )
    expect_match(
      warnings[[3]],
      paste0("The `life_stage` argument of `map_dep\\(\\)` ",
             "is deprecated as of camtraptor 1.0.0."),
      all = FALSE
    )
  })
