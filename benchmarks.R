# benchmark dplyr and data.table (rewrite)



# reading data ------------------------------------------------------------
mica_path <-
  file.path(
    "inst",
    "extdata",
    "mica-management-of-invasive-coypu-and-muskrat-in-europe-20230206112303/")

# get colspects

readr::as.col_spec(readr::read_csv(file.path(mica_path, "media.csv")))

## readr & dplyr -----------------------------------------------------------
do_dplyr <- function() {
  media <- readr::read_csv(file.path(mica_path, "media.csv"),
                           # col_select = c(deploymentID, sequenceID),
                           show_col_types = FALSE,
                           progress = FALSE)
  depl <- readr::read_csv(file.path(mica_path, "deployments.csv"),
                          # col_select = c(locationID, deploymentID),
                          show_col_types = FALSE,
                          progress = FALSE)
  obs <- readr::read_csv(
    file.path(mica_path, "observations.csv"),
    # col_select = c(scientificName, sequenceID),
    show_col_types = FALSE,
    progress = FALSE
  )
  ### wrangling
  left_join(depl, media, by  = "deploymentID", multiple = "all") %>%
    left_join(obs, by = "sequenceID", multiple = "all") %>%
    group_by(locationID, scientificName) %>%
    tally()
}

do_dplyr_colsel <- function() {
  media <- readr::read_csv(file.path(mica_path, "media.csv"),
                           col_select = c(deploymentID, sequenceID),
                           show_col_types = FALSE,
                           progress = FALSE)
  depl <- readr::read_csv(file.path(mica_path, "deployments.csv"),
                          col_select = c(locationID, deploymentID),
                          show_col_types = FALSE,
                          progress = FALSE)
  obs <- readr::read_csv(
    file.path(mica_path, "observations.csv"),
    col_select = c(scientificName, sequenceID),
    show_col_types = FALSE,
    progress = FALSE
  )
  ### wrangling
  left_join(depl, media, by  = "deploymentID", multiple = "all") %>%
    left_join(obs, by = "sequenceID", multiple = "all") %>%
    group_by(locationID, scientificName) %>%
    tally()
}
## data.table --------------------------------------------------------------
do_dt <- function() {
  media <- data.table::fread(
    file.path(mica_path, "media.csv"),
    select = c("sequenceID","deploymentID"),
    key = c("deploymentID","sequenceID")
  )
  depl <- data.table::fread(
    file.path(mica_path, "deployments.csv"),
    select = c("locationID", "deploymentID"),
    key = "deploymentID"
  )
  obs <- data.table::fread(
    file.path(mica_path, "observations.csv"),
    select = c("scientificName", "sequenceID"),
    key = "sequenceID"
  )
  ### wrangling
  depl[media, on = "deploymentID"][
    obs, on = "sequenceID", allow.cartesian = TRUE][
      , .(n = .N), by = c("scientificName", "locationID")]
}


# benchmark ---------------------------------------------------------------


bm <- microbenchmark::microbenchmark(do_dt(),do_dplyr(), times = 5)
bm <- microbenchmark::microbenchmark(do_dplyr_colsel(),do_dplyr(), times = 5)
microbenchmark:::autoplot.microbenchmark(bm)
microbenchmark:::boxplot.microbenchmark(bm)


# filtering

filter_dt <- function(){
  dplyr::filter(do_dt(), scientificName == "Anser")
}

filter_dply <- function(){
  do_dt()[scientificName == "Anser",]
}