#' Select a species name
#'
#' Presents a table of species names with observation count for each
#' and allows the user to interactively select one.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @return A character string, scientific species name.
#' @family density estimation functions
#' @export
select_species <- function(package){
  tab <-  get_species(package)
  tab <- tab[, grepl("Name", names(tab))]
  n <- table(package$data$observations$scientificName)
  tab$n_observations <- n
  
  print.data.frame(tab)
  i <- NA
  while(is.na(i) || i<1 || i>nrow(tab))
    i <- readline("Enter row number of species to analyse: ") %>%
    as.numeric() %>%
    suppressWarnings()
  as.character(tab$scientificName[i])
}