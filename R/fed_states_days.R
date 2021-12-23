#' Title
#'
#' @param days
#'
#' @return
#'
#'
#' @examples
federal_states_days <- function(days) {
  states <- c("Schleswig-Holstein",
              "Mecklenburg-vorpommern",
              "Brandenburg",
              "Bremen",
              "Hamburg",
              "Berlin",
              "Sachsen",
              "Sachsen-Anhalt",
              "Thüringen",
              "Niedersachsen",
              "Hessen",
              "NRW",
              "Rheinland-Pfalz",
              "Saarland",
              "Baden-Württemberg",
              "Bayern")

  expand.grid(days, states) %>%
    dplyr::rename(date = Var1, level = Var2)
}
