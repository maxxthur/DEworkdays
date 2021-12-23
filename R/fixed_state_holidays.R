#' Title
#'
#' @param Y
#'
#' @return
#'
#'
#' @examples
fixed_state_holidays <- function(Y) {
  days <- c("08-03",
            "15-08",
            "20-09",
            rep("31-10", 9),
            rep("01-11", 5),
            rep("06-01", 3))

  dates <- expand.grid(days, Y) %>%
    dplyr::mutate(date = lubridate::dmy(paste(Var1, Var2, sep = "-"))) %>%
    .$date

  level <- c("Berlin",
             "Saarland",
             "Thüringen",
             "Brandenburg",
             "Bremen",
             "Hamburg",
             "Mecklenburg-Vorpommern",
             "Niedersachsen",
             "Sachsen",
             "Sachsen-Anhalt",
             "Schleswig-Holstein",
             "Thüringen",
             "Baden-Württemberg",
             "Bayern",
             "NRW",
             "Rheinland-Pfalz",
             "Saarland",
             "Bayern",
             "Baden-Württemberg",
             "Sachsen-Anhalt")


  data.frame(date = dates,
             level = rep(level, length(Y)))
}
