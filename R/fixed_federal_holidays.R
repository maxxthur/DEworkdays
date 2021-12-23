#' Title
#'
#' @return
#'
#'
#' @examples
fixed_federal_holidays <- function(Y) {

  days <- c("01-01",
            "03-10",
            "25-12",
            "26-12")

  dates <- expand.grid(days, Y) %>%
    dplyr::mutate(date = lubridate::dmy(paste(Var1, Var2, sep = "-"))) %>%
    .$date

  data.frame(date = dates, level = "federal")
}
