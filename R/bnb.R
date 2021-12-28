#' Title
#'
#' @param dates
#'
#' @return
#'
#'
#' @examples
bnb <- function(dates) {
  data.frame(date = dates,
             weekday = weekdays(dates)) %>%
    dplyr::mutate(year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = lubridate::day(date)) %>%
    dplyr::filter(month == 12) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(day <= 25) %>%
    dplyr::filter(weekday == "Sonntag") %>%
    dplyr::filter(day == max(day)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = date - 32,
                  weekday = weekdays(date)) %>%
    dplyr::select(date) %>%
    dplyr::mutate(level = "Sachsen")
}
