#' Title
#'
#' @param Y
#' @param state_level
#'
#' @return
#' @export
#'
#' @examples
workdays <- function(Y, state_level = FALSE) {
  start <- paste0("01-01-", min(Y))
  end <- paste0("31-12-", max(Y))
  all_days <- init_dates(start = start, end = end)

  without_we <- data.frame(date = all_days) %>%
    dplyr::mutate(weekday = weekdays(date)) %>%
    dplyr::filter(!weekday %in% c("Samstag", "Sonntag"))

  fixed_fed <- fixed_federal_holidays(Y)

  fixed_state <- fixed_state_holidays(Y)

  eastern <- gauss_easter(Y)

  eastern_ded_fed <- easter_deduct_federal(eastern = eastern)

  eastern_ded_state <- easter_deduct_state(eastern = eastern)

  bnb <-bnb(dates = all_days)



  if(state_level == FALSE) {
    without_we %>%
      dplyr::anti_join(fixed_fed, by = "date") %>%
      dplyr::anti_join(eastern_ded_fed, by = "date") %>%
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    week = lubridate::week(date),
                    quarter = lubridate::quarter(date),
                    day = lubridate::day(date))
  } else {
    fed_wd <- without_we %>%
      dplyr::anti_join(fixed_fed, by = "date") %>%
      dplyr::anti_join(eastern_ded_fed, by = "date")

    fed_states <- federal_states_days(fed_wd$date)

    fed_states %>%
      dplyr::anti_join(fixed_state, by = c("date", "level")) %>%
      dplyr::anti_join(eastern_ded_state, by = c("date", "level")) %>%
      dplyr::anti_join(bnb, by = c("date", "level")) %>%
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    week = lubridate::week(date),
                    quarter = lubridate::quarter(date),
                    day = lubridate::day(date))
  }
}


#' Title
#'
#' @param Y
#' @param state_level
#'
#' @return
#' @export
#'
#' @examples
holidays <- function(Y, state_level = FALSE) {
  start <- paste0("01-01-", min(Y))
  end <- paste0("31-12-", max(Y))
  fed_hol <- dplyr::bind_rows(fixed_federal_holidays(Y),
                       easter_deduct_federal(eastern = gauss_easter(Y)),
                       data.frame(date = gauss_easter(Y), level = "federal"))
  all_days <- init_dates(start = start, end = end)


  if(state_level == FALSE) {
    fed_hol
  } else {
  fixed_fed_states <- federal_states_days(days = fed_hol$date)

  dplyr::bind_rows(fixed_fed_states,
                   fixed_state_holidays(Y),
                   easter_deduct_state(eastern = gauss_easter(Y)),
                   bnb(dates = all_days)) %>%
    dplyr::mutate(year = lubridate::year(date),
                  month = lubridate::month(date),
                  week = lubridate::week(date),
                  quarter = lubridate::quarter(date),
                  day = lubridate::day(date))

  }
}


#' Title
#'
#' @param Y
#'
#' @return
#' @export
#'
#' @examples
federal_workdays_weighted <- function(Y) {
  workdays(Y, state_level = T) %>%
    dplyr::group_by(date) %>%
    dplyr::count() %>%
    dplyr::mutate(level = "federal",
                  weighted_workdays = n/16) %>%
    dplyr::select(date, weighted_workdays, level) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = lubridate::year(date),
                  month = lubridate::month(date),
                  week = lubridate::week(date),
                  quarter = lubridate::quarter(date),
                  day = lubridate::day(date))
}
