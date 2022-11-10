#' Title
#'
#' @param eastern
#'
#' @return
#' @export
#'
#' @examples
easter_deduct_federal <- function(eastern) {
  easter_m <- eastern + 1
  easter_f <- eastern - 2
  ch <- eastern + 39
  pm <- eastern + 50

  data.frame(date = c(easter_m, easter_f, ch, pm),
             level = "federal")
}

#' Title
#'
#' @param eastern
#'
#' @return
#' @export
#'
#' @examples
easter_deduct_state <- function(eastern) {
  data.frame(date = rep(eastern + 60, 6),
                   level = c(rep("Bayern", length(eastern)),
                             rep("Baden-Württemberg", length(eastern)),
                             rep("Hessen", length(eastern)),
                             rep("NRW", length(eastern)),
                             rep("Rheinland-Pfalz", length(eastern)),
                             rep("Saarland", length(eastern))))
}
