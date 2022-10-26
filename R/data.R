#' Study site activation data
#'
#' Data on when study sites activated each study protocol and what domains
#' where available at the site.
#'
#' @format ## `site_activation`
#'
#' A data frame:
#' \describe{
#'   \item{state}{Site state}
#'   \item{site}{Site code}
#'   \item{health_service}{Health service}
#'   \item{active_v3}{Start date on protocol version 3}
#'   \item{active_v5}{Start date on protocol version 5}
#'   \item{country}{Country of site}
#' }
"site_activation"
