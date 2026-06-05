#' Relative change function
#'
#' @description
#' This function calculates the relative change of a vector, taking the first value as
#' a reference value.
#' @param v A [`numeric`] vector with length greater than 1.
#' @param fac A [`numeric`] constant multiplier on the resulting metric.
#' @returns A [`numeric`] vector.
#' @examples
#' # Example vector
#' x <- c(20,6,2,1,15,25)
#' relChange(x)
#'
#' @author Martin Jung
#' @keywords utils
#' @export
relChange <- function(v, fac = 100){
  assertthat::assert_that(
    length(v)>1,
    is.numeric(v),
    is.numeric(fac)
  )
  if(v[1] == 0) {
    warning("Reference value (first element) is 0; returning NA for relative change.")
    return(rep(NA_real_, length(v)))
  }

  return(
    (((v-v[1]) / v[1]) * fac)
  )
}

#' Symmetric relative difference function
#'
#' @description
#' Computes the symmetric relative difference of a numeric vector with respect
#' to its first element. Unlike the standard relative change
#' (\code{\link{relChange}}), this metric is bounded in \eqn{[-1, 1]} and
#' remains well-defined when the baseline value is small.
#'
#' @details
#' The symmetric relative difference is defined as:
#'
#' \deqn{D_{sym}(t) = \frac{x_t - x_0}{x_t + x_0}}
#'
#' This formulation is preferred over the standard relative change when:
#' \itemize{
#'   \item The baseline value \eqn{x_0} is small, causing the standard formula
#'         to produce arbitrarily large values (common for rare species or
#'         freshly colonised habitat).
#'   \item Symmetric treatment of gains and losses is required: a change from
#'         \eqn{a} to \eqn{b} has the same magnitude (opposite sign) as from
#'         \eqn{b} to \eqn{a}.
#'   \item A bounded, directly comparable index across species or regions with
#'         very different baseline areas is needed.
#' }
#'
#' @param v A [`numeric`] vector with length greater than 1 and first element > 0.
#' @returns A [`numeric`] vector of symmetric relative differences, bounded in
#'   \eqn{[-1, 1]}. Returns \code{NA} where the denominator
#'   \eqn{x_t + x_0 = 0}.
#' @examples
#' x <- c(20, 6, 2, 1, 15, 25)
#' relChangeSym(x)
#'
#' @author Martin Jung
#' @keywords utils
#' @export
relChangeSym <- function(v) {
  assertthat::assert_that(
    length(v) > 1,
    is.numeric(v)
  )
  assertthat::assert_that(
    v[1] > 0,
    msg = "Reference value (first element) must be > 0 for symmetric relative difference."
  )
  denom <- v + v[1]
  ifelse(denom == 0, NA_real_, (v - v[1]) / denom)
}
