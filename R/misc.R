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
