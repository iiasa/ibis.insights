#' Clamp stars to a specific bounds or range
#'
#' @description
#' This small helper function simply clamps a given [`SpatRaster`] or [`stars`] object
#' to a provided range. This can help to adjust for example land-use fractions for use
#' in `insights_fraction()`.
#' @param env The original variable stacks as [`SpatRaster`] or [`stars`].
#' @param lb The lower bound for clamping (Default: \code{-Inf}).
#' @param ub The upper bound for clamping (Default: \code{Inf}).
#'
#' @returns The same as input \code{'env'}.
#' @examples
#' \dontrun{
#'   # Use
#' }
#'
#' @author Martin Jung
#' @keywords utils
st_clamp <- function(env, lb = -Inf, ub = Inf){
  assertthat::assert_that(
    ibis.iSDM::is.Raster(env) || inherits(env, "stars"),
    # Check numeric
    is.numeric(lb),
    is.numeric(ub),
    lb < ub,
    lb != ub
  )

  # Apply lower and upper bounds if set and finite
  if(is.finite(lb)){
    env[env < lb] <- lb
  }
  if(is.finite(ub)){
    env[env > ub] <- ub
  }

  # Checks and return
  assertthat::assert_that(
    ibis.iSDM::is.Raster(env) || inherits(env, "stars")
  )
  return(env)
}
