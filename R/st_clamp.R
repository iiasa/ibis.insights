#' Clamp raster values to specific bounds
#'
#' @description
#' Clamp all cell values in a [`terra::SpatRaster`] or [`stars`] object to a
#' provided numeric interval. Values below the lower bound are set to the lower
#' bound, and values above the upper bound are set to the upper bound. Missing
#' values are preserved.
#'
#' This is useful for keeping suitability, fractional land-use, or habitat
#' condition layers inside valid ranges before they are passed to functions such
#' as [`insights_fraction()`].
#'
#' @param env A [`terra::SpatRaster`] or [`stars`] object.
#' @param lb A single [`numeric`] lower bound for clamping. Defaults to
#'   \code{-Inf}, which leaves the lower tail unchanged.
#' @param ub A single [`numeric`] upper bound for clamping. Defaults to
#'   \code{Inf}, which leaves the upper tail unchanged.
#' @param lower Alias for \code{lb}.
#' @param upper Alias for \code{ub}.
#'
#' @returns An object of the same class as \code{env}, with values clamped to
#'   \code{[lb, ub]}.
#'
#' @details
#' For [`terra::SpatRaster`] inputs, \code{st_clamp()} delegates to
#' [`terra::clamp()`]. For [`stars`] inputs, each attribute array is clamped
#' directly with [`pmax()`] and [`pmin()`], preserving the original dimensions
#' and attribute names.
#' @examples
#' require(terra)
#' r <- terra::rast(nrow = 2, ncol = 2, vals = c(-0.2, 0.4, 1.2, NA))
#' st_clamp(r, lower = 0, upper = 1)
#'
#' require(stars)
#' s <- stars::st_as_stars(r)
#' st_clamp(s, lower = 0, upper = 1)
#'
#' @author Martin Jung
#' @keywords utils
#' @export
st_clamp <- function(env, lb = -Inf, ub = Inf, lower = lb, upper = ub) {
  if(!missing(lower)) {
    lb <- lower
  }
  if(!missing(upper)) {
    ub <- upper
  }

  is_spatraster <- inherits(env, "SpatRaster")
  is_stars <- inherits(env, "stars")

  assertthat::assert_that(
    is_spatraster || is_stars,
    is.numeric(lb),
    length(lb) == 1,
    !is.na(lb),
    is.numeric(ub),
    length(ub) == 1,
    !is.na(ub),
    lb < ub,
    msg = "env must be a SpatRaster or stars object, and lb/lower must be smaller than ub/upper."
  )

  if(is_spatraster) {
    return(
      terra::clamp(env, lower = lb, upper = ub, values = TRUE)
    )
  }

  for(attr in names(env)) {
    assertthat::assert_that(
      is.numeric(env[[attr]]),
      msg = "All stars attributes must be numeric to be clamped."
    )
    if(is.finite(lb)) {
      env[[attr]] <- pmax(env[[attr]], lb)
    }
    if(is.finite(ub)) {
      env[[attr]] <- pmin(env[[attr]], ub)
    }
  }

  assertthat::assert_that(
    inherits(env, "stars")
  )
  env
}
