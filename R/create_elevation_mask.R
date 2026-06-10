#' Create an elevation suitability mask
#'
#' @description
#' Convert a digital elevation model (DEM) to a suitability mask from a
#' species-specific preferred elevation interval. Values inside the preferred
#' interval receive a value of 1. Values outside the interval can either be set
#' to 0 directly or down-weighted with a linear or negative exponential cutoff.
#'
#' @param dem A [`terra::SpatRaster`] or [`stars`] object containing elevation
#'   values.
#' @param elevation_range A two-element [`numeric`] vector giving the minimum
#'   and maximum suitable elevation, in the same units as \code{dem}.
#' @param cutoff A [`character`] cutoff type. One of \code{"binary"},
#'   \code{"linear"}, \code{"negative_exponential"}, or \code{"exponential"}.
#'   \code{"exponential"} is accepted as an alias for
#'   \code{"negative_exponential"}.
#' @param tolerance A single positive [`numeric`] value, in the same units as
#'   \code{dem}, describing the uncertainty distance outside
#'   \code{elevation_range}. Required for \code{"linear"} and
#'   \code{"negative_exponential"} cutoffs. It is ignored for
#'   \code{"binary"}.
#'
#' @returns An object of the same class, dimensions, and attribute names as
#'   \code{dem}, with values in \code{[0, 1]}. Missing elevation values are
#'   preserved.
#'
#' @details
#' For soft cutoffs, the threshold distance \eqn{d} is the elevation distance
#' to the closest threshold: \eqn{d = 0} inside \code{elevation_range},
#' \eqn{min - x} below the lower threshold, and \eqn{x - max} above the upper
#' threshold. The binary cutoff uses direct threshold comparisons and does not
#' construct an intermediate distance raster. Soft cutoffs evaluate lower and
#' upper threshold distances only for cells outside the preferred interval.
#'
#' The supported cutoffs are:
#' \itemize{
#'   \item \code{"binary"}: \eqn{1} inside the preferred range and \eqn{0}
#'         outside it.
#'   \item \code{"linear"}: \eqn{max(0, 1 - d / tolerance)}.
#'   \item \code{"negative_exponential"}: \eqn{exp(-d / tolerance)}.
#' }
#'
#' @examples
#' require(terra)
#'
#' dem <- terra::rast(
#'   nrow = 1,
#'   ncol = 6,
#'   vals = c(100, 150, 200, 250, 300, 350)
#' )
#'
#' create_elevation_mask(dem, elevation_range = c(200, 300))
#' create_elevation_mask(
#'   dem,
#'   elevation_range = c(200, 300),
#'   cutoff = "linear",
#'   tolerance = 100
#' )
#' create_elevation_mask(
#'   dem,
#'   elevation_range = c(200, 300),
#'   cutoff = "negative_exponential",
#'   tolerance = 100
#' )
#'
#' require(stars)
#' create_elevation_mask(stars::st_as_stars(dem), c(200, 300))
#'
#' @author Martin Jung
#' @keywords utils
#' @name create_elevation_mask
#' @export
NULL

#' @name create_elevation_mask
#' @rdname create_elevation_mask
#' @exportMethod create_elevation_mask
#' @export
methods::setGeneric(
  "create_elevation_mask",
  signature = methods::signature("dem"),
  function(
    dem,
    elevation_range,
    cutoff = c("binary", "linear", "negative_exponential", "exponential"),
    tolerance = NULL
  ) standardGeneric("create_elevation_mask")
)

#' @name create_elevation_mask
#' @rdname create_elevation_mask
#' @aliases create_elevation_mask,SpatRaster-method
#' @usage \S4method{create_elevation_mask}{SpatRaster}(dem,elevation_range,cutoff,tolerance)
methods::setMethod(
  "create_elevation_mask",
  methods::signature(dem = "SpatRaster"),
  function(
    dem,
    elevation_range,
    cutoff = c("binary", "linear", "negative_exponential", "exponential"),
    tolerance = NULL
  ) {
    assertthat::assert_that(
      is.numeric(elevation_range),
      length(elevation_range) == 2,
      all(is.finite(elevation_range)),
      elevation_range[1] <= elevation_range[2],
      msg = "elevation_range must be a finite numeric vector c(min, max)."
    )

    cutoff <- match.arg(
      cutoff,
      choices = c("binary", "linear", "negative_exponential", "exponential"),
      several.ok = FALSE
    )
    if(cutoff == "exponential") {
      cutoff <- "negative_exponential"
    }

    if(cutoff == "binary") {
      if(!is.null(tolerance)) {
        assertthat::assert_that(
          is.numeric(tolerance),
          length(tolerance) == 1,
          !is.na(tolerance),
          tolerance >= 0,
          msg = "tolerance must be a single non-negative numeric value."
        )
      }
    } else {
      assertthat::assert_that(
        !is.null(tolerance),
        is.numeric(tolerance),
        length(tolerance) == 1,
        is.finite(tolerance),
        tolerance > 0,
        msg = "tolerance must be a single positive numeric value for soft cutoffs."
      )
    }

    elev_min <- elevation_range[1]
    elev_max <- elevation_range[2]

    if(cutoff == "binary") {
      out <- (dem >= elev_min & dem <= elev_max) * 1
    } else {
      out <- terra::app(dem, fun = function(x) {
        out <- x
        storage.mode(out) <- "double"
        out[] <- 1
        out[is.na(x)] <- NA_real_

        below <- !is.na(x) & x < elev_min
        above <- !is.na(x) & x > elev_max

        if(cutoff == "linear") {
          out[below] <- pmax(0, 1 - (elev_min - x[below]) / tolerance)
          out[above] <- pmax(0, 1 - (x[above] - elev_max) / tolerance)
        } else {
          out[below] <- exp(-(elev_min - x[below]) / tolerance)
          out[above] <- exp(-(x[above] - elev_max) / tolerance)
        }

        out
      })
    }
    names(out) <- names(dem)

    assertthat::assert_that(
      inherits(out, "SpatRaster")
    )
    out
  }
)

#' @name create_elevation_mask
#' @rdname create_elevation_mask
#' @aliases create_elevation_mask,stars-method
#' @usage \S4method{create_elevation_mask}{stars}(dem,elevation_range,cutoff,tolerance)
methods::setMethod(
  "create_elevation_mask",
  methods::signature(dem = "stars"),
  function(
    dem,
    elevation_range,
    cutoff = c("binary", "linear", "negative_exponential", "exponential"),
    tolerance = NULL
  ) {
    assertthat::assert_that(
      is.numeric(elevation_range),
      length(elevation_range) == 2,
      all(is.finite(elevation_range)),
      elevation_range[1] <= elevation_range[2],
      msg = "elevation_range must be a finite numeric vector c(min, max)."
    )

    cutoff <- match.arg(
      cutoff,
      choices = c("binary", "linear", "negative_exponential", "exponential"),
      several.ok = FALSE
    )
    if(cutoff == "exponential") {
      cutoff <- "negative_exponential"
    }

    if(cutoff == "binary") {
      if(!is.null(tolerance)) {
        assertthat::assert_that(
          is.numeric(tolerance),
          length(tolerance) == 1,
          !is.na(tolerance),
          tolerance >= 0,
          msg = "tolerance must be a single non-negative numeric value."
        )
      }
    } else {
      assertthat::assert_that(
        !is.null(tolerance),
        is.numeric(tolerance),
        length(tolerance) == 1,
        is.finite(tolerance),
        tolerance > 0,
        msg = "tolerance must be a single positive numeric value for soft cutoffs."
      )
    }

    elev_min <- elevation_range[1]
    elev_max <- elevation_range[2]

    for(attr in names(dem)) {
      assertthat::assert_that(
        is.numeric(dem[[attr]]),
        msg = "All stars attributes must be numeric elevation values."
      )

      x <- dem[[attr]]

      if(cutoff == "binary") {
        out <- (x >= elev_min & x <= elev_max) * 1
        out[is.na(x)] <- NA_real_
        dem[[attr]] <- out
        next
      }

      out <- array(1, dim = dim(x), dimnames = dimnames(x))
      out[is.na(x)] <- NA_real_

      below <- !is.na(x) & x < elev_min
      above <- !is.na(x) & x > elev_max

      if(cutoff == "linear") {
        out[below] <- pmax(0, 1 - (elev_min - x[below]) / tolerance)
        out[above] <- pmax(0, 1 - (x[above] - elev_max) / tolerance)
      } else {
        out[below] <- exp(-(elev_min - x[below]) / tolerance)
        out[above] <- exp(-(x[above] - elev_max) / tolerance)
      }

      dem[[attr]] <- out
    }

    assertthat::assert_that(
      inherits(dem, "stars")
    )
    dem
  }
)

#' @name create_elevation_mask
#' @rdname create_elevation_mask
#' @aliases create_elevation_mask,ANY-method
#' @usage \S4method{create_elevation_mask}{ANY}(dem,elevation_range,cutoff,tolerance)
methods::setMethod(
  "create_elevation_mask",
  methods::signature(dem = "ANY"),
  function(
    dem,
    elevation_range,
    cutoff = c("binary", "linear", "negative_exponential", "exponential"),
    tolerance = NULL
  ) {
    stop(
      "dem must be a terra::SpatRaster or stars object.",
      call. = FALSE
    )
  }
)
