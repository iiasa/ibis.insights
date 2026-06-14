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

#' Apply an elevation suitability mask to a species projection
#'
#' @description
#' Align an elevation suitability mask to a species projection and multiply the
#' species values by the mask. The output follows the class, geometry, layer
#' count, and temporal dimension of \code{species_projection}.
#'
#' @param elevation_mask A [`terra::SpatRaster`] or [`stars`] object containing
#'   suitability weights in \code{[0, 1]}, typically from
#'   \code{\link{create_elevation_mask}()}.
#' @param species_projection A [`terra::SpatRaster`] or [`stars`] species
#'   projection to be masked.
#'
#' @returns A masked species projection with the same class as
#'   \code{species_projection}.
#'
#' @details
#' The mask is reprojected, cropped, and resampled to the species grid when
#' needed. Static masks are repeated across temporal species projections. When
#' both inputs are same-class temporal objects, mask time steps are aligned to
#' the species projection with \code{\link{align_temporal}()}.
#'
#' @examples
#' require(terra)
#' species <- terra::rast(nrow = 1, ncol = 3, vals = c(0.2, 0.6, 1))
#' mask <- terra::rast(nrow = 1, ncol = 3, vals = c(0, 0.5, 1))
#' apply_elevation_mask(mask, species)
#'
#' @author Martin Jung
#' @keywords utils
#' @export
apply_elevation_mask <- function(elevation_mask, species_projection) {
  mask_is_raster <- inherits(elevation_mask, "SpatRaster")
  mask_is_stars <- inherits(elevation_mask, "stars")
  species_is_raster <- inherits(species_projection, "SpatRaster")
  species_is_stars <- inherits(species_projection, "stars")

  assertthat::assert_that(
    mask_is_raster || mask_is_stars,
    species_is_raster || species_is_stars,
    msg = "elevation_mask and species_projection must be SpatRaster or stars objects."
  )

  if(species_is_stars) {
    assertthat::assert_that(
      length(species_projection) == 1,
      msg = "species_projection stars objects must have one attribute."
    )
    species_dims <- stars::st_dimensions(species_projection)
    species_time_dim <- which(names(species_dims) %in% c("time", "Time"))
    if(length(species_time_dim) > 1) {
      stop("species_projection has more than one time dimension.", call. = FALSE)
    }
    if(length(species_time_dim) == 1) {
      names(species_dims)[species_time_dim] <- "time"
      stars::st_dimensions(species_projection) <- species_dims
    }
  }

  if(mask_is_stars) {
    mask_dims <- stars::st_dimensions(elevation_mask)
    mask_time_dim <- which(names(mask_dims) %in% c("time", "Time"))
    if(length(mask_time_dim) > 1) {
      stop("elevation_mask has more than one time dimension.", call. = FALSE)
    }
    if(length(mask_time_dim) == 1) {
      names(mask_dims)[mask_time_dim] <- "time"
      stars::st_dimensions(elevation_mask) <- mask_dims
    }
  }

  if(mask_is_stars && species_is_stars &&
     "time" %in% names(stars::st_dimensions(elevation_mask)) &&
     "time" %in% names(stars::st_dimensions(species_projection))) {
    mask_time <- stars::st_get_dimension_values(elevation_mask, "time")
    species_time <- stars::st_get_dimension_values(species_projection, "time")
    if(length(mask_time) != length(species_time) ||
       !identical(as.character(mask_time), as.character(species_time))) {
      elevation_mask <- align_temporal(elevation_mask, species_projection)
    }
  }
  if(mask_is_raster && species_is_raster) {
    mask_time <- terra::time(elevation_mask)
    species_time <- terra::time(species_projection)
    mask_has_time <- terra::nlyr(elevation_mask) > 1 &&
      length(mask_time) == terra::nlyr(elevation_mask) && any(!is.na(mask_time))
    species_has_time <- terra::nlyr(species_projection) > 1 &&
      length(species_time) == terra::nlyr(species_projection) && any(!is.na(species_time))
    if(mask_has_time && species_has_time &&
       (length(mask_time) != length(species_time) ||
        !identical(as.character(mask_time), as.character(species_time)))) {
      elevation_mask <- align_temporal(elevation_mask, species_projection)
    }
  }

  species_rast <- if(species_is_stars) {
    terra::rast(species_projection)
  } else {
    species_projection
  }
  mask_rast <- if(mask_is_stars) {
    terra::rast(elevation_mask)
  } else {
    elevation_mask
  }

  if(!terra::same.crs(species_rast, mask_rast)) {
    assertthat::assert_that(
      nzchar(terra::crs(species_rast)) && nzchar(terra::crs(mask_rast)),
      msg = "Both inputs must have a CRS when reprojection is needed."
    )
    mask_rast <- terra::project(mask_rast, terra::crs(species_rast))
  }
  if(!terra::compareGeom(species_rast, mask_rast, stopOnError = FALSE)) {
    mask_rast <- terra::crop(mask_rast, species_rast)
    mask_rast <- terra::resample(mask_rast, species_rast, method = "average", threads = TRUE)
  }

  mask_range <- terra::global(mask_rast, "range", na.rm = TRUE)
  assertthat::assert_that(
    all(mask_range[["min"]] >= 0),
    all(mask_range[["max"]] <= 1),
    msg = "elevation_mask must contain suitability weights in [0, 1]."
  )

  species_time <- terra::time(species_rast)
  if(terra::nlyr(mask_rast) == 1 && terra::nlyr(species_rast) > 1) {
    mask_rast <- do.call(c, rep(list(mask_rast), terra::nlyr(species_rast)))
    if(length(species_time) == terra::nlyr(species_rast) && any(!is.na(species_time))) {
      terra::time(mask_rast) <- species_time
    }
  } else if(terra::nlyr(mask_rast) != terra::nlyr(species_rast)) {
    stop(
      "elevation_mask must have one layer or the same temporal layers as species_projection.",
      call. = FALSE
    )
  }

  out <- species_rast * mask_rast
  names(out) <- names(species_rast)
  if(length(species_time) == terra::nlyr(out) && any(!is.na(species_time))) {
    terra::time(out) <- species_time
  }

  if(species_is_stars) {
    out <- stars::st_as_stars(out, crs = sf::st_crs(species_projection))
    stars::st_dimensions(out) <- stars::st_dimensions(species_projection)
    names(out) <- names(species_projection)
  }

  out
}
