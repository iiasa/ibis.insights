#' Apply temporal discount to land-use layers based on an age variable
#'
#' @description
#' This function applies a temporal discount to land-use or habitat layers based
#' on a corresponding age or maturity variable. The age variable is always
#' connected to the layer being discounted (e.g. forest age linked to forest
#' fraction) and represents increasing habitat value over time.
#'
#' Newly established habitat (low age) does not provide full habitat value.
#' Three maturity parameterizations are supported:
#'
#' \strong{Target-age parameterization} (default): use this when the available
#' information is the age at which a land-use class should reach a chosen
#' fraction of its full habitat value. The \code{target_age} parameter is the
#' age at which habitat reaches \code{target} of its full value:
#'
#' \deqn{H_{\mathrm{eff}} = H \times [1 - (1 - p)^{a / a_p}]}{H_eff = H * (1 - (1 - p)^(a / a_p))}
#'
#' where \eqn{H} is the land-use value, \eqn{a} is the cell age,
#' \eqn{a_p} is \code{target_age}, and \eqn{p} is \code{target}. Internally,
#' this is equivalent to deriving the per-age discount rate:
#'
#' \deqn{d = 1 - (1 - p)^{1 / a_p}}{d = 1 - (1 - p)^(1 / a_p)}
#'
#' and applying:
#'
#' \deqn{H_{\mathrm{eff}} = H \times [1 - (1 - d)^a]}{H_eff = H * (1 - (1 - d)^a)}
#'
#' This requires a land-use layer and an age or maturity layer for the same
#' land-use class, with \code{age} and \code{target_age} in the same time unit.
#'
#' \strong{Timescale parameterization}: use this when the input age represents
#' habitat age or time since transition into suitable land use, and a species-
#' or group-specific establishment/maturation timescale is available as
#' \code{tau}:
#'
#' \deqn{Q = 1 - \exp(-a / \tau)}{Q = 1 - exp(-a / tau)}
#'
#' \deqn{H_{\mathrm{eff}} = H \times Q}{H_eff = H * Q}
#'
#' This requires a habitat or suitable land-use layer, a matching
#' \code{habitat_age} layer supplied as \code{age}, and \code{tau} in the same
#' time unit as \code{age}. When \code{tau} is supplied, \code{target_age} and
#' \code{target} are ignored. The target-age and timescale parameterizations
#' describe the same curve family, with
#' \eqn{\tau = -a_p / \log(1 - p)}.
#'
#' \strong{Smoothed-threshold parameterization}: use this when establishment
#' quality should transition smoothly around a threshold age. Supply \code{a50},
#' the habitat age at which establishment quality reaches \code{0.5}, and
#' \code{k}, the slope or steepness of the transition:
#'
#' \deqn{Q_{i,t} = \frac{1}{1 + \exp[-k_s(a_{i,t} - a_{50,s})]}}{Q_i,t = 1 / (1 + exp(-k * (a_i,t - a50)))}
#'
#' \deqn{H_{\mathrm{eff}} = H \times Q}{H_eff = H * Q}
#'
#' This requires a habitat or suitable land-use layer, a matching habitat-age
#' or time-since-transition layer supplied as \code{age}, \code{a50} in the
#' same time unit as \code{age}, and \code{k} in the inverse of that time unit.
#' When \code{a50} and \code{k} are supplied, \code{target_age} and
#' \code{target} are ignored. The \code{tau} and smoothed-threshold
#' parameterizations are mutually exclusive.
#'
#' \itemize{
#'   \item In the target-age and timescale forms, at \code{age = 0}: the
#'     factor is \code{0} -- no habitat value for brand-new land-use.
#'   \item In the target-age form, at \code{age = target_age}: the factor is
#'     \code{target}, e.g. \code{0.95} by default.
#'   \item In the timescale form, at \code{age = tau}: the factor is
#'     \code{1 - exp(-1)}, about \code{0.63}.
#'   \item In the smoothed-threshold form, at \code{age = a50}: the factor is
#'     \code{0.5}; \code{k} controls how abruptly values move from low to high.
#'   \item As \code{age} increases: the factor approaches \code{1} -- mature
#'     habitat reaches full value.
#' }
#'
#' @param lu A [`SpatRaster`] or temporal [`stars`] object of the land-use
#'   variable (e.g. forest fraction or area). Can be single or multi-layer.
#' @param age A [`SpatRaster`] or temporal [`stars`] object of the
#'   corresponding age or maturity variable (values \code{>= 0}).
#'   Must match \code{lu} in number of layers / time steps.
#' @param target_age A single positive [`numeric`] age at which habitat reaches
#'   \code{target} of full value. Used only when \code{tau = NULL} and
#'   \code{a50 = NULL}. Default: \code{20}.
#' @param target A single [`numeric`] target maturity value strictly between
#'   \code{0} and \code{1}. Used only when \code{tau = NULL} and
#'   \code{a50 = NULL}. Default: \code{0.95}.
#' @param tau Optional single positive [`numeric`] establishment/maturation
#'   timescale. If supplied, the maturity factor is calculated as
#'   \code{1 - exp(-age / tau)} and \code{target_age} / \code{target} are
#'   ignored.
#' @param a50 Optional single non-negative [`numeric`] threshold age at which
#'   establishment quality reaches \code{0.5}. Must be supplied together with
#'   \code{k}. Mutually exclusive with \code{tau}.
#' @param k Optional single positive [`numeric`] slope or steepness parameter
#'   for the smoothed-threshold parameterization. Must be supplied together with
#'   \code{a50}.
#' @returns A discounted version of \code{lu} in the same format as the input.
#' @author Martin Jung
#' @importClassesFrom terra SpatRaster
#' @importFrom ibis.iSDM is.Raster
#' @examples
#' require(terra)
#' # Load package example rasters
#' range <- terra::rast(system.file(
#'   "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
#' ))
#' lu <- terra::rast(system.file(
#'   "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
#' ))
#' lu <- lu / 10000
#'
#' # Use sparse vegetation as a simple proxy for habitat age/maturity.
#' # In real applications, use an age or maturity layer for the same land-use class.
#' age <- terra::rast(system.file(
#'   "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
#' ))
#' age <- age / 10000
#' age <- age * 20
#'
#' # Specify that habitat reaches 95% of full value at age 20.
#' lu_discounted <- insights_discount(lu, age, target_age = 20, target = 0.95)
#' # Alternative timescale form if tau is known directly:
#' lu_discounted_tau <- insights_discount(lu, age, tau = 20)
#' # Smoothed-threshold form:
#' lu_discounted_smooth <- insights_discount(lu, age, a50 = 10, k = 0.4)
#' out1 <- insights_fraction(range = range, lu = lu)
#' out2 <- insights_fraction(range = range, lu = lu_discounted)
#' op <- graphics::par(mfrow = c(1, 2))
#' terra::plot(out1, main = "Original grassland")
#' terra::plot(out2, main = "Discounted grassland")
#' graphics::par(op)
#' @name insights_discount
#' @export
NULL

#' @name insights_discount
#' @rdname insights_discount
#' @exportMethod insights_discount
#' @export
methods::setGeneric(
  "insights_discount",
  signature = methods::signature("lu", "age"),
  function(lu, age, target_age = 20, target = 0.95, tau = NULL,
           a50 = NULL, k = NULL) {
    standardGeneric("insights_discount")
  })

#' @name insights_discount
#' @rdname insights_discount
#' @aliases insights_discount,SpatRaster,SpatRaster-method
#' @usage \S4method{insights_discount}{SpatRaster,SpatRaster}(lu,age,target_age,target,tau,a50,k)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "SpatRaster", age = "SpatRaster"),
  function(lu, age, target_age = 20, target = 0.95, tau = NULL,
           a50 = NULL, k = NULL) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(lu),
      ibis.iSDM::is.Raster(age)
    )
    assertthat::assert_that(
      is.null(tau) || is.null(a50),
      msg = "Use either tau or a50/k, not both!"
    )
    assertthat::assert_that(
      is.null(a50) == is.null(k),
      msg = "Both a50 and k must be supplied for smoothed threshold!"
    )
    if(!is.null(tau)) {
      assertthat::assert_that(
        is.numeric(tau),
        length(tau) == 1,
        is.finite(tau),
        tau > 0
      )
    } else if(!is.null(a50)) {
      assertthat::assert_that(
        is.numeric(a50),
        length(a50) == 1,
        is.finite(a50),
        a50 >= 0,
        is.numeric(k),
        length(k) == 1,
        is.finite(k),
        k > 0
      )
    } else {
      assertthat::assert_that(
        is.numeric(target_age),
        length(target_age) == 1,
        is.finite(target_age),
        target_age > 0,
        is.numeric(target),
        length(target) == 1,
        is.finite(target),
        target > 0,
        target < 1
      )
    }

    # Check that age values are non-negative
    rr <- terra::global(age, "range", na.rm = TRUE)
    assertthat::assert_that(all(rr[["min"]] >= 0),
                            msg = "Age values must be >= 0!")
    rm(rr)

    # Check that lu and age have the same number of layers
    assertthat::assert_that(
      terra::nlyr(lu) == terra::nlyr(age),
      msg = "lu and age must have the same number of layers!"
    )

    # Ensure that lu and age are comparable (CRS and geometry)
    if(!(terra::same.crs(lu, age) && terra::compareGeom(lu, age, stopOnError = FALSE))) {
      if(!terra::same.crs(lu, age)) {
        ibis.iSDM:::myLog("Preparation", "yellow", "Reprojecting age layer to lu crs.")
        age <- terra::project(age, terra::crs(lu))
      }
      if(!terra::compareGeom(lu, age, stopOnError = FALSE)) {
        ibis.iSDM:::myLog("Preparation", "yellow", "Cropping and resampling age layer(s) to lu.")
        age <- terra::crop(age, lu)
        age <- terra::resample(age, lu, method = "average", threads = TRUE)
      }
    }

    # --- #
    # Apply calibrated maturity factor.
    if(is.null(tau) && is.null(a50)) {
      discount <- 1 - (1 - target)^(1 / target_age)
    }
    n <- terra::nlyr(lu)
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    out <- terra::rast()
    for(i in seq_len(n)) {
      if(!is.null(tau)) {
        factor <- 1 - exp(-age[[i]] / tau)
      } else if(!is.null(a50)) {
        factor <- 1 / (1 + exp(-k * (age[[i]] - a50)))
      } else {
        factor <- 1 - (1 - discount) ^ age[[i]]
      }
      discounted <- lu[[i]] * factor
      suppressWarnings(out <- c(out, discounted))
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)

    # Preserve names and time attributes from original
    names(out) <- names(lu)
    terra::time(out) <- terra::time(lu)

    return(out)
  }
)

#' @name insights_discount
#' @rdname insights_discount
#' @aliases insights_discount,SpatRaster,stars-method
#' @usage \S4method{insights_discount}{SpatRaster,stars}(lu,age,target_age,target,tau,a50,k)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "SpatRaster", age = "stars"),
  function(lu, age, target_age = 20, target = 0.95, tau = NULL,
           a50 = NULL, k = NULL) {
    # Convert stars age to SpatRaster and delegate
    age <- terra::rast(age)
    insights_discount(
      lu = lu,
      age = age,
      target_age = target_age,
      target = target,
      tau = tau,
      a50 = a50,
      k = k
    )
  }
)

#' @name insights_discount
#' @rdname insights_discount
#' @aliases insights_discount,stars,SpatRaster-method
#' @usage \S4method{insights_discount}{stars,SpatRaster}(lu,age,target_age,target,tau,a50,k)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "stars", age = "SpatRaster"),
  function(lu, age, target_age = 20, target = 0.95, tau = NULL,
           a50 = NULL, k = NULL) {
    # Convert stars lu to SpatRaster, apply discount, convert back
    assertthat::assert_that(
      inherits(lu, "stars"),
      ibis.iSDM::is.Raster(age)
    )

    # Convert lu to SpatRaster
    lu_rast <- terra::rast(lu)
    out_rast <- insights_discount(
      lu = lu_rast,
      age = age,
      target_age = target_age,
      target = target,
      tau = tau,
      a50 = a50,
      k = k
    )

    # Convert back to stars preserving dimensions
    proj <- stars::st_as_stars(out_rast, crs = sf::st_crs(lu))
    if(length(stars::st_dimensions(lu)) >= 3) {
      out_dims <- stars::st_dimensions(proj)
      names(out_dims)[3] <- "time"
      out_dims$time <- stars::st_dimensions(lu)[[3]]
      stars::st_dimensions(proj) <- out_dims
    }
    return(proj)
  }
)

#' @name insights_discount
#' @rdname insights_discount
#' @aliases insights_discount,stars,stars-method
#' @usage \S4method{insights_discount}{stars,stars}(lu,age,target_age,target,tau,a50,k)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "stars", age = "stars"),
  function(lu, age, target_age = 20, target = 0.95, tau = NULL,
           a50 = NULL, k = NULL) {
    assertthat::assert_that(
      inherits(lu, "stars"),
      inherits(age, "stars")
    )
    assertthat::assert_that(
      is.null(tau) || is.null(a50),
      msg = "Use either tau or a50/k, not both!"
    )
    assertthat::assert_that(
      is.null(a50) == is.null(k),
      msg = "Both a50 and k must be supplied for smoothed threshold!"
    )
    if(!is.null(tau)) {
      assertthat::assert_that(
        is.numeric(tau),
        length(tau) == 1,
        is.finite(tau),
        tau > 0
      )
    } else if(!is.null(a50)) {
      assertthat::assert_that(
        is.numeric(a50),
        length(a50) == 1,
        is.finite(a50),
        a50 >= 0,
        is.numeric(k),
        length(k) == 1,
        is.finite(k),
        k > 0
      )
    } else {
      assertthat::assert_that(
        is.numeric(target_age),
        length(target_age) == 1,
        is.finite(target_age),
        target_age > 0,
        is.numeric(target),
        length(target) == 1,
        is.finite(target),
        target > 0,
        target < 1
      )
    }

    # Check that stars lu has a time dimension
    assertthat::assert_that(
      length(stars::st_dimensions(lu)) >= 3,
      any(c("Time", "time") %in% names(stars::st_dimensions(lu))),
      msg = "No dimension with name \"time\" found in land-use time series!"
    )
    dims <- stars::st_dimensions(lu)
    names(dims)[3] <- "time"
    stars::st_dimensions(lu) <- dims
    times <- stars::st_get_dimension_values(lu, "time")

    # Check that stars age has a time dimension
    assertthat::assert_that(
      length(stars::st_dimensions(age)) >= 3,
      any(c("Time", "time") %in% names(stars::st_dimensions(age))),
      msg = "No dimension with name \"time\" found in age time series!"
    )
    age_dims <- stars::st_dimensions(age)
    names(age_dims)[3] <- "time"
    stars::st_dimensions(age) <- age_dims
    age_times <- stars::st_get_dimension_values(age, "time")

    assertthat::assert_that(
      length(unique(times)) == length(unique(age_times)),
      msg = "Number of time steps between lu and age must match!"
    )

    # Aggregate lu variables if more than one
    if(length(lu) > 1) {
      lu <- ibis.iSDM:::st_reduce(lu, names(lu), newname = "lu", fun = "sum")
    }

    # Process each time step
    if(is.null(tau) && is.null(a50)) {
      discount <- 1 - (1 - target)^(1 / target_age)
    }
    n_times <- length(unique(times))
    pb <- utils::txtProgressBar(min = 0, max = n_times, style = 3)

    proj <- terra::rast()
    for(tt in seq_len(n_times)) {
      s_lu <- lu |> stars:::slice.stars("time", tt)
      s_age <- age |> stars:::slice.stars("time", tt)
      lu_r <- terra::rast(s_lu)
      age_r <- terra::rast(s_age)

      if(!is.null(tau)) {
        factor <- 1 - exp(-age_r / tau)
      } else if(!is.null(a50)) {
        factor <- 1 / (1 + exp(-k * (age_r - a50)))
      } else {
        factor <- 1 - (1 - discount) ^ age_r
      }
      discounted <- lu_r * factor
      suppressWarnings(proj <- c(proj, discounted))
      utils::setTxtProgressBar(pb, tt)
    }
    close(pb)

    # Convert back to stars
    proj <- stars::st_as_stars(proj, crs = sf::st_crs(lu))

    # Reset time dimension for consistency
    out_dims <- stars::st_dimensions(proj)
    names(out_dims)[3] <- "time"
    out_dims$time <- stars::st_dimensions(lu)[["time"]]
    stars::st_dimensions(proj) <- out_dims

    return(proj)
  }
)
