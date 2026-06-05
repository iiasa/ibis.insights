#' Apply temporal discount to land-use layers based on an age variable
#'
#' @description
#' This function applies a temporal discount to land-use layers based on a
#' corresponding age or maturity variable. The age variable is always connected
#' to a specific land-use class (e.g. forest age linked to forest fraction) and
#' represents increasing value over time.
#'
#' Newly established habitat (low age) does not provide full habitat value.
#' The \code{target_age} parameter controls how quickly the age translates to
#' effective habitat value: it is the age at which habitat reaches
#' \code{target} of its full value. The function produces a discounted version
#' of \code{lu} by applying a maturity factor derived from the age:
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
#' \itemize{
#'   \item At \code{age = 0}: the factor is \code{0} -- no habitat value for
#'     brand-new land-use.
#'   \item At \code{age = target_age}: the factor is \code{target}, e.g.
#'     \code{0.95} by default.
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
#'   \code{target} of full value. Default: \code{20}.
#' @param target A single [`numeric`] target maturity value strictly between
#'   \code{0} and \code{1}. Default: \code{0.95}.
#' @returns A discounted version of \code{lu} in the same format as the input.
#' @author Martin Jung
#' @importClassesFrom terra SpatRaster
#' @importFrom ibis.iSDM is.Raster
#' @examples
#' require(terra)
#' # Load package example rasters
#' range <- terra::rast(system.file(
#'   "extdata/example_range.tif", package = "insights", mustWork = TRUE
#' ))
#' lu <- terra::rast(system.file(
#'   "extdata/Grassland.tif", package = "insights", mustWork = TRUE
#' ))
#' lu <- lu / 10000
#'
#' # Use sparse vegetation as a simple proxy for habitat age/maturity.
#' # In real applications, use an age or maturity layer for the same land-use class.
#' age <- terra::rast(system.file(
#'   "extdata/Grassland.tif", package = "insights", mustWork = TRUE
#' ))
#' age <- age / 10000
#' age <- age * 20
#'
#' # Specify that habitat reaches 95% of full value at age 20.
#' lu_discounted <- insights_discount(lu, age, target_age = 20, target = 0.95)
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
  function(lu, age, target_age = 20, target = 0.95) {
    standardGeneric("insights_discount")
  })

#' @name insights_discount
#' @rdname insights_discount
#' @usage \S4method{insights_discount}{SpatRaster,SpatRaster}(lu,age,target_age,target)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "SpatRaster", age = "SpatRaster"),
  function(lu, age, target_age = 20, target = 0.95) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(lu),
      ibis.iSDM::is.Raster(age),
      is.numeric(target_age),
      length(target_age) == 1,
      target_age > 0,
      is.numeric(target),
      length(target) == 1,
      target > 0,
      target < 1
    )

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
    discount <- 1 - (1 - target)^(1 / target_age)
    n <- terra::nlyr(lu)
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    out <- terra::rast()
    for(i in seq_len(n)) {
      factor <- 1 - (1 - discount) ^ age[[i]]
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
#' @usage \S4method{insights_discount}{SpatRaster,stars}(lu,age,target_age,target)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "SpatRaster", age = "stars"),
  function(lu, age, target_age = 20, target = 0.95) {
    # Convert stars age to SpatRaster and delegate
    age <- terra::rast(age)
    insights_discount(
      lu = lu,
      age = age,
      target_age = target_age,
      target = target
    )
  }
)

#' @name insights_discount
#' @rdname insights_discount
#' @usage \S4method{insights_discount}{stars,SpatRaster}(lu,age,target_age,target)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "stars", age = "SpatRaster"),
  function(lu, age, target_age = 20, target = 0.95) {
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
      target = target
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
#' @usage \S4method{insights_discount}{stars,stars}(lu,age,target_age,target)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "stars", age = "stars"),
  function(lu, age, target_age = 20, target = 0.95) {
    assertthat::assert_that(
      inherits(lu, "stars"),
      inherits(age, "stars"),
      is.numeric(target_age),
      length(target_age) == 1,
      target_age > 0,
      is.numeric(target),
      length(target) == 1,
      target > 0,
      target < 1
    )

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
    discount <- 1 - (1 - target)^(1 / target_age)
    n_times <- length(unique(times))
    pb <- utils::txtProgressBar(min = 0, max = n_times, style = 3)

    proj <- terra::rast()
    for(tt in seq_len(n_times)) {
      s_lu <- lu |> stars:::slice.stars("time", tt)
      s_age <- age |> stars:::slice.stars("time", tt)
      lu_r <- terra::rast(s_lu)
      age_r <- terra::rast(s_age)

      factor <- 1 - (1 - discount) ^ age_r
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
