#' Apply temporal discount to land-use layers based on an age variable
#'
#' @description
#' This function applies a temporal discount to land-use layers based on a
#' corresponding age or maturity variable. The age variable is always connected
#' to a specific land-use class (e.g. forest age linked to forest fraction) and
#' represents increasing value over time.
#'
#' Newly established habitat (low age) does not provide full habitat value.
#' The discount parameter controls how quickly the age translates to effective
#' habitat value. The function produces a discounted version of \code{lu} by
#' applying a discount factor derived from the age:
#'
#' \deqn{\mathrm{effective\_lu} = \mathrm{lu} \times \left(1 - (1 - \mathrm{discount})^{\mathrm{age}}\right)}
#'
#' \itemize{
#'   \item At \code{age = 0}: the factor is \code{0} — no habitat value for
#'     brand-new land-use.
#'   \item As \code{age} increases: the factor approaches \code{1} — mature
#'     habitat reaches full value.
#'   \item A higher \code{discount} means the age accumulates effective value
#'     faster (light discounting); a lower value means slower accumulation
#'     (heavy discounting).
#' }
#'
#' @param lu A [`SpatRaster`] or temporal [`stars`] object of the land-use
#'   variable (e.g. forest fraction or area). Can be single or multi-layer.
#' @param age A [`SpatRaster`] or temporal [`stars`] object of the
#'   corresponding age or maturity variable (values \code{>= 0}).
#'   Must match \code{lu} in number of layers / time steps.
#' @param discount A [`numeric`] discount rate strictly between 0 and 1
#'   (exclusive). Higher values mean faster value accumulation (light
#'   discounting); lower values mean slower accumulation (heavy discounting).
#'   Default: \code{0.5}.
#'
#' @returns A discounted version of \code{lu} in the same format as the input.
#' @author Martin Jung
#' @importClassesFrom terra SpatRaster
#' @importFrom ibis.iSDM is.Raster
#' @examples
#' \dontrun{
#'  # Discount forest fraction by forest age
#'  lu_discounted <- insights_discount(lu_forest, age_forest, discount = 0.3)
#'  # Then use in InSiGHTS refinement
#'  out <- insights_fraction(range, lu_discounted)
#' }
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
  function(lu, age, discount = 0.5) standardGeneric("insights_discount"))

#' @name insights_discount
#' @rdname insights_discount
#' @usage \S4method{insights_discount}{SpatRaster,SpatRaster,numeric}(lu,age,discount)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "SpatRaster", age = "SpatRaster"),
  function(lu, age, discount = 0.5) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(lu),
      ibis.iSDM::is.Raster(age),
      is.numeric(discount),
      length(discount) == 1,
      discount > 0,
      discount < 1
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
    # Apply discount: effective_lu = lu * (1 - (1 - discount)^age)
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
#' @usage \S4method{insights_discount}{SpatRaster,stars,numeric}(lu,age,discount)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "SpatRaster", age = "stars"),
  function(lu, age, discount = 0.5) {
    # Convert stars age to SpatRaster and delegate
    age <- terra::rast(age)
    insights_discount(lu = lu, age = age, discount = discount)
  }
)

#' @name insights_discount
#' @rdname insights_discount
#' @usage \S4method{insights_discount}{stars,SpatRaster,numeric}(lu,age,discount)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "stars", age = "SpatRaster"),
  function(lu, age, discount = 0.5) {
    # Convert stars lu to SpatRaster, apply discount, convert back
    assertthat::assert_that(
      inherits(lu, "stars"),
      ibis.iSDM::is.Raster(age),
      is.numeric(discount),
      length(discount) == 1,
      discount > 0,
      discount < 1
    )

    # Convert lu to SpatRaster
    lu_rast <- terra::rast(lu)
    out_rast <- insights_discount(lu = lu_rast, age = age, discount = discount)

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
#' @usage \S4method{insights_discount}{stars,stars,numeric}(lu,age,discount)
methods::setMethod(
  "insights_discount",
  methods::signature(lu = "stars", age = "stars"),
  function(lu, age, discount = 0.5) {
    assertthat::assert_that(
      inherits(lu, "stars"),
      inherits(age, "stars"),
      is.numeric(discount),
      length(discount) == 1,
      discount > 0,
      discount < 1
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
