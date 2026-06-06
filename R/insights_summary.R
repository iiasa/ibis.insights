#' Summarize inSiGHTS into an index
#'
#' @description
#' This function handily summarizes the suitable habitat for a given species or
#' biodiversity feature into an index. If a single timestep (or object with a single layer)
#' is provided, this function simply summarizes the suitable area.
#'
#' @param obj A [`SpatRaster`] or temporal [`stars`] object with the
#' applied InSiGHTS outputs from \code{insights_fraction} or \code{insights_area}.
#' If the number of layers is greater than 1, the parameter \code{"relative"}
#' might be applied.
#' @param toArea A [`logical`] flag whether fractional suitable habitat should
#' be multiplied by cell area before summarizing (Default: \code{TRUE}).
#' Use \code{FALSE} for outputs from \code{insights_area()}, which are already
#' in area units.
#' @param fun A [`character`] indicating the summary function to be applied (Default: \code{'sum'}).
#' Currently supported are \code{'sum'}, \code{'min'}, \code{'max'}, \code{'median'} and \code{'mean'}.
#' @param relative A [`logical`] flag whether a relative index is to be constructed (Default: \code{TRUE}).
#' @param symmetric A [`logical`] flag whether to additionally compute the symmetric relative
#' difference (Default: \code{FALSE}). Requires \code{relative = TRUE}.
#' @details
#' When \code{relative = TRUE}, the standard relative change (in percent) is computed as
#' \eqn{D(t) = (x_t - x_0) / x_0 \times 100}.
#'
#' When \code{symmetric = TRUE}, the symmetric relative difference is also reported as
#' an additional column \code{relative_change_sym}:
#'
#' \deqn{D_{sym}(t) = \frac{x_t - x_0}{x_t + x_0}}
#'
#' This metric is bounded in \eqn{[-1, 1]} and is preferred over the standard relative
#' change when the baseline habitat area \eqn{x_0} is small (causing the standard
#' metric to become arbitrarily large), or when a bounded, symmetric index is needed
#' for cross-species comparisons. Requires the baseline suitability (\eqn{x_0}) to be
#' positive.
#' @returns A [`data.frame`] with area estimates or the respective indicator.
#' @author Martin Jung
#' @examples
#' require(terra)
#' range <- terra::rast(system.file(
#'   "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
#' ))
#' lu <- terra::rast(system.file(
#'   "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
#' )) / 10000
#'
#' out <- insights_fraction(range = range, lu = lu)
#' insights_summary(out, relative = FALSE)
#'
#' ts <- c(out, out * 0.8, out * 0.6)
#' terra::time(ts, tstep = "years") <- c(2020, 2040, 2060)
#' insights_summary(ts, relative = TRUE, symmetric = TRUE)
#' @references
#' * Baisero, Daniele, Piero Visconti, Michela Pacifici, Marta Cimatti, and Carlo Rondinini. "Projected global loss of mammal habitat due to land-use and climate change." One Earth 2, no. 6 (2020): 578-585.
#' * Powers, Ryan P., and Walter Jetz. "Global habitat loss and extinction risk of terrestrial vertebrates under future land-use-change scenarios." Nature Climate Change 9, no. 4 (2019): 323-329.
#' @name insights_summary
#' @export
NULL

#' @name insights_summary
#' @rdname insights_summary
#' @exportMethod insights_summary
#' @export
methods::setGeneric(
  "insights_summary",
  signature = methods::signature("obj"),
  function(obj, toArea = TRUE, fun = 'sum', relative = TRUE, symmetric = FALSE) standardGeneric("insights_summary"))

#' @name insights_summary
#' @rdname insights_summary
#' @aliases insights_summary,SpatRaster-method
#' @usage \S4method{insights_summary}{SpatRaster}(obj,toArea,fun,relative,symmetric)
methods::setMethod(
  "insights_summary",
  methods::signature(obj = "SpatRaster"),
  function(obj, toArea = TRUE, fun = 'sum', relative = TRUE, symmetric = FALSE) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(obj),
      is.logical(toArea),
      is.character(fun),
      is.logical(relative),
      is.logical(symmetric)
    )

    # Match summary function
    fun <- match.arg(fun, c('sum', 'min', 'max', 'mean', 'median'), several.ok = FALSE)

    # Some basic checks
    rr <- terra::global(obj,"range",na.rm=TRUE)
    assertthat::assert_that(all(rr[["min"]]>=0 ),
                            msg = "Not a properly refined range!"
    )
    already_area <- any(rr[["max"]] > 1, na.rm = TRUE)
    rm(rr)

    # Apply area correction if set
    if(toArea){
      if(already_area){
        warning(
          "Input values exceed 1; treating them as already in area units and skipping cell-area multiplication. Use toArea = FALSE to silence this warning."
        )
        unit <- "input"
      } else {
      # Calculate the area size in km2
      ar <- terra::cellSize(obj, unit = "km")
      obj <- obj * ar
      unit <- "km2"
      }
    } else {
      unit <- "input"
    }

    # --- #
    # Summarize
    # terra::global does not support "median"; handle separately
    if(fun == "median"){
      suitability_vals <- sapply(seq_len(terra::nlyr(obj)), function(i)
        stats::median(terra::values(obj[[i]]), na.rm = TRUE))
    } else {
      suitability_vals <- terra::global(obj, fun, na.rm = TRUE)[, 1]
    }
    results <- data.frame(
      time = terra::time(obj),
      suitability = suitability_vals
    )
    results$unit <- unit

    # Validate symmetric option
    if(symmetric) assertthat::assert_that(relative,
                                          msg = "symmetric = TRUE requires relative = TRUE.")

    # Relative conversion if set
    if(relative && nrow(results)>1){
      results$relative_change_perc <- relChange(results$suitability)
      if(symmetric) results$relative_change_sym <- relChangeSym(results$suitability)
      results$suitability <- results$suitability - results$suitability[1]
    }

    assertthat::assert_that(is.data.frame(results),
                            nrow(results)>=1)
    return(results)
  }
)

#' @name insights_summary
#' @rdname insights_summary
#' @aliases insights_summary,stars-method
#' @usage \S4method{insights_summary}{stars}(obj,toArea,fun,relative,symmetric)
methods::setMethod(
  "insights_summary",
  methods::signature(obj = "stars"),
  function(obj, toArea = TRUE, fun = 'sum', relative = TRUE, symmetric = FALSE) {
    assertthat::assert_that(
      inherits(obj, "stars"),
      is.logical(toArea),
      is.character(fun),
      is.logical(relative),
      is.logical(symmetric)
    )

    # Match summary function
    fun <- match.arg(fun, c('sum', 'min', 'max', 'mean', 'median'), several.ok = FALSE)

    # Basic checks on input
    assertthat::assert_that(length(obj)==1,
                            msg = "Multiple scenario attributes found?")
    assertthat::assert_that(!is.na(sf::st_crs(obj)),
                            msg = "Scenario not correctly projected.")

    # --- #
    # Get the scenario predictions and from there the thresholds
    time <- stars::st_get_dimension_values(obj, which = 3) # Assuming band 3 is the time dimension
    already_area <- max(obj[[1]], na.rm = TRUE) > 1

    new <- obj |> terra::rast()
    terra::time(new) <- time

    # Apply area correction if set
    if(toArea){
      if(already_area){
        warning(
          "Input values exceed 1; treating them as already in area units and skipping cell-area multiplication. Use toArea = FALSE to silence this warning."
        )
        ar_unit <- "input"
      } else {
        # Calculate the cell area in km2 on the raster representation used below.
        ar <- terra::cellSize(new[[1]], unit = "km")
        new <- new * ar
        terra::time(new) <- time
        ar_unit <- "km2"
      }
    } else {
      ar_unit <- "input"
    }
    assertthat::assert_that(ibis.iSDM::is.Raster(new))
    names(new) <- rep(names(obj), terra::nlyr(new))

    # Convert to the scenarios to a data.frame
    df <- stars:::as.data.frame.stars(stars:::st_as_stars(new)) |>
      (\(.) subset(., stats::complete.cases(.)))()
    # Rename
    names(df)[3:4] <- c("band", "area")
    # --- #
    # Now calculate from this data.frame several metrics related to the area and change in area
    cell_key <- paste(df$x, df$y, sep = "\r")
    df$id <- match(cell_key, unique(cell_key))
    df <- df[, setdiff(names(df), c("x", "y")), drop = FALSE]
    df$area[is.na(df$area)] <- 0
    df <- df[order(df$id, df$band), , drop = FALSE]

    # --- #
    # Summarize
    func <- switch (fun,
      "sum" = sum,
      "min" = min, "max" = max,
      "mean" = mean, "median" = stats::median
    )
    results <- stats::aggregate(
      list(suitability = df$area),
      by = list(band = df$band),
      FUN = func,
      na.rm = TRUE
    )
    if(length(time) == nrow(results)){
      results$time <- time
    } else {
      results$time <- results$band
    }
    results <- results[, c("time", setdiff(names(results), "time"))]
    results$unit <- ar_unit

    # Validate symmetric option
    if(symmetric) assertthat::assert_that(relative,
                                          msg = "symmetric = TRUE requires relative = TRUE.")

    # Relative conversion if set
    if(relative && nrow(results)>1){
      results$relative_change_perc <- relChange(results$suitability)
      if(symmetric) results$relative_change_sym <- relChangeSym(results$suitability)
      results$suitability <- results$suitability - results$suitability[1]
    }

    assertthat::assert_that(is.data.frame(results),
                            nrow(results)>=1)
    return(results)

  }
)
