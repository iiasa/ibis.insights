#' Summarize inSiGHTS into an index
#'
#' @description
#' This function handily summarizes the suitable habitat for a given species or
#' biodiversity feature into an index. If a single timestep (or object with a single layer)
#' is provided, this function simply summarizes the suitable area.
#'
#' @param obj A [`SpatRaster`] or temporal [`stars`] object with the
#' applied InSiGHTS outputs from \code{insights_fraction}. If the number of layers is greater
#' than 1, the parameter \code{"relative"} might be applied.
#' @param toArea A [`logical`] flag whether the suitable habitat should be summarized to area (Default: \code{TRUE})?
#' @param fun A [`character`] indicating the summary function to be applied (Default: \code{'sum'}).
#' Currently supported are \code{'sum'}, \code{'min'}, \code{'max'}, \code{'median'} and \code{'mean'}.
#' @param relative A [`logical`] flag whether a relative index is to be constructed (Default: \code{TRUE}).
#' @returns A [`data.frame`] with area estimates or the respective indicator.
#' @author Martin Jung
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
  function(obj, toArea = TRUE, fun = 'sum', relative = TRUE) standardGeneric("insights_summary"))

#' @name insights_summary
#' @rdname insights_summary
#' @usage \S4method{insights_summary}{SpatRaster,logical,character,logical}(obj,toArea,fun,relative)
methods::setMethod(
  "insights_summary",
  methods::signature(obj = "SpatRaster"),
  function(obj, toArea = TRUE, fun = 'sum', relative = TRUE) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(obj),
      is.logical(toArea),
      is.character(fun),
      is.logical(relative)
    )

    # Match summary function
    fun <- match.arg(fun, c('sum', 'min', 'max', 'mean', 'median'), several.ok = FALSE)

    # Some basic checks
    rr <- terra::global(obj,"range",na.rm=TRUE)
    assertthat::assert_that(all(rr[["min"]]>=0 ),
                            all(rr[["max"]]<=1 ),
                            msg = "Not a properly refined range!"
    )
    rm(rr)

    # Apply area correction if set
    if(toArea){
      # Calculate the area size in km2
      ar <- terra::cellSize(obj, unit = "km")
      obj <- obj * ar
      unit <- "km2"
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

    # Relative conversion if set
    if(relative && nrow(results)>1){
      relChange <- function(v, fac = 100) (((v-v[1]) / v[1]) * fac)
      results$relative_change_perc <- relChange(results$suitability)
      results$suitability <- results$suitability - results$suitability[1]
    }

    assertthat::assert_that(is.data.frame(results),
                            nrow(results)>=1)
    return(results)
  }
)

#' @name insights_summary
#' @rdname insights_summary
#' @usage \S4method{insights_summary}{stars,logical,character,logical}(obj,toArea,fun,relative)
methods::setMethod(
  "insights_summary",
  methods::signature(obj = "stars"),
  function(obj, toArea = TRUE, fun = 'sum', relative = TRUE) {
    assertthat::assert_that(
      inherits(obj, "stars"),
      is.logical(toArea),
      is.character(fun),
      is.logical(relative)
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

    # Apply area correction if set
    if(toArea){
      # Calculate the area size in km2
      ar <- stars:::st_area.stars(obj)
      # Get the unit
      ar_unit <- units::deparse_unit(ar$area)

      new <- (obj |> terra::rast()) * (ar |> terra::rast())
      if(ar_unit == "m2"){
        new <- new / 1e6
        ar_unit <- "km2"
      } else {ar_unit <- "km2"}
      terra::time(new) <- time
    } else {
      new <- (obj |> terra::rast())
      ar_unit <- "input"
      terra::time(new) <- time
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
    df <- df |> dplyr::group_by(x,y) |> dplyr::mutate(id = dplyr::cur_group_id()) |>
      dplyr::ungroup() |> dplyr::select(-x,-y) |>
      dplyr::mutate(area = dplyr::if_else(is.na(area), 0, area)) |> # Convert missing data to 0
      dplyr::arrange(id, band)

    # --- #
    # Summarize
    func <- switch (fun,
      "sum" = sum,
      "min" = min, "max" = max,
      "mean" = mean, "median" = median
    )
    results <- df |> dplyr::group_by(band) |> dplyr::summarise(suitability = func(area, na.rm = TRUE))
    results$unit <- ar_unit

    # Relative conversion if set
    if(relative && nrow(results)>1){
      results$relative_change_perc <- relChange(results$suitability)
      results$suitability <- results$suitability - results$suitability[1]
    }

    assertthat::assert_that(is.data.frame(results),
                            nrow(results)>=1)
    return(results)

  }
)
