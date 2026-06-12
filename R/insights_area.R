#' Apply InSiGHTS with continuous areal land-use data
#'
#' @description
#' Apply an area-of-habitat (AOH) refinement to a binary or fractional range
#' estimate using land-use or habitat layers already expressed as area per cell.
#' Use this method when \code{lu} values are continuous areal units such as
#' \code{km2}. For cell shares or suitability weights in \code{[0, 1]}, use
#' [`insights_fraction()`] instead.
#'
#' If \code{lu} has multiple layers or attributes, they are summed before being
#' applied to the range. Optional \code{other} layers can be supplied as
#' additional suitability or condition masks, but they must already be scaled to
#' \code{[0, 1]}; raw environmental layers such as elevation should be converted
#' to a suitability mask before calling this function.
#'
#' Raster inputs are reprojected, cropped, and resampled to the range geometry
#' when needed. Temporal [`stars`] inputs may use a \code{time} or \code{Time}
#' dimension name. If \code{outfile} is supplied, the extension is adjusted to
#' \code{.tif} for raster output or \code{.nc} for [`stars`] output.
#'
#' Direct \code{ibis.iSDM} \code{DistributionModel} and
#' \code{BiodiversityScenario} methods use thresholded layers. Continuous
#' suitability projections should be extracted from \code{ibis.iSDM} as
#' \code{SpatRaster} or \code{stars} objects before calling this function.
#'
#' @note
#' This function does not infer species-habitat relationships from raw land-use
#' classes. Select, weight, or transform land-use and condition layers before
#' calling \code{insights_area()}.
#' *No checks are conducted on whether supplied area layers add up to the area
#' of a grid cell.*
#' @param range A [`SpatRaster`] or temporal [`stars`] object describing the estimated distribution of a
#' biodiversity feature (e.g. species). Values must be binary or fractional in \code{[0, 1]}.
#' Alternatively a \code{DistributionModel} or \code{BiodiversityScenario}
#' fitted with \code{ibis.iSDM} package can be supplied.
#' @param lu A [`SpatRaster`] or temporal [`stars`] object of the future land-use areas to be applied to the range.
#' **Each layer has to be in area units and greater than or equal to 0.** Multi-layer inputs are summed.
#' @param other Optional [`SpatRaster`] or temporal [`stars`] object describing additional suitable conditions for the species.
#' Values must already be suitability weights in \code{[0, 1]}.
#' @param outfile A writeable [`character`] of where the output should be written to. If missing, the function will return
#' a [`SpatRaster`] or [`stars`] object respectively. Missing \code{.tif} or \code{.nc} extensions are added as needed.
#' @returns Either a [`SpatRaster`] or temporal [`stars`] object or nothing if outputs are written directly to drive.
#' @author Martin Jung
#' @importClassesFrom terra SpatRaster
#' @importFrom ibis.iSDM is.Raster
#' @examples
#' require(terra)
#' range <- terra::rast(system.file(
#'   "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
#' ))
#' lu_fraction <- terra::rast(system.file(
#'   "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
#' )) / 10000
#'
#' # Convert a fractional land-use layer to area per cell in km2.
#' lu_km2 <- lu_fraction * terra::cellSize(lu_fraction, unit = "km")
#' out <- insights_area(range = range, lu = lu_km2)
#'
#' # The output is already in area units, so do not multiply by cell area again.
#' insights_summary(out, toArea = FALSE)
#'
#' @references
#' * Rondinini, Carlo, and Piero Visconti. "Scenarios of large mammal loss in Europe for the 21st century." Conservation Biology 29, no. 4 (2015): 1028-1036.
#' * Visconti, Piero, Michel Bakkenes, Daniele Baisero, Thomas Brooks, Stuart HM Butchart, Lucas Joppa, Rob Alkemade et al. "Projecting global biodiversity indicators under future development scenarios." Conservation Letters 9, no. 1 (2016): 5-13.
#' @name insights_area
#' @export
NULL

#' @name insights_area
#' @rdname insights_area
#' @exportMethod insights_area
#' @export
methods::setGeneric(
  "insights_area",
  signature = methods::signature("range", "lu"),
  function(range, lu, other, outfile = NULL) standardGeneric("insights_area"))

#' @name insights_area
#' @rdname insights_area
#' @aliases insights_area,SpatRaster,SpatRaster-method
#' @usage \S4method{insights_area}{SpatRaster,SpatRaster}(range,lu,other,outfile)
methods::setMethod(
  "insights_area",
  methods::signature(range = "SpatRaster", lu = "SpatRaster"),
  function(range, lu, other, outfile = NULL) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(range),
      ibis.iSDM::is.Raster(lu),
      missing(other) || ibis.iSDM::is.Raster(other) || inherits(other, "stars"),
      is.null(outfile) || is.character(outfile)
    )
    # --- #
    # Check inputs
    rr <- terra::global(range,"range",na.rm=TRUE)
    assertthat::assert_that(all(rr[["min"]]>=0 ),
                            all(rr[["max"]]<=1 ),
                            # all(apply(terra::unique(range), 2, function(z) length(which(!is.nan(unique(z))))) <= 2),
                            msg = "Input range has to be in binary or fractional format!"
    )
    rm(rr)
    assertthat::assert_that(
      !is.na(terra::crs(range)),
      msg = "Range has unspecified projection!"
    )

    # Check raster stack
    rr <- terra::global(lu,"range",na.rm=TRUE)
    assertthat::assert_that(all(rr[["min"]]>=0 ),
                            msg = "Supplied area stacks need to be greater or equal to 0!"
    )
    rm(rr)

    # Ensure that range and lu are comparable
    if(ibis.iSDM::is.Raster(lu) && ibis.iSDM::is.Raster(range)){
      if(!(terra::same.crs(range, lu) && terra::compareGeom(range, lu, stopOnError = FALSE))){
        if(!terra::same.crs(range, lu)){
          ibis.iSDM:::myLog("Preparation", "yellow", "Reprojecting land-use layer to range crs.")
          lu <- terra::project(lu, terra::crs(range))
        }
        if(!terra::compareGeom(range, lu, stopOnError = FALSE)){
          ibis.iSDM:::myLog("Preparation", "yellow", "Cropping and resampling land-use layer(s) to range.")
          lu <- terra::crop(lu, range)
          lu <- terra::resample(lu, range, method = "average", threads = TRUE)
        }
      }
      assertthat::assert_that(ibis.iSDM::is.Raster(lu),
                              ibis.iSDM::is.Raster(range))
    }

    # Align others if set
    if(!missing(other)){
      if(inherits(other, "stars")) {
        other <- terra::rast(other)
      }
      assertthat::assert_that(ibis.iSDM::is.Raster(other),
                              msg = "other provided layers need to be SpatRaster or stars objects.")
      rr <- terra::global(other,"range",na.rm=TRUE)
      assertthat::assert_that(all(rr[["min"]]>=0 ),
                              all(rr[["max"]]<=1 ),
                              msg = "Supply other layers with values between 0 and 1!"
      )

      if(!(terra::same.crs(range, other) && terra::compareGeom(range, other, stopOnError = FALSE))){
        if(!terra::same.crs(range, other)){
          ibis.iSDM:::myLog("Preparation", "yellow", "Reprojecting other layers to range crs.")
          other <- terra::project(other, terra::crs(range))
        }
        if(!terra::compareGeom(range, other, stopOnError = FALSE)){
          ibis.iSDM:::myLog("Preparation", "yellow", "Cropping and resampling other layer(s) to range.")
          other <- terra::crop(other, range)
          other <- terra::resample(other, range, method = "average", threads = TRUE)
        }
      }
      # Aggregating other layers to a single layer
      if(terra::nlyr(other)>1){
        other <- terra::app(other, 'sum', na.rm = TRUE)
      }
      assertthat::assert_that(ibis.iSDM::is.Raster(other),
                              terra::nlyr(other)==1)
    }

    # Check output file if needed
    if(!is.null(outfile)){
      assertthat::assert_that(dir.exists(dirname(outfile)),
                              msg = "Output file directory does not exist!")
      # Correct output file name depending on type
      if(ibis.iSDM::is.Raster(range) && tolower(tools::file_ext(outfile))!="tif"){
        outfile <- paste0(outfile, ".tif")
      } else if(inherits(range, "stars") && tolower(tools::file_ext(outfile))!="nc"){
        outfile <- paste0(outfile, ".nc")
      }
    }
    # --- #
    # Now apply the share per lu
    out <- range
    # Now sum land use areas
    lus <- terra::app(lu, 'sum', na.rm = TRUE)
    out <- out * lus
    if(!missing(other)) out <- out * other
    if(terra::nlyr(range)>1){
      # Check if time dimension is there
      if(all(!is.na(terra::time(out)))){
        names(out) <- paste0("insights_suitability","__",as.character(terra::time(out)))
      } else {
        names(out) <- paste0("insights_suitability","__",1:terra::nlyr(out))
      }
    } else {
      names(out) <- "insights_suitability"
    }

    # --- #
    # Write or return outputs
    if(is.null(outfile)){
      return(out)
    } else {
      terra::writeRaster(out, outfile, overwrite = TRUE)
      invisible()
    }
  }
)

#' @name insights_area
#' @rdname insights_area
#' @aliases insights_area,SpatRaster,stars-method
#' @usage \S4method{insights_area}{SpatRaster,stars}(range,lu,other,outfile)
methods::setMethod(
  "insights_area",
  methods::signature(range = "SpatRaster", lu = "stars"),
  function(range, lu, other, outfile = NULL) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(range),
      inherits(lu, "stars"),
      missing(other) || (inherits(other, "stars") || ibis.iSDM::is.Raster(other)),
      is.null(outfile) || is.character(outfile)
    )

    # Convert if needed
    if(!missing(other)){
      if(inherits(other, "stars")){
        other <- terra::rast(other)
      }
    }

    # Correct output file extension if necessary
    if(!is.null(outfile)){
      assertthat::assert_that(dir.exists(dirname(outfile)),
                              msg = "Output file directory does not exist!")
      # Correct output file name depending on type
      if(inherits(lu, "stars") && tolower(tools::file_ext(outfile))!="nc"){
        outfile <- paste0(outfile, ".nc")
      }
    }

    # Check that stars is correct
    assertthat::assert_that(length(lu)>=1,
                            length( stars::st_dimensions(lu) )>=3,
                            any(c("Time","time") %in% names(stars::st_dimensions(lu))),
                            msg = "No dimension with name \"time\" found in land-use time series!")
    dims <- stars::st_dimensions(lu)
    time_dim <- match(TRUE, names(dims) %in% c("time", "Time"))
    names(dims)[time_dim] <- "time"
    stars::st_dimensions(lu) <- dims
    times <- stars::st_get_dimension_values(lu, "time")

    # --- #
    # For the processing:
    # Aggregate all variables together
    if(length(lu)>1){
      lu <- ibis.iSDM:::st_reduce(lu, names(lu), newname = "suitability", fun = "sum")
    }

    # Then convert each time step to a SpatRaster and pass to insights_area
    n_times <- length(unique(times))
    pb <- utils::txtProgressBar(min = 0, max = n_times, style = 3)
    proj <- terra::rast()
    for(tt in 1:n_times){
      # Make a slice
      s <- lu |> stars:::slice.stars('time', tt)
      # Convert to raster
      s <- terra::rast(s)
      assertthat::assert_that(terra::global(s, "min", na.rm = TRUE)[,1] >=0,
                              msg = "Values smaller than 0 found?")
      o <- insights_area(range = range,
                             lu = s,
                             other = other,
                             outfile = NULL)
      suppressWarnings(
        proj <- c(proj, o)
      )
      utils::setTxtProgressBar(pb, tt)
    }
    close(pb)
    # Finally convert to stars and rename
    proj <- stars::st_as_stars(proj,
                               crs = sf::st_crs(range)
    )

    # Reset time dimension for consistency
    dims <- stars::st_dimensions(proj)
    names(dims)[3] <- "time"
    dims$time <- stars::st_dimensions(lu)[["time"]]
    stars::st_dimensions(proj) <- dims

    # Return result or write respectively
    if(is.null(outfile)){
      return(proj)
    } else {
      stars::write_stars(proj, outfile)
    }
  }
)

#' @name insights_area
#' @rdname insights_area
#' @aliases insights_area,stars,stars-method
#' @usage \S4method{insights_area}{stars,stars}(range,lu,other,outfile)
methods::setMethod(
  "insights_area",
  methods::signature(range = "stars", lu = "stars"),
  function(range, lu, other, outfile = NULL) {
    assertthat::assert_that(
      inherits(range, "stars"),
      inherits(lu, "stars"),
      missing(other) || (inherits(other, "stars") || ibis.iSDM::is.Raster(other)),
      is.null(outfile) || is.character(outfile)
    )

    # Some check
    assertthat::assert_that(
      length(range)==1,
      msg = "More than one layer in range found...?"
    )

    range_dims <- stars::st_dimensions(range)
    if(any(c("time", "Time") %in% names(range_dims))) {
      range_time_dim <- match(TRUE, names(range_dims) %in% c("time", "Time"))
      names(range_dims)[range_time_dim] <- "time"
      stars::st_dimensions(range) <- range_dims
    }

    # Convert if needed
    if(!missing(other)){
      # In this case we recreate / warp the raster to dem
      other <- stars::st_as_stars(other)
      names(other) <- "other"
      other_dims <- stars::st_dimensions(other)
      other_has_time <- any(c("time", "Time") %in% names(other_dims))
      if(other_has_time) {
        other_time_dim <- match(TRUE, names(other_dims) %in% c("time", "Time"))
        names(other_dims)[other_time_dim] <- "time"
        stars::st_dimensions(other) <- other_dims
      }
      if(!other_has_time && length(other_dims) > 2) {
        stop("other has extra dimensions but no time or Time dimension.",
             call. = FALSE)
      }
      if(other_has_time) {
        assertthat::assert_that(
          "time" %in% names(stars::st_dimensions(range)),
          msg = "Cannot align temporal other because range has no time dimension."
        )
        other_time <- stars::st_get_dimension_values(other, "time")
        range_time <- stars::st_get_dimension_values(range, "time")
        if(length(other_time) != length(range_time) ||
           !identical(as.character(other_time), as.character(range_time))) {
          other <- align_temporal(source = other, target = range)
        }
        other_dims <- stars::st_dimensions(other)
        other_has_time <- any(c("time", "Time") %in% names(other_dims))
      }
      # Reproejct and rewarp
      other <- other |> sf::st_transform(crs = sf::st_crs(range))

      ibis.iSDM:::myLog("[Reprojection]", "yellow", "Aligning other layers to range.")
      range_grid <- range
      if(!other_has_time && "time" %in% names(stars::st_dimensions(range_grid))) {
        range_grid <- range_grid |> stars:::slice.stars("time", 1)
      }
      assert_fnn_available()
      other <- other |>
        stars::st_warp(range_grid, cellsize = stars::st_res(range_grid), use_gdal = FALSE)
      if(length(other)>1){
        other <- ibis.iSDM:::st_reduce(other, names(other), newname = "other", fun = "sum")
      }
      assertthat::assert_that(
        min(other[[1]], na.rm = TRUE) >= 0,
        max(other[[1]], na.rm = TRUE) <= 1,
        msg = "Supply other layers with values between 0 and 1!"
      )
    }

    # Correct output file extension if necessary
    if(!is.null(outfile)){
      assertthat::assert_that(dir.exists(dirname(outfile)),
                              msg = "Output file directory does not exist!")
      # Correct output file name depending on type
      if(inherits(lu, "stars") && tolower(tools::file_ext(outfile))!="nc"){
        outfile <- paste0(outfile, ".nc")
      }
    }

    # Aggregate all variables together
    if(length(lu)>1){
      lu <- ibis.iSDM:::st_reduce(lu, names(lu), newname = "suitability", fun = "sum")
    }

    # Check that stars is correct
    assertthat::assert_that(length(lu)>=1,
                            length( stars::st_dimensions(lu) )>=3,
                            any(c("time", "Time") %in% names(stars::st_dimensions(lu))),
                            msg = "No dimension with name \"time\" found in land-use time series!")
    dims <- stars::st_dimensions(lu)
    time_dim <- match(TRUE, names(dims) %in% c("time", "Time"))
    names(dims)[time_dim] <- "time"
    stars::st_dimensions(lu) <- dims
    times <- stars::st_get_dimension_values(lu, "time")
    # Align land-use time steps to the range if needed.
    range_times <- stars::st_get_dimension_values(range, "time")
    if(length(times) != length(range_times) ||
       !identical(as.character(times), as.character(range_times))) {
      lu <- align_temporal(source = lu, target = range)
      dims <- stars::st_dimensions(lu)
      time_dim <- match(TRUE, names(dims) %in% c("time", "Time"))
      names(dims)[time_dim] <- "time"
      stars::st_dimensions(lu) <- dims
      times <- stars::st_get_dimension_values(lu, "time")
    }

    # --- #
    # Check that both sets of layers are comparable
    # If x or y differ, rewarp
    if(all( base::range(stars::st_get_dimension_values(lu, 1)) != base::range(stars::st_get_dimension_values(range, 1)) )){
      assert_fnn_available()
      lu <- stars::st_warp(lu, range,
                           cellsize = stars::st_res(range),
                           use_gdal = FALSE,
                           method = "near")
    }
    # Reset dimensions
    stars::st_dimensions(lu) <- stars::st_dimensions(range)
    # --- #

    # Multiply both layers
    proj <- range * lu
    if(!missing(other)) proj <- proj * other
    names(proj) <- "insights_suitability"
    assertthat::assert_that(length(proj)==1)

    # Return result or write respectively
    if(is.null(outfile)){
      return(proj)
    } else {
      stars::write_stars(proj, outfile)
    }
  }
)

#' @name insights_area
#' @rdname insights_area
#' @aliases insights_area,stars,SpatRaster-method
#' @usage \S4method{insights_area}{stars,SpatRaster}(range,lu,other,outfile)
methods::setMethod(
  "insights_area",
  methods::signature(range = "stars", lu = "SpatRaster"),
  function(range, lu, other, outfile = NULL) {
    assertthat::assert_that(
      inherits(range, "stars"),
      ibis.iSDM::is.Raster(lu),
      missing(other) || (inherits(other, "stars") || ibis.iSDM::is.Raster(other)),
      is.null(outfile) || is.character(outfile)
    )

    # Some check
    assertthat::assert_that(
      length(range)==1,
      msg = "More than one layer in range found...?"
    )

    range_dims <- stars::st_dimensions(range)
    if(any(c("time", "Time") %in% names(range_dims))) {
      range_time_dim <- match(TRUE, names(range_dims) %in% c("time", "Time"))
      names(range_dims)[range_time_dim] <- "time"
      stars::st_dimensions(range) <- range_dims
    }

    # Convert if needed
    if(!missing(other)){
      # In this case we recreate / warp the raster to dem
      other <- stars::st_as_stars(other)
      names(other) <- "other"
      other_dims <- stars::st_dimensions(other)
      other_has_time <- any(c("time", "Time") %in% names(other_dims))
      if(other_has_time) {
        other_time_dim <- match(TRUE, names(other_dims) %in% c("time", "Time"))
        names(other_dims)[other_time_dim] <- "time"
        stars::st_dimensions(other) <- other_dims
      }
      if(!other_has_time && length(other_dims) > 2) {
        stop("other has extra dimensions but no time or Time dimension.",
             call. = FALSE)
      }
      if(other_has_time) {
        assertthat::assert_that(
          "time" %in% names(stars::st_dimensions(range)),
          msg = "Cannot align temporal other because range has no time dimension."
        )
        other_time <- stars::st_get_dimension_values(other, "time")
        range_time <- stars::st_get_dimension_values(range, "time")
        if(length(other_time) != length(range_time) ||
           !identical(as.character(other_time), as.character(range_time))) {
          other <- align_temporal(source = other, target = range)
        }
        other_dims <- stars::st_dimensions(other)
        other_has_time <- any(c("time", "Time") %in% names(other_dims))
      }
      # Reproject and rewarp
      other <- other |> sf::st_transform(crs = sf::st_crs(range))

      ibis.iSDM:::myLog("[Reprojection]", "yellow", "Aligning other layers to range.")
      range_grid <- range
      if(!other_has_time && "time" %in% names(stars::st_dimensions(range_grid))) {
        range_grid <- range_grid |> stars:::slice.stars("time", 1)
      }
      assert_fnn_available()
      other <- other |>
        stars::st_warp(range_grid, cellsize = stars::st_res(range_grid), use_gdal = FALSE)
      other <- terra::rast(other)
    }

    # Correct output file extension if necessary
    if(!is.null(outfile)){
      assertthat::assert_that(dir.exists(dirname(outfile)),
                              msg = "Output file directory does not exist!")
      # Correct output file name depending on type
      if((inherits(lu, "stars") || ibis.iSDM::is.Raster(lu))
         && tolower(tools::file_ext(outfile))!="nc"){
        outfile <- paste0(outfile, ".nc")
      }
    }

    # SpatRaster assumed, sum if too many
    if(terra::nlyr(lu)>1){
      lu <- terra::app(lu, 'sum', na.rm = TRUE)
    }

    # Get dimensions from range
    times <- stars::st_get_dimension_values(range, "time")
    assertthat::assert_that(
      is.numeric(times) || lubridate::is.Date(times) || lubridate::is.POSIXct(times)
    )

    # --- #
    # Then convert each time step to a SpatRaster and pass to insights_area
    n_times <- length(unique(times))
    pb <- utils::txtProgressBar(min = 0, max = n_times, style = 3)
    proj <- terra::rast()
    for(tt in 1:n_times){
      # Make a slice
      s <- range |> stars:::slice.stars('time', tt)
      # Convert to raster
      s <- terra::rast(s)
      assertthat::assert_that(terra::global(s, "max", na.rm = TRUE)[,1] >=0,
                              msg = "Values in range smaller than 0 found?")
      o <- insights_area(range = s,
                             lu = lu,
                             other = other,
                             outfile = NULL)
      suppressWarnings(
        proj <- c(proj, o)
      )
      utils::setTxtProgressBar(pb, tt)
    }
    close(pb)
    # Finally convert to stars and rename
    proj <- stars::st_as_stars(proj,
                               crs = sf::st_crs(range)
    )

    # Reset time dimension for consistency
    dims <- stars::st_dimensions(proj)
    names(dims)[3] <- "time"
    dims$time <- stars::st_dimensions(range)[["time"]]
    stars::st_dimensions(proj) <- dims

    # Return result or write respectively
    if(is.null(outfile)){
      return(proj)
    } else {
      assertthat::assert_that(inherits(proj, "stars"))
      stars::write_stars(proj, outfile)
    }
  }
)

#### Implementation for ibis.iSDM predictions and projections ####
#' @name insights_area
#' @rdname insights_area
#' @aliases insights_area,DistributionModel,ANY-method
#' @usage \S4method{insights_area}{DistributionModel,ANY}(range,lu,other,outfile)
methods::setMethod(
  "insights_area",
  methods::signature(range = "DistributionModel", lu = "ANY"),
  function(range, lu, other, outfile = NULL) {
    assertthat::assert_that(
      inherits(lu, "stars") || ibis.iSDM::is.Raster(lu),
      missing(other) || ibis.iSDM::is.Raster(other) || inherits(other, "stars"),
      is.null(outfile) || is.character(outfile)
    )

    rasters <- range$show_rasters()
    assertthat::assert_that(
      length(rasters) > 0,
      msg = "DistributionModel contains no raster predictions."
    )
    assertthat::assert_that(
      !ibis.iSDM::is.Waiver(range$get_thresholdvalue()),
      is.numeric(range$get_thresholdvalue()),
      msg = paste(
        "No thresholded raster was found.",
        "Call ibis.iSDM::threshold() before using an ibis.iSDM object directly."
      )
    )
    threshold_layer <- grep("threshold", rasters, value = TRUE,
                            ignore.case = TRUE)
    assertthat::assert_that(
      length(threshold_layer) > 0,
      msg = paste(
        "No thresholded raster was found.",
        "Call ibis.iSDM::threshold() before using an ibis.iSDM object directly."
      )
    )
    if(length(threshold_layer) > 1) {
      warning("Multiple thresholded layers found. Using the first one.",
              call. = FALSE)
    }
    threshold <- range$get_data(threshold_layer[1])
    if(ibis.iSDM::is.Raster(threshold) && terra::nlyr(threshold) > 1) {
      mean_layer <- grep("mean", names(threshold), value = TRUE,
                         ignore.case = TRUE)
      if(length(mean_layer) > 0) {
        threshold <- threshold[[mean_layer[1]]]
      } else {
        warning(
          "Multiple threshold layers found. No mean layer was available; using the first layer.",
          call. = FALSE
        )
        threshold <- threshold[[1]]
      }
    }

    if(missing(other)) {
      insights_area(range = threshold, lu = lu, outfile = outfile)
    } else {
      insights_area(range = threshold, lu = lu, other = other,
                    outfile = outfile)
    }
  }
)

#' @name insights_area
#' @rdname insights_area
#' @aliases insights_area,BiodiversityScenario,ANY-method
#' @usage \S4method{insights_area}{BiodiversityScenario,ANY}(range,lu,other,outfile)
methods::setMethod(
  "insights_area",
  methods::signature(range = "BiodiversityScenario", lu = "ANY"),
  function(range, lu, other, outfile = NULL) {
    assertthat::assert_that(
      inherits(lu, "stars") || ibis.iSDM::is.Raster(lu),
      missing(other) || ibis.iSDM::is.Raster(other) || inherits(other, "stars"),
      is.null(outfile) || is.character(outfile)
    )

    data <- range$get_data()
    assertthat::assert_that(
      inherits(data, "stars"),
      msg = "No scenario projection found."
    )
    assertthat::assert_that(
      "threshold" %in% names(data),
      msg = paste(
        "No threshold in scenario projection.",
        "Call ibis.iSDM::threshold() before project() when using a scenario object directly."
      )
    )
    threshold <- data["threshold"]

    if(inherits(lu, "stars")) {
      lu_dims <- stars::st_dimensions(lu)
      lu_has_time <- any(c("time", "Time") %in% names(lu_dims))
      if(!lu_has_time && length(lu_dims) > 2) {
        stop("lu has extra dimensions but no time or Time dimension.",
             call. = FALSE)
      }
      if(!lu_has_time) {
        lu <- terra::rast(lu)
      }
    } else if(ibis.iSDM::is.Raster(lu)) {
      lu_time <- terra::time(lu)
      if(terra::nlyr(lu) > 1 && length(lu_time) == terra::nlyr(lu) &&
         any(!is.na(lu_time))) {
        stop(
          "Cannot align temporal SpatRaster lu to a stars scenario projection. ",
          "Supply lu as stars or as a static SpatRaster.",
          call. = FALSE
        )
      }
    }

    if(missing(other)) {
      insights_area(range = threshold, lu = lu, outfile = outfile)
    } else {
      insights_area(range = threshold, lu = lu, other = other,
                    outfile = outfile)
    }
  }
)
