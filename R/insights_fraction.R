#' Apply InSiGHTS with fractional land-use data
#'
#' @description
#' Apply an area-of-habitat (AOH) refinement to a binary or fractional range
#' estimate using land-use or habitat layers expressed as fractions. Use this
#' method when the land-use values represent cell shares or suitability weights
#' in the interval \code{[0, 1]}. For land-use inputs already expressed as area
#' per cell, use [`insights_area()`] instead.
#'
#' If \code{lu} has multiple layers or attributes, they are summed before being
#' applied to the range. This is useful when several land-use classes jointly
#' represent suitable habitat. Optional \code{other} layers can be supplied as
#' additional suitability or condition masks, but they must already be scaled to
#' \code{[0, 1]}; raw environmental layers such as elevation should be converted
#' to a suitability mask before calling this function.
#'
#' Raster inputs are reprojected, cropped, and resampled to the range geometry
#' when needed. Temporal [`stars`] inputs may use a \code{time} or \code{Time}
#' dimension name. If \code{outfile} is supplied, the extension is adjusted to
#' \code{.tif} for raster output or \code{.nc} for [`stars`] output.
#'
#' @note
#' This function does not infer species-habitat relationships from raw land-use
#' classes. Select, weight, or transform land-use and condition layers before
#' calling \code{insights_fraction()}.
#' @param range A [`SpatRaster`] or temporal [`stars`] object describing the estimated distribution of a
#' biodiversity feature (e.g. species). Values must be binary or fractional in \code{[0, 1]}.
#' Alternatively a \code{DistributionModel} fitted with \code{ibis.iSDM} package can be supplied.
#' @param lu A [`SpatRaster`] or temporal [`stars`] object of the future land-use fractions to be applied to the range.
#' **Each layer has to be in fractional units between 0 and 1.** Multi-layer inputs are summed.
#' @param other Optional [`SpatRaster`] or temporal [`stars`] object describing additional suitable conditions for the species.
#' Values must already be suitability weights in \code{[0, 1]}.
#' @param outfile A writeable [`character`] of where the output should be written to. If missing, the function will return
#' a [`SpatRaster`] or [`stars`] object respectively. Missing \code{.tif} or \code{.nc} extensions are added as needed.
#' @param clamp A [`logical`] on whether \code{lu} and summed fractional suitability should be clamped to 0 and 1 beforehand (Default: \code{FALSE}).
#'
#' @returns Either a [`SpatRaster`] or temporal [`stars`] object or nothing if outputs are written directly to drive.
#' @author Martin Jung
#' @importClassesFrom terra SpatRaster
#' @importFrom ibis.iSDM is.Raster
#' @examples
#' require(terra)
#' # Load package example rasters
#' range <- terra::rast(system.file(
#'   "extdata/example_range.tif", package = "ibis.insights", mustWork = TRUE
#' ))
#' lu <- c(
#'   terra::rast(system.file(
#'     "extdata/Grassland.tif", package = "ibis.insights", mustWork = TRUE
#'   )),
#'   terra::rast(system.file(
#'     "extdata/Sparsely.vegetated.areas.tif", package = "ibis.insights", mustWork = TRUE
#'   ))
#' )
#'
#' # Convert example land-use layers to fractions between 0 and 1.
#' lu <- lu / 10000
#'
#' out <- insights_fraction(range = range, lu = lu, clamp = TRUE)
#' head(insights_summary(out))
#'
#' @references
#' * Rondinini, Carlo, and Piero Visconti. "Scenarios of large mammal loss in Europe for the 21st century." Conservation Biology 29, no. 4 (2015): 1028-1036.
#' * Visconti, Piero, Michel Bakkenes, Daniele Baisero, Thomas Brooks, Stuart HM Butchart, Lucas Joppa, Rob Alkemade et al. "Projecting global biodiversity indicators under future development scenarios." Conservation Letters 9, no. 1 (2016): 5-13.
#' @name insights_fraction
#' @export
NULL

#' @name insights_fraction
#' @rdname insights_fraction
#' @exportMethod insights_fraction
#' @export
methods::setGeneric(
  "insights_fraction",
  signature = methods::signature("range", "lu"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) standardGeneric("insights_fraction"))

#' @name insights_fraction
#' @rdname insights_fraction
#' @aliases insights_fraction,SpatRaster,SpatRaster-method
#' @usage \S4method{insights_fraction}{SpatRaster,SpatRaster}(range,lu,other,outfile,clamp)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "SpatRaster", lu = "SpatRaster"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(range),
      ibis.iSDM::is.Raster(lu),
      missing(other) || ibis.iSDM::is.Raster(other) || inherits(other, "stars"),
      is.null(outfile) || is.character(outfile),
      is.logical(clamp)
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

    # If clamp is true, bound lu shares to 0 to 1
    if(clamp) lu <- st_clamp(lu, lb = 0, ub = 1)

    # Check raster stack
    rr <- terra::global(lu,"range",na.rm=TRUE)
    assertthat::assert_that(all(rr[["min"]]>=0 ),
                            all(rr[["max"]]<=1 ),
                            msg = "Supply fractional land-use layers!"
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
                              msg = "Supply fractional other layers!"
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
    # Now sum land use shares
    lus <- terra::app(lu, 'sum', na.rm = TRUE)
    if(clamp) lus <- st_clamp(lus, lb = 0, ub = 1)
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

#' @name insights_fraction
#' @rdname insights_fraction
#' @aliases insights_fraction,SpatRaster,stars-method
#' @usage \S4method{insights_fraction}{SpatRaster,stars}(range,lu,other,outfile,clamp)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "SpatRaster", lu = "stars"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(range),
      inherits(lu, "stars"),
      missing(other) || (inherits(other, "stars") || ibis.iSDM::is.Raster(other)),
      is.null(outfile) || is.character(outfile),
      is.logical(clamp)
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

    # If clamp is true, bound lu shares to 0 to 1
    if(clamp) lu <- st_clamp(lu, lb = 0, ub = 1)

    # Then convert each time step to a SpatRaster and pass to insights_fraction
    n_times <- length(unique(times))
    pb <- utils::txtProgressBar(min = 0, max = n_times, style = 3)
    proj <- terra::rast()
    for(tt in 1:n_times){
      # Make a slice
      s <- lu |> stars:::slice.stars('time', tt)
      # Convert to raster
      s <- terra::rast(s)
      assertthat::assert_that(terra::global(s, "max", na.rm = TRUE)[,1] <=1,
                              msg = "Values larger than 1 found?")
      o <- insights_fraction(range = range,
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

#' @name insights_fraction
#' @rdname insights_fraction
#' @aliases insights_fraction,stars,stars-method
#' @usage \S4method{insights_fraction}{stars,stars}(range,lu,other,outfile,clamp)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "stars", lu = "stars"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) {
    assertthat::assert_that(
      inherits(range, "stars"),
      inherits(lu, "stars"),
      missing(other) || (inherits(other, "stars") || ibis.iSDM::is.Raster(other)),
      is.null(outfile) || is.character(outfile),
      is.logical(clamp)
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

    # If clamp is true, bound lu shares to 0 to 1
    if(clamp) lu <- st_clamp(lu, lb = 0, ub = 1)

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
    # Check that time steps are identical to range
    assertthat::assert_that(
      length(stars::st_get_dimension_values(lu, "time")) ==
        length(stars::st_get_dimension_values(range, "time")),
      msg = "Number of time steps between range and land-use rasters is differring?"
    )

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

    # Multiply both fractional layers
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

#' @name insights_fraction
#' @rdname insights_fraction
#' @aliases insights_fraction,stars,SpatRaster-method
#' @usage \S4method{insights_fraction}{stars,SpatRaster}(range,lu,other,outfile,clamp)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "stars", lu = "SpatRaster"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) {
    assertthat::assert_that(
      inherits(range, "stars"),
      ibis.iSDM::is.Raster(lu),
      missing(other) || (inherits(other, "stars") || ibis.iSDM::is.Raster(other)),
      is.null(outfile) || is.character(outfile),
      is.logical(clamp)
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

    # If clamp is true, bound lu shares to 0 to 1
    if(clamp) lu <- st_clamp(lu, lb = 0, ub = 1)

    # Get dimensions from range
    times <- stars::st_get_dimension_values(range, "time")
    assertthat::assert_that(
      is.numeric(times) || lubridate::is.Date(times) || lubridate::is.POSIXct(times)
    )

    # --- #
    # Then convert each time step to a SpatRaster and pass to insights_fraction
    n_times <- length(unique(times))
    pb <- utils::txtProgressBar(min = 0, max = n_times, style = 3)
    proj <- terra::rast()
    for(tt in 1:n_times){
      # Make a slice
      s <- range |> stars:::slice.stars('time', tt)
      # Convert to raster
      s <- terra::rast(s)
      assertthat::assert_that(terra::global(s, "max", na.rm = TRUE)[,1] <=1,
                              msg = "Values in range larger than 1 found?")
      o <- insights_fraction(range = s,
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
#' @name insights_fraction
#' @rdname insights_fraction
#' @aliases insights_fraction,ANY,ANY-method
#' @usage \S4method{insights_fraction}{ANY,ANY}(range,lu,other,outfile,clamp)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "ANY", lu = "ANY"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) {
    assertthat::assert_that(
      inherits(range, "DistributionModel") || inherits(range, "BiodiversityScenario"),
      inherits(lu, "stars") || ibis.iSDM::is.Raster(lu),
      missing(other) || ibis.iSDM::is.Raster(other) || inherits(other, "stars"),
      is.null(outfile) || is.character(outfile),
      is.logical(clamp)
    )

    # Correct output file extension if necessary
    if(!is.null(outfile)){
      assertthat::assert_that(dir.exists(dirname(outfile)),
                              msg = "Output file directory does not exist!")
      # Correct output file name depending on type
      if(inherits(lu, "stars") && tolower(tools::file_ext(outfile))!="nc"){
        outfile <- paste0(outfile, ".nc")
      }
    }

    # Approach for fitted models
    if(inherits(range, "DistributionModel")){
      # Check that layer has predictions
      assertthat::assert_that(
        length( range$show_rasters() ) >0,
        msg = "Fitted model contains no predictions!"
      )

      # Check that fitted object has threshold
      assertthat::assert_that(
        !ibis.iSDM::is.Waiver(range$get_thresholdvalue()),
        is.numeric(range$get_thresholdvalue()),
        msg = "No thresholded raster was found!"
      )

      # Get thresholded raster and recall with SpatRaster object
      if( any(grep('threshold', range$show_rasters())) ){
        tr_lyr <- grep('threshold', range$show_rasters(),value = TRUE)
        if(length(tr_lyr)>1) warning("There appear to be multiple thresholded layers. Using the first one.")
        threshold <- range$get_data(tr_lyr[1])
        # Get mean layer if there are multiple
        if( grep("mean", names(threshold),value = TRUE ) != ""){
          threshold <- threshold[[grep("mean", names(threshold),value = TRUE )]]
        }

        # Now call the InSiGHTS workflow again
        out <- insights_fraction(range = threshold,
                                 lu = lu,
                                 other = other,
                                 outfile = outfile,
                                 clamp = clamp)
        return(out)
      } else {
        stop("No thresholded raster was found!")
      }
    } else {
      # Scenario object assumed as input

      # Get data
      data <- range$get_data()
      assertthat::assert_that(inherits(data,"stars"),
                              msg = "No projection found!")
      assertthat::assert_that("threshold" %in% names(data),
                              msg = "No threshold in projection found!")
      data <- data['threshold']

      # Apply on stars
      out <- insights_fraction(range = data,
                               lu = lu,
                               other = other,
                               outfile = outfile,
                               clamp = clamp
                               )
      return(out)
    }
  }
)

