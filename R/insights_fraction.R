#' Apply InSiGHTS with fractional land use data
#'
#' @description
#' This function applies an area-of-habitat (AOH) correction to a provided single time step or multiple step range estimate
#' (binary format). This thus assumes that species-habitat relations remain stable also in future conditions within a
#' provided climatic niche.
#'
#' It is assumed that the land-use layers come in fractional units, so are ranging from \code{0} to \code{1}.
#' Optionally also a elevation (\code{elev}) layer and habitat condition (\code{condition}) can be provided to support refinements
#' by elevational range or habitat condition.
#'
#' @note
#' This function does not do the refinement of land-use fraction to relevant habitats.
#' This needs to be done by the analyst a-priori.
#' @param range A [`SpatRaster`] or temporal [`stars`] object describing the estimated distribution of a
#' biodiversity feature (e.g. species). **Has to be in binary format!**
#' Alternatively a \code{DistributionModel} fitted with \code{ibis.iSDM} package can be supplied.
#' @param lu A [`SpatRaster`] or temporal [`stars`] object of the future land-use fractions to be applied to the range.
#' **Each layer has to be in units of fractions, e.g. between 0 and 1.**
#' @param other Any other [`SpatRaster`] or temporal [`stars`] objects that describe suitable conditions for the species.
#' @param outfile A writeable [`character`] of where the output should be written to. If missing, the function will return
#' a [`SpatRaster`] or [`stars`] object respectively.
#' @param clamp A [`logical`] on whether lu should be clamped to 0 and 1 beforehand (Default: \code{FALSE}).
#'
#' @returns Either a [`SpatRaster`] or temporal [`stars`] object or nothing if outputs are written directly to drive.
#' @author Martin Jung
#' @importClassesFrom terra SpatRaster
#' @importFrom ibis.iSDM is.Raster
#' @examples
#' \dontrun{
#'  out <- insights_fraction(range, landuse)
#' }
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
#' @usage \S4method{insights_fraction}{SpatRaster,SpatRaster,SpatRaster,character,logical}(range,lu,other,outfile,clamp)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "SpatRaster", lu = "SpatRaster"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) {
    assertthat::assert_that(
      ibis.iSDM::is.Raster(range),
      ibis.iSDM::is.Raster(lu),
      missing(other) || ibis.iSDM::is.Raster(other),
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
      assertthat::assert_that(ibis.iSDM::is.Raster(other),
                              msg = "other provided layers need to be in SpatRaster format.")
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
#' @usage \S4method{insights_fraction}{SpatRaster,stars,ANY,character,logical}(range,lu,other,outfile,clamp)
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
    names(dims)[3] <- "time"
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
#' @usage \S4method{insights_fraction}{stars,stars,ANY,character,logical}(range,lu,other,outfile,clamp)
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

    # Convert if needed
    if(!missing(other)){
      # In this case we recreate / warp the raster to dem
      other <- stars::st_as_stars(other)
      names(other) <- "other"
      # Reproejct and rewarp
      other <- other |> sf::st_transform(crs = sf::st_crs(range))

      ibis.iSDM:::myLog("[Reprojection]", "yellow", "Aligning other layers to range.")
      grid <- other |> sf::st_bbox() |> stars::st_as_stars()
      other <- other |>
        stars::st_warp(grid, cellsize = stars::st_res(range),use_gdal = FALSE)
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
    names(dims)[3] <- "time"
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
    if(all( range(stars::st_get_dimension_values(lu, 1)) != range(stars::st_get_dimension_values(range, 1)) )){
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
#' @usage \S4method{insights_fraction}{stars,SpatRaster,ANY,character,logical}(range,lu,other,outfile,clamp)
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

    # Convert if needed
    if(!missing(other)){
      # In this case we recreate / warp the raster to dem
      other <- stars::st_as_stars(other)
      names(other) <- "other"
      # Reproject and rewarp
      other <- other |> sf::st_transform(crs = sf::st_crs(range))

      ibis.iSDM:::myLog("[Reprojection]", "yellow", "Aligning other layers to range.")
      grid <- other |> sf::st_bbox() |> stars::st_as_stars()
      other <- other |>
        stars::st_warp(grid, cellsize = stars::st_res(range),use_gdal = FALSE)
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
    times <- stars::st_get_dimension_values(range, 3)
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
    dims$time <- stars::st_dimensions(range)[[3]]
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
#' @usage \S4method{insights_fraction}{ANY,ANY,SpatRaster,character,logical}(range,lu,other,outfile,clamp)
methods::setMethod(
  "insights_fraction",
  methods::signature(range = "ANY", lu = "ANY"),
  function(range, lu, other, outfile = NULL, clamp = FALSE) {
    assertthat::assert_that(
      inherits(range, "DistributionModel") || inherits(range, "BiodiversityScenario"),
      inherits(lu, "stars") || ibis.iSDM::is.Raster(lu),
      missing(other) || ibis.iSDM::is.Raster(other),
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

        # Now call again insights
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
                               outfile = outfile
                               )
      return(out)
    }
  }
)

