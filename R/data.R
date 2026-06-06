#' A range layer for testing the functions
#'
#' @description
#' An example testing layer in [`SpatRaster`] format describing the range of a species.
#' The example data is formatted in Lamberts equal area projection, at European extent and
#' at 10km resolution.
#'
#' @note
#' This layer is an example Species distribution modelling estimate of *Spermophilus citellus*
#' Alternatively a layer of the frog *Bombina bombina* is loaded.
#'
#' @param timeperiod A [`character`] on which example layer is to be loaded. Options
#' include \code{"current"} and \code{"future"}.
#' @format A \code{SpatRaster} object in binary format providing a range.
#' @author Martin Jung
#' @keywords internal
#' @noRd
load_exampledata <- function(timeperiod = "current"){
  assertthat::assert_that(
    is.character(timeperiod)
  )
  # Match
  timeperiod <- match.arg(timeperiod, c("current", "future"),several.ok = FALSE)

  if(timeperiod == "current"){
    range <- terra::rast(system.file('extdata/example_range.tif', package='ibis.insights',mustWork = TRUE))
  } else {
    range <- stars::read_mdim(system.file('extdata/Bombina_bombina__ssp126.nc',
                                          package = 'ibis.insights',mustWork = TRUE))
    range <- range |> split()
    # Focus on threshold only
    range <- range["ensemble_threshold"]
  }
  assertthat::assert_that(inherits(range, 'stars') || inherits(range, 'SpatRaster'))
  return(range)
}
