#' Harmonize and align temporal raster layers
#'
#' @description
#' Align the temporal dimension of a source raster-like object to the time steps
#' of a target object. For each target time step, \code{align_temporal()} selects
#' the most recent source time step that is less than or equal to it. Target
#' time steps earlier than the first source time step use the first source
#' layer.
#'
#' Before alignment, time coordinates are harmonized to integer calendar years.
#' This supports common \code{stars} time dimensions stored as \code{Date},
#' \code{POSIXct}, numeric day offsets from 1970-01-01, or \code{units} values
#' such as \code{"days since 1970-01-01"}. For \code{terra::SpatRaster}
#' objects, the output time is written with \code{tstep = "years"}. For
#' \code{stars} objects, the \code{time} dimension values are returned as
#' \code{units} in \code{"years"}.
#'
#' @param source A \code{\link[terra:SpatRaster]{terra::SpatRaster}} or
#'   \code{\link{stars}} object providing the values to align.
#' @param target A \code{\link[terra:SpatRaster]{terra::SpatRaster}} or
#'   \code{\link{stars}} object providing the target time steps. It must be the
#'   same object type as \code{source}.
#' @param unit A single \code{character} value describing the harmonized time
#'   unit. Currently only \code{"years"} is supported.
#'
#' @returns An object of the same class as \code{source}, with temporal layers
#'   selected from \code{source} and time coordinates matching the harmonized
#'   target time steps.
#'
#' @details
#' \code{align_temporal()} is implemented as an S4 generic with methods for
#' \code{SpatRaster,SpatRaster} and \code{stars,stars} inputs. It is useful when
#' a range or environmental raster is available at coarser time steps than a
#' land-use or scenario raster. The function implements a "previous value
#' carried forward" alignment: a target year of 2035 will use a source layer
#' from 2030 when the next source layer is 2040.
#'
#' For \code{stars} inputs, a time dimension named \code{Time} is normalized to
#' \code{time}. The function does not alter spatial dimensions, attributes,
#' coordinate reference systems, or cell values beyond selecting/repeating
#' temporal slices.
#'
#' @examples
#' require(terra)
#'
#' source <- terra::rast(nrow = 1, ncol = 1, nlyr = 3, vals = c(10, 20, 30))
#' terra::time(source) <- as.Date(c("2000-01-01", "2010-01-01", "2020-01-01"))
#'
#' target <- terra::rast(nrow = 1, ncol = 1, nlyr = 4, vals = 1)
#' terra::time(target) <- as.Date(c("1995-01-01", "2005-01-01",
#'                                  "2015-01-01", "2025-01-01"))
#'
#' align_temporal(source, target)
#'
#' @author Martin Jung
#' @keywords utils
#' @name align_temporal
#' @export
NULL

#' @name align_temporal
#' @rdname align_temporal
#' @exportMethod align_temporal
#' @export
methods::setGeneric(
  "align_temporal",
  signature = methods::signature("source", "target"),
  function(source, target, unit = "years") standardGeneric("align_temporal")
)

#' @name align_temporal
#' @rdname align_temporal
#' @aliases align_temporal,SpatRaster,SpatRaster-method
#' @usage \S4method{align_temporal}{SpatRaster,SpatRaster}(source,target,unit)
methods::setMethod(
  "align_temporal",
  methods::signature(source = "SpatRaster", target = "SpatRaster"),
  function(source, target, unit = "years") {
    assertthat::assert_that(
      is.character(unit),
      length(unit) == 1,
      msg = "unit must be a single character value."
    )
    unit <- match.arg(unit, choices = "years")

    to_years <- function(vals, time_name) {
      if(is.null(vals) || length(vals) == 0 || all(is.na(vals))) {
        stop(time_name, " must have non-missing time values.", call. = FALSE)
      }
      if(any(is.na(vals))) {
        stop(time_name, " time values must not contain missing values.", call. = FALSE)
      }

      if(inherits(vals, "units")) {
        unit_str <- trimws(paste(units::deparse_unit(vals), collapse = " "))
        unit_str_lower <- tolower(unit_str)

        if(unit_str_lower %in% c("year", "years", "yr", "yrs", "a")) {
          years <- as.integer(as.numeric(vals))
        } else {
          base_unit <- trimws(sub("\\s+since\\s+.*$", "", unit_str_lower))
          epoch_match <- regexpr("\\d{4}-\\d{2}-\\d{2}", unit_str_lower)

          if(epoch_match[1] < 0) {
            stop(
              time_name,
              " units must be year-based or include an epoch such as ",
              "'days since 1970-01-01'.",
              call. = FALSE
            )
          }

          epoch <- as.Date(regmatches(unit_str_lower, epoch_match))
          days_per_value <- switch(
            base_unit,
            "d" = 1,
            "day" = 1,
            "days" = 1,
            "h" = 1 / 24,
            "hour" = 1 / 24,
            "hours" = 1 / 24,
            "min" = 1 / 1440,
            "minute" = 1 / 1440,
            "minutes" = 1 / 1440,
            "s" = 1 / 86400,
            "sec" = 1 / 86400,
            "secs" = 1 / 86400,
            "second" = 1 / 86400,
            "seconds" = 1 / 86400,
            "month" = 30.4375,
            "months" = 30.4375,
            "year" = 365.25,
            "years" = 365.25,
            "yr" = 365.25,
            "yrs" = 365.25,
            stop(time_name, " has unsupported time unit: ", base_unit, call. = FALSE)
          )

          years <- as.integer(format(
            epoch + round(as.numeric(vals) * days_per_value),
            "%Y"
          ))
        }
      } else if(inherits(vals, "POSIXt")) {
        years <- as.integer(format(as.POSIXct(vals), "%Y"))
      } else if(inherits(vals, "Date")) {
        years <- as.integer(format(vals, "%Y"))
      } else if(is.numeric(vals)) {
        vals <- as.numeric(vals)
        whole_years <- abs(vals - round(vals)) < .Machine$double.eps^0.5

        if(all(vals >= 1000 & vals <= 9999 & whole_years)) {
          years <- as.integer(round(vals))
        } else {
          years <- as.integer(format(as.Date(vals, origin = "1970-01-01"), "%Y"))
        }
      } else {
        stop(
          time_name,
          " has unsupported time values of class: ",
          paste(class(vals), collapse = ", "),
          call. = FALSE
        )
      }

      if(any(is.na(years))) {
        stop(time_name, " time values could not be converted to calendar years.", call. = FALSE)
      }

      units::set_units(as.integer(years), "years")
    }

    source_time <- to_years(terra::time(source), "source")
    target_time <- to_years(terra::time(target), "target")
    source_key <- as.numeric(source_time)
    target_key <- as.numeric(target_time)

    source_order <- order(source_key)
    if(!identical(source_order, seq_along(source_order))) {
      source <- source[[source_order]]
      source_time <- source_time[source_order]
      source_key <- as.numeric(source_time)
    }

    idx <- findInterval(target_key, source_key)
    idx[idx == 0] <- 1

    out <- source[[idx]]
    terra::time(out, tstep = "years") <- as.integer(as.numeric(target_time))
    out
  }
)

#' @name align_temporal
#' @rdname align_temporal
#' @aliases align_temporal,stars,stars-method
#' @usage \S4method{align_temporal}{stars,stars}(source,target,unit)
methods::setMethod(
  "align_temporal",
  methods::signature(source = "stars", target = "stars"),
  function(source, target, unit = "years") {
    assertthat::assert_that(
      is.character(unit),
      length(unit) == 1,
      msg = "unit must be a single character value."
    )
    unit <- match.arg(unit, choices = "years")

    to_years <- function(vals, time_name) {
      if(is.null(vals) || length(vals) == 0 || all(is.na(vals))) {
        stop(time_name, " must have non-missing time values.", call. = FALSE)
      }
      if(any(is.na(vals))) {
        stop(time_name, " time values must not contain missing values.", call. = FALSE)
      }

      if(inherits(vals, "units")) {
        unit_str <- trimws(paste(units::deparse_unit(vals), collapse = " "))
        unit_str_lower <- tolower(unit_str)

        if(unit_str_lower %in% c("year", "years", "yr", "yrs", "a")) {
          years <- as.integer(as.numeric(vals))
        } else {
          base_unit <- trimws(sub("\\s+since\\s+.*$", "", unit_str_lower))
          epoch_match <- regexpr("\\d{4}-\\d{2}-\\d{2}", unit_str_lower)

          if(epoch_match[1] < 0) {
            stop(
              time_name,
              " units must be year-based or include an epoch such as ",
              "'days since 1970-01-01'.",
              call. = FALSE
            )
          }

          epoch <- as.Date(regmatches(unit_str_lower, epoch_match))
          days_per_value <- switch(
            base_unit,
            "d" = 1,
            "day" = 1,
            "days" = 1,
            "h" = 1 / 24,
            "hour" = 1 / 24,
            "hours" = 1 / 24,
            "min" = 1 / 1440,
            "minute" = 1 / 1440,
            "minutes" = 1 / 1440,
            "s" = 1 / 86400,
            "sec" = 1 / 86400,
            "secs" = 1 / 86400,
            "second" = 1 / 86400,
            "seconds" = 1 / 86400,
            "month" = 30.4375,
            "months" = 30.4375,
            "year" = 365.25,
            "years" = 365.25,
            "yr" = 365.25,
            "yrs" = 365.25,
            stop(time_name, " has unsupported time unit: ", base_unit, call. = FALSE)
          )

          years <- as.integer(format(
            epoch + round(as.numeric(vals) * days_per_value),
            "%Y"
          ))
        }
      } else if(inherits(vals, "POSIXt")) {
        years <- as.integer(format(as.POSIXct(vals), "%Y"))
      } else if(inherits(vals, "Date")) {
        years <- as.integer(format(vals, "%Y"))
      } else if(is.numeric(vals)) {
        vals <- as.numeric(vals)
        whole_years <- abs(vals - round(vals)) < .Machine$double.eps^0.5

        if(all(vals >= 1000 & vals <= 9999 & whole_years)) {
          years <- as.integer(round(vals))
        } else {
          years <- as.integer(format(as.Date(vals, origin = "1970-01-01"), "%Y"))
        }
      } else {
        stop(
          time_name,
          " has unsupported time values of class: ",
          paste(class(vals), collapse = ", "),
          call. = FALSE
        )
      }

      if(any(is.na(years))) {
        stop(time_name, " time values could not be converted to calendar years.", call. = FALSE)
      }

      units::set_units(as.integer(years), "years")
    }

    source_dims <- stars::st_dimensions(source)
    source_time_dim <- which(names(source_dims) %in% c("time", "Time"))
    if(length(source_time_dim) == 0) {
      stop("source must have a time dimension named 'time' or 'Time'.", call. = FALSE)
    }
    if(length(source_time_dim) > 1) {
      stop("source has more than one time dimension.", call. = FALSE)
    }
    names(source_dims)[source_time_dim] <- "time"
    stars::st_dimensions(source) <- source_dims

    target_dims <- stars::st_dimensions(target)
    target_time_dim <- which(names(target_dims) %in% c("time", "Time"))
    if(length(target_time_dim) == 0) {
      stop("target must have a time dimension named 'time' or 'Time'.", call. = FALSE)
    }
    if(length(target_time_dim) > 1) {
      stop("target has more than one time dimension.", call. = FALSE)
    }
    names(target_dims)[target_time_dim] <- "time"
    stars::st_dimensions(target) <- target_dims

    source_time <- to_years(stars::st_get_dimension_values(source, "time"), "source")
    target_time <- to_years(stars::st_get_dimension_values(target, "time"), "target")
    source_key <- as.numeric(source_time)
    target_key <- as.numeric(target_time)

    source_order <- order(source_key)
    if(!identical(source_order, seq_along(source_order))) {
      source_idx <- rep(list(TRUE), length(stars::st_dimensions(source)))
      names(source_idx) <- names(stars::st_dimensions(source))
      source_idx[["time"]] <- source_order
      source <- do.call(`[`, c(list(source, TRUE), unname(source_idx), drop = FALSE))
      source_time <- source_time[source_order]
      source_key <- as.numeric(source_time)
    }

    idx <- findInterval(target_key, source_key)
    idx[idx == 0] <- 1

    source_idx <- rep(list(TRUE), length(stars::st_dimensions(source)))
    names(source_idx) <- names(stars::st_dimensions(source))
    source_idx[["time"]] <- idx
    out <- do.call(`[`, c(list(source, TRUE), unname(source_idx), drop = FALSE))

    stars::st_set_dimensions(out, "time", values = target_time)
  }
)

#' @name align_temporal
#' @rdname align_temporal
#' @aliases align_temporal,ANY,ANY-method
#' @usage \S4method{align_temporal}{ANY,ANY}(source,target,unit)
methods::setMethod(
  "align_temporal",
  methods::signature(source = "ANY", target = "ANY"),
  function(source, target, unit = "years") {
    stop(
      "source and target must both be stars objects or both be terra::SpatRaster objects.",
      call. = FALSE
    )
  }
)
