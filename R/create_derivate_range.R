#' Recreate a derivative variable based on their range
#'
#' @description
#' The purpose of this function is to create the range from several derivative
#' variables. The information to do so is taken from the variable name and it
#' is assumed that those have been created by [`ibis.iSDM::predictor_derivate()`] function.
#'
#' This function return the range of values from the original data that fall within
#' the set of coefficients. Currently only positive coefficients are taken by default.
#'
#' @details
#' * This function really only makes sense for \code{'bin'}, \code{'thresh'} and
#' \code{'hinge'} transformations.
#'
#' * For \code{'hinge'} the combined \code{min} is returned.
#'
#' @note
#' This is rather an internal function created for a specific use and project. It
#' might be properly described in an example later.
#'
#' @param env The original variable stacks as [`SpatRaster`] or [`stars`].
#' @param varname A [`character`] of the variable name. Needs to be present in
#' \code{"env"}.
#' @param co A set of coefficients obtained via [`stats::coef()`] and a
#' [`ibis.iSDM::BiodiversityDistribution`] object.
#' @param to_binary A [`logical`] flag if the output should be converted to binary
#' format or left in the original units (Default: \code{FALSE}).
#'
#' @returns A [`SpatRaster`] object containing the predictor range.
#' @examples
#' \dontrun{
#'  # Assuming derivates of temperature have been created for a model, this
#'  # recreates the range over which they apply.
#'  deriv <- create_derivate_range(env, varname = "Temperature",
#'   co = coef(fit), to_binary = TRUE)
#' }
#'
#' @author Martin Jung
#' @keywords utils
create_derivate_range <- function(env, varname, co, to_binary = FALSE){
  assertthat::assert_that(
    ibis.iSDM::is.Raster(env) || inherits(env, "stars"),
    is.character(varname) && length(varname)==1,
    is.data.frame(co) || is.character(co),
    is.logical(to_binary)
  )

  if(is.data.frame(co)){
    if(nrow(co)==0) {
      message("No valid coefficients found?")
      return(NULL)
    }
  }
  if(!(varname %in% names(env))){
    message("Variable not found in environmental stack!")
    return(NULL)
  }
  # --- #

  # Get the deriv names from the coefficients
  # If character is supplied, assume those are all positive.
  if(is.character(co)){
    co <- data.frame(Feature = co, Beta = 0.01)
  }
  deriv <- co[,1]
  if(is.data.frame(deriv)) deriv <- deriv[[1]] # For Tibble

  # Remove intercept if found
  if(length(grep("intercept", deriv,ignore.case = TRUE))>0){
    co <- co[-grep("intercept", deriv,ignore.case = TRUE, value = FALSE),]
    deriv <- grep("intercept", deriv,ignore.case = TRUE, value = TRUE, invert = TRUE)
  }
  assertthat::assert_that(length(deriv)>0,nrow(co)>0,
                          msg = "Somehow all coefficients got removed...")

  # Now split the names use base R
  cu <- base::strsplit(deriv, "_")
  df <- data.frame()
  for(i in 1:length(cu)){ # Number of derivs
    o <- as.data.frame(cu[[i]] |> t())
    # Also check for actual variable coverage
    if(o[1,1] %in% c("thresh")){
      # FIXME: Why does this use points rather than _ ?
      check <- paste0(o[,c(2:(ncol(o)-1))],collapse = '_')
    } else {
      check <- paste0(o[,c(2:(ncol(o)-2))],collapse = '_')
    }
    if(check == varname){
      df <- rbind(df, cbind(co[i,], o) )
    }
  }
  assertthat::assert_that(nrow(df)>0,
                          msg = "Coefficient derivate not found for variable?")
  # Split and get the derivative option
  df <- split(df, df$V1)
  assertthat::assert_that(length(df)==1,
                          msg = "Currently this works only for a single option.")
  # Match
  option <- match.arg(names(df), c('hinge', 'thresh', 'bin'),
                      several.ok = FALSE)

  # Get first entry (only one)
  o <- df[[1]]
  assertthat::assert_that(nrow(o)>0,
                          msg = "Missing coefficients...?")

  # Get lowest and highest values from all positive coefficients
  ind <- which(o[,2] > 0)
  if(length(ind)==0){
    message("Only negative coefficients found...")
    return(NULL)
  }

  # Get all valid values
  vals <- suppressWarnings(
    o[ind, c(ncol(o)-1,ncol(o))] |> base::unlist() |>
      as.numeric() |> sort()
  )
  assertthat::assert_that(!all(is.na(vals)),
                          msg = "No valid values found in variable names?")

  if(option %in% c("bin", "thresh")){
    # These are simply thresholds so within the range
    if(option == "thresh"){
      lb <- o[, ncol(o)] |> as.numeric() |> min()

      # Small helper function
      apply_threshold <- function(z, lb){
        z[z<lb] <- 0
        return(z)
      }
      # Create output
      if(inherits(env, "stars")){
        out <- ibis.iSDM:::stars_to_raster(env[varname])
        out <- lapply(out, function(z) apply_threshold(z, lb))
      } else {
        out <- env[[varname]]
        out <- apply_threshold(out, lb)
      }
    }

    if(option == "bin"){
      lb <- min(vals, na.rm = TRUE)
      ub <- max(vals, na.rm = TRUE)
      assertthat::assert_that(is.numeric(lb),is.numeric(ub))

      # Small helper function
      apply_bin <- function(z, lb, ub){
        z[z<lb] <- 0
        z[z>ub] <- 0
        return(z)
      }

      # Create output
      if(inherits(env, "stars")){
        out <- ibis.iSDM:::stars_to_raster(env[varname])
        out <- lapply(out, function(z) apply_bin(z, lb, ub))
      } else {
        out <- env[[varname]]
        out <- apply_bin(out, lb, ub)
      }
    }

  } else if (option == "hinge") {
    # Here it gets a bit more tricky as the hinge transformations need to be reproduced
    if(inherits(env, "stars")){
      out <- ibis.iSDM:::stars_to_raster(env[varname])
      out <- Reduce("c",out)
    } else {
      out <- ibis.iSDM::emptyraster(env[varname])
    }
    for(lyr in 1:terra::nlyr(out)){
      all <- ibis.iSDM::emptyraster(out)
      for(i in 1:nrow(o[ind, c(ncol(o)-1,ncol(o))]) ){
        new <- ibis.iSDM::emptyraster(env)
        cu <- c( o[ind, c(ncol(o)-1,ncol(o))][i,1], o[ind, c(ncol(o)-1,ncol(o))][i,2] )
        # Remove any leading points
        if(any(substr(cu,1, 1)==".")){
          cu[which(substr(cu,1, 1)==".")] <- gsub("^.","",cu[which(substr(cu,1, 1)==".")])
        }
        cu <- as.numeric(cu)

        new[] <- ibis.iSDM:::hingeval(out[[lyr]][],cu[1], cu[2])
        names(new) <- o[,1][ind][i]
        if(!is.na(terra::time(out)[lyr])) terra::time(new) <- terra::time(out)[lyr]
        suppressWarnings(all <- c(all, new))
        rm(new)
      }
      # Now for the spatial mask, simply take the average
      m <- terra::mean(all)
      if( terra::has.time(out[[lyr]]) ) terra::time(m) <- terra::time(out[[lyr]])
      out[[lyr]] <- m
      rm(m)
    }

  } else {
    stop("Not yet implemented")
  }

  # For stars derived lists, reduce back to raster
  if(is.list(out)){
    out <- Reduce("c",out)
  }

  # If env is stars, try to convert back
  # if(inherits(env, "stars")) out <- stars::st_as_stars(out)

  # Return binary layer or subset
  if(to_binary){
    out[out!=0] <- 1
  }

  assertthat::assert_that(ifelse(ibis.iSDM::is.Raster(out),
                                 terra::hasValues(out),
                                 terra::hasValues(out[[1]])))
  return(out)
}
