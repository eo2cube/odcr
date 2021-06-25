#' Suppress messages and warnings
#' @keywords internal
#' @noRd
quiet <- function(expr){
  #return(expr)
  return(suppressWarnings(suppressMessages(expr)))
}

#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @keywords internal
#' @noRd
.out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("odcr.verbose")){
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input),sep="\n")
    } else{message(paste0(sign,input))}}}}
}

#' @importFrom sf st_as_sf st_crs
#' @importFrom stars st_as_stars
#'
#' @keywords internal
#' @noRd
.xarray_convert <- function(ds, crs, filename = tempfile(fileext = ".ncdf"), method = "stars"){

  if(!any(grepl("xarray", class(ds)))){
    .out("'ds' must be of class 'xarray...'.", type = 3)
  }

  # DELTE time$attrs$units first #hereistopped

  # write ds to ncdf
  ds$to_netcdf(path = filename, format = "NETCDF4")

  # load values from xarray
  if(method == "stars"){

    x <- read_stars(filename)

    # as_stars with the bbox method
    bbox <- st_as_sf(data.frame(x = range(x$x$values), y = range(x$y$values)), coords = c("x", "y"))
    st_crs(bbox) <- crs
    st_as_stars(st_bbox(bbox), nx = dim(v)[1], ny = dim(v)[2], n = dim(v)[1]*dim(v)[2], values = v)
  }


  # load values from xarray
  if(method == "stars_mem"){
    v <- x$values

    # as_stars with the bbox method
    bbox <- st_as_sf(data.frame(x = range(x$x$values), y = range(x$y$values)), coords = c("x", "y"))
    st_crs(bbox) <- crs
    st_as_stars(st_bbox(bbox), nx = dim(v)[1], ny = dim(v)[2], n = dim(v)[1]*dim(v)[2], values = v)
  }
}

# global reference to datacube
datacube <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  datacube <<- reticulate::import("datacube", delay_load = TRUE)
}
