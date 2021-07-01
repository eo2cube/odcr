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
out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("odcr.verbose")){
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input),sep="\n")
    } else{message(paste0(sign,input))}}}}
}

# translate measurements vector
#' @keywords internal
#' @noRd
.translate_measurements <- function(x, dc_measurements){
  sapply(x, function(.x){
    if(any(grepl(tolower(.x), tolower(dc_measurements$name)))){
      return(.x)
    }else{
      #sapply(tolower(dc_measurements$aliases), function(y) , simplify = F, USE.NAMES = F)
      dc_measurements$name[sapply(dc_measurements$aliases, function(y) any(sapply(y, function(.y) .x == .y)))]
    }
  }, simplify = F, USE.NAMES = F)
}

# get measurements from dataset
#' @keywords internal
#' @noRd
.get_measurements <- function(ds){
  unlist(lapply(tail(strsplit(as.character(ds$data_vars), ".\n")[[1]], n=-1), function(x) trimws(strsplit(x, "\\(")[[1]][1])))
}


#' @importFrom sf st_crs st_set_crs
#' @importFrom stars read_stars st_set_dimensions
#'
#' @keywords internal
#' @noRd
.xarray_convert <- function(ds, filename = tempfile(fileext = ".ncdf"), method = "stars"){

  if(!any(grepl("xarray", class(ds)))){
    out("'ds' must be of class 'xarray...'.", type = 3)
  }

  # delete ncdf conflicting attribute
  try(ds$time$attrs$units <- NULL, silent = T)

  # write ds to ncdf
  ds$to_netcdf(path = filename, format = "NETCDF4")

  # load values from xarray
  if(method == "stars"){
    x <- read_stars(filename, quiet = T)
    x <- st_set_crs(x, as.numeric(ds$spatial_ref$values)) # THIS MIGHT BE BREAKING IF THERE IS NO EPSG VALUE AS CRS!
    x <- st_set_dimensions(x, "band", names = "time")
    #names(x) <- gsub("X..", "", names(x))
    return(x)
  }
}


#' @keywords internal
#' @noRd
.dc_find_datasets <- function(dc, query){

  if(!any(grepl("datacube.api.core.Datacube", class(dc)))){
    out("'dc' must be of class 'datacube...'.", type = 3)
  }

  # translate measurements for this dc
  if(!is.null(query$measurements)){
    dc_measurements <- dc$list_measurements()
    query$measurements <- unlist(.translate_measurements(query$measurements, dc_measurements))
  }

  # query datasets
  ds_list <- do.call(dc$find_datasets, query)
  ds_paths <- as.data.frame(t(sapply(ds_list, function(x){
    paths <- sapply(x$measurements, function(y) y$path)

    if(!is.null(query$measurements)){
      return(paths[query$measurements])
    } else{
      return(paths)
    }
  })))

  return(ds_paths)
}


#' @importFrom sf st_crs st_set_crs
#' @importFrom stars read_stars st_set_dimensions
#'
#' @keywords internal
#' @noRd
.dc_query <- function(dc, query, method = "find_datasets", return_class = "stars"){

  if(!any(grepl("datacube.api.core.Datacube", class(dc)))){
    out("'dc' must be of class 'datacube...'.", type = 3)
  }

  # load: query and subsetting done by datacube
  if(method == "load"){
    ds <- do.call(dc$load, query)

    # return xarray python object
    if(return_class == "xarray"){
      return(ds)
    }

    # return converted stars object
    if(return_class == "stars"){
      .xarray_convert(ds)
    }
  }

  # find_datasets: create a proxy/vrt stack instead of loading
  if(method == "find_datasets"){

    # get paths
    .dc_find_datasets(dc, query)

    ###### ADD METHOD TO DEAL WITH PATHS HERE #######

  }
}

# global reference to datacube
datacube <- NULL
np <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  datacube <<- reticulate::import("datacube", delay_load = TRUE)
  np <<- reticulate::import("numpy", delay_load = TRUE)

}
