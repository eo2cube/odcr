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

#' @keywords internal
#' @noRd
.check_class <- function(x, name, class_str){
  if(!any(grepl(class_str, class(x)))){
    out(paste0("'", name, "' must be of class '", class_str, "'..."), type = 3)
  }
}

#' @keywords internal
#' @noRd
.transform_query <- function(dc, query){

  # translate measurements for this dc
  if(!is.null(query$measurements)){
    dc_measurements <- dc$list_measurements()
    query$measurements <- unlist(.translate_measurements(query$measurements, dc_measurements))
  }
  return(query)
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
#' @importFrom utils tail
#' @noRd
.get_measurements <- function(x){
  unlist(lapply(tail(strsplit(as.character(x$data_vars), ".\n")[[1]], n=-1), function(.x) trimws(strsplit(.x, "\\(")[[1]][1])))
}


#' @importFrom sf st_crs st_set_crs
#' @importFrom stars read_stars st_set_dimensions
#' @importFrom methods as
#'
#' @keywords internal
#' @noRd
.xarray_convert <- function(x, filename = tempfile(fileext = ".ncdf"), method = "stars"){

  if(!any(grepl("xarray", class(x)))){
    out("'x' must be of class 'xarray...'.", type = 3)
  }

  # delete ncdf conflicting attribute
  try(x$time$attrs$units <- NULL, silent = T)

  # write x to ncdf
  x$to_netcdf(path = filename, format = "NETCDF4")

  # load values from xarray
  if(any(method == "stars", method == "raster")){
    y <- read_stars(filename, quiet = T)
    y <- st_set_crs(y, as.numeric(x$spatial_ref$values)) # THIS MIGHT BE BREAKING IF THERE IS NO EPSG VALUE AS CRS!
    y <- st_set_dimensions(y, "band", names = "time")
  }

  if(method == "raster"){
    y_names <- names(y)
    if(length(y_names) > 1){
      out("Raster* cannot represent four dimensions, only three. Coercing to a list of Raster* instead.", type = 2)
      y <- lapply(1:length(y_names), function(i){
        as(y[i], "Raster")
      })
      names(y) <- y_names
    } else{
      y <- as(y, "Raster")
    }
  }

  return(y)
}

# #' @importFrom sf st_crs st_set_crs
# #' @importFrom stars read_stars st_set_dimensions
# #'
# #' @keywords internal
# #' @noRd
# .dc_query <- function(dc, query, method = "find_datasets", return_class = "stars"){
#
#   if(!any(grepl("datacube.api.core.Datacube", class(dc)))){
#     out("'dc' must be of class 'datacube...'.", type = 3)
#   }
#
#   # load: query and subsetting done by datacube
#   if(method == "load"){
#     ds <- do.call(dc$load, query)
#
#     # return xarray python object
#     if(return_class == "xarray"){
#       return(ds)
#     }
#
#     # return converted stars object
#     if(return_class == "stars"){
#       .xarray_convert(ds)
#     }
#   }
#
#   # find_datasets: create a proxy/vrt stack instead of loading
#   if(method == "find_datasets"){
#
#     # get paths
#     dc_find_datasets(dc, query)
#
#     ###### ADD METHOD TO DEAL WITH PATHS HERE #######
#
#   }
# }

# global reference to datacube
datacube <- NULL
np <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  datacube <<- reticulate::import("datacube", delay_load = TRUE)
  np <<- reticulate::import("numpy", delay_load = TRUE)

  options(odcr.dc = NA)
  options(odcr.verbose = TRUE)
}
