#' convert index into name
#' @keywords internal
#' @noRd
.index2name <- function(x, ...){
  i <- list(...)[[1]]
  if(is.numeric(i)){
    i <- .get_measurements(x)[i]
  }
  return(i)
}

#' select var with this method
#' @importFrom reticulate py_get_item
#' @keywords internal
#' @noRd
.index_var <- function(x, ...) {
  i <- .index2name(x, ...)
  return(py_get_item(x, i))
}

#' assign/set var with this method
#' @importFrom reticulate py_set_item
#' @keywords internal
#' @noRd
.set_var <- function(x, ..., value){
  i <- .index2name(x, ...)
  py_set_item(x, i, value)
  return(x)
}

#' select dim with this method
#' @keywords internal
#' @noRd
.index_dim <- function(x, i = NULL, j = NULL, k = NULL, ...) {
  index <- c(list(i, j, k), list(...))

  dims <- dim(x)
  if(length(index) != length(dims)) out("Number of dimensions of object differ from provided index.", type = 3)
  names(index) <- names(dims)
  index <- index[!sapply(index, is.null)]

  if(any(!sapply(index, is.numeric))) out("Index must be numeric for this method.", type = 3)
  index <- lapply(index, function(i) as.integer(i-1))

  # check out of bounds! currently on the Python side

  do.call(x$isel, index)
}

#' get dimensions
#' @keywords internal
#' @noRd
.dim <- function(x) {
  x <- gsub("'", "", strsplit(strsplit(as.character(x$sizes), "\\{")[[1]][2], ",")[[1]])
  x <- sapply(x, function(.x){
    gsub("\\)", "", gsub("\\}", "", .x))
  }, USE.NAMES = F)

  dims <- unlist(lapply(x, function(.x) as.numeric(strsplit(.x, ": ")[[1]][2])))
  names(dims) <- unlist(lapply(x, function(.x) trimws(strsplit(.x, ": ")[[1]][1])))
  dims <- dims[order(names(dims))]
  return(dims)
}

#' @title Methods to extract from or assign to \code{odcr} classes
#'
#' @description `[` allows to subset an `xarray` object by its dimensions (see [`dim()`])
#'
#' @name Extract
#' @md
#'
#' @param i numeric, first dimension index
#' @param j numeric, second dimension index
#' @param k numeric, third dimension index
#'
#' @return Subsetted `xarray` object
#'
#' @examples
#' \dontrun{
#' library(odcr)
#'
#' # connect to a database, store the Daatcube connection internally (default and recommended)
#' database_connect(app = "Sentinel_2")
#'
#' # build a query list
#' lat <- 22.821
#' lon <- 28.518
#' buffer <- 0.05
#'
#' query <- list(
#'   'time' = c('2020-01', '2020-03'),
#'   'x' = c(lon - buffer, lon + buffer),
#'   'y' = c(lat + buffer, lat - buffer),
#'   'output_crs' = 'epsg:6933',
#'   'resolution' = c(-20,20)
#' )
#'
#' # load data and return an xarray object for a query
#' ds <- dc_load(query = c(product = "s2_l2a", dask_chunks = dict(), query))
#'
#' # return dimensions of ds
#' dim(ds)
#'
#' # subset/index ds by time, x, y
#' ds[13, 11:20, 21:30]
#'
#' # subset/index ds by time
#' ds[13,,]
#'
#' # subset/index ds by variable/measurement
#' ds[[2]]
#'
#' # or do the same with a named variable or alias
#' ds[["nir"]]
#'
#' # subset/index by exact time vectors
#' xar_sel_time(ds, c("2020-01-02", "2020-01-07"))
#'
#' # or find closest times/date subsets by time vectors
#' xar_sel_time(ds, c("2020-01-01", "2020-01-07"), exact_match = F)
#' }
#' @export
"[.xarray.core.dataset.Dataset" <- .index_dim

#' @rdname Extract
#' @export
"[.xarray.core.dataarray.DataArray" <- .index_dim


#' @description `[[` allows to subset an `xarray` object by measurements/variables (e.g. spectral band), either named (character) or indexed (numeric).
#'
#' @rdname Extract
#' @md
#'
#' @param ... numeric or character, index or indices by which to extract/assign (additional) elements
#'
#' @export
"[[.xarray.core.dataset.Dataset" <- .index_var

#' @rdname Extract
#' @export
"[[.xarray.core.dataarray.DataArray" <- function(x, ...) return(x)

#' @description `xar_sel_time()` allows to subset an `xarray` object by a character vector of dates/times.
#' @rdname Extract
#' @md
#'
#' @param x `xarray` object, the dataset to be subsetted from or assigned to
#' @param query character vector, one or more time/date character vectors in the format of the time dimension of `x` or an abbreviated form of it (e.g. only the date component)
#' @param exact_match logical, whether to return only exact matches (default) or to search for closest elements to each element in `query` using `[difftime()]`
#'
#' @export
xar_sel_time <- function(x, query, exact_match = T){
  v <- x$time$values
  if(isTRUE(exact_match)){
    subs <- sapply(query, function(.q) which(grepl(.q, v)), USE.NAMES = F)
    if(length(subs) == 0) out("Query times are not matching times of x.", type = 3)
    subs_l <- sapply(subs, length)
    if(all(subs_l == 0)) out("Query times are not matching times of x.", type = 3)
    if(any(subs_l == 0)) out("Some query times are not matching times of x, subsetting only by valid times.", type = 2)
    subs <- unlist(subs)
    x$sel(time = v[subs])
  }else{
    subs <- sapply(as.POSIXct(query), function(.q) which.min(abs(difftime(.q, as.POSIXct(v)))))
    if(any(duplicated(subs))) out("Output contains duplicated times, since some query times are closest to the same times of x.", type = 2)
    x$sel(time = v[subs])
  }
}

#' @description `[[<-` allows to assign a measurement/variable (e.g. spectral band), either named (character) or indexed (numeric), to an existing `xarray` object.
#' @md
#'
#' @param value `xarray` object, the dataset that should be assigned to `x`
#' @rdname Extract
#'
#' @export
"[[<-.xarray.core.dataset.Dataset" <- .set_var

#' @rdname Extract
#'
#' @export
"$<-.xarray.core.dataset.Dataset" <- .set_var


#' Dimensions of \code{odcr} classes
#'
#' `dim()` retrieves dimensions from an `xarray` object.
#'
#' @name dim
#' @md
#'
#' @param x object of which dimensions should be retrieved.
#'
#' @return a (named) vector
#'
#' @examples
#' \dontrun{
#' library(odcr)
#'
#' # connect to a database, store the Daatcube connection internally (default and recommended)
#' database_connect(app = "Sentinel_2")
#'
#' # build a query list
#' lat <- 22.821
#' lon <- 28.518
#' buffer <- 0.05
#'
#' query <- list(
#'   'time' = c('2020-01', '2020-03'),
#'   'x' = c(lon - buffer, lon + buffer),
#'   'y' = c(lat + buffer, lat - buffer),
#'   'output_crs' = 'epsg:6933',
#'   'resolution' = c(-20,20)
#' )
#'
#' # load data and return an xarray object for a query
#' ds <- dc_load(query = c(product = "s2_l2a", dask_chunks = dict(), query))
#'
#' # return dimensions of ds
#' dim(ds)
#' }
#' @export
dim.xarray.core.dataset.Dataset <- .dim

#' dim method
#' @rdname dim
#' @export
dim.xarray.core.dataarray.DataArray <- .dim


#' Plotting of \code{odcr} classes
#'
#' `plot()` plots an `xarray` object using `[stars::plot()]`
#'
#' @name plot
#' @md
#'
#' @param x object which should be plotted
#' @param ... additional arguments passed to [`stars::plot()`]
#'
#' @return a `stars` plot
#'
#' @examples
#' \dontrun{
#' library(odcr)
#'
#' # connect to a database, store the Daatcube connection internally (default and recommended)
#' database_connect(app = "Sentinel_2")
#'
#' # build a query list
#' lat <- 22.821
#' lon <- 28.518
#' buffer <- 0.05
#'
#' query <- list(
#'   'time' = c('2020-01', '2020-03'),
#'   'x' = c(lon - buffer, lon + buffer),
#'   'y' = c(lat + buffer, lat - buffer),
#'   'output_crs' = 'epsg:6933',
#'   'resolution' = c(-20,20)
#' )
#'
#' # load data and return an xarray object for a query
#' ds <- dc_load(query = c(product = "s2_l2a", dask_chunks = dict(), query))
#'
#' # plot first timestamp, spatial subset and only first band/variable
#' plot(ds[1,101:300,101:300][[1]])
#'
#' # plot first timestamp, spatial subset and only two bands/variables
#' plot(ds[1,101:300,101:300][[c(1,2)]])
#' }
#' @export
plot.xarray.core.dataset.Dataset <- function(x, ...) {
  plot(.xarray_convert(x), ...)
}

#' plot method
#' @rdname plot
#' @export
plot.xarray.core.dataarray.DataArray <- function(x, ...) {
  arg <- list(...)
  if(is.null(arg$main)) arg$main <- x$name
  do.call(plot, c(list(x = .xarray_convert(x)), arg))
}


#' @title Arithmetic operators for \code{odcr} classes
#'
#' @description These arithmetic operators perform arithmetic on two objects of the same class, shape and dimensions.
#'
#' @name Arithmetic
#' @md
#'
#' @param xds  `xarray` object
#' @param yds `xarray` object
#'
#' @return An `xarray` object of same class, shape and dimensions as the inputs to the operator, holding the result.
#'
#' @examples
#' \dontrun{
#' library(odcr)
#'
#' # connect to a database, store the Daatcube connection internally (default and recommended)
#' database_connect(app = "Sentinel_2")
#'
#' # build a query list
#' lat <- 22.821
#' lon <- 28.518
#' buffer <- 0.05
#'
#' query <- list(
#'   'time' = c('2020-01', '2020-03'),
#'   'x' = c(lon - buffer, lon + buffer),
#'   'y' = c(lat + buffer, lat - buffer),
#'   'output_crs' = 'epsg:6933',
#'   'resolution' = c(-20,20)
#' )
#'
#' # load data and return an xarray object for a query
#' ds <- dc_load(query = c(product = "s2_l2a", dask_chunks = dict(), query))
#'
#' ndvi <- (ds[["nir"]] - ds[["red"]]) / (ds[["nir"]] + ds[["red"]])
#' ndvi
#'
#' }
#' @export
"+.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  y <- np$add(xds, yds)
  .rm_attr(.rm_name(y), "units")
}

#' @rdname Arithmetic
#' @export
"-.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  y <- np$subtract(xds, yds)
  .rm_attr(.rm_name(y), "units")
}

#' @rdname Arithmetic
#' @export
"/.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  y <- np$divide(xds, yds)
  .rm_attr(.rm_name(y), "units")
}

#' @rdname Arithmetic
#' @export
"*.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  y <- np$multiply(xds, yds)
  .rm_attr(.rm_name(y), "units")
}

#' "xarray.core.dataset.Dataset" class
#'
#' @name xarray.core.dataset.Dataset-class
#' @aliases xarray.core.dataset.Dataset
#' @family xarray.core.dataset.Dataset
#'
#' @importFrom methods setOldClass
#' @exportClass xarray.core.dataset.Dataset
setOldClass("xarray.core.dataset.Dataset")

#' Methods to coerce \code{odcr} classes to native spatial classes
#' @name as
#' @rdname coerce-methods
#' @aliases coerce,xarray.core.dataset.Dataset,stars-method.
#'
#' @importFrom methods setAs
setAs("xarray.core.dataset.Dataset", "stars", function(from) as.stars(from))


#' "xarray.core.dataarray.DataArray" class
#'
#' @name xarray.core.dataarray.DataArray-class
#' @aliases xarray.core.dataarray.DataArray
#' @family xarray.core.dataarray.DataArray
#'
#' @importFrom methods setOldClass
#' @exportClass xarray.core.dataarray.DataArray
setOldClass("xarray.core.dataarray.DataArray")

#' Methods to coerce \code{odcr} classes to native spatial classes
#' @name as
#' @rdname coerce-methods
#' @aliases coerce,xarray.core.dataarray.DataArray,stars-method.
#'
#' @importFrom methods setAs
setAs("xarray.core.dataarray.DataArray", "stars", function(from) as.stars(from))

#' Methods to coerce \code{odcr} classes to native spatial classes
#'
#' [as.stars()] allows to convert `xarray.core.dataset.Dataset` to `stars`.
#'
#' @rdname coerce-methods
#' @name as.stars
#' @md
#'
#' @param from object of class `xarray.core.dataset.Dataset`
#'
#' @return [as.stars()] retruns an object of class `stars`
#'
#' @examples
#' \dontrun{
#' library(odcr)
#'
#' # connect to a database, store the Daatcube connection internally (default and recommended)
#' database_connect(app = "Sentinel_2")
#'
#' # build a query list
#' lat <- 22.821
#' lon <- 28.518
#' buffer <- 0.05
#'
#' query <- list(
#'   'time' = c('2020-01', '2020-03'),
#'   'x' = c(lon - buffer, lon + buffer),
#'   'y' = c(lat + buffer, lat - buffer),
#'   'output_crs' = 'epsg:6933',
#'   'resolution' = c(-20,20)
#' )
#'
#' # load data and return an xarray object for a query
#' ds <- dc_load(query = c(product = "s2_l2a", dask_chunks = dict(), query))
#' ds <- ds[,101:300,101:300]
#'
#' # convert to stars (multi-dimensional and mulit-varariable)
#' ds_stars <- as.stars(ds)
#' ds_stars <- as(ds, "stars")
#'
#' # convert to stars (single variable)
#' ds_stars <- as.stars(ds[[1]])
#'
#' # convert to stars (2-dim, multi-variable)
#' ds_stars <- as.stars(ds[1,,])
#'
#' # convert to raster (multi-dimensional and mulit-varariable)
#' ds_raster <- as.raster(ds)
#' ds_raster <- as(ds, "raster")
#' # raster cannot represent 4-dim objects, thus coereced to list of RasterBricks
#'
#' # convert to starasterrs (single variable)
#' ds_raster <- as.raster(ds[[1]])
#'
#' # convert to raster (2-dim, multi-variable)
#' ds_raster <- as.raster(ds[1,,])
#' }
#'
#' @export
as.stars <- function(from){
  .xarray_convert(from, method = "stars")
}

#' Methods to coerce \code{odcr} classes to native spatial classes
#'
#' [as.raster()] allows to convert `xarray.core.dataset.Dataset` to `RasterLayer` or `RasterBrick`.
#'
#' @rdname coerce-methods
#' @name as.raster
#' @md
#'
#' @param from object of class `xarray.core.dataset.Dataset`
#'
#' @return [as.raster()] retruns an object of class `RasterLayer` or `RasterBrick` or a `list` of such in case of more than 3 dimensions
#'
#' @importFrom utils installed.packages
#' @export
as.raster <- function(from){
  if(!any("raster" == installed.packages())) out("Package 'raster' must be installed to convert to 'Raster*'", type = 3)
  .xarray_convert(from, method = "raster")
}

