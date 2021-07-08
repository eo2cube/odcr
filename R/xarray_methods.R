#' select var with this method
#' @importFrom reticulate py_get_item
#' @keywords internal
#' @noRd
.index_var <- function(ds, ...) {
  i <- list(...)[[1]]

  if(is.numeric(i)){
    i <- .get_measurements(ds)[i]
  }
  return(py_get_item(ds, i))
}

#' select dim with this method
#' @keywords internal
#' @noRd
.index_dim <- function(ds, i = NULL, j = NULL, k = NULL, ...) {
  index <- c(list(i, j, k), list(...))

  dims <- dim(ds)
  if(length(index) != length(dims)) out("Number of dimensions of object differ from provided index.", type = 3)
  names(index) <- names(dims)
  index <- index[!sapply(index, is.null)]

  if(any(!sapply(index, is.numeric))) out("Index must be numeric for this method.", type = 3)
  index <- lapply(index, function(i) as.integer(i-1))

  # check out of bounds! currently on the Python side

  do.call(ds$isel, index)
}

#' get dimensions
#' @keywords internal
#' @noRd
.dim <- function(ds) {
  x <- gsub("'", "", strsplit(strsplit(as.character(ds$sizes), "\\{")[[1]][2], ",")[[1]])
  x <- sapply(x, function(.x){
    gsub("\\)", "", gsub("\\}", "", .x))
  }, USE.NAMES = F)

  dims <- unlist(lapply(x, function(.x) as.numeric(strsplit(.x, ": ")[[1]][2])))
  names(dims) <- unlist(lapply(x, function(.x) trimws(strsplit(.x, ": ")[[1]][1])))
  dims <- dims[order(names(dims))]
  return(dims)
}

#' @title Methods to extract from \code{odcr} classes
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
#' @param ... numeric or character, index or indices by which to extract (additional) elements
#'
#' @export
"[[.xarray.core.dataset.Dataset" <- .index_var

#' @rdname Extract
#' @export
"[[.xarray.core.dataarray.DataArray" <- function(ds, ...) return(ds)

#' @description `xar_sel_time()` allows to subset an `xarray` object by a character vector of dates/times.
#' @rdname Extract
#' @md
#'
#' @param ds `xarray` object, the dataset to be subsetted
#' @param query character vector, one or more time/date character vectors in the format of the time dimension of `ds` or an abbreviated form of it (e.g. only the date component)
#' @param exact_match logical, whether to return only exact matches (default) or to search for closest elements to each element in `query` using `[difftime()]`
#'
#' @export
xar_sel_time <- function(ds, query, exact_match = T){
  x <- ds$time$values
  if(isTRUE(exact_match)){
    subs <- sapply(query, function(.q) which(grepl(.q, x)), USE.NAMES = F)
    if(length(subs) == 0) out("Query times are not matching ds times.", type = 3)
    ds$sel(time = x[subs])
  }else{
    subs <- sapply(as.POSIXct(query), function(.q) which.min(abs(difftime(.q, as.POSIXct(x)))))
    ds$sel(time = x[subs])
  }
}



#' Dimensions of \code{odcr} classes
#'
#' `dim()` retrieves dimensions from an `xarray` object.
#'
#' @name dim
#' @md
#'
#' @param ds dataset of which dimensions should be retrieved.
#'
#' @return a (named) vector
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
#' @param ds dataset of which should be plotted
#' @param ... additional arguments passed to `[stars::plot()]`
#'
#' @return a starsplot
#' @export
plot.xarray.core.dataset.Dataset <- function(ds, ...) {
  plot(.xarray_convert(ds), ...)
}

#' plot method
#' @rdname plot
#' @export
plot.xarray.core.dataarray.DataArray <- function(ds, ...) {
  arg <- list(...)
  if(is.null(arg$main)) arg$main <- ds$name
  do.call(plot, c(list(x = .xarray_convert(ds)), arg))
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
#' @export
"+.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$add(xds, yds)
}

#' @rdname Arithmetic
#' @export
"-.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$subtract(xds, yds)
}

#' @rdname Arithmetic
#' @export
"/.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$divide(xds, yds)
}

#' @rdname Arithmetic
#' @export
"*.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$multiply(xds, yds)
}

#' "xarray.core.dataset.Dataset" class
#'
#' @name xarray.core.dataset.Dataset-class
#' @aliases xarray.core.dataset.Dataset
#' @family xarray.core.dataset.Dataset
#'
#' @exportClass xarray.core.dataset.Dataset
setOldClass("xarray.core.dataset.Dataset")

#' Methods to coerce \code{odcr} classes to native spatial classes
#' @name as
#' @rdname coerce-methods
#' @aliases coerce,xarray.core.dataset.Dataset,stars-method.
#' @exportMethod coerce
setAs("xarray.core.dataset.Dataset", "stars", function(from) as.stars(from))


#' "xarray.core.dataarray.DataArray" class
#'
#' @name xarray.core.dataarray.DataArray-class
#' @aliases xarray.core.dataarray.DataArray
#' @family xarray.core.dataarray.DataArray
#'
#' @exportClass xarray.core.dataarray.DataArray
setOldClass("xarray.core.dataarray.DataArray")

#' Methods to coerce \code{odcr} classes to native spatial classes
#' @name as
#' @rdname coerce-methods
#' @aliases coerce,xarray.core.dataarray.DataArray,stars-method.
#' @exportMethod coerce
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
#' @export
as.raster <- function(from){
  .xarray_convert(from, method = "raster")
}

