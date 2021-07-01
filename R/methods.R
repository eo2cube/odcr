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
  x <- gsub("'", "", strsplit(gsub("\\}\\)\\)", "", strsplit(as.character(ds$sizes), "\\{")[[1]][2]), ",")[[1]])
  dims <- unlist(lapply(x, function(.x) as.numeric(strsplit(.x, ": ")[[1]][2])))
  names(dims) <- unlist(lapply(x, function(.x) trimws(strsplit(.x, ": ")[[1]][1])))
  dims <- dims[order(names(dims))]
  return(dims)
}

#' @noRd
#' @export
"[[.xarray.core.dataset.Dataset" <- .index_var

#' @noRd
#' @export
"[[.xarray.core.dataarray.DataArray" <- function(ds) return(ds)

#' @noRd
#' @export
"[.xarray.core.dataset.Dataset" <- .index_dim

#' @noRd
#' @export
"[.xarray.core.dataarray.DataArray" <- .index_dim

#' dim method
#' @noRd
#' @export
dim.xarray.core.dataset.Dataset <- .dim

#' dim method
#' @noRd
#' @export
dim.xarray.core.dataarray.DataArray <- .dim

#' plot method
#' @noRd
#' @export
plot.xarray.core.dataset.Dataset <- function(ds) {
  plot(.xarray_convert(ds))
}

#' plot method
#' @noRd
#' @export
plot.xarray.core.dataarray.DataArray <- function(ds) {
  plot(.xarray_convert(ds), main = ds$name)
}

#' add method
#' @noRd
#' @export
"+.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$add(xds, yds)
}

#' substract method
#' @noRd
#' @export
"-.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$subtract(xds, yds)
}

#' divide method
#' @noRd
#' @export
"/.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$divide(xds, yds)
}

#' divide method
#' @noRd
#' @export
"*.xarray.core.dataarray.DataArray" <- function(xds, yds) {
  np$multiply(xds, yds)
}


