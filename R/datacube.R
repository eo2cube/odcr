#' Query a datacube
#'
#' These functions query a datacube to which a connection has been established using \code{\link{database_connect}}.
#'
#' @param dc a datacube connection of class 'datacube.api.core.Datacube'. By default, the session connection initialized using \code{connect()} is used.
#' @param query list, containing query parameters such as
#' \itemize{
#'    \item \code{'product'}, character, name of the product to be queries (see \code{dc_list_products()})
#'    \item \code{'time'}, character vector of length == 2, containing a time query
#'    \item \code{'x'}, numeric vector of length == 2, containing xmin and xmax
#'    \item \code{'y'}, numeric vector of length == 2, containing ymin and ymax
#'    \item \code{'output_crs'}, character, the output CRS, e.g. as epsg in the format \code{'epsg:6933'}
#'    \item \code{'resolution'}, numeric vector of length == 2
#'    \item \code{'measurements'}, character vector containing the names or aliases of measurements
#' }
#' @param ... additional optional arguments.
#'
#' @return
#' None or \code{data.frame}
#'
#' @export
#' @name datacube
dc_close <- function(dc = getOption("odcr.dc")){
  if(!is.environment(dc)) out("Connect to a datacube to close open connections.", type = 3)
  .check_class(dc, "dc", "datacube.api.core.Datacube")

  dc$close()
}

#' @rdname datacube
#' @export
dc_find_datasets <- function(dc = getOption("odcr.dc"), query, lazy = FALSE){
  if(!is.environment(dc)) out("Connect to a datacube to find datasets.", type = 3)
  .check_class(dc, "dc", "datacube.api.core.Datacube")

  # transform query
  query <- .transform_query(dc, query)

  # query datasets
  if(isFALSE(lazy)){
    ds_list <- do.call(dc$find_datasets, query)
  } else{
    ds_list <- do.call(dc$find_datasets_lazy, query)
  }

  # transpose paths
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

#' @rdname datacube
#' @export
dc_list_measurements <- function(dc = getOption("odcr.dc")){
  if(!is.environment(dc)) out("Connect to a datacube to list measurements.", type = 3)
  .check_class(dc, "dc", "datacube.api.core.Datacube")
  as.data.frame(dc$list_measurements(), stringAsFactors = F)
}

#' @rdname datacube
#' @export
dc_list_products <- function(dc = getOption("odcr.dc")){
  if(!is.environment(dc)) out("Connect to a datacube to list products.", type = 3)
  .check_class(dc, "dc", "datacube.api.core.Datacube")
  as.data.frame(dc$list_products(), stringAsFactors = F)
}

#' @rdname datacube
#' @importFrom reticulate dict
#' @export
dc_load <- function(dc = getOption("odcr.dc"), query){
  .check_class(dc, "dc", "datacube.api.core.Datacube")

  if(is.null(query$dask_chunks)) query$dask_chunks <- dict()
  ds <- do.call(dc$load, query)
  return(ds)
}

