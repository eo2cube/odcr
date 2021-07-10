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
#' @param lazy logical, whether to use lazy dataset finding or not.
#'
#' @return
#' None or \code{data.frame}
#'
#' @examples
#' \dontrun{
#' library(odcr)
#'
#' # connect to a database
#' database_connect(app = "Sentinel_2")
#'
#' # return a data.frame containing all stored variables/measurements of the current connection
#' # and its aliases, data types etc.
#' dc_list_measurements()
#'
#' # return a data.frame containing all products of the current connection
#' dc_list_products()
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
#' # return a data.frame of all datasets matching the query
#' dc_find_datasets(query = c(product = "s2_l2a", query))
#'
#' # load data and return an xarray object for a query
#' ds <- dc_load(query = c(product = "s2_l2a", dask_chunks = dict(), query))
#'
#' # after a lot of queries, you may want to close open connections
#' dc_close()
#' }
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

