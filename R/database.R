#' Connect to a database
#'
#' These functions connect to a database that houses a datacube.
#'
#' @param ... optional arguments to index or configure a connection.
#' @param object logical, whether the connection should be returned or not.
#'
#' @return
#' None or the connection of class 'datacube.api.core.Datacube', if \code{object = T}
#'
#' @export
#' @name database
database_connect <- function(..., object = F){
  if(is.environment(getOption("odcr.dc"))) out("Exisiting datacube connection is being replaced.", type = 2)
  options("odcr.dc" = datacube$Datacube(...))

  if(isTRUE(object)) return(getOption("odcr.dc"))
}

#' @rdname database
#' @export
database_get_active <- function(){
  return(getOption("odcr.dc"))
}
