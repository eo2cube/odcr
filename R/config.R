#' Configure `odcr`
#'
#' This function configures `odcr` to connect with the correct Python environment and check the availability of the `datacube` Python library.
#'
#' @param python character, path to a python or conda executable that should be used by `odcr`. Note that the selected Python environment must have the `datacube` library installed (and its upstream Python and system dependencies must be available).
#' @md
#'
#' @return
#' None
#'
#' @importFrom reticulate py_config import
#' @examples
#' \dontrun{
#' library(odcr)
#'
#' config(python = "/env/bin/python3")
#' }
#' @export
#' @name config
config <- function(python = NA){
  if(!is.na(python)){
    Sys.setenv(RETICULATE_PYTHON = python)
  }
  out("'odcr' is using the following python configuration:")
  py_config()

  tryCatch({
    dc <- import("datacube")
  }, error = function(e) out("Python library 'datacube' cannot be loaded, is your Python configuration correct?", type = 3),
  finally = out("Python library 'datacube' is available, 'odcr' configuration successfull."))
}
