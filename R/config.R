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
#' @importFrom reticulate use_python py_config import
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
    #Sys.setenv(RETICULATE_PYTHON = python)
    use_python(python, required = T)
  }
  out("'odcr' is using the following python configuration:\n")
  print(py_config())
  cat("\n")

  dc <- try(import("datacube"))

  if(inherits(ds, "try-error")){
    out("Python library 'datacube' cannot be loaded. Is odcr using the correct Python installation? Is 'datacube' installed and working? Are its upstream dependencies installed?", type = 3)
  } else{
    out("Python library 'datacube' is available, 'odcr' configuration successfull.")
  }
}
