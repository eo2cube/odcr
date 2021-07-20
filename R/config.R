#' Configure `odcr`
#'
#' This function configures `odcr` to connect with the correct Python environment and check the availability of the `datacube` Python library.
#'
#' @param python character, path to a python or conda binary/environment that should be used by `odcr`. Note that the selected Python environment must have the `datacube` library installed (and its upstream Python and system dependencies must be available). To set up a datacube environment ready for Pyhton and R 'out of the box', please see [eo2cube's odcbox](https://github.com/eo2cube/odcbox).
#' @param required logical, whether the use of this python install should be forced even if major libraries are missing or not
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
config <- function(python = NA, required = T){
  if(!is.na(python)){
    #Sys.setenv(RETICULATE_PYTHON = python)
    use_python(python, required)
  }
  out("'odcr' is using the following python configuration:\n")
  print(py_config())
  cat("\n")

  dc <- try(import("datacube"), silent = T)

  if(inherits(dc, "try-error")){
    out("Python library 'datacube' cannot be loaded. Did you specify the correct Python binary/environment? Is the 'datacube' module installed for this Python binary/environment and working? Are its upstream dependencies installed? To set up a datacube environment ready for Pyhton and R 'out of the box', please see https://github.com/eo2cube/odcbox", type = 3)
  } else{
    v <- dc$"__version__"
    out(paste0("Linking to datacube ", v, ", 'odcr' configuration successfull."))
  }
}
