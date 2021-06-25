# global reference to datacube
datacube <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  datacube <<- reticulate::import("datacube", delay_load = TRUE)
}