devtools::document()
system(paste0("R CMD INSTALL ", getwd()))
pkgdown::build_site(preview = F)
