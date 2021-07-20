# `odcr`

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/odcr)](https://CRAN.R-project.org/package=odcr)
[![R-CMD-check](https://github.com/eo2cube/odcr/workflows/R-CMD-check/badge.svg)](https://github.com/eo2cube/odcr/actions)
[![Package dependencies](https://tinyverse.netlify.com/badge/odcr)](https://CRAN.R-project.org/package=odcr)
<!-- badges: end -->

`odcr` is an R package that serves as a simple interface to the *Open Data Cube*. As such, it facilitates interaction with an *Open Data Cube* instance to access, query, list and load data as a native `xarray`-equivalent class. It implements basic methods to subset/index, plot and execute basic arithmetic operations on data and convert to native spatial raster classes such as `stars` or `raster`.

## Features (in development)

- [x] basic database connection (see `?odcr::database`)
- [x] `datacube` core `Datacube` class methods (see `?odcr::datacube`)
- [x] `datacube` core data discovery methods (see `?odcr::datacube`
- [x] `datacube` core data loading methods (see `?odcr::datacube`
- [ ] `datacube` core grid processing methods
- [ ] `datacube` core indexing methods
- [ ] support for native proxy class for streaming values directly (such as `stars_proxy`)
- [x] `xarray` `xarray.core.dataarray.DataArray` class and formal S3 definition
- [x] `xarray` `xarray.core.dataset.Dataset` class and formal S3 definition
- [x] `xarray` native S3 methods for retrieving dimensions (see `?odcr::dim`)
- [x] `xarray` native S3 methods for indexing by arbitrary number of dimensions (see `?odcr::Extract`)
- [x] `xarray` native S3 methods for indexing by variable (see `?odcr::Extract`)
- [x] `xarray` native S3 methods for indexing by time strings (see `?odcr::Extract`)
- [x] `xarray` native S3 methods for arithmetic operators (see `?odcr::Arithmetic`)
- [x] `xarray` native S3 methods for coercing to native spatial classes `raster*` and `stars` (see `?odcr::as`)
- [x] `xarray` native S3 methods for plotting (see `?odcr::plot`)
- [ ] unit tests
- [x] configuration (see `?odcr::config`)
- [ ] ...


## System requirements

`odcr` requires the *Open Data Cube* Python library `datacube` which needs to be installed as part of a working *Open Data Cube* installation. See [odcbox](https://github.com/eo2cube/odcbox), if you want to install and set-up an *Open Data Cube* container environment that meets all `odcr` system requirements "out-of-the-box".

In case you have multiple Python installations or (virtual) environments installed, use `odcr::config` to make sure that `odcr` is using the correct Python installation and environment so that it can load and interact with the `datacube` library.

## Installation

`odcr` is in active development and thus breaking changes to its interface need to be expected. Install the latest development version of `odcr` from GitHub:

```r
devtools::install_github("eo2cube/odcr")
```

## Build instructions

In case you want to build the package manually, clone the repository and `cd` to its main directory. `odcr` uses `roxygen2` for documentation. Make sure, `devtools`, `roxygen2` and `pkgdown` are installed. Run:

```R
source("build.R")
```

## Get Started

See the vignette [*Using odcr*](https://eo2cube.github.io/odcr/articles/using_odcr.html) for a full introduction on how to use `odcr` with Sentinel-2 example data. Documentation and examples are available at [https://eo2cube.github.io/odcr/](https://eo2cube.github.io/odcr/).

## List of functions and methods

See [the package index](https://eo2cube.github.io/odcr/reference/index.html).




