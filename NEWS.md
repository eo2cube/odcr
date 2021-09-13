# `odcr` changelog

## 0.0.2 (in development)

This release implements additional `xarray` features_

**Features:**

- `xarray` native S3 methods for assigning/replacing variables (see `?odcr::Extract`)

**Bug fixes:**

- The `xarray` attribute "unit" and the `xarray` name of the first object used in an arithmetic operation are not copied to the resulting `xarray` object anymore but stripped instead.


***

## 0.0.1 (pre-release)

This release implements a basic API for interacting with `datacube` and `xarray`.

**Features:**

- basic database connection (see `?odcr::database`)
- `datacube` core `Datacube` class methods (see `?odcr::datacube`)
- `datacube` core data discovery methods (see `?odcr::datacube`
- `datacube` core data loading methods (see `?odcr::datacube`
- `xarray` `xarray.core.dataarray.DataArray` class and formal S3 definition
- `xarray` `xarray.core.dataset.Dataset` class and formal S3 definition
- `xarray` native S3 methods for retrieving dimensions (see `?odcr::dim`)
- `xarray` native S3 methods for indexing by arbitrary number of dimensions (see `?odcr::Extract`)
- `xarray` native S3 methods for indexing by variable (see `?odcr::Extract`)
- `xarray` native S3 methods for indexing by time strings (see `?odcr::Extract`)
- `xarray` native S3 methods for arithmetic operators (see `?odcr::Arithmetic`)
- `xarray` native S3 methods for coercing to native spatial classes `raster*` and `stars` (see `?odcr::as`)
- `xarray` native S3 methods for plotting (see `?odcr::plot`)
- configuration (see `?odcr::config`)



