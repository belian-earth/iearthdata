
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iearthdata

<!-- badges: start -->

<!-- badges: end -->

An R client for the [iEarth
DataHub](https://data-starcloud.pcl.ac.cn/iearthdata). It performs
bounding-box and time-period queries against hosted Earth-observation
datasets and returns pre-signed download URLs for the intersecting
assets. The primary target is the SDC30 Embedded Seamless Data (ESD)
global land-monitoring collection.

## Installation

``` r
# install.packages("pak")
pak::pak("belian-earth/iearthdata")
```

## Credentials

ESD access is gated by credentials issued by the dataset author; see
<https://github.com/shuangchencc/ESD>. Set them once per session:

``` r
Sys.setenv(
  IEARTHDATA_USER     = "your-account",
  IEARTHDATA_PASSWORD = "your-password"
)
```

The JWT returned at login is cached on disk under
`tools::R_user_dir("iearthdata", "cache")` and reused across sessions
until it expires.

## Example: ESD tiles over Beijing for 2020

``` r
library(iearthdata)

ie_login()

# Bounding box around Beijing, then ESD tiles intersecting 2020.
bbox  <- c(116, 39, 117, 40)
tiles <- esd_query(bbox, years = 2020)
tiles
#> # A tibble: 4 x 10
#>     gid file                              size path   geometry  dataset_id  r_id type      suffix table
#>   <int> <chr>                            <dbl> <chr>  <wk_wkt>       <int> <int> <chr>     <chr>  <chr>
#> 1   ... SDC30_EBD_V001_01KAA_2020.tif      ... SDC30… POLYGON(…         64    64 SDC30_EBD .tif   rs_s…
#> ...

# Attach pre-signed download URLs (requires login).
signed <- esd_query(bbox, years = 2020, signed = TRUE)
signed$url[1]
```

Each row carries the tile bbox in a `wk_wkt` geometry column, so the
result can be passed straight to `sf::st_as_sf()` for plotting or
further spatial filtering.

## Reading tiles with GDAL

The signed URLs are method-specific (signed for `GET`), so GDAL’s
default `HEAD` probe against `/vsicurl/` returns a 403. Pass
`vsicurl = TRUE` to have the `url` column returned in GDAL’s
`/vsicurl?use_head=no&url=...` form (GDAL \>= 3.6), which suppresses the
probe per-URL:

``` r
tiles <- esd_query(bbox, years = 2020, signed = TRUE, vsicurl = TRUE)
r <- terra::rast(tiles$url[1])
```

Equivalently, `as_vsicurl()` wraps any existing signed URL:

``` r
terra::rast(as_vsicurl(signed_url))
```

ESD tiles are tiled GeoTIFFs (256x256, DEFLATE, 13 bands `UInt16`) but
are not strict COGs: there are no overviews.
