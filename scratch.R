# scratch.R --------------------------------------------------------------
# Manual integration probes for iearthdata. Each labeled section is a
# self-contained block you can run via ctrl+enter / source-on-save.
# Not part of the package build (see .Rbuildignore).

devtools::load_all() # load the package functions into the session

# ---- 1. Discovery (no auth) --------------------------------------------

ds <- list_datasets()
ds # all 35 datasets
ds[ds$type == "SDC30_EBD", ] # ESD row
ds[ds$id != ds$r_id, ] # rows where id != r_id (68/67)

# Single-dataset metadata + citation
meta <- get_dataset(64L)
meta$name
cat(meta$citation, "\n")
substr(meta$description, 1, 300)

# Catalog tree → leaf paths
cat <- get_catalog(64L)
cat$table
cat$suffix
all_paths <- catalog_paths(cat$catalog)
length(all_paths)
head(all_paths)
catalog_paths(cat$catalog, time = 2020:2022) # year filter

# Try a deeper tree (MODIS water = id 9, two-level: year/tile)
cat9 <- get_catalog(9L)
cat9$table
cat9$suffix
head(catalog_paths(cat9$catalog), 5)
head(catalog_paths(cat9$catalog, time = 2020), 5)

# ---- 2. Geometry handling ----------------------------------------------

to_wkt_polygon(c(116, 39, 117, 40)) # numeric bbox
to_wkt_polygon("POLYGON((0 0,1 0,1 1,0 1,0 0))") # WKT pass-through

# sf interop (if sf is installed)
if (requireNamespace("sf", quietly = TRUE)) {
  geom <- sf::st_as_sfc(
    "POLYGON((116 39, 117 39, 117 40, 116 40, 116 39))",
    crs = 4326
  )
  to_wkt_polygon(geom)
  to_wkt_polygon(sf::st_bbox(geom))
}

# ---- 3. AOI query (no auth) --------------------------------------------

# Small bbox around Beijing, single year
beijing_bbox <- c(116.0, 39.0, 117.0, 40.0)
res <- esd_query(beijing_bbox, years = 2020L)
res
# Inspect geometry column
res$geometry[1]

# Multi-year (a single bbox, several years → cartesian over years × tiles)
res_multi <- esd_query(beijing_bbox, years = 2018:2020)
nrow(res_multi)
table(substr(res_multi$path, nchar(res_multi$path) - 3, nchar(res_multi$path)))

# Empty-bbox sanity (middle of ocean)
res_empty <- esd_query(c(-150, 0, -149, 1), years = 2020L)
nrow(res_empty) # expect 0

# Other dataset via generic query_files
# e.g. Global Wetland Dynamics (id 60)
res_w <- query_files(60L, geometry = beijing_bbox)
head(res_w)

# ---- 4. Auth lever ------------------------------------------------------

# Credentials: get them from https://github.com/shuangchencc/ESD then set:
Sys.setenv(IEARTHDATA_USER = "shuangchen", IEARTHDATA_PASSWORD = "SDC@2024test")
# or pass directly to ie_login(user = ..., password = ...)

# Check pubkey lookup independently of login
key <- iearthdata_public_key()
key # openssl pubkey print method

# Force a fresh fetch (bypass cache)
ie_login(refresh = TRUE)

# Inspect cached auth state
.iearthdata$auth$user_name
.iearthdata$auth$expires_at
token_cache_file(.iearthdata$auth$account)

# JWT exp round-trip
jwt_exp(.iearthdata$auth$token)

# ---- 5. Signed URLs -----------------------------------------------------
res <- esd_query(beijing_bbox, years = 2020L) # picks up new
signed <- get_signed_url(res)
signed[1]
# Sign URLs for the tiles found above
Sys.setenv(CPL_VSIL_CURL_USE_HEAD = "NO")
r <- terra::rast(paste0("/vsicurl/", signed[1]))

terra::plot(r[[1]], main = "First band of first signed URL")

system(glue::glue("gdalinfo '{paste0('/vsicurl/', signed[1])}'"))

curl::curl_download(
  signed[1],
  destfile = tempfile(fileext = ".tif"),
  quiet = TRUE
)


# One-shot convenience
res_signed <- esd_query(beijing_bbox, years = 2020L, signed = TRUE)
res_signed$url

# Verify a URL responds (HEAD)
if (length(signed) > 0 && !is.na(signed[1])) {
  r <- httr2::request(signed[1]) |>
    httr2::req_method("HEAD") |>
    httr2::req_perform()
  httr2::resp_status(r)
  httr2::resp_header(r, "Content-Length")
  httr2::resp_header(r, "Content-Type")
}


ie_logout()
# Confirm cache file removed
list.files(token_cache_dir())
