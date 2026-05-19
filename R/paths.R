# Walk the catalog tree returned by getCatalogTree into a vector of
# leaf-level path strings (the form polygonQuery expects).
#
# The tree shape is:
#   [{label, children: [{label, children: [{label}, ...]}]}]
# where any node missing `children` is a leaf. Paths are built by joining
# labels root-to-leaf with `/`.

catalog_paths <- function(catalog, time = NULL) {
  paths <- unlist(lapply(catalog, walk_node, prefix = ""), use.names = FALSE)
  if (!is.null(time)) {
    paths <- filter_paths_by_time(paths, time)
  }
  paths
}

walk_node <- function(node, prefix) {
  label <- node$label
  path <- if (nzchar(prefix)) paste(prefix, label, sep = "/") else label
  children <- node$children
  if (is.null(children) || length(children) == 0L) {
    return(path)
  }
  unlist(lapply(children, walk_node, prefix = path), use.names = FALSE)
}

# Keep paths that contain any of `time` as a full path segment.
# Used for year filtering on datasets whose catalog has a year level
# (ESD: SDC30_EBD_V001/<year>; MODIS water: <root>/<year>/<tile>).
filter_paths_by_time <- function(paths, time) {
  segs <- as.character(time)
  pat <- paste0("(^|/)(", paste(segs, collapse = "|"), ")(/|$)")
  paths[grepl(pat, paths)]
}
