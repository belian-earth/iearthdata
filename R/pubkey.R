# Fetch and cache the iEarth DataHub RSA public key used to encrypt login
# payloads. The key lives in a hashed Next.js chunk that is lazy-loaded
# only when the login form is visited, so the initial HTML does not
# reference it directly. We resolve it by:
#   1. fetching the index HTML,
#   2. extracting the webpack runtime URL and its `r.p` basePath,
#   3. parsing the runtime for every chunk URL it can emit (literal
#      special-cases like 547/662, plus the `{id: "hash"}` map),
#   4. probing each candidate chunk for `setPublicKey("...")`.
# The verified key at time of writing is bundled as a fallback so the
# package keeps working through brief site outages.

# Bundled fallback: base64 DER SubjectPublicKeyInfo extracted from
# chunk 662-f3e52549b83fe106.js (2026-05-18, 2048-bit RSA).
ie_pubkey_fallback <- paste0(
  "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvrzz4DGWHc6YmK0BZ30LMqZvWTLO",
  "suIzPJn9LrJ++5416UwqpnnR5DxI4NOAdwwAOv7aOdiZ6ny5u8BX5potv+cB3evrcpw5HbxS",
  "bj1kUzfOv4VCnGSdPMRnx/i3DCaQN1ubliJrm/jfGBEVioTNkT+iNxcZZYxazgP1PHJOpmUw",
  "u7LME+zdGSB+y0MIZasmKi6aVFBIHug83ku0lNpA+hdWTJu+Unsl6cD58wf7fSF3zLbb9Cmy",
  "/kg+qcS0QzzBajSXh1UuRm+4KuQZfDRDuIagICtXvrY/u2Ow3Kdw4YGqEMe+TLiuxFoCQO9s",
  "mGCOi9sCFAVrC3DaGPhGYT422QIDAQAB"
)

#' Override the RSA public key used for login encryption
#'
#' Advanced use only. The package fetches the key from the live site on first
#' use and falls back to a bundled constant if the fetch fails. Supply a
#' value here only if you have verified a different key out-of-band.
#'
#' @param b64 Base64-encoded DER `SubjectPublicKeyInfo` (the literal string
#'   passed to `setPublicKey()` in the JS bundle). Pass `NULL` to clear.
#' @return Invisible `NULL`.
#' @export
set_iearthdata_public_key <- function(b64) {
  if (is.null(b64)) {
    .iearthdata$pubkey <- NULL
  } else {
    .iearthdata$pubkey <- read_pubkey_b64(b64)
  }
  invisible(NULL)
}

iearthdata_public_key <- function(refresh = FALSE) {
  if (!refresh && !is.null(.iearthdata$pubkey)) {
    return(.iearthdata$pubkey)
  }
  key <- tryCatch(fetch_public_key_from_site(), error = function(e) NULL)
  if (is.null(key) && !is.null(ie_pubkey_fallback)) {
    key <- tryCatch(read_pubkey_b64(ie_pubkey_fallback), error = function(e) NULL)
  }
  if (is.null(key)) {
    rlang::abort(c(
      "Could not obtain the iEarth DataHub RSA public key.",
      i = "Dynamic fetch from the live site failed and the bundled fallback could not be parsed.",
      i = "Inspect the JS bundle for `setPublicKey(\"...\")` and pass the value to `set_iearthdata_public_key()`."
    ))
  }
  .iearthdata$pubkey <- key
  key
}

read_pubkey_b64 <- function(b64) {
  der <- openssl::base64_decode(b64)
  openssl::read_pubkey(der)
}

fetch_public_key_from_site <- function() {
  html <- fetch_text(paste0(ie_site_root, "/iearthdata/"))
  if (is.null(html)) return(NULL)

  candidates <- discover_chunk_urls(html)
  if (length(candidates) == 0L) return(NULL)

  key_pat <- "setPublicKey\\(\\s*[\"']([A-Za-z0-9+/=]+)[\"']"
  for (url in candidates) {
    js <- fetch_text(url)
    if (is.null(js)) next
    m <- regmatches(js, regexec(key_pat, js))[[1]]
    if (length(m) >= 2 && nzchar(m[2])) {
      return(read_pubkey_b64(m[2]))
    }
  }
  NULL
}

fetch_text <- function(url) {
  tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(ie_user_agent()) |>
      httr2::req_perform() |>
      httr2::resp_body_string(),
    error = function(e) NULL
  )
}

# Build the candidate chunk URL list:
# - all `<basePath>/_next/static/chunks/*.js` paths found in the HTML
# - all chunks resolvable from the webpack runtime (which encodes the
#   lazy-loadable chunks that aren't referenced by the initial HTML)
discover_chunk_urls <- function(html) {
  base <- ie_site_root
  rel_paths <- unique(unlist(regmatches(
    html,
    gregexpr("/[A-Za-z0-9_/-]*_next/static/chunks/[A-Za-z0-9_./-]+\\.js", html)
  )))
  urls <- paste0(base, rel_paths)

  webpack <- rel_paths[grepl("webpack-[A-Za-z0-9]+\\.js$", rel_paths)]
  if (length(webpack) > 0L) {
    runtime <- fetch_text(paste0(base, webpack[1]))
    if (!is.null(runtime)) {
      urls <- unique(c(urls, parse_webpack_chunk_urls(runtime, base)))
    }
  }
  urls
}

# Pull every chunk URL the webpack runtime can produce. Two shapes:
#   "static/chunks/<id>-<hash>.js"   (literal, e.g. 547 and 662)
#   "static/chunks/" + e + "." + {id: "hash"}[e] + ".js"  (default branch)
parse_webpack_chunk_urls <- function(runtime, base) {
  base_path <- regmatches(runtime, regexec('r\\.p\\s*=\\s*"([^"]+)"', runtime))[[1]]
  prefix <- if (length(base_path) >= 2) paste0(base, base_path[2]) else paste0(base, "/iearthdata/_next/")

  literal <- regmatches(runtime, gregexpr('"static/chunks/[^"]+\\.js"', runtime))[[1]]
  literal <- gsub('"', "", literal, fixed = TRUE)
  literal_urls <- paste0(prefix, literal)

  # Hash map for chunks emitted via the default branch.
  map_match <- regmatches(runtime, regexec("\\{([^{}]*)\\}\\)\\[e\\]", runtime))[[1]]
  hash_urls <- character()
  if (length(map_match) >= 2) {
    pairs <- regmatches(map_match[2], gregexpr('(\\d+)\\s*:\\s*"([a-f0-9]+)"', map_match[2]))[[1]]
    for (p in pairs) {
      kv <- regmatches(p, regexec('(\\d+)\\s*:\\s*"([a-f0-9]+)"', p))[[1]]
      if (length(kv) >= 3) {
        hash_urls <- c(hash_urls, paste0(prefix, "static/chunks/", kv[2], ".", kv[3], ".js"))
      }
    }
  }
  unique(c(literal_urls, hash_urls))
}
