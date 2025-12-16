## Install latest *development* gratia with dependency checks (ggplot2, mgcv, mirai >= 2.3.0)

# --- configuration -------------------------------------------------------------
cran <- "https://cloud.r-project.org"
lib  <- .libPaths()[1]
min_req <- list(ggplot2 = "3.5.0", mgcv = "1.9-0", mirai = "2.3.0")

message("Installing into library: ", lib)
options(repos = c(CRAN = cran))

# --- helpers ------------------------------------------------------------------
vlt <- function(a, b) utils::compareVersion(as.character(a), as.character(b)) < 0

ensure_pkg_version <- function(pkg, min_version = NULL, lib, repos) {
  cur <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  need <- is.na(cur) || (!is.null(min_version) && vlt(cur, min_version))
  if (need) {
    msg <- if (is.null(min_version)) sprintf("Installing %s ...", pkg)
    else sprintf("Installing/upgrading %s to >= %s ...", pkg, min_version)
    message(msg)
    utils::install.packages(pkg, lib = lib, repos = repos)
  } else {
    message(sprintf("%s %s OK%s",
                    pkg, cur,
                    if (!is.null(min_version)) sprintf(" (>= %s)", min_version) else ""))
  }
}

unload_if_loaded <- function(pkg) {
  ns <- paste0("package:", pkg)
  if (ns %in% search()) {
    message("Detaching loaded package: ", pkg)
    try(detach(ns, unload = TRUE, character.only = TRUE), silent = TRUE)
  }
  if (pkg %in% loadedNamespaces()) {
    message("Unloading namespace: ", pkg)
    try(unloadNamespace(pkg), silent = TRUE)
  }
}

check_mirai_daemons_set <- function() {
  ok <- "mirai" %in% rownames(installed.packages(lib.loc = lib)) &&
    "daemons_set" %in% getNamespaceExports("mirai")
  if (!ok) stop("mirai too old: missing 'daemons_set'.")
  invisible(TRUE)
}

# --- 0) Clean loaded namespaces ----------------------------------------------
for (pkg in c("gratia", "mirai", "ggplot2", "mgcv")) unload_if_loaded(pkg)

# --- 1) Ensure ggplot2 / mgcv -------------------------------------------------
ensure_pkg_version("ggplot2", min_req$ggplot2, lib, cran)
ensure_pkg_version("mgcv",    min_req$mgcv,    lib, cran)

# --- 2) Ensure nanonext + mirai >= 2.3.0 --------------------------------------
# First try CRAN
ensure_pkg_version("nanonext", NULL, lib, cran)
ensure_pkg_version("mirai",    min_req$mirai, lib, cran)

# If mirai still too old, pull from r-Universe (author’s repo)
need_mirai <- tryCatch(
  utils::compareVersion(as.character(packageVersion("mirai")), min_req$mirai) < 0,
  error = function(e) TRUE
)
if (need_mirai) {
  message("CRAN provided old mirai; pulling newer from r-Universe …")
  install.packages("nanonext", lib = lib,
                   repos = c("https://shikokuchuo.r-universe.dev", cran))
  install.packages("mirai", lib = lib,
                   repos = c("https://shikokuchuo.r-universe.dev", cran))
}

# Reload / verify export
unload_if_loaded("mirai")
check_mirai_daemons_set()

# --- 3) Install latest dev gratia ---------------------------------------------
install_ok <- TRUE
tryCatch({
  message("\nAttempting r-Universe install of gratia …")
  install.packages(
    "gratia",
    repos = c("https://gavinsimpson.r-universe.dev", cran),
    lib = lib
  )
}, error = function(e) {
  install_ok <<- FALSE
  message("r-Universe install failed: ", conditionMessage(e))
})

if (!install_ok) {
  message("\nFalling back to GitHub (source build) …")
  if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes", lib = lib, repos = cran)
  remotes::install_github("gavinsimpson/gratia", lib = lib, upgrade = "never")
}

# --- 4) Verify ----------------------------------------------------------------
deps <- list(
  gratia  = tryCatch(as.character(packageVersion("gratia")), error = function(e) NA),
  ggplot2 = as.character(packageVersion("ggplot2")),
  mgcv    = as.character(packageVersion("mgcv")),
  mirai   = as.character(packageVersion("mirai")),
  nanonext= as.character(packageVersion("nanonext"))
)
print(deps)

stopifnot(!is.na(deps$gratia))
stopifnot(utils::compareVersion(deps$ggplot2, min_req$ggplot2) >= 0)
stopifnot(utils::compareVersion(deps$mgcv,    min_req$mgcv)    >= 0)
stopifnot(utils::compareVersion(deps$mirai,   min_req$mirai)   >= 0)
stopifnot("daemons_set" %in% getNamespaceExports("mirai"))

message("\nAll set. gratia installed at: ", deps$gratia)
