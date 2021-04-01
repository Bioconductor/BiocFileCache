.onLoad <- function(libname, pkgname, ...) {
    ## options from getOption or Sys.env or default, in that order
    if (is.null(getBFCOption("CACHE"))) {
        path <- tools::R_user_dir("BiocFileCache", which="cache")
        opt <- getOption("BFC_CACHE", path)
        opt <- Sys.getenv("BFC_CACHE", opt)
        setBFCOption("CACHE", opt)
    }
}
