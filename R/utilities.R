.CACHE_FILE <- "BiocFileCache.sqlite"

.CURRENT_SCHEMA_VERSION <- "0.99.3"

.SUPPORTED_SCHEMA_VERSIONS <- c("0.99.1", "0.99.2", "0.99.3")

.RESERVED <- list(                       # dynamically, in .onLoad?
    TABLES = c("metadata", "resource", "sqlite_sequence"),
    COLUMNS = c(
        "id", "rname", "create_time", "access_time", "rpath", "rtype",
        "fpath", "last_modified_time", "etag"
    )
)

.biocfilecache_flags <- local({
    update_asked <- FALSE
    create_asked <- FALSE
    list(get_update_asked = function() {
        update_asked
    }, set_update_asked = function() {
        update_asked <<- TRUE
    }, get_create_asked = function() {
        create_asked
    }, set_create_asked = function() {
        create_asked <<- TRUE
    })
})

.util_standardize_rtype <-
    function(rtype, fpath, action)
{
    if (identical(rtype, "auto")) {
        test <- startsWith(fpath, "http") || startsWith(fpath, "ftp")
        if (test)
            rtype <- "web"
        else if (action == "asis")
            rtype <- "local"
        else
            rtype <- "relative"
    } else if (rtype != "local" && action == "asis") {
        warning(
            "action = 'asis' requires rtype = 'local'; ",
            "setting rtype = 'local'"
        )
        rtype <- "local"
    }

    rtype
}

.util_ask <-
    function(..., .interactive = interactive())
{
    if (!.interactive)
        return(FALSE)
    txt <- paste0(..., " (yes/no): ")
    repeat {
        response <- substr(tolower(readline(txt)), 1, 1)
        doit <- switch(response, y = TRUE, n = FALSE, NA)
        if (!is.na(doit))
            break
    }
    doit
}

.util_unlink <-
    function(rpaths, ...)
{
    gc()
    status <- unlink(rpaths, ..., force=TRUE) == 0L
    if (!all(status))
        warning(
            "failed to unlink cache resource(s):",
            "\n  ", paste(sQuote(rpaths[status]), collapse="\n  ")
        )
    gc()
    status
}

.util_set_cache_info <-
    function(bfc, rid, fpath = .sql_get_fpath(bfc, rid))
{
    cache_info <- .httr_get_cache_info(fpath)
    .sql_set_last_modified(bfc, rid, cache_info[["modified"]])
    .sql_set_etag(bfc, rid, cache_info[["etag"]])
    bfc
}

.util_download <-
    function(bfc, rid, proxy, config, call)
{
    rpath <- .sql_get_rpath(bfc, rid)
    fpath <- .sql_get_fpath(bfc, rid)

    status <- .httr_download(fpath, rpath, proxy, config)
    if (!status) {
        bfcremove(bfc, rid)
        stop(
            call, " failed; resource removed",
            "\n  rid: ", rid,
            "\n  fpath: ", sQuote(fpath),
            "\n  reason: download failed",
            call. = FALSE)
    }

    .util_set_cache_info(bfc, rid)
}

.util_download_and_rename <-
    function(bfc, rid, proxy, config, call, fpath = .sql_get_fpath(bfc, rid))
{
    rpath <- .sql_get_rpath(bfc, rid)
    temppath <- tempfile(tmpdir=bfccache(bfc))
    on.exit(unlink(temppath))

    status <- .httr_download(fpath, temppath, proxy, config)
    if (!status)
        stop(
            call, " failed",
            "\n  rid: ", rid,
            "\n  file: ", sQuote(fpath),
            "\n  reason: download failed",
            call. = FALSE
        )

    status <- file.rename(temppath, rpath)
    if (!status)
        stop(
            call, " failed",
            "\n  rid: ", rid,
            "\n  temporary path: ", sQuote(temppath),
            "\n  rpath: ", sQuote(rpath),
            "\n  reason: file.rename() failed",
            call. = FALSE
        )

    .util_set_cache_info(bfc, rid, fpath)
}

.util_export_file <-
    function(bfc, rid, dir)
{

    rtype <- .sql_get_rtype(bfc, rid)
    rpath <- .sql_get_rpath(bfc, rid)
    loc <- file.exists(rpath)
    if (!loc){
        if (identical(rtype, "web")){
            vl <- "web"
        } else {
            vl <- NA_character_
        }
    } else {
        if (identical(rtype, "local")){
            vl <- "local"
        } else {
            newpath <- file.path(dir, basename(rpath))
            file.copy(rpath, newpath)
            vl <- "relative"
        }
    }
    vl
}
