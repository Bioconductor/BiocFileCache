.CACHE_FILE <- "BiocFileCache.sqlite"
.CACHE_FILE_LOCK <- "BiocFileCache.sqlite.LOCK"

.CURRENT_SCHEMA_VERSION <- "0.99.4"

.SUPPORTED_SCHEMA_VERSIONS <- c("0.99.1", "0.99.2", "0.99.3", "0.99.4")

.RESERVED <- list(                       # dynamically, in .onLoad?
    TABLES = c("metadata", "resource", "sqlite_sequence"),
    COLUMNS = c(
        "id", "rname", "create_time", "access_time", "rpath", "rtype",
        "fpath", "last_modified_time", "etag", "expires"
    )
)

.biocfilecache_flags <- local({
    update_asked <- FALSE
    create_asked <- FALSE
    ## used for unit tests -- default response to '.util_ask'
    ask_response <- NULL
    list(get_update_asked = function() {
        update_asked
    }, set_update_asked = function() {
        update_asked <<- TRUE
    }, get_create_asked = function() {
        create_asked
    }, set_create_asked = function() {
        create_asked <<- TRUE
    }, get_ask_response = function() {
        ask_response
    }, set_ask_response  = function(value) {
        oresponse <- ask_response
        ask_response <<- value
        invisible(oresponse)
    })
})

.util_standardize_rtype <-
    function(rtype, fpath, action)
{
    stopifnot(length(rtype) == length(fpath),
              length(fpath) == length(action))

    vapply(seq_along(fpath),
           function(i, rtype, fpath, action){
               .util_standardize_rtype_helper(rtype[i], fpath[i], action[i])
           },
           character(1), USE.NAMES=FALSE, rtype=rtype, fpath=fpath,
           action=action)
}

.util_standardize_rtype_helper<-
    function(rtype, fpath, action)
{
    if (identical(unname(rtype), "auto")) {
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
    if (!is.null(.biocfilecache_flags$get_ask_response())) {
        ## unit tests only
        message(txt)
        return(.biocfilecache_flags$get_ask_response())
    }
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
    if (length(rid) == 0L)
        return(bfc)

    cache_info <- .httr_get_cache_info(fpath)
    .sql_set_last_modified(bfc, rid, cache_info[["modified"]])
    .sql_set_etag(bfc, rid, cache_info[["etag"]])
    .sql_set_expires(bfc, rid, cache_info[["expires"]])
    bfc
}

.util_download <-
    function(bfc, rid, proxy, config, call, ...)
{
    rpath <- .sql_get_rpath(bfc, rid)
    fpath <- .sql_get_fpath(bfc, rid)
    status <- Map(
        .httr_download, fpath, rpath,
        MoreArgs = list(proxy = proxy, config = config, ...)
    )
    ok <- vapply(status, isTRUE, logical(1))
    if (!all(ok)) {
        bfcremove(bfc, rid[!ok])
        warning(
            call, " failed; resource removed",
            "\n  rid: ", paste(rid[!ok], collapse = " "),
            "\n  fpath: ", paste(sQuote(fpath[!ok]), collapse = "\n    "),
            "\n  reason: download failed",
            call. = FALSE
        )
    }
    .util_set_cache_info(bfc, rid[ok])

    if (!all(ok))
        stop(call, " failed; see warnings()")
}

.util_download_and_rename <-
    function(bfc, rid, proxy, config, call, fpath = .sql_get_fpath(bfc, rid),
             FUN, ...)
{
    rpath <- .sql_get_rpath(bfc, rid)
    force(fpath)

    # The connection is not actually necessary - but we just use it to
    # handle thread-safe locking, specifically to avoid race conditions
    # from multiple threads choosing the same tempfile name during download.
    info <- .sql_connect_RW(.sql_dbfile(bfc))
    on.exit(.sql_disconnect(info))

    if (missing(FUN))
        FUN <- file.rename

    status <- Map(function(rpath, fpath) {
        temppath <- tempfile(tmpdir=bfccache(bfc))

        status <- .httr_download(fpath, temppath, proxy, config, ...)
        if (!status)
            return("download failed")

        status <- tryCatch({
            st <- FUN(temppath, rpath)
            if (file.exists(temppath)){ file.remove(temppath) }
            st
        }, error = function(err){
            warning("FUN() failed",
                    "\n  reason: ", conditionMessage(err),
                    call.=FALSE)
            FALSE
        })

        status
    }, rpath, fpath)

    ok <- vapply(status, isTRUE, logical(1))
    if (!all(ok))
        warning(
            call, " failed",
            "\n  rid: ", paste(rid[!ok], collapse=" "),
            "\n  file: ", paste(sQuote(fpath)[!ok], collapse = "\n    "),
            "\n  reason: ", paste(unique(unlist(status[!ok])), collapse = ", "),
            call. = FALSE
        )

    .util_set_cache_info(bfc, rid[ok], fpath[ok])

    if (!all(ok))
        stop("download failed; see warnings()", call.=FALSE)
}

.util_export_file <-
    function(bfc, rid, dir)
{

    rtype <- .sql_get_rtype(bfc, rid)
    rpath <- .sql_get_rpath(bfc, rid)
    loc <- file.exists(rpath)
    if (!loc) {
        if (identical(unname(rtype), "web")) {
            vl <- "web"
        } else {
            vl <- NA_character_
        }
    } else {
        if (identical(unname(rtype), "local")) {
            vl <- "local"
        } else {
            newpath <- file.path(dir, basename(rpath))
            file.copy(rpath, newpath)
            vl <- "relative"
        }
    }
    vl
}
