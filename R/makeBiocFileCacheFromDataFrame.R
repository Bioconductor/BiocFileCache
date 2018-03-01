#' Make BiocFileCache objects from an existing data.frame
#'
#' If there are a lot of resources being added this could take some
#' time but if a cache is saved in a permanent location this should
#' only have to be run once. The original data.frame must have the
#' required columns 'rtype', 'fpath', and 'rpath'; See the vignette
#' for more information on the expected information contained in these
#' columns. Similarly, the optional columns 'rname', 'etag',
#' 'last_modified_time', and 'expires' may be included. Any additional columns
#' not listed as required or optional will be kept as an additional
#' metadata table in the BiocFileCache database.
#'
#' @param df data.frame or tibble to convert
#' @param cache character(1) On-disk location (directory path) of
#'     cache. For default location see
#'     \code{\link[rappdirs]{user_cache_dir}}.
#' @param actionLocal If local copy of file should be moved, copied or
#'     left in original location. See 'action' param of bfcadd.
#' @param actionWeb If a local copy of a remote resource already
#'     exists, should the file be copied or moved to the
#'     cache. Locally downloaded remote resources must exist in the
#'     cache location.
#' @param metadataName If there are additional columns of data in the
#'     original data.frame besides required BiocFileCache columns,
#'     this data will be added as a metadata table with this name.
#' @param ... additional arguments passed to `file.copy()`.
#' @param ask logical(1) Confirm creation of BiocFileCache.
#'
#' @return A BiocFileCache object
#' @export
setGeneric("makeBiocFileCacheFromDataFrame",
    function(df, cache,
             actionLocal=c("move","copy","asis"), actionWeb=c("move","copy"),
             metadataName,
             ..., ask = TRUE)
    standardGeneric("makeBiocFileCacheFromDataFrame"),
    signature = "df"
)

#' @rdname makeBiocFileCacheFromDataFrame
#' @aliases makeBiocFileCacheFromDataFrame,ANY-method
#' @exportMethod makeBiocFileCacheFromDataFrame
setMethod("makeBiocFileCacheFromDataFrame", "ANY",
    function(df, cache,
             actionLocal=c("move","copy","asis"), actionWeb=c("move","copy"),
             metadataName,
             ..., ask = TRUE)
{
    stopifnot(is.data.frame(df))
    DF <- as.data.frame(df, stringsAsFactors = FALSE)
    if (missing(cache))
        cache <- user_cache_dir(appname="BiocFileCache")
    stopifnot(
        is.character(cache), length(cache) == 1L, !is.na(cache),
        !dir.exists(cache)
    )
    actionLocal <- match.arg(actionLocal)
    actionWeb <- match.arg(actionWeb)

    .required <- c("rtype", "fpath", "rpath")
    .optional <- c("rname", "etag", "last_modified_time", "expires")
    .possible <- c(.required, .optional)
    if (!all(.required %in% names(DF))) {
        stop("One of the following required columns in not in data.frame:",
             "\n   ", paste(.required, collapse=", "),
             "\n   Please insert into original data.frame")
    }
    .optional <- .optional[.optional %in% names(DF)]
    .available <- c(.required, .optional)
    metadata <- names(DF)[!names(DF) %in% .available]
    if (any(metadata %in% c("rid",.RESERVED$COLUMNS))) {
        nocols <- c("rid", setdiff(.RESERVED$COLUMNS, .possible))
        stop("The following are reserved column names:",
             "\n    ", paste(nocols, collapse=", "),
             "\n    Please rename offending column name.")
    }
    if (length(metadata) != 0)
        stopifnot(!missing(metadataName),
                  is.character(metadataName), length(metadataName) == 1L,
                  !is.na(metadataName), !(metadataName %in% .RESERVED$TABLES))

    # validity of .required columns
    stopifnot(is.character(DF[["rtype"]]),
              is.character(DF[["fpath"]]),
              is.character(DF[["rpath"]]))
    rtype <- DF[["rtype"]]
    fpath <- DF[["fpath"]]
    rpath <- DF[["rpath"]]

    stopifnot(all(rtype %in% c("web", "local")))
    web <- which(rtype == "web")
    if (length(web) != 0L) {
        webpaths <- fpath[web]
        test <- startsWith(webpaths, "http") | startsWith(webpaths, "ftp")
        if (!all(test))
            stop("Some source urls for files identified with 'rtype=web'\n",
                 "  do not start with: http, https, or ftp")
    }
    nonweb <- which(rtype != "web")
    if (length(nonweb) != 0L && !all(file.exists(rpath[nonweb])))
        stop("Not all files identified as 'rtype=local' have existing files")

    # validity of .optional columns
    if (length(.optional) != 0L) {
        check <- vapply(.optional, FUN = function(x, df) {
            is.character(df[[x]])
        }, logical(1), df=DF)
        if (!all(check))
            stop("The following columns must have entries of type 'character':",
                 "\n    ", paste(.optional, collapse=", "))
    }
    if ("last_modified_time" %in% .optional) {
        check <- tryCatch({
            as.Date(DF[["last_modified_time"]])
            TRUE
        }, error=function(e) {
            warning(conditionMessage(e))
            FALSE
        })
        if (!check) {
            stop("Column 'last_modified_time' must have entries of type ",
                 "'character' that can be converted to Date via 'as.Date()'")
        }
        modified <- DF[["last_modified_time"]]
    } else {
        modified <- rep(NA_character_, nrow(DF))
    }

    if ("rname" %in% .optional) {
        rname <- DF[["rname"]]
    } else {
        rname <- fpath
    }

    if ("etag" %in% .optional) {
        etag <- DF[["etag"]]
    } else {
        etag <- rep(NA_character_, nrow(DF))
    }

    if ("expires" %in% .optional) {
        expires <- DF[["expires"]]
    } else {
        expires <- rep(NA_character_, nrow(DF))
    }

    bfc <- BiocFileCache(cache, ask = ask)

    # add resources to cache
    for (i in seq_len(nrow(DF))) {

        if (rtype[i] == "web") {
            npath <- fpath[i]
            action <- actionWeb
        } else {
            npath <- rpath[i]
            action <- actionLocal
        }

        res <- bfcadd(bfc, rname=rname[i], fpath = npath, rtype = "auto",
                      action = action, download=FALSE, ...)
        rid <- names(res)
        .sql_set_last_modified(bfc, rid, modified[i])
        .sql_set_etag(bfc, rid, etag[i])
        .sql_set_expires(bfc, rid, expires[i])
    }

    # if local version of remote exists, copy or move
    for (i in web) {
        cpath <- bfcrpath(bfc, rids=paste0("BFC",i))
        opath <- rpath[i]
        if (file.exists(opath)) {
            switch(actionWeb,
                   copy = file.copy(opath, cpath, ...),
                   move = file.rename(opath, cpath)
                   )
        }
    }

    # create metadata
    if (length(metadata) != 0) {
        tbl <- cbind(rid=paste0("BFC",seq_len(nrow(DF))),
                     DF[,metadata,drop=FALSE])
        bfcmeta(bfc, name=metadataName) <- tbl
    }

    bfc
})
