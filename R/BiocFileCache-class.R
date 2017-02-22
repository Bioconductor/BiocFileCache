#' @import methods
#' @import RSQLite
#' @import dplyr
#' @import httr
.BiocFileCache = setClass("BiocFileCache",
    slots=c(cache="character"))

#' BiocFileCache class
#'
#' This class represents the location of files stored on disk. Use the
#' return value to add and retrieve files that persist across
#' sessions.
#'
#' @param cache character(1) On-disk location (directory path) of
#'     cache.
#' @return For 'BiocFileCache': A \code{BiocFileCache} instance.
#' @examples
#' bfc <- BiocFileCache()            # global cache
#' bfc
#' @name BiocFileCache-class
#' @aliases BiocFileCache
#' @export BiocFileCache
BiocFileCache <-
    function(cache=path.expand("~/.BiocFileCache"))
{
    if (!file.exists(cache))
        dir.create(cache)
    bfc <- .BiocFileCache(cache=cache)
    .sql_create_db(bfc)
    bfc
}

#' @export
setGeneric("bfcCache", function(x) standardGeneric("bfcCache"))

#' @describeIn BiocFileCache Get the location of the on-disk cache.
#' @param x \code{BiocFileCache} instance.
#' @return For 'bfcCache': character(1) location of the directory
#' containing the cache.
#' @examples
#' bfcCache(bfc)
#' @aliases bfcCache
#' @exportMethod bfcCache
setMethod("bfcCache", "BiocFileCache",
    function(x)
{
    x@cache
})

#' @describeIn BiocFileCache Get the number of object in the file
#'     cache.
#' @return For 'length': integer(1) Number of objects in the file cache.
#' @examples
#' length(bfc)
#' @importFrom stats setNames
#' @exportMethod length
setMethod("length", "BiocFileCache",
    function(x)
{
    bfcinfo(x) %>% summarize_(.dots=setNames(list(~ n()), "n")) %>%
        collect %>% `[[`("n")
})

#' @describeIn BiocFileCache Get a file path for select resources from
#' the cache.
#' @param i Rid numbers
#' @param j Not applicable
#' @return For '[[': rpath for the given resource in the cache
#' @exportMethod [[
setMethod("[[", c("BiocFileCache", "numeric", "missing"),
    function(x, i, j)
{
    stopifnot(length(i) == 1L)
    stopifnot(i %in% .get_all_rids(x))
    sqlfile <- .sql_update_time(x, i)
    .sql_get_rpath(x, i)
})

#' @describeIn BiocFileCache Set the file path of a
#' select resources from the cache.
#' @param value character(1) Replace file path
#' @return For '[[<-': Updated biocFileCache, invisibly
#' @exportMethod [[<-
setReplaceMethod("[[",
c("BiocFileCache", "numeric", "missing", "character"),
    function(x, i, j, ..., value)
{
    stopifnot(length(i) == 1L, is.character(value), length(value) == 1L)
    stopifnot(file.exists(value))
    sqlfile <- .sql_update_time(x, i)
    sqlfile <- .sql_set_rpath(x, i, value)
    x
})

#' @export
setGeneric("bfcnew",
    function(x, rname)
    standardGeneric("bfcnew"),
    signature="x")
#' @describeIn BiocFileCache Add a resource to the database
#'
#' @param rname character(1) Name of object in file cache. For 'bfcupdate' a
#' character vector of replacement rnames
#' @return For 'bfcnew': named character(1) The path to save your object/file.
#' The name of the character is the unique rid for the resource
#' @examples
#' bfc0 <- BiocFileCache(tempfile())         # temporary catch for examples
#' path <- bfcnew(bfc0, "NewResource")
#' path
#' @aliases bfcnew
#' @exportMethod bfcnew
setMethod("bfcnew", "BiocFileCache",
    function(x, rname)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname))
    rid <- .sql_new_resource(x, rname, "local", NA_character_)
    rpath <- .sql_get_rpath(x, rid)
    setNames(rpath, rid)
})

#' @export
setGeneric("bfcadd",
    function(
        x, rname, fpath=NA_character_, rtype=c("auto", "local", "web"),
        action=c("copy", "move", "asis"), proxy="", ...)
    standardGeneric("bfcadd"),
signature="x")

#' @describeIn BiocFileCache Add an existing resource to the database
#'
#' @param fpath character(1) Path to current file location or remote
#' web resource
#' @param rtype character(1) local or web indicating if the resource is a local
#' file or a web resource
#' @param action How to handle the file: create a \code{copy} of
#'     \code{fpath} in the cache directory; \code{move} the file to
#'     the cache directory; or \code{asis} leave the file in current
#'     location but save the path in the cache.
#' @param proxy proxy server
#' @param ... For \code{action="copy"}, additional arguments passed to
#'     \code{file.copy}.
#' @return For 'bfcadd': named character(1) The path to save your object/file.
#' The name of the character is the unique rid for the resource.
#' @examples
#' fl1 <- tempfile(); file.create(fl1)
#' bfcadd(bfc0, "Test1", fl1)                 # copy
#' fl2 <- tempfile(); file.create(fl2)
#' bfcadd(bfc0, "Test2", fl2, action="move")         # move
#' fl3 <- tempfile(); file.create(fl3)
#' add3 <- bfcadd(bfc0, "Test3", fl3, action="asis")         # reference
#' rid3 <- as.integer(names(add3))
#' 
#' bfc0
#' file.exists(fl1)                                # TRUE
#' file.exists(fl2)                                # FALSE
#' file.exists(fl3)                                # TRUE
#'
#' # add a remote resource
#' url <- "http://httpbin.org/get"
#' bfcadd(bfc0, "TestWeb", fpath=url)
#' @aliases bfcadd
#' @exportMethod bfcadd
setMethod("bfcadd", "BiocFileCache",
    function(
        x, rname, fpath=NA_character_, rtype=c("auto", "local", "web"),
        action=c("copy", "move", "asis"), proxy="", ...)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname))
    stopifnot(length(fpath) == 1L, is.character(fpath), !is.na(fpath))
    rtype <- match.arg(rtype)
    action <- match.arg(action)

    if (rtype == "auto") rtype <- .check_rtype(fpath)

    if (rtype=="local"){
        stopifnot(file.exists(fpath))
        rid <- .sql_new_resource(x, rname, rtype, NA_character_)
    }

    if (rtype=="web"){
        temploc <- tempfile()
        wasSuccess <- .download_resource(fpath, temploc, proxy)
        if (wasSuccess){
            rid <- .sql_new_resource(x, rname, rtype, fpath)
            weblink <- .sql_get_field(x, rid, "weblink")
            web_time <- .get_web_last_modified(weblink)
            if (length(web_time) != 0L)
                vl <- .sql_set_modifiedTime(x, rid, web_time)
            action <- "move"
            fpath <- temploc
        }else{
            stop(
                "download failed:",
                "\n file: '", fpath, "'")
        }
    }
    switch(
        action,
        copy = file.copy(fpath, .sql_get_rpath(x, rid), ...),
        move = file.rename(fpath, .sql_get_rpath(x, rid)),
        asis = .sql_set_rpath(x, rid, fpath))

    rpath <- .sql_get_rpath(x, rid)
    setNames(rpath, rid)
})

.check_rtype <- function(path){
    if (startsWith(path, "http") || startsWith(path, "ftp")) {
        "web"
    } else {
        "local"
    }
}


#' @export
setGeneric("bfcinfo",
    function(x, rids)
    standardGeneric("bfcinfo"),
    signature="x")

#' @describeIn BiocFileCache list resources in database
#' @param rids character() List of rids.
#' @return For 'bfcinfo': A list of current resources in the database
#' @examples
#' bfcinfo(bfc0)
#' @aliases bfcinfo
#' @exportMethod bfcinfo
setMethod("bfcinfo", "BiocFileCache",
    function(x, rids)
{
    if (missing(rids))
        rids <- integer(0)
    .sql_get_resource_table(x, rids)
})

#' @export
dim.tbl_bfc <- function(x)
{
    result <- NextMethod("dim")
    result[1] <- .sql_get_nrows(x)
    result
}

#' @export
setGeneric("bfcpath",
    function(x, rid) standardGeneric("bfcpath"))

#' @describeIn BiocFileCache display paths of resource
#' @param rid numeric(1) Unique resource id
#' @return For 'bfcpath': The file path location to load and original source
#' information for web resources.
#' @examples
#' bfcpath(bfc0, rid3)
#' @aliases bfcpath
#' @exportMethod bfcpath
setMethod("bfcpath", "BiocFileCache",
    function(x, rid)
{
    stopifnot(!missing(rid), length(rid) == 1L)
    stopifnot(rid %in% .get_all_rids(x))
    sqlfile <- .sql_update_time(x, rid)
    path <- .sql_get_rpath(x, rid)
    if (.sql_get_field(x, rid, "rtype")=="web"){
        weblink <- .sql_get_field(x, rid, "weblink")
        setNames(c(path, weblink), c(rid, "weblink"))
    }else{
        setNames(path, rid)
    }
})

#' @export
setGeneric("bfcrpath",
    function(x, rids) standardGeneric("bfcrpath"))

#' @describeIn BiocFileCache display rpath of resource
#' @return For 'bfcrpath': The local file path location to load.
#' If no 'rids' are valid, returns NULL.
#' @examples
#' bfcrpath(bfc0, rid3)
#' @aliases bfcrpath
#' @exportMethod bfcrpath
setMethod("bfcrpath", "BiocFileCache",
    function(x, rids)
{
    if (missing(rids))
        rids <- .get_all_rids(x)
    
    helper <- function(i, x0){
        if (i %in% .get_all_rids(x0)) {
            sqlfile <- .sql_update_time(x0, i)
            path <- .sql_get_rpath(x0, i)
            setNames(path, i)
        }
    }
    unlist(lapply(rids, FUN=helper, x0=x))    
})

#' @export
setGeneric("bfcupdate",
    function(x, rids, value, colID, ...)
    standardGeneric("bfcupdate"),
    signature="x")

#' @describeIn BiocFileCache Update a resource in the cache
#'
#' @param rpath character vector of replacement rpaths
#' @param weblink character vector of replacement web resources
#' @examples
#' bfcupdate(bfc0, rid3, rpath=fl3, rname="NewRname")
#' bfc0[[rid3]] = fl1
#' bfcupdate(bfc0, 5, weblink="http://google.com")
#' @aliases bfcupdate
#' @return For 'bfcupdate': Returns updated biocFileCache object, invisibly
#' @exportMethod bfcupdate
setMethod("bfcupdate", "BiocFileCache",
    function(x, rids, rname=NULL, rpath=NULL, weblink=NULL, proxy="")
{

    stopifnot(!missing(rids), all(rids %in% .get_all_rids(x)))
    
    stopifnot(((length(rids) == length(rname)) || is.null(rname)), 
              ((length(rids) == length(rpath)) || is.null(rpath)), 
              ((length(rids) == length(weblink)) || is.null(weblink)))

    if (!is.null(rname)) stopifnot(is.character(rname))
    if (!is.null(rpath)) stopifnot(is.character(rpath))
    if (!is.null(weblink)) stopifnot(is.character(weblink))
    
    for (i in seq_along(rids)){

        sqlfile <- .sql_update_time(x, rids[i])
        
        if (!is.null(rname)){
            sqlfile <- .sql_set_rname(x, rids[i], rname[i])
        }
        
        if (!is.null(rpath)){
            chk <- file.exists(rpath[i])
            if (chk) {
                sqlfile <- .sql_set_rpath(x, rids[i], rpath[i])
            } else {
                message(paste("set rpath failed",
                              "\n rpath not updated.",
                              "\n rid: ", rids[i],
                              "\n rpath: '", rpath[i], "' does not exist.",
                              sep="")) 
            }
        }

        if (!is.null(weblink)){
            chk <- .sql_get_field(x, rids[i], "rtype")=="web"
            if (chk) {
                localpath <- .sql_get_rpath(x, rids[i])
                wasSuccess <- .download_resource(weblink[i], localpath, proxy)
                if (wasSuccess) {
                    sqlfile <- .sql_set_weblink(x, rids[i], weblink[i])
                    web_time <- .get_web_last_modified(weblink[i])
                    if (length(web_time) != 0L) {
                        sqlfile <- .sql_set_modifiedTime(x, rids[i], web_time)
                    } else {
                        sqlfile <- .sql_set_modifiedTime(
                            x, rids[i], as.character(Sys.Date())
                            )
                    }
                } else {
                    message(
                        "download failed",
                        "\n weblink not updated.",
                        "\n rid: ", rids[i],
                        "\n file: '", weblink, "'")
                }                           
            }else{
                message(paste("set weblink failed",
                              "\n weblink not updated.",
                              "\n rid: ", rids[i],
                              "\n Resource rtype is not web.", sep=""))
            }
        }
    } 
   
    invisible(x)
}) 

#' @export
setGeneric("bfcquery",
    function(x, queryValue) standardGeneric("bfcquery"))

#' @describeIn BiocFileCache query resource
#' @param queryValue character vector of patterns to match in resource. It will
#' match the pattern against rname, rpath, and weblink. 
#' @return For 'bfcquery': A list of current resources in the database whose
#' rname, rpath, or weblink contained queryValue. If multiple values are given,
#' the resource must contain all of the patterns. If a resource is not found
#' matching all patterns listed, returns NA.
#' @examples
#' bfcquery(bfc0, "test")
#' @aliases bfcquery
#' @exportMethod bfcquery
setMethod("bfcquery", "BiocFileCache",
    function(x, queryValue)
{
    stopifnot(is.character(queryValue))
    rids <- .sql_query_resource(x, queryValue)
    if (length(rids) == 0L){
        NA
    } else {
        .sql_get_resource_table(x, rids)
    }
})

#' @export
setGeneric("bfcneedsupdate",
    function(x, rids) standardGeneric("bfcneedsupdate"))

#' @describeIn BiocFileCache check if a resource needs to be updated
#' @return For 'bfcneedsupdate': named logical vector if resource needs to
#' be updated. The name is the unique 'rid' for the resource. If no 'rids'
#' are web resources, or no 'rids' are valid, returns NULL
#' @examples
#' bfcneedsupdate(bfc0, 5)
#' @aliases bfcneedsupdate
#' @exportMethod bfcneedsupdate
setMethod("bfcneedsupdate", "BiocFileCache",
    function(x, rids)
{

    if (missing(rids))
        rids <- .get_all_rids(x)

    helper <- function(i, x0){
        if (i %in% .get_all_web_rids(x0)) {
            sqlfile <- .sql_update_time(x0, i)
            file_time <- .sql_get_field(x0, i, "last_modified_time")
            link <- .sql_get_field(x0, i, "weblink")
            web_time <- .get_web_last_modified(link)
            if ((length(file_time) == 0L) || (length(web_time) == 0L)){
                toUpdate <- NA
                message(paste("rid ", i,
                              ": Cannot Determine: Recommend Update.",
                              sep=""))
            }else{
                toUpdate <- as.Date(web_time) > as.Date(file_time)
            }
            setNames(toUpdate, i)
        } else {
            if (i %in% .get_all_rids(x0)) 
                message(paste("rid ", i, ": Is not a web resource.",
                              sep=""))
            setNames(NA, i)
        }            
    }# end helper
    tmp <- unlist(lapply(rids, FUN=helper, x0=x))
    tmp <- tmp[names(tmp) %in% as.character(.get_all_web_rids(x))]
    if (length(tmp) == 0L ) NULL
    else tmp
})

#' @export
setGeneric("bfcdownload",
    function(x, rid, proxy="") standardGeneric("bfcdownload"))

#' @describeIn BiocFileCache Redownload resource to location in cache
#' @examples
#' bfcdownload(bfc0, 5)
#' @aliases bfcdownload
#' @return For 'bfcdownload': Returns rpath to data, invisibly
#' @exportMethod bfcdownload
setMethod("bfcdownload", "BiocFileCache",
    function(x, rid, proxy="")
{
    stopifnot(!missing(rid), length(rid) == 1L)
    stopifnot(.sql_get_field(x, rid, "rtype")=="web")
    stopifnot(rid %in% .get_all_rids(x))
    sqlfile <- .sql_update_time(x, rid)
    downloadFile <- .sql_get_field(x, rid, "weblink")
    saveFile <- .sql_get_field(x, rid, "rpath")
    wasSuccess <- .download_resource(downloadFile, saveFile, proxy)
    if (wasSuccess) {
        web_time <- .get_web_last_modified(downloadFile)
        if (length(web_time) != 0L) {
            sqlfile <- .sql_set_modifiedTime(x, rid, web_time)
        } else {
            sqlfile <- .sql_set_modifiedTime(
                x,
                rid,
                as.character(Sys.Date()))
        }       
    }
    invisible(saveFile)
})

#' @export
setGeneric("bfcremove",
    function(x, rids) standardGeneric("bfcremove"))

#' @describeIn BiocFileCache Remove a resource to the database.
#' If the local file is located in the bcfCache,
#' the file will also be deleted.
#' @examples
#' bfcremove(bfc0, rid3)
#' bfcinfo(bfc0)
#' @aliases bfcremove
#' @return For 'bfcremove': Returns updated biocFileCache object, invisibly
#' @exportMethod bfcremove
setMethod("bfcremove", "BiocFileCache",
    function(x, rids)
{
    for (i in rids) {
        if (i %in% .get_all_rids(x)) {
            rpath <- .sql_get_rpath(x, i)
            if (startsWith(rpath, bfcCache(x)))  unlink(rpath, force=TRUE)
        }
    }
    sqlfile <- .sql_remove_resource(x, rids)
    invisible(x)
})

#' @export
setGeneric("bfcsync",
    function(x, verbose=TRUE) standardGeneric("bfcsync"))

#' @describeIn BiocFileCache sync cache and resource.
#' @param verbose If descriptive message and list of issues should be included
#' as output
#' @return For 'bfcsync': logical if cache is in sync. 'verbose' is TRUE by
#' default, so descriptive messages will also be included
#' @examples
#' bfcsync(bfc0)
#' bfcremove(bfc0, 1)
#' bfcsync(bfc0, FALSE)
#' @aliases bfcsync
#' @exportMethod bfcsync
setMethod("bfcsync", "BiocFileCache",
    function(x, verbose=TRUE)
{
    # files not found
    rids <- .get_rid_filenotfound(x)

    # files untracked in cache location
    files <- file.path(
        bfcCache(x), setdiff(list.files(bfcCache(x)), "BiocFileCache.sqlite"))
    untracked <- setdiff(files, .get_all_rpath(x))

    if ( (length(rids) == 0L) && (length(untracked) == 0L) ){
        if (verbose) message("Cache in sync")
        TRUE
    }else{
        if (verbose) {
            if (length(rids) != 0L) {
                txt = "The following entries have local files
                        specified but not found.
                        Consider updating or removing:"
                message(paste(strwrap(txt, exdent=4), collapse="\n"), "\n\n")
                print(bfcinfo(x, rids))
                message("\n\n\n")
            }
            if (length(untracked) != 0L) {
                txt = "The following entries are in the cache
                        but not being tracked.
                        Consider adding to cache with 'bfcadd()':"
                message(paste(strwrap(txt, exdent=4), collapse="\n"), "\n\n")
                cat(paste(untracked, collapse="\n"))
                message("\n\n")
            }
        }
        FALSE
    }
})


#' @export
setGeneric("cleanCache",
    function(x, days = 120, ask = TRUE) standardGeneric("cleanCache"),
    signature="x")

#' @describeIn BiocFileCache Remove old/unused files in BiocFileCache. If file
#' to be removed is not in the bfcCache location it will not be deleted. 
#'
#' @param days Number of days between accessDate and currentDate; if exceeded
#' entry will be deleted
#' @param ask check if really want to remove cache and files
#' @return For 'cleanCache': TRUE if successfully removed.
#' @examples
#' \dontrun{cleanCache(bfc, ask=FALSE)}
#' @aliases cleanCache
#' @exportMethod cleanCache
setMethod("cleanCache", "BiocFileCache",
    function(x, days = 120, ask=TRUE)
{
    idsToDel <- .sql_clean_cache(x, days)

    if (length(idsToDel) != 0L) {

        if (ask) {
            for (id in idsToDel) {
                doit <- FALSE
                entry <- .sql_get_resource(x, id)
                txt <- sprintf(
                    "Remove from cache id: '%d' and delete file '%s' (y/N): ",
                    entry$rid, entry$rpath)
                repeat {
                    response <- readline(txt)
                    doit <- switch(
                        substr(tolower(response), 1, 1),
                        y = TRUE, n = FALSE, NA)
                    if (!is.na(doit))
                        break
                }
                if (doit) {
                    if (startsWith(entry$rpath, bfcCache(x)))
                        file <- unlink(entry$rpath, force=TRUE)
                    file <- .sql_remove_resource(x, id)
                }
            }
        } else {

            paths <- unlist(lapply(idsToDel, .sql_get_rpath, bfc=x),
                            use.names=FALSE)
            rmMe <- startsWith(paths, bfcCache(x))
            paths <- paths[rmMe]
            if (length(paths) != 0L) file <- unlink(paths, force=TRUE)
            file <- .sql_remove_resource(x, idsToDel)
        }

    }
})

#' @export
setGeneric("removeCache",
    function(x, ask = TRUE) standardGeneric("removeCache"),
    signature="x")

#' @describeIn BiocFileCache Completely remove the BiocFileCache
#'
#' @return For 'removeCache': TRUE if successfully removed.
#' @examples
#' \dontrun{removeCache(bfc, ask=FALSE)}
#' @aliases removeCache
#' @exportMethod removeCache
setMethod("removeCache", "BiocFileCache",
    function(x, ask=TRUE)
{
    doit <- FALSE
    if (ask) {
        txt <- sprintf("remove cache and %d resource (y/N): ", length(x))
        repeat {
            response <- readline(txt)
            doit <- switch(
                substr(tolower(response), 1, 1),
                y = TRUE, n = FALSE, NA)
            if (!is.na(doit))
                break
        }
    } else {
        doit <- TRUE
    }

    if (doit)
        doit <- unlink(bfcCache(x), recursive=TRUE, force=TRUE) == 0
    doit
})

#' @describeIn BiocFileCache Display a \code{BiocFileCache} instance.
#' @param object A \code{BiocFileCache} instance.
#' @exportMethod show
setMethod("show", "BiocFileCache",
    function(object)
{
    cat("class: ", class(object), "\n",
        "bfcCache: ", bfcCache(object), "\n",
        "length: ", length(object), "\n",
        "For more information see: bfcinfo or bfcquery \n",
        sep="")
#    print(bfcinfo(object))
})
