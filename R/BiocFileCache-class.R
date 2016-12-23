#' @import methods
#' @import RSQLite
#' @import dplyr
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
#' @return A \code{BiocFileCache} instance.
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
#' @return character(1) location of the directory containing the cache.
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
#' @return integer(1) Number of objects in the file cache.
#' @examples
#' length(bfc)
#' @importFrom stats setNames
#' @exportMethod length
setMethod("length", "BiocFileCache",
    function(x)
{
    listResources(x) %>% summarize_(.dots=setNames(list(~ n()), "n")) %>%
        collect %>% `[[`("n")
})

#' @describeIn BiocFileCache Get a file path for select resources from the cache.
#' @param i Rid numbers
#' @param j Not applicable
#' @return rpath for the given resource in the cache
#' @exportMethod [[
setMethod("[[", c("BiocFileCache", "numeric", "missing"),
     function(x, i, j)
{
     stopifnot(length(i) == 1L)
     .sql_get_rpath(x, i)
})

#' @describeIn BiocFileCache Set the file path of a
#' select resources from the cache.
#' @param value character(1) Replace file path 
#' @return Updated BiocFileCache object
#' @exportMethod [[<-
setReplaceMethod("[[",
c("BiocFileCache", "numeric", "missing", "character"),
     function(x, i, j, ..., value)
{
     stopifnot(length(i) == 1L, is.character(value), length(value) == 1L)

     sqlfile <- .sql_update_time(x, i)
     sqlfile <- .sql_set_rpath(x, i, value)
     x
})

#' @export
setGeneric("newResource",
    function(x, rname)
    standardGeneric("newResource"),
    signature="x")
#' @describeIn BiocFileCache Add a resource to the database
#'
#' @param rname character(1) Name of object in file cache
#' @return named character(1) The path to save your object/file.
#' The name of the character is the unique rid for the resource
#' @examples
#' bfc0 <- BiocFileCache(tempfile())         # temporary catch for examples
#' path <- newResource(bfc0, "NewResource")
#' path
#' @aliases newResource
#' @exportMethod newResource
setMethod("newResource", "BiocFileCache",
    function(x, rname)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname))
    rid <- .sql_new_resource(x, rname)
    rpath  = .sql_get_rpath(x, rid)
    setNames(rpath, rid)
})

#' @export
setGeneric("addResource",
    function(x, fpath, rname, action=c("copy", "move", "asis"), ...)
    standardGeneric("addResource"),
    signature="x")

#' @describeIn BiocFileCache Add an existing resource to the database
#'
#' @param fpath character(1) Path to current file location
#' @param action How to handle the file: create a \code{copy} of
#'     \code{fpath} in the cache directory; \code{move} the file to
#'     the cache directory; or \code{asis} leave the file in current
#'     location but save the path in the cache.
#' @param ... For \code{action="copy"}, additional arguments passed to
#'     \code{file.copy}.
#' @return numeric(1) The unique id of the resource in the cache.
#' @examples
#' fl1 <- tempfile(); file.create(fl1)
#' addResource(bfc0, fl1, "Test1")                 # copy
#' fl2 <- tempfile(); file.create(fl2)
#' addResource(bfc0, fl2, "Test2", "move")         # move
#' fl3 <- tempfile(); file.create(fl3)
#' rid3 <- addResource(bfc0, fl3, "Test3", "asis")         # reference
#'
#' bfc0
#' file.exists(fl1)                                # TRUE
#' file.exists(fl2)                                # FALSE
#' file.exists(fl3)                                # TRUE
#' @aliases addResource
#' @exportMethod addResource
setMethod("addResource", "BiocFileCache",
    function(x, fpath, rname, action=c("copy", "move", "asis"), ...)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname),
              length(fpath) == 1L, is.character(fpath), !is.na(fpath))
    stopifnot(file.exists(fpath))

    rid <- .sql_new_resource(x, rname)
    switch(
        match.arg(action),
        copy = file.copy(fpath, .sql_get_rpath(x, rid), ...),
        move = file.rename(fpath, .sql_get_rpath(x, rid)),
        asis = .sql_set_rpath(x, rid, fpath))

    rid
})

#' @export
setGeneric("listResources", function(x) standardGeneric("listResources"))

#' @describeIn BiocFileCache list resources in database
#' @return A list of current resources in the database
#' @examples
#' listResources(bfc0)
#' @aliases listResources
#' @exportMethod listResources
setMethod("listResources", "BiocFileCache",
    function(x)
{
    .sql_get_resource_table(x)
})

#' @export
setGeneric("loadResource",
    function(x, rid) standardGeneric("loadResource"))

#' @describeIn BiocFileCache load resource
#' @param rid numeric(1) Unique resource id
#' @return The file path location to load
#' @examples
#' loadResource(bfc0, rid3)
#' @aliases loadResource
#' @exportMethod loadResource
setMethod("loadResource", "BiocFileCache",
    function(x, rid)
{
    sqlfile <- .sql_update_time(x, rid)
    path <- .sql_get_rpath(x, rid)
    path
})

#' @export
setGeneric("updateResource",
    function(x, rid, value, colID, ...)
    standardGeneric("updateResource"),
    signature="x")

#' @describeIn BiocFileCache Update a resource in the cache
#'
#' @param rpath character(1) replacement value for rpath
#' @examples
#' updateResource(bfc0, rid3, rpath=fl2, rname="NewRname")
#' bfc0[[rid3]] = fl1
#' @aliases updateResource
#' @exportMethod updateResource
setMethod("updateResource", "BiocFileCache",
    function(x, rid, rname=NULL, rpath=NULL)
{
    stopifnot(!missing(rid), length(rid) == 1L)
    sqlfile <- .sql_update_time(x, rid)
    if (!is.null(rname)){
        stopifnot(is.character(rname))
        sqlfile <- .sql_set_rname(x, rid, rname)
    }
    if (!is.null(rpath)){
        stopifnot(is.character(rpath))
        sqlfile <- .sql_set_rpath(x, rid, rpath)
    }
})

#' @export
setGeneric("removeResource",
    function(x, rids) standardGeneric("removeResource"))

#' @describeIn BiocFileCache Remove a resource to the database.
#' @param rids character() Unique resource ids (see rid of ouput from
#'     listResource).
#' @examples
#' removeResource(bfc0, rid3)
#' listResources(bfc0)
#' @aliases removeResource
#' @exportMethod removeResource
setMethod("removeResource", "BiocFileCache",
    function(x, rids)
{
    sqlfile <- .sql_remove_resource(x, rids)
})

#' @export
setGeneric("cleanCache",
    function(x, days = 120, ask = TRUE) standardGeneric("cleanCache"),
    signature="x")

#' @describeIn BiocFileCache Remove old/unused files in BiocFileCache
#'
#' @param days Number of days between accessDate and currentDate; if exceeded
#' entry will be deleted
#' @param ask check if really want to remove cache and files
#' @return TRUE if successfully removed.
#' @examples
#' \dontrun{cleanCache(bfc, ask=FALSE)}
#' @aliases cleanCache
#' @exportMethod cleanCache
setMethod("cleanCache", "BiocFileCache",
    function(x, days = 120, ask=TRUE)
{
    idsToDel <- .sql_clean_cache(x, days)
    if (ask) {
        for (id in idsToDel) {
            doit <- FALSE
            entry <- .sql_get_resource(x, id)
            txt <- sprintf(
                "Remove from cache id: '%d' and delete file '%s' (y/N): ",
                entry$rid, entry$rpath)
            repeat {
                response <- readline(txt)
                doit <- switch(substr(tolower(response), 1, 1),
                               y = TRUE, n = FALSE, NA)
                if (!is.na(doit))
                    break
            }
            if (doit) {
                file <- unlink(entry$rpath, force=TRUE)
                file <- .sql_remove_resource(x, id)
            }
        }
    } else {

        paths <- unname(unlist(lapply(idsToDel,
                                 .sql_get_rpath, bfc=x)))
        file <- unlink(paths, force=TRUE)
        file <- .sql_remove_resource(x, idsToDel)
    }
})

#' @export
setGeneric("removeCache",
    function(x, ask = TRUE) standardGeneric("removeCache"),
    signature="x")

#' @describeIn BiocFileCache Completely remove the BiocFileCache
#'
#' @return TRUE if successfully removed.
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
            doit <- switch(substr(tolower(response), 1, 1),
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
        sep="")
    print(listResources(object))
})
