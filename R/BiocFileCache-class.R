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
#' bfc <- BiocFileCache(tempfile())  # temporary cache, for examples
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

#' @describeIn BiocFileCache Get a subset of resources from the cache.
#' 
#' @param i Rid numbers
#' @param j Not applicable
#' @param drop Not applicable
#' @return List of resources
#' @exportMethod [
setMethod("[", c("BiocFileCache", "ANY", "missing", "ANY"),
     function(x, i, j, ..., drop=TRUE)
{
    if (missing(i))
        i <- NA_integer_
    stopifnot(is.numeric(i) || anyNA(i))
    .sql_subset_resources(x, i)    
})

#' @describeIn BiocFileCache Get a select resources from the cache.
#' 
#' @return Entry for the resource in the cache
#' @exportMethod [[
setMethod("[[", c("BiocFileCache", "numeric", "missing"),
     function(x, i, j)
{
     stopifnot(length(i) == 1L)
     .sql_get_resource(x, i)
})

#' @export
setGeneric("newResource",
    function(x, rname)
    standardGeneric("newResource"),
    signature="x")
#' @describeIn BiocFileCache Add a resource to the database 
#'
#' @param rname Name of object in file cache
#' @return named character(1) The path to save your object/file.
#' The name of the character is the unique rid for the resource 
#' @examples
#' path <- newResource(bfc, "NewResource")
#' @aliases newResource
#' @exportMethod newResource
setMethod("newResource", "BiocFileCache",
    function(x, rname)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname))
    rid <- .sql_new_resource(x, rname)
    rpath  = .sql_get_cache_file_path(x, rid)
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
#' @param action How to handle the file: Copy to the cache directory, move the
#' file to the cache directory, or leave the file in current location but save
#' the path in the cache
#' @param ... Aditional Arguments to file.copy
#' @return numeric(1) The unique id of the resource in the cache
#' @examples
#' rid1 <- addResource(bfc, "path/to/File", "Test1", "asis")
#' rid2 <- addResource(bfc, "tomy/File", "Test2", "asis")
#' rid3 <- addResource(bfc, "another/path", "Test3", "asis")
#' @aliases addResource
#' @exportMethod addResource
setMethod("addResource", "BiocFileCache",
    function(x, fpath, rname, action=c("copy", "move", "asis"), ...)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname),
              length(fpath) == 1L, is.character(fpath), !is.na(fpath))

    rid <- .sql_new_resource(x, rname)
    switch(match.arg(action),
       asis = {
           .sql_set_cache_file_path(x, rid, fpath)
       },
       copy = {
           if (file.exists(fpath))
               file.copy(fpath, .sql_get_cache_file_path(x, rid), ...)
           else
               message(paste0("File Does Not Exists: ", fpath))
       },
       move = {
           if (file.exists(fpath))
               system(paste0("mv ", fpath, " ",
                             .sql_get_cache_file_path(x, rid)))
           else
               message(paste0("File Does Not Exists: ", fpath))
       })
    rid
})

#' @export
setGeneric("listResources", function(x) standardGeneric("listResources"))

#' @describeIn BiocFileCache list resources in database
#' @return A list of current resources in the database
#' @examples
#' listResources(bfc)
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
#' loadResource(bfc, rid2)
#' @aliases loadResource
#' @exportMethod loadResource
setMethod("loadResource", "BiocFileCache",
    function(x, rid)
{
    sqlfile <- .sql_update_time(x, rid)
    path <- .sql_get_cache_file_path(x, rid)
    path
})

#' @export
setGeneric("updateResource",
    function(x, rid, value, colID, ...)
    standardGeneric("updateResource"),
    signature="x")

#' @describeIn BiocFileCache Update a resource in the cache
#'
#' @param value character(1) replacement value
#' @param colID character(1) either "cache_file_path" or "rname" indicating
#' which parameter to change. Defaults to "cache_file_path"
#' @examples
#' updateResource(bfc, rid3, "updated/Path/to/File")
#' updateResource(bfc, rid3, "newRname", "rname")
#' @aliases updateResource
#' @exportMethod updateResource
setMethod("updateResource", "BiocFileCache",
    function(x, rid, value, colID)
{
    stopifnot(!missing(rid), length(rid) == 1L,
              !missing(value),  length(rid) == 1L)
    if (missing(colID))
        colID = "cache_file_path"
    stopifnot(colID == "cache_file_path" || colID == "rname")
    sqlfile <- .sql_update_time(x, rid)
    if (colID == "cache_file_path"){
        sqlfile <- .sql_set_cache_file_path(x, rid, value)
    } else{
        sqlfile <- .sql_set_rname(x, rid, value)
    }
})

#' @export
setGeneric("removeResource",
    function(x, rids) standardGeneric("removeResource"))

#' @describeIn BiocFileCache Remove a resource to the database.
#' @param rids character() Unique resource ids (see rid of ouput from
#'     listResource).
#' @return character(1) The path to the sqlite file resource was
#'     removed from.
#' @examples
#' removeResource(bfc, rid2)
#' listResources(bfc)
#' @aliases removeResource
#' @exportMethod removeResource
setMethod("removeResource", "BiocFileCache",
    function(x, rids)
{
    .sql_remove_resource(x, rids)
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
                entry$rid, entry$cache_file_path)
            repeat {
                response <- readline(txt)
                doit <- switch(substr(tolower(response), 1, 1),
                               y = TRUE, n = FALSE, NA)
                if (!is.na(doit))
                    break
            }
            if (doit) {
                file <- unlink(entry$cache_file_path, force=TRUE)
                file <- .sql_remove_resource(x, id)
            }
        }
    } else {

        paths <- unname(unlist(lapply(idsToDel,
                                 .sql_get_cache_file_path, bfc=x)))
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
