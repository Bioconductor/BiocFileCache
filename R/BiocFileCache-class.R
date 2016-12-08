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

#' @export
setGeneric("addResource",
    function(x, rname, resource, save=TRUE, ...) standardGeneric("addResource"),
    signature="x")

#' @describeIn BiocFileCache Add a resource to the database
#'
#' @param rname Name of object in file cache
#' @param resource Any R object or path to file
#' @param save logical if object should be saved as file
#' @param ... additional parameters to saveRDS
#' @return numeric(1) The unique id of the resource in the cache
#' @examples
#' rid1 <- addResource(bfc, "TestName", "path/to/File")
#' rid2 <- addResource(bfc, "TestName2", "path/to/File")
#' obj <- list(one = 1, two = 2)
#' rid3 <- addResource(bfc, "TestName", obj)
#' @aliases addResource
#' @exportMethod addResource
setMethod("addResource", "BiocFileCache",
    function(x, rname, resource, save=TRUE, ...)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname))

    # is resource a path to existing file
    check <- length(resource) == 1L && is.character(resource) &&
        !is.na(resource)
    if (check){
        path <- resource
        save <- FALSE
    # resource is an object to save
    } else {
        path <- file.path(bfcCache(x), rname)
    }

    if (save)
        saveRDS(resource, file = path, ...)

    id <- .sql_add_resource(x, rname, path)
    id
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
#' @return A loaded R object
#' @examples
#' loadResource(bfc, rid3)
#' @aliases loadResource
#' @exportMethod loadResource
setMethod("loadResource", "BiocFileCache",
    function(x, rid)
{
    sqlfile <- .sql_update_time(x, rid)
    .sql_load_resource(x, rid)
})


#' @export
setGeneric("updateResource",
    function(x, rid, resource, save=TRUE, ...)
    standardGeneric("updateResource"),
    signature="x")

#' @describeIn BiocFileCache Update a resource in the cache
#'
#' @return numeric(1) The unique id of the resource in the cache
#' @examples
#' obj$two = 3
#' updateResource(bfc, rid3, obj)
#' updateResource(bfc, rid1, "updated/Path/to/File")
#' @aliases updateResource
#' @exportMethod updateResource
setMethod("updateResource", "BiocFileCache",
    function(x, rid, resource, save=TRUE, ...)
{
    path <- as.character(.sql_get_entry(x, rid, "cache_file_path"))
    # is resource a path to existing file
    check <- length(resource) == 1L && is.character(resource) &&
        !is.na(resource)
    if (check){
        path <- resource
        save <- FALSE
    # resource is an object to save
    } else {
        # else resource is an object to save
        if (save)
            saveRDS(resource, file = path, ...)
    }
    # update the path and last_access
    sqlfile <- .sql_update_path(x, rid, path)

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
    if(ask){
        for (id in idsToDel){
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
            if (doit){
                file <- unlink(entry$cache_file_path, force=TRUE)
                file <- .sql_remove_resource(x, id)
            }
        }
    }else{

        paths <- unname(unlist(lapply(idsToDel,
                                 .sql_get_entry, bfc=x, field="cache_file_path")))
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
    }else{
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
})
