#' @import methods
#' @import RSQLite
#' @import dplyr
.BiocFileCache = setClass("BiocFileCache",
    slots=c(cache="character"))

#' BiocFileCache class
#'
#' This class represents the location of files stored on disk. Use the return
#' value to add and retrieve files that persist across sessions.
#'
#' @param cache character(1) On-disk location (directory path) of cache.
#' @return A \code{BiocFileCache} instance.
#' @examples
#' bfc <- BiocFileCache()
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
#' bfc
#' @aliases bfcCache
#' @exportMethod bfcCache
setMethod("bfcCache", "BiocFileCache",
   function(x)
{
    x@cache
})

#' @describeIn BiocFileCache Get the number of object in the file cache.
#' @return integer(1) Number of objects in the file cache.
#' @examples
#' length(bfc)
#' @exportMethod length
setMethod("length", "BiocFileCache",
    function(x)
{
    ## FIXME: query listResources() for summarize(n=n())
    ## listResources(bfc) %>% summarize(n=n()) %>% .[[n]]
    length(dir(bfcCache(x))) - 1L
})    

#' @export
setGeneric("addResource",
    function(x, rname, resource) standardGeneric("addResource"),
    signature="x")

#' @describeIn BiocFileCache Add a resource to the database
#'
#' @param resource Any R object.
#' @return character(1) The path to the sqlite file resource was added to
#' @examples
#' rid1 <- addResource(bfc, "TestName")
#' rid2 <- addResource(bfc, "TestName2")
#' rid3 <- addResource(bfc, "TestName")
#' @aliases addResource
#' @exportMethod addResource
setMethod("addResource", "BiocFileCache",
    function(x, rname, resource)
{
    stopifnot(length(rname) == 1L, is.character(rname), !is.na(rname))
    fname <- .sql_add_resource(x, rname)
    saveRDS(resource, fname)
    ## FIXME: return rid
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
setGeneric("removeResource",
    function(x, rids) standardGeneric("removeResource"))

#' @describeIn BiocFileCache Add a resource to the database.
#' @param rids character() Unique resource ids (see rid of ouput from
#'     listResource).
#' @return character(1) The path to the sqlite file resource was
#'     removed from.
#' @examples
#' removeResource(bfc, 1, "TestName")
#' listResources(bfc)
#' @aliases removeResource
#' @exportMethod removeResource
setMethod("removeResource", "BiocFileCache",
    function(x, rids)
{
    .sql_remove_resource(x, rids)
})

#' @export
setGeneric("removeCache",
    function(x, ask = TRUE) standardGeneric("removeCache"),
    signature="x")

#' @describeIn BiocFileCache Completely remove the BiocFileCache
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
            repsonse <- readline(txt)
            doit <- switch(substr(tolower(answer), 1, 1),
                           y = TRUE, n = FALSE, NA)
            if (!is.na(doit))
                break
        }
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
