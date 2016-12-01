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
    .BiocFileCache(cache=cache)
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

#' @describeIn BiocFileCache Get the number of object in the file cache.
#' @return integer(1) Number of objects in the file cache.
#' @examples
#' length(bfc)
#' @exportMethod length
setMethod("length", "BiocFileCache",
    function(x)
{
    length(dir(bfcCache(x)))
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

#' @export
setGeneric("createDb", function(x) standardGeneric("createDb"))

#' @describeIn BiocFileCache Create the sqlite database to keep track of files
#' @return character(1) The path to the sqlite file
#' @examples
#' createDb(bfc)
#' @aliases createDb
#' @exportMethod createDb
setMethod("createDb", "BiocFileCache",
    function(x)
{
    if (!file.exists(.sql_dbfile(x)))
        .sql_create_db(x)
    else
        .sql_dbfile(x)
})

#' @export
setGeneric("addResource", function(x, rname) standardGeneric("addResource"))

#' @describeIn BiocFileCache Add a resource to the database
#' @param rname Resource name
#' @return character(1) The path to the sqlite file resource was added to
#' @examples
#' addResource(bfc, "TestName")
#' addResource(bfc, "TestName2")
#' addResource(bfc, "TestName")
#' @aliases addResource
#' @exportMethod addResource
setMethod("addResource", "BiocFileCache",
    function(x, rname)
{
    .sql_add_resource(x, rname)
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
setGeneric("removeResource", function(x, rid, rname) standardGeneric("removeResource"))

#' @describeIn BiocFileCache Add a resource to the database
#' @param rid Unique resource id (see rid of ouput from listResource)
#' @return character(1) The path to the sqlite file resource was removed from
#' @examples
#' removeResource(bfc, 1, "TestName")
#' listResources(bfc)
#' @aliases removeResource
#' @exportMethod removeResource
setMethod("removeResource", "BiocFileCache",
    function(x, rid, rname)
{
    .sql_remove_resource(x, rid, rname)
})

#' @export
setGeneric("destroyDb", function(x) standardGeneric("destroyDb"))

#' @describeIn BiocFileCache Destroy the sqlite database
#' @return TRUE if successfully deleted
#' @examples
#' destroyDb(bfc)
#' @aliases destroyDb
#' @exportMethod destroyDb
setMethod("destroyDb", "BiocFileCache",
    function(x)
{
    if (file.exists(.sql_dbfile(x)))
        .sql_destroy_db(x)
})
