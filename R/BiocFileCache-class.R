#' @import methods
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

