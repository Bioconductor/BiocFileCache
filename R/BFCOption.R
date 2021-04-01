#' BFCOption
#' These functions help get and set an R variable CACHE that controls the
#' default caching location.
#' @details
#' Currently the only supported option is CACHE. This controls the default
#' location of the BiocFileCache caching directory. By default the value is
#' established by \code{tools::R_user_dir("BiocFileCache",which="cache")}. This
#' value can also be defaultly set by using system and global environment
#' variables visible \emph{before} the package is loaded. The variable that
#' should be set if utilized is \dQuote{BFC_CACHE}
#' @param arg character(1) option to get or set
#' @param value The value to be assigned to the designated option
#' @return Value of request option or invisible successfully set option
#' @examples
#' origPath = getBFCOption('CACHE')
#' \donttest{ setBFCOption('CACHE', "~/.myBFC") }
#' @name BFCOption
#' @aliases getBFCOption
#' @aliases setBFCOption
#' @author Lori Shepherd
#' @export getBFCOption
#' @export setBFCOption


.bfc_options <- new.env(parent=emptyenv())


.bfc_option_key <- function(key0=c("CACHE"))
    match.arg(key0)

getBFCOption <- function(arg) {
    arg <- .bfc_option_key(toupper(arg))
    .bfc_options[[arg]]
}

setBFCOption <- function(arg, value)
{
    key <- .bfc_option_key(toupper(trimws(arg)))

    .bfc_options[[key]] <- switch(key, CACHE={
        value <- as.character(value)
        stopifnot(length(value)==1)
        value
    })
}
