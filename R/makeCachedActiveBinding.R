#' makeCachedActiveBinding
#'
#' Like \code{\link{makeActiveBinding}} but the value of the active
#' binding gets only evaluated once and is "remembered".
#'
#' @param sym See \code{\link{makeActiveBinding}} in the \pkg{base}
#'     package.
#' @param fun See \code{\link{makeActiveBinding}} in the \pkg{base}
#'     package.
#' @param env See \code{\link{makeActiveBinding}} in the \pkg{base}
#'     package.
#' @param verbose Set to TRUE to see caching in action (useful for
#'     troubleshooting).
#'
#' @name makeCachedActiveBinding
#' @aliases makeCachedActiveBinding
#' @export makeCachedActiveBinding
#'
#' @examples
#' makeCachedActiveBinding("x", function() runif(1), verbose=TRUE)
#' x
#' x
makeCachedActiveBinding <- function(sym, fun, env=.GlobalEnv, verbose=FALSE)
{
    caching_env <- new.env(parent=emptyenv())
    fun2 <- function(value) {
        if (!missing(value))
            stop("assignment to active binding '", sym, "' is not allowed")
        val <- try(get(sym, envir=caching_env, inherits=FALSE), silent=TRUE)
        if (inherits(val, "try-error")) {
            if (verbose)
                cat("evaluating and caching value ",
                    "for active binding '", sym, "' ... ", sep="")
            val <- fun()
            assign(sym, val, envir=caching_env)
            if (verbose)
                cat("OK\n")
        } else {
            if (verbose)
                cat("using cached value for active binding '", sym, "'\n",
                    sep="")
        }
        val
    }
    makeActiveBinding(sym, fun2, env=env)
}

