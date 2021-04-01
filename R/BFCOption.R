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
        stopifnot(isSingleString(value))
        value
    })
}
