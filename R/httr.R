# taken/based on httr::parse_http_date
.fmt_http_date <- function(x, failure = structure(NA_real_, class = "Date")) {
    if (length(x) == 0)
        return(NULL)
    fmts <- c("%a, %d %b %Y %H:%M:%S", "%A, %d-%b-%y %H:%M:%S",
        "%a %b %d %H:%M:%S %Y")
    for (fmt in fmts) {
        parsed <- as.POSIXct(strptime(x, fmt, tz = "GMT"))
        if (all(!is.na(parsed)))
            return(parsed)
    }
    rep(failure, length(x))
}

# taken/based on httr::parse_cache_control
.fmt_cache_control <- function(cachecontrol){
    if (is.null(cachecontrol))
        return(list())
    pieces <- strsplit(cachecontrol, ",")[[1]]
    pieces <- gsub("^\\s+|\\s+$", "", pieces)
    pieces <- tolower(pieces)
    is_value <- grepl("=", pieces)
    flags <- pieces[!is_value]
    keyvalues <- strsplit(pieces[is_value], "\\s*=\\s*")
    keys <- c(rep("flags", length(flags)), lapply(keyvalues,
        "[[", 1))
    values <- c(flags, lapply(keyvalues, "[[", 2))
    stats::setNames(values, keys)
}

# taken/based on httr::cache_info
.get_http_expires <- function(response){
    expires <- .fmt_http_date(resp_header(response, "expires"), Inf) %||% NULL
    control <- .fmt_cache_control(resp_header(response, "cache-control"))
    max_age <- as.integer(control$`max-age`) %||% NULL
    if (!is.null(max_age)) {
        expires <- .fmt_http_date(resp_header(response, "date")) + max_age
    } else {
        if (!is.null(resp_header(response, "expires"))) {
            expires <- .fmt_http_date(resp_header(response, "expires"), -Inf)
        } else {
            expires <- NULL
        }
    }
    expires
}

.httr_get_cache_info <-
    function(link, config)
{
    if(missing(config))
       config <- NULL

    if((length(config)==0)){
        config <- NULL
    } else {
        stopifnot(is.list(config))
    }

    req <- request(link) %>%
        req_method("HEAD")

    # Apply the config if it's not NULL
    if (!is.null(config)) {
        req <- req %>% req_options(!!!config)
    }

    response = tryCatch({
        req %>% req_perform()
    }, warning = function(w) {
        invokeRestart("muffleWarning")
    }, error = function(e){
        message("Error while performing HEAD request.\n",
                "   Proceeding without cache information.")
        response()
    })

    tryCatch({
        etag <- ifelse(resp_header_exists(response, "etag"),
                       gsub("\"", "",as.character(resp_header(response, "etag"))),
                       NA_character_)
        last_mod <- ifelse(resp_header_exists(response, "last-modified"),
                           as.character(.fmt_http_date(resp_header(response, "last-modified"))),
                           NA_character_)
        expires <- .get_http_expires(response)
        expires <- ifelse(is.null(expires), NA_character_, as.character(expires))

        c(etag = etag, modified = last_mod, expires = expires)
    },  error = function(err) {

        if ("etag" %in% names(response$headers)){
            etag = as.character(response[["headers"]]["etag"])
        } else {
            etag = NA_character_
        }
        if ("last-modified" %in% names(response$headers)){
            modified = as.character(response[["headers"]]["last-modified"])
        } else {
            modified = NA_character_
        }
        if ("cache-control" %in% names(response$headers)){
            cachecontrol =
                .fmt_cache_control(as.character(response[["headers"]]["cache-control"]))
            max_age = as.integer(control$`max-age`) %||% NULL
            if (!is.null(max_age)) {
                expires <- .fmt_http_date(as.character(response[["headers"]]["date"])) + max_age
            }else{
                if ("expires" %in% names(response$headers)){
                    expires = as.character(response[["headers"]]["expires"])
                } else {
                    expires = NA_character_
                }
            }
        } else {
            expires = NA_character_
        }

        c(etag = etag, modified = modified, expires = expires)
    })
}

#' @importFrom utils packageVersion
.httr_download <-
    function(websource, localfile, proxy, config, ...)
{

    ## retrieve file from hub to cache
    tryCatch({

        if (proxy == ""){
            proxy <- NULL
        }         

        if((length(config)==0)){
            config <- NULL
        } else {
            stopifnot(is.list(config))
        }

        if (!all(file.exists(dirname(localfile)))) dir.create(dirname(localfile), recursive=TRUE)

        # set up request using httr2
        req <- request(websource)
        # This enables progress bars in httr2
        if(interactive()){
            req <- req %>% req_progress()
        }
        # Apply the proxy if it's not NULL
        if (!is.null(proxy)) {
            req <- req %>% req_proxy(proxy)
        }
        # Apply the config if it's not NULL
        if (!is.null(config)) {
            req <- req %>% req_options(!!!config)
        }
        ## httr2 req_perform does not have an overwrite
        ## assume overwrite will occur automatically
        ## Perform the request and capture the response
        response <- req_perform(req, path=localfile, ...)

        cat("\n") ## line break after progress bar

#  No longer needed?
#        if (length(status_code(response))) {
#            if (status_code(response) != 302L)
#                stop_for_status(response)
#        }

        TRUE
    }, error = function(err) {
        warning("download failed",
            "\n  web resource path: ", sQuote(websource),
            "\n  local file path: ", sQuote(localfile),
            "\n  reason: ", conditionMessage(err),
            call.=FALSE)
        FALSE
    })

}
