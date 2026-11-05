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
    function(link, proxy, config)
{
    if(missing(config))
       config <- NULL

    if((length(config)==0)){
        config <- NULL
    } else {
        stopifnot(is.list(config))
    }

    if (missing(proxy)){
        proxy <- ""
    }
    if (proxy == ""){
        proxy <- NULL
    }

    req <- request(link) %>%
        req_method("HEAD")

    # Apply the proxy if it's not NULL
    if (!is.null(proxy)) {
        req <- req %>% req_proxy(proxy)
    }

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
    function(websource, localfile, proxy, progress, config, ...)
{
    tmpfile <- NULL
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

        if(missing(progress)){
            progress=TRUE
        }

        if (!all(file.exists(dirname(localfile)))) dir.create(dirname(localfile), recursive=TRUE)

        # set up request using httr2
        req <- request(websource)
        # This enables progress bars in httr2
        if((interactive() && progress)){
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

        # Create temp file to write to first
        tmpfile <- tempfile(
            pattern = paste0(".", basename(localfile), "-"),
            tmpdir = dirname(localfile)
        )

        ## httr2 req_perform does not have an overwrite
        ## assume overwrite will occur automatically
        ## Perform the request and capture the response
        response <- req_perform(req, path=tmpfile, ...)

        cat("\n") ## line break after progress bar

        # Verify downloaded file size when Content-Length is available
        if (!file.exists(tmpfile) ||
            is.na(file.info(tmpfile)$size) ||
            file.info(tmpfile)$size == 0) {
            stop("Download produced no file or an empty file.")
        }
        .verify_download_checksum(
            websource, tmpfile, proxy, config, download_url = resp_url(response)
        )
        # Only populate live location if temp file
        # if downloaded and can be renamed
        if (!file.rename(tmpfile, localfile)) {
            stop("Could not move downloaded file to cache path: ", localfile)
        }
        tmpfile <- NULL

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
    }, finally = {
         if (!is.null(tmpfile) && file.exists(tmpfile)) {
          unlink(tmpfile)
       }
    })

}


#' @importFrom digest digest
.verify_download_checksum <-
    function(websource, tmpfile, proxy, config, download_url)
{
    # Checksum sidecar retrieval currently supports HTTP(S) only
    if (!grepl("^https?://", websource, ignore.case = TRUE)) {
        return(TRUE)
    }

    # Construct sidecar URLs, preserving query parameters
    checksum_urls <- c(
        sha256 = sub("([?#].*)?$", ".sha256\\1", websource),
        md5 = sub("([?#].*)?$", ".md5\\1", websource)
    )

    for (algo in names(checksum_urls)) {
        req <- request(checksum_urls[[algo]])

        if (!is.null(proxy)) {
            req <- req %>% req_proxy(proxy)
        }

        if (!is.null(config)) {
            req <- req %>% req_options(!!!config)
        }

        # Allow inspection of HTTP error responses such as 404
        req <- req %>%
            req_options(followlocation = FALSE) %>%
            req_error(is_error = function(resp) FALSE)

        response <- req_perform(req)
        status <- resp_status(response)

        # Check whether the sidecar redirects to the actual data resource
        if (status %in% c(301L, 302L, 303L, 307L, 308L)) {
            location <- resp_header(response, "location")

            if (!is.null(location) &&
                identical(location, download_url)) {
                next
            }

            # Follow legitimate redirects to checksum files
            response <- req_perform(
                req %>% req_options(followlocation = TRUE)
            )
            status <- resp_status(response)
        }

        # No checksum available; try the next algorithm
        if (status %in% c(404L, 410L)) {
            next
        }

        if (status < 200L || status >= 300L) {
            stop(
                "Could not retrieve ", algo, " checksum: HTTP ", status
            )
        }

        checksum_text <- resp_body_string(response)

        # Extract the checksum value from the sidecar
        checksum <- regmatches(
            checksum_text,
            regexpr(
                if (algo == "sha256") {
                    "[[:xdigit:]]{64}"
                } else {
                    "[[:xdigit:]]{32}"
                },
                checksum_text,
                perl = TRUE
            )
        )

        if (length(checksum) == 0L || !nzchar(checksum)) {
            stop("Invalid ", algo, " checksum file.")
        }

        expected <- tolower(checksum)
        actual <- digest(
            file = tmpfile,
            algo = algo,
            serialize = FALSE
        )

        if (!identical(expected, tolower(actual))) {
            stop(
                algo, " checksum mismatch.",
                "\n  expected: ", expected,
                "\n  actual: ", actual
            )
        }

        return(TRUE)
    }

    # Neither checksum sidecar was available
    TRUE
}
