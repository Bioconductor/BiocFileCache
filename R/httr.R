.httr_get_cache_info <-
    function(link)
{
    response = withCallingHandlers({
        HEAD(link)
    }, warning = function(w) {
        invokeRestart("muffleWarning")
    })

    tryCatch({
        ci <- cache_info(response)
        last_mod <- as.character(ci$modified)
        etag <- gsub("\"", "",as.character(ci$etag))
        if (length(last_mod) == 0L) last_mod <- NA_character_
        if (length(etag) == 0L) etag <- NA_character_
        c(etag = etag, modified = last_mod)
    },  error = function(err) {
        c(etag = NA_character_, modified = NA_character_)
    })
}

#' @importFrom utils packageVersion
.httr_download <-
    function(websource, localfile, proxy, config)
{
    ## retrieve file from hub to cache
    tryCatch({
        if (!all(file.exists(dirname(localfile))))
            dir.create(dirname(localfile), recursive=TRUE)

        ## Download the resource in a way that supports https
        if (interactive() && (packageVersion("httr") > "1.0.0")) {
            response <- suppressWarnings({
                GET(websource, progress(), config=config,
                    write_disk(localfile, overwrite=TRUE), proxy=proxy)
            })
            cat("\n") ## line break after progress bar
        } else {
            response <- suppressWarnings({
                GET(websource, write_disk(localfile, overwrite=TRUE),
                    proxy=proxy, config=config)
            })
        }
        if (length(status_code(response))) {
            if (status_code(response) != 302L)
                stop_for_status(response)
        }
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
