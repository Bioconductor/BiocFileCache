.get_web_last_modified <-function(link)
{
    response = withCallingHandlers({
        HEAD(link)
    }, warning=function(w) {
        invokeRestart("muffleWarning")
    })
    
    status = tryCatch({
        stop_for_status(response)
    }, http_403 = function(e) {
        identity(e)
    }, http_error=function(e) {
        stop(e)
    }, error=identity)    
    
    
    if ((is(status, "error") || is(status, "http_403")) &&
        is.null(cache_info(response)$modified)) {
        response = withCallingHandlers(suppressWarnings(GET(link)))
        status = stop_for_status(response)
    }

    as.character(cache_info(response)$modified)
}


#
#.hub_cache_resource <- function(hubpath, cachepath, proxy) {
# stolen from AnnotationHub


#' @importFrom utils packageVersion
.download_resource <- function(websource, localfile, proxy) {
    ## retrieve file from hub to cache
    tryCatch({
        if (!all(file.exists(dirname(localfile))))
            dir.create(dirname(localfile), recursive=TRUE)

        ## Download the resource in a way that supports https
        if (interactive() && (packageVersion("httr") > "1.0.0")) {
            response <- withCallingHandlers(suppressWarnings(
                GET(websource, progress(),
                    write_disk(localfile, overwrite=TRUE), proxy=proxy)
                ))
            cat("\n") ## line break after progress bar
        } else {
            response <- withCallingHandlers(suppressWarnings(
                GET(websource, write_disk(localfile, overwrite=TRUE),
                    proxy=proxy)
                ))
        }
        if (length(status_code(response)))  
        {
            if (status_code(response) != 302L)
                stop_for_status(response)
        } 
        TRUE
    }, error=function(err) {
        warning("download failed",
                "\n  web resource path: ", sQuote(websource),
                "\n  local file path: ", sQuote(localfile),
                "\n  reason: ", conditionMessage(err),
                call.=FALSE)
        FALSE
    })
}
