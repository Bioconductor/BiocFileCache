.get_web_last_modified <-function(bfc, i)
{
    link <- .sql_get_field(bfc, i, "weblink")
    tryHead <- TRUE

    if(substr(link,1,6) == "ftp://"){
        tryHead <- FALSE
        status <- NA
        #response = withCallingHandlers({
        #    suppressWarnings(HEAD(link))
        #})
        #status = NA
    }

    if(tryHead){

        response = withCallingHandlers({
            HEAD(link)
        }, warning=function(w) {
            invokeRestart("muffleWarnings")
        })

        status = tryCatch({
            stop_for_status(response)
        }, http_403 = function(e) {
            identity(e)
        }, http_error=function(e) {
            stop(e)
        }, error=identity)

    }

    if (is(status, "error") || is(status, "http_403") ||
        !tryHead) {
        response = withCallingHandlers(suppressWarnings(GET(link)))
        status = stop_for_status(response)
    }

    cache_info(response)$modified
}

# TODO:
#   function to check validity of url??
#     - keep in mind redirects will be valid but fail in checks like url.exists
