
library(devtools)
library(RSQLite)
library(dplyr)
library(httr)
document()
install()
library(BiocFileCache)
example("BiocFileCache-class")

addResource(bfc0, fl1, "webTestWork", rtype="web", weblink="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit")
addResource(bfc0, fl1, "webTestFTP", rtype="web", weblink="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")
addResource(bfc0, fl1, "webTestReDir", rtype="web", weblink="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip")
addResource(bfc0, fl1, "webTestNotFound", rtype="web", weblink="https://hehehaf")



#########################

## example of hub resource (sometimes convenient)
## hub = 'https://annotationhub.bioconductor.org/metadata/annotationhub.sqlite3'

# what is proxy??
.hub_cache_resource <- function(hubpath, cachepath, proxy) {
    ## retrieve file from hub to cache
    tryCatch({
        tmp <- tempfile()
        ## Download the resource in a way that supports https
        if (interactive() && (packageVersion("httr") > "1.0.0")) {
            response <-
                GET(hubpath, progress(), write_disk(tmp), proxy)
            cat("\n") ## line break after progress bar
        } else {
            response <- GET(hubpath, write_disk(tmp), proxy)
        }
        if (length(status_code(response)))  
        {
            # FTP requests return empty status code, see
            # https://github.com/hadley/httr/issues/190
            if (status_code(response) != 302L)
                stop_for_status(response)
        }
        if (!all(file.exists(dirname(cachepath))))
            dir.create(dirname(cachepath), recursive=TRUE)
        file.copy(from=tmp, to=cachepath)
        file.remove(tmp)
        TRUE
    }, error=function(err) {
        warning("download failed",
                "\n  hub path: ", sQuote(hubpath),
                "\n  cache path: ", sQuote(cachepath),
                "\n  reason: ", conditionMessage(err),
                call.=FALSE)
        FALSE
    })
}


hubpath ="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit"
cachepath = "/tmp/Rtmp3f09Rn/file3a223bc58c7c/3a226a3db524"
hubpath ="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz"

.hub_cache_resource <- function(hubpath, cachepath) {
    ## retrieve file from hub to cache
    tryCatch({
        tmp <- tempfile()
        ## Download the resource in a way that supports https
        if (interactive() && (packageVersion("httr") > "1.0.0")) {
            response <-
                GET(hubpath, progress(), write_disk(tmp))
            cat("\n") ## line break after progress bar
        } else {
            response <- GET(hubpath, write_disk(tmp))
        }
        if (length(status_code(response)))  
        {
            if (status_code(response) != 302L)
                stop_for_status(response)
        }
        if (!all(file.exists(dirname(cachepath))))
            dir.create(dirname(cachepath), recursive=TRUE)
        file.copy(from=tmp, to=cachepath)
        file.remove(tmp)
        TRUE
    }, error=function(err) {
        warning("download failed",
                "\n  hub path: ", sQuote(hubpath),
                "\n  cache path: ", sQuote(cachepath),
                "\n  reason: ", conditionMessage(err),
                call.=FALSE)
        FALSE
    })
}
    if (is.null(getAnnotationHubOption("PROXY"))) {
        opt <- getOption("ANNOTATION_HUB_PROXY", "")
        opt <- Sys.getenv("ANNOTATION_HUB_PROXY", opt)
        if (nzchar(opt))
            setAnnotationHubOption("PROXY", opt)
    }
