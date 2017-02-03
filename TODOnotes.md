#########################
#
# quick run 
#
#########################

library(devtools)
library(RSQLite)
library(dplyr)
document()
install()
library(BiocFileCache)
example("BiocFileCache-class")





############################
#
# Helpful notes
#
###########################


HTTR package: 

library(httr)
GET(url = NULL, config = list(), ..., handle = NULL)
HEAD(url = NULL, config = list(), ..., handle = NULL)
add_headers(..., .headers = character())
cache_info()

r3 <- HEAD("http://httpbin.org/cache")
headers()
cache_info(r3)


Request Fields: 
If-Modified-Since: Allows a 304 Not Modified to be returned if content is unchanged.
If-None-Match: Allows a 304 Not Modified to be returned if content is unchanged,
see HTTP ETag.

Response Fields:
Age
Date
ETag
Last-Modified

The semantics of the GET method change to a "conditional GET" if the request
message includes an If-Modified-Since, If-Unmodified-Since, If-Match,
If-None-Match, or If-Range header field. A conditional GET method requests that
the entity be transferred only under the circumstances described by the
conditional header field(s). The conditional GET method is intended to reduce
unnecessary network usage by allowing cached entities to be refreshed without
requiring multiple requests or transferring data already held by the client. 

The response to a HEAD request MAY be cacheable in the sense that the
information contained in the response MAY be used to update a previously cached
entity from that resource. If the new field values indicate that the cached
entity differs from the current entity (as would be indicated by a change in
Content-Length, Content-MD5, ETag or Last-Modified), then the cache MUST treat
the cache entry as stale.

https://github.com/hadley/httr/issues/129


etag and last modify doesnt have to exist. 

r3 <-HEAD("https://annotationhub.bioconductor.org/metadata/annotationhub.sqlite3")
# last modified exists but etag does not 
cache_info(r3)

# has last modified but not etag
curl -I ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz

TxDb resource

Develpers - query web resource - 

httpbin.org

library(AnnotationHub)
library(DBI)
hub <- AnnotationHub()
grep("http:", hub$sourceurl, value=TRUE)[1:4]



https://annotationhub.bioconductor.org/metadata/annotationhub.sqlite3"
ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz

http://s3.amazonaws.com/annotationhub/refnet/gerstein-2012.tsv
http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit


library(httr)
h1 = HEAD("http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit")
cache_info(h1)$modified
cache_info(h1)$etag
h1$status

h2 = HEAD("http://s3.amazonaws.com/annotationhub/refnet/gerstein-2012.tsv")
h3 = HEAD("ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")


h4 = HEAD("https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip")
h5 = HEAD("https://www.sec.gov/Archives/edgar/full-index/1993/QTR2/master.gz")



response = withCallingHandlers({
    HEAD(url)
}, warning=function(w) {
    invokeRestart("muffleWarnings")
})

status = tryCatch({
    stop_for_status(repsonse)
}, http_403 = function(e) {
    identity(e)
}, http_error=function(e) {
    stop(e)
}, error=identity)

if (is(status, "error") || is(status, "http_403")) {
    response = GET(url)
    status = stop_for_status(repsonse)
} 

## ok, 'response' is either HEAD(url) or GET(url)
