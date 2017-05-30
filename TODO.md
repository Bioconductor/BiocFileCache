#########################
#
# quick run 
#
#########################

library(devtools)
library(RSQLite)
library(DBI)
library(dplyr)
library(httr)
library(testthat)
document()
install()
library(BiocFileCache)
example("BiocFileCache-class")


fpath="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit"
fpath="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz"
fpath="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip"
fpath="http://httpbin.org/get"
fpath="https://en.wikipedia.org/wiki/Bioconductor"




## SCHEMA help
library(devtools)
library(RSQLite)
library(dplyr)
library(httr)
library(DBI)
install()
library(BiocFileCache)
source("R/utilities.R")
source("R/sql.R")

bfc = BiocFileCache()

bfc0 <- BiocFileCache(tempfile())
path <- bfcnew(bfc0, "NewResource")


sqlfile <- .sql_dbfile(bfc0)
con <- dbConnect(SQLite(), sqlfile)
dbReadTable(con, "resource")
