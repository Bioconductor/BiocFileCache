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

## meta data testing
x = BiocFileCache()

bfcinfo()

bfclistmeta()

meta = as.data.frame(list(rid=c("BFC1", "BFC3"), info=c("something", "to add"),
num=c(1,3)))
meta2 = as.data.frame(list(rid=c("BFC5", "BFC6"), new=c("blah", "foo"), info=c(4,6)))

bfcaddmeta(meta=meta)
bfcaddmeta(meta=meta2)
bfcaddmeta(meta=meta2, name="secondname")

bfclistmeta()

bfcinfo()
bfcinfo(rids=c("BFC11", "BFC7"))
bfcgetmeta(name="resourcedata")
