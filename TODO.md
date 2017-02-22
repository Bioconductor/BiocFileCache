#########################
#
# quick run 
#
#########################

library(devtools)
library(RSQLite)
library(dplyr)
library(httr)
library(testthat)
document()
install()
library(BiocFileCache)
example("BiocFileCache-class")



# test http
bfcadd(bfc0, "webTestWork", rtype="web", fpath="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit")
# test ftp 
bfcadd(bfc0, "webTestFTP", fpath="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")
# test a redirection
bfcadd(bfc0, "webReDir", fpath="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip")
# test a not found 
bfcadd(bfc0, "webNotFound", rtype="web", fpath="https://hehehaf")

# test simple no last modified tag
bfcadd(bfc0, "website", fpath="http://httpbin.org/get")
# test simple with last modified tag 
bfcadd(bfc0, "website", fpath="https://en.wikipedia.org/wiki/Bioconductor")

#####################
#
# TODO
#
#####################

#
# How to Update if schema updated  (maybe keep track of version #)
#

- Create a second table in the BiocFileCache.sqlite to include
  'metadata', in particular a schema version and BiocFileCache version
  when sqlite file created. Do something graceful when the schema has
  changes and the user tries to access an old sqlite schema with a
  version of BiocFileCache that supports only the new sqlite schema --
  BiocGenerics::updateObject,BiocFileCache-method at the R level.

https://www.sqlite.org/lang_altertable.html
select * into [TargetTable] from [SourceTable];
drop table [TargetTable];

http://stackoverflow.com/questions/3604310/alter-table-add-column-if-not-exists-in-sqlite





1. implement to get rid of ?? show
5. attempt implement "{", change class representation see tmp.R
7. multiple ids for bfcupdate and brcneedsupdate
8. add extended use section