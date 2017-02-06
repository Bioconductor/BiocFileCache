#########################
#
# quick run 
#
#########################

library(devtools)
library(RSQLite)
library(dplyr)
library(httr)
document()
install()
library(BiocFileCache)
example("BiocFileCache-class")

addResource(bfc0, "webTestWork", rtype="web", fpath="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit")
addResource(bfc0, "webTestFTP", rtype="web", fpath="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")
addResource(bfc0, "webReDir", rtype="web", fpath="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip")
addResource(bfc0, "webTestNotFound", rtype="web", fpath="https://hehehaf")

checkResource(bfc0, 3)
checkResource(bfc0, 4)
checkResource(bfc0, 5)
checkResource(bfc0, 6)
checkResource(bfc0, 7)
checkResource(bfc0, 8)


 
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


#
# general utility
#

- should remove resource remove file or just from cache -  if in cache location unlink
- function to compare untracked files in cache location? - sync function 