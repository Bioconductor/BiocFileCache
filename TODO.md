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

bfcadd(bfc0, "webTestWork", rtype="web", fpath="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit")
bfcadd(bfc0, "webTestFTP", rtype="web", fpath="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")
bfcadd(bfc0, "webReDir", rtype="web", fpath="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip")
bfcadd(bfc0, "webNotFound", rtype="web", fpath="https://hehehaf")

# check not web
bfcneedsupdate(bfc0, 3)

# check not exist
bfcneedsupdate(bfc0, 4)

# check can't be determined
bfcneedsupdate(bfc0, 5)

# check available  
bfcneedsupdate(bfc0, 6)


bfcpath(bfc0, rid3)
bfcpath(bfc0, 5)
bfcpath(bfc0, 2)

bfcupdate(bfc0, 2, weblink="http://google.com")
bfcupdate(bfc0, 6, weblink="http://jibbb")


bfcquery(bfc0, "test")
bfcquery(bfc0, "ftp")


# url <- "http://httpbin.org/get"


 
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

- should remove resource remove file or just from cache -  if in cache location
  remove file:
  removeResource can have 1 or multiple rids - loop remove file 

- function to compare untracked files in cache location - sync function
  do local files exist?  visaversa are files in location being tracked?   

- add unit tests
