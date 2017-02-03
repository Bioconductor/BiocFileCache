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

addResource(bfc0, fl1, "webTestWork", rtype="web", weblink="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit")
addResource(bfc0, fl1, "webTestFTP", rtype="web", weblink="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")
addResource(bfc0, fl1, "webTestReDir", rtype="web", weblink="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip")
addResource(bfc0, fl1, "webTestNotFound", rtype="web", weblink="https://hehehaf")

checkResource(bfc0, 6)

newResource(bfc0, "web", "web", "ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")


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
# Web Resource
#

- actually download web resource (when added and on load)
- when addResource/newResource/checkResource > better handling of bad link
     add/new will include in cache even tho bad link / more elegant handling in checkResource 
- when updateResource weblink check for valid url
- function to check valid url - keep in mind redirects will be valid but fail in checks like url.exists


#
# general utility
#

- should remove resource remove file or just from cache - orphaned files??
- function to compare untracked files in cache location? (essentially clean
    /home/user/.BiocFileCache if files are not tracked in sqlite obj) 
- rname option:  [[, [[<-, listResources, loadResources, ?updateResource,
     checkResource, removeResource, ?new: searchResources
