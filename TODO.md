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

temp = bfc0[2:3]




fpath="http://hgdownload.cse.ucsc.edu/goldenpath/canFam1/bigZips/canFam1.2bit"
fpath="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz"
fpath="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip"
fpath="http://httpbin.org/get"
fpath="https://en.wikipedia.org/wiki/Bioconductor"


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







# ask about documentation link??? 

# change bfcadd(x, rname, fpath, rtype, action, proxy, ...) so
  fpath = rname by default

# bfcrpath(x, rnames, ..., rids): character() file path to resource in cacache


bfcrpath(bfc, "https://bioc.org/index.html")
bfcrpath(bfc, "c:/my/file")

- rnames OR rids
- all rnames OR all rids exist
- if rnames then
  - does rname exist (and unique) in database?
    yes
	return rpath of resource
    no
	rpaths <- vapply(rnames, function(x, rname, ...) {
            tryCatch({
                bfcadd(x, rname, ...)
            }, error=function(e) {
	        warning(conditionMessage(e))
                NA_character_
            })
        if (anyNA(rids)) {
	    ## clean up -- remove all added resources
            stop()
        }
	bfcrpath(bfc, rids=rid)


