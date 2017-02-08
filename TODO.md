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
bfcadd(bfc0, "webTestFTP", fpath="ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz")
bfcadd(bfc0, "webReDir", fpath="https://github.com/wch/webshot/releases/download/v0.3/phantomjs-2.1.1-macosx.zip")
bfcadd(bfc0, "webNotFound", rtype="web", fpath="https://hehehaf")


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
