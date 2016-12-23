#
# quick run 
#

library(devtools)
library(RSQLite)
library(dplyr)
document()
install()
library(BiocFileCache)
example("BiocFileCache-class")


#####################
#
# TODO
#
#####################

#
# Current make changes
#

5. [ should return a new BiocFileCache??
    - update EVERYTHING  - new slot integer listing active rid 
    - rid(bfc) 
    - new method listResource(s) - to give certain entries?? 
      different implementation of .sql_subset_resources


#
# For web resources 
#

- Add local/remote to original table 

- Create a second table in the BiocFileCache.sqlite to include
  'metadata', in particular a schema version and BiocFileCache version
  when sqlite file created. Do something graceful when the schema has
  changes and the user tries to access an old sqlite schema with a
  version of BiocFileCache that supports only the new sqlite schema --
  BiocGenerics::updateObject,BiocFileCache-method at the R level.

(httr - creation time and etags - get header of resource and parse)

- summary: if remote, use etags and creation time to see if resource needs to be
  updated 