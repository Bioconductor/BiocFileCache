# TODO

- Create a second table in the BiocFileCache.sqlite to include
  'metadata', in particular a schema version and BiocFileCache version
  when sqlite file created. Do something graceful when the schema has
  changes and the user tries to access an old sqlite schema with a
  version of BiocFileCache that supports only the new sqlite schema --
  BiocGenerics::updateObject,BiocFileCache-method at the R level.

# quick run 
library(devtools)
library(RSQLite)
library(dplyr)
document()
install()
library(BiocFileCache)
example("BiocFileCache-class")


## Adding

1. addResource - have path to file - track through us 
   
   addResource(bfc, fpath, rname, action=c("copy", "move", "asis"))
   rid = .sql_new_resource(bfc, rname, ...)
   switch(match.arg(action),
       asis = .sql_set_cache_file_path(bfc, rid, fname), # returns fname
       copy = file.copy(fname, .sql_get_cache_file_path(bfc, rid)),
       move = file.move(fname, .sql_get_cache_file_path(bfc, rid)))

2. newResource -  return our default save cache_path 
 
   newResource(bfc, rname, saveMethod=NULL, loadMethod=NULL)
     rid = .sql_new_resource(bfc, ...)
     rpath  = .sql_get_cache_file_path(bfc, rid)
     setNames(rpath, rid)
   
## Loading

1. resourcePath(bfc, rid) <- isn't this loadResource
2. loadResource(bfc, rid) <- what does this do?? should this do more than return
the path?? 

## maintenance

1. listResources(bfc)
2. updateResource(bfc, rid, object=NULL, rpath=NULL) 
   this is how user would update:  bfc[[rid]] = myNewObjectOrPath


##
## Make changes
##


2.  change scheme to rpath rather than cache_file_path - make change globally

3.  updateResource have rname=NULL rpath=NULL and update accordingly

4.  [[  only export path / [[<-  only set path 

5. [ should return a new BiocFileCache??
    - update EVERYTHING  - new slot integer listing active rid 
    - rid(bfc) 
 
##
## FUTURE
## 

add local or remote
if remote  - additional table with addtional information to see if need to be
updated (package httr - creation time and etags - get header of resource and parse) 