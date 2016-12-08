# TODO

- Create a second table in the BiocFileCache.sqlite to include
  'metadata', in particular a schema version and BiocFileCache version
  when sqlite file created. Do something graceful when the schema has
  changes and the user tries to access an old sqlite schema with a
  version of BiocFileCache that supports only the new sqlite schema --
  BiocGenerics::updateObject,BiocFileCache-method at the R level.
