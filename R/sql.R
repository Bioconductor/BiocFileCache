.sql_dbfile <-
    function(bfc)
{
    file.path(bfcCache(bfc), "BiocFileCache.sqlite")
}        

.sql_do <-
    function(bfc, sql)
{
    sqlfile <- .sql_dbfile(bfc)
    con <- dbConnect(SQLite(), sqlfile)
    df <- dbGetQuery(con, sql)
    dbDisconnect(con)

    sqlfile
}      

.sql_get_cmd <-
    function(cmd_name)
        ## e.g., "-- INSERT"
{
    sql_cmd_file <- system.file(
        package="BiocFileCache", "schema", "BiocFileCache.sql")
    sql_cmds <- readLines(sql_cmd_file)
    grps <- cumsum(grepl("^--", sql_cmds))
    cmds <- split(sql_cmds, grps)
    names <- vapply(cmds, "[[", character(1), 1)
    paste(cmds[[which(names == cmd_name)]], collapse="\n")
}

.sql_create_db <-
    function(bfc)
{
    sql <- .sql_get_cmd("-- TABLE")
    .sql_do(bfc, sql)
}

.sql_destroy_db <-
    function(bfc)
{
    file.remove(.sql_dbfile(bfc))
}

.sql_add_resource <-
    function(bfc, rname)
{
    tmpl <- .sql_get_cmd("-- INSERT")
    sql <- sprintf(tmpl, rname, basename(tempfile("", bfcCache(bfc))))
    .sql_do(bfc, sql)
}

.sql_remove_resource <-
    function(bfc, rid, rname)
{
    tmpl <- .sql_get_cmd("-- REMOVE")
    sql <- sprintf(tmpl, rid, rname, basename(tempfile("", bfcCache(bfc))))
    .sql_do(bfc, sql)
}

.sql_get_resource_table <-
    function(bfc)
{
    src <- src_sqlite("~/.BiocFileCache/BiocFileCache.sqlite")
    tbl(src, "resource")
}
