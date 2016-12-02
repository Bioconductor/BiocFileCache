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
    fl <- .sql_dbfile(bfc)
    if (!file.exists(fl)) {
        sql <- .sql_get_cmd("-- TABLE")
        .sql_do(bfc, sql)
    }
    fl
}

.sql_add_resource <-
    function(bfc, rname)
{
    tmpl <- .sql_get_cmd("-- INSERT")
    fname <- tempfile("", bfcCache(bfc))
    sql <- sprintf(tmpl, rname, basename(fname))
    .sql_do(bfc, sql)
    ## FIXME: return newly created resource id
    fname
}

.sql_remove_resource <-
    function(bfc, rids)
{
    tmpl <- .sql_get_cmd("-- REMOVE")
    sql <- sprintf(tmpl, paste0("'", rids, ".", collapse=", "))
    .sql_do(bfc, sql)
}

.sql_get_resource_table <-
    function(bfc)
{
    src <- src_sqlite("~/.BiocFileCache/BiocFileCache.sqlite")
    tbl(src, "resource")
}
