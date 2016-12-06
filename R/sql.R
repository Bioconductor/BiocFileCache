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
    if (startsWith(sql, "-- INSERT")){
        calls <- strsplit(sql, ";")
        df <- dbGetQuery(con, calls[[1]][1])
        id <- dbGetQuery(con, calls[[1]][2])
    }else{
        dbGetQuery(con,sql)
    }
    dbDisconnect(con)
    
    if (startsWith(sql, "-- INSERT"))
        as.numeric(id)
    else
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
    function(bfc, rname, path)
{
    tmpl <- .sql_get_cmd("-- INSERT")
    fname <- tempfile("", bfcCache(bfc))
    sql <- sprintf(tmpl, rname, path, basename(fname))
    .sql_do(bfc, sql)
    
}

.sql_remove_resource <-
    function(bfc, rids)
{
    tmpl <- .sql_get_cmd("-- REMOVE")
    sql <- sprintf(tmpl, paste0("'", rids, "'", collapse=", "))
    .sql_do(bfc, sql)
}

.sql_get_resource_table <-
    function(bfc)
{
    src <- src_sqlite(.sql_dbfile(bfc))
    tbl(src, "resource")
}

.sql_get_entry <-
    function(bfc, id, field)
{
    mytbl <- .sql_get_resource_table(bfc)
    df <-  mytbl %>% filter(rid == id)
    dx <- which(colnames(df) == field)
    df %>% select(dx) %>% as.data.frame()     
}

.sql_load_resource <-
    function(bfc, rid)
{
    path <- as.character(.sql_get_entry(bfc, rid, "filepath"))
    if (file.exists(path))
        readRDS(path)
    else
        message(paste0("ERROR: '", path, "' does Not Exist"))
}

.sql_update_path <- function(bfc, rid, path){
    tmpl <- .sql_get_cmd("-- UPDATEPATH")
    sql <- sprintf(tmpl, path, rid)
    .sql_do(bfc, sql)
}

.sql_update_time <- function(bfc, rid){
    tmpl <- .sql_get_cmd("-- UPDATETIME")
    sql <- sprintf(tmpl, rid)
    .sql_do(bfc, sql)
}
