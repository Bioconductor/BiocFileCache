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

.sql_sprintf <-
    function(cmd_name, ...)
        ## e.g., "-- INSERT"
{
    sql_cmd_file <- system.file(
        package="BiocFileCache", "schema", "BiocFileCache.sql")
    sql_cmds <- readLines(sql_cmd_file)
    grps <- cumsum(grepl("^--", sql_cmds))
    cmds <- split(sql_cmds, grps)
    names <- vapply(cmds, "[[", character(1), 1)
    cmds <- paste(cmds[[which(names == cmd_name)]], collapse="\n")
    sprintf(cmds, ...)
}

.sql_create_db <-
    function(bfc)
{
    fl <- .sql_dbfile(bfc)
    if (!file.exists(fl)) {
        sql <- .sql_sprintf("-- TABLE")
        .sql_do(bfc, sql)
    }
    fl
}

.sql_add_resource <-
    function(bfc, rname, path)
{
    fname <- tempfile("", bfcCache(bfc))
    sql <- .sql_sprintf("-- INSERT", rname, path, basename(fname))
    .sql_do(bfc, sql)
    
}

.sql_remove_resource <-
    function(bfc, rids)
{
    sql <- .sql_sprintf("-- REMOVE", paste0("'", rids, "'", collapse=", "))
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
    df <-  mytbl %>% filter_(~ rid == id)
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

.sql_update_path <-
    function(bfc, rid, path)
{
    sql <- .sql_sprintf("-- UPDATEPATH", path, rid)
    .sql_do(bfc, sql)
}

.sql_update_time <-
    function(bfc, rid)
{
    sql <- .sql_sprintf("-- UPDATETIME", rid)
    .sql_do(bfc, sql)
}

.sql_clean_cache <-
    function(bfc, days)
{
    mytbl <- .sql_get_resource_table(bfc) %>%
        select_(~ rid, ~ last_accessed) %>% as.data.frame()
    currentDate <- Sys.Date()
    accessDate <- as.Date(sapply(strsplit(mytbl[,2], split=" "), `[`, 1))
    diffTime <- currentDate - accessDate
    mytbl[diffTime > days,1]
}

.sql_get_resource <-
    function(bfc, id)
{
    mytbl <- .sql_get_resource_table(bfc)
    mytbl %>% filter_(~ rid == id) %>% collect(Inf)
}
