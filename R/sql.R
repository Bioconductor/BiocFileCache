.sql_file <-
    function(bfc, file)
{
    file.path(bfcCache(bfc), file)
}

.sql_dbfile <-
    function(bfc)
{
    .sql_file(bfc, "BiocFileCache.sqlite")
}

.sql_get_query <-
    function(bfc, sql)
{
    sqls <- strsplit(sql, ";", fixed=TRUE)[[1]]
    sqlfile <- .sql_dbfile(bfc)
    con <- dbConnect(SQLite(), sqlfile)
    for (sql in sqls)
        result <- dbGetQuery(con, sql)
    dbDisconnect(con)
    result
}

.sql_sprintf <-
    function(cmd_name, ...)
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
        .sql_get_query(bfc, sql)
    }
    fl
}

.sql_new_resource <-
    function(bfc, rname, rtype, weblink)
{
    fname <- tempfile("", bfcCache(bfc))
    sql <- .sql_sprintf("-- INSERT", rname, fname, rtype, weblink)
    .sql_get_query(bfc, sql)[[1]]
}

.sql_remove_resource <-
    function(bfc, rids)
{
    sql <- .sql_sprintf("-- REMOVE", paste0("'", rids, "'", collapse=", "))
    .sql_get_query(bfc, sql)
}

.sql_full_resource_table <-
    function(bfc)
{
    src <- src_sqlite(.sql_dbfile(bfc))
    tbl(src, "resource")
}

.sql_get_resource_table <-
    function(bfc, i)
{
    if (missing(i))
        i = integer(0)
    src <- src_sqlite(.sql_dbfile(bfc))
    if (length(i) == 0)
        vl <- tbl(src, "resource")
    else if (length(i) == 1)
        vl <- tbl(src, "resource") %>% filter_(~ rid == i)
    else
        vl <- tbl(src, "resource") %>% filter_(~ rid %in% i)

    class(vl) <- c("tbl_bfc", class(vl))
    vl
}

.sql_get_nrows <-
    function(x)
{
    x %>% count() %>% collect(Inf) %>% `[[`('n')
}

.sql_get_field <-
    function(bfc, id, field)
{
    .sql_get_resource_table(bfc) %>% filter_(~ rid == id) %>%
        select_(field) %>% collect(Inf) %>% `[[`(field)
}

.sql_get_rpath <-
    function(bfc, rid)
{
    .sql_get_field(bfc, rid, "rpath")
}

.sql_set_rpath <-
    function(bfc, rid, path)
{
    sql <- .sql_sprintf("-- UPDATE_PATH", path, rid)
    .sql_get_query(bfc, sql)
}

.sql_update_time <-
    function(bfc, rid)
{
    sql <- .sql_sprintf("-- UPDATE_TIME", rid)
    .sql_get_query(bfc, sql)
}

.sql_set_rname <-
    function(bfc, rid, value)
{
    sql <- .sql_sprintf("-- UPDATE_RNAME", value, rid)
    .sql_get_query(bfc, sql)
}

.sql_clean_cache <-
    function(bfc, days)
{
    mytbl <- .sql_get_resource_table(bfc) %>%
        select_(~ rid, ~ access_time) %>% collect(Inf)
    currentDate <- Sys.Date()
    accessDate <- as.Date(
        sapply(strsplit(
            as.character(mytbl[,2]), split=" "), `[`, 1))
    diffTime <- currentDate - accessDate
    mytbl[diffTime > days,1] %>% collect(Inf) %>% `[[`("rid")
}

.sql_get_resource <-
    function(bfc, id)
{
    mytbl <- .sql_get_resource_table(bfc)
    mytbl %>% filter_(~ rid == id) %>% collect(Inf)
}

.sql_subset_resources <-
    function(bfc, i)
{
    if (length(i) == 1L)
        .sql_get_resource(bfc, i)
    else
        .sql_get_resource_table(bfc) %>% filter_(~ rid %in% i) %>% collect(Inf)
}

.get_all_rids <-
    function(bfc)
{
    .sql_get_resource_table(bfc) %>% select_("rid") %>%
        collect(Inf) %>% `[[`("rid")

}
.get_all_web_rids <-
    function(bfc)
{
    .sql_get_resource_table(bfc) %>% filter_(~ rtype == "web") %>%
        select_("rid") %>% collect(Inf) %>% `[[`("rid")

}

.sql_set_modifiedTime <-
    function(bfc, rid, value)
{
    sql <- .sql_sprintf("-- UPDATE_MODIFIED", value, rid)
    .sql_get_query(bfc, sql)
}

.sql_set_weblink <-
    function(bfc, rid, value)
{
    sql <- .sql_sprintf("-- UPDATE_WEBLINK", value, rid)
    .sql_get_query(bfc, sql)
}

.sql_query_resource <-
    function(bfc, value)
{ 
    helperFun <- function(bfc0, vl){
        sql <- .sql_sprintf("-- QUERY_NAMES", vl)
        .sql_get_query(bfc0, sql) %>% select_("rid") %>% collect(Inf) %>%
            `[[`("rid")
    }
    res <- lapply(value, FUN=helperFun, bfc0=bfc)
    Reduce(intersect, res)
}

.get_all_rpath <-
    function(bfc)
{
    .sql_get_resource_table(bfc) %>% select_("rpath") %>%
        collect(Inf) %>% `[[`("rpath")

}

.get_rid_filenotfound <-
    function(bfc)
{
    vec <- file.exists(
        .sql_get_resource_table(bfc) %>% select_("rpath") %>%
        collect(Inf) %>% `[[`("rpath"))
    .get_all_rids(bfc)[!vec]
}
