.sql_file <-
    function(bfc, file)
{
    file.path(bfccache(bfc), file)
}

.sql_dbfile <-
    function(bfc)
{
    .sql_file(bfc, .CACHE_FILE)
}

.sql_validate_version <-
    function(bfc)
{
    sqlfile <- .sql_dbfile(bfc)
    con <- dbConnect(SQLite(), sqlfile)
    sql <- .sql_sprintf("-- SELECT_METADATA")
    mdata <- dbGetQuery(con, sql)
    dbDisconnect(con)
    if (!mdata[mdata$key=="schema_version",2] %in% .SUPPORTED_SCHEMA_VERSIONS)
        stop(
            "unsupported schema version '",
            mdata[mdata$key=="schema_version",2], "'",
            "\n  use BiocFileCache version '",
            mdata[mdata$key=="package_version",2], "'"
            )
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
        ## update metadata table
        sql <- .sql_sprintf("-- METADATA")
        .sql_get_query(bfc, sql)
        sql <- .sql_sprintf("-- INSERT_METADATA",
                            sprintf("'schema_version', '%s'",
                                    .CURRENT_SCHEMA_VERSION))
        .sql_get_query(bfc, sql)
        sql <- .sql_sprintf("-- INSERT_METADATA",
                            sprintf("'package_version', '%s'",
                                    as.character(packageVersion("BiocFileCache"))
                                    ))
        .sql_get_query(bfc, sql)
        ## create new resource table
        sql <- .sql_sprintf("-- TABLE")
        .sql_get_query(bfc, sql)
    }
    .sql_validate_version(bfc)
    fl
}

.sql_new_resource <-
    function(bfc, rname, rtype, fpath)
{
    fname <- path.expand(tempfile("", bfccache(bfc)))
    sql <- .sql_sprintf("-- INSERT", rname, fname, rtype, fpath)
    .sql_get_query(bfc, sql)[[1]]
}

.sql_remove_resource <-
    function(bfc, rids)
{
    sql <- .sql_sprintf("-- REMOVE", paste0("'", rids, "'", collapse=", "))
    .sql_get_query(bfc, sql)
}

.sql_get_resource_table <-
    function(bfc, i)
{
    src <- src_sqlite(.sql_dbfile(bfc))
    tbl <- tbl(src, "resource")

    if (missing(i)) {
        ## tbl <- tbl
    } else if (length(i) == 0) {
        tbl <- tbl %>% filter_(~ rid == NA_character_)
    } else if (length(i) == 1) {
        tbl <- tbl %>% filter_(~ rid == i)
    } else {
        tbl <- tbl %>% filter_(~ rid %in% i)
    }

    class(tbl) <- c("tbl_bfc", class(tbl))
    tbl %>% select_(~ -id)
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

.sql_get_rtype <-
    function(bfc, rid)
{
    .sql_get_field(bfc, rid, "rtype")
}

.sql_get_fpath <-
    function(bfc, rid)
{
    .sql_get_field(bfc, rid, "fpath")
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

#    accessDate <- as.Date(
#        sapply(strsplit(
#            as.character(mytbl[,2]), split=" "), `[`, 1))
    accessDate <- as.Date(
        sapply(strsplit(
            (mytbl %>% `[[`("access_time")), split=" "), `[`, 1))

    diffTime <- currentDate - accessDate
    mytbl[diffTime > days,1] %>% collect(Inf) %>% `[[`("rid")
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

.sql_get_last_modified <-
    function(bfc, rid)
{
    .sql_get_field(bfc, rid, "last_modified_time")
}

.sql_set_last_modified <-
    function(bfc, rid, value)
{
    sql <- .sql_sprintf("-- UPDATE_MODIFIED", value, rid)
    .sql_get_query(bfc, sql)
}

.sql_set_fpath <-
    function(bfc, rid, value)
{
    sql <- .sql_sprintf("-- UPDATE_FPATH", value, rid)
    .sql_get_query(bfc, sql)
}

.sql_query_resource <-
    function(bfc, value)
{
    helperFun <- function(bfc0, vl) {
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

.get_tbl_rid <-
    function(tbl)
{
    tbl %>% collect(Inf) %>% `[[`("rid")
}

.fix_rnames <- function(bfc, rnames){

    stopifnot(!missing(rnames))
    unname(vapply(rnames, .sql_query_resource, character(1),bfc=bfc))
}
