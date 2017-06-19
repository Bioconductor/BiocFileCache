#' @import RSQLite
#' @importFrom DBI dbExecute dbSendStatement
#' @import dbplyr
#' @importFrom dplyr %>% src_sqlite tbl select_ collect summarize filter_ n

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

.sql_cmd <-
    function(cmd_name, add=FALSE, ...)
{
    sql_cmd_file <-
        system.file(package="BiocFileCache", "schema", "BiocFileCache.sql")
    sql_cmds <- readLines(sql_cmd_file)
    grps <- cumsum(grepl("^--", sql_cmds))
    cmds <- split(sql_cmds, grps)
    names <- vapply(cmds, "[[", character(1), 1)
    cmds <- paste(cmds[[which(names == cmd_name)]], collapse="\n")
    if (add)
        cmds <- sprintf(cmds, ...)
    cmds
}

.sql_validate_version <-
    function(bfc)
{
    sqlfile <- .sql_dbfile(bfc)
    con <- dbConnect(SQLite(), sqlfile)
    sql <- .sql_cmd("-- SELECT_METADATA")
    mdata <- dbGetQuery(con, sql)
    dbDisconnect(con)
    if (!mdata[mdata$key=="schema_version",2] %in% .SUPPORTED_SCHEMA_VERSIONS)
        stop(
            "unsupported schema version ",
            "\n  sqlite file: ", sqlfile,
            "\n  file schema version: ",
            sQuote(mdata[mdata$key=="schema_version",2]),
            "\n  supported version(s): ",
            paste(sQuote(.SUPPORTED_SCHEMA_VERSIONS), collapse=" ")
        )
}

## R / RSQLite, DBI interface

.sql_db_execute <-
    function(bfc, sql, ...)
{
    params <- list(...)
    con <- dbConnect(SQLite(), .sql_dbfile(bfc))
    if (length(params) == 0L) {
        result <- dbExecute(con, sql)
    } else {
        result <- dbExecute(con, sql, params = params)
    }
    dbDisconnect(con)
    result
}

.sql_db_send_query <-
    function(bfc, sql, ...)
{
    params <- list(...)
    con <- dbConnect(SQLite(), .sql_dbfile(bfc))
    rs <- dbSendStatement(con, sql)
    dbBind(rs, params)
    dbClearResult(rs)
    dbDisconnect(con)
}

.sql_db_fetch_query <-
    function(bfc, sql, ...)
{
    params <- list(...)

    con <- dbConnect(SQLite(), .sql_dbfile(bfc))
    rs <- dbSendStatement(con, sql)
    dbBind(rs, params)
    result <- dbFetch(rs)
    dbClearResult(rs)
    dbDisconnect(con)

    result
}

## BiocFileCache / RSQLite interface

.sql_create_db <-
    function(bfc)
{
    fl <- .sql_dbfile(bfc)
    if (!file.exists(fl)) {
        ## update metadata table
        sql <- .sql_cmd("-- METADATA")
        .sql_db_execute(bfc, sql)
        sql <- .sql_cmd("-- INSERT_METADATA")
        package_version <- as.character(packageVersion("BiocFileCache"))
        .sql_db_execute(
            bfc, sql,
            key = c('schema_version', 'package_version'),
            value = c(.CURRENT_SCHEMA_VERSION, package_version)
        )

        ## create new resource table
        sql <- .sql_cmd("-- TABLE")
        .sql_db_execute(bfc, sql)
    }
    .sql_validate_version(bfc)
    fl
}

.sql_new_resource <-
    function(bfc, rname, rtype, fpath, ext=NA_character_)
{
    rpath <- path.expand(tempfile("", bfccache(bfc)))
    if (identical(rtype, "relative"))
        rpath <- basename(rpath)

    if (is.na(fpath))
        fpath <- rpath

    rpath <- paste(rpath, basename(fpath), sep="_")

    if (!is.na(ext))
        rpath <- paste(rpath, ext, sep=".")

    # insert is special case need last_insert_rowid()
    # can't use dbExecute because auto clear destroys use case
    sql <- .sql_cmd("-- INSERT")
    sqls <- strsplit(sql, ";", fixed=TRUE)[[1]]
    sqlfile <- .sql_dbfile(bfc)
    con <- dbConnect(SQLite(), sqlfile)
    for(i in seq_along(sqls)){
        if (i == 1){
            param = list(rname=rname, rpath=rpath, rtype=rtype, fpath=fpath)
            temp <- dbSendQuery(con, sqls[i])
            dbBind(temp, param)
            result <- dbFetch(temp)
            dbClearResult(temp)
        } else {
            temp <- dbSendQuery(con, sqls[i])
            result <- dbFetch(temp)
            dbClearResult(temp)
        }
    }
    dbDisconnect(con)
    result[[1]]
}

.sql_remove_resource <-
    function(bfc, rid)
{
    sql <- .sql_cmd("-- REMOVE")
    .sql_db_send_query(bfc, sql, rid = rid)
}

.sql_get_resource_table <-
    function(bfc, rids)
{
    src <- src_sqlite(.sql_dbfile(bfc))
    tbl <- tbl(src, "resource")

    if (missing(rids)) {
        ## tbl <- tbl
    } else if (length(rids) == 0) {
        tbl <- tbl %>% filter_(~ rid == NA_character_)
    } else if (length(rids) == 1) {
        tbl <- tbl %>% filter_(~ rid == rids)
    } else {
        tbl <- tbl %>% filter_(~ rid %in% rids)
    }

    class(tbl) <- c("tbl_bfc", class(tbl))
    tbl %>% select_(~ -id)
}

.sql_get_nrows <-
    function(bfc)
{
    summarize(bfc, n=n()) %>% collect %>% `[[`("n")
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
    rtype <- .sql_get_rtype(bfc, rid)
    rpath <- .sql_get_field(bfc, rid, "rpath")
    if (identical(rtype, "relative"))
        rpath <- file.path(bfccache(bfc), rpath)
    rpath
}

.sql_set_rpath <-
    function(bfc, rid, rpath)
{
    sql <- .sql_cmd("-- UPDATE_PATH")
    .sql_db_execute(bfc, sql, rid = rid, rpath = rpath)
}

.sql_update_time <-
    function(bfc, rid)
{
    sql <- .sql_cmd("-- UPDATE_TIME")
    .sql_db_execute(bfc, sql, rid = rid)
}

.sql_set_rname <-
    function(bfc, rid, rname)

{
    sql <- .sql_cmd("-- UPDATE_RNAME")
    .sql_db_execute(bfc, sql, rid = rid, rname = rname)
}

.sql_set_rtype <-
    function(bfc, rid, rtype)
{
    sql <- .sql_cmd("-- UPDATE_RTYPE")
    .sql_db_execute(bfc, sql, rid = rid, rtype = rtype)
}

.sql_clean_cache <-
    function(bfc, days)
{
    mytbl <- .sql_get_resource_table(bfc) %>%
        select_(~ rid, ~ access_time) %>% collect(Inf)
    currentDate <- Sys.Date()

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
    function(bfc, rid, last_modified_time)
{
    sql <- .sql_cmd("-- UPDATE_MODIFIED")
    .sql_db_execute(
        bfc, sql, rid = rid, last_modified_time = last_modified_time
    )
}

.sql_set_fpath <-
    function(bfc, rid, fpath)
{
    sql <- .sql_cmd("-- UPDATE_FPATH")
    .sql_db_execute(bfc, sql, rid = rid, fpath = fpath)
}

.sql_query_resource <-
    function(bfc, value)
{
    helperFun <- function(bfc, value) {
        sql <- .sql_cmd("-- QUERY_NAMES")
        .sql_db_fetch_query(bfc, sql, value = value) %>%
            select_("rid") %>% collect(Inf) %>% `[[`("rid")
    }
    res <- lapply(value, FUN=helperFun, bfc=bfc)
    Reduce(intersect, res)
}

.get_all_rpath <-
    function(bfc)
{
    unname(bfcrpath(bfc))
}

.get_rid_filenotfound <-
    function(bfc)
{
    allpaths <- bfcrpath(bfc)
    names(allpaths)[!file.exists(allpaths)]
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

.get_tbl_rid <-
    function(tbl)
{
    tbl %>% collect(Inf) %>% `[[`("rid")
}

.fix_rnames <- function(bfc, rnames){

    stopifnot(!missing(rnames))
    unname(vapply(rnames, .sql_query_resource, character(1),bfc=bfc))
}
