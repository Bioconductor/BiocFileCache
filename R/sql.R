#' @import RSQLite
#' @importFrom DBI dbExecute dbSendStatement
#' @import dbplyr
#' @importFrom dplyr %>% tbl select_ collect summarize filter_ n left_join
#' @importFrom curl curl_escape

.formatID <- . %>% collect(Inf) %>% `[[`("rid")

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

.sql_schema_version <-
    function(bfc)
{
    con <- dbConnect(RSQLite::SQLite(), .sql_dbfile(bfc))
    on.exit(dbDisconnect(con))
    src <- src_dbi(con)
    tbl <- tbl(src, "metadata") %>% collect(Inf)
    tbl$value[tbl$key=="schema_version"]
}

## R / RSQLite, DBI interface

.sql_db_execute <-
    function(bfc, sql, ..., con)
{
    param <- data.frame(..., stringsAsFactors = FALSE)
    if (nrow(param) == 0L)
        param <- NULL

    if (missing(con)) {
        con <- dbConnect(SQLite(), .sql_dbfile(bfc))
        on.exit(dbDisconnect(con))
    }

    dbExecute(con, sql, param = param)
}

.sql_db_get_query <-
    function(bfc, sql, ..., con)
{
    param <- data.frame(..., stringsAsFactors = FALSE)
    if (nrow(param) == 0L)
        param <- NULL

    if (missing(con)) {
        con <- dbConnect(SQLite(), .sql_dbfile(bfc))
        on.exit(dbDisconnect(con))
    }

    dbGetQuery(con, sql, param)
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

.sql_add_resource <-
    function(bfc, rname, rtype, fpath, ext = NA_character_)
{
    rpath <- path.expand(tempfile("", bfccache(bfc)))
    rtype <- unname(rtype)
    if (identical(rtype, "relative") || identical(rtype, "web"))
        rpath <- basename(rpath)

    fpath[is.na(fpath)] <- rpath[is.na(fpath)]
    ext[is.na(ext)] <- ""
    bfname <- basename(Fpatho)
    bfname <- curl_escape(bfname)
    rpath <- sprintf("%s_%s%s", rpath, bfname, ext)

    con <- dbConnect(SQLite(), .sql_dbfile(bfc))
    on.exit(dbDisconnect(con))

    sql <- strsplit(.sql_cmd("-- INSERT"), ";")[[1]]
    original_rid <- .sql_db_get_query(bfc, sql[[1]], con = con)[["rid"]]
    .sql_db_execute(
        bfc, sql[[2]],
        rname = rname, rtype = rtype, fpath = fpath, rpath = rpath,
        last_modified_time = as.Date(NA_character_), etag = NA_character_,
        expires = NA_character_,
        con = con
    )
    .sql_db_execute(bfc, sql[[3]], con = con)

    rid <- .sql_db_get_query(bfc, sql[[1]], con = con)[["rid"]]
    .sql_get_rpath(bfc, setdiff(rid, original_rid))
}

.sql_remove_resource <-
    function(bfc, rid)
{
    sql <- .sql_cmd("-- REMOVE")
    cmd <- sprintf(sql, paste0("'", rid, "'", collapse = ","))
    .sql_db_execute(bfc, cmd)
}

.sql_get_resource_table <-
    function(bfc, rids)
{
    con <- dbConnect(RSQLite::SQLite(), .sql_dbfile(bfc))
    on.exit(dbDisconnect(con))
    src <- src_dbi(con)
    tbl <- tbl(src, "resource")

    if (missing(rids)) {
    } else if (length(rids) == 0) {
        tbl <- tbl %>% filter_(~ rid == NA_character_)
    } else if (length(rids) == 1) {
        tbl <- tbl %>% filter_(~ rid == rids)
    } else {
        tbl <- tbl %>% filter_(~ rid %in% rids)
    }

    ## join metadata
    meta <- setdiff(dbListTables(con), .RESERVED$TABLES)
    for (m in meta)
        tbl <- left_join(tbl, tbl(src, m), by="rid")

    tbl <- tbl %>% collect
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
    tbl <- .sql_get_resource_table(bfc) %>% filter_(~ rid %in% id) %>%
        select_(~ rid, field) %>% collect(Inf)
    setNames(tbl[[field]], tbl[["rid"]])
}

.sql_get_rname <-
    function(bfc, rid)
{
    .sql_get_field(bfc, rid, "rname")
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
    idx <- rtype %in% c("relative", "web")
    rpath[idx] <- file.path(bfccache(bfc), rpath)[idx]
    rpath
}

.sql_set_rpath <-
    function(bfc, rid, rpath)
{
    sql <- .sql_cmd("-- UPDATE_PATH")
    .sql_db_execute(bfc, sql, rid = rid, rpath = rpath)
}

.sql_set_time <-
    function(bfc, rid)
{
    sql <- .sql_cmd("-- UPDATE_TIME")
    cmd <- sprintf(sql, paste0("'", rid, "'", collapse = ","))
    .sql_db_execute(bfc, cmd)
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
    accessDate <- as.Date(as.character(mytbl$access_time))
    diffTime <- Sys.Date() - accessDate
    mytbl[diffTime > days, 1] %>% .formatID
}

.get_all_rids <-
    function(bfc)
{
    .sql_get_resource_table(bfc) %>% select_("rid") %>% .formatID
}

.get_all_web_rids <-
    function(bfc)
{
    .sql_get_resource_table(bfc) %>% filter_(~ rtype == "web") %>%
        select_("rid") %>% .formatID

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

.sql_get_etag <-
    function(bfc, rid)
{
    .sql_get_field(bfc, rid, "etag")
}

.sql_set_etag <-
    function(bfc, rid, etag)
{
    sql <- .sql_cmd("-- UPDATE_ETAG")
    .sql_db_execute(bfc, sql, rid = rid, etag = etag)
}
.sql_get_expires <-
    function(bfc, rid)
{
    .sql_get_field(bfc, rid, "expires")
}

.sql_set_expires <-
    function(bfc, rid, expires)
{
    sql <- .sql_cmd("-- UPDATE_EXPIRES")
    .sql_db_execute(bfc, sql, rid = rid, expires = expires)
}

.sql_set_fpath <-
    function(bfc, rid, fpath)
{
    sql <- .sql_cmd("-- UPDATE_FPATH")
    .sql_db_execute(bfc, sql, rid = rid, fpath = fpath)
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
    tbl %>% .formatID
}

.get_all_colnames <-
    function(bfc)
{
    colnames(.sql_get_resource_table(bfc))
}

.get_nonrelative_ids <-
    function(bfc)
{
    rpaths <- .sql_get_rpath(bfc, bfcrid(bfc))
    res <- startsWith(rpaths, bfccache(bfc))
    names(rpaths)[!res]
}

##
## .sql_meta_*
##

.sql_meta_gets <-
    function(bfc, name, value, ...)
{
    con <- DBI::dbConnect(RSQLite::SQLite(), .sql_dbfile(bfc))
    on.exit(dbDisconnect(con))
    dbWriteTable(con, name, value, ...)
}

.sql_meta_remove <-
    function(bfc, name, ...)
{
    con <- DBI::dbConnect(RSQLite::SQLite(), .sql_dbfile(bfc))
    on.exit(dbDisconnect(con))
    if (dbExistsTable(con, name))
        dbRemoveTable(con, name, ...)
}

.sql_meta <-
    function(bfc, name, ...)
{
    con <- DBI::dbConnect(RSQLite::SQLite(), .sql_dbfile(bfc))
    on.exit(dbDisconnect(con))
    if (!dbExistsTable(con, name))
        stop("'", name, "' not found in database")
    dbReadTable(con, name, ...)
}

.sql_meta_list <-
    function(bfc)
{
    con <- DBI::dbConnect(RSQLite::SQLite(), .sql_dbfile(bfc))
    on.exit(dbDisconnect(con))
    res <- dbListTables(con)
    setdiff(res, .RESERVED$TABLES)
}

.sql_filter_metadata <-
    function(bfc, name, verbose)
{
    df <- bfcmeta(bfc, name)
    rids <- bfcrid(bfc)
    check <- as.character(df$rid) %in% rids
    if (all(!check)) {
        bfcmetaremove(bfc, name)
        vl <- FALSE
    } else if (any(!check)) {
        df <- df[check,]
        bfcmeta(bfc, name, overwrite=TRUE) <- df
        vl <- FALSE
    } else {
        vl <- TRUE
    }
    vl
}
