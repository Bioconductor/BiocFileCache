#' @import RSQLite
#' @importFrom DBI dbExecute dbSendStatement
#' @import dbplyr
#' @importFrom dplyr %>% tbl select collect summarize filter n left_join
#' @importFrom curl curl_escape
#' @importFrom filelock lock unlock

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

.sql_lock_path <- 
    function(dbfile)
{
    paste0(dbfile, '.LOCK')
}

.sql_connect_RO <-
    function(dbfile)
{
    ## See notes in AnnotationDbi::dbFileConnect
    ## did not want to import AnnotationDbi in BFC because it is large
    ## overkill for this function
    if (!file.exists(dbfile))
        stop("DB file '", dbfile, "' not found")

    loc <- lock(.sql_lock_path(dbfile), exclusive=FALSE)

    if (.Platform$OS.type == "unix") {
        con <- dbConnect(SQLite(), dbname=dbfile, cache_size=64000L,
                         synchronous="off", flags=SQLITE_RO, vfs="unix-none")
    } else {
        ## Use default 'vfs' on Windows.
        con <- dbConnect(SQLite(), dbname=dbfile, cache_size=64000L,
                         synchronous="off", flags=SQLITE_RO)
    }

    list(lock=loc, con=con)
}

.sql_connect_RW <-
    function(dbfile)
{
    ## We also need a RW function to allow writing to the cache

    loc <- lock(.sql_lock_path(dbfile))

    if (.Platform$OS.type == "unix") {
        con <- dbConnect(SQLite(), dbname=dbfile, cache_size=64000L,
                         synchronous="off", vfs="unix-none")
    } else {
        ## Use default 'vfs' on Windows.
        con <- dbConnect(SQLite(), dbname=dbfile, cache_size=64000L,
                         synchronous="off")
    }

    list(lock=loc, con=con)
}

.sql_disconnect <- 
    function(info)
{
    dbDisconnect(info$con)
    unlock(info$lock)
}

.sql_schema_version <-
    function(bfc)
{
    tryCatch({
        info <- .sql_connect_RO(.sql_dbfile(bfc))
        con <- info$con
        src <- src_dbi(con)
        tbl <- tbl(src, "metadata") %>% collect(Inf)
        }, finally={.sql_disconnect(info)})
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
        info <- .sql_connect_RW(.sql_dbfile(bfc))
        con <- info$con
        on.exit(.sql_disconnect(info))
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
        info <- .sql_connect_RO(.sql_dbfile(bfc))
        con <- info$con      
        on.exit(.sql_disconnect(info))
    }
    dbGetQuery(con, sql, param)
}

## BiocFileCache / RSQLite interface

.sql_create_db <-
    function(bfc)
{
    fl <- .sql_dbfile(bfc)
    if (!file.exists(fl)) {
        sql <- strsplit(.sql_cmd("-- CREATE_DB"), ";")[[1]]
        tryCatch({
            info <- .sql_connect_RW(.sql_dbfile(bfc))
            con <- info$con
            dbExecute(con, sql[[1]])
            ## update metadata table
            .sql_db_execute(bfc, sql[[2]], con=con)
            package_version <- as.character(packageVersion("BiocFileCache"))
            .sql_db_execute(
                bfc, sql[[3]],
                key = c('schema_version', 'package_version'),
                value = c(.CURRENT_SCHEMA_VERSION, package_version),
                con=con)
            ## create new resource table
            .sql_db_execute(bfc, sql[[4]], con=con)
            dbExecute(con, sql[[5]])
        }, finally={.sql_disconnect(info)})
    }
    .sql_validate_version(bfc)
    fl
}

.sql_add_resource <-
    function(bfc, rname, rtype, fpath, ext = NA_character_)
{
    # The connection attempt handles locking if another process is adding a
    # resource at the same time; by trying to connect first, we don't have to
    # worry about whether the choice of temporary file name is thread-safe.
    info <- .sql_connect_RW(.sql_dbfile(bfc))
    on.exit(if (!is.null(info)) { .sql_disconnect(info) })

    rpath <- rep(path.expand(tempfile("", bfccache(bfc))), length(fpath))
    rtype <- unname(rtype)
    dx <- rtype == "relative" | rtype == "web"
    rpath[dx] <- basename(rpath[dx])

    fpath[is.na(fpath)] <- rpath[is.na(fpath)]
    ext[is.na(ext)] <- ""
    bfname <- basename(fpath)
    bfname <- curl_escape(bfname)
    rpath <- sprintf("%s_%s%s", rpath, bfname, ext)

    sql <- strsplit(.sql_cmd("-- INSERT"), ";")[[1]]
    con <- info$con
    dbExecute(con, sql[[1]])
    original_rid <- .sql_db_get_query(bfc, sql[[2]], con=con)[["rid"]]
    .sql_db_execute(
        bfc, sql[[3]],
        rname = rname, rtype = rtype, fpath = fpath, rpath = rpath,
        last_modified_time = as.Date(NA_character_), etag = NA_character_,
        expires = NA_character_, con=con
        )
    .sql_db_execute(bfc, sql[[4]], con=con)
    rid <- .sql_db_get_query(bfc, sql[[2]], con=con)[["rid"]]
    dbExecute(con, sql[[5]])

    # Free the file, as .sql_get_rpath() reacquires the lock internally.
    .sql_disconnect(info)
    info <- NULL

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
    tryCatch({
        info <- .sql_connect_RO(.sql_dbfile(bfc))
        con <- info$con
        src <- src_dbi(con)
        tbl <- tbl(src, "resource")

        if (missing(rids)) {
        } else if (length(rids) == 0) {
            tbl <- tbl %>% dplyr::filter(rid == NA_character_)
        } else if (length(rids) == 1) {
            tbl <- tbl %>% dplyr::filter(rid == rids)
        } else {
            tbl <- tbl %>% dplyr::filter(rid %in% rids)
        }

        ## join metadata
        meta <- setdiff(dbListTables(con), .RESERVED$TABLES)
        for (m in meta)
            tbl <- left_join(tbl, tbl(src, m), by="rid")

        tbl <- tbl %>% collect
    }, finally={.sql_disconnect(info)})
    class(tbl) <- c("tbl_bfc", class(tbl))
    tbl %>% dplyr::select(-id)
}

.sql_get_nrows <-
    function(bfc)
{
    summarize(bfc, n=n()) %>% collect %>% `[[`("n")
}

.sql_get_field <-
    function(bfc, id, field)
{
    tbl <- .sql_get_resource_table(bfc) %>% dplyr::filter(rid %in% id) %>%
        dplyr::select(rid, field) %>% collect(Inf)
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
        dplyr::select(rid, access_time) %>% collect(Inf)
    accessDate <- as.Date(as.character(mytbl$access_time))
    diffTime <- Sys.Date() - accessDate
    mytbl[diffTime > days, 1] %>% .formatID
}

.get_all_rids <-
    function(bfc)
{
    .sql_get_resource_table(bfc) %>% dplyr::select("rid") %>% .formatID
}

.get_all_web_rids <-
    function(bfc)
{
    .sql_get_resource_table(bfc) %>% dplyr::filter(rtype == "web") %>%
        dplyr::select("rid") %>% .formatID

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
    tryCatch({
        info <- .sql_connect_RW(.sql_dbfile(bfc))
        con <- info$con
        dbWriteTable(con, name, value, ...)
    }, finally={.sql_disconnect(info)})
}

.sql_meta_remove <-
    function(bfc, name, ...)
{
    tryCatch({
        info <- .sql_connect_RW(.sql_dbfile(bfc))
        con <- info$con
        if (dbExistsTable(con, name))
            dbRemoveTable(con, name, ...)
    }, finally={.sql_disconnect(info)})
}

.sql_meta <-
    function(bfc, name, ...)
{
    tryCatch({
        info <- .sql_connect_RO(.sql_dbfile(bfc))
        con <- info$con
        if (!dbExistsTable(con, name))
            stop("'", name, "' not found in database")
        dbReadTable(con, name, ...)
    }, finally={.sql_disconnect(info)})
}

.sql_meta_list <-
    function(bfc)
{
    tryCatch({
        info <- .sql_connect_RO(.sql_dbfile(bfc))
        con <- info$con
        res <- dbListTables(con)
        setdiff(res, .RESERVED$TABLES)
    }, finally={.sql_disconnect(info)})
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
