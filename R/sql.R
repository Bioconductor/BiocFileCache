#' @import RSQLite
#' @importFrom DBI dbExecute dbSendStatement
#' @import dbplyr
#' @importFrom dplyr %>% tbl select_ collect summarize filter_ n left_join

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
    if (mdata[mdata$key=="schema_version",2] != .CURRENT_SCHEMA_VERSION){
        if (!.biocfilecache_flags$get_update_asked())
            .sql_updateOldSchema(bfc)
    }
}

.sql_updateOldSchema <-
    function(bfc)
{
    # This is necessary for a few modifications from the old schema
    # We made web resource rpaths relative since we only allow to use download
    # and checks if using a cache location for the files
    # default last_modified_time to NA instead of Sys.Date for
    # local/relative/Non downloaded web resources
    # We added option to Non download resource which can't default to
    # Sys.Date

    sqlfile <- .sql_dbfile(bfc)
    con <- dbConnect(SQLite(), sqlfile)
    sql <- .sql_cmd("-- SELECT_METADATA")
    mdata <- dbGetQuery(con, sql)
    dbDisconnect(con)

    message("WARNING:\n",
            "Current schema_version ",
            mdata[mdata$key=="schema_version",2]," is out-of-date.\n\n",
            "Current Version will NOT work as expect.\n",
            "Recommend Updating to lastest schema_version.\n",
            "Notable Changes:\n",
            "  1. Web Resource 'rpath' stored as relative path\n",
            "  2. Default last_modified time for\n",
            "     local/relative/nondownloaded/last_modified_notfound\n",
            "     resources is NA not Sys.Date\n",
            "  3. Added etag to schema\n")
    doit <- .util_ask("Update current BiocFileCache to be consistent with\n",
                      "  schema_version: ", .CURRENT_SCHEMA_VERSION, "\n",
                      "  This will be a permanent change but only necessary once.\n",
                      "Y/N: ")
    .biocfilecache_flags$set_update_asked()

    if (!doit){
        return()
    }

    if (mdata[mdata$key=="schema_version",2] == "0.99.1"){
        bfc <- .update_schema_0991(bfc)
    }

    # add new column for etag
    sql <- "ALTER TABLE resource ADD etag TEXT;"
    message("Updating schema to include 'etag'\n")
    res <- .sql_db_execute(bfc, sql)

    # update metadata table for package version and schema
    sql <- paste0("update metadata set value = '",
                  .CURRENT_SCHEMA_VERSION,
                  "' where key = 'schema_version'; ")
    res <- .sql_db_execute(bfc, sql)
    sql <- paste0("update metadata set value = '",
                  as.character(packageVersion("BiocFileCache")),
                  "' where key = 'package_version';")
    res <- .sql_db_execute(bfc, sql)
}

.update_schema_0991 <- function(bfc){

    # truncate rpaths of all web resources
    wid <- .get_all_web_rids(bfc)
    badpaths <- bfcrpath(bfc, rids=wid)
    pattern <- paste0(bfccache(bfc),"/", bfccache(bfc),"/")
    check <- startsWith(badpaths, pattern)
    if (any(!check)){
        ids <- wid[!check]
        filenames <- basename(badpaths[!check])
        warning("Some web resources do not currently have rpath in cache.\n",
                "  Bad paths: ", paste0("'", ids, "'", collapse=" "), "\n",
                "  These resources will now be considered rtype='local'")
        for(i in seq_along(ids)){
            .sql_set_rtype(bfc, ids[i], "local")
        }
    }
    wid <- wid[check]
    badpaths <- badpaths[check]
    newpaths <- gsub(badpaths, pattern=pattern, replacement="")
    message("Updating rpath for the following web resources:\n",
            "  ", paste0("'", wid, "'", collapse=" "))
    for(i in seq_along(wid)){
        .sql_set_rpath(bfc, wid[i], newpaths[i])
    }

    # change local/relative lmt to NA
    nonweb <- setdiff(.get_all_rids(bfc), wid)
    if (length(nonweb) != 0){
        message("Updating last modified time for the following\n",
                "  non web resources:\n",
                "  ", paste0("'", nonweb, "'", collapse=" "))
        for(i in seq_along(nonweb)){
            .sql_set_last_modified(bfc, nonweb[i], NA_character_)
        }
    }
    # check last_modified of all web
    for(i in seq_along(wid)){
        fpath <- .sql_get_fpath(bfc, wid[i])
        check_time <- .httr_get_cache_info(fpath)[["modified"]]
        if (is.na(check_time))
            .sql_set_last_modified(bfc, wid[i], NA_character_)
    }
    bfc
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
    if (length(params) != 0L)
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
    if (identical(rtype, "relative") || identical(rtype, "web"))
        rpath <- basename(rpath)

    if (is.na(fpath))
        fpath <- rpath

    rpath <- paste(rpath, basename(fpath), sep="_")

    if (!is.na(ext))
        rpath <- paste(rpath, ext, sep=".")

    # insert is special case need last_insert_rowid()
    sql <- .sql_cmd("-- INSERT")
    sqls <- strsplit(sql, ";", fixed=TRUE)[[1]]
    sqlfile <- .sql_dbfile(bfc)
    con <- dbConnect(SQLite(), sqlfile)
    for(i in seq_along(sqls)){
        # 1 = INSERT
        # 2 = UPDATE ID
        # 3 = SELECT last_insert_rowid()
        switch(i,
               "1" = {
                   param = list(rname=rname, rpath=rpath,
                       rtype=rtype, fpath=fpath)
                   result <- dbExecute(con, sqls[i], param)
               },
               "2" = {
                   result <- dbExecute(con, sqls[i])
               },
               "3" = {
                   temp <- dbSendQuery(con, sqls[i])
                   result <- dbFetch(temp)
                   dbClearResult(temp)
               })
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
    con <- DBI::dbConnect(RSQLite::SQLite(), .sql_dbfile(bfc))
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
    if (identical(rtype, "relative") || identical(rtype, "web"))
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
    mytbl[diffTime > days,1] %>% .formatID
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
    .sql_db_execute(
        bfc, sql, rid = rid, etag = etag
    )
}

.sql_set_fpath <-
    function(bfc, rid, fpath)
{
    sql <- .sql_cmd("-- UPDATE_FPATH")
    .sql_db_execute(bfc, sql, rid = rid, fpath = fpath)
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
    tbl %>% .formatID
}

.fix_rnames <-
    function(bfc, rnames)
{
    stopifnot(!missing(rnames))
    unname(vapply(rnames, function(bfc, rnames){bfcrid(bfcquery(bfc, rnames))},
                  character(1), bfc=bfc))
}

.get_all_colnames <-
    function(bfc)
{
    colnames(.sql_get_resource_table(bfc))
}

.get_nonrelative_ids <-
    function(bfc)
{
    rpaths <- .get_all_rpath(bfc)
    rids <- bfcrid(bfc)
    cacheloc <- bfccache(bfc)
    res <- startsWith(rpaths, cacheloc)
    rids[!res]
}

.get_local_ids <- function(bfc){

    rids <- bfcrid(bfc)
    rtypes <- vapply(rids, .sql_get_rtype, character(1), bfc = bfc)
    rids[which(rtypes == "local")]
}

.sql_set_relative <-
    function(bfc, rids, action, verbose)
{
    for(rid in rids){
        rpath <- .sql_get_rpath(bfc, rid)
        fileBase <- basename(rpath)
        newpath <- .sql_file(bfc, fileBase)
        if (file.exists(newpath))
            newpath <- paste(path.expand(tempfile("", bfccache(bfc))), fileBase,
                             sep="_")
        switch(
            action,
            copy = file.copy(rpath, newpath),
            move = file.rename(rpath, newpath)
            )
        .sql_set_rpath(bfc, rid, basename(newpath))
        if (identical(.sql_get_rtype(bfc, rid), "local")){
            if (verbose){
                message("Updating 'rtype' from local to relative")
            }
            .sql_set_rtype(bfc, rid, "relative")
        }
    }
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
    if (all(!check)){
        bfcmetaremove(bfc, name)
        vl <- FALSE
    } else if (any(!check)){
        df <- df[check,]
        bfcmeta(bfc, name, overwrite=TRUE) <- df
        vl <- FALSE
    } else {
        vl <- TRUE
    }
    vl
}
