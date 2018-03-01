.sql_validate_version <-
    function(bfc)
{
    schema_version <- .sql_schema_version(bfc)

    if (!schema_version %in% .SUPPORTED_SCHEMA_VERSIONS)
        stop(
            "unsupported schema version ",
            "\n  sqlite file: ", .sql_dbfile(bfc),
            "\n  file schema version: '", schema_version, "'",
            "\n  supported version(s): ",
            paste(sQuote(.SUPPORTED_SCHEMA_VERSIONS), collapse=" ")
        )

    if (schema_version != .CURRENT_SCHEMA_VERSION)
        .sql_migration(bfc)

    schema_version
}

.sql_migration_update_schema_version <-
    function(bfc, schema_version)
{
    ## update metadata table for package version and schema
    sql <- .sql_cmd("-- MIGRATION_UPDATE_METADATA")
    .sql_db_execute(bfc, sql, key = "schema_version", value = schema_version)
    .sql_db_execute(
        bfc, sql, key = "package_version",
        value = as.character(packageVersion("BiocFileCache"))
    )

    schema_version
}

.sql_migration <-
    function(bfc)
{
    schema_version <- .sql_schema_version(bfc)

    if (.biocfilecache_flags$get_update_asked())
        return(schema_version)

    ## This is necessary for a few modifications from the old schema
    ## We made web resource rpaths relative since we only allow to use
    ## download and checks if using a cache location for the files
    ## default last_modified_time to NA instead of Sys.Date for
    ## local/relative/Non downloaded web resources We added option to
    ## Non download resource which can't default to Sys.Date

    message("Current schema_version ", schema_version, " is out-of-date.\n\n",
            "Current Version will NOT work as expect.\n",
            "Recommend Updating to lastest schema_version.\n",
            "Notable Changes:\n",
            "  1. Web Resource 'rpath' stored as relative path\n",
            "  2. Default last_modified time for\n",
            "     local/relative/nondownloaded/last_modified_notfound\n",
            "     resources is NA not Sys.Date\n",
            "  3. Added etag to schema\n",
            "  4. Added expires to schema\n")
    doit <- .util_ask(
        "Update current BiocFileCache to be consistent with\n",
        "  schema_version: ", .CURRENT_SCHEMA_VERSION, "\n",
        "  This will be a permanent change but only necessary once.\n",
        "  Continue?"
    )
    .biocfilecache_flags$set_update_asked()

    if (!doit) {
        warning("BiocFileCache schema not updated\n",
                "  bfccache(): ", bfccache(bfc))
        return()
    }

    if (schema_version == "0.99.1")
        schema_version <- .sql_migration_0991_to_0992(bfc)

    if (schema_version == "0.99.2")
        schema_version <- .sql_migration_0992_to_0993(bfc)

    if (schema_version == "0.99.3")
        schema_version <- .sql_migration_0993_to_0994(bfc)

    schema_version
}

.sql_migration_0993_to_0994 <-
    function(bfc)
{
    message("applying migration from 0.99.3 to 0.99.4")
    sql <- .sql_cmd("-- MIGRATION_0_99_3_to_0_99_4")
    .sql_db_execute(bfc, sql)
    .sql_migration_update_schema_version(bfc, "0.99.4")
}

.sql_migration_0992_to_0993 <-
    function(bfc)
{
    message("applying migration from 0.99.2 to 0.99.3")
    sql <- .sql_cmd("-- MIGRATION_0_99_2_to_0_99_3")
    .sql_db_execute(bfc, sql)
    .sql_migration_update_schema_version(bfc, "0.99.3")
}

.sql_migration_0991_to_0992 <-
    function(bfc)
{
    message("applying migration from 0.99.1 to 0.99.2")
    ## truncate rpaths of all web resources
    wid <- .get_all_web_rids(bfc)
    badpaths <- bfcrpath(bfc, rids=wid)
    pattern <- paste0(bfccache(bfc),"/", bfccache(bfc),"/")
    check <- startsWith(badpaths, pattern)
    if (any(!check)) {
        ids <- wid[!check]
        warning("Some web resources do not currently have rpath in cache.\n",
                "  Bad paths: ", paste0("'", ids, "'", collapse=" "), "\n",
                "  These resources will now be considered rtype='local'")
        .sql_set_rtype(bfc, ids, "local")
    }

    wid <- wid[check]
    badpaths <- badpaths[check]
    newpaths <- gsub(badpaths, pattern=pattern, replacement="")
    message("Updating rpath for the following web resources:\n",
            "  ", paste0("'", wid, "'", collapse=" "))
    for(i in seq_along(wid)){
        .sql_set_rpath(bfc, wid[i], newpaths[i])
    }

    ## change local/relative lmt to NA
    nonweb <- setdiff(.get_all_rids(bfc), wid)
    if (length(nonweb) != 0) {
        message("Updating last modified time for the following\n",
                "  non web resources:\n",
                "  ", paste0("'", nonweb, "'", collapse=" "))
        .sql_set_last_modified(bfc, nonweb, NA_character_)
    }

    ## check last_modified of all web
    for (i in seq_along(wid)) {
        fpath <- .sql_get_fpath(bfc, wid[i])
        check_time <- .httr_get_cache_info(fpath)[["modified"]]
        if (is.na(check_time))
            .sql_set_last_modified(bfc, wid[i], NA_character_)
    }

    .sql_migration_update_schema_version(bfc, "0.99.2")
}
