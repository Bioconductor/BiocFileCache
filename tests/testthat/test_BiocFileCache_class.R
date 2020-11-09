context("BiocFileCache_class")

test_that("BiocFileCache creation works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    expect_true(file.exists(bfccache(bfc)))

    # test that sql file also gets created
    expect_true(file.exists(file.path(bfccache(bfc), "BiocFileCache.sqlite")))
    removebfc(bfc, ask=FALSE)

    fl <- tempfile()
    bfc <- BiocFileCache(fl, ask = FALSE)
    expect_true(file.exists(fl))
})

test_that("bfcadd and bfcnew works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    fl <- tempfile(); file.create(fl)
    expect_identical(length(bfc), 0L)
    expect_identical(bfccount(bfcinfo(bfc)), 0L)

    # test file add and copy
    rid <- bfcadd(bfc, 'test-1', fl)
    expect_identical(length(bfc), 1L)
    expect_identical(bfccount(bfcinfo(bfc)), 1L)
    expect_true(file.exists(fl))

    # test file add and location not in cache
    path <- bfcadd(bfc, 'test-2', fl, rtype='local', action='asis')
    rid <- names(path)
    expect_identical(length(bfc), 2L)
    expect_true(file.exists(fl))
    expect_identical(bfc[[rid]], setNames(fl, rid))

    # test file add and move
    rid <- bfcadd(bfc, 'test-3', fl, action='move')
    expect_identical(length(bfc), 3L)
    expect_true(!file.exists(fl))

    # test add web resource
    url <- "http://httpbin.org/get"
    path <- bfcadd(bfc, 'test-4', url, rtype="web")
    rid <- names(path)
    expect_identical(length(bfc), 4L)
    expect_true(file.exists(bfc[[rid]]))

    # test add new (return path to save)
    path <- bfcnew(bfc, 'test-5')
    expect_identical(length(bfc), 5L)
    expect_identical(bfccount(bfcinfo(bfc)), 5L)
    expect_true(!file.exists(path))
    expect_identical(bfc[[names(path)]], path)

    # test out of bounds and file not found
    expect_error(bfc[[7]])
    suppressWarnings(expect_error(bfcadd(
            bfc, 'test-6', "https://httpbin.org/status/404", rtype="web"
    )))
    expect_error(bfcadd(bfc, 'test-2', fl, rtype='local', action='asis'))

    # test no fpath given
    url <- "http://httpbin.org/get"
    path <- bfcadd(bfc, url)
    expect_identical(.sql_get_fpath(bfc,names(path)),
                     .sql_get_rname(bfc,names(path)))

    # test web resource not download
    url <- "http://httpbin.org/get"
    path <- bfcadd(bfc, 'test-noDownload', url, rtype="web", download=FALSE)
    rid <- names(path)
    expect_identical(length(bfc), 7L)
    expect_false(file.exists(bfc[[rid]]))
    expect_true(is.na(.sql_get_last_modified(bfc, rid)))

    # test relative paths
    path <- bfcnew(bfc, "relative-test", "relative")
    expect_identical(
        .sql_get_rtype(bfc,names(path)), setNames("relative", names(path))
    )
    temp <- file.path(bfccache(bfc), .sql_get_field(bfc,names(path), "rpath"))
    expect_identical(
        .sql_get_rpath(bfc,names(path)), setNames(temp, names(path))
    )
    basename <- strsplit(
        .sql_get_field(bfc, names(path), "rpath"),
        split="_"
    )[[1]][2]
    expect_identical(
        .sql_get_fpath(bfc,names(path)), setNames(basename, names(path))
    )


    fl <- tempfile(); file.create(fl)
    path <- bfcadd(bfc, fl, rtype = "relative")
    expect_identical(
        .sql_get_rtype(bfc,names(path)), setNames("relative", names(path))
    )

    temp <- file.path(bfccache(bfc), .sql_get_field(bfc,names(path), "rpath"))
    expect_identical(
        .sql_get_rpath(bfc,names(path)), setNames(temp, names(path))
    )
    expect_true(file.exists(fl))

    path <- bfcadd(bfc, fl, rtype = "relative", action="move")
    expect_identical(
        .sql_get_rtype(bfc,names(path)), setNames("relative", names(path))
    )

    temp <- file.path(bfccache(bfc), .sql_get_field(bfc,names(path), "rpath"))
    expect_identical(
        .sql_get_rpath(bfc,names(path)), setNames(temp, names(path))
    )
    expect_true(!file.exists(fl))

    fl <- tempfile(); file.create(fl)
    expect_warning(bfcadd(bfc, fl, rtype = "relative", action="asis"))
})

test_that("bfcadd() works for multiple inserts", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    fpath <- replicate(6L, tempfile())
    file.create(fpath)
    rname <- letters[seq_along(fpath)]

    rpath <- bfcadd(bfc, rname[1:2], fpath[1:2], action = "asis")
    expect_identical(rpath, setNames(fpath[1:2], names(rpath)))

    rpath <- bfcadd(bfc, rname[3], fpath[3], action = "asis")
    expect_identical(rpath, setNames(fpath[3], names(rpath)))

    rpath <- bfcadd(bfc, rname[4:5], fpath[4:5])
    expect_identical(names(rpath), paste0("BFC", 4:5))
    expect_true(all(file.exists(rpath)))

    rpath <- bfcadd(bfc, rname[6], fpath[6])
    expect_identical(names(rpath), paste0("BFC", 6))
    expect_true(all(file.exists(rpath)))
})

test_that("bfcnew() works for multiple inserts", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)

    rnames <- paste0("foo", 1:2)
    rpath <- bfcnew(bfc, rnames)
    expect_identical(bfcinfo(bfc)$rname, rnames)

    rnames <- "foo3"
    rpath <- bfcnew(bfc, rnames, ext=".foo3")
    expect_identical(tools::file_ext(rpath), "foo3")

    rnames <- paste0("foo", 4:5)
    rpath <- bfcnew(bfc, rnames, ext=".foo4")
    expect_identical(tools::file_ext(rpath), rep("foo4", 2))

    rnames <- paste0("foo", 6:7)
    ext <- paste0(".", rnames)
    rpath <- bfcnew(bfc, rnames, ext=ext)
    expect_identical(tools::file_ext(rpath), rnames)
})

#
# construct bfc for further test, avoiding construction in each
#
bfc <- BiocFileCache(tempfile(), ask = FALSE)
fl <- tempfile(); file.create(fl)
add1 <- bfcadd(bfc, 'test-1', fl)
rid1 <- names(add1)
add2 <- bfcadd(bfc, 'test-2', fl, rtype='local', action='asis')
rid2 <- names(add2)
url <- "http://httpbin.org/get"
add3 <- bfcadd(bfc, 'test-3', url, rtype="web")
rid3 <- names(add3)
path <- bfcnew(bfc, 'test-4')
rid4 <- names(path)
url <- "http://httpbin.org/get"
add5 <- bfcadd(bfc, 'test-5', url, rtype="web", download=FALSE)
rid5 <- names(add5)

test_that("bfcinfo works", {
    # print all
    expect_identical(dim(as.data.frame(bfcinfo(bfc))),
                     c(5L, 10L))
    expect_is(bfcinfo(bfc), "tbl_df")
    # print subset
    expect_identical(dim(as.data.frame(bfcinfo(bfc, paste0("BFC", 1:3)))),
                     c(3L, 10L))
    # print one found and one not found
    expect_error(bfcinfo(bfc, c(1, 6)))

    # index not found
    expect_error(bfcinfo(bfc, 6))

    # check rpaths updated
    expect_identical(bfcinfo(bfc)[["rpath"]], unname(bfcrpath(bfc)))
})

test_that("bfcpath and bfcrpath works", {
    # local file
    expect_identical(length(bfcpath(bfc, rid1)), 1L)
    expect_identical(names(bfcpath(bfc, rid1)), as.character(rid1))
    expect_identical(bfcpath(bfc, rid1), bfcrpath(bfc, rids=rid1))

    # web file
    expect_identical(length(bfcpath(bfc, rid3)), 1L)
    expect_identical(names(bfcpath(bfc, rid3)), as.character(rid3))
    expect_identical(bfcpath(bfc, rid3), bfcrpath(bfc, rids=rid3))

    # index not found
    expect_error(bfcpath(bfc, 6))
    expect_error(bfcrpath(bfc, rids=6))

    # expect error
    expect_error(bfcrpath(bfc, rnames="testweb", rids="BFC5"))

    # multiple files
    expect_identical(length(bfcrpath(bfc, rids=paste0("BFC", 1:3))), 3L)
    expect_identical(length(bfcrpath(bfc)), 5L)
    expect_identical(length(bfcpath(bfc)), length(bfc))
    expect_identical(length(bfcpath(bfc, rids=paste0("BFC", 1:3))), 3L)
    expect_identical(length(bfcpath(bfc)), length(bfcrpath(bfc)))
    
    # test bfcrpath with rname
    expect_identical(length(bfcrpath(bfc, c("test-1", "test-3"))), 2L)
    suppressWarnings(expect_error(bfcrpath(bfc, "test")))
    url = "https://en.wikipedia.org/wiki/Bioconductor"
    suppressWarnings(expect_error(bfcrpath(bfc, c("test-1",url, "notworking"))))
    expect_identical(length(bfcrid(bfc)), 5L)
    expect_identical(length(bfcrpath(bfc, c("test-1", url, "test-3"))), 3L)
    expect_identical(length(bfcrid(bfc)), 6L)
    expect_identical(bfccount(bfcinfo(bfc)), 6L)
    expect_identical(unname(.sql_get_field(bfc, "BFC7", "rname")), url)
    expect_identical(unname(.sql_get_fpath(bfc, "BFC7")), url)
})

test_that("bfcquery, bfcrpath allow regular expressions and exact matches", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    file.create(fl <- tempfile())
    fl1 <- bfcadd(bfc, "fl1", fl)
    fl10 <- bfcadd(bfc, "fl10", fl)
    ## bfcquery
    expect_identical(NROW(bfcquery(bfc, "fl1", "rname")), 2L)
    expect_identical(NROW(bfcquery(bfc, "fl1", "rname", exact = TRUE)), 1L)
    expect_identical(NROW(bfcquery(bfc, "fl", "rname", exact = TRUE)), 0L)
    ## bfcrpath
    expect_error(
        suppressWarnings(bfcrpath(bfc, "fl1", exact = FALSE)),
        "not all 'rnames' found or unique."
    )
    expect_identical(bfcrpath(bfc, "fl1", exact = TRUE), fl1)
    expect_identical(bfcrpath(bfc, "fl1$", exact = FALSE), fl1)
})

test_that("check_rtype works", {
    fun <- .util_standardize_rtype_helper

    # test web types
    expect_identical(fun("auto", "http://somepath.com"), "web")
    expect_identical(fun("auto", "ftp://somepath.com"), "web")
    expect_identical(fun("local", "https://some.path", "web"), "local")
    expect_identical(fun("relative", "https://some.path", "web"), "relative")

    # test not web type
    expect_identical(fun("auto", "not/a/web/path", "copy"), "relative")
    expect_identical(fun("auto", "not/a/web/path", "move"), "relative")
    expect_identical(fun("auto", "not/a/web/path", "asis"), "local")

    # expect noopt
    expect_identical(fun("local", "some/path", "copy"), "local")
    expect_identical(fun("local", "some/path", "move"), "local")
    expect_identical(fun("local", "some/path", "asis"), "local")

    expect_identical(fun("relative", "some/path", "copy"), "relative")
    expect_identical(fun("relative", "some/path", "move"), "relative")
    expect_warning(fun("relative", "some/path", "asis"))
    suppressWarnings({
        expect_identical(fun("relative", "some/path", "asis"), "local")
    })
})

test_that("subsetting works", {
    # out of bounds
    expect_error(bfc[3:5])
    expect_error(bfc[10])

    # empty
    bfcsub3 <- bfc[]
    expect_identical(length(bfcsub3), length(bfc))
    subin <- as.data.frame(
        bfcinfo(bfcsub3)[,-which(names(bfcinfo(bfcsub3)) == "access_time")])
    bfcin <- as.data.frame(
        bfcinfo(bfc)[,-which(names(bfcinfo(bfc)) == "access_time")])
    expect_identical(subin, bfcin)

    # test restricted methods on subset
    expect_error(bfcnew(bfcsub3))
    expect_error(bfcadd(bfcsub3))
    expect_error(bfcupdate(bfcsub3, rname="test"))
    fltemp <- tempfile(); file.create(fltemp)
    expect_error(bfcsub3[[2]] <- fltemp)
})

test_that("bfcupdate works", {
    # test [[<-, only updates rpath
    fl2 <- tempfile(); file.create(fl2)
    bfc[[rid2]] <- fl2
    expect_identical(unname(bfcpath(bfc, rid2)), fl2)
    expect_error(bfc[[rid1]] <- "A/file/doesnt/work")

    # test errors, files not found
    expect_error(bfcupdate(bfc, rid2, fpath="rid2/local/notweb", ask=FALSE))
    suppressWarnings(expect_error(bfcupdate(
        bfc, rid3, fpath="https://httpbin.org/status/404", ask=FALSE
    )))
    expect_error(bfcupdate(bfc, rid2, rpath="path/not/valid", ask=FALSE))

    # test update fpath and rname
    link = "https://en.wikipedia.org/wiki/Bioconductor"
    suppressWarnings(bfcupdate(
        bfc, rid3, fpath=link, rname="prepQuery", ask=FALSE
    ))
    vl <- as.character(unname(as.data.frame(
        bfcinfo(bfc,rid3))[c("rname", "fpath")]))
    expect_identical(vl, c("prepQuery", link))
    time <- as.data.frame(bfcinfo(bfc,rid3))$last_modified_time
    expect_identical(time,
                     .httr_get_cache_info(link)[["modified"]])

    # test rpath update and give second query example
    suppressWarnings(bfcupdate(bfc, rid1, rpath=fl2, rname="prepQuery2"))
    expect_identical(unname(bfcpath(bfc, rid1)), fl2)

    # test error
    expect_error(bfcupdate(bfc, c(rid2, rid1), rname="oneName"))
    expect_error(bfcupdate(bfc, 1:7))
})

test_that("bfcmeta works", {
    meta <- data.frame(
        rid=paste("BFC", seq_len(bfccount(bfc)), sep=""),
        num=seq(bfccount(bfc), 1, -1),
        data=c(paste("Letter", letters[seq_len(bfccount(bfc))])),
        stringsAsFactors=FALSE
    )

    # test no meta
    expect_identical(bfcmetalist(bfc), character(0))
    expect_identical(names(bfcinfo(bfc)),bfcquerycols(bfc))

    # try add meta with bad rid
    expect_error(bfcmeta(bfc, name="resourcedata") <- meta)
    # add valid
    meta$rid[6] = "BFC7"
    metaOrig = meta
    bfcmeta(bfc, name="resourcedata") <- meta
    expect_identical(bfcmetalist(bfc),"resourcedata")
    expect_true("resourcedata" %in% bfcmetalist(bfc))

    # add additional
    bfcmeta(bfc, name="table2") <- meta
    expect_identical(length(bfcmetalist(bfc)), 2L)
    expect_true("table2" %in% bfcmetalist(bfc))

    # try and add same table name
    meta$num = seq(1, bfccount(bfc), 1)
    expect_error(bfcmeta(bfc, name="table2") <- meta)
    bfcmeta(bfc, name="table2", overwrite=TRUE) <- meta
    expect_identical(length(bfcmetalist(bfc)), 2L)

    # try and add reserved table name
    expect_error(bfcmeta(bfc, name="metadata") <- meta)

    # try and add with reserved col name
    names(meta)[2] = "rpath"
    expect_error(bfcmeta(bfc, name="table3") <- meta)

    # try add meta with missing column rid
    names(meta)[1:2] = c("id", "num")
    expect_error(bfcmeta(bfc, name="table3") <- meta)

    # remove table
    bfcmetaremove(bfc, "table2")
    expect_identical(length(bfcmetalist(bfc)), 1L)
    expect_true(!("table2" %in% bfcmetalist(bfc)))

    # try and remove reserved table
    expect_error(bfcmetaremove(bfc, "metadata"))

    # retrieve table
    metaGet <- bfcmeta(bfc, "resourcedata")
    expect_true(all(metaGet == metaOrig))

    # retrieve bad table
    expect_error(bfcmeta(bfc, "table2"))

    # querycols should include meta columns
    expect_true(all(names(metaOrig) %in% bfcquerycols(bfc)))
    expect_identical(names(bfcinfo(bfc)),bfcquerycols(bfc))

})

test_that("bfcquery and bfccount works", {

    # test count
    expect_identical(bfccount(bfc), bfccount(bfcinfo(bfc)))
    expect_identical(bfccount(bfc), length(bfc))

    # query found
    q1 <- as.data.frame(bfcquery(bfc, "prep"))
    expect_identical(dim(q1)[1], 2L)
    expect_identical(q1$rid, c(rid1,rid3))

    # test query on fpath
    q2 <- as.data.frame(bfcquery(bfc, "wiki"))
    expect_identical(dim(q2)[1], 2L)
    q2b <- as.data.frame(bfcquery(bfc, "wiki", field="fpath"))
    q2 <- q2[,-which(names(q2) == "access_time")]
    q2b <- q2b[,-which(names(q2b) == "access_time")]
    expect_true(all(q2 == q2b, na.rm=TRUE))

    # query not found
    expect_identical(bfccount(bfcquery(bfc, "nothere")), 0L)

    # multiple value all found
    path <- file.path(bfccache(bfc), "myFile")
    file.create(path)
    bfc[[rid2]] <- path
    q3 <- as.data.frame(bfcquery(bfc, c("test-2", "myF")))
    expect_identical(dim(q3)[1], 1L)
    expect_identical(q3$rid, rid2)

    # multi value some not found
    expect_identical(bfccount(bfcquery(bfc, c("prep", "not"))), 0L)

    # test case sensitive
    q3 <- as.data.frame(bfcquery(bfc, c("test-2", "myf")))
    expect_identical(dim(q3)[1], 0L)
    q3 <- as.data.frame(bfcquery(bfc, c("test-2", "myf"), ignore.case=TRUE))
    expect_identical(dim(q3)[1], 1L)

    # test exact
    q4 <- as.data.frame(bfcquery(bfc, "^test-4$"))
    expect_identical(dim(q4)[1], 1L)

})

test_that("bfcneedsupdate works", {
    # test not web source
    expect_error(bfcneedsupdate(bfc, rid4))
    # test out of bounds
    expect_error(bfcneedsupdate(bfc, 7))

    # test expires and last modified not available
    link = "http://httpbin.org/get"
    bfcupdate(bfc, rid3, fpath=link, ask=FALSE)
    expect_true(is.na(bfcneedsupdate(bfc, rid3)))
    expect_true(is.na(as.data.frame(bfcinfo(bfc,rid3))$last_modified_time))
    expect_true(is.na(as.data.frame(bfcinfo(bfc,rid3))$expires))

    # remove those that aren't web
    expect_identical(
        length(bfcneedsupdate(bfc)),
        length(.get_all_web_rids(bfc))
    )
    expect_identical(
        names(bfcneedsupdate(bfc)),
        as.character(.get_all_web_rids(bfc))
    )

    # test non downloaded is TRUE
    expect_true(bfcneedsupdate(bfc, rid5))

    # test etag available and check order
    link = "https://www.wikipedia.org/"
    bfcupdate(bfc, rid3, fpath=link, ask=FALSE)
    cache_info <- .httr_get_cache_info(link)
    expect_identical(as.data.frame(bfcinfo(bfc,rid3))$last_modified_time,
                     cache_info[["modified"]])
    expect_identical(as.data.frame(bfcinfo(bfc,rid3))$etag,
                     cache_info[["etag"]])
    expect_true(!is.na(bfcneedsupdate(bfc, rid3)))
    expect_true(!is.na(as.data.frame(bfcinfo(bfc,rid3))$etag))
    # wiki has expires so manually set to NA for testing
    .sql_set_expires(bfc, rid3, NA_character_)
    expect_false(bfcneedsupdate(bfc, rid3))
    .sql_set_etag(bfc, rid3, "somethingElse")
    expect_true(bfcneedsupdate(bfc, rid3))
    .sql_set_etag(bfc, rid3, NA_character_)
    expect_false(bfcneedsupdate(bfc, rid3))
    .sql_set_last_modified(bfc, rid3,
                  as.character(as.Date(.sql_get_last_modified(bfc, rid3)) - 1))
    expect_true(bfcneedsupdate(bfc, rid3))

    # maually test expires
    .sql_set_expires(bfc, rid3, as.character(as.Date(Sys.time()) + 2))
    .sql_set_etag(bfc, rid3, NA_character_)
    .sql_set_last_modified(bfc, rid3,NA_character_)
    expect_true(is.na(bfcneedsupdate(bfc, rid3)))
    .sql_set_expires(bfc, rid3, as.character(as.Date(Sys.time()) -1))
    expect_true(bfcneedsupdate(bfc, rid3))
})

test_that("bfcdownload works", {
    response <- .biocfilecache_flags$set_ask_response(FALSE)

    time1 <- file.info(.sql_get_rpath(bfc, rid3))[["ctime"]]
    temp <- bfcdownload(bfc, rid3, ask=TRUE)
    time2 <- file.info(.sql_get_rpath(bfc, rid3))[["ctime"]]
    expect_identical(time1, time2)

    temp <- bfcdownload(bfc, rid3, ask=FALSE)
    time3 <- file.info(.sql_get_rpath(bfc, rid3))[["ctime"]]
    expect_true(time1 < time3)
    expect_error(bfcdownload(bfc, rid1))

    url <- "http://bioconductor.org/packages/stats/bioc/BiocFileCache/BiocFileCache_stats.tab"
    headFile <-
    function(url, file)
        {
            dat <- readLines(url)
            dat <- head(dat, n=3L)
            writeLines(dat, file)
            TRUE
        }
    rid <- names(bfcadd(bfc, rname="testFun", fpath=url, download=FALSE))
    temp <- bfcdownload(bfc, rid, FUN=headFile)
    file <- readLines(temp)
    expect_identical(length(file), 3L)

    expect_error(bfcdownload(bfc, rid, ask=FALSE, FUN=rnorm))

    .biocfilecache_flags$set_ask_response(response)
})

test_that("exportbfc and importbfc works",{

    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    fl <- tempfile(); file.create(fl)
    add1 <- bfcadd(bfc, 'relative', fl)
    rid1 <- names(add1)
    add2 <- bfcadd(bfc, 'local', fl, rtype='local', action='asis')
    rid2 <- names(add2)
    url <- "http://httpbin.org/get"
    add3 <- bfcadd(bfc, 'web', url, rtype="web")
    rid3 <- names(add3)
    path <- bfcnew(bfc, 'notfound')
    rid4 <- names(path)
    url <- "http://httpbin.org/get"
    add5 <- bfcadd(bfc, 'webno', url, rtype="web", download=FALSE)
    rid5 <- names(add5)

    dirloc <- dirname(bfccache(bfc))
    temploc <- file.path(dirloc, "ExportTest")
    dir.create(temploc)
    ids <- bfcrid(bfc)
    res <- vapply(ids, .util_export_file, character(1),
                  bfc=bfc, dir=temploc)
    expect_identical(length(list.files(temploc)), 2L)
    expect_identical(unname(res),
                     c("relative", "local", "relative", NA_character_, "web"))
    .util_unlink(temploc, recursive=TRUE)
    expect_false(file.exists(file.path(dirloc, "BFCExport.tar")))
    file <- exportbfc(bfc, outputFile=file.path(dirloc, "BFCExport.tar"),
                               verbose=FALSE)
    expect_true(file.exists(file.path(dirloc, "BFCExport.tar")))
    expect_false(dir.exists("BiocFileCacheExport"))
    bfc2 <- importbfc(file, exdir=dirloc)
    expect_true(dir.exists(file.path(dirloc,"BiocFileCacheExport")))
    expect_identical(bfccount(bfc2), 4L)
    locpath <- file.path(dirloc, "BiocFileCacheExport")
    expect_true(file.exists(file.path(locpath,"BiocFileCache.sqlite")))
    ## add 1 for file LOCK
    expect_identical(length(list.files(locpath)), 5L)
    sub <- bfc[c(rid1,rid2)]
    .util_unlink(locpath, recursive=TRUE)
    file.remove(file)
    file <- exportbfc(sub, outputFile=file.path(dirloc, "SubExport.zip"),
                      verbose=FALSE, outputMethod="zip")
    expect_true(file.exists(file.path(dirloc, "SubExport.zip")))
    bfc3 <- importbfc(file, exdir=dirloc, archiveMethod="unzip")
    expect_identical(bfccount(bfc3), 2L)
    .util_unlink(locpath, recursive=TRUE)
    file.remove(file)
    removebfc(bfc, ask=FALSE)
})

test_that("bfcsync and bfcremove works", {
    response <- .biocfilecache_flags$set_ask_response(FALSE)
    ## setup
    bfc2 <- BiocFileCache(tempfile(), ask = FALSE)
    fl <- tempfile(); file.create(fl)
    add1 <- bfcadd(bfc2, 'test-1', fl)
    rid1 <- names(add1)
    add2 <- bfcadd(bfc2, 'test-2', fl, rtype='local', action='asis')
    rid2 <- names(add2)
    url <- "http://httpbin.org/get"
    add3 <- bfcadd(bfc2, 'test-3', url, rtype="web")
    rid3 <- names(add3)
    path <- bfcnew(bfc2, 'test-4')
    rid4 <- names(path)
    suppressWarnings(bfcupdate(bfc2, rid1, rpath=add3))
    add5 <- bfcnew(bfc2, "test-5", rtype="relative")
    rid5 <- names(add5)
    add6 <- bfcadd(bfc2, "test-6", fl, rtype="relative")
    rid6 <- names(add6)

    # test sync
    expect_message(bfcsync(bfc2))
    expect_false(bfcsync(bfc2, FALSE))

    bfcremove(bfc2, rid4)
    bfcremove(bfc2, rid5)
    files <- file.path(
        bfccache(bfc2),
        setdiff(list.files(bfccache(bfc2)), c("BiocFileCache.sqlite", "BiocFileCache.sqlite.LOCK"))
    )
    # normalizePath on windows
    # can't across platform - no opt on linux but added hidden (private)
    # on mac
    paths <- .sql_get_rpath(bfc2, bfcrid(bfc2))
    if (tolower(.Platform$OS.type) == "windows"){
        files = normalizePath(files)
        paths = normalizePath(paths)
    }
    untracked <- setdiff(files, paths)
    .util_unlink(untracked)
    expect_true(bfcsync(bfc2, FALSE))

    # test that remove, deletes file if in cache
    path <- .sql_get_rpath(bfc2, rid3)
    expect_true(file.exists(path))
    bfcremove(bfc2, rid3)
    expect_false(file.exists(path))

    # test remove leaves file if not in cache
    path <- .sql_get_rpath(bfc2, rid2)
    expect_true(file.exists(path))
    bfcremove(bfc2, rid2)
    expect_true(file.exists(path))

    .biocfilecache_flags$set_ask_response(response)
})

test_that("cleanbfc works", {
    # can't test functiuon but test helper
    expect_true(length(.sql_clean_cache(bfc, 1)) == 0L)

    # manually change access_time so longer than a day
    sql<- "UPDATE resource SET access_time = '2016-01-01' WHERE rid = :rid"
    .sql_db_execute(bfc, sql, rid = rid1)
    expect_identical(.sql_clean_cache(bfc, 1), rid1)

    ## bfclean() works on an empty cache
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    expect_identical(character(0), .sql_clean_cache(bfc, 1L))
})

test_that("removebfc works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    path <- bfccache(bfc)
    expect_true(file.exists(path))
    expect_true(removebfc(bfc, ask=FALSE))
    expect_false(file.exists(path))
})
