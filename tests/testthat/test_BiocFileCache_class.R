context("BiocFileCache_class")

test_that("BiocFileCache creation works", {
    bfc <- BiocFileCache()
    expect_true(file.exists(bfccache(bfc)))

    # test that sql file also gets created
    expect_true(file.exists(file.path(bfccache(bfc), "BiocFileCache.sqlite")))
    removebfc(bfc, ask=FALSE)
})

test_that("bfcadd and bfcnew works", {
    bfc <- BiocFileCache()
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
    expect_identical(bfc[[rid]], fl)

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
    expect_identical(unname(path),
                     bfc[[names(path)]])

    # test out of bounds and file not found
    expect_error(bfc[[7]])
    expect_error(bfcadd(
        bfc, 'test-6', "https://httpbin.org/status/404", rtype="web"
    ))
    expect_error(bfcadd(bfc, 'test-2', fl, rtype='local', action='asis'))

    # test no fpath given
    url <- "http://httpbin.org/get"
    path <- bfcadd(bfc, url)
    expect_identical(BiocFileCache:::.sql_get_fpath(bfc,names(path)),
                     BiocFileCache:::.sql_get_field(bfc,names(path), "rname"))

    # test web resource not download
    url <- "http://httpbin.org/get"
    path <- bfcadd(bfc, 'test-noDownload', url, rtype="web", download=FALSE)
    rid <- names(path)
    expect_identical(length(bfc), 7L)
    expect_false(file.exists(bfc[[rid]]))
    expect_true(is.na(BiocFileCache:::.sql_get_last_modified(bfc, rid)))

    # test relative paths
    path <- bfcnew(bfc, "relative-test", "relative")
    expect_identical(BiocFileCache:::.sql_get_field(bfc,names(path), "rtype"),
                     "relative")
    temp <- file.path(BiocFileCache::bfccache(bfc),
        BiocFileCache:::.sql_get_field(bfc,names(path), "rpath"))
    expect_identical(BiocFileCache:::.sql_get_rpath(bfc,names(path)), temp)
    basename <- strsplit(BiocFileCache:::.sql_get_field(bfc,
                         names(path), "rpath"), split="_")[[1]][2]
    expect_identical(basename,
                     BiocFileCache:::.sql_get_field(bfc,names(path), "fpath"))

    fl <- tempfile(); file.create(fl)
    path <- bfcadd(bfc, fl, rtype = "relative")
    expect_identical(BiocFileCache:::.sql_get_field(bfc,names(path), "rtype"),
                     "relative")
    temp <- file.path(BiocFileCache::bfccache(bfc),
        BiocFileCache:::.sql_get_field(bfc,names(path), "rpath"))
    expect_identical(BiocFileCache:::.sql_get_rpath(bfc,names(path)), temp)
    expect_true(file.exists(fl))
    path <- bfcadd(bfc, fl, rtype = "relative", action="move")
    expect_identical(BiocFileCache:::.sql_get_field(bfc,names(path), "rtype"),
                     "relative")
    temp <- file.path(BiocFileCache::bfccache(bfc),
        BiocFileCache:::.sql_get_field(bfc,names(path), "rpath"))
    expect_identical(BiocFileCache:::.sql_get_rpath(bfc,names(path)), temp)
    expect_true(!file.exists(fl))
    fl <- tempfile(); file.create(fl)
    expect_warning(bfcadd(bfc, fl, rtype = "relative", action="asis"))
    removebfc(bfc, ask=FALSE)
})

#
# construct bfc for further test, avoiding construction in each
#
bfc <- BiocFileCache()
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
                     c(5L, 8L))
    expect_is(bfcinfo(bfc), "tbl_df")
    # print subset
    expect_identical(dim(as.data.frame(bfcinfo(bfc, paste0("BFC", 1:3)))),
                     c(3L, 8L))
    # print one found and one not found
    expect_error(bfcinfo(bfc, c(1, 6)))

    # index not found
    expect_error(bfcinfo(bfc, 6))
})

test_that("bfcpath and bfcrpath works", {
    # local file
    expect_identical(length(bfcpath(bfc, rid1)), 1L)
    expect_identical(names(bfcpath(bfc, rid1)), as.character(rid1))
    expect_identical(bfcpath(bfc, rid1), bfcrpath(bfc, rids=rid1))

    # web file
    expect_identical(length(bfcpath(bfc, rid3)), 2L)
    expect_identical(names(bfcpath(bfc, rid3)), c(as.character(rid3), "fpath"))
    expect_identical(bfcpath(bfc, rid3)[1], bfcrpath(bfc, rids=rid3))

    # index not found
    expect_error(bfcpath(bfc, 6))
    expect_error(bfcrpath(bfc, rids=6))

    # expect error
    expect_error(bfcrpath(bfc, rnames="testweb", rids="BFC5"))

    # multiple files
    expect_identical(length(bfcrpath(bfc, rids=paste0("BFC", 1:3))), 3L)
    expect_identical(length(bfcrpath(bfc)), 5L)

    # test bfcrpath with rname
    expect_identical(length(bfcrpath(bfc, c("test-1", "test-3"))), 2L)
    expect_error(bfcrpath(bfc, "test"))
    url = "https://en.wikipedia.org/wiki/Bioconductor"
    expect_error(bfcrpath(bfc, c("test-1",url, "notworking")))
    expect_identical(length(bfcrid(bfc)), 5L)
    expect_identical(length(bfcrpath(bfc, c("test-1", url, "test-3"))), 3L)
    expect_identical(length(bfcrid(bfc)), 6L)
    expect_identical(bfccount(bfcinfo(bfc)), 6L)
    expect_identical(BiocFileCache:::.sql_get_field(bfc, "BFC7", "rname"),
                     url)
    expect_identical(BiocFileCache:::.sql_get_field(bfc, "BFC7", "fpath"),
                     url)

})

test_that("check_rtype works", {
    fun <- BiocFileCache:::.util_standardize_rtype

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
    expect_identical(as.data.frame(bfcinfo(bfcsub3)),
                     as.data.frame(bfcinfo(bfc)))

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
    expect_error(bfcupdate(bfc, rid2, fpath="rid2/local/notweb"))
    expect_error(bfcupdate(bfc, rid3, fpath="https://httpbin.org/status/404"))
    expect_error(bfcupdate(bfc, rid2, rpath="path/not/valid"))

    # test update fpath and rname
    link = "https://en.wikipedia.org/wiki/Bioconductor"
    bfcupdate(bfc, rid3, fpath=link, rname="prepQuery")
    vl <- as.character(unname(as.data.frame(
        bfcinfo(bfc,rid3))[c("rname", "fpath")]))
    expect_identical(vl, c("prepQuery", link))
    time <- as.data.frame(bfcinfo(bfc,rid3))$last_modified_time
    expect_identical(time, BiocFileCache:::.httr_get_last_modified(link))

    # test rpath update and give second query example
    bfcupdate(bfc, rid1, rpath=fl2, rname="prepQuery2")
    expect_identical(unname(bfcpath(bfc, rid1)), fl2)

    # test error
    expect_error(bfcupdate(bfc, c(rid2, rid1), rname="oneName"))
    expect_error(bfcupdate(bfc, 1:7))
})

test_that("bfcmeta works", {

    meta = data.frame(list(rid = paste("BFC",seq_len(bfccount(bfc)), sep=""),
                  num=seq(bfccount(bfc),1,-1),
        data=c(paste("Letter", letters[seq_len(bfccount(bfc))]))),
        stringsAsFactors=FALSE)

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
    expect_true(all(q2 == q2b))

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

    # test last modified not available
    expect_false(bfcneedsupdate(bfc, rid3))

    # test last modified not available
    link = "http://google.com"
    bfcupdate(bfc, rid3, fpath=link)
    expect_true(is.na(bfcneedsupdate(bfc, rid3)))
    expect_true(is.na(as.data.frame(bfcinfo(bfc,rid3))$last_modified_time))

    # remove those that aren't web
    expect_identical(
        length(bfcneedsupdate(bfc)),
        length(BiocFileCache:::.get_all_web_rids(bfc))
    )
    expect_identical(
        names(bfcneedsupdate(bfc)),
        as.character(BiocFileCache:::.get_all_web_rids(bfc))
    )

    # test non downloaded is TRUE
    expect_true(bfcneedsupdate(bfc, rid5))

})

test_that("bfcisrelative, bfcrelative, and helpers works",{

    expect_identical(length(BiocFileCache:::.get_local_ids(bfc)), 2L)
    expect_identical(BiocFileCache:::.get_local_ids(bfc), c(rid1, rid2))
    expect_identical(length(BiocFileCache:::.get_nonrelative_ids(bfc)), 1L)
    expect_identical(BiocFileCache:::.get_nonrelative_ids(bfc), rid1)
    fltemp <- file.path(dirname(tempdir()), "tempFile"); file.create(fltemp)
    addtemp <- bfcadd(bfc, "temp", fltemp, rtype="local", action="asis")
    ridtemp <- names(addtemp)
    expect_identical(.sql_get_rtype(bfc, ridtemp), "local")
    expect_identical(length(BiocFileCache:::.get_local_ids(bfc)), 3L)
    expect_identical(BiocFileCache:::.get_local_ids(bfc), c(rid1, rid2,ridtemp))
    expect_false(bfcisrelative(bfc, verbose=FALSE))
    sub1 = bfc[c(rid3, rid4, rid5)]
    expect_true(bfcisrelative(sub1, verbose=FALSE))
    sub2 = bfc[c(rid2, rid3, rid4)]
    expect_false(bfcisrelative(sub2, verbose=FALSE))
    bfcrelative(bfc, ask=FALSE, verbose=FALSE)
    expect_true(bfcisrelative(bfc))
    expect_identical(.sql_get_rtype(bfc, ridtemp), "relative")
    BiocFileCache:::.sql_set_fpath(bfc, ridtemp, "http://httpbin.org/get")
    temp <- BiocFileCache:::.util_rtype_check(bfc, ridtemp, FALSE, FALSE)
    expect_identical(.sql_get_rtype(bfc, ridtemp), "web")
})

removebfc(bfc, ask=FALSE)

test_that("bfcsync and bfcremove works", {

    bfc2 <- BiocFileCache()
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
    bfcupdate(bfc2, rid1, rpath=add3)
    add5 <- bfcnew(bfc2, "test-5", rtype="relative")
    rid5 <- names(add5)
    add6 <- bfcadd(bfc2, "test-6", fl, rtype="relative")
    rid6 <- names(add6)

    # test sync
    expect_message(bfcsync(bfc2))
    expect_false(bfcsync(bfc2, FALSE))
    bfcremove(bfc2, rid4)
    bfcremove(bfc2, rid5)
    files <- file.path(bfccache(bfc2),
                       setdiff(list.files(bfccache(bfc2)),
                               "BiocFileCache.sqlite")
                       )
    # normalizePath on windows
    # can't across platform - no opt on linux but added hidden (private)
    # on mac
    paths <- BiocFileCache:::.get_all_rpath(bfc2)
    if (tolower(.Platform$OS.type) == "windows"){
        files = normalizePath(files)
        paths = normalizePath(paths)
    }
    untracked <- setdiff(files, paths)
    unlink(untracked)
    expect_true(bfcsync(bfc2, FALSE))

    # test that remove, deletes file if in cache
    path <- BiocFileCache:::.sql_get_rpath(bfc2, rid3)
    expect_true(file.exists(path))
    bfcremove(bfc2, rid3)
    expect_false(file.exists(path))

    # test remove leaves file if not in cache
    path <- BiocFileCache:::.sql_get_rpath(bfc2, rid2)
    expect_true(file.exists(path))
    bfcremove(bfc2, rid2)
    expect_true(file.exists(path))
})

test_that("cleanbfc works", {
    # can't test functiuon but test helper
    expect_true(length(BiocFileCache:::.sql_clean_cache(bfc, 1)) == 0L)

    # manually change access_time so longer than a day
    sql<- "UPDATE resource SET access_time = '2016-01-01' WHERE rid = :rid"
    sqlfile <- BiocFileCache:::.sql_db_fetch_query(bfc, sql, rid = rid1)
    expect_identical(BiocFileCache:::.sql_clean_cache(bfc, 1), rid1)
})

test_that("removebfc works", {
    bfc <- BiocFileCache()
    path <- bfccache(bfc)
    expect_true(file.exists(path))
    expect_true(removebfc(bfc, ask=FALSE))
    expect_false(file.exists(path))
})
