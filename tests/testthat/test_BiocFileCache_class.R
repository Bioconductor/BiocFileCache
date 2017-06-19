context("BiocFileCache_class")

test_that("BiocFileCache creation works", {
    dir <- tempfile()

    bfc <- BiocFileCache(dir)
    expect_identical(bfccache(bfc), dir)
    expect_true(file.exists(bfccache(bfc)))

    # test that sql file also gets created
    expect_true(file.exists(file.path(bfccache(bfc), "BiocFileCache.sqlite")))
})

test_that("bfcadd and bfcnew works", {
    bfc <- BiocFileCache(tempfile())
    fl <- tempfile(); file.create(fl)

    # test file add and copy
    rid <- bfcadd(bfc, 'test-1', fl)
    expect_identical(length(bfc), 1L)
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

})

#
# construct bfc for further test, avoiding construction in each
#
bfc <- BiocFileCache(tempfile())
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

test_that("bfcinfo works", {
    # print all
    expect_identical(dim(as.data.frame(bfcinfo(bfc))),
                     c(4L, 8L))
    expect_is(bfcinfo(bfc), "tbl_sql")
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
    expect_identical(length(bfcrpath(bfc)), 4L)

    # test bfcrpath with rname
    expect_identical(length(bfcrpath(bfc, c("test-1", "test-3"))), 2L)
    expect_error(bfcrpath(bfc, "test"))
    url = "https://en.wikipedia.org/wiki/Bioconductor"
    expect_error(bfcrpath(bfc, c("test-1",url, "notworking")))
    expect_identical(length(bfcrid(bfc)), 4L)
    expect_identical(length(bfcrpath(bfc, c("test-1", url, "test-3"))), 3L)
    expect_identical(length(bfcrid(bfc)), 5L)
    expect_identical(BiocFileCache:::.sql_get_field(bfc, "BFC6", "rname"),
                     url)
    expect_identical(BiocFileCache:::.sql_get_field(bfc, "BFC6", "fpath"),
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
    expect_error(bfcupdate(bfc, rid3, fpath="http://notworking/web"))
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

test_that("bfcquery works", {
    # query found
    q1 <- as.data.frame(bfcquery(bfc, "prep"))
    expect_identical(dim(q1), c(2L,8L))
    expect_identical(q1$rid, c(rid1,rid3))

    # test query on fpath
    q2 <- as.data.frame(bfcquery(bfc, "wiki"))
    expect_identical(dim(q2), c(2L,8L))

    # query not found
    expect_identical(bfccount(bfcquery(bfc, "nothere")), 0L)

    # multiple value all found
    path <- file.path(bfccache(bfc), "myFile")
    file.create(path)
    bfc[[rid3]] <- path
    q3 <- as.data.frame(bfcquery(bfc, c("prep", "myf")))
    expect_identical(dim(q3), c(1L,8L))
    expect_identical(q3$rid, rid3)

    # multi value some not found
    expect_identical(bfccount(bfcquery(bfc, c("prep", "not"))), 0L)
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
    expect_identical(
        as.character(Sys.Date()),
        as.data.frame(bfcinfo(bfc,rid3))$last_modified_time
    )

    # remove those that aren't web
    expect_identical(
        length(bfcneedsupdate(bfc)),
        length(BiocFileCache:::.get_all_web_rids(bfc))
    )
    expect_identical(
        names(bfcneedsupdate(bfc)),
        as.character(BiocFileCache:::.get_all_web_rids(bfc))
    )
})

test_that("bfcsync and bfcremove works", {

    bfc2 <- BiocFileCache(tempfile())
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
    bfc <- BiocFileCache(tempfile())
    path <- bfccache(bfc)
    expect_true(file.exists(path))
    expect_true(removebfc(bfc, ask=FALSE))
    expect_false(file.exists(path))
})
