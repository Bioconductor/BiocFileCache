test_that("BiocFileCache creation works", {
    dir <- tempfile()

    bfc <- BiocFileCache(dir)
    expect_identical(bfcCache(bfc), dir)
    expect_true(file.exists(bfcCache(bfc)))

    # test that sql file also gets created
    expect_true(file.exists(file.path(bfcCache(bfc), "BiocFileCache.sqlite")))
})

test_that("bfcadd and bfcnew works", {
    bfc <- BiocFileCache(tempfile())
    fl <- tempfile(); file.create(fl)

    # test file add and copy
    rid <- bfcadd(bfc, 'test-1', fl)
    expect_identical(length(bfc), 1L)
    expect_true(file.exists(fl))

    # test file add and location not in cache
    path <- bfcadd(bfc, 'test-2', fl, action='asis')
    rid <- as.integer(names(path))
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
    rid <- as.integer(names(path))
    expect_identical(length(bfc), 4L)
    expect_true(file.exists(bfc[[rid]]))

    # test add new (return path to save)
    path <- bfcnew(bfc, 'test-5')
    expect_identical(length(bfc), 5L)
    expect_true(!file.exists(path))
    expect_identical(unname(path),
                     bfc[[as.integer(names(path))]])

    # test out of bounds and file not found
    expect_error(bfc[[7]])
    expect_error(suppressWarnings(bfcadd(bfc, 'test-6',
                        "http://jibberish", rtype="web")))
    expect_error(bfcadd(bfc, 'test-2', fl, action='asis'))
})

#
# construct bfc for further test, avoiding construction in each
#
bfc <- BiocFileCache(tempfile())
fl <- tempfile(); file.create(fl)
add1 <- bfcadd(bfc, 'test-1', fl)
rid1 <- as.integer(names(add1))
add2 <- bfcadd(bfc, 'test-2', fl, action='asis')
rid2 <- as.integer(names(add2))
url <- "http://httpbin.org/get"
add3 <- bfcadd(bfc, 'test-3', url, rtype="web")
rid3 <- as.integer(names(add3))
path <- bfcnew(bfc, 'test-4')
rid4 <- as.integer(names(path))

test_that("bfcinfo works", {
    # print all 
    expect_identical(dim(as.data.frame(bfcinfo(bfc))),
                     c(4L, 8L))
    expect_is(bfcinfo(bfc), "tbl_sqlite")
    # print subset
    expect_identical(dim(as.data.frame(bfcinfo(bfc, 1:3))),
                     c(3L, 8L))
    # print one found and one not found 
    expect_identical(dim(as.data.frame(bfcinfo(bfc, c(1, 6)))),
                     c(1L, 8L))
    # index not found 
    expect_identical(dim(as.data.frame(bfcinfo(bfc, 6))),
                     c(0L, 8L))
})

test_that("bfcpath and bfcrpathworks", {
    # local file
    expect_identical(length(bfcpath(bfc, rid1)), 1L)
    expect_identical(names(bfcpath(bfc, rid1)), "localFile")
    expect_identical(bfcpath(bfc, rid1), bfcrpath(bfc, rid1))
        
    # web file
    expect_identical(length(bfcpath(bfc, rid3)), 2L)
    expect_identical(names(bfcpath(bfc, rid3)), c("localFile", "weblink"))
    expect_identical(bfcpath(bfc, rid3)[1], bfcrpath(bfc,rid3))
        
    # index not found 
    expect_error(bfcpath(bfc, 6))
    expect_error(bfcrpath(bfc, 6))
})

test_that("check rtpye works", {
    # test web types
    expect_identical(BiocFileCache:::.check_rtype("http://somepath.com"), "web")
    expect_identical(BiocFileCache:::.check_rtype("ftp://somepath.com"), "web")

    # test not web type 
    expect_identical(BiocFileCache:::.check_rtype("not/a/web/path"), "local")
})

test_that("bfcupdate works", {   
    # test [[<-, only updates rpath 
    fl2 <- tempfile(); file.create(fl2)
    bfc[[rid2]] <- fl2
    expect_identical(unname(bfcpath(bfc, rid2)), fl2)
    expect_error(bfc[[rid1]] <- "A/file/doesnt/work")

    # test errors, files not found 
    expect_error(bfcupdate(bfc, rid2, weblink="rid2/local/notweb"))
    expect_error(suppressWarnings(bfcupdate(bfc, rid3,
                                            weblink="http://notworking/web")))
    expect_error(bfcupdate(bfc, rid2, rpath="path/not/valid"))
    
    # test update weblink and rname
    link = "https://en.wikipedia.org/wiki/Bioconductor"
    bfcupdate(bfc, rid3, weblink=link, rname="prepQuery")
    vl <- as.character(unname(as.data.frame(
        bfcinfo(bfc,rid3))[c("rname", "weblink")]))
    expect_identical(vl, c("prepQuery", link))
    time <- as.data.frame(bfcinfo(bfc,rid3))$last_modified_time
    expect_identical(time, BiocFileCache:::.get_web_last_modified(link))
    
    # test rpath update and give second query example
    bfcupdate(bfc, rid1, rpath=fl2, rname="prepQuery2")
    expect_identical(unname(bfcpath(bfc, rid1)), fl2)
})

test_that("bfcquery works", {
    # query found
    q1 <- as.data.frame(bfcquery(bfc, "prep"))
    expect_identical(dim(q1), c(2L,8L))
    expect_identical(q1$rid, c(rid1,rid3))

    # test query on weblink
    q2 <- as.data.frame(bfcquery(bfc, "wiki"))
    expect_identical(dim(q2), c(1L,8L))

    # query not found 
    expect_true(is.na(bfcquery(bfc, "nothere")))

    # multiple value all found
    path <- file.path(bfcCache(bfc), "myFile")
    file.create(path)
    bfc[[rid3]] <- path
    q3 <- as.data.frame(bfcquery(bfc, c("prep", "myf")))
    expect_identical(dim(q3), c(1L,8L))
    expect_identical(q3$rid, rid3)

    # multi value some not found
    expect_true(is.na(bfcquery(bfc, c("prep", "not"))))    
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
    bfcupdate(bfc, rid3, weblink=link)
    expect_true(is.na(bfcneedsupdate(bfc, rid3)))
    expect_message(is.na(bfcneedsupdate(bfc, rid3)))
    expect_identical(as.character(Sys.Date()),
        as.data.frame(bfcinfo(bfc,rid3))$last_modified_time)
})

test_that("bfcsync and bfcremove works", {
    # test sync
    expect_message(bfcsync(bfc))
    expect_false(bfcsync(bfc, FALSE))
    bfcremove(bfc, rid4)
    files <- file.path(bfcCache(bfc),
                       setdiff(list.files(bfcCache(bfc)),
                               "BiocFileCache.sqlite")
                       )
    untracked <- setdiff(files, BiocFileCache:::.get_all_rpath(bfc))
    unlink(untracked) 
    expect_true(bfcsync(bfc, FALSE))
    expect_message(bfcsync(bfc))

    # test that remove, deletes file if in cache
    path <- BiocFileCache:::.sql_get_rpath(bfc, rid3)
    expect_true(file.exists(path))
    bfcremove(bfc, rid3)
    expect_false(file.exists(path))
    
    # test remove leaves file if not in cache
    path <- BiocFileCache:::.sql_get_rpath(bfc, rid2)
    expect_true(file.exists(path))
    bfcremove(bfc, rid2)
    expect_true(file.exists(path))
})

test_that("cleanCache works", {
    #cant test functiuon but test helper
    expect_true(length(BiocFileCache:::.sql_clean_cache(bfc, 1)) == 0L)
    
    # manually change access_time so longer than a day
    sql<-
     sprintf("UPDATE resource SET access_time = '2016-01-01' WHERE rid = %d",
              rid1)
    sqlfile <- BiocFileCache:::.sql_get_query(bfc,sql)
    expect_identical(BiocFileCache:::.sql_clean_cache(bfc, 1), rid1)
})

test_that("removeCache works", {
    path <- bfcCache(bfc)
    expect_true(file.exists(path))
    expect_true(removeCache(bfc, ask=FALSE))
    expect_false(file.exists(path))
})

