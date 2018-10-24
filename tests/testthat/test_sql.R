context("sql")

test_that("schema versioning works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    expect_identical(.sql_schema_version(bfc), .CURRENT_SCHEMA_VERSION)
    expect_identical(.sql_validate_version(bfc), .CURRENT_SCHEMA_VERSION)

    .sql_migration_update_schema_version(bfc, "0.99.1")
    expect_identical(.sql_schema_version(bfc), "0.99.1")

    .sql_migration_update_schema_version(bfc, "0.0.1")
    expect_error(.sql_validate_version(bfc), "unsupported schema version")
})

test_that(".sql_add_resource() works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    expect_identical(.sql_get_nrows(bfcinfo(bfc)), 0L)

    rpath <- .sql_add_resource(bfc, "foo1", "rtype", "fpath", NA)
    expect_identical(names(rpath), "BFC1" )

    rpath <- .sql_add_resource(bfc, c("foo2", "foo3"), "rtype", "fpath", NA)
    expect_identical(names(rpath), c("BFC2", "BFC3"))

    .sql_remove_resource(bfc, "BFC3")
    rpath <- .sql_add_resource(bfc, "foo4", "rtype", "fpath", NA)
    expect_identical(names(rpath), "BFC4")
})

test_that(".sql_add_resource(ext=.) works", {
    getext <- function(rpath) {
        ext <- tools::file_ext(rpath)
        ext[nzchar(ext)] <- sprintf(".%s", ext)[nzchar(ext)]
        ext
    }

    bfc <- BiocFileCache(tempfile(), ask = FALSE)

    ext <- ""
    rpath <- .sql_add_resource(bfc, "foo1", "rtype", "fpath")
    expect_identical(getext(rpath), ext)

    ext <- ".ext2"
    rpath <- .sql_add_resource(bfc, "foo2", "rtype", "fpath", ext)
    expect_identical(getext(rpath), ext)

    ext <- ".ext3"
    rpath <- .sql_add_resource(bfc, c("foo3", "foo4"), "rtype", "fpath", ext)
    expect_identical(getext(rpath), c(ext, ext))

    ext <- c(".ext5", ".ext6")
    rpath <- .sql_add_resource(bfc, c("foo5", "foo6"), "rtype", "fpath", ext)
    expect_identical(getext(rpath), ext)
})

test_that(".sql_add_resource() sets last_modified_time to NA", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)

    rid <- names(.sql_add_resource(bfc, "foo1", "rtype", "fpath"))
    expected <- setNames(NA_real_, "BFC1")
    expect_identical(.sql_get_last_modified(bfc, rid), expected)
    expected <- setNames(NA_character_, "BFC1")
    expect_identical(.sql_get_etag(bfc, rid), expected)
})

test_that(".sql_remove_resource() works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    expect_identical(.sql_remove_resource(bfc, "BFC1"), 0L)

    rpath <- .sql_add_resource(bfc, paste0("foo", 1:4), "rtype", "fpath", NA)
    expect_identical(.sql_remove_resource(bfc, "BFC1"), 1L)
    expect_identical(.sql_remove_resource(bfc, paste0("BFC", c(2, 4))), 2L)
    expect_identical(bfccount(bfc), 1L)
})

test_that(".sql_get_field() works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)

    rname <- "foo1"
    rid <- names(.sql_add_resource(bfc, rname, "local", "fpath"))
    expect_identical(.sql_get_field(bfc, rid, "rname"), setNames(rname, rid))

    rname <- c("foo2", "foo3")
    rid <- names(.sql_add_resource(bfc, rname, "local", "fpath"))
    expect_identical(.sql_get_field(bfc, rid, "rname"), setNames(rname, rid))
})

test_that(".sql_set_rpath() works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    rid <- names(.sql_add_resource(bfc, paste0("foo", 1:3), "local", "fpath"))

    rpath <- "bar1"
    .sql_set_rpath(bfc, rid[1], rpath)
    expect_identical(.sql_get_rpath(bfc, rid[1]), setNames(rpath, rid[1]))

    rpath <- paste0("bar", 2:3)
    .sql_set_rpath(bfc, rid[2:3], rpath)
    expect_identical(.sql_get_rpath(bfc, rid[2:3]), setNames(rpath, rid[2:3]))
})

test_that(".sql_get_rpath() works", {
    bfc <- BiocFileCache(tempfile(), ask = FALSE)

    fpath <- "fpath"
    rid <- names(.sql_add_resource(bfc, "foo1", "local", fpath))
    .sql_set_rpath(bfc, rid, fpath)
    expect_identical(.sql_get_rpath(bfc, rid), setNames(fpath, rid))

    rid <- names(.sql_add_resource(bfc, "foo2", "relative", fpath))
    .sql_set_rpath(bfc, rid, fpath)
    expected <- setNames(file.path(bfccache(bfc), fpath), rid)
    expect_identical(.sql_get_rpath(bfc, rid), expected)

    rid <- names(.sql_add_resource(bfc, "foo3", "web", fpath))
    .sql_set_rpath(bfc, rid, fpath)
    expected <- setNames(file.path(bfccache(bfc), fpath), rid)
    expect_identical(.sql_get_rpath(bfc, rid), expected)

    rid <- paste0("BFC", 1:3)
    expected <- setNames(c(fpath, expected, expected), rid)
    expect_identical(.sql_get_rpath(bfc, rid), expected)
})

test_that(".sql_add_resource() changes remote special", {

    url =
        "https://s3.amazonaws.com/annotationhub/ncbi/uniprot/3.7/org.'Caballeronia_concitans'.eg.sqlite"
    bfc <- BiocFileCache(tempfile(), ask = FALSE)
    rpath <- path.expand(tempfile("", bfccache(bfc)))
    ext <- ""
    bfname <- basename(url)
    bfname <- curl::curl_escape(bfname)
    rpath <- sprintf("%s_%s%s", rpath, bfname, ext)
    id1 <- bfcadd(bfc, url)
    expect_identical(unname(.sql_get_rname(bfc, names(id1))), url)
    expect_identical(unname(.sql_get_fpath(bfc, names(id1))), url)
    # can't do identical because different random identifier
    expect_true(grepl(bfname, basename(unname(.sql_get_rpath(bfc, names(id1))))))

})
