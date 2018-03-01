context("httr")

test_that("internal .httr_get_cache_info works", {

    # example neither
    url <- "http://httpbin.org/get"
    info <- .httr_get_cache_info(url)
    expect_true(all(is.na(info)))
    expect_identical(length(info), 3L)
    expect_identical(names(info), c("etag", "modified", "expires"))

    # example both
    url <- "https://www.wikipedia.org/"
    info <- .httr_get_cache_info(url)
    expect_true(all(!is.na(info)))
    expect_identical(length(info), 3L)
    expect_identical(names(info), c("etag", "modified", "expires"))

    # example only time
    url <- "https://en.wikipedia.org/wiki/Bioconductor"
    info <- .httr_get_cache_info(url)
    expect_true(is.na(info[["etag"]]))
    expect_true(!is.na(info[["modified"]]))
    expect_true(!is.na(info[["expires"]]))
    expect_identical(length(info), 3L)
    expect_identical(names(info), c("etag", "modified", "expires"))
    # more tests in bfcneedsupdate

})
