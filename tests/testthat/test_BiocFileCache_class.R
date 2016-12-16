test_that("BiocFileCache creation works", {
    dir <- tempfile()

    bfc <- BiocFileCache(dir)
    expect_identical(bfcCache(bfc), dir)
    expect_true(file.exists(bfcCache(bfc)))
})

test_that("addResource works", {
    bfc <- BiocFileCache(tempfile())
    fl <- tempfile(); file.create(fl)

    rid <- addResource(bfc, fl, 'test-1')
    expect_identical(length(bfc), 1L)
    expect_true(file.exists(fl))

    rid <- addResource(bfc, fl, 'test-2', 'asis')
    expect_identical(length(bfc), 2L)
    expect_true(file.exists(fl))

    rid <- addResource(bfc, fl, 'test-1', 'move')
    expect_identical(length(bfc), 3L)
    expect_true(!file.exists(fl))
})
