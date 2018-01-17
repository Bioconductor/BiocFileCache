context("utility")

test_that("utility works", {
    response <- .util_ask("", .interactive = FALSE)
    expect_identical(response, FALSE)
})
