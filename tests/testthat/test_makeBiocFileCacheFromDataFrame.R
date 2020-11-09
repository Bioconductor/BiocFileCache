context("makeBiocFileCacheFromDataFrame")

test_that("makeBiocFileCacheFromDataFrame works",{

    bfc2 <- BiocFileCache(tempfile(), ask = FALSE)
    fl <- tempfile(); file.create(fl)
    add1 <- bfcadd(bfc2, 'relative', fl)
    add2 <- bfcadd(bfc2, 'local', fl, rtype='local', action='asis')
    url <- "http://httpbin.org/get"
    add3 <- bfcadd(bfc2, 'noDown', url, rtype="web", download=FALSE)
    url  <- "https://www.wikipedia.org/"
    add4 <- bfcadd(bfc2, 'web', url, rtype="web")
    fl <- tempfile(); file.create(fl)
    add5 <- bfcadd(bfc2, 'localnoexist', fl, rtype='local', action='asis')
    file.remove(fl)

    temp = bfcinfo(bfc2)

    # error directory already exists
    expect_error(makeBiocFileCacheFromDataFrame(
        temp, cache=bfccache(bfc2), actionLocal="copy", actionWeb="copy",
        metadataName="resourceMetadata", ask = FALSE
    ), "!dir.exists\\(cache\\) is not TRUE")

    newcache <- file.path(dirname(bfccache(bfc2)), "testNEW")

    # error reserved column names
    expect_error(makeBiocFileCacheFromDataFrame(
        temp, cache=newcache, actionLocal="copy", actionWeb="copy", ask = FALSE
    ), "The following are reserved column names:.*")

    names(temp)[1] = "origID"
    names(temp)[3] = "origTimeC"
    names(temp)[4] = "origTimeA"

    # expect error metadataName missing without default
    expect_error(makeBiocFileCacheFromDataFrame(
        temp, cache=newcache, actionLocal="copy", actionWeb="copy", ask = FALSE
    ), "!missing\\(metadataName\\) is not TRUE")


    # error relative path
    expect_error(makeBiocFileCacheFromDataFrame(
        temp, cache=newcache, actionLocal="copy", actionWeb="copy",
        metadataName="resourceMetadata", ask = FALSE
    ))

    temp= temp[which(temp$rtype != "relative"),]

    # error local file doesn't exist
    expect_error(makeBiocFileCacheFromDataFrame(
        temp,cache=newcache, actionLocal="copy", actionWeb="copy",
        metadataName="resourceMetadata", ask = FALSE
    ))

    temp = temp[-which(temp$origID == names(add5)),]
    temp$rpath = unname(bfcrpath(bfc2,
        rids=as.character(temp$origID)))

    removebfc(bfc2, ask=FALSE)
    newbfc <- makeBiocFileCacheFromDataFrame(
        temp, cache=newcache, actionLocal="copy", actionWeb="copy",
        metadataName="resourceMetadata", ask = FALSE
    )

    expect_identical(length(newbfc), 3L)
    expect_identical(length(.get_all_web_rids(newbfc)), 2L)
    expect_identical(length(.get_nonrelative_ids(newbfc)), 0L)
    # neither web file will be found, only local and sqlite
    expect_identical(length(list.files(bfccache(newbfc))), 3L)
    expect_identical(ncol(bfcinfo(newbfc)), 13L)
    expect_identical(length(bfcmetalist(newbfc)), 1L)
    expect_identical(bfcinfo(newbfc)$origID, temp$origID)
    expect_identical(bfcinfo(newbfc)$etag, temp$etag)
    expect_identical(bfcinfo(newbfc)$fpath, temp$fpath)
    expect_true(all(bfcinfo(newbfc)$rpath !=  temp$rpath))


    removebfc(newbfc, ask=FALSE)
    names(temp)[2] = "origRname"
    names(temp)[8] = "origlmt"
    names(temp)[9] = "origetag"
    newbfc <- makeBiocFileCacheFromDataFrame(
        temp,cache=newcache, actionLocal="copy", actionWeb="copy",
        metadataName="resourceMetadata", ask = FALSE
    )
    expect_identical(ncol(bfcinfo(newbfc)), 16L)

    removebfc(newbfc, ask=FALSE)
    # fail because required not available
    names(temp)[5] = "origrpath"
    expect_error(makeBiocFileCacheFromDataFrame(
        temp, cache=newcache, actionLocal="copy", actionWeb="copy", ask = FALSE
    ))
    names(temp)[5] = "rpath"
    names(temp)[6] = "origrtype"
    expect_error(makeBiocFileCacheFromDataFrame(temp,cache=newcache,
                               actionLocal="copy", actionWeb="copy"))
    names(temp)[6] = "rtype"
    names(temp)[7] = "origfpath"
    expect_error(makeBiocFileCacheFromDataFrame(
        temp, cache=newcache, actionLocal="copy", actionWeb="copy", ask = FALSE
    ))
    names(temp)[7] = "fpath"
    temp <- temp[,c("fpath","rpath","rtype")]
    newbfc <- makeBiocFileCacheFromDataFrame(
        temp, cache=newcache, actionLocal="copy", actionWeb="copy", ask = FALSE
    )
    expect_identical(ncol(bfcinfo(newbfc)), 10L)
    expect_identical(length(bfcmetalist(newbfc)), 0L)
    removebfc(newbfc, ask=FALSE)
})
