context("test-extract")


test_that("extract: can extract from pre-parsed", {
    qof_extract <- f__extract(qof_root, bExtractFromRaw = FALSE)

    testthat::expect_is(qof_extract, "list")
})


test_that("transform: can extract from raw", {
    qof_extract <- f__extract__load_raw(qof_root, bSaveData = FALSE)

    testthat::expect_is(qof_extract, "list")
})
