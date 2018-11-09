context("test-pipeline")

# globals

qof_root = c(
    "qof-1718", "qof-1617"
    , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
    , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
    , "qof-0506", "qof-0405"
)[2]

bExtractFromRaw = FALSE
bWriteCSV = FALSE
bSaveData = FALSE
bLoadData = FALSE


test_that("transform: can create local ccg lu", {
    lu_local <- f__transform__create_local_lu()

    testthat::expect_is(lu_local, "list")
})


lu_local <- f__transform__create_local_lu()

lu_ccgs <- lu_local$lu_ccgs
lu_ccg_groups <- lu_local$lu_ccg_groups

# tests


test_that("extract: can extract from pre-parsed", {
    qof_extract <- f__extract(qof_root, bExtractFromRaw = FALSE)

    testthat::expect_is(qof_extract, "list")
})


test_that("transform: can transform from extract", {
    qof_extract <- f__extract(qof_root, bExtractFromRaw = TRUE)

    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    testthat::expect_is(qof_transform, "list")
})


test_that("process: can process measures from transform", {
    qof_extract <- f__extract(qof_root, bExtractFromRaw)

    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    qof_measures_ind <- qof_transform %>%
        f__process__calc_measures_ind(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    testthat::expect_is(qof_measures_ind, "data.frame")


    qof_measures_prev <- qof_transform %>%
        f__process__calc_measures_prev(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    testthat::expect_is(qof_measures_prev, "data.frame")


    qof_measures <- qof_transform %>%
        f__process__measures(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    testthat::expect_is(qof_measures, "data.frame")
})


test_that("process: can compare from measures", {
    qof_extract <- f__extract(qof_root, bExtractFromRaw)

    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    qof_measures <- qof_transform %>%
        f__process__measures(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    qof_compare <- qof_measures %>%
        f__process__compare(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    testthat::expect_is(qof_compare, "list")
})


# multiple roots

qof_roots <- c(
    "qof-1718", "qof-1617"
    , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
    , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
    , "qof-0506", "qof-0405"
)[c(2, 3)]

# find unique values across a number of dataframes
#
# function to take a list of dataframes, extract values from common field
# name, and find unique value
#
l__unique <- function(x, method = c("sapply", "bindrows")) {
    method <- match.arg(method)
    if (method == "bindrows") {
        x %>% bind_rows() %>% .$qof_period %>% unique()
    } else {
        x %>%
            lapply(function(y) {
                y$qof_period %>% unique()}
            ) %>%
            unlist() %>% unique()
    }
}

test_that("pipeline: can handle multiple qof periods", {
    qof_extract <- f__extract(qof_roots, bExtractFromRaw)

    qep <- qof_extract %>% l__unique()

    testthat::expect_equal(sort(qep), sort(qof_roots))


    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    qtp <- qof_transform %>% l__unique()

    testthat::expect_equal(sort(qtp), sort(qof_roots))


    qof_measures <- qof_transform %>%
        f__process__measures(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    testthat::expect_is(qof_measures, "data.frame")


    qof_compare <- qof_measures %>%
        f__process__compare(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    testthat::expect_is(qof_compare, "list")
})
