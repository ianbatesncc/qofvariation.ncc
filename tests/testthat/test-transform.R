context("test-transform")

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

lu_local <- f__transform__create_local_lu()

lu_ccgs <- lu_local$lu_ccgs
lu_ccg_groups <- lu_local$lu_ccg_groups


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


test_that("transform works", {
    qof_extract <- f__extract(qof_root, bExtractFromRaw)

    # separate steps

    q1 <- qof_extract %>%
        f__transform__preprocess()

    qp1 <- q1 %>% l__unique()

    testthat::expect_equal(qp1, qof_root)


    q2 <- q1 %>%
        f__transform__data__add_orgtype()

    qp2 <- q2 %>% l__unique()

    testthat::expect_equal(qp2, qof_root)


    # subtotals - default (eng and ccgs)

    q3 <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
        )

    qp3 <- q3 %>% l__unique()

    testthat::expect_equal(qp3, qof_root)


    # subtotals - eng only

    q3_eng <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = TRUE
            , bCalcCCGTotals = FALSE
        )

    qp3_eng <- q3_eng %>% l__unique()

    testthat::expect_equal(qp3_eng, qof_root)


    # subtotals - ccgs only

    q3_ccg <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = FALSE
            , bCalcCCGTotals = TRUE
        )

    qp3_ccg <- q3_ccg %>% l__unique()

    testthat::expect_equal(qp3_ccg, qof_root)


    # subtotals - eng and ccgs

    q3_eng_ccg <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = TRUE
            , bCalcCCGTotals = TRUE
        )

    qp3_eng_ccg <- q3_eng_ccg %>% l__unique()

    testthat::expect_equal(qp3_eng_ccg, qof_root)


    # subtotals - no eng and no ccgs

    q3_none <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = FALSE
            , bCalcCCGTotals = FALSE
        )

    qp3_none <- q3_none %>% l__unique()

    testthat::expect_equal(qp3_none, qof_root)


    # ccg_groups

    q4 <- q3 %>%
        f__transform__meta__ccg_groups(lu_ccg_groups = lu_ccg_groups)

    qp4 <- q4 %>% l__unique()

    testthat::expect_equal(qp4, qof_root)


    # altogether

    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    testthat::expect_is(qof_transform, "list")

    qp <- qof_transform %>% l__unique()

    testthat::expect_equal(qp, qof_root)
})


# multiple roots

qof_roots <- c(
    "qof-1718", "qof-1617"
    , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
    , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
    , "qof-0506", "qof-0405"
)[c(2, 3)]


test_that("transform works with mulitple qof periods", {

    qof_extract <- f__extract(qof_roots, bExtractFromRaw)

    # separate steps

    q1 <- qof_extract %>%
        f__transform__preprocess()

    qp1 <- q1 %>% l__unique()

    testthat::expect_equal(sort(qp1), sort(qof_roots))


    q2 <- q1 %>%
        f__transform__data__add_orgtype()

    qp2 <- q2 %>% l__unique()

    testthat::expect_equal(sort(qp2), sort(qof_roots))


    # subtotals - default (eng and ccgs)

    q3 <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = TRUE
            , bCalcCCGTotals = TRUE
        )

    qp3 <- q3 %>% l__unique()

    testthat::expect_equal(sort(qp3), sort(qof_roots))


    # ccg_groups

    q4 <- q3 %>%
        f__transform__meta__ccg_groups(lu_ccg_groups = lu_ccg_groups)

    qp4 <- q4 %>% l__unique()

    testthat::expect_equal(sort(qp4), sort(qof_roots))


    # altogether

    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    testthat::expect_is(qof_transform, "list")


    qp <- qof_transform %>% l__unique()

    testthat::expect_equal(sort(qp), sort(qof_roots))
})
