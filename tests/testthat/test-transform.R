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


test_that("transform works", {
    qof_extract <- f__extract(qof_root, bExtractFromRaw)

    # separate steps

    q1 <- qof_extract %>%
        f__transform__preprocess()

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
                    y$qof_period %>% unique()
                }) %>%
                unlist() %>% unique()
        }
    }

    qp1 <- q1 %>% l__unique()

    testthat::expect_true(!any(is.na(qp1), qp1 == "NA"))


    q2 <- q1 %>%
        f__transform__data__add_orgtype()

    qp2 <- q2 %>% l__unique()

    testthat::expect_true(!any(is.na(qp2), qp2 == "NA"))

    # subtotals - default (eng and ccgs)

    q3 <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
        )

    qp3 <- q3 %>% l__unique()

    testthat::expect_true(!any(is.na(qp3), qp3 == "NA"))

    # subtotals - eng only
    # fails: ? no CCG ..

    q3_eng <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = TRUE
            , bCalcCCGTotals = FALSE
        )

    qp3_eng <- q3_eng %>% l__unique()

    testthat::expect_true(!any(is.na(qp3_eng), qp3_eng == "NA"))

    # subtotals - ccgs only
    # passes: with CCG ..

    q3_ccg <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = FALSE
            , bCalcCCGTotals = TRUE
        )

    qp3_ccg <- q3_ccg %>% l__unique()

    testthat::expect_true(!any(is.na(qp3_ccg), qp3_ccg == "NA"))

    # subtotals - eng and ccgs
    # passes: with CCG ..

    q3_eng_ccg <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = TRUE
            , bCalcCCGTotals = TRUE
        )

    qp3_eng_ccg <- q3_eng_ccg %>% l__unique()

    testthat::expect_true(!any(is.na(qp3_eng_ccg), qp3_eng_ccg == "NA"))

    # subtotals - no eng and no ccgs
    # fails: ? no CCG ..

    q3_none <- q2 %>%
        f__transform__data__add_subtotals(
            lu_ccgs = lu_ccgs, lu_ccg_groups = lu_ccg_groups
            , bCalcEngTotal = FALSE
            , bCalcCCGTotals = FALSE
        )

    qp3_none <- q3_none %>% l__unique()

    testthat::expect_true(!any(is.na(qp3_none), qp3_none == "NA"))

    # ccg_groups

    q4 <- q3 %>%
        f__transform__meta__ccg_groups(lu_ccg_groups = lu_ccg_groups)

    qp4 <- q4 %>% l__unique()

    testthat::expect_true(!any(is.na(qp4), qp4 == "NA"))


    # altogether

    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    testthat::expect_is(qof_transform, "list")

    qp <- qof_transform %>% l__unique()

    testthat::expect_true(!any(is.na(qp), qp == "NA"))
})
