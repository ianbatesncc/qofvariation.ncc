#
# development
#

if (FALSE) {
    require("devtools")
    require("testthat")
    require("dplyr")
    require("data.table")
    require("readxl")

    source("./R/qofvariation.ncc-package.R")
    source("./R/utils.R")
    source("./R/extract.R")
    source("./R/transform.R")
    source("./R/process.R")
    source("./R/calcci.R")
    source("./R/testci.R")
}


#' Process raw datasets
#'
#' Options to specify year. Option to specify if to run the raw process routines
#' Option to specify if to save results to disc as .csv Option to specify just
#' to load the data
#'
#' @param qof_period (character) Of the form "YYZZ"
#' @param bExtractFromRaw (bool) if FALSE just load.  If TRUE process and then
#'   load. Default FALSE.
#' @param bWriteCSV (bool) Flag to indicate to write results to file.  Default
#'   FALSE.
#' @param bSaveData (bool) Specify to save measures and compare phases.  Default
#'   FALSE.
#' @param bLoadData (bool) Specify to load raw numbers too.  Default FALSE.
#'
#' @note
#'
#' Indirectly can re-create groups lookup (\code{lu__ccgs_groups.csv})
#'
#' Process raw returns measures,reference data and raw data. Otherwise just
#' loads measures and reference data.  To additionally load raw data specify
#' \code{bLoadData = TRUE}
#'
#' @examples
#' \dontrun{
#' v1 <- main(qof_root = "1516", bExtractFromRaw = TRUE)
#' v2 <- main(qof_root = "1516", bExtractFromRaw = FALSE)
#' }
#' @export
#'
main <- function(
    qof_root = c(
        "qof-1718", "qof-1617"
        , "qof-1516", "qof-1415", "qof-1314", "qof-1213", "qof-1112"
        , "qof-1011", "qof-0910", "qof-0809", "qof-0708", "qof-0607"
        , "qof-0506", "qof-0405"
    )
    , bExtractFromRaw = FALSE
    , bWriteCSV = FALSE
    , bSaveData = FALSE
    , bLoadData = FALSE
) {
    qof_root <- match.arg(qof_root, several.ok = TRUE)

    lu_local <- f__transform__create_local_lu()

    lu_ccgs <- lu_local$lu_ccgs
    lu_ccg_groups <- lu_local$lu_ccg_groups

    # short inspection of lookup
    lu_ccg_groups %>%
        reshape2::dcast(
            ... ~ ccg_code
            , fun.aggregate = length
            , value.var = "ccg_group_code"
        ) %>%
        print()

    retval <- NULL

    qof_extract <- f__extract(qof_root, bExtractFromRaw)

    qof_transform <- qof_extract %>%
        f__transform(lu_ccgs, lu_ccg_groups)

    qof_measures <- qof_transform %>%
        f__process__measures(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    qof_compare <- qof_measures %>%
        f__process__compare(bWriteCSV = bWriteCSV, bSaveData = bSaveData)

    # return

    invisible(list(
        qof = qof
        , measures = qof_measures
        , compare = qof_compare
    ))
}

#' Test the main routine
#'
#' Iterate over known instances.
#' goes through 1617 and 1516 both processing raw and loading processed dataset.
#'
#' @param bProcessRaw if FALSE just load.  If TRUE process and then load.
#'
test_main <- function(bProcessRaw = TRUE) {

    v1 <- NA
    v2 <- NA
    v3 <- NA
    v4 <- NA

    if (is.na(bProcessRaw) | bProcessRaw) {
        v1 <- main(qof_root = "qof-1516", bExtractFromRaw = TRUE)
        v3 <- main(qof_root = "qof-1617", bExtractFromRaw = TRUE)
    }

    if (is.na(bProcessRaw) | !bProcessRaw) {
        v2 <- main(qof_root = "qof-1516", bExtractFromRaw = FALSE)
        v4 <- main(qof_root = "qof-1617", bExtractFromRaw = FALSE)
    }

    invisible(list(v1 = v1, v2 = v2, v3 = v3, v4 = v4))
}
