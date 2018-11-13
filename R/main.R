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

# EXTRACT, TRANSFORM, PROCESS, LOAD ####

#' Process raw datasets
#'
#' Options to specify year. Option to specify if to run the raw process routines
#' Option to specify if to save results to disc as .csv Option to specify just
#' to load the data
#'
#' @param qof_root (character) Of the form "qof-YYZZ"
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
#' v1 <- qof__main(qof_root = "1516", bExtractFromRaw = TRUE)
#' v2 <- qof__main(qof_root = "1516", bExtractFromRaw = FALSE)
#' }
#'
#' @export
#'
qof__main <- function(
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
    if (verbosity.showatlevel("chatty")) {
        lu_ccg_groups %>%
            reshape2::dcast(
                ... ~ ccg_code
                , fun.aggregate = length
                , value.var = "ccg_group_code"
            ) %>%
            print()
    }

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
        v1 <- qof__main(qof_root = "qof-1516", bExtractFromRaw = TRUE)
        v3 <- qof__main(qof_root = "qof-1617", bExtractFromRaw = TRUE)
    }

    if (is.na(bProcessRaw) | !bProcessRaw) {
        v2 <- qof__main(qof_root = "qof-1516", bExtractFromRaw = FALSE)
        v4 <- qof__main(qof_root = "qof-1617", bExtractFromRaw = FALSE)
    }

    invisible(list(v1 = v1, v2 = v2, v3 = v3, v4 = v4))
}

# LOAD ####

#' Load preprocessed data
#'
#' Make package provided data available as one onbect.
#'
#' @inheritParams qof__main
#'
#' @return (list of objects) list(qof = list(meta_org, meta_ind, data_ind,
#'   data_prev), measures, compare)
#'
#' @export
#'
qof__main__load <- function(qof_root = NULL) {
    invisible(list(
        qof = c(
            qof__main__load_reference(qof_root)
            , qof__main__load_data(qof_root)
        )
        , measures = qof__main__load_measures(qof_root)
        , compare = qof__main__load_compare(qof_root)
    ))
}

#' load raw data
#'
#' Default is not to do anything further.
#'
#' @note
#' Call tree:
#'
#' f__extract__load_raw
#' f__main__preprocess
#'
#' @inheritParams f__transform__preprocess
#' @inheritParams f__extract__load_raw
#'
#' @return (list of data.frame objects) reference list with named items
#'   \itemize{\item{data_ind}\item{data_prev}}
#'
#' @family Internal routines
#' @family Load routines
#'
#' @export
#'
qof__main__load_data <- function(
    qof_root = NULL
) {
    .Deprecated("data")

    retval <-list(
        data_ind = qof_data_ind
        , data_prev = qof_data_prev
    )

    if (is.null(qof_root)) {
        retval
    } else {
        retval %>% lapply(function(x){filter(x, qof_period %in% qof_root)})
    }
}

#' Load reference
#'
#' @inheritParams qof__main__load_data
#'
#' @return (list of data.frame objects) reference list with named items
#'   \itemize{\item{meta_org}\item{meta_ind}}
#'
#'
#' @family Internal routines
#' @family Load routines
#' @family Reference routines
#'
#' @export
#'
qof__main__load_reference <- function(
    qof_root = NULL
) {
    .Deprecated("data")

    retval <- list(
        meta_ind = qof_meta_ind
        , meta_org = qof_meta_org
    )

    if (is.null(qof_root)) {
        retval
    } else {
        retval %>% lapply(function(x){filter(x, qof_period %in% qof_root)})
    }
}

#' Load measures
#'
#' @inheritParams qof__main__load_data
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @return (data.frame) measures data frame
#'
#'
#' @family Internal routines
#' @family Load routines
#' @family Measure routines
#'
#' @export
#'
qof__main__load_measures <- function(
    qof_root = NULL
    , file_suffix = "__eng_ccg_prac__measure_ndv"
) {
    .Deprecated("data")

    if (is.null(qof_root))
        qof_measures
    else
        qof_measures %>% filter(qof_period %in% qof_root)
}

#' Load compare
#'
#' @inheritParams qof__main__load_measures
#'
#' @return compare data frame
#'
#'
#' @family Internal routines
#' @family Load routines
#' @family Compare routines
#'
#' @export
#'
qof__main__load_compare <- function(
    qof_root = NULL
    , file_suffix = "__eng_ccg_prac__compare__bench_spc23__eng_ccg"
) {
    .Deprecated("data")

    if (is.null(qof_root))
        qof_compare
    else
        qof_compare %>% filter(qof_period %in% qof_root)
}
