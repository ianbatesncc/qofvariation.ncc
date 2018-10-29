#' Process raw datasets
#'
#' Options to specify year. Option to specify if to run the raw process routines
#' Option to specify if to save results to disc as .csv Option to specify just
#' to load the data
#'
#' @param qof_period Of the form "YYZZ"
#' @param bProcessRaw if FALSE just load.  If TRUE process and then load.
#' @param bWriteCSV Flag to indicate to write results to file.
#' @param bLoadData Specify to load raw numbers too.
#'
#' @note
#'
#' Indirectly can re-create groups lookup (\code{lu__ccgs_groups.csv})
#'
#' Process raw returns measures,reference data and raw data. Otherwise just
#' loads measures and reference data.  To additionally load raw data specify
#' \code{bLoadData = TRUE}
#'
#'
#'
#' @examples
#' \dontrun{
#' v1 <- main(qof_period = "1516", bProcessRaw = TRUE, bWriteCSV = TRUE)
#' v2 <- main(qof_period = "1516", bProcessRaw = FALSE, bLoadData = FALSE)
#' }
#' @export
#'
main <- function(
    qof_period = c("1617", "1516")
    , bProcessRaw = FALSE
    , bWriteCSV = FALSE
    , bLoadData = FALSE
) {
    qof_period <- match.arg(qof_period)

    lu_local <- f__transform__create_local_lu()

    lu_ccgs <- lu_local$lu_ccgs

    lu_ccg_groups <- lu_local$lu_ccg_groups

    # short inspection of lookup
    lu_ccg_groups %>%
        reshape2::dcast(... ~ ccg_code, fun.aggregate = length, value.var = "ccg_group_code") %>%
        print()

    retval <- NULL

    if (!is.na(bProcessRaw)) {
        if (bProcessRaw == TRUE) {
            retval <- f__91__process__reference_measures_compare(
                qof_period, bWriteCSV = bWriteCSV
                , lu_ccgs = lu_ccgs
                , lu_ccg_groups = lu_ccg_groups
            )
        } else {
            retval <- f__91__load__reference_measures_compare(
                qof_period
                , bLoadData
            )
            if (bLoadData == TRUE) {
                retval <- retval %>% f__transform__add_subtotals(
                    bCalcEngTotal = TRUE
                    , bCalcCCGTotals = TRUE
                    , lu_ccgs = lu_ccgs
                    , lu_ccg_groups = lu_ccg_groups
                )
            }
        }
    }

    # return

    invisible(retval)
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
        v1 <- main(qof_period = "1516", bProcessRaw = TRUE, bWriteCSV = TRUE)
        v3 <- main(qof_period = "1617", bProcessRaw = TRUE, bWriteCSV = TRUE)
    }

    if (is.na(bProcessRaw) | !bProcessRaw) {
        v2 <- main(qof_period = "1516", bProcessRaw = FALSE, bLoadData = FALSE)
        v4 <- main(qof_period = "1617", bProcessRaw = FALSE, bLoadData = FALSE)
    }

    invisible(list(v1 = v1, v2 = v2, v3 = v3, v4 = v4))
}
