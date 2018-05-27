#
# Main
#

options(warn = 1)

source("./Analysis/cdg_91_qof.R")

main <- function(
    bWriteCSV = FALSE
    , qof_period = "1516"
    , bProcessRaw = FALSE
) {
    if (bProcessRaw == TRUE) {
        retval <- f__91__process__reference_measures_compare(qof_period, bWriteCSV)
    } else {
        retval <- f__91__load__reference_measures_compare(qof_period)
    }
}
