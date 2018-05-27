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
        retval <- f__91__process_all(bWriteCSV, qof_period)
    } else {
        retval <- f__91__load_measures_compare(qof_period)
    }
}
