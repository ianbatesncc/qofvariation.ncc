#
# Main
#

options(warn = 1)

source("./Analysis/cdg_91_qof.R")

main <- function(
    qof_period = "1516"
    , bProcessRaw = FALSE
    , bWriteCSV = FALSE
) {
    if (bProcessRaw == TRUE) {
        retval <- f__91__process__reference_measures_compare(qof_period, bWriteCSV = bWriteCSV)
    } else {
        retval <- f__91__load__reference_measures_compare(qof_period)
    }
}

test_main <- function() {
    v1 <- main(qof_period = "1516", bProcessRaw = TRUE, bWriteCSV = TRUE)
    v2 <- main(qof_period = "1516", bProcessRaw = FALSE)

    v3 <- main(qof_period = "1617", bProcessRaw = TRUE, bWriteCSV = TRUE)
    v4 <- main(qof_period = "1617", bProcessRaw = FALSE)

    return(TRUE)
}
