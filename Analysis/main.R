#
# Main
#

options(warn = 1)

source("./Analysis/cdg_91_qof.R")

main <- function(
    bWriteCSV = FALSE
    , qof_period = "1516"
) {
    retval <- f__91(bWriteCSV, qof_period)
}
