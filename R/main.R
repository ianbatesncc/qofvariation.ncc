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

    lu.orgs.ccgs.local <- c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))

    lu.orgs.ccgs.groups <- data.table::fread(input = "
ccg_group_type,ccg_group_type_name,ccg_code,ccg_group_code,ccg_group_name
uop,Unit of Planning,02Q,nno,North Notts. UOP
uop,Unit of Planning,04E,mno,Mid. Notts. UOP
uop,Unit of Planning,04H,mno,Mid. Notts. UOP
uop,Unit of Planning,04L,sno,South Notts. UOP
uop,Unit of Planning,04M,sno,South Notts. UOP
uop,Unit of Planning,04N,sno,South Notts. UOP
uop,Unit of Planning,04K,sno,South Notts. UOP
stp,Sus. and Trans. P-ship,04E,not,Nottinghamshire STP
stp,Sus. and Trans. P-ship,04H,not,Nottinghamshire STP
stp,Sus. and Trans. P-ship,04L,not,Nottinghamshire STP
stp,Sus. and Trans. P-ship,04M,not,Nottinghamshire STP
stp,Sus. and Trans. P-ship,04N,not,Nottinghamshire STP
stp,Sus. and Trans. P-ship,04K,not,Nottinghamshire STP
utla,Upper Tier LA,02Q,ncc,Nottinghamshire CC
utla,Upper Tier LA,04E,ncc,Nottinghamshire CC
utla,Upper Tier LA,04H,ncc,Nottinghamshire CC
utla,Upper Tier LA,04L,ncc,Nottinghamshire CC
utla,Upper Tier LA,04M,ncc,Nottinghamshire CC
utla,Upper Tier LA,04N,ncc,Nottinghamshire CC
utla,Upper Tier LA,04K,nci,Nottingham City UA
lep,Local Enterprise P-ship,02Q,n2,Nottingham and Nottinghamshire LEP N2
lep,Local Enterprise P-ship,04E,n2,Nottingham and Nottinghamshire LEP N2
lep,Local Enterprise P-ship,04H,n2,Nottingham and Nottinghamshire LEP N2
lep,Local Enterprise P-ship,04L,n2,Nottingham and Nottinghamshire LEP N2
lep,Local Enterprise P-ship,04M,n2,Nottingham and Nottinghamshire LEP N2
lep,Local Enterprise P-ship,04N,n2,Nottingham and Nottinghamshire LEP N2
lep,Local Enterprise P-ship,04K,n2,Nottingham and Nottinghamshire LEP N2
"
    ) %>%
        merge(
            data.table::fread(input = "
ccg_group_type,type_display_order
lep,1
utla,2
stp,3
uop,4
")
            , by = "ccg_group_type"
            , all.x = TRUE
        )

    if (bWriteCSV) {
        this_csv <- proj_path("./data-raw", "lu__ccg_groups.csv")

        cat("INFO: saving", this_file, "...", "\n")

        data.table::fwrite(lu.orgs.ccgs.groups, file = this_csv)
    }

    # short inspection of lookup
    lu.orgs.ccgs.groups %>%
        reshape2::dcast(... ~ ccg_code, fun.aggregate = length, value.var = "ccg_group_code") %>%
        print()

    retval <- NULL

    if (!is.na(bProcessRaw)) {
        if (bProcessRaw == TRUE) {
            retval <- f__91__process__reference_measures_compare(
                qof_period, bWriteCSV = bWriteCSV
                , lu.orgs.ccgs.local = lu.orgs.ccgs.local
                , lu.orgs.ccgs.groups = lu.orgs.ccgs.groups
            )
        } else {
            retval <- f__91__load__reference_measures_compare(
                qof_period
                , bLoadData
            )
            if (bLoadData == TRUE) {
                retval <- retval %>% f__91__amend_data__add_subtotals(
                    bCalcEngTotal = TRUE
                    , bCalcCCGTotals = TRUE
                    , lu.orgs.ccgs.local = lu.orgs.ccgs.local
                    , lu.orgs.ccgs.groups = lu.orgs.ccgs.groups
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
