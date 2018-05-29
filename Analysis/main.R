#
# Main
#

options(warn = 1)

source("./Analysis/cdg_91_qof.R")
source("./Analysis/calcci.R")
source("./Analysis/testci.R")

main <- function(
    qof_period = "1516"
    , bProcessRaw = FALSE
    , bWriteCSV = FALSE
) {

    lu.orgs.ccgs.local <- c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))

    lu.orgs.ccgs.groups <- fread(input = "
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
)
    # short inspection of lookup
    lu.orgs.ccgs.groups %>%
        dcast(... ~ ccg_code, fun = length, value.var = "ccg_group_code") %>%
        print()

    if (bProcessRaw == TRUE) {
        retval <- f__91__process__reference_measures_compare(
            qof_period, bWriteCSV = bWriteCSV
            , lu.orgs.ccgs.local = lu.orgs.ccgs.local
            , lu.orgs.ccgs.groups = lu.orgs.ccgs.groups
        )
    } else {
        retval <- f__91__load__reference_measures_compare(
            qof_period
        )
    }

}

#' Test the main routine
#'
#' Iterate over known instances.
#'
test_main <- function(bProcessRaw = TRUE) {

    if (is.na(bProcessRaw) | bProcessRaw) {
        v1 <- main(qof_period = "1516", bProcessRaw = TRUE, bWriteCSV = TRUE)
        v3 <- main(qof_period = "1617", bProcessRaw = TRUE, bWriteCSV = TRUE)
    }

    if (is.na(bProcessRaw) | !bProcessRaw){
        v2 <- main(qof_period = "1516", bProcessRaw = FALSE)
        v4 <- main(qof_period = "1617", bProcessRaw = FALSE)
    }

    return(TRUE)
}
