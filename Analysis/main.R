#
# Main
#

options(warn = 1)

# HELPERS ####

#
# Concept of 'root directory' depends on context.
# - from project root is ./Analysis
# - from ./Reports/dashboard it is, not surprisingly, ./Reports ...
#
proj_root <- function() {
    if (interactive()) {
        getwd()
    } else {
        normalizePath("..")
    }
}

# shortcut for constructing paths
paste_paths <- function(...) {
    normalizePath(gsub("//", "/", paste(..., sep = "/")), mustWork = FALSE)
}

# Setup ####

taskdir <- proj_root()

source(file = paste_paths(taskdir, "Analysis/cdg_91_qof.R"))
source(file = paste_paths(taskdir, "Analysis/calcci.R"))
source(file = paste_paths(taskdir, "Analysis/testci.R"))

# Main ####

main <- function(
    qof_period = "1516"
    , bProcessRaw = FALSE
    , bWriteCSV = FALSE
    , bLoadData = FALSE
) {

    require("dplyr")
    require("data.table")

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
) %>%
        merge(
            fread(input = "
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
        this_file <- paste_paths(taskdir, "./Results", "lu__ccg_groups.csv")

        fwrite(lu.orgs.ccgs.groups, file = this_file)
    }

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

    # return

    invisible(retval)
}

#' Test the main routine
#'
#' Iterate over known instances.
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
