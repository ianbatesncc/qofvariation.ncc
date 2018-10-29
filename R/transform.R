#
# transform.R
#
# pre-process extracted data
#
# * reshape
# * join meta
#

#' Default local lookups
#'
#' @param bSaveData flag to write use_data sets
#'
#' @return list of lu_ccgs, lu_ccg_groups
#'
f__transform__create_local_lu <- function(
    bSaveData = FALSE
) {
    lu_ccgs <- c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))

    lu_ccg_groups <- data.table::fread(input = "
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
    ) %>% merge(
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

    #lu_ccgs <- lu_ccg_groups$ccg_code %>% unique()

    if (bSaveData) {
        usethis::use_data(lu_ccgs, overwrite = TRUE)
        usethis::use_data(lu_ccg_groups, overwrite = TRUE)

        this_csv <- proj_path("./data-raw", "lu_ccg_groups.csv")
        cat("INFO: saving", this_csv, "...", "\n")
        data.table::fwrite(lu_ccg_groups, file = this_csv)
    }

    return(list(
        lu_ccgs = lu_ccgs
        , lu_ccg_groups = lu_ccg_groups
    ))

}

#' Preprocess
#'
#' - melt data_ind, data_prev onto measure/value pairs
#' - tag on meta_org ccg and indicator_group, domain_code
#' - identify register indicators in meta_ind
#'
#' @param qof list of data items (see \code{\link{f__extract__load_raw}})
#'
#' @return a list with named items
#' \tabular{ll}{
#'   \code{meta_org}  \tab Organisation metadata \cr
#'   \code{meta_ind}  \tab Indicator metadata \cr
#'   \code{data_prev} \tab Prevalence data (molten on measure/value) \cr
#'   \code{data_ind}  \tab Indicator data  (molten on measure/value) \cr
#' }
#'
#' @family Internal routines
#' @family Process routines
#'
f__transform__preprocess <- function(
    qof
) {
    cat("INFO: f__transform__preprocess: processing lookups ...", "\n")

    # meta_org ####

    # drop uneeded columns
    # - that is, keep practice and ccg lookup
    q.meta_org <- qof$meta_org %>%
        select(starts_with("practice"), starts_with("ccg")) %>%
        data.table::setDT()

    # meta_ind ####

    # data_ind ####

    cat("INFO: f__transform__preprocess: processing indicators ...", "\n")

    # ind - qof indicator counts
    # Tag CCG

    # melt onto measure/value fields

    measure_vars <- qof$data_ind[FALSE, ] %>% select_if(is.numeric) %>% names()

    q.data_ind <- qof$data_ind %>%
        melt(
            measure.vars = measure_vars
            , variable.name = "measure", variable.factor = FALSE
        )

    # Find register indicators

    these_inds <- q.data_ind %>%
        filter(measure == "REGISTER", !is.na(value)) %>%
        .$indicator_code %>%
        unique()

    # Add an 'is.register' flag to the indicator map

    q.meta_ind <- qof$meta_ind %>%
        mutate(is.register = (indicator_code %in% these_inds)) %>%
        setDT()

    # Remove register-type indicators

    q.data_ind <- q.data_ind %>%
        filter(!(indicator_code %in% these_inds)) %>%
        # lowercase measure
        # remove points
        # tag ccg, indicator group
        mutate(measure = tolower(measure)) %>%
        filter(!(measure == "achieved_points")) %>%
        setDT() %>%
        # tag ccg
        merge(
            q.meta_org %>% select(practice_code, ccg_code)
            , by = "practice_code"
            , all.x = TRUE
        ) %>%
        # tag non-register indicators with indicator_group_code, domain_code
        merge(
            q.meta_ind %>%
                filter(is.register == FALSE) %>%
                select(indicator_code, indicator_group_code, domain_code)
            , by = "indicator_code"
            , all.x = TRUE, all.y = TRUE
        )

    # data_prev  ####

    cat("INFO: f__transform__preprocess: processing prevalence ...", "\n")

    # prev - qof registers and list sizes

    # tag ccg
    # practice age list sizes, convenience
    # ... add 'indicator code' too , for consistency
    # ... can lose 'patient_list_type' too
    # spin down (register, patient_list_size) on measure

    q.data_prev <- qof$data_prev %>%
        # tag ccg
        merge(
            q.meta_org %>% select(practice_code, ccg_code)
            , by = "practice_code"
            , all.x = TRUE
        ) %>%
        select(-patient_list_type) %>%
        # tag with register indicator code and domain_code
        merge(
            q.meta_ind %>%
                filter(is.register == TRUE) %>%
                select(indicator_group_code, indicator_code, domain_code)
            , by = "indicator_group_code"
            , all.x = FALSE, all.y = TRUE
        )

    q.data_prev_melt <- q.data_prev %>%
        melt(
            measure.vars = c("register", "patient_list_size")
            , variable.name = "measure", variable.factor = FALSE
        )

    # return ####

    return(list(
        meta_org = q.meta_org
        , meta_ind = q.meta_ind
        , data_prev = q.data_prev_melt
        #, data_prev_cast = q.data_prev
        , data_ind = q.data_ind
    ))
}

#' Amend data by adding subtotals
#'
#' Can add england totals, ccg totals, filter for local ccgs, and group by given lookup.
#'
#' @param qof list of lists (see \code{\link{f__extract__load_raw}})
#' @param bCalcEngTotal Add an 'eng' that is total over all practices.
#' @param bCalcCCGTotals Add CCG totals (group practices by ccg_code)
#' @param lu.orgs.ccgs.local Filter on these ccgs (ccg_code)
#' @param lu.orgs.ccgs.groups Groups of ccgs (ccg_code, practice_code -> type, instance)
#'
#' @return list of lists (see \code{\link{f__extract__load_raw}})
#'
#' @family Internal routines
#' @family Process routines
#'
f__91__amend_data__add_subtotals <- function(
    qof
    , bCalcEngTotal = FALSE
    , bCalcCCGTotals = FALSE
    , lu.orgs.ccgs.local = NA
    , lu.orgs.ccgs.groups = NA
) {
    cat("INFO: f__91__amend_data__add_subtotals: amending ...", "\n")

    # eng totals
    l_add_eng <- function(x, bProcess = FALSE) {
        if (bProcess) {
            x$data <- x$data[c("ind", "prev.melt")] %>%
                lapply(function(x) {
                    list(
                        x
                        , x %>%
                            filter(org.type == "ccg::practice") %>%
                            group_by_at(vars(-value, -ccg_code, -practice_code)) %>%
                            summarise_at(vars(value), sum, na.rm = TRUE) %>%
                            ungroup() %>%
                            mutate(ccg_code = "eng", practice_code = "eng", org.type = "england")
                    ) %>% bind_rows()
                })
        }
        invisible(x)
    }

    # filter local ccgs
    l_filter_ccgs <- function(x, lu.orgs.ccgs.local) {
        if (length(lu.orgs.ccgs.local) > 1 | !any(is.na(lu.orgs.ccgs.local))) {
            x$data <- x$data %>%
                lapply(function(x) {
                    list(
                        x %>% filter(org.type != "ccg::practice")
                        , x %>% filter(org.type == "ccg::practice", ccg_code %in% lu.orgs.ccgs.local)
                    ) %>% bind_rows()
                })
        }
        invisible(x)
    }

    # ccg totals
    #'
    #' Consistently use practice_code as instance, ccg_code as type ... even for
    #' CCGs ...
    #'
    l_add_ccgs <- function(x, bProcess = FALSE) {
        if (bProcess) {
            x$data <- x$data[c("ind", "prev.melt")] %>%
                lapply(function(x) {
                    list(
                        x,
                        x %>%
                            filter(org.type == "ccg::practice") %>%
                            group_by_at(vars(-value, -practice_code)) %>%
                            summarise_at(vars(value), sum, na.rm = TRUE) %>%
                            ungroup() %>%
                            mutate(
                                practice_code = ccg_code
                                , ccg_code = "ccg"
                                , org.type = "ccg::instance"
                            )
                    ) %>% bind_rows()
                })
        }
        invisible(x)
    }

    # create ccg groups
    # really need to learn about quosures
    l_add_groups <- function(x, lu.orgs.ccgs.groups) {
        if (length(lu.orgs.ccgs.groups) > 1 | !any(is.na(lu.orgs.ccgs.groups))) {

            l_group <- function(x) {
                list(
                    x
                    , x %>%
                        # choose ccg quantities
                        filter(org.type == "ccg::instance") %>%
                        # tag ccg groups
                        merge(
                            lu.orgs.ccgs.groups %>%
                                rename(practice_code = "ccg_code") %>%
                                select(practice_code, ccg_group_code, ccg_group_type)
                            , all.y = TRUE
                            , by = "practice_code"
                        ) %>%
                        mutate(
                            org.type = paste(ccg_group_type, "instance", sep = "::")
                            , ccg_code = ccg_group_type
                            , practice_code = ccg_group_code
                        ) %>% select(-starts_with("ccg_group")) %>%
                        # summarise over the new ccg groups
                        group_by_at(vars(-value)) %>%
                        summarise_at(vars(value), sum, na.rm = TRUE) %>%
                        ungroup()
                ) %>% bind_rows()
            }
            x$data <- x$data[c("ind", "prev.melt")] %>% lapply(l_group)
        }
        invisible(x)
    }

    qof %>%
        status("INFO: - calculating england total ...") %>%
        l_add_eng(bCalcEngTotal) %>%

        status("INFO: - filtering local ccgs ...") %>%
        l_filter_ccgs(c(lu.orgs.ccgs.local)) %>%

        status("INFO: - calculating ccg totals ...") %>%
        l_add_ccgs(bCalcCCGTotals) %>%

        status("INFO: - calculating ccg groups ...") %>%
        l_add_groups(lu.orgs.ccgs.groups) %>%

        status("INFO: - done.") %>%

        invisible()
}

#' Merge ccg groups into orgref
#'
#' @param qof list of lists (see \code{\link{f__extract__load_raw}})
#' @param lu.orgs.ccgs.groups ccg group lookup (see \code{\link{main}})
#'
#' @return list of lists (see \code{\link{f__extract__load_raw}})
#'
#' @family Internal routines
#' @family Process routines
#'
f__91__amend_orgref__ccg_groups <- function(
    qof
    , lu.orgs.ccgs.groups = NA
){
    # ccg_group_type,ccg_group_name,ccg_code,ccg_group_code,ccg_group_name
    # uop,Unit of Planning,02Q,nno,North Notts. UOP

    qof$reference$orgref <- list(
        qof$reference$orgref
        , lu.orgs.ccgs.groups %>%
            select(-ccg_code, -type_display_order) %>%
            rename(
                practice_code = "ccg_group_code", practice_name = "ccg_group_name"
                , ccg_code = "ccg_group_type", ccg_name = "ccg_group_type_name"
            ) %>%
            mutate(
                ccg_geography_code = ccg_code
                , data_source = qof$reference$orgref$data_source %>% unique()
            ) %>%
            unique()
    ) %>% bind_rows()

    invisible(qof)
}

#' Save reference
#'
#' Save reference data
#'
#' @param qof list of lists (see \code{\link{f__extract__load_raw}})
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#' @param bWriteCSV Flag to indicate to write results to file.
#'
#' @return Not sure, possibly NULL.
#'
#' @family Internal routines
#' @family Save routines
#' @family Reference routines
#'
f__91__save_reference <- function(
    qof
    , qof_root
    , file_suffix = "__processed"
    , bWriteCSV = TRUE
) {
    cat("INFO: f__91__save_reference: saving ...", "\n")

    if (bWriteCSV == TRUE) {
        cat("INFO: f__91__save_reference: saving reference data ...", "\n")

        this.file <- paste0("./data-raw/", qof_root, "_orgref", file_suffix, ".csv")
        fwrite(qof$reference$orgref, file = this.file)

        this.file <- paste0("./data-raw/", qof_root, "_indmap", file_suffix, ".csv")
        fwrite(qof$reference$indmap, file = this.file)
    }
}
