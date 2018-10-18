#
# transform.R
#
# pre-process extracted data
#
# * reshape
# * join meta
#

#' Preprocess
#'
#' optionally save tweaked reference data
#'
#' @param qof list of lists (see \code{\link{f__extract__load_raw}})
#' @param bWriteCSV Flag to indicate to write results to file.
#'
#' @return a list of lists with named items
#' \describe{
#'     \item{reference}{
#'         \itemize{\item{orgref}\item{indmap}}
#'     }
#'     \item{data}{
#'         \itemize{\item{prev}\item{ind}\item{prev.melt}}
#'     }
#' }
#'
#' @family Internal routines
#' @family Process routines
#'
f__91__preprocess <- function(
    qof
    , bWriteCSV = FALSE
) {
    cat("INFO: f__91__preprocess: processing lookups ...", "\n")

    # orgref - organisation lookups ####

    #qof$orgref %>% filter(FALSE) %>% str()
    # Classes ‘data.table’ and 'data.frame':	7619 obs. of  13 variables:
    # $ practice_code           : chr  "B82007" "B82020" "B82028" "B82053" ...
    # $ practice_name           : chr  "TOWNHEAD SURGERY" "CROSSHILLS GROUP PRACTICE" "FISHER MEDICAL CENTRE" "DYNELEY HOUSE SURGERY" ...
    # $ ccg_code                : chr  "02N" "02N" "02N" "02N" ...
    # $ ccg_geography_code      : chr  "E38000001" "E38000001" "E38000001" "E38000001" ...
    # $ ccg_name                : chr  "NHS AIREDALE, WHARFEDALE AND CRAVEN CCG" "NHS AIREDALE, WHARFEDALE AND CRAVEN CCG" "NHS AIREDALE, WHARFEDALE AND CRAVEN CCG" "NHS AIREDALE, WHARFEDALE AND CRAVEN CCG" ...
    # $ subregion_code          : chr  "Q72" "Q72" "Q72" "Q72" ...
    # $ subregion_geography_code: chr  "E39000029" "E39000029" "E39000029" "E39000029" ...
    # $ subregion_name          : chr  "NHS ENGLAND YORKSHIRE AND HUMBER" "NHS ENGLAND YORKSHIRE AND HUMBER" "NHS ENGLAND YORKSHIRE AND HUMBER" "NHS ENGLAND YORKSHIRE AND HUMBER" ...
    # $ region_code             : chr  "Y54" "Y54" "Y54" "Y54" ...
    # $ region_geography_code   : chr  "E40000001" "E40000001" "E40000001" "E40000001" ...
    # $ region_name             : chr  "NORTH OF ENGLAND" "NORTH OF ENGLAND" "NORTH OF ENGLAND" "NORTH OF ENGLAND" ...
    # $ country                 : chr  "ENGLAND" "ENGLAND" "ENGLAND" "ENGLAND" ...
    # $ revised_maximum_points  : int  559 559 559 559 559 559 559 559 559 559 ...

    # drop uneeded columns
    q.orgref <- qof$reference$orgref %>%
        select(starts_with("practice"), starts_with("ccg")) %>%
        data.table::setDT()

    # indmap - qof indicator lookups ####

    #qof$indmap %>% filter(FALSE) %>% str()
    # Classes ‘data.table’ and 'data.frame':	78 obs. of  8 variables:
    # $ indicator_code             : chr  "AF001" "AF006" "AF007" "AST001" ...
    # $ indicator_description      : chr  "The contractor establishes and maintains a register of patients with atrial fibrillation" "The percentage of patients with atrial fibrillation in whom stroke risk has been assessed using the CHA2DS2-VASc score risk str"| __truncated__ "In those patients with atrial fibrillation with a record of a CHA2DS2-VASc score of 2 or more, the percentage of patients who a"| __truncated__ "The contractor establishes and maintains a register of patients with asthma, excluding patients with asthma who have been presc"| __truncated__ ...
    # $ indicator_point_value      : int  5 12 12 4 15 20 6 15 5 6 ...
    # $ indicator_group_code       : chr  "AF" "AF" "AF" "AST" ...
    # $ indicator_group_description: chr  "Atrial fibrillation" "Atrial fibrillation" "Atrial fibrillation" "Asthma" ...
    # $ domain_code                : chr  "CL" "CL" "CL" "CL" ...
    # $ domain_description         : chr  "Clinical" "Clinical" "Clinical" "Clinical" ...
    # $ patient_list_type          : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...

    # process indicators ####

    cat("INFO: f__91__preprocess: processing indicators ...", "\n")

    # ind - qof indicator counts
    # Tag CCG
    # Find register indicators

    #qof$ind %>% filter(FALSE) %>% str()
    # Classes ‘data.table’ and 'data.frame':	1956549 obs. of  4 variables:
    # $ practice_code : chr  "A81001" "A81001" "A81001" "A81001" ...
    # $ indicator_code: chr  "AF001" "AF001" "AF006" "AF006" ...
    # $ measure       : chr  "ACHIEVED_POINTS" "REGISTER" "ACHIEVED_POINTS" "DENOMINATOR" ...
    # $ value         : num  5 96 12 57 2 53 12 76 4 71 ...

    ## Remove register-type indicators
    # add an 'is.register' flag to the indicator map
    q.indmap <- qof$reference$indmap %>%
        mutate(is.register = (indicator_code %in% (
            qof$data$ind %>%
                filter(measure == "REGISTER") %>%
                .$indicator_code %>%
                unique()
        ))) %>%
        data.table::setDT()

    # filter out non-register indicators via join with indmap
    q.ind <- qof$data$ind %>%
        filter(!(indicator_code %in% (
            q.indmap %>% filter(is.register == TRUE) %>% .$indicator_code))
        ) %>%
        # lowercase measure
        # remove points
        # tag ccg, indicator group
        mutate(measure = tolower(measure)) %>%
        filter(!(measure == tolower("ACHIEVED_POINTS"))) %>%
        data.table::setDT() %>%
        # tag ccg
        merge(q.orgref %>% select(practice_code, ccg_code)
              , by = "practice_code") %>%
        # filter non-register AND tag indicator_group_code, domain_code
        merge(
            q.indmap %>%
                filter(is.register == FALSE) %>%
                select(domain_code, indicator_code, indicator_group_code)
            , by = "indicator_code"
        )


    # process prevalence  ####

    cat("INFO: f__91__preprocess: processing prevalence ...", "\n")

    # prev - qof registers and list sizes
    # Tag CCG

    #qof$prev %>% filter(FALSE) %>% str()
    # Classes ‘data.table’ and 'data.frame':	159999 obs. of  5 variables:
    # $ practice_code       : chr  "A81001" "A81001" "A81001" "A81001" ...
    # $ indicator_group_code: chr  "AF" "AST" "CAN" "CHD" ...
    # $ register            : int  96 325 107 174 201 117 18 51 296 259 ...
    # $ patient_list_type   : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...
    # $ patient_list_size   : int  4247 4247 4247 4247 3408 4247 2291 4247 3408 3451 ...

    # tag ccg
    # practice age list sizes, convenience
    # ... add 'indicator code' too , for consistency
    # ... can lose 'patient_list_type' too
    # spin down (register, patient_list_size) on measure

    q.prev <- qof$data$prev %>%
        merge(q.orgref %>% select(practice_code, ccg_code)
              , by = "practice_code") %>%
        #mutate(org.type = "ccg, practice") %>%
        select(-patient_list_type) %>%
        # filter non-register AND tag indicator_code, domain_code
        merge(q.indmap %>%
                  filter(is.register == TRUE) %>%
                  select(domain_code, indicator_group_code, indicator_code)
              , by = "indicator_group_code"
        )

    # q.prev.praclists <- q.prev %>%
    #     select(practice_code, patient_list_type, patient_list_size) %>%
    #     unique()
    # q.prev.praclists.tbl <- q.prev.praclists %>%
    #     dcast(... ~ patient_list_type, sum, value.var = "patient_list_size")

    q.prev.melt <- q.prev %>%
        melt(measure.vars = c("register", "patient_list_size")
             , variable.name = "measure", variable.factor = FALSE
             , value.name = "value")

    # return

    return(list(
        reference = list(
            orgref = q.orgref
            , indmap = q.indmap
        )
        , data = list(
            ind = q.ind
            , prev = q.prev
            , prev.melt = q.prev.melt
        )
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
