#
# process.R
#
# Take data from extract and produce measures and comapre them.
#

#' Process all
#'
#' Load raw data and produce measures and compare against england.
#'
#' @note
#' Call tree:
#'
#' f__91__load_data
#' f__91__amend_data__add_subtotals
#' f__91__amend_orgref__ccg_groups
#' f__91__save_reference
#' f__91__measures
#' f__91__compare
#'
#' @param qof (list of data.frames) result of \code{f__transform}
#' @param bWriteCSV Flag to indicate to write results to file.
#'
#' @return a list with named items
#' \describe{
#'     \item{qof (list)}{A list of data_ind, data_prev, meta_ind, meta_prev tables}
#'     \item{measures (data.frame)}{Measures}
#'     \item{compare (dtat.frame)}{Comparisons}
#' }
#'
#' @family External routines
#' @family Process routines
#'
#' @seealso f__transform
#'
#' @export
#'
f__process__reference_measures_compare <- function(
    qof
    , bWriteCSV = FALSE
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__process__reference_measures_compare: processing ...", "\n")

    if (verbosity.showatlevel("chatty"))
        cat("INFO: bWriteCSV =", bWriteCSV, "\n")

    # measures and grouping

    qof_measures <- qof %>%
        f__process__measures(bWriteCSV = bWriteCSV)

    # compare

    qof_compare <- qof_measures %>%
        f__process__compare(bWriteCSV = bWriteCSV)

    # return

    return(list(
        qof = qof
        , measures = qof_measures
        , compare = qof_compare
    ))
}

#' Load processed
#'
#' Load pre-processed measures.  Optionally load the raw numbers behind the
#' measures.
#'
#' @note Call tree:
#'
#' f__91__load_reference
#' f__91__load_measures
#' f__91__load_compare
#' f__91__load_data
#'
#' @param qof_period of the form "ZZYY"
#' @param bLoadData Specify to load raw numbers too.
#'
#' @return a list with named items
#' \describe{
#'     \item{data}{Raw numbers}
#'     \item{reference}{Reference tables}
#'     \item{measures}{Measures}
#'     \item{compare}{Comparisons}
#' }
#'
#' @family External routines
#' @family Load routines
#'
#' @export
#'
f__91__load__reference_measures_compare <- function(
    qof_period = c("1617", "1516")
    , bLoadData = FALSE
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__91__load__reference_measures_compare: loading ...", "\n")

    qof_period <- match.arg(qof.period)

    qof_root <- paste("qof", qof_period, sep = "-")

    # Localisation

    qof_reference <- f__91__load_reference(qof_root)
    qof_measures <- f__91__load_measures(qof_root)
    qof_compare <- f__91__load_compare(qof_root)

    lu.orgs.ccgs.local <- qof_measures$ccg_code %>% unique()

    if (bLoadData == TRUE) {
        qof_data <- f__91__load_data(qof_root)$data
    } else {
        qof_data <- list(ind = data.frame(), prev.melt = data.frame())
    }

    # return

    return(list(
        data = qof_data
        , reference = qof_reference
        , measures = qof_measures
        , compare = qof_compare
    ))
}

# MEASURES ####

#' Calculate QOF measures
#'
#' Wrapper around measures_ind and measures_prev.
#'
#' Separate at this stage as ind and prev although a  lot of similarity have
#' different underlying data structure.
#'
#' @note
#' Call tree:
#'
#' f__91__measures_ind
#' f__91__measures_prev
#'
#' @param qof list of lists (see \code{\link{f__extract__load_raw}})
#' @param bWriteCSV Flag to indicate to write results to file.
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @family Internal routines
#' @family Process routines
#' @family Measure routines
#'
f__process__measures <- function(
    qof
    , bWriteCSV = FALSE
    , file_suffix = "__eng_ccg_prac__measure_ndv"
    , bSaveData = TRUE
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__process__measures: processing ...", "\n")

    m.ind <- f__process__calc_measures_ind(
        qof
        , bWriteCSV = bWriteCSV
        , file_suffix
        , bSaveData = bSaveData
    ) %>% data.table::setDT()

    m.prev <- f__process__calc_measures_prev(
        qof
        , bWriteCSV = bWriteCSV
        , file_suffix
        , bSaveData = bSaveData
    )

    # some practices with a zero register will have zeros for indicators
    # : : : (num, den, value) = (0, 0, NA)
    # also some achievement denominators may be zero (after exceptions)
    # : : : (num, den, value) = (0, 0, NA)
    # : : : NA and NaN behave as is.na() == TRUE ... leaving as is

    # combine

    m.comb <- list(m.ind, m.prev) %>% bind_rows()

    # Save

    if (bWriteCSV) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__measures: saving m.comb ...", "\n")

        for (qof_root in m.comb$qof_period %>% unique()) {
            this_csv <- paste0("./data-raw/", qof_root, "_all", file_suffix, ".csv")

            if (verbosity.showatlevel("chatty"))
                cat("INFO: f__process__measures: saving", this_csv, "...", "\n")

            fwrite(m.comb %>% filter(qof_period == qof_root), file = this_csv)
        }

    } else {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__measures: NOT saving m.comb ...", "\n")
    }

    if (bSaveData) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__measures: storing qof_measures_combined ...", "\n")

        store_data(m.comb, "qof_measures")
    }

    invisible(m.comb)
}

# * Indicators ####

#' Create QOF performance measures
#'
#' @param qof list of lists (see \code{\link{f__extract__load_raw}})
#' @param bWriteCSV Flag to indicate to write results to file.
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @return performance data frame
#'
#' @family Internal routines
#' @family Process routines
#' @family Measure routines
#'
f__process__calc_measures_ind <- function(
    qof
    , bWriteCSV = FALSE
    , file_suffix = "__eng_ccg_prac__measure_ndv"
    , bSaveData = TRUE
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__process__calc_measures_ind: processing indicator measures ...", "\n")

    #
    # To do: practice level prevalence, achievement / treatment
    # - and local CCGs
    # - and England
    #
    # qofprev: (reg, agelist, value)
    #
    # achievem: (num, den, value)
    # treat: (num, den + except, value)
    # except: (ex, den + except, value)
    #
    # sub: (den - num, den + except)
    # suborexcpet: (den - num + except, den + except)
    #
    # (practice_code, group, indicator, measure, num, den, value)

    q.ind.combined <- qof$data_ind

    # Calculate measures

    lu_measures <- fread(strip.white = TRUE, input = "
m.type,      m.name,        m.stat,      i.num, i.den, i.exc
performance, achievement,   numerator,   1,     0,     0
performance, achievement,   denominator, 0,     1,     0
performance, treatment,     numerator,   1,     0,     0
performance, treatment,     denominator, 0,     1,     1
performance, exceptions,    numerator,   0,     0,     1
performance, exceptions,    denominator, 0,     1,     1
performance, suboptimal_no_except,    numerator,  -1,     1,     0
performance, suboptimal_no_except,    denominator, 0,     1,     1
performance, suboptimal,    numerator,  -1,     1,     1
performance, suboptimal,    denominator, 0,     1,     1
")

    q.ind.measures <- q.ind.combined %>%
        filter(!is.na(value)) %>%
        # spin up numerator, denominator, exceptions
        dcast(... ~ measure, fun.aggregate = sum, value.var = "value") %>%
        # cross join measures and combine and remove intermediate columns
        merge(lu_measures %>% filter(m.type == "performance")) %>%
        mutate(value = (i.num * numerator + i.den * denominator + i.exc * exceptions)) %>%
        select(-matches("num|den|exc")) %>%
        # spin up m.numerator, m.denominator, ensure double and calculate m.value
        dcast(... ~ m.stat, fun.aggregate = sum, value.var = "value") %>%
        mutate(value = 100.0 * numerator / denominator) %>%
        # melt down numerator, denominator and value on m.stat
        melt(
            measure.vars = c("numerator", "denominator", "value")
            , variable.name = "m.stat", variable.factor = FALSE
            , value.name = "value"
        )

    # Save

    if (bWriteCSV) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__calc_measures_ind: saving q.ind.measures ...", "\n")

        for (qof_root in q.ind.measures$qof_period %>% unique()) {
            this_csv <- paste0("./data-raw/", qof_root, "_ind", file_suffix, ".csv")

            if (verbosity.showatlevel("chatty"))
                cat("INFO: f__process__measures: saving", this_csv, "...", "\n")

            fwrite(q.ind.measures %>% filter(qof_period == qof_root), file = this_csv)
        }

    } else {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__calc_measures_ind: NOT saving q.ind.measures ...", "\n")
    }

    if (bSaveData) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__calc_measures_ind: storing qof_measures_ind ...", "\n")

        store_data(q.ind.measures, "qof_measures_ind")
    }

    #q.ind.measures %>% filter(FALSE) %>% str()
    # Classes ‘data.table’ and 'data.frame':	51714 obs. of  9 variables:
    #  $ indicator_group_code: chr  "AF" "AF" "AF" "AF" ...
    #  $ indicator_code      : chr  "AF006" "AF006" "AF006" "AF006" ...
    #  $ ccg_code            : chr  "02Q" "02Q" "02Q" "02Q" ...
    #  $ practice_code       : chr  "C84001" "C84001" "C84008" "C84008" ...
    #  $ org.type            : chr  "ccg, practice" "ccg, practice" "ccg, practice" "ccg, practice" ...
    #  $ m.type              : chr  "performance" "performance" "performance" "performance" ...
    #  $ m.name              : chr  "achievement" "treatment" "achievement" "treatment" ...
    #  $ m.stat              : chr  "numerator" "numerator" "numerator" "numerator" ...
    #  $ value               : num  389 389 104 104 171 171 316 316 204 204 ...

    # return

    return(q.ind.measures)
}

# * Prevalence ####

#' Create QOF prevalance measures
#'
#' @param qof list of lists (see \code{\link{f__extract__load_raw}})
#' @param bWriteCSV Flag to indicate to write results to file.
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @return prevalence data frame
#'
#' @family Internal routines
#' @family Process routines
#' @family Measure routines
#'
f__process__calc_measures_prev <- function(
    qof
    , bWriteCSV = FALSE
    , file_suffix = "__eng_ccg_prac__measure_ndv"
    , bSaveData = TRUE
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__process__calc_measures_prev: processing prevalence measures ...", "\n")

    # England, CCG, CDG, Practice level

    #str(qof.prev.melt)
    # Classes ‘data.table’ and 'data.frame':	319998 obs. of  6 variables:
    #  $ practice_code       : chr  "B82007" "B82007" "B82007" "B82007" ...
    #  $ indicator_group_code: chr  "AF" "AST" "CAN" "CHD" ...
    #  $ patient_list_type   : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...
    #  $ ccg_code            : chr  "02N" "02N" "02N" "02N" ...
    #  $ measure             : chr  "register" "register" "register" "register" ...
    #  $ value               : int  238 783 290 399 485 257 71 122 710 495 ...

    qof.prev.combined <- qof$data_prev

    # Calculate measures

    lu_measures <- fread(strip.white = TRUE, input = "
m.type,      m.name,        m.stat,      i.num, i.den, i.exc
prevalence,  qofprevalence, numerator,   1,     0,     NA
prevalence,  qofprevalence, denominator, 0,     1,     NA
")

    q.prev.measures <- qof.prev.combined %>%
        filter(!is.na(value)) %>%
        # spin up numerator, denominator, exceptions
        dcast(... ~ measure, fun.aggregate = sum, value.var = "value") %>%
        # cross join measures and combine and remove intermediate columns
        merge(lu_measures %>% filter(m.type == "prevalence")) %>%
        mutate(value = (i.num * register + i.den * patient_list_size)) %>%
        select(-matches("num|den|exc|register|list_size")) %>%
        # spin up m.numerator, m.denominator, ensure double and calculate m.value
        dcast(... ~ m.stat, fun.aggregate = sum, value.var = "value") %>%
        mutate_at(c("numerator", "denominator"), as.double) %>%
        mutate(value = 100 * numerator / denominator) %>%
        # melt down numerator, denominator and value on m.stat
        melt(
            measure.vars = c("numerator", "denominator", "value")
             , variable.name = "m.stat", variable.factor = FALSE
             , value.name = "value"
        )

    # Save

    if (bWriteCSV) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__calc_measures_prev: saving q.prev.measures ...", "\n")

        for (qof_root in q.prev.measures$qof_period %>% unique()) {
            this_csv <- paste0("./data-raw/", qof_root, "_prev", file_suffix, ".csv")

            if (verbosity.showatlevel("chatty"))
                cat("INFO: f__process__measures: saving", this_csv, "...", "\n")

            fwrite(q.prev.measures %>% filter(qof_period == qof_root), file = this_csv)
        }

    } else {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__calc_measures_prev: NOT saving q.prev.measures ...", "\n")
    }

    if (bSaveData) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__calc_measures_ind: storing qof_measures_prev ...", "\n")

        store_data(q.prev.measures, "qof_measures_prev")
    }

    # return

    return(q.prev.measures)
}

# COMPARE - Add England comparator and significance test ####

#' Compare routines
#'
#' Benchmark against reference value e.g. England - CI overlap with reference
#' SPC methods - compare point value with control limits
#'
#' @param qof_measures measures data frame
#' @param bWriteCSV Flag to indicate to write results to file.
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @return compare data frame
#'
#'
#' @family Internal routines
#' @family Compare routines
#'
f__process__compare <- function(
    qof_measures
    , bWriteCSV = FALSE
    , file_suffix = "__eng_ccg_prac__compare__bench_spc23__eng_ccg"
    , bSaveData = TRUE
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__process__compare: processing statistical significance comparison ...", "\n")

    ##
    ## QOF
    ##
    # Tag prevalence at practice level with England
    # Tag achievement at CDG level with England
    # [optional] tag Treatment at CDG level with England.
    #

    # Measures ####

    # Melted on statistic.  Extract England, spin both up on m.stat, tag England,
    # do stat. compare, remove uneeded columns, spin back down

    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__process__compare: creating reference lookups ...", "\n")

    #qof_measures$prev$org.type %>% unique() %>% print()
    # [1] "ccg::practice" "ccg::instance" "england"

    # * All that is not England ####

    q.var.cast <- qof_measures %>%
        filter(org.type != "england", m.stat %in% c("value", "numerator", "denominator")) %>%
        reshape2::dcast(... ~ m.stat, value.var = "value")

    # * National reference ####

    tmp.nat <- merge(
        q.var.cast
        , qof_measures %>%
            filter(org.type == "england", m.stat == "value") %>%
            select(-ccg_code, -practice_code) %>%
            reshape2::dcast(... ~ m.stat, value.var = "value")
        , by = c(
            "qof_period"
            , "domain_code", "indicator_group_code"
            , "indicator_code", "m.type", "m.name"
        )
        , all.x = TRUE, suffixes = c(".var", ".ref")
    )

    # * N2 (Nottinghamshire and Nottingham) reference ####

    tmp.n2 <- merge(
        q.var.cast
        , qof_measures %>%
            filter(org.type == "lep::instance", m.stat == "value") %>%
            select(-ccg_code, -practice_code) %>%
            reshape2::dcast(... ~ m.stat, value.var = "value")
        , by = c(
            "qof_period"
            , "domain_code", "indicator_group_code", "indicator_code"
            , "m.type", "m.name"
        )
        , all.x = TRUE, suffixes = c(".var", ".ref")
    )

    # * CCG reference ####

    tmp.ccg <- merge(
        q.var.cast
        , qof_measures %>%
            filter(org.type == "ccg::instance", m.stat == "value") %>%
            select(-ccg_code) %>%
            rename(ccg_code = "practice_code") %>%
            reshape2::dcast(... ~ m.stat, value.var = "value")
        , by = c(
            "qof_period"
            , "domain_code", "indicator_group_code", "indicator_code"
            , "ccg_code"
            , "m.type", "m.name"
        )
        , all.x = FALSE, all.y = FALSE, suffixes = c(".var", ".ref")
    )

    # * Combine ####

    qof.combined <- list(tmp.nat, tmp.n2, tmp.ccg) %>%
        bind_rows() %>%
        setDT()

    # Compare ####

    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__process__compare: calculating confidence intervals ... (combined)", "\n")

    benchmark.level <- 0.95

    qof.comp.bench <- data.table::copy(qof.combined) %>%
        status("INFO: - confidence intervals (", benchmark.level, ") ...") %>%
        .[, c('cilo', 'cihi') := aphoci_gen(
            numerator, denominator, multiplier = 100
            , ci_type = 'proportion', level = benchmark.level
            , bTransposeResults = TRUE
        )] %>%
        status("INFO: - prevalence ...") %>%
        .[(m.type == "prevalence")
          , statsig := testci_hilo_s(value.ref, transpose(list(cilo, cihi)))] %>%
        status("INFO: - exceptions ...") %>%
        .[(m.type == "performance") & (m.name %in% "exceptions")
          , statsig := testci_sense_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = FALSE)] %>%
        status("INFO: - remaining ...") %>%
        .[(m.type == "performance") & !(m.name %in% "exceptions")
          , statsig := testci_sense_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = TRUE)] %>%
        status("INFO: - cleaning ...") %>%
        .[, c('cilo', 'cihi') :=  NULL] %>%
        mutate(compare.type = "benchmark", compare.param = benchmark.level) %>%
        #select(-ends_with("ator"), -starts_with("value"))
        status("INFO: done.")

    spc.sd <- 3

    qof.comp.spc.3 <- data.table::copy(qof.combined) %>%
        status("INFO: - control limits (", spc.sd, ") ...") %>%
        .[, statsig := testspc_hilo_s(
            value.var, value.ref, denominator.var = denominator, multiplier = 100
            , ci_type = "proportion", sd = spc.sd
        )] %>%
        status("INFO: - cleaning ...") %>%
        mutate(compare.type = "spc", compare.param = spc.sd) %>%
        status("INFO: done.")

    spc.sd <- 2

    qof.comp.spc.2 <- data.table::copy(qof.combined) %>%
        status("INFO: - control limits (", spc.sd, ") ...") %>%
        .[, statsig := testspc_hilo_s(
            value.var, value.ref, denominator.var = denominator, multiplier = 100
            , ci_type = "proportion", sd = spc.sd
        )] %>%
        status("INFO: - cleaning ...") %>%
        mutate(compare.type = "spc", compare.param = spc.sd) %>%
        status("INFO: done.")

    qof.comp <- list(
        qof.comp.bench
        , qof.comp.spc.3
        , qof.comp.spc.2
    ) %>% bind_rows() %>%
        # drop values and counts - can join with measures or raw if needed.
        select(
            -starts_with("value")
            , -ends_with("ator")
        )

    # Save ####

    if (bWriteCSV) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__compare: saving qof.comp ...", "\n")

        for (qof_root in qof.comp$qof_period %>% unique()) {

            this_csv <- paste0("./data-raw/", qof_root, "_all", file_suffix, ".csv")

            if (verbosity.showatlevel("chatty"))
                cat("INFO: f__process__compare: saving", this_csv, "...", "\n")

            fwrite(
                qof.comp %>% filter(qof_period == qof_root)
                , file = this_csv
            )

            this_csv <- paste0("./data-raw/", qof_root, "_prev", file_suffix, ".csv")

            if (verbosity.showatlevel("chatty"))
                cat("INFO: f__process__compare: saving", this_csv, "...", "\n")

            fwrite(
                qof.comp %>%
                    filter(qof_period == qof_root, m.type == "prevalence")
                , file = this_csv
            )

            this_csv <- paste0("./data-raw/", qof_root, "_ind", file_suffix, ".csv")

            if (verbosity.showatlevel("chatty"))
                cat("INFO: f__process__compare: saving", this_csv, "...", "\n")

            fwrite(
                qof.comp %>%
                    filter(qof_period == qof_root, m.type == "performance")
                , file = this_csv
            )
        }

    } else {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__compare: NOT saving qof.comp ...", "\n")
    }

    if (bSaveData) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: f__process__compare: storing qof_compare ...", "\n")

        store_data(qof.comp, "qof_compare")
    }

    # return

    return(qof.comp)
}

# LOAD ####

#' load raw data
#'
#' Default is not to do anything further.
#'
#' @note
#' Call tree:
#'
#' f__extract__load_raw
#' f__91__preprocess
#'
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @return list of lists (see \code{\link{f__extract__load_raw}})
#'
#'
#' @family Internal routines
#' @family Load routines
#'
f__91__load_data <- function(
    qof_root
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__91__load_data: loading ...", "\n")

    # Add org.type to data elements
    l_add_orgtype <- function(x) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: l_add_orgtype: amending ...", "\n")

        x$data <- x$data %>% lapply(mutate, org.type = "ccg::practice")

        invisible(x)
    }

    # Tag on data source
    l_add_qof_root <- function(x, qof_root) {
        if (verbosity.showatlevel("chatty"))
            cat("INFO: l_add_qof_root: amending ...", "\n")

        x$data <- x$data %>% lapply(mutate, data_source = qof_root)
        x$reference <- x$reference %>% lapply(mutate, data_source = qof_root)

        invisible(x)
    }

    qof <- f__extract__load_raw(qof_root) %>%
        # process lookups
        f__91__preprocess() %>%
        l_add_orgtype() %>%
        l_add_qof_root(qof_root)

    # return

    return(qof)
}

#' Load reference
#'
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @return reference list with named items \itemize{\item{orgref}\item{indmap}}
#'
#'
#' @family Internal routines
#' @family Load routines
#' @family Reference routines
#'
f__91__load_reference <- function(
    qof_root
    , file_suffix = "__processed"
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__91__load_reference: loading ...", "\n")

    taskdir <- proj_root()

    this.file <- paste_paths(taskdir, "./data-raw/", paste0(qof_root, "_orgref", file_suffix, ".csv"))
    q.orgref <- fread(file = this.file)

    this.file <- paste_paths(taskdir, "./data-raw/", paste0(qof_root, "_indmap", file_suffix, ".csv"))
    q.indmap <- fread(file = this.file)

    # return

    return(reference = list(orgref = q.orgref, indmap = q.indmap))
}

#' Load measures
#'
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @return measures data frame
#'
#'
#' @family Internal routines
#' @family Load routines
#' @family Measure routines
#'
f__91__load_measures <- function(
    qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv"
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__91__load_measures: loading ...", "\n")

    taskdir <- proj_root()

    #this.file <- paste_paths(taskdir, "./data-raw/", paste0(qof_root, "_ind", file_suffix, ".csv"))
    #q.ind <- fread(file = this.file)

    #this.file <- paste_paths(taskdir, "./data-raw/", paste0(qof_root, "_prev", file_suffix, ".csv"))
    #q.prev <- fread(file = this.file)

    this.file <- paste_paths(taskdir, "./data-raw/", paste0(qof_root, "_all", file_suffix, ".csv"))
    q.all <- data.table::fread(file = this.file)

    q.ind <- q.all %>% filter(m.type == "performance")
    q.prev <- q.all %>% filter(m.type == "prevalence")

    # return

    return(list(prev = q.prev, ind = q.ind) %>% bind_rows())
}

#' Load compare
#'
#' @param qof_root
#'
#'   Directory root for loading and saving any processed data.  Of the form
#'   "qof-YYZZ"
#'
#' @param file_suffix For loading and saving of any processed data
#'
#' @return compare data frame
#'
#'
#' @family Internal routines
#' @family Load routines
#' @family Compare routines
#'
f__91__load_compare <- function(
    qof_root
    , file_suffix = "__eng_ccg_prac__compare__bench_spc23__eng_ccg"
) {
    if (verbosity.showatlevel("chatty"))
        cat("INFO: f__91__load_compare: loading ...", "\n")

    taskdir <- proj_root()

    #this.file <- paste_paths(taskdir, "./data-raw", paste0(qof_root, "_ind", file_suffix, ".csv"))
    #q.ind <- fread(file = this.file)

    #this.file <- paste_paths(taskdir, "./data-raw", paste0(qof_root, "_prev", file_suffix, ".csv"))
    #q.prev <- fread(file = this.file)

    this.file <- paste_paths(taskdir, "./data-raw/", paste0(qof_root, "_all", file_suffix, ".csv"))
    q.all <- data.table::fread(file = this.file)

    q.ind <- q.all %>% filter(m.type == "performance")
    q.prev <- q.all %>% filter(m.type == "prevalence")

    # return

    return(list(prev = q.prev, ind = q.ind) %>% bind_rows())
}


# Done. ####
