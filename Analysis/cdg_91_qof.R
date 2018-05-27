#
# cdg_91_qof.R
#
# Process QOF for visualisation (interactive?)
#

#
# Require certain disease areas
# - practice level prevalnce and stat. sig. relative to England
# - CDG level achievement (?treatment) and stat. sig. relative to England.
#


cat("INFO: cdg_91_qof: starting...", "\n")

#
# COUNTS - Load QOF data ####
#

#'
#'
#'
f__91__load_reference_raw <- function(qof_root) {
    qof.orgref <- fread(file = paste0("./Data/", qof_root, "-csv/ORGANISATION_REFERENCE.csv")) %>% setnames.clean()
    qof.indmap <- fread(file = paste0("./Data/", qof_root, "-csv/INDICATOR_MAPPINGS.csv")) %>% setnames.clean()

    # return

    return(list(
        orgref = qof.orgref
        , indmap = qof.indmap
    ))
}

#'
#'
#'
f__91__load_data_raw <- function(qof_root) {
    qof.prev <- fread(file = paste0("./Data/", qof_root, "-csv/PREVALENCE.csv")) %>% setnames.clean()

    this.file <- paste0("./Data/", qof_root, "-csv/ACHIEVEMENT_EXCEPTIONS.csv")
    if (!(file.exists(this.file)))
        this.file <- paste0("./Data/", qof_root, "-csv/ACHIEVEMENT.csv")
    qof.ind <- fread(file = this.file) %>% setnames.clean()

    # return

    return(list(
        prev = qof.prev
        , ind = qof.ind
    ))
}

#' load raw QOF data
#'
#' put in an R list for later analysis
#'
f__91__load_raw <- function(
    qof_root
) {
    cat("INFO: q91: loading data ...", "\n")

    #q.ref <- f__91__load_reference_raw(qof_root)
    qof.orgref <- fread(file = paste0("./Data/", qof_root, "-csv/ORGANISATION_REFERENCE.csv")) %>% setnames.clean()
    qof.indmap <- fread(file = paste0("./Data/", qof_root, "-csv/INDICATOR_MAPPINGS.csv")) %>% setnames.clean()

    #q.dat <- f__91__load_data_raw(qof_root)
    qof.prev <- fread(file = paste0("./Data/", qof_root, "-csv/PREVALENCE.csv")) %>% setnames.clean()
    this.file <- paste0("./Data/", qof_root, "-csv/ACHIEVEMENT_EXCEPTIONS.csv")
    if (!(file.exists(this.file)))
        this.file <- paste0("./Data/", qof_root, "-csv/ACHIEVEMENT.csv")
    qof.ind <- fread(file = this.file) %>% setnames.clean()

    # return

    return(list(
        orgref = qof.orgref
        , indmap = qof.indmap
        , prev = qof.prev
        , ind = qof.ind
    ))
}

#' preprocess
#'
#' optionally save tweaked reference data
#'
#'
f__91__preprocess <- function(
    qof
    , bWriteCSV = FALSE
) {

    cat("INFO: q91: processing lookups ...", "\n")

    #~~ orgref - organisation lookups ####

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
    q.orgref <- qof$orgref %>%
        select(starts_with("practice"), starts_with("ccg"))

    #~~ indmap - qof indicator lookups ####

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

    #~ process indicators ####

    cat("INFO: q91: processing indicators ...", "\n")

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
    q.indmap <- qof$indmap %>%
        mutate(is.register = (indicator_code %in% (
            qof$ind %>%
                filter(measure == "REGISTER") %>%
                .$indicator_code %>%
                unique()
        )))

    # filter out non-register indicators via join with indmap
    q.ind <- qof$ind %>%
        filter(!(indicator_code %in% (q.indmap %>% filter(is.register == TRUE) %>% .$indicator_code))) %>%
        # lowercase measure
        # remove points
        # tag ccg, indicator group
        mutate(measure = tolower(measure)) %>%
        filter(!(measure == tolower("ACHIEVED_POINTS"))) %>%
        merge(q.orgref %>% select(practice_code, ccg_code)
              , by = "practice_code") %>%
        merge(q.indmap %>% filter(is.register == FALSE) %>%
                  select(indicator_code, indicator_group_code)
              , by = "indicator_code")

    #~ process prevalence  ####

    cat("INFO: q91: processing prevalence ...", "\n")

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
    # spin down (register, patient_list_size) on measure

    q.prev <- qof$prev %>%
        merge(q.orgref %>% select(practice_code, ccg_code)
              , by = "practice_code") %>%
        mutate(org.type = "ccg, practice")

    # q.prev.praclists <- q.prev %>%
    #     select(practice_code, patient_list_type, patient_list_size) %>%
    #     unique()
    # q.prev.praclists.tbl <- q.prev.praclists %>%
    #     dcast(... ~ patient_list_type, sum, value.var = "patient_list_size")

    q.prev.melt <- q.prev %>%
        melt(measure.vars = c("register", "patient_list_size")
             , variable.name = "measure", variable.factor = FALSE
             , value.name = "value")

    # intermediate save for reference data

    if (bWriteCSV == TRUE) {
        cat("INFO: saving refrence data ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_orgref", "__processed", ".csv")
        fwrite(q.orgref, file = this.file)

        this.file <- paste0("./Results/", qof_root, "_indmap", "__processed", ".csv")
        fwrite(q.indmap, file = this.file)
    }

    # return

    return(list(
        orgref = q.orgref
        , indmap = q.indmap
        , ind = q.ind
        , prev = q.prev
        , prev.melt = q.prev.melt
    ))

}


#
# MEASURES - QOF indicators ####
#

#'
#'
#'
f__91__measures_ind <- function(
    qof
    , bWriteCSV = FALSE
    , qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv"
) {
    cat("INFO: q91: processing indicator measures ...", "\n")

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

    #~ Practice level ####

    q.ind <- qof$ind %>%
        mutate(org.type = "ccg, practice")

    #~ England ####

    q.ind.eng <- qof$ind %>%
        group_by(indicator_group_code, indicator_code, measure) %>%
        summarise(value = sum(value)) %>%
        mutate(ccg_code = "eng", practice_code = "eng", org.type = "england")

    #~ Local CCGs ####

    q.ind.ccgs <- qof$ind %>%
        filter(ccg_code %in% lu.orgs.ccgs.local) %>%
        group_by(ccg_code, indicator_group_code, indicator_code, measure) %>%
        summarise(value = sum(value)) %>%
        mutate(practice_code = "ccg", org.type = "ccg")

    #~ Combine local practice, local CCG, England ####
    # England, local CCGs, local practices
    q.ind.combined <- list(
        q.ind.eng
        , q.ind.ccgs
        , q.ind %>% filter(ccg_code %in% lu.orgs.ccgs.local)
    ) %>%
        rbindlist(use.names = TRUE)

    lu_measures <- fread(strip.white = TRUE, input = "
m.type,      m.name,        m.stat,      i.num, i.den, i.exc
performance, achievement,   numerator,   1,     0,     0
performance, achievement,   denominator, 0,     1,     0
performance, treatment,     numerator,   1,     0,     0
performance, treatment,     denominator, 0,     1,     1
performance, exceptions,    numerator,   0,     0,     1
performance, exceptions,    denominator, 0,     1,     1
performance, suboptimal,    numerator,  -1,     1,     1
performance, suboptimal,    denominator, 0,     1,     1
")

    q.ind.measures <- q.ind.combined %>%
        filter(!is.na(value)) %>%
        # spin up numerator, denominator, exceptions
        dcast(... ~ measure, fun = sum, value.var = "value") %>%
        # cross join measures and combine and remove intermediate columns
        merge(lu_measures %>% filter(m.type == "performance")) %>%
        mutate(value = (i.num * numerator + i.den * denominator + i.exc * exceptions)) %>%
        select(-matches("num|den|exc")) %>%
        # spin up m.numerator, m.denominator, ensure double and calculate m.value
        dcast(... ~ m.stat, fun = sum, value.var = "value") %>%
        mutate(value = 100.0 * numerator / denominator) %>%
        # melt down numerator, denominator and value on m.stat
        melt(measure.vars = c("numerator", "denominator", "value")
             , variable.name = "m.stat", variable.factor = FALSE
             , value.name = "value")

    #~ Save ####

    if (bWriteCSV) {
        cat("INFO: cdg_91_qof: saving qof.ind.combined ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_ind", file_suffix, ".csv")
        fwrite(q.ind.measures, file = this.file)

    } else {
        cat("INFO: cdg_91_qof: NOT saving qof.ind.combined ...", "\n")
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
    return(list(ind = q.ind.measures))

}

#qof.ind.measures <- f__measures_ind(qof)[[1]]

#
# PREVALENCE ####
#

#'
#'
#'
f__91__measures_prev <- function(
    qof
    , bWriteCSV = FALSE
    , qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv"
) {

    cat("INFO: q91: processing prevalence measures ...", "\n")

    # England, CCG, CDG, Practice level

    #str(qof.prev.melt)
    # Classes ‘data.table’ and 'data.frame':	319998 obs. of  6 variables:
    #  $ practice_code       : chr  "B82007" "B82007" "B82007" "B82007" ...
    #  $ indicator_group_code: chr  "AF" "AST" "CAN" "CHD" ...
    #  $ patient_list_type   : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...
    #  $ ccg_code            : chr  "02N" "02N" "02N" "02N" ...
    #  $ measure             : chr  "register" "register" "register" "register" ...
    #  $ value               : int  238 783 290 399 485 257 71 122 710 495 ...

    #~ England ####

    # spin down on measure, ignore missing values, tag as England

    q.prev.eng <- qof$prev.melt %>%
        group_by(indicator_group_code, patient_list_type, measure) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        mutate(ccg_code = "eng", practice_code = "eng", org.type = "england")

    #~ CCGs ####

    # spin down on measure, ignore missing values, tag as ccg

    q.prev.ccgs <- qof$prev.melt %>%
        filter(ccg_code %in% lu.orgs.ccgs.local) %>%
        group_by(ccg_code, indicator_group_code, patient_list_type, measure) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        mutate(practice_code = "ccg", org.type = "ccg")

    #~ Combine local practice, local CCG, England ####

    # England, local CCGs, local practices

    qof.prev.combined <- list(
        q.prev.eng
        , q.prev.ccgs
        , qof$prev.melt %>% filter(ccg_code %in% lu.orgs.ccgs.local)
    ) %>%
        rbindlist(use.names = TRUE)

    #~ Calculate measures ####

    # # spin up numerator, denominator, exceptions
    # # Oops - need Larwood somehow but is not in QOF.  Filter out for now.
    # tmp <- qof.prev.combined %>% filter(!is.na(value)) %>%
    #     dcast(... ~ measure, sum, value.var = "value")
    # # cross join measures and combine
    # # NOTE: merge.data.table does not seem to do the cross join - use merge.data.frame
    # tmp2 <- setDT(merge(setDF(tmp), setDF(lu_measures[m.type == "prevalence"]))
    #               )[, value := sum(i.num * register, i.den * patient_list_size)
    #                 , .(indicator_group_code, org.type, ccg_code, practice_code, m.type, m.name, m.stat)
    #                 ][, c("register", "patient_list_size", "i.num", "i.den", "i.exc") := NULL]
    #
    # # spin up m.numerator, m.denominator and calculate m.value, ensure numerator and denominator are double
    # tmp3 <- dcast(tmp2, ... ~ m.stat, sum, value.var = "value"
    #               )[, c("numerator", "denominator") := list(as.double(numerator), as.double(denominator))
    #                 ][, value := 100 * numerator / denominator]
    # # melt down numerator, denominator and value on m.stat
    # tmp4 <- melt(tmp3
    #              , measure.vars = c("numerator", "denominator", "value")
    #              , variable.name = "m.stat", variable.factor = FALSE
    #              , value.name = "value")
    #
    # qof.prev.combined <- tmp4
    # rm(tmp, tmp2, tmp3, tmp4)

    lu_measures <- fread(strip.white = TRUE, input = "
m.type,      m.name,        m.stat,      i.num, i.den, i.exc
prevalence,  qofprevalence, numerator,   1,     0,     NA
prevalence,  qofprevalence, denominator, 0,     1,     NA
")

    q.prev.measures <- qof.prev.combined %>%
        filter(!is.na(value)) %>%
        # spin up numerator, denominator, exceptions
        dcast(... ~ measure, fun = sum, value.var = "value") %>%
        # cross join measures and combine and remove intermediate columns
        merge(lu_measures %>% filter(m.type == "prevalence")) %>%
        mutate(value = (i.num * register + i.den * patient_list_size)) %>%
        select(-matches("num|den|exc|register|list_size")) %>%
        # spin up m.numerator, m.denominator, ensure double and calculate m.value
        dcast(... ~ m.stat, fun = sum, value.var = "value") %>%
        mutate_at(c("numerator", "denominator"), as.double) %>%
        mutate(value = 100 * numerator / denominator) %>%
        # melt down numerator, denominator and value on m.stat
        melt(measure.vars = c("numerator", "denominator", "value")
             , variable.name = "m.stat", variable.factor = FALSE
             , value.name = "value")


    #~ Save ####

    if (bWriteCSV) {
        cat("INFO: cdg_91_qof: saving qof.prev.combined ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_prev", file_suffix, ".csv")
        fwrite(q.prev.measures, file = this.file)

    } else {
        cat("INFO: cdg_91_qof: NOT saving qof.prev.combined ...", "\n")
    }

    # return
    return(list(prev = q.prev.measures))

}

#'
#'
#'
f__91__load_measures <- function(
    qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv"
) {

    this.file <- paste0("./Results/", qof_root, "_ind", file_suffix, ".csv")
    q.ind <- fread(file = this.file)

    this.file <- paste0("./Results/", qof_root, "_prev", file_suffix, ".csv")
    q.prev <- fread(file = this.file)

    # return

    return(list(prev = q.prev, ind = q.ind))
}

#
# COMPARE - Add England comparator and significance test ####
#

#'
#'
#'
f__91__compare <- function(
    qof_measures
    , bWriteCSV = TRUE
    , qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv__comp_eng_ccg"
    ) {

    cat("INFO: q91: processing statistical significance comparison ...", "\n")

    #source("./Analysis/aphoci.R")
    source("./Analysis/calcci.R")
    source("./Analysis/testci.R")

    ##
    ## QOF
    ##
    # Tag prevalence at practice level with England
    # Tag achievement at CDG level with England
    # [optional] tag Treatment at CDG level with England.
    #

    #~ Prevalence ####

    # Melted on statistic.  Extract England, spin both up on m.stat, tag England,
    # do stat. compare, remove uneeded columns, spin back down

    cat("INFO: q91: creating reference lookups ...", "\n")

    #qof_measures$prev$org.type %>% unique() %>% print()
    # [1] "ccg, practice" "ccg"           ""england"

    #~~ All that is not England ####

    q.prev.var.cast <- qof_measures$prev %>%
        filter(org.type != "england"
               , m.stat %in% c('value', 'numerator', 'denominator')) %>%
        select(indicator_group_code
               , org.type, ccg_code, practice_code
               , m.stat, value) %>%
        dcast(... ~ m.stat, value.var = 'value')

    #~~ National reference ####


    tmp.nat <- merge(
        q.prev.var.cast
        # qof.prev.ref.cast
        , qof_measures$prev %>%
            filter(org.type == 'england', m.stat %in% c('value')) %>%
            select(indicator_group_code, org.type, m.stat, value) %>%
            dcast(... ~ m.stat, value.var = 'value')
        , by = c('indicator_group_code')
        , all.x = TRUE, suffixes = c('.var', '.ref')
    )

    #~~ ccg reference ####

    tmp.ccg <- merge(
        q.prev.var.cast
        # qof.prev.ref.cast
        , qof_measures$prev %>%
            filter(org.type == 'ccg', m.stat %in% c('value')) %>%
            select(indicator_group_code, org.type, ccg_code, m.stat, value) %>%
            dcast(... ~ m.stat, value.var = 'value')
        , by = c('indicator_group_code', 'ccg_code')
        , all.x = TRUE, suffixes = c('.var', '.ref')
    )

    #~~ combine ####


    cat("INFO: q91: calculating confidence intervals ...", "\n")

    if (FALSE) {
        qof.prev.comp <- list(tmp.nat, tmp.ccg) %>%
            rbindlist(use.names = TRUE) %>%
            .[, c('cilo', 'cihi') := vaphoci_gen(numerator, denominator, multiplier = 100, ci.type = 'proportion')] %>%
            .[, statsig := vtestci_s(value.ref, transpose(list(cilo, cihi)))] %>%
            .[, c('cilo', 'cihi') :=  NULL]
    } else {
        qof.prev.comp <- list(tmp.nat, tmp.ccg) %>%
            rbindlist(use.names = TRUE) %>%
            .[, c('cilo', 'cihi') := aphoci_gen(numerator, denominator, multiplier = 100, ci.type = "proportion", bTransposeResults = TRUE)] %>%
            .[, statsig := testci_hilo_s(value.ref, transpose(list(cilo, cihi)))] %>%
            .[, c('cilo', 'cihi') :=  NULL]
    }

    #~ save ####

    if (bWriteCSV) {
        cat("INFO: saving qof.prev.comp ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_prev", file_suffix, ".csv")
        fwrite(qof.prev.comp, file = this.file)
    } else {
        cat("INFO: NOT saving qof.prev.comp ...", "\n")
    }


    #~ Indicators ####

    # Melted on statistic.  Extract England, spin both up on m.stat, tag England,
    # do stat. compare, remove uneeded columns, spin back down

    # qof.ind.combined <- qof_measures$ind

    #this.file <- paste0("./Results/", qof_root, "_ind__eng_ccg_prac__measure_ndv.csv")
    #qof.ind.combined <- fread(file = this.file)

    #print(unique(qof_measures$ind[, org.type]))
    # [1] "ccg, practice" "ccg"           "england"
    #print(unique(qof_measures$ind[, m.name]))
    # [1] "achievement" "treatment"

    cat("INFO: q91: creating reference lookups ...", "\n")

    #~~ All that is not England ####

    q.ind.var.cast <- qof_measures$ind %>%
        filter(org.type != "england", m.stat %in% c('value', 'numerator', 'denominator')) %>%
        select(indicator_group_code, indicator_code
               , org.type, ccg_code, practice_code
               , m.name, m.stat, value) %>%
        dcast(... ~ m.stat, value.var = 'value')

    #~~ National reference ####


    tmp.nat <- merge(
        q.ind.var.cast
        # qof.ind.ref.cast
        , qof_measures$ind %>%
            filter(org.type == 'england', m.stat %in% c('value')) %>%
            select(indicator_group_code, indicator_code
                   , org.type
                   , m.name, m.stat, value) %>%
            dcast(... ~ m.stat, value.var = 'value')
        , by = c('indicator_group_code', 'indicator_code', 'm.name')
        , all.x = TRUE, suffixes = c('.var', '.ref')
    )

    #~~ ccg reference ####


    tmp.ccg <- merge(
        q.ind.var.cast
        # q.ind.ref.cast
        , qof_measures$ind %>%
            filter(org.type == 'ccg', m.stat %in% c('value')) %>%
            select(indicator_group_code, indicator_code
                   , org.type, ccg_code
                   , m.name, m.stat, value) %>%
            dcast(... ~ m.stat, value.var = 'value')
        , by = c('indicator_group_code', 'indicator_code', 'ccg_code', 'm.name')
        , all.x = TRUE, suffixes = c('.var', '.ref')
    )

    #~ combine ####

    # qof.ind.comp <- list(tmp.nat, tmp.ccg) %>% rbindlist(use.names = TRUE)
    #
    # qof.ind.comp[, c('cilo', 'cihi') := vaphoci_gen(numerator, denominator, multiplier = 100, ci.type = 'proportion')]
    # qof.ind.comp[, statsig := vtestci_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = TRUE)]
    # qof.ind.comp[, c('cilo', 'cihi') :=  NULL]

    cat("INFO: q91: calculating confidence intervals ...", "\n")

    if (FALSE) {
        qof.ind.comp <- list(tmp.nat, tmp.ccg) %>%
            rbindlist(use.names = TRUE) %>%
            .[, c('cilo', 'cihi') := vaphoci_gen(numerator, denominator, multiplier = 100, ci.type = 'proportion')] %>%
            .[, statsig := vtestci_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = TRUE)] %>%
            .[, c('cilo', 'cihi') :=  NULL]
    } else {
        qof.ind.comp <- list(tmp.nat, tmp.ccg) %>%
            rbindlist(use.names = TRUE) %>%
            .[, c('cilo', 'cihi') := aphoci_gen(numerator, denominator, multiplier = 100, ci.type = 'proportion', bTransposeResults = TRUE)] %>%
            .[!(m.name %in% "exceptions"), statsig := testci_sense_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = TRUE)] %>%
            .[(m.name %in% "exceptions"), statsig := testci_sense_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = FALSE)] %>%
            .[, c('cilo', 'cihi') :=  NULL]
    }

    #~ save ####

    if (bWriteCSV) {
        cat("INFO: saving qof.ind.comp ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_ind", file_suffix, ".csv")
        fwrite(qof.ind.comp, file = this.file)

    } else {
        cat("INFO: NOT saving qof.ind.comp ...", "\n")
    }

    # return

    return(list(
        prev = qof.prev.comp
        , ind = qof.ind.comp
    ))

}

#'
#'
#'
f__91__load_compare <- function(
    qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv__comp_eng_ccg"
) {

    this.file <- paste0("./Results/", qof_root, "_ind", file_suffix, ".csv")
    q.ind <- fread(file = this.file)

    this.file <- paste0("./Results/", qof_root, "_prev", file_suffix, ".csv")
    q.prev <- fread(file = this.file)

    # return

    return(list(prev = q.prev, ind = q.ind))
}

#' process all
#'
#' Laod raw data and produce measures and compare aginst england.
#'
#'
f__91__process_all <- function(
    bWriteCSV = FALSE
    , qof_period = "1516" # "1617"
) {

    #
    # Set environment ####
    #

    require("data.table")
    require("dplyr")

    # For use in e.g. dcast to ignore NAs
    sum.rmna <- function(x) return(sum(x, na.rm = TRUE))
    # to clean table/frame names
    setnames.clean <- function(x) {
        setnames(x, make.names(tolower(colnames(x))))
    }

    # Config ####

    cat("INFO: bWriteCSV =", bWriteCSV, "\n")

    if (qof_period %in% c("1516", "1617")) {

        qof_root <- paste("qof", qof_period, sep = "-")

    } else {
        cat("WARNING: qof period", qof_period, "unknown ...", "\n")

        return(FALSE)
    }

    qof <- f__91__load_raw(qof_root) %>%
        #~ process lookups
        f__91__preprocess()

    # Localisation

    lu.orgs.ccgs.local <- c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))

    #~ Calculate performance measures ####

    qof_measures <- c(
        f__91__measures_ind(qof, bWriteCSV, qof_root)
        , f__91__measures_prev(qof, bWriteCSV, qof_root)
    )
    #qof_measures <- f__91__load_measures(qof_root)

    qof_compare <- f__91__compare(qof_measures, bWriteCSV, qof_root)
    #qof_compare <- f__91__load_compare(qof_root)

    # return

    return(list(
        ind = qof$ind %>% filter(ccg_code %in% lu.orgs.ccgs.local)
        , prev = qof$prev %>% filter(ccg_code %in% lu.orgs.ccgs.local)
        , ind.measures = qof_measures$ind
        , prev.measures = qof_measures$ind
        , ind.comp = qof_compare$ind
        , prev.comp = qof_compare$prev
        , orgref = qof$orgref
        , indmap = qof$indmap
    ))
}

#'
#'
#'
f__91__load_measures_compare <- function(
    qof_period = "1516" # "1617"
) {

    require("data.table")
    require("dplyr")

    if (qof_period %in% c("1516", "1617")) {
        qof_root <- paste("qof", qof_period, sep = "-")
    } else {
        cat("WARNING: qof period", qof_period, "unknown ...", "\n")
        return(FALSE)
    }

    # Localisation

    lu.orgs.ccgs.local <- c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))

    qof_measures <- f__91__load_measures(qof_root)
    qof_compare <- f__91__load_compare(qof_root)

    # return

    return(list(
        ind.measures = qof_measures$ind
        , prev.measures = qof_measures$ind
        , ind.comp = qof_compare$ind
        , prev.comp = qof_compare$prev
    ))
}

# Done. ####

cat("INFO: Done.", "\n")


