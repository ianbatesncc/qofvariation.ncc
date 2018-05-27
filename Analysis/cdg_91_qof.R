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
# Helpers ####
#

require("data.table")
require("dplyr")

# progress with a pipe
status <- function(x, ...){cat(..., "\n");invisible(x)}

# For use in e.g. dcast to ignore NAs
sum.rmna <- function(x) return(sum(x, na.rm = TRUE))

# to clean table/frame names
setnames.clean <- function(x) {setnames(x, make.names(tolower(colnames(x))))}


#
# COUNTS - Load QOF data ####
#

#' load raw QOF data
#'
#' put in an R list for later analysis
#'
f__91__load_raw <- function(
    qof_root
) {
    cat("INFO: f__91__load_raw: loading data ...", "\n")

    qof.orgref <- fread(file = paste0("./Data/", qof_root, "-csv/ORGANISATION_REFERENCE.csv")) %>% setnames.clean()
    qof.indmap <- fread(file = paste0("./Data/", qof_root, "-csv/INDICATOR_MAPPINGS.csv")) %>% setnames.clean()

    qof.prev <- fread(file = paste0("./Data/", qof_root, "-csv/PREVALENCE.csv")) %>% setnames.clean()
    this.file <- paste0("./Data/", qof_root, "-csv/ACHIEVEMENT_EXCEPTIONS.csv")
    if (!(file.exists(this.file)))
        this.file <- paste0("./Data/", qof_root, "-csv/ACHIEVEMENT.csv")
    qof.ind <- fread(file = this.file) %>% setnames.clean()

    # return

    return(list(
        reference = list(
            orgref = qof.orgref
            , indmap = qof.indmap
        )
        , data = list(
            prev = qof.prev
            , ind = qof.ind
        )
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
    cat("INFO: f__91__preprocess: processing lookups ...", "\n")

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
    q.orgref <- qof$reference$orgref %>%
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
        )))

    # filter out non-register indicators via join with indmap
    q.ind <- qof$data$ind %>%
        filter(!(indicator_code %in% (q.indmap %>% filter(is.register == TRUE) %>% .$indicator_code))) %>%
        # lowercase measure
        # remove points
        # tag ccg, indicator group
        mutate(measure = tolower(measure)) %>%
        filter(!(measure == tolower("ACHIEVED_POINTS"))) %>%
        # tag ccg
        merge(q.orgref %>% select(practice_code, ccg_code)
              , by = "practice_code") %>%
        # filter non-register AND tag indicator_group_code
        merge(
            q.indmap %>%
                filter(is.register == FALSE) %>%
                select(indicator_code, indicator_group_code)
            , by = "indicator_code"
        )


    #~ process prevalence  ####

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
        # tag on indicator_code
        merge(q.indmap %>%
                  filter(is.register == TRUE) %>%
                  select(indicator_group_code, indicator_code)
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

#'
#'
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

        this.file <- paste0("./Results/", qof_root, "_orgref", file_suffix, ".csv")
        fwrite(qof$reference$orgref, file = this.file)

        this.file <- paste0("./Results/", qof_root, "_indmap", file_suffix, ".csv")
        fwrite(qof$reference$indmap, file = this.file)
    }
}

#'
#'
#'
f__91__load_reference <- function(
    qof_root
    , file_suffix = "__processed"
) {
    cat("INFO: f__91__load_reference: loading ...", "\n")

    this.file <- paste0("./Results/", qof_root, "_orgref", file_suffix, ".csv")
    q.orgref <- fread(file = this.file)

    this.file <- paste0("./Results/", qof_root, "_indmap", file_suffix, ".csv")
    q.indmap <- fread(file = this.file)

    # return

    return(reference = list(orgref = q.orgref, indmap = q.indmap))
}

#'
#'
#'
f__91__load_data <- function(
    qof_period
) {
    cat("INFO: f__91__load_data: loading ...", "\n")

    if (qof_period %in% c("1516", "1617")) {
        qof_root <- paste("qof", qof_period, sep = "-")
    } else {
        cat("WARNING: qof period", qof_period, "unknown ...", "\n")
        return(FALSE)
    }

    qof <- f__91__load_raw(qof_root) %>%
        #~ process lookups
        f__91__preprocess()

    # return

    return(data = qof$data)
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
    , lu.orgs.ccgs.local = c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))
) {
    cat("INFO: f__91__measures_ind: processing indicator measures ...", "\n")

    cat("INFO: - local ccgs = (", paste(lu.orgs.ccgs.local, sep = ", "), ")", "\n")

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

    q.ind <- qof$data$ind %>%
        mutate(org.type = "ccg, practice")

    #~ England ####

    q.ind.eng <- qof$data$ind %>%
        group_by_at(vars(-value, -ccg_code, -practice_code)) %>%
        summarise(value = sum(value)) %>%
        mutate(ccg_code = "eng", practice_code = "eng", org.type = "england")

    #~ Local CCGs ####

    q.ind.ccgs <- qof$data$ind %>%
        filter(ccg_code %in% lu.orgs.ccgs.local) %>%
        group_by_at(vars(-value, -practice_code)) %>%
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

    #~ Calculate measures ####

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

    return(q.ind.measures)
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
    , lu.orgs.ccgs.local = c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))
) {
    cat("INFO: f__91__measures_prev: processing prevalence measures ...", "\n")

    cat("INFO: - local ccgs = (", paste(lu.orgs.ccgs.local, sep = ", "), ")", "\n")

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

    q.prev.eng <- qof$data$prev.melt %>%
        group_by_at(vars(-value, -ccg_code, -practice_code)) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        mutate(ccg_code = "eng", practice_code = "eng", org.type = "england")

    #~ CCGs ####

    # spin down on measure, ignore missing values, tag as ccg

    q.prev.ccgs <- qof$data$prev.melt %>%
        filter(ccg_code %in% lu.orgs.ccgs.local) %>%
        group_by_at(vars(-value, -practice_code)) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        mutate(practice_code = "ccg", org.type = "ccg")

    #~ Combine local practice, local CCG, England ####

    # England, local CCGs, local practices

    qof.prev.combined <- list(
        q.prev.eng
        , q.prev.ccgs
        , qof$data$prev.melt %>%
            filter(ccg_code %in% lu.orgs.ccgs.local) %>%
            mutate(org.type = "ccg, practice")
    ) %>%
        rbindlist(use.names = TRUE)

    #~ Calculate measures ####

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

    return(q.prev.measures)
}

#'
#'
#'
f__91__measures <- function(
    qof
    , bWriteCSV = FALSE
    , qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv"
    , lu.orgs.ccgs.local = c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))
) {
    cat("INFO: f__91__measures: processing ...", "\n")

    cat("INFO: - local ccgs = (", paste(lu.orgs.ccgs.local, sep = ", "), ")", "\n")

    m.ind <- f__91__measures_ind(
        qof
        , bWriteCSV = bWriteCSV
        , qof_root, file_suffix
        , lu.orgs.ccgs.local = lu.orgs.ccgs.local
    )

    m.prev <- f__91__measures_prev(
        qof
        , bWriteCSV = bWriteCSV
        , qof_root, file_suffix
        , lu.orgs.ccgs.local = lu.orgs.ccgs.local
    )

browser()
    # return

    return(list(m.ind, m.prev) %>% rbindlist(use.names = TRUE))
}

#'
#'
#'
f__91__load_measures <- function(
    qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv"
) {
    cat("INFO: f__91__load_measures: loading ...", "\n")

    this.file <- paste0("./Results/", qof_root, "_ind", file_suffix, ".csv")
    q.ind <- fread(file = this.file)

    this.file <- paste0("./Results/", qof_root, "_prev", file_suffix, ".csv")
    q.prev <- fread(file = this.file)

    # return

    return(list(prev = q.prev, ind = q.ind) %>% rbindlist(use.names = TRUE))
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

    cat("INFO: f__91__compare: processing statistical significance comparison ...", "\n")

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

    cat("INFO: f__91__compare: creating reference lookups ...", "\n")

    #qof_measures$prev$org.type %>% unique() %>% print()
    # [1] "ccg, practice" "ccg"           ""england"

    #~~ All that is not England ####

    q.var.cast <- qof_measures %>%
        filter(org.type != "england", m.stat %in% c('value', 'numerator', 'denominator')) %>%
        select(indicator_group_code, indicator_code
               , org.type, ccg_code, practice_code
               , m.type, m.name, m.stat, value) %>%
        dcast(... ~ m.stat, value.var = 'value')

    #~~ National reference ####

    tmp.nat <- merge(
        q.var.cast
        , qof_measures %>%
            filter(org.type == 'england', m.stat %in% c('value')) %>%
            select(indicator_group_code, indicator_code
                   , org.type
                   , m.type, m.name, m.stat, value) %>%
            dcast(... ~ m.stat, value.var = 'value')
        , by = c('indicator_group_code', 'indicator_code', "m.type", 'm.name')
        , all.x = TRUE, suffixes = c('.var', '.ref')
    )

    #~~ ccg reference ####

    tmp.ccg <- merge(
        q.var.cast
        , qof_measures %>%
            filter(org.type == 'ccg', m.stat %in% c('value')) %>%
            select(indicator_group_code, indicator_code
                   , org.type, ccg_code
                   , m.type, m.name, m.stat, value) %>%
            dcast(... ~ m.stat, value.var = 'value')
        , by = c('indicator_group_code', 'indicator_code', 'ccg_code', "m.type", 'm.name')
        , all.x = TRUE, suffixes = c('.var', '.ref')
    )

    #~~ combine ####

    qof.combined <- list(tmp.nat, tmp.ccg) %>%
        rbindlist(use.names = TRUE)

    #~~ compare ####

    cat("INFO: f__91__compare: calculating confidence intervals ... (combined)", "\n")

    qof.comp <- qof.combined %>%
        status("INFO: - aphoci_gen ...") %>%
        .[, c('cilo', 'cihi') := aphoci_gen(numerator, denominator, multiplier = 100, ci.type = 'proportion', bTransposeResults = TRUE)] %>%
        status("INFO: - prevalence ...") %>%
        .[(m.type == "prevalence"), statsig := testci_hilo_s(value.ref, transpose(list(cilo, cihi)))] %>%
        status("INFO: - exceptions ...") %>%
        .[(m.type == "performance") & (m.name %in% "exceptions"), statsig := testci_sense_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = FALSE)] %>%
        status("INFO: - remaining ...") %>%
        .[(m.type == "performance") & !(m.name %in% "exceptions"), statsig := testci_sense_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = TRUE)] %>%
        status("INFO: - cleaning ...") %>%
        .[, c('cilo', 'cihi') :=  NULL] %>%
        status("INFO: done.")

    #~ save ####

    if (bWriteCSV) {
        cat("INFO: saving qof.comp ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_all", file_suffix, ".csv")
        fwrite(qof.comp, file = this.file)

        cat("INFO: saving qof.prev.comp ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_prev", file_suffix, ".csv")
        fwrite(qof.comp %>% filter(m.type == "prevalence"), file = this.file)

        cat("INFO: saving qof.ind.comp ...", "\n")

        this.file <- paste0("./Results/", qof_root, "_ind", file_suffix, ".csv")
        fwrite(qof.comp %>% filter(m.type == "performance"), file = this.file)

    } else {
        cat("INFO: NOT saving qof.comp ...", "\n")
    }

    # return

    return(qof.comp)
}

#'
#'
#'
f__91__load_compare <- function(
    qof_root
    , file_suffix = "__eng_ccg_prac__measure_ndv__comp_eng_ccg"
) {
    cat("INFO: f__91__load_compare: loading ...", "\n")

    this.file <- paste0("./Results/", qof_root, "_ind", file_suffix, ".csv")
    q.ind <- fread(file = this.file)

    this.file <- paste0("./Results/", qof_root, "_prev", file_suffix, ".csv")
    q.prev <- fread(file = this.file)

    # return

    return(list(prev = q.prev, ind = q.ind) %>% rbindlist(use.names = TRUE))
}

# EXPORT routines that string these together ####

#' process all
#'
#' Load raw data and produce measures and compare against england.
#'
#' @export
#'
f__91__process__reference_measures_compare <- function(
    qof_period = "1516" # "1617"
    , lu.orgs.ccgs.local = c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))
    , bWriteCSV = FALSE
) {
    cat("INFO: f__91__process__reference_measures_compare: processing ...", "\n")
    # Config ####

    cat("INFO: bWriteCSV =", bWriteCSV, "\n")

    if (qof_period %in% c("1516", "1617")) {
        qof_root <- paste("qof", qof_period, sep = "-")
    } else {
        cat("WARNING: qof period", qof_period, "unknown ...", "\n")
        return(FALSE)
    }

    qof <- f__91__load_raw(qof_root) %>%
        f__91__preprocess()

    qof %>% f__91__save_reference(qof_root, bWriteCSV = bWriteCSV)

    # Localisation

    #lu.orgs.ccgs.local <- c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))

    #~ Calculate performance measures ####

    qof_measures <- f__91__measures(
        qof
        , bWriteCSV = bWriteCSV, qof_root
        , lu.orgs.ccgs.local = lu.orgs.ccgs.local
    )

    qof_compare <- f__91__compare(qof_measures, bWriteCSV = bWriteCSV, qof_root)

    # return

    return(list(
        data = qof$data %>% lapply(filter, ccg_code %in% lu.orgs.ccgs.local)
        , reference = qof$reference
        , measures = qof_measures
        , compare = qof_compare
    ))
}

#' load processed
#'
#' @export
#'
f__91__load__reference_measures_compare <- function(
    qof_period = "1516" # "1617"
) {
    cat("INFO: f__91__load__reference_measures_compare: loading ...", "\n")

    require("data.table")
    require("dplyr")

    if (qof_period %in% c("1516", "1617")) {
        qof_root <- paste("qof", qof_period, sep = "-")
    } else {
        cat("WARNING: qof period", qof_period, "unknown ...", "\n")
        return(FALSE)
    }

    # Localisation

    qof_reference <- f__91__load_reference(qof_root)
    qof_measures <- f__91__load_measures(qof_root)
    qof_compare <- f__91__load_compare(qof_root)

    lu.orgs.ccgs.local <- qof_measures$ind$ccg_code %>% unique()

    # return

    return(list(
        reference = qof_reference
        , measures = qof_measures
        , compare = qof_compare
    ))
}

# Done. ####

cat("INFO: Done.", "\n")

