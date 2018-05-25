##
## cdg_92_qof_present.R
##
## Process QOF for visualisation (interactive?)
##

##
## Require certain disease areas
## - practice level prevalnce and stat. sig. relative to England
## - CDG level achievement (?treatemtn) and stat. sig. relative to England.
##

#
## Output Files
# ./Results/qof_ind__eng_ccg_cdg_prac__measure_ndv.csv
# ./Results/qof_prev__eng_ccg_cdg_prac__measure_ndv.csv
#
## Output Variables
# qof.prev.combined
# qof.ind.combined
#
## Input Files
# ./Data/qof-1516-csv/ORGANISATION_REFERENCE.csv
# ./Data/qof-1516-csv/INDICATOR_MAPPINGS.csv
# ./Data/qof-1516-csv/PREVALENCE.csv
# ./Data/qof-1516-csv/ACHIEVEMENT_EXCEPTIONS.csv
#
## Depends
# ./Analysis/cdg_00_org_relations.R
# ./Analysis/cdg_00_geo_lsoa.R
#

cat("INFO: cdg_92_qof_present: starting...", "\n")

#
# Set environment ####
#

taskdir <- "/Users/bates/Work/Txx CDG Profiles"
#taskdir <- "C:/Users/Public/Documents/Public Work UNRESTRICTED/Txx CDG Profiles"
datadir <- "./Data"
analysisdir <- "./Analysis"
resultsdir <- "./Results"
mappingdir <- "./Mapping"
setwd(taskdir)


library("data.table")
library("dplyr")

# For use in e.g. dcast to ignore NAs
sum.rmna <- function(x) return(sum(x, na.rm = TRUE))

#

# Load relations ####

#source("./Analysis/cdg_00_org_relations.R")
#source("./Analysis/cdg_00_geo_lsoa.R")

# Config ####

# flag write .CSV files
bWriteCSV <- FALSE
bWriteCSV <- TRUE

##
## QOF data
##

qof.orgref <- fread("./Data/qof-1516-csv/ORGANISATION_REFERENCE.csv")
qof.indmap <- fread("./Data/qof-1516-csv/INDICATOR_MAPPINGS.csv")
#qof.prev <- fread("./Data/qof-1516-csv/PREVALENCE.csv")
#qof.ind <- fread("./Data/qof-1516-csv/ACHIEVEMENT_EXCEPTIONS.csv")

# Some tidying
setnames(qof.orgref, make.names(tolower(colnames(qof.orgref))))
setnames(qof.indmap, make.names(tolower(colnames(qof.indmap))))
#setnames(qof.prev, make.names(tolower(colnames(qof.prev))))
#setnames(qof.ind, make.names(tolower(colnames(qof.ind))))

qof.ind.combined <- fread("./Results/qof_ind__eng_ccg_prac__measure_ndv.csv")
qof.ind.comp <- fread("./Results/qof_ind__eng_ccg_prac__measure_ndv__comp_eng_ccg.csv")

# drop uneeded columns
qof.orgref[, grep("practice|ccg", colnames(qof.orgref), invert = TRUE) := NULL]

## indmap - qof indicator lookups

str(qof.indmap)
# Classes ‘data.table’ and 'data.frame':	78 obs. of  8 variables:
# $ indicator_code             : chr  "AF001" "AF006" "AF007" "AST001" ...
# $ indicator_description      : chr  "The contractor establishes and maintains a register of patients with atrial fibrillation" "The percentage of patients with atrial fibrillation in whom stroke risk has been assessed using the CHA2DS2-VASc score risk str"| __truncated__ "In those patients with atrial fibrillation with a record of a CHA2DS2-VASc score of 2 or more, the percentage of patients who a"| __truncated__ "The contractor establishes and maintains a register of patients with asthma, excluding patients with asthma who have been presc"| __truncated__ ...
# $ indicator_point_value      : int  5 12 12 4 15 20 6 15 5 6 ...
# $ indicator_group_code       : chr  "AF" "AF" "AF" "AST" ...
# $ indicator_group_description: chr  "Atrial fibrillation" "Atrial fibrillation" "Atrial fibrillation" "Asthma" ...
# $ domain_code                : chr  "CL" "CL" "CL" "CL" ...
# $ domain_description         : chr  "Clinical" "Clinical" "Clinical" "Clinical" ...
# $ patient_list_type          : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...

## ind - qof indicator counts
# Tag CCG
# Find register indicators

str(qof.ind)
# Classes ‘data.table’ and 'data.frame':	1956549 obs. of  4 variables:
# $ practice_code : chr  "A81001" "A81001" "A81001" "A81001" ...
# $ indicator_code: chr  "AF001" "AF001" "AF006" "AF006" ...
# $ measure       : chr  "ACHIEVED_POINTS" "REGISTER" "ACHIEVED_POINTS" "DENOMINATOR" ...
# $ value         : num  5 96 12 57 2 53 12 76 4 71 ...

## Remove register-type indicators
# add an 'is.register' flag to the indicator map
qof.indmap[, is.register := FALSE]
qof.indmap[indicator_code %in% unique(qof.ind[measure == "REGISTER", indicator_code])
           , is.register := TRUE]
# filter out non-register indicators via join with indmao
qof.ind <- qof.ind[qof.indmap[, .(indicator_code, is.register)], on = "indicator_code"
                   ][is.register == FALSE, ][, is.register := NULL]
# lowercase measure
qof.ind[, measure := tolower(measure)]
# remove points
qof.ind <- qof.ind[!(measure %like% tolower("ACHIEVED_POINTS")), ]
# tag ccg, indicator group
qof.ind <- qof.ind[qof.orgref[, .(practice_code, ccg_code)], on = "practice_code"]
qof.ind <- qof.ind[qof.indmap[is.register == FALSE
                              , .(indicator_code, indicator_group_code)], on = "indicator_code"]
qof.ind <- qof.ind[!is.na(practice_code), ]

## prev - qof registers and list sizes
# Tag CCG

str(qof.prev)
# Classes ‘data.table’ and 'data.frame':	159999 obs. of  5 variables:
# $ practice_code       : chr  "A81001" "A81001" "A81001" "A81001" ...
# $ indicator_group_code: chr  "AF" "AST" "CAN" "CHD" ...
# $ register            : int  96 325 107 174 201 117 18 51 296 259 ...
# $ patient_list_type   : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...
# $ patient_list_size   : int  4247 4247 4247 4247 3408 4247 2291 4247 3408 3451 ...

# tag ccg
qof.prev <- qof.prev[qof.orgref[, .(practice_code, ccg_code)], on = "practice_code"]
qof.prev[, org.type := "ccg, practice"]
# practice age list sizes, convenience
qof.prev.praclists <- unique(qof.prev[, .(practice_code, patient_list_type, patient_list_size)])
qof.prev.praclists.tbl <- dcast(qof.prev.praclists, ... ~ patient_list_type, sum, value.var = "patient_list_size")
# spin down (register, patient_list_size) on measure
qof.prev.melt <- melt(qof.prev, measure.vars = c("register", "patient_list_size"),
                 variable.name = "measure", variable.factor = FALSE
                 ,value.name = "value")

##
## Inspect data coverage
##

lu.orgs.ccgs.local <- c("02Q", paste0("04", c("E", "H", "K", "L", "M", "N")))

# Inspect practice and CCG relations
# ? what are CCGs practices in dataset?
# ? does it agree with cdg.prac.calcs
# If not amend accordingly
cat('INFO: dataset: QOF, practices', '\n')
cat("INFO: practices in dataset that are not known to CDG lookup:", "\n")
tmp_pracsindata <- qof.orgref %>%
    filter(ccg_code %in% lu.orgs.ccgs.local) %>%
    select(practice_code, ccg_code)
rm(tmp_pracsindata)

##
## QOF indicators - measures
##

#
# To do: practice level prevalence, achievement / treatment
# To do: CDG aggregates too
# - and local CCGs
# - and England
#
# qofprev: (reg, agelist, value)
#
# achievem: (num, den, value)
# treat: (num, den + except, value)
# except: (ex, den + except, value)
# sub: (den - num, den + except)
# suborexcpet: (den - num + except, den + except)
#
# (practice_code, group, indicator, measure, num, den, value)

# Practice level

qof.ind[, org.type := "ccg, practice"]

# England

qof.ind.eng <- qof.ind[, .(value = sum(value)), .(indicator_group_code, indicator_code, measure)]
qof.ind.eng[, c("ccg_code", "practice_code", "org.type") := list("eng", "eng", "england")]

# Local CCGs

qof.ind.ccgs <- qof.ind[ccg_code %in% lu.orgs.ccgs.local
                        , .(value = sum(value))
                        , .(ccg_code, indicator_group_code, indicator_code, measure)]
qof.ind.ccgs[, c("practice_code", "org.type") := list("ccg", "ccg")]

## Combine local practice, local CCG, England
# England, local CCGs, local practices
qof.ind.combined <- list(
    qof.ind.eng
    , qof.ind.ccgs
    , qof.ind %>% filter(ccg_code %in% lu.orgs.ccgs.local)
    ) %>%
    rbindlist(use.names = TRUE)

## Calculate performance measures

lu_measures <- fread(strip.white = TRUE, input = "
m.type,      m.name,        m.stat,      i.num, i.den, i.exc
performance, achievement,   numerator,   1,     0,     0
performance, achievement,   denominator, 0,     1,     0
performance, treatment,     numerator,   1,     0,     0
performance, treatment,     denominator, 0,     1,     1
prevalence,  qofprevalence, numerator,   1,     0,     NA
prevalence,  qofprevalence, denominator, 0,     1,     NA
")

# spin up numerator, denominator, exceptions
# cross join measures and combine and remove intermediate columns
# Oops - need Larwood somehow but is not in QOF.  Filter out for now.
tmp <- qof.ind.combined %>% filter(!is.na(value)) %>%
    dcast(... ~ measure, sum, value.var = "value")
tmp2 <- setDT(merge(setDF(tmp), setDF(lu_measures[m.type == "performance"]))
              )[, value := sum(i.num * numerator, i.den * denominator, i.exc * exceptions)
                , .(indicator_code, org.type, ccg_code, practice_code, m.type, m.name, m.stat)
                ][, c("numerator", "denominator", "exceptions", "i.num", "i.den", "i.exc") := NULL]
# spin up m.numerator, m.denominator, ensure double and calculate m.value
tmp3 <- dcast(tmp2, ... ~ m.stat, sum, value.var = "value"
              )[, c("numerator", "denominator") := list(as.double(numerator), as.double(denominator))
                ][, value := 100 * numerator / denominator]
# melt down numerator, denominator and value on m.stat
tmp4 <- melt(tmp3, measure.vars = c("numerator", "denominator", "value")
             , variable.name = "m.stat", variable.factor = FALSE
             , value.name = "value")

qof.ind.combined <- tmp4
rm(tmp, tmp2, tmp3, tmp4)

if (bWriteCSV) {

    cat("qof: saving ...", "\n")

    fwrite(qof.ind.combined, "./Results/qof_ind__eng_ccg_prac__measure_ndv.csv")
}

#str(qof.ind.combined)
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


##
## now onto prevalence
##

# England, CCG, CDG, Practice level

#str(qof.prev.melt)
# Classes ‘data.table’ and 'data.frame':	319998 obs. of  6 variables:
#  $ practice_code       : chr  "B82007" "B82007" "B82007" "B82007" ...
#  $ indicator_group_code: chr  "AF" "AST" "CAN" "CHD" ...
#  $ patient_list_type   : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...
#  $ ccg_code            : chr  "02N" "02N" "02N" "02N" ...
#  $ measure             : chr  "register" "register" "register" "register" ...
#  $ value               : int  238 783 290 399 485 257 71 122 710 495 ...

# England
# spin down on measure, ignore missing values, tag as England

qof.prev.eng <- qof.prev.melt[
    , .(value = sum(value, na.rm = TRUE))
    , .(indicator_group_code, patient_list_type, measure)
    ][, c("ccg_code", "practice_code", "org.type") := list("eng", "eng", "england")]

# CCGs
# spin down on measure, ignore missing values, tag as ccg

qof.prev.ccgs <- qof.prev.melt[ccg_code %in% lu.orgs.ccgs.local
    , .(value = sum(value, na.rm = TRUE))
    , .(ccg_code, indicator_group_code, patient_list_type, measure)
    ][, c("practice_code", "org.type") := list("ccg", "ccg")]

## Combine local practice, local CDG, local CCG, England
# England, local CCGs, local CDGs, local practices

qof.prev.combined <- list(
    qof.prev.eng
    , qof.prev.ccgs
    , qof.prev.melt %>% filter(ccg_code %in% lu.orgs.ccgs.local)
    ) %>%
    rbindlist(use.names = TRUE)

## Calculate measures

# spin up numerator, denominator, exceptions
# Oops - need Larwood somehow but is not in QOF.  Filter out for now.
tmp <- qof.prev.combined %>% filter(!is.na(value)) %>%
    dcast(... ~ measure, sum, value.var = "value")
# cross join measures and combine
# NOTE: merge.data.table does not seem to do the cross join - use merge.data.frame
tmp2 <- setDT(merge(setDF(tmp), setDF(lu_measures[m.type == "prevalence"]))
              )[, value := sum(i.num * register, i.den * patient_list_size)
                , .(indicator_group_code, org.type, ccg_code, practice_code, m.type, m.name, m.stat)
                ][, c("register", "patient_list_size", "i.num", "i.den", "i.exc") := NULL]

# spin up m.numerator, m.denominator and calculate m.value, ensure numerator and denominator are double
tmp3 <- dcast(tmp2, ... ~ m.stat, sum, value.var = "value"
              )[, c("numerator", "denominator") := list(as.double(numerator), as.double(denominator))
                ][, value := 100 * numerator / denominator]
# melt down numerator, denominator and value on m.stat
tmp4 <- melt(tmp3
             , measure.vars = c("numerator", "denominator", "value")
             , variable.name = "m.stat", variable.factor = FALSE
             , value.name = "value")

qof.prev.combined <- tmp4
rm(tmp, tmp2, tmp3, tmp4)


if (bWriteCSV) {

    cat("qof: saving ...", "\n")

    fwrite(qof.prev.combined, "./Results/qof_prev__eng_ccg_prac__measure_ndv.csv")
}

cat("INFO: cdg_91_qof: Done.", "\n")

#qof.prev.combined <- fread("./Results/qof_prev__eng_ccg_cdg_prac__measure_ndv.csv")

##
# Add England comparator and signficance test ####
##

##

source("./Analysis/aphoci.R")

cat("INFO: cdg_10_compare: starting QOF...", "\n")

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

qof.prev.combined <- fread("./Results/qof_prev__eng_ccg_prac__measure_ndv.csv")

print(unique(qof.prev.combined[, org.type]))
# [1] "ccg, practice" "ccg"           ""england"

# All that is not England

qof.prev.var <- qof.prev.combined[(org.type != "england") & (m.stat %in% c('value', 'numerator', 'denominator'))
                                  , .(indicator_group_code
                                      , org.type, ccg_code, practice_code
                                      , m.stat, value)]
qof.prev.var.cast <- dcast(qof.prev.var, ... ~ m.stat, value.var = 'value')

# National reference

qof.prev.ref <- qof.prev.combined[(org.type == 'england') & (m.stat %in% c('value'))
                                  , .(indicator_group_code
                                      , org.type
                                      , m.stat, value)]
qof.prev.ref.cast <- dcast(qof.prev.ref, ... ~ m.stat, value.var = 'value')

tmp.nat <- merge(qof.prev.var.cast, qof.prev.ref.cast, by = c('indicator_group_code'), all.x = TRUE, suffixes = c('.var', '.ref'))

# ccg reference

qof.prev.ref <- qof.prev.combined[(org.type == 'ccg') & (m.stat %in% c('value'))
                                  , .(indicator_group_code
                                      , org.type, ccg_code
                                      , m.stat, value)]
qof.prev.ref.cast <- dcast(qof.prev.ref, ... ~ m.stat, value.var = 'value')

tmp.ccg <- merge(qof.prev.var.cast, qof.prev.ref.cast
                 , by = c('indicator_group_code', 'ccg_code')
                 , all.x = TRUE, suffixes = c('.var', '.ref'))

# combine

qof.prev.comp <- rbindlist(list(tmp.nat, tmp.ccg), use.names = TRUE)

qof.prev.comp[, c('cilo', 'cihi') := vaphoci_gen(numerator, denominator, multiplier = 100, ci.type = 'proportion')]
qof.prev.comp[, statsig := vtestci_s(value.ref, transpose(list(cilo, cihi)))]
qof.prev.comp[, c('cilo', 'cihi') :=  NULL]

# store

if (bWriteCSV)
    fwrite(qof.prev.comp, './Results/qof_prev__eng_ccg_prac__measure_ndv__comp_eng_ccg.csv')


#~ Indicators ####


# Melted on statistic.  Extract England, spin both up on m.stat, tag England,
# do stat. compare, remove uneeded columns, spin back down

qof.ind.combined <- fread("./Results/qof_ind__eng_ccg_prac__measure_ndv.csv")

print(unique(qof.ind.combined[, org.type]))
# [1] "ccg, practice" "ccg"           "england"
print(unique(qof.ind.combined[, m.name]))
# [1] "achievement" "treatment"

# All that is not England

qof.ind.var <- qof.ind.combined[(org.type != "england") & (m.stat %in% c('value', 'numerator', 'denominator'))
                                  , .(indicator_group_code, indicator_code
                                      , org.type, ccg_code, practice_code
                                      , m.name, m.stat, value)]
qof.ind.var.cast <- dcast(qof.ind.var, ... ~ m.stat, value.var = 'value')

# National reference

qof.ind.ref <- qof.ind.combined[(org.type == 'england') & (m.stat %in% c('value'))
                                  , .(indicator_group_code, indicator_code
                                      , org.type
                                      , m.name, m.stat, value)]
qof.ind.ref.cast <- dcast(qof.ind.ref, ... ~ m.stat, value.var = 'value')

tmp.nat <- merge(qof.ind.var.cast, qof.ind.ref.cast
                 , by = c('indicator_group_code', 'indicator_code'
                          , 'm.name')
                 , all.x = TRUE, suffixes = c('.var', '.ref'))

# ccg reference

qof.ind.ref <- qof.ind.combined[(org.type == 'ccg') & (m.stat %in% c('value'))
                                  , .(indicator_group_code, indicator_code
                                      , org.type, ccg_code
                                      , m.name, m.stat, value)]
qof.ind.ref.cast <- dcast(qof.ind.ref, ... ~ m.stat, value.var = 'value')

tmp.ccg <- merge(qof.ind.var.cast, qof.ind.ref.cast
                 , by = c('indicator_group_code', 'indicator_code'
                          , 'ccg_code'
                          , 'm.name'), all.x = TRUE, suffixes = c('.var', '.ref'))

# combine

qof.ind.comp <- rbindlist(list(tmp.nat, tmp.ccg), use.names = TRUE)

qof.ind.comp[, c('cilo', 'cihi') := vaphoci_gen(numerator, denominator, multiplier = 100, ci.type = 'proportion')]
qof.ind.comp[, statsig := vtestci_s(value.ref, transpose(list(cilo, cihi)), bSenseHigherisBetter = TRUE)]
qof.ind.comp[, c('cilo', 'cihi') :=  NULL]

# store

if (bWriteCSV) {
    fwrite(qof.ind.comp, './Results/qof_ind__eng_ccg_prac__measure_ndv__comp_eng_ccg.csv')
}

