##
## cdg_10_compare.R
##
## Add Comparator and statistical significance tests for QOF and LocalHealth datasets.
##

#
## Output Files
# ./Results/qof_prev__eng_ccg_cdg_prac__measure_ndv__comp_eng_ccg.csv
# ./Results/qof_ind__eng_ccg_cdg_prac__measure_ndv__comp_eng_ccg.csv
# ./Results/vi_ind__eng_ccg_cdg_prac__measure_ndv__comp_rag.csv
# ./Results/LH_PH_CCG_CDG__comp_ccg_eng.csv
#
## Output Variables
#
## Input Files
# ./Results/qof_prev__eng_ccg_cdg_prac__measure_ndv.csv
#
## Depends
# ./Analysis/aphoci.R
# ./Analysis/cdg_00_geo_lsoa.R
#

# source("/Users/bates/Work/Txx CDG Profiles/Analysis/cdg_10_compare.R")
# source("/Users/bates/Work/Txx CDG Profiles/Analysis/cdg_10_compare.R", echo = TRUE)

cat("INFO: cdg_10_compare: starting...", "\n")

#
# Set environment and config ####
#

taskdir <- "/Users/bates/Work/Txx CDG Profiles"
#taskdir <- "C:/Users/Public/Documents/Public Work UNRESTRICTED/Txx CDG Profiles"
datadir <- "./Data"
analysisdir <- "./Analysis"
resultsdir <- "./Results"
mappingdir <- "./Mapping"
setwd(taskdir)

library("data.table") # includes melt, cast (can be used for data.tables too)


# Loading relations ####

source('./Analysis/cdg_00_geo_lsoa.R')
# lu.orgs.ccgs.gss.local

source("./Analysis/aphoci.R")
# aphoci_rate, prop, gen
# testci
# testci_s

# Config ####

bWriteCSV <- FALSE
bWriteCSV <- TRUE

# QOF ####

cat("INFO: cdg_10_compare: starting QOF...", "\n")

##
## QOF
##
# Tag prevalence at practice level with England
# Tag achievement at CDG level with England
# [optional] tag Treatment at CDG level with England.
#

## _ Prevalence ####

# Melted on statistic.  Extract England, spin both up on m.stat, tag England,
# do stat. compare, remove uneeded columns, spin back down

qof.prev.combined <- fread("./Results/qof_prev__eng_ccg_cdg_prac__measure_ndv.csv")

print(unique(qof.prev.combined[, org.type]))
# [1] "ccg, practice" "ccg"           "ccg, cdg"      "england"

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
    fwrite(qof.prev.comp, './Results/qof_prev__eng_ccg_cdg_prac__measure_ndv__comp_eng_ccg.csv')


## _ Indicators ####


# Melted on statistic.  Extract England, spin both up on m.stat, tag England,
# do stat. compare, remove uneeded columns, spin back down

qof.ind.combined <- fread("./Results/qof_ind__eng_ccg_cdg_prac__measure_ndv.csv")

print(unique(qof.ind.combined[, org.type]))
# [1] "ccg, practice" "ccg"           "ccg, cdg"      "england"
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

if (bWriteCSV)
    fwrite(qof.ind.comp, './Results/qof_ind__eng_ccg_cdg_prac__measure_ndv__comp_eng_ccg.csv')

# Vaccs Imms ####
##

cat("INFO: cdg_10_compare: starting VaccsImms...s", "\n")

##
## VaccsImms
##
# At practice level, RAG rate at 90% or more
#
# Cast up on a few sat. variable, tag statsig.

vi.ind.rag <- fread('./Results/vi_ind__eng_ccg_cdg_prac__measure_ndv.csv')

# unique(vi.ind.combined[, org.type])
#[1] "england"      "ccg"          "ccg_cdg"      "ccg_practice" "cdg_practice"
# > unique(vi.ind.combined[, .(intervention, age)])
#          intervention        age
#  1:      dtap.ipv.hib X12.months
#  2:             men.c X12.months
#  3:               pcv X12.months
#  4:             hep.b X12.months
#  5:      dtap.ipv.hib X24.months
#  6:               mmr X24.months
#  7:      infant.men.c X24.months
#  8:     men.c.booster X24.months
#  9:       pcv.booster X24.months
# 10:             hep.b X24.months
# 11:    dt.pol.primary   X5.years
# 12:  dtap.ipv.booster   X5.years
# 13: pertussis.primary   X5.years
# 14:        infant.hib   X5.years
# 15:      infant.men.c   X5.years
# 16: hib.men.c.booster   X5.years
# 17:        mmr.dose.1   X5.years
# 18:        mmr.dose.2   X5.years
# 19:        infant.pcv   X5.years
# 20:       pcv.booster   X5.years

vi.ind.rag[, coverage.band := as.character(
    cut(100 * value, breaks = c(0, 90, 95, 100), right = FALSE, include.lowest = TRUE))]

# store

if (bWriteCSV)
    fwrite(vi.ind.rag, './Results/vi_ind__eng_ccg_cdg_prac__measure_ndv__comp_rag.csv')


# LocalHealth ####

cat("INFO: cdg_10_compare: starting LocalHealth...", "\n")

##
## Localhealth
##
# Tag CDG level with England value and stat.comp
# Need to define stat. method (rate, proportion) and sense (Higher is Better)
#

# Meta data.  Strip out the non-indicators.
lh.meta <- fread(file = "./Data/LocalhealthSpreadsheets20170307FilesMeta.csv")
lh.meta <- lh.meta[!is.na(number)]

# Load cdg results.  Load national result but filter for just those orgs. needed
# (ENG, local CCGs).  Combined with local CDG results
lh.data.cdg <- fread(file = './Results/PH_LH_CCG_CDG_melt.csv')

lh.data <- fread(file = './Results/PH_LH_ENG_CTY_LAUA_CCG_melt.csv')
lh.data <- lh.data %>%
    filter(geotype == 'ENG' |
               ((geotype == 'CCG') & (area.code %in% lu.orgs.ccgs.gss.local)) |
               (area.code %in% c('E10000024', 'E06000018'))) %>%
    setDT()

# Change the GSS CCG codes to NHS codes
lh.data[geotype == 'CCG', area.code := geo.ccg.gss.local[CCG16CD == area.code, CCG16CDH], by = area.code]

# combine PHE LH and calculations
lh.data.combined <- list(
    lh.data %>%
        mutate(s.wk = NA) %>%
        select(domain, number, indicator.title
               , geotype, area.code1 = area.code, area.code2 = area.name
               , s.wk, statistic, value)
    , lh.data.cdg %>%
        mutate(geotype = 'ccg_cdg') %>%
        select(domain, number, indicator.title
               , geotype, area.code1 = TARGETL1CD, area.code2 = TARGETL2CD
               , s.wk, statistic, value)) %>%
    rbindlist(use.names = TRUE)

#setkey(lh.data.combined, domain, number, geotype, area.code1, area.code2)

## put (observed, numerator) and (expected, denominator) into (num, den)
# - set NA to zero, sum, revert to NA if needed
# spin up

lh.data.cast <- lh.data.combined %>%
    dcast(... ~ statistic)

lh.data.cast[, c('num', 'den') := as.numeric(NA)]
#lh.data.tmp[, c('num', 'den') := NULL]
# lh.data.tmp[!is.na(numerator) & !is.na(observed), ]
# - empty
# lh.data.tmp[!is.na(denominator) & !is.na(expected), ]
# - empty
lh.data.cast[!is.na(observed),    num := observed]
lh.data.cast[!is.na(numerator),   num := numerator]
lh.data.cast[!is.na(expected),    den := expected]
lh.data.cast[!is.na(denominator), den := denominator]

lh.data.cast[, c('numerator', 'observed', 'denominator', 'expected') := NULL]

lh.data.eng <- lh.data.cast %>% filter(geotype == 'ENG')
lh.data.ccg <- lh.data.cast %>% filter(geotype == 'CCG')

# unique(lh.data.cast[, .(geotype, area.code1, area.code2)])
#     geotype area.code1                        area.code2
#  1:     CCG        02Q                 NHS Bassetlaw CCG
#  2:     CCG        04E    NHS Mansfield and Ashfield CCG
#  3:     CCG        04H         NHS Newark & Sherwood CCG
#  4:     CCG        04K           NHS Nottingham City CCG
#  5:     CCG        04L NHS Nottingham North and East CCG
#  6:     CCG        04M           NHS Nottingham West CCG
#  7:     CCG        04N                NHS Rushcliffe CCG
#  8:     CTY  E10000024                Nottinghamshire CC
#  9:     ENG  E92000001                           England
# 10:    LAUA  E06000018                     Nottingham UA
# 11: ccg_cdg        04L                               LC1
# 12: ccg_cdg        04L                               LC2
# 13: ccg_cdg        04L                               LC3
# 14: ccg_cdg        04M                               BEE
# 15: ccg_cdg        04M                               EWK
# 16: ccg_cdg        04M                               STB
# 17: ccg_cdg        04N                               CEN
# 18: ccg_cdg        04N                               NTH
# 19: ccg_cdg        04N                               STH

# Tag on stat.method and bsense AND multiplier, tag on ENG value (and CCG for the heck of it)
# do stat compare, done - maybe spin down again.

tmp2 <- lh.data.cast %>%
    merge(lh.meta %>% select(domain, number, indicator.title, multiplier
                             , ci.type = stat.test, bsensehigherisbetter)
          , by = c('domain', 'number', 'indicator.title')
          , all.x = TRUE)

# Presume all indicators have ENG data - but set 'all.x' TRUE in any case
tmp3 <- tmp2 %>%
    merge(lh.data.eng %>%
              select(domain, number, indicator.title
                     , geotype.eng = geotype
                     , value.eng = indicator.value)
          , by = c('domain', 'number', 'indicator.title')
          , all.x = TRUE)

# Not all indicators have CCG data - so set 'all.x' TRUE
tmp4 <- tmp3 %>%
    merge(lh.data.ccg %>%
              select(domain, number, indicator.title
                     , geotype.ccg = geotype, area.code1
                     , value.ccg = indicator.value)
          , by = c('domain', 'number', 'indicator.title', 'area.code1')
          , all.x = TRUE)

# Use published (lci, uci) when available i.e. for ENG, CCG, LAUA, CTY
geotype.pub <- c('ENG', 'CTY', 'LAUA', 'MSOA', 'EW', 'CCG')
tmp4[(geotype %in% geotype.pub) & (!is.na(lci + uci)), c('cilo', 'cihi') := list(lci, uci)]
# Attempt to calculate (cilo, cihi) for all remaining indicators
tmp4[(!(geotype %in% geotype.pub)) & (is.na(cilo + cihi)), c('cilo', 'cihi') := vaphoci_gen(num, den, multiplier, 0.95, ci.type)]
# Substitute weighted (lci, uci) where (cilo, cihi) cannot be calculated due to
# no numerator or denominator
# - effects mortality indicators - so CI testing more indicative than statistical
tmp4[(is.na(cilo + cihi) & !(is.na(lci + uci))), c('cilo', 'cihi') := list(lci, uci)]
# round values to one decimal place when comparing - as ENG value is rounded to 1 D.P. ...
tmp4[, statsig.eng := vtestci_s(round.nearest(value.eng, 1)
                                , transpose(list(round.nearest(cilo, 1)
                                                 , round.nearest(cihi, 1)))
                                , bsensehigherisbetter)]
tmp4[, statsig.ccg := vtestci_s(round.nearest(value.ccg, 1)
                                , transpose(list(round.nearest(cilo, 1)
                                                 , round.nearest(cihi, 1)))
                                , bsensehigherisbetter)]
tmp4[, c('cilo', 'cihi') :=  NULL]

if (bWriteCSV)
    fwrite(tmp4, './Results/LH_PH_CCG_CDG__comp_ccg_eng.csv')


# Done. ####

cat("INFO: cdg_10_compare: Done.", "\n")

