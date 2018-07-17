##
## aphoci.R
##
## aphoci style confidence intervals and tests
## made easy with R
##
## Output Functions
#
# aphoci_rate(num, den, mult, level)
# aphoci_prop(num, den, mult, level)
#
# testci(ci.ref, ci.var, bSenseHigherisBetter, iVerbose)
# testci_s(ci.ref, ci.var, bSenseHigherisBetter, iVerbose)
#

# vi.ind.combined[
#     , c("cilo", "cihi") := as.list(aphoci_prop(numerator, denominator))
#     , .(ccg.code, gp.practice.code, age, intervention)]
# vi.ind.combined[, citype := "proportion"]

## General ####

## Round
# round to nearest whole number
#
round.nearest <- function(x, digits = 0) {
    d = 10^(-digits)
    m = 10^(digits)
    return((floor(x * m) + ((x * m) - floor(x * m) >= 0.5)) * d)
}

## APHO CI ####

## APHO CI routines - but in R
# Need to protect against some corner cases
## prop.test
# (x, 0) (NA, x) (x, NA) generates an error
# (0,  >0..<10) generates a warning
## poisson.test
# (0, 0) ci : (NaN, Inf)
# (x, 0) ci : (Inf, Inf)
# (NA, x) (x, NA) generates an error

aphoci_gen <- function(num, den, multiplier = 1, level = 0.95, ci.type = 'poisson') {
    ci <- c(NA, NA)

    if (!is.na(ci.type)) {
        f <- poisson.test
        if (ci.type == 'proportion') {
            f <- prop.test
        } else if (ci.type == 'binomial') {
            f <- binom.test
        }

        #cat('DEBUG: aphoci_gen: (num, den) = (', num, ', ', den, ')', "\n")

        if (!any(is.na(c(num, den))) & (den > 0) & (num >= 0)) {
            if (ci.type == 'poisson')
                num <- round.nearest(num)
            a <- f(num, den, conf.level = level)
            ci <- a[['conf.int']][1:2] * multiplier
        }
    }

    return(ci)
}

aphoci_rate <- function(num, den, multiplier = 1, level = 0.95) {
    return(aphoci_gen(num, den, multiplier, level, 'poisson'))
}

aphoci_prop <- function(num, den, multiplier = 1, level = 0.95) {
    return(aphoci_gen(num, den, multiplier, level, 'proportion'))
}

## Test overlapping CIs ####
#
# Test if two CI's overlap.  High or lower coded as 1's and -1's.
# Can change meaning to Better (1) or worse (-1) if specified by flag.
#
# (Sig higher, higher, similar, lower, stat. sig.lower) -> (-1, 0, +1)
# testci(c(0.5, 0.7), c(0.4, 0.6))
# Can set either cicmop or civar to NA to 'not test'
# Returns
# +1 - Higher | Better
#  0 - Similar
# -1 - Lower | Worse
# NA - not tested
#
# Difficult to check for sensible input - lists, vectors, ...
# Will assume ci to be either NA, double or vector of 2 or more (in which case
# only first two elements are used)
#
testci <- function(ci.ref, ci.var, bSenseHigherisBetter = NA, iVerbose = 0) {
    comp <- NA

    # Sort first two elements of vector in ascending order (cilo, cihi) : cilo <= cihi
    ci.ref <- sort(ci.ref[1:2])
    ci.var <- sort(ci.var[1:2])

    if (length(ci.ref) == 1) ci.ref <- rep(ci.ref, 2)
    if (length(ci.var) == 1) ci.var <- rep(ci.var, 2)

    #cat("DEBUG: testci: (ci.ref, ci.var) = ", ci.ref, ", ", ci.var, ")", "\n")

    if (any(is.na(c(ci.ref, ci.var)), length(ci.ref) == 0, length(ci.var) == 0)) {
        if (iVerbose > 0)
            cat("WARNING: testci: NA's detected - not testing.", "\n")
        return(comp)
    }

    # If variable cilo is greater than reference cihi then sig.hi
    # If variable cihi is less than reference cilo then sig.lo
    # else similar
    comp <- 0
    if (ci.var[1] > ci.ref[2]) {
        if (iVerbose > 1) cat('DEBUG: testci: sig.hi value detected: ASSERT(var.cilo > ref.cihi) = (', ci.var[1], ' > ', ci.ref[2], ')', '\n')
        comp <- 1
    } else if (ci.var[2] < ci.ref[2]) {
        if (iVerbose > 1) cat('DEBUG: testci: sig.lo value detected: ASSERT(var.cihi < ref.cilo) = (', ci.var[2], ' < ', ci.ref[1], ')', '\n')
        comp <- -1
    }

    if (!is.na(bSenseHigherisBetter) & (!bSenseHigherisBetter))
        comp <- -sign(comp)

    if (iVerbose > 1) cat("DEBUG: testci: (ci.ref, ci.var, comp) = ", ci.ref, ", ", ci.var, ', ', comp, ")", "\n")

    return(comp)
}

# test a measure against a reference value given instance num, den, mult and reference value
# use when CI needs to be calcualted but value not needed
testci_ndr_gen <- function(num, den, multiplier
                           , ref.value
                           , ci.type = "poisson", level = 0.95
                           , bSenseHigherisBetter = NA) {
    ci.var <- aphoci_gen(num, den, multiplier, level, ci.type)

    return(testci(ci.var, ref.value, bSenseHigherisBetter))
}

testci_ndr_poisson <- function(num, den, multiplier, ref.value, level = 0.95, bSenseHigherisBetter = NA) {
    return(testci_ndr_gen(num, den, multiplier, ref.value, level, ci.type = "poisson", bSenseHigherisBetter))
}

testci_ndr_proportion <- function(num, den, multiplier, ref.value, level = 0.95, bSenseHigherisBetter = NA) {
    return(testci_ndr_gen(num, den, multiplier, ref.value, level, ci.type = "proportion", bSenseHigherisBetter))
}

## Convert a return value from testci into a string.
# Not intended to be called directly
#
# (NA, 0) interpreted as (Not tested, Similar)
# (1, -1) as (Higher, Lower) if bSenseHigher... is NA
# (1, -1) as (Better, Worse) if FALSE
# (1, -1) as (Worse, Better) if TRUE
#
# Some optimisations: choose most likely cases at top
# 1. comp == 0
# 2. No sense set
# 3. Sense set
#
comp_s <- function(comp, bSenseHigherisBetter = NA) {

    retval <- "Not tested"

    if (!is.na(comp)) {
        if (comp == 0) {
            retval <- "Similar"
        } else {
            if (is.na(bSenseHigherisBetter)) {
                retval <- c("Lower", NA, "Higher")[comp + 2]
            } else {
                if (bSenseHigherisBetter == FALSE)
                    comp = -comp
                retval <- c("Worse", NA, "Better")[comp + 2]
            }
        }
    }

    return(retval)
}

# test confidence intervals, return a string to denote higher/lower or better/worse
# use testci to do simple hi/lo then do sense check if needed.
#
testci_s <- function(ci.ref, ci.var, bSenseHigherisBetter = NA) {
    return(comp_s(testci(ci.ref, ci.var), bSenseHigherisBetter))
}

##
## Vectorisation ####
##
## Functions work for individual cases.  Different story when integrating with
# e.g data.table.  Need wrappers to 'vectorise'.

# Considers vectors of num, den and calculates ci's based on (ni, di) -> ci
# prop.test, poisson.test not 'vectorised' - must be an efficient work around
# this is the best I've arrived at without fully vectorising x.test
vaphoci_gen <- function(num.v, den.v, multiplier = 1, level = 0.95, ci.type = 'poisson') {
    ret <- mapply(aphoci_gen, num.v, den.v, multiplier, level, ci.type)
    # ret is length-1 list of ci length-2 vectors
    # retval is a length-2 list of ci length-1 values
    retval <- list((ret[1, ]), (ret[2, ]))
    return(retval)
}

# take a list of ci.ref's and ci.var's and mapply() them
vtestci <- function(ci.ref.l, ci.var.l, bSenseHigherisBetter = NA) {
    return(mapply(testci, ci.ref.l, ci.var.l, bSenseHigherisBetter))
}

# take a list of ci.ref's and ci.var's and mapply() them
vtestci_s <- function(ci.ref.l, ci.var.l, bSenseHigherisBetter = NA) {
    return(mapply(testci_s, ci.ref.l, ci.var.l, bSenseHigherisBetter))
}
