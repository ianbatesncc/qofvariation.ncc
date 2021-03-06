# testci.R
#
# Routines to compare confidence intervals.
#

#' Family of testci routines
#'
#' Container for testci routines
#'
#' @family testci routines
#' @family sections
#'
#' @name testci
NULL

#' Family of testspc routines
#'
#' Container for testspc routines
#'
#' @family testspc routines
#' @family sections
#'
#' @name testspc
NULL

#DEBUG_TESTCI <- TRUE
DEBUG_TESTCI <- FALSE

#
# Generic test routine ####
#

#' Test if two confidence intervals overlap.
#'
#' Generic Version.
#'
#' @param ci.ref Reference point estimate or value range (vector(2))
#' @param ci.var Variable point estimate or value range (vector(2))
#' @param bAsString bool to specify string over numeric
#' @param bSenseHigherisBetter bool to determine any sense checking.  Default is
#'   NA - no sense check.
#'
#' @return Value indicating relative position of variable to reference value.
#'   Numeric by default, string if \code{bAsString == TRUE}
#'
#'   If the values within the range specified by \code{ci.var} are all lower
#'   than the values within the range specified by \code{ci.ref} then \code{var}
#'   is considered 'signficantly lower' than \code{ref} and the return value of
#'   \code{testci_hilo} is \code{-1}.
#'
#'   Conversely if the range of values specified by \code{ci.var} area all
#'   higher then \code{var} is considered 'significantly HIGHER' than \code{ref}
#'   and the return value is \code{1}.
#'
#'   if the range of values specified by \code{ci.ref} and \code{ci.var} overlap
#'   then \code{ref} and \code{var} are considered 'statistically SIMILAR' and
#'   the return value is \code{0}.
#'
#'   If there is no test done then the return value is \code{NA}.
#'
#'   * +2 - Better
#'   * +1 - Higher
#'   *  0 - Similar
#'   * -1 - Lower
#'   * -2 - Worse
#'   * NA - Not tested
#'
#' @note
#'
#'   If strings are returned they are from the set {"Worse", "Lower",
#'   "Similar", "Higher", "Better", "Not tested"}.
#'
#'   * if bHigherisBetter == NA (default) then 1 - Higher, -1 - Lower
#'
#'   * if bHigherisBetter == TRUE or FALSE then sense of value is considered and 2 -
#'   Better, -2 - Worse.
#'
#' @family testci routines
#' @family generic routines
#' @family returns hilo
#' @family returns sense
#' @family returns numeric
#' @family returns string
#'
#' @md
#' @export
#'
testci_gen <- function(
    ci.ref, ci.var
    , bAsString = FALSE, bSenseHigherisBetter = NA
    , return.type = "minimal" # "data.frame" "data.table"
) {

    if (DEBUG_TESTCI) {
        cat("DEBUG: testci_gen:", "ci.ref =", paste(ci.ref), "\n")
        cat("DEBUG: testci_gen:", "ci.var =", paste(ci.var), "\n")
        cat("DEBUG: testci_gen:", "bAsString =", bAsString, "\n")
        cat("DEBUG: testci_gen:", "bSenseHigherisBetter =", bSenseHigherisBetter, "\n")
    }

    # simple compare overlap
    hilo <- function(cilo.ref, cihi.ref, cilo.var, cihi.var) {
        # compare
        retval <- NA
        if (!any(is.na(c(cilo.ref, cihi.ref, cilo.var, cihi.var)))) {
            retval <- 0
            if (cilo.var > cihi.ref) {
                retval <- 1
            } else if (cihi.var < cilo.ref) {
                retval <- -1
            }
        }
        # done
        return(retval)
    }
    # simple apply sense
    apply_sense <- function(comp, bSenseHigherisBetter = NA) {
        retval <- comp
        # change sign and magnitude only if needed - premature optimisation
        if (!is.na(bSenseHigherisBetter) & !is.na(comp) & (comp != 0)) {
            mult <- 2
            if (bSenseHigherisBetter == FALSE)
                mult <- -mult
            retval <- comp * mult
        }
        return(retval)
    }
    # simple convert to string
    # comp can be single or vector
    as_string <- function(comp) {
        strs <- c("Worse", "Lower", "Similar", "Higher", "Better", "Not tested")
        # interpret comp as index into vector
        comp <- comp + 3
        comp[is.na(comp)] <- 6 # "Not tested"
        # done
        return(strs[comp])
    }

    # Compare

    # play with inputs - to get cilo, cihi for .ref and .var

    if (class(ci.ref) == "list") {
        if (length(ci.ref) > 2)
            ci.ref <- transpose(ci.ref)
        names(ci.ref)[1:2] <- c("cilo.ref", "cihi.ref")
    } else {
        ci.ref = list(cilo.ref = ci.ref, cihi.ref = ci.ref)
    }

    if (class(ci.var) == "list") {
        if (length(ci.var) > 2)
            ci.var <- transpose(ci.var)
        names(ci.var)[1:2] <- c("cilo.var", "cihi.var")
    } else {
        ci.var = list(cilo.var = ci.var, cihi.var = ci.var)
    }

    dat <- data.frame(
        ci.ref, ci.var
        , bAsString, bSenseHigherisBetter
        , stringsAsFactors = FALSE
    ) %>% mutate(
        comp = mapply(hilo, cilo.ref, cihi.ref, cilo.var, cihi.var)
        , comp_sense = mapply(apply_sense, comp, bSenseHigherisBetter)
        , comp_string = mapply(as_string, comp_sense)
    )

    if (!is.na(bAsString) & bAsString)
        retval <- dat$comp_string
    else if (!is.na(bSenseHigherisBetter))
        retval <- dat$comp_sense
    else
        retval <- dat$comp

    # return

    if (return.type == "data.table" & !is.installed("data.table"))
        return.type = "data.frame"

    return(switch(
        return.type
        , data.frame = dat
        , data.table = data.table::setDT(dat)
        , retval
    ))
}

#' Test variation with SPC methods
#'
#' special cause variation detection using SPC methods
#' Variable value is compared to control limits around a reference value
#'
#' @param sd sds to consider for limit.  2 corresponds to level 95.44997%, 3 to 99.73002%.
#'
#' @return c(-1, 0, 1)
#' @return c("Lower", "Similar", "Higher")
#'
#' @family testspc routines
#' @family generic routines
#' @family returns hilo
#' @family returns sense
#' @family returns numeric
#' @family returns string
#'
#' @md
#' @export
#'
testspc_gen <- function(
    value.var, value.ref
    , denominator.var, multiplier = 1
    , ci.type = "poisson", sd = 3
    , bAsString = FALSE
    , return.type = "minimal" # "data.frame" "data.table"
) {

    dat <- data.frame(
        value.var, value.ref, denominator.var
        , ci.type, sd
        #, bAsString
        , stringsAsFactors = FALSE) %>%
        mutate(
            numerator = denominator.var * value.ref / multiplier
            , level.spc = 2 * pnorm(sd) - 1
        ) %>%
        mutate(ci.ref = aphoci_gen(
            numerator, denominator.var, multiplier, level.spc, ci.type
        )) %>%
        mutate(comp = testci_gen(ci.ref, value.var, bAsString = bAsString))

    # return

    if (return.type == "data.table" & !is.installed("data.table"))
        return.type = "data.frame"

    return(switch(
        return.type
        , data.frame = dat
        , data.table = data.table::setDT(dat)
        , dat$comp
    ))
}

#' Tranpose a list of vectors
#'
#' Wrapper around transpose routines.  data.table or purrr.  No 'native' yet.
#'
transpose <- function(l) {
    #ftranspose <- purrr::transpose
    ftranspose <- data.table::transpose

    ftranspose(l)
}

#' check if package is installed
#'
#'
is.installed <- function(p) {
    is.element(p, utils::installed.packages()[, 1])
}

#' Test routine for testspc_gen
#'
#'
testing__spc <- function() {
    require("dplyr")

    n <- 16
    xn <- runif(n, 10, 90)
    yn <- xn + runif(n, 100, 900)
    multiplier = 1000
    ci.type = "poisson"
    level = 0.95


    cat("INFO: setting up data frame ...", "\n")

    dat <- data.frame(
        num = xn, den = yn, multiplier, ci.type, level
        , stringsAsFactors = FALSE
    )

    cat("INFO: aphoci_gen OUTSIDE data frame ...", "\n")

    ci <- aphoci_gen(xn, yn, multiplier, level, ci.type)
    cit <- aphoci_gen(xn, yn, multiplier, level, ci.type, bTransposeResults = TRUE)

    cat("INFO: aphoci_gen WITHIN data frame ...", "\n")

    calc1 <- dat %>% mutate(ci.var = aphoci_gen(num, den, multiplier, level, ci.type))

    value.var = xn * multiplier / yn
    value.ref = median(value.var)
    sd = 3

    cat("INFO: aphoci_gen WITHIN data frame ...", "\n")

    calc2 <- calc1 %>% mutate(
        value.var = value.var
        , value.ref = value.ref
        , ci.ref = aphoci_gen(value.ref * den / multiplier, den, multiplier, level, ci.type)
    )

    cat("INFO: testci_gen OUTSIDE data frame ... (minimal)", "\n")

    retval_minimal <- testci_gen(calc2$ci.ref, calc2$value.var)

    cat("INFO: testci_gen OUTSIDE data frame ... (data.frame)", "\n")

    retval_data.frame <- testci_gen(
        calc2$ci.ref, calc2$value.var, return.type = "data.frame"
    )

    cat("INFO: testci_gen OUTSIDE data frame ... (data.table)", "\n")

    retval_data.table <- testci_gen(
        calc2$ci.ref, calc2$value.var, return.type = "data.table"
    )

    cat("INFO: testci_gen WITHIN data frame ... (minimal)", "\n")

    calc3 <- calc2 %>%
        mutate(retval_comp = testci_gen(ci.ref, value.var))

    cat("INFO: testspc_gen OUTSIDE data frame ... ", "\n")

    retval <- testspc_gen(value.var, value.ref, yn, multiplier, ci.type, sd)
    retval.df <- testspc_gen(
        value.var, value.ref, yn, multiplier, ci.type, sd
        , return.type = "data.frame"
    )
    retval.str.df <- testspc_gen(
        value.var, value.ref, yn, multiplier, ci.type, sd
        , bAsString = TRUE, return.type = "data.frame"
    )

    cat("INFO: testspc_gen WITHIN data frame ... ", "\n")

    calc4 <- calc3 %>%
        mutate(comp.spc = testspc_gen(value.var, value.ref, yn, multiplier, ci.type, sd))

    calc4.string <- calc3 %>%
        mutate(comp.spc = testspc_gen(value.var, value.ref, yn, multiplier, ci.type, sd, bAsString = TRUE))

    return(list(
        calc4 = calc4
        , calc4.string = calc4.string
        , retval = retval
        , retval.df = retval.df
        , retval.str.df = retval.str.df
    ))
}

#
# Instances of the generic routine ####
#

#' Return simple hilo comparison
#'
#' @inheritParams testci_gen
#'
#' @family testci routines
#' @family returns hilo
#' @family returns numeric
#'
#' @export
testci_hilo <- function(ci.ref, ci.var) {
    return(testci_gen(ci.ref, ci.var))
}

#' Return simple hilo comparison
#'
#' @inheritParams testspc_gen
#'
#' @family testspc routines
#' @family returns hilo
#' @family returns numeric
#'
#' @export
testspc_hilo <- function(
    value.var, value.ref
    , denominator.var, multiplier = 1
    , ci.type = "poisson", sd = 3
    , return.type = "minimal" # "data.frame" "data.table"
) {
    testspc_gen(value.var, value.ref, denominator.var, multiplier, ci.type, sd, bAsString = FALSE, return.type)
}

#' Apply sense (Better / Worse) to testci comparison
#'
#' @inheritParams testci_gen
#'
#' @return Integer indicating relative position of variable to reference value.
#'
#' * +2 - Better
#' *  0 - Similar
#' * -2 - Worse
#' * NA - Not tested
#'
#' @family testci routines
#' @family instances of generic routines
#' @family returns sense
#' @family returns numeric
#'
#' @md
#' @export
#'
testci_sense <- function(ci.ref, ci.var, bSenseHigherisBetter = NA) {
    return(testci_gen(ci.ref, ci.var, bSenseHigherisBetter = bSenseHigherisBetter))
}

#' Return a string to denote higher/lower/similar/not tested
#'
#' @inheritParams testci_hilo
#'
#' @family testci routines
#' @family instances of generic routines
#' @family returns hilo
#' @family returns string
#'
#' @export
#'
testci_hilo_s <- function(ci.ref, ci.var) {
    return(testci_gen(ci.ref, ci.var, bAsString = TRUE))
}

#' Return a string to denote higher/lower/similar/not tested
#'
#' @inheritParams testspc_hilo
#'
#' @family testspc routines
#' @family instances of generic routines
#' @family returns hilo
#' @family returns string
#'
#' @export
#'
testspc_hilo_s <- function(
    value.var, value.ref
    , denominator.var, multiplier = 1
    , ci.type = "poisson", sd = 3
    , return.type = "minimal" # "data.frame" "data.table"
) {
    testspc_gen(value.var, value.ref, denominator.var, multiplier, ci.type, sd, bAsString = TRUE, return.type)
}

#' Return a string to denote better/worse/similar/not tested
#'
#' @inherit testci_sense
#'
#' @family testci routines
#' @family instances of generic routines
#' @family returns sense
#' @family returns string
#'
#' @export
#'
testci_sense_s <- function(ci.ref, ci.var, bSenseHigherisBetter = NA) {
    return(testci_gen(ci.ref, ci.var, bAsString = TRUE, bSenseHigherisBetter = bSenseHigherisBetter))
}

#
# Integration ####
#

#' Take a list of ci.ref's and ci.var's and mapply() them
#'
#' Instance that considers high and low and returns an integer
#'
#' @inherit testci_hilo
#'
#' @family testci routines
#' @family instances of generic routines
#' @family returns hilo
#' @family returns numeric
#'
#' @export
#'
#' @templateVar fun vtestci_hilo
#' @template template-depr_fun
NULL

#' @templateVar old vtestci_hilo
#' @templateVar new testci_gen
#' @template template-depr_pkg
#'
#' @export
vtestci_hilo <- function(ci.ref, ci.var) {
    .Deprecated("testci_gen", "aphoci")
    return(testci_gen(ci.ref, ci.var))
}

#' Take a list of ci.ref's and ci.var's and mapply() them
#'
#' Instance to considers sense and returns an integer.
#'
#' @inherit testci_sense
#'
#' @family testci routines
#' @family instances of generic routines
#' @family returns sense
#' @family returns numeric
#'
# #' @family deprecated
#'
#' @export
#'
#' @templateVar fun vtestci_sense
#' @template template-depr_fun
NULL

#' @templateVar old vtestci_sense
#' @templateVar new testci_gen
#' @template template-depr_pkg
#'
#' @export
vtestci_sense <- function(ci.ref, ci.var, bSenseHigherisBetter = NA) {
    .Deprecated("testci_gen", "aphoci")
    return(testci_gen(ci.ref, ci.var, bSenseHigherisBetter = bSenseHigherisBetter))
}

#' Take a list of ci.ref's and ci.var's and mapply() them
#'
#' Instance that considers high and low and returns a string.
#'
#' @inherit testci_hilo
#'
#' @family testci routines
#' @family instances of generic routines
#' @family returns hilo
#' @family returns string
#'
# #' @family deprecated
#'
#' @export
#'
#' @templateVar fun vtestci_hilo_s
#' @template template-depr_fun
NULL

#' @templateVar old vtestci_hilo_s
#' @templateVar new testci_gen
#' @template template-depr_pkg
#'
#' @export
vtestci_hilo_s <- function(ci.ref, ci.var) {
    .Deprecated("testci_gen", "aphoci")
    return(testci_gen(ci.ref, ci.var, bAsString = TRUE))
}

#' Take a list of ci.ref's and ci.var's and mapply() them
#'
#' Instance that considers sense and returns a string.
#'
#' @inherit testci_sense
#'
#' @family testci routines
#' @family instances of generic routines
#' @family returns sense
#' @family returns string
#'
# #' @family deprecated
#'
#' @export
#'
#' @templateVar fun vtestci_sense_s
#' @template template-depr_fun
NULL

#' @templateVar old vtestci_sense_s
#' @templateVar new testci_gen
#' @template template-depr_pkg
#'
#' @export
vtestci_sense_s <- function(ci.ref, ci.var, bSenseHigherisBetter = NA) {
    .Deprecated("testci_gen", "aphoci")
    return(testci_gen(ci.ref, ci.var, bAsString = TRUE, bSenseHigherisBetter = bSenseHigherisBetter))
}
