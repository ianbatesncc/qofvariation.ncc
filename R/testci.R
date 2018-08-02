# testci.R
#
# Routines to compare confidence intervals.
#

#' Family of testci routines
#'
#' Container for testci routines
#'
#' @family testci routines
#' @family confidence interval routines
#'
#' @name testci
NULL

#' Family of testspc routines
#'
#' Container for testspc routines
#'
#' @family testspc routines
#' @family confidence interval routines
#'
#' @name testspc
NULL

#
# Generic test routine ####
#

#' Test if two confidence intervals overlap.
#'
#' Generic Version.
#'
#' @inheritParams aphoci_gen
#'
#' @param ci.ref Reference point estimate or value range (vector(2))
#' @param ci.var Variable point estimate or value range (vector(2))
#' @param bAsString bool to specify string over numeric
#' @param bSenseHigherisBetter bool to determine any sense checking.  Default is
#'   NA - no sense check.
# @param return_type Specify list of vectors (default) or data.frame or
#   data.table.
#' @param verbose set to TRUE to print some debug output.
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
# @family confidence interval routines
#' @family testci routines
#' @family generic routines
#' @family returns hilo
#' @family returns sense
#' @family returns numeric
#' @family returns string
#'
#' @md
# @export
#'
testci_gen <- function(
    ci.ref, ci.var
    , bAsString = FALSE, bSenseHigherisBetter = NA
    , return_type = c("minimal", "data.frame", "data.table")
    , verbose = FALSE
) {

    return_type <- match.arg(return_type)

    if (verbose) {
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

    if (return_type == "data.table" & !is_installed("data.table"))
        return_type = "data.frame"

    return(switch(
        return_type
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
#' @inheritParams testci_gen
#' @inheritParams aphoci_gen
#'
#' @param value.var variable value
#' @param value.ref reference value
#' @param denominator.var variable denominator
#' @param sd sds to consider for limit.  2 corresponds to level 95.44997\%, 3 to
#'   99.73002\%.
#'
#' @return c(-1, 0, 1)
#' @return c("Lower", "Similar", "Higher")
#'
# @family confidence interval routines
#' @family testspc routines
#' @family generic routines
#' @family returns hilo
#' @family returns sense
#' @family returns numeric
#' @family returns string
#'
#' @importFrom stats pnorm
#'
#' @md
# @export
#'
testspc_gen <- function(
    value.var, value.ref
    , denominator.var, multiplier = 1
    , ci_type = "poisson", sd = 3
    , bAsString = FALSE
    , return_type = c("minimal", "data.frame", "data.table")
) {

    return_type <- match.arg(return_type)

    dat <- data.frame(
        value.var, value.ref, denominator.var
        , ci_type, sd
        #, bAsString
        , stringsAsFactors = FALSE) %>%
        mutate(
            numerator = denominator.var * value.ref / multiplier
            , level.spc = 2 * stats::pnorm(sd) - 1
        ) %>%
        mutate(ci.ref = aphoci_gen(
            numerator, denominator.var, multiplier, level.spc, ci_type
        )) %>%
        mutate(comp = testci_gen(ci.ref, value.var, bAsString = bAsString))

    # return

    if (return_type == "data.table" & !is_installed("data.table"))
        return_type = "data.frame"

    return(switch(
        return_type
        , data.frame = dat
        , data.table = data.table::setDT(dat)
        , dat$comp
    ))
}

#' Tranpose a list of vectors
#'
#' Takes a list of length m of vectors length n and returns a list of length n
#' of vectors length m
#'
#' Native transpose of list of vectors.  Was a wrapper around transpose
#' routines.  data.table or purrr.
#'
#' @param l list of length m of vectors length n
#' @param method method to use.  Currently ignored.  Default "native".
#'
#' @return list of length n of vectors length m
#'
# @importFrom data.table transpose
# @importFrom purrr transpose
#'
#' @family Helper routines
#'
transpose <- function(l, method = c("native", "data.table", "purrr")) {
    #method <- match.arg(method)

    #ftranspose <- purrr::transpose
    #ftranspose <- data.table::transpose
    ftranspose <- function(x) {
        stopifnot(class(x) =="list")
        x <- sapply(x, unlist)
        lapply(seq_len(dim(x)[1]), function(i, y){y[i, ]}, x)
    }

    ftranspose(l)
}

#' check if package is installed
#'
#' @param p name of package (character)
#'
#' @importFrom utils installed.packages
#'
#' @family Helper routines
#'
is_installed <- function(p) {
    is.element(p, utils::installed.packages()[, 1])
}

#
# Instances of the generic routine ####
#

#' @describeIn testci_gen
#' Return simple hilo comparison
#'
#' @inheritParams testci_gen
#'
# @family confidence interval routines
#' @family testci routines
#' @family returns hilo
#' @family returns numeric
#'
# @export
testci_hilo <- function(ci.ref, ci.var) {
    return(testci_gen(ci.ref, ci.var))
}

#' @describeIn testspc_gen
#' Return simple hilo comparison
#'
#' @inheritParams testspc_gen
#'
# @family confidence interval routines
# @family testspc routines
# @family returns hilo
# @family returns numeric
#'
# @export
#'
testspc_hilo <- function(
    value.var, value.ref
    , denominator.var, multiplier = 1
    , ci_type = "poisson", sd = 3
    , return_type = c("minimal", "data.frame", "data.table")[1]
) {
    testspc_gen(value.var, value.ref, denominator.var, multiplier, ci_type, sd, bAsString = FALSE, return_type)
}

#' @describeIn testci_gen
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
# @family confidence interval routines
#' @family testci routines
#' @family instances of generic routines
#' @family returns sense
#' @family returns numeric
#'
#' @md
# @export
#'
testci_sense <- function(ci.ref, ci.var, bSenseHigherisBetter = NA) {
    return(testci_gen(ci.ref, ci.var, bSenseHigherisBetter = bSenseHigherisBetter))
}

#' @describeIn testci_gen
#' Return a string to denote higher/lower/similar/not tested
#'
#' @inheritParams testci_hilo
#'
# @family confidence interval routines
#' @family testci routines
#' @family instances of generic routines
#' @family returns hilo
#' @family returns string
#'
# @export
#'
testci_hilo_s <- function(ci.ref, ci.var) {
    return(testci_gen(ci.ref, ci.var, bAsString = TRUE))
}

#' @describeIn testspc_gen
#' Return a string to denote higher/lower/similar/not tested
#'
#' @inheritParams testspc_hilo
#'
# @family confidence interval routines
# @family testspc routines
# @family instances of generic routines
# @family returns hilo
# @family returns string
#'
# @export
#'
testspc_hilo_s <- function(
    value.var, value.ref
    , denominator.var, multiplier = 1
    , ci_type = "poisson", sd = 3
    , return_type = c("minimal", "data.frame", "data.table")[1]
) {
    testspc_gen(value.var, value.ref, denominator.var, multiplier, ci_type, sd, bAsString = TRUE, return_type)
}

#' @describeIn testci_gen
#' Return a string to denote better/worse/similar/not tested
#'
#' @inherit testci_sense
#'
# @family confidence interval routines
#' @family testci routines
#' @family instances of generic routines
#' @family returns sense
#' @family returns string
#'
# @export
#'
testci_sense_s <- function(ci.ref, ci.var, bSenseHigherisBetter = NA) {
    return(testci_gen(ci.ref, ci.var, bAsString = TRUE, bSenseHigherisBetter = bSenseHigherisBetter))
}
