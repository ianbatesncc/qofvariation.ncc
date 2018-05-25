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
testci_gen <- function(ci.ref, ci.var, bAsString = FALSE, bSenseHigherisBetter = NA) {

    if (DEBUG_TESTCI) {
        cat("DEBUG: testci_gen:", "ci.ref =", ci.ref, "\n")
        cat("DEBUG: testci_gen:", "ci.var =", ci.var, "\n")
        cat("DEBUG: testci_gen:", "bAsString =", bAsString, "\n")
        cat("DEBUG: testci_gen:", "bSenseHigherisBetter =", bSenseHigherisBetter, "\n")
    }

    # simple compare overlap
    hilo <- function(l_ci_ref, l_ci_var) {
        # prepare parameters
        ensure_length2_sorted <- function(x) {
            if (length(x) == 1)
                x <- rep(x, 2)
            return(sort(x[1:2], na.last = FALSE))
        }
        l_ci_ref <- ensure_length2_sorted(l_ci_ref)
        l_ci_var <- ensure_length2_sorted(l_ci_var)

        # compare
        retval <- NA
        if (!any(is.na(c(l_ci_var, l_ci_ref)))) {
            retval <- 0
            if (l_ci_var[1] > l_ci_ref[2]) {
                retval <- 1
            } else if (l_ci_var[2] < l_ci_ref[1]) {
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

    comp <- NA

    # Compare

    if (!(any(sapply(list(ci.ref, ci.var, bSenseHigherisBetter), typeof) == "list"))) {
        # simple case - all NOT list

        comp <- hilo(ci.ref, ci.var)
        comp <- apply_sense(comp, bSenseHigherisBetter)
    } else {
        # some lists involved

        # ensure all as list
        ensure_list <- function(x) {
            if (typeof(x) != "list") list(x)
            else x
        }
        ci.ref <- ensure_list(ci.ref)
        ci.var <- ensure_list(ci.var)

        comp <- mapply(hilo, ci.ref, ci.var)
        comp <- mapply(apply_sense, comp, bSenseHigherisBetter)
    }

    if (!is.na(bAsString) & bAsString)
        retval <- as_string(comp)
    else
        retval <- comp

    return(retval)
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
