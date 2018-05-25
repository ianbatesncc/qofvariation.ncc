# calcci.R
#
# Routines to calculate confidence intervals.
#

#' Family of calculate routines
#'
#' Container for calculate routines
#'
#' @family calculate routines
#' @family sections
#'
#' @name calcci
NULL

#' Byars approximation to poission
#'
#' @param x Numerator / number of events
#' @param T Denominator / time base for event count
#' @param conf.level Confidence level (\code{0 < level <= 1}).  Optional -
#'   default 0.95
#'
#' @return Double vector dim 2 containing lower and upper confidence level.
#'
#' @importFrom stats qnorm
#'
#' @export

poisson_byars.test <- function(x, T, conf.level = 0.95) {
    if (conf.level == 0.95) {
        z <- 1.959664
    } else {
        z <- stats::qnorm((conf.level + 1) / 2)
    }
    o <- x
    o.lower <- (o    ) * ((1 - (1 / (9 * (o    ))) - (z / (3 * sqrt(o    )))) ^ 3)
    o.upper <- (o + 1) * ((1 - (1 / (9 * (o + 1))) + (z / (3 * sqrt(o + 1)))) ^ 3)

    return(list(conf.int = c(o.lower, o.upper) / T))
}

#' Wilson approximation to proportion
#'
#' @param x Numerator / count of successes
#' @param n Denominator / number of trials
#' @param conf.level Confidence level (\code{0 < level <= 1}).  Optional -
#'   default 0.95
#'
#' @return Double vector dim 2 containing lower and upper confidence level.
#'
#' @importFrom stats qnorm
#'
#' @export

prop_wilson.test <- function(x, n, conf.level = 0.95) {
    if (conf.level == 0.95) {
        z <- 1.959664
    } else {
        z <- stats::qnorm((conf.level + 1) / 2)
    }
    o <- x

    # p <- x / n
    # q <- 1 - p
    z2 <- z^2

    n1 <- (2 * o + z2)
    # sqrt() complains with NA/Inf/NaN/negative corner case input - to handle at
    # some point generically - suppress warning for now
    n2 <- z * suppressWarnings(sqrt(z2 + 4 * o * (1 - x / n)))
    d1 <- 2*(n + z2)

    pd.lower <- (n1 - n2)
    pd.upper <- (n1 + n2)

    return(list(conf.int = c(pd.lower, pd.upper) / d1))
}

#
# Generic calculation routine ####
#

#' Implementation details
#'
#' APHO CI routines - but in R
#'
#' Need to protect against some corner cases
#'
#' prop.test
#'
#' (x, 0) (NA, x) (x, NA) generates an error (0,  >0..<10) generates a warning
#'
#' poisson.test
#'
#' (0, 0) ci : (NaN, Inf) (x, 0) ci : (Inf, Inf) (NA, x) (x, NA) generates an
#' error
#'
#' binom.test ??
#'
#' For vectors, need to assemble as 1D list of pairs of (num, den, mult,
#' ci.type) return type compatible with data.frame.  Use "t" prefix for
#' data.table use.

#' Calculate confidence intervals.
#'
#' Calculate confidence intervals for differing types of measure given a
#' numerator and denominator and optionally a multiplier, confidence level and
#' statistical method.
#'
#' @param num Numerator
#' @param den Denominator
#' @param multiplier Multiplier.  Optional - default 1.
#' @param level Confidence level (\code{0 < level <= 1}).  Optional - default
#'   0.95
#' @param ci.type Type of confidence interval.  Optional - default "poisson"
#'   Possible values {"poisson", "proportion", "poisson_byars", "prop_wilson",
#'   "poisson_native", "proportion_native", "binomial_native"}. Also byars and
#'   wilson can be used (aliased to poisson_byars and prop_wilson respectively).
#' @param bTransposeResults Boolean.  Only used when vector options are passed.
#'   If TRUE spin results around.  See \code{return}.  Defaults to FALSE.
#'
#' @note poisson and proportion use the apho defintions by default.  OVerride to
#'   use the native r exact methods by appending "_native" e.g. poission_native.
#'
#' @note The native proportion test applies a continuity correction by default.
#'   We do not want this to occur so this case is treated specially.
#'
#' @return Double vector dim 2 containing lower and upper confidence level.
#'
#'   When vector options are passed the return value is a list of cis i.e.
#'   list(vec_ci, vec_ci, vec_ci, ...)
#'
#'   When bTranspose is TRUE the return value is a list of two vectors i.e.
#'   list(vec_cilo, vec_cihi)
#'
#' @importFrom stats poisson.test prop.test binom.test
#'
#' @examples
#' ## For data.table
#' if (require("data.table")) {
#' n <- 100
#' xn <- stats::runif(n)
#'
#' dat <- data.table(
#'     num = xn
#'     , den = stats::runif(n) + xn
#'     , mult = 10^stats::runif(n, min = 1, max = 3)
#'     , level = stats::runif(n, min = 0.95, max = 0.99)
#' )
#' dat[, ci := aphoci_prop(num, den, mult, level)]
#' dat[, c("cilo", "cihi") := aphoci_prop(num, den, mult, level, bTransposeResults = TRUE)]
#' # or equivalently
#' dat[, c("cilo", "cihi") := transpose_list_vector(aphoci_prop(num, den, mult, level))]
#' dat[, c("cilo", "cihi") := transpose_list_vector(ci)]
#' dat[, c("cilo", "cihi") := ci_ci2comps(ci)]
#' }
#'
#' ## for data.frame
#' if (require("dplyr")) {
#'
#' n <- 100
#' xn <- stats::runif(n)
#'
#' dat <- data.frame(
#'     num = xn
#'     , den = stats::runif(n) + xn
#'     , mult = 10^stats::runif(n, min = 1, max = 3)
#'     , level = stats::runif(n, min = 0.95, max = 0.99)
#' )
#' dat <- dat %>% dplyr::mutate(ci = aphoci_prop(num, den, mult, level))
#' dat$cilo <- transpose_list_vector(dat$ci)[[1]]
#' dat$cihi <- transpose_list_vector(dat$ci)[[2]]
#' }
#'
#' @note If any parameter is a vector remaining parameters will be recycled and
#'   the return value will be a list of vectors.
#'
#' @family calculate routines
#' @family generic routines
#'
#' @export
#'
aphoci_gen <- function(num, den, multiplier = 1, level = 0.95, ci.type = "poisson"
                       , bTransposeResults = FALSE) {
    ci <- c(NA, NA)
    #cat("DEBUG: aphoci_gen: (num, den) = (", num, ", ", den, ")", "\n")

    # The worker routine
    calcci <- function(l_num, l_den, l_mult, l_level, l_ci_type) {
        ci <- as.numeric(c(NA, NA))

        if (!is.na(l_ci_type)) {

            # choose test method
            f.test <- switch(
                l_ci_type
                # defaults
                , poisson           = poisson_byars.test
                , proportion        = prop_wilson.test
                # r native
                , poisson_native    = stats::poisson.test
                , proportion_native = stats::prop.test
                , binomial_native   = stats::binom.test
                # apho ci tech 3
                , poisson_apho      = poisson_byars.test
                , proportion_apho   = prop_wilson.test
                # aliases
                , proportion_wilson = prop_wilson.test
                , poisson_byars     = poisson_byars.test
                , byars             = poisson_byars.test
                , wilson            = prop_wilson.test
                # default fallback
                ,                     poisson_byars.test
            )

            # apply test method
            if (!any(is.na(c(l_num, l_den))) & (l_den > 0) & (l_num >= 0)) {

                # catch case for poisson
                if (identical(f.test, stats::poisson.test))
                    l_num <- round_nearest(l_num)

                # catch case for binomial
                if (identical(f.test, stats::binom.test)) {
                    l_num <- round_nearest(l_num)
                    l_den <- round_nearest(l_den)
                }

                # catch case proportion_native
                if (identical(f.test, stats::prop.test)) {
                    a <- f.test(l_num, l_den, conf.level = l_level, correct = FALSE)
                } else {
                    a <- f.test(l_num, l_den, conf.level = l_level)
                }
                ci <- a[["conf.int"]][1:2] * l_mult
                ci2 <- a$conf.int[1:2] * l_mult
            }
        }

        return(ci)
    }

    #cat("DEBUG: length(num, den, multiplier, level, ci.type) = ("
    #    , length(num), length(den), length(multiplier), length(level), length(ci.type),
    #    ")", "\n")

    # If there are vector arguments then mapply otherwise call direct the worker
    # routine.
    if (max(sapply(list(num, den, multiplier, level, ci.type), length)) > 1) {
        ci <- mapply(calcci, num, den, multiplier, level, ci.type)
        # mapply returns a matrix(rowx2, colxn).  Observations are in each
        # column with (cilo, cihi) in rows 1 and 2.
        #
        # Choose which way to present - list of rows (vecnx2) or list of
        # cols(vec2xn) ... Default _gen routines is to return list of n (vectors
        # length 2) ... bTransposeResults to change this ... lapply scans across
        # each column and returns all the rows
        #ci <- array_aslist(ci, bTransposeResults = !bTransposeResults)
        if (!bTransposeResults) # odd ... but true
            ci <- t(ci)
        ci <- lapply(seq(1, dim(ci)[1]), function(i, v){v[i, ]}, ci)
    } else {
        ci <- calcci(num, den, multiplier, level, ci.type)
    }

    return(ci)
}

#
# Instances of the generic routine ####
#

#' Instance to use poisson method (for rates)
#'
#' @inheritParams aphoci_gen
#'
#' @family calculate routines
#' @family instances of generic routines
#'
#' @export
#'
aphoci_rate <- function(num, den, multiplier = 1, level = 0.95, bTransposeResults = FALSE) {
    return(aphoci_gen(num, den, multiplier, level, "poisson", bTransposeResults))
}

#' Instance to use method for proportions
#'
#' @inheritParams aphoci_gen
#'
#' @family calculate routines
#' @family instances of generic routines
#'
#' @export
#'
aphoci_prop <- function(num, den, multiplier = 1, level = 0.95, bTransposeResults = FALSE) {
    return(aphoci_gen(num, den, multiplier, level, "proportion", bTransposeResults))
}

#
# Integration ####
#

## Functions work for individual cases.  Different story when integrating with
# e.g data.table.  Need wrappers to "vectorise".

# Considers vectors of num, den and calculates ci's based on (ni, di) -> ci
# prop.test, poisson.test not "vectorised" - must be an efficient work around
# this is the best I've arrived at without fully vectorising x.test

#' Convenience to transpose return value
#'
#' @inheritParams aphoci_gen
#'
#' @family calculate routines
#' @family generic routines
#'
#' @export
#'
#' @templateVar fun taphoci_gen
#' @template template-depr_fun
NULL

#' @templateVar old taphoci_gen
#' @templateVar new aphoci_gen
#' @template template-depr_pkg
#'
#' @export
taphoci_gen <- function(num, den, multiplier = 1, level = 0.95, ci.type = "poisson") {
    .Deprecated("aphoci_gen", "aphoci")
    return(aphoci_gen(num, den, multiplier, level, ci.type, bTransposeResults = TRUE))
}

#' Instance to use poisson method (for rates)
#'
#' @inheritParams taphoci_gen
#'
#' @family calculate routines
#' @family instances of generic routines
#'
#' @export
#'
#' @templateVar fun taphoci_rate
#' @template template-depr_fun
NULL

#' @templateVar old taphoci_rate
#' @templateVar new aphoci_gen
#' @template template-depr_pkg
#'
#' @export
taphoci_rate <- function(num, den, multiplier = 1, level = 0.95) {
    .Deprecated("aphoci_gen", "aphoci")
    return(aphoci_gen(num, den, multiplier, level, ci.type = "poisson", bTransposeResults = TRUE))
}

#'  Instance to use method for proportions
#'
#' @inheritParams taphoci_gen
#'
#' @family calculate routines
#' @family instances of generic routines
#'
#' @export
#'
#' @templateVar fun taphoci_prop
#' @template template-depr_fun
NULL

#' @templateVar old taphoci_prop
#' @templateVar new aphoci_gen
#' @template template-depr_pkg
#'
#' @export
taphoci_prop <- function(num, den, multiplier = 1, level = 0.95) {
    .Deprecated("aphoci_gen", "aphoci")
    return(aphoci_gen(num, den, multiplier, level, ci.type = "proportion", bTransposeResults = TRUE))
}
