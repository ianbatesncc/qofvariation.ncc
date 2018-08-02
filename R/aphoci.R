#' Family of confidence interval routines
#'
#' aphoci style confidence intervals and tests
#' made easy with R
#'
#' Output Functions
#'
#' aphoci_rate(num, den, mult, level)
#' aphoci_prop(num, den, mult, level)
#'
#' testci(ci.ref, ci.var, bSenseHigherisBetter, iVerbose)
#' testci_s(ci.ref, ci.var, bSenseHigherisBetter, iVerbose)
#'
#' @family confidence interval routines
#'
#' @name aphoci
NULL

#' Round
#'
#' round to nearest whole number
#'
#' @param x number
#' @param digits precision
#' @return x rounded to precision
#'
#' @family Helper routines
#'
round_nearest <- function(x, digits = 0) {
    d = 10^(-digits)
    m = 10^(digits)
    return((floor(x * m) + ((x * m) - floor(x * m) >= 0.5)) * d)
}
