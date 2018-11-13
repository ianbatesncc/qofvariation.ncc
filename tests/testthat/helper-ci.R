#
# helper-ci.R
#

n <- 16
xn <- stats::runif(n, 10, 90)
yn <- xn + stats::runif(n, 100, 900)
multiplier = 1000
ci_type = "poisson"
level = 0.95

dat <- data.frame(
    num = xn, den = yn, multiplier, ci_type, level
    , stringsAsFactors = FALSE
)

value.var = xn * multiplier / yn
value.ref = median(value.var)
sd = 3
