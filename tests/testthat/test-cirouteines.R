#
# Test routine for testspc_gen
#

context("ci")

# suppresswarnings to dodge message about compiled under different version
suppressWarnings(require("bindrcpp"))
#suppressWarnings(require("dplyr"))
#if (is_installed("data.table"))
#    suppressWarnings(require("data.table"))

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

test_that("ci routines work as expected", {
    #cat("INFO: setting up data frame ...", "\n")

    #cat("INFO: aphoci_gen OUTSIDE data frame ...", "\n")

    ci <- aphoci_gen(xn, yn, multiplier, level, ci_type)
    cit <- aphoci_gen(xn, yn, multiplier, level, ci_type, bTransposeResults = TRUE)

    expect_equal(ci, transpose(cit))

    #cat("INFO: aphoci_gen WITHIN data frame ...", "\n")

    calc1 <- dat %>% mutate(ci.var = aphoci_gen(num, den, multiplier, level, ci_type))

    expect_is(calc1$ci.var, "list")
    expect_length(calc1$ci.var, n)


    #cat("INFO: testci_gen OUTSIDE data frame ... (minimal)", "\n")

    calc2 <- dat %>% mutate(
        value.var = value.var
        , ci.var = aphoci_gen(num, den, multiplier, level, ci_type)
        , value.ref = value.ref
        , ci.ref = aphoci_gen(value.ref * den / multiplier, den, multiplier, level, ci_type)
    )

    #cat("INFO: testci_gen OUTSIDE data frame ... (minimal)", "\n")

    retval_minimal <- testci_gen(calc2$ci.ref, calc2$value.var)

    expect_is(retval_minimal, "numeric")
    expect_length(retval_minimal, n)

    #cat("INFO: testci_gen OUTSIDE data frame ... (data.frame)", "\n")

    retval_data.frame <- testci_gen(
        calc2$ci.ref, calc2$value.var, return_type = "data.frame"
    )

    expect_is(retval_data.frame, "data.frame")
    expect_equal(retval_minimal, retval_data.frame$comp)

    #cat("INFO: testci_gen OUTSIDE data frame ... (data.table)", "\n")

    retval_data.table <- testci_gen(
        calc2$ci.ref, calc2$value.var, return_type = "data.table"
    )

    expect_is(retval_data.table, "data.table")
    expect_equal(retval_minimal, retval_data.table$comp)

    #cat("INFO: testci_gen WITHIN data frame ... (minimal)", "\n")

    calc3 <- calc2 %>%
        mutate(retval_comp = testci_gen(ci.ref, value.var))

    expect_equal(retval_minimal, calc3$retval_comp)
})

context("spc")

test_that("spc routines work as expected", {

    #cat("INFO: testspc_gen OUTSIDE data frame ... ", "\n")

    retval <- testspc_gen(value.var, value.ref, yn, multiplier, ci_type, sd)
    retval.df <- testspc_gen(
        value.var, value.ref, yn, multiplier, ci_type, sd
        , return_type = "data.frame"
    )
    retval.str.df <- testspc_gen(
        value.var, value.ref, yn, multiplier, ci_type, sd
        , bAsString = TRUE, return_type = "data.frame"
    )

    #cat("INFO: testspc_gen WITHIN data frame ... ", "\n")

    calc3 <- dat %>% mutate(
        value.var = value.var
        , ci.var = aphoci_gen(num, den, multiplier, level, ci_type)
        , value.ref = value.ref
        , ci.ref = aphoci_gen(value.ref * den / multiplier, den, multiplier, level, ci_type)
        #, retval_comp = testci_gen(ci.ref, value.var)
    )

    calc4 <- calc3 %>%
        mutate(comp.spc = testspc_gen(value.var, value.ref, yn, multiplier, ci_type, sd))

    expect_is(calc4$comp.spc, "numeric")

    calc4.string <- calc3 %>%
        mutate(comp.spc = testspc_gen(value.var, value.ref, yn, multiplier, ci_type, sd, bAsString = TRUE))

    expect_is(calc4.string$comp.spc, "character")

})
