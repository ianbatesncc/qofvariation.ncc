library(testthat)
library(qofvariation.ncc)

# setup
previous_verbosity <- verbosity_level
verbosity.level <- verbosity_levels["suppress"]

proj_get(quiet = TRUE)


test_check("qofvariation.ncc")


verbosity <- previous_verbosity
