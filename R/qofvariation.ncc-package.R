#' qofvariation.ncc.
#'
#'
#' cdg_91_qof.R
#'
#' Process QOF for visualisation (interactive?)
#'
#'
#' Require certain disease areas
#'
#' - practice level prevalence and stat. sig. relative to England
#' - CDG level achievement (?treatment) and stat. sig. relative to England.
#'
#' @import dplyr
#' @importFrom reshape2 dcast melt
#' @importFrom data.table fread fwrite copy setDT setnames
#'
#' @name qofvariation.ncc
#' @docType package
#'
NULL


#' Families
#'
#' List of families.
#'
#' @family Helper routines
#'
#' @family External routines
#' @family Internal routines
#'
#' @family Load routines
#' @family Save routines
#'
#' @family Reference routines
#' @family Measure routines
#' @family Compare routines
#'
#' @name families
NULL


#' Family of Helper routines
#' @family Helper routines
#' @name family_of_helpers
#' @rdname families
NULL

#' Family of External routines
#' @family External routines
#' @name family_of_external
#' @rdname families
NULL

#' Family of Internal routines
#' @family Internal routines
#' @name family_of_internal
#' @rdname families
NULL

#' Family of Load routines
#' @family Load routines
#' @name family_of_load
#' @rdname families
NULL

#' Family of Save routines
#' @family Save routines
#' @name family_of_save
#' @rdname families
NULL

#' Family of Measure routines
#' @family Measure routines
#' @name family_of_measure
#' @rdname families
NULL

#' Family of Compare routines
#' @family Compare routines
#' @name family_of_compare
#' @rdname families
NULL

#' Family of Reference routines
#' @family Reference routines
#' @name family_of_reference
#' @rdname families
NULL


# Workaround to dodge check warnings about field names
utils::globalVariables(
    c(
        ":="
        , "org.type", "org.code"
        , "practice_code", "ccg_code"
        , "ccg_group_code", "ccg_group_type"
        , "type_display_order"
        , "m.type", "m.stat", "m.name"
        , "data.source", "data_source"
        , "."
        , "numerator", "denominator"
        , "value", "value.ref", "value.var"
        , "cilo", "cihi", "cilo.ref", "cihi.ref", "cilo.var", "cihi.var"
        , "ci.ref"
        , "comp", "comp.sense", "comp_sense"
        , "statsig"
        , "i.num", "i.den", "i.exc"
        , "register", "patient_list_size", "patient_list_type", "exceptions"
        , "is.register"
        , "measure"
        , "domain_code", "indicator_group_code", "indicator_code"
        , "qof.period"
        , "taskdir"
        , "level.spc"
    )
)
