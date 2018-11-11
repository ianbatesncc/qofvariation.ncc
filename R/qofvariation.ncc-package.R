#' qofvariation.ncc-package.R
#'
#' Process QOF for visualisation (interactive?)
#'
#'
#' Require certain disease areas
#'
#' - practice level prevalence and stat. sig. relative to England
#' - CDG level achievement (?treatment) and stat. sig. relative to England.
#'
#' @rawNamespace import(dplyr, except = c(first, between, last))
#' @rawNamespace import(data.table, except = c(dcast, melt))
#' @importFrom reshape2 dcast melt
# @rawNamespace import(devtools, except = c(use_data))
# @importFrom usethis use_data
#'
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

#' possible verbosity levels (global)
#'
#' 0 - suppress (default)
#' 1 - information
#' 2 - chatty
#'
#' @rdname verbosity
#'
verbosity_levels <- seq(0, 2)
names(verbosity_levels) <- c("suppress", "info", "chatty")

#' verbosity level (global)
#'
#' 0 - suppress (default)
#' 1 - information
#' 2 - chatty
#'
#' @rdname verbosity
#'
verbosity_level <- verbosity_levels["chatty"]

#' compare verbosity with setting for code chunk
#'
#' Chatty by default.  Can make less chatty by DECREASING verbosity level
#' Conversely, INCREASE verbosity with INCREASING values ...
#'
#' ... warnings and errors always show
#' ... switch messages off with 0
#' ... useful stuff for 1
#' ... details at level 2
#'
#' check level is greater than "suppress" yet lower than "code classification"
#'
#' @param this_msg_category
#'
#' @examples
#' verbosity_level <- verbosity_levels["chatty"]
#' names(verbosity_levels) %>% sapply(verbosity.showatlevel)
#' names(verbosity_levels) %>% sapply(function(x) {
#'   msg <- paste("global level: ", names(verbosity_level))
#'   if (verbosity.showatlevel(x)) msg <- paste(msg, "This is a[n]", x, "message")
#'   msg
#' })
#'
#' if (verbosity.showatlevel("info")) cat("Hello World!\n")
#'
#' verbosity_level <- verbosity_levels["info"]
#' names(verbosity_levels) %>% sapply(verbosity.showatlevel)
#'
#' if (verbosity.showatlevel("info")) cat("Hello World!\n")
#'
#' verbosity_level <- verbosity_levels["suppress"]
#' names(verbosity_levels) %>% sapply(verbosity.showatlevel)
#'
#' if (verbosity.showatlevel("info")) cat("Hello World!\n")
#'
#' @rdname verbosity
#'
verbosity.showatlevel <- function(this_msg_category) {
    this_msg_level <- verbosity_levels[this_msg_category]

    return(
        (verbosity_level != verbosity_levels["suppress"])
        && (this_msg_level <= verbosity_level)
    )
}


# Workaround to dodge check warnings about field names ####
utils::globalVariables(
    c(
        ":="
        , "."
        , "ccg_code"
        , "ccg_group_code"
        , "ccg_group_type"
        , "ci.ref"
        , "cilo"
        , "cilo.ref"
        , "cilo.var"
        , "cihi"
        , "cihi.ref"
        , "cihi.var"
        , "comp"
        , "comp.sense"
        , "comp_sense"
        , "data.source"
        , "data_source"
        , "denominator"
        , "disease"
        , "disease_register"
        , "disease_register_size"
        , "domain_code"
        , "domain_points"
        , "exception_count"
        , "exceptions"
        , "i.den"
        , "i.exc"
        , "i.num"
        , "indicator_code"
        , "indicator_description"
        , "indicator_group"
        , "indicator_group_code"
        , "indicator_group_desc_prev"
        , "indicator_group_description"
        , "indicator_type"
        , "indicator_version"
        , "is.register"
        , "level.spc"
        , "list_type"
        , "m.name"
        , "m.type"
        , "m.stat"
        , "measure"
        , "numerator"
        , "org.type"
        , "org.code"
        , "patient_list_size"
        , "patient_list_type"
        , "practice_code"
        , "practice_list_size"
        , "practice_listsize"
        , "practicecode"
        , "prevalence"
        , "qof.period"
        , "qof_measure"
        , "register"
        , "register_description"
        , "register_list_size"
        , "register_size"
        , "revised_maximum_points"
        , "s1"
        , "sheet"
        , "statistic"
        , "statsig"
        , "tbl_heading"
        , "this_file"
        , "type_display_order"
        , "value"
        , "value.ref"
        , "value.var"
    )
)
