#
# utils.R
#

#' Concept of 'root directory' depends on context.
#'
#' - from project root is ./R
#' - from ./inst/dashboard it is, not surprisingly, ./R ...
#'
#' @family Helper routines
#'
proj_root <- function() {
    require("devtools")
    normalizePath(devtools::package_file(), winslash = "/", mustWork = TRUE)
}

#' Shortcut for constructing paths
#'
#' @param ... path elements.  Passed to paste.
#'
#' @family Helper routines
#'
proj_path <- function(...) {
    require("devtools")
    normalizePath(devtools::package_file(...), winslash = "/", mustWork = FALSE)
}

#' Contruct paths
#'
#' @param ... path elements.  Passed to paste.
#'
paste_paths <- function(...) {
    .Deprecated("proj_path")
    proj_path(...)
}

#' Progress within a pipe
#'
#' Show a message then pass on the object invisibly.
#'
#' @note \code{"\\n"} added to message.
#'
#' @param x data object
#' @param ... message elements.  Passed to cat().
#'
#' @examples
#' \dontrun{
#'     data %>% status("Processing ...") %>% some_long_calc()
#' }
#'
#' @family Helper routines
#'
status <- function(x, ...){
    cat(..., "\n");invisible(x)
}

#' Sum ignoring NAs
#'
#' For use in e.g. dcast to ignore NAs
#'
#' @param x vector object
#'
#' @family Helper routines
#'
sum.rmna <- function(x) {
    return(sum(x, na.rm = TRUE))
}

#' Clean data.frame field names
#'
#' To clean table/frame names
#'
#' @param x data.frame object
#'
#' @family Helper routines
#'
setnames.clean <- function(x) {
    setnames(x, make.names(tolower(colnames(x))))
}
