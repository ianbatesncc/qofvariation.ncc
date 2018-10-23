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
    setnames(x, gsub("\\.*$", "", make.names(tolower(colnames(x)))))
}

#' infix like but ignore case
#'
#' @seealso %like%
#'
`%ilike%` <- function(vector, pattern) {
    if (is.factor(vector)) {
        as.integer(vector) %in% grep(pattern, levels(vector), ignore.case = TRUE)
    }
    else {
        grepl(pattern, vector, ignore.case = TRUE)
    }
}

#' My summary
#'
#' factor version of summary
fsummary <- function(x, ...) {
    x %>%
        mutate_if(is.character, as.factor) %>%
        summary(...)
}

#' My glimpse
#'
#' factor version of glimpse
fglimpse <- function(x, width = NULL, ...) {
    if (!"data.frame" %in% class(x))
        stop("need a data frame object")

    if (is.null(width))
        width = options()$width

    cat("Observations:", nrow(x), "\n")
    cat("Variables:", ncol(x), "\n")

    these_names <- names(x)
    n_names_max <- max(nchar(these_names))

    these_names %>%
        lapply(
            function(field, x, width) {
                spad <- paste0(rep(" ", n_names_max - nchar(field) + 1), collapse = "")
                msg <- paste0("$ ", field, spad)
                vx <- as.data.frame(x)[, field]

                if (!is.character(vx)) {
                    msg <- paste0(msg, "<", vx %>% class() %>% substr(., 1, 3), "> ")
                    s <- summary(vx)
                    msg <- paste0(
                        msg
                        , data.frame(n = names(s), v = formatC(as.numeric(s), digits = 3, width = 0)) %>%
                            mutate(l = paste0(n, ": ", v)) %>%
                            .$l %>%
                            paste(collapse = ", ")
                    )
                } else {
                    f <- vx %>% unique()
                    nf <- length(f)
                    msg <- paste0(
                        msg, "<chr n=", nf, "> "
                        , ifelse(nf > 0, "\"", "")
                        , paste(f, collapse = "\", \"")
                        , ifelse(nf > 0, "\"", "")
                    )
                }

                nmsg <- nchar(msg)

                if (!is.null(width))
                    if (nmsg > width)
                        msg <- paste0(substr(msg, 1, width - 3), "...")

                cat(msg, "\n")

            }
            , x = x
            , width = width
        )

    invisible(x)
}
