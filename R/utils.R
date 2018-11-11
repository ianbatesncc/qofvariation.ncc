#
# utils.R
#

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
status <- function(x, ...) {
    if (verbosity.showatlevel("chatty"))
        cat(..., "\n")
    invisible(x)
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
#' @param vector Either a \code{character} vector or a \code{factor}.  A
#'   \code{factor} is faster.
#' @param pattern Passed on to \code{grepl}
#'
ilike <- function(vector, pattern) {
    if (is.factor(vector)) {
        as.integer(vector) %in%
            grep(pattern, levels(vector), ignore.case = TRUE)
    } else {
        grepl(pattern, vector, ignore.case = TRUE)
    }
}

#' @inherit ilike
#' @inheritParams ilike
#' @describeIn ilike
#'
`%ilike%` <- ilike

#' My summary
#'
#' @param x data to summary-ise
#' @param ... extra options to pass to summary
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
#'
#' @param x (data.frame) data to gimpse
#' @param width (int) console width, NULL means use default options()$width
#' @param showvalues (bool) Show field values (default TRUE)
#' @param ... (ignored) extra options
#'
fglimpse <- function(x, width = NULL, showvalues = TRUE, ...) {
    v <- NULL

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
                    msg <- paste0(msg, "<", vx %>% class() %>% substr(., 1, 3), ">")
                    if (showvalues == TRUE) {
                        s <- summary(vx)
                        msg <- paste0(
                            msg
                            , " "
                            , data.frame(
                                n = names(s)
                                # avoid NA conversion messages - expected
                                , v = formatC(suppressWarnings(as.numeric(s)), digits = 3, width = 0)
                            ) %>%
                                mutate(l = paste0(n, ": ", v)) %>%
                                .$l %>%
                                paste(collapse = ", ")
                        )
                    }
                } else {
                    f <- vx %>% unique()
                    nf <- length(f)
                    msg <- paste0(msg, "<chr n=", nf, ">")
                    if (showvalues == TRUE) {
                        if (nf > 0)
                            msg <- paste0(
                                msg, " \"", paste(f, collapse = "\", \""), "\""
                            )
                    }
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

#' My describe
#'
#' Helper for generating roxygen documentation for a data.frame
#'
#' @inheritParams fglimpse
#' @param type (character) specify describe or tabular lists.
#'
#' @return
#'
#' \preformatted{
#' #' \describe\{
#' #'   \item\{\code\{field1\}\}\{description 1\}
#' #'   \item\{\code\{field2\}\}\{description 2\}
#' #'  ...
#' #' \}
#'
#' #' \tabular\{ll\}\{
#' #'   \{\{\code\{field1\}\} \tab \{description 1\} \cr
#' #'   \{\{\code\{field2\}\} \tab \{description 1\} \cr
#' #'   ...
#' #' \}
#' }
#'
describe <- function(x, width = NULL, showvalues = FALSE, type = c("describe", "tabular")) {
    v <- NULL
    type <- match.arg(type)

    if (!"data.frame" %in% class(x))
        stop("need a data frame object")

    if (is.null(width))
        width = options()$width

    these_names <- names(x)
    n_names_max <- max(nchar(these_names))

    if (type == "describe") {
        cat("#' \\describe{", "\n")
    } else {
        cat("#' \\tabular{ll}{", "\n")
    }


    these_names %>%
        lapply(
            function(field, x, width) {
                if (type == "describe") {
                    msg <- paste0("#'  \\item{\\code{", field, "}}{")
                } else {
                    msg <- paste0("#'  \\code{", field, "} \\tab {")
                }
                vx <- as.data.frame(x)[, field]

                if (!is.character(vx)) {
                    msg <- paste0(msg, "<", vx %>% class() %>% substr(., 1, 3), ">")
                    if (showvalues == TRUE) {
                        s <- summary(vx)
                        msg <- paste0(
                            msg
                            , " "
                            , data.frame(
                                n = names(s)
                                , v = formatC(as.numeric(s), digits = 3, width = 0)
                            ) %>%
                                mutate(l = paste0(n, ": ", v)) %>%
                                .$l %>%
                                paste(collapse = ", ")
                        )
                    }
                } else {
                    f <- vx %>% unique()
                    nf <- length(f)
                    msg <- paste0(msg, "<chr n=", nf, ">")
                    if (showvalues == TRUE)
                        if (nf > 0)
                        msg <- paste0(
                            msg, " \"", paste(f, collapse = "\", \""), "\""
                        )
                }

                nmsg <- nchar(msg)

                if (!is.null(width))
                    if (nmsg > width)
                        msg <- paste0(substr(msg, 1, width - 3), "...")

                if (type == "describe") {
                    cat(paste0(msg, "}"), "\n")
                } else {
                    cat(paste0(msg, "} \\cr"), "\n")
                }

            }
            , x = x
            , width = width
        )

    cat("#' }", "\n")

    NULL
}
