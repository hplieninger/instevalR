#' Read insteval data from directory
#'
#' Reads all CSV-files from a given directory and returns all data in a single list
#'
#' @param path If \code{NULL}, the user is prompted to choose the files
#'   interactively; this is only available on Windows. Otherwise, a character
#'   vector containing path name(s), pointing to the CSV-files. The files
#'   probably have names ending with "evaluationen.csv". Make sure to put only
#'   those files in the directory and no other files.
#' @param names Optional character vector supplying the names of the courses.
#'   Number of elements must match the number of files.
#' @param pattern An optional regular expression passed to
#'   \code{\link[base]{list.files}}. Can be used to read data only from files
#'   with a matching filename.
#' @param recursive Logical passed to \code{\link[base]{list.files}}. Should the
#'   file seach recurse into directories?
#' @param shiny Logical. For internal use only.
#' @return List of data frames.
# @importFrom magrittr %>%
# @importFrom utils choose.files read.table
#' @export
#' @examples
#' \dontrun{
#' dat1 <- read_eval()
#'
#' # Specify own names and read only files matching a specific pattern
#' dat1 <- read_eval("./data", names = c("Lab1", "Lab2"), pattern = "lab")
#' }
read_eval <- function(path = NULL,
                      names = NULL,
                      pattern = NULL,
                      recursive = FALSE,
                      shiny = FALSE) {

    checkmate::qassert(recursive, "B1")

    if (is.null(path)) {
        if (Sys.info()["sysname"] == "Windows") {
            cat("Use the dialog to select the CSV-file(s) of your course evaluations\n")
            files <- utils::choose.files()
        } else {
            stop("A 'path' must be specified on non-Windows systems.")
        }
    } else if (shiny == FALSE) {
        path <- normalizePath(path, mustWork = TRUE)

        files <- list.files(path, pattern = pattern, full.names = TRUE,
                            recursive = recursive) %>%
            grep("[.]csv$|[.]xls$|[.]xlsx", x = ., value = TRUE) %>%
            grep(paste(c("kommentare", "fragen"), collapse = "|"), x = .,
                 value = TRUE, invert = TRUE)

        # files2 <- list.files(path, pattern = pattern, full.names = TRUE,
        #                      recursive = recursive) %>%
        #     grep("[.]csv$|[.]xls$|[.]xlsx", x = ., value = TRUE) %>%
        #     grep(paste(c("kommentare", "fragen"), collapse = "|"), x = .,
        #          value = TRUE, invert = TRUE)
    } else if (shiny == TRUE) {

        files <- path %>%
            grep("[.]csv$|[.]xls$|[.]xlsx", x = ., value = TRUE) %>%
            grep(paste(c("kommentare", "fragen"), collapse = "|"), x = .,
                 value = TRUE, invert = TRUE)

        # names <- paste0("Course", 1:length(files))
    }

    checkmate::assert_character(names, min.chars = 1,
                                any.missing = FALSE, all.missing = FALSE,
                                len = length(files), unique = TRUE,
                                null.ok = TRUE)
    if (!is.null(names)) {
        file_names <- names
    } else if (all(grepl("^InstEvaL-Rohdaten-vlg", basename(files)))) {
        # file_names <- substr(files, start = 23, stop = 1000) %>%
        #     sub("-evaluationen.csv", "", x = .) %>%
        #     as.numeric
        file_names <- as.numeric(regmatches(basename(files), regexpr("\\d+", basename(files))))
    } else {
        file_names <- sub(".csv$", "", basename(files))
    }

    dat <- vector("list", length = length(files))
    names(dat) <- file_names
    for (iii in seq_len(length(files))) {
        if (tools::file_ext(files[iii]) == "csv") {
            dat[[iii]] <- utils::read.table(files[[iii]], sep = ";", header = FALSE,
                                            skip = 1,
                                            col.names = c("item_no", "number", "id", "resp"))
        } else if (tools::file_ext(files[iii]) %in% c("xls", "xlsx")) {
            dat[[iii]] <- readxl::read_excel(files[[iii]], skip = 1,
                                             col_names = c("item_no", "number", "id", "resp"))
        }
    }

    if (shiny == FALSE)
        message("Data from the following ", length(dat), " file(s) have been imported:",
                paste0("\n  ", basename(files)))

    if (is.null(names)) {
        return(dat[order(file_names)])
    } else {
        return(dat)
    }
}
