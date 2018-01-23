#' Join multiple course evaluations into one data frame
#'
#' This function takes a list of data frames from \code{\link{read_eval}} and
#' returns a single data frame with appropriate labels.
#'
#' @param data Data frame as returned from \code{\link{read_eval}}
#' @param subset Optional numeric vector identifying a subset of data sets,
#'   which should be used (e.g., \code{c(1, 3:5)}).
#' @param lang Character string specifying the language of the variable names.
#' @param wide_format Logical indicating whether data should be returned in wide format.
#' @inheritParams read_eval
#' @return A data frame (more precisely, a tibble) with scores for all scales and persons.
# @return Returns a list of five elements:
# \itemize{
#  \item{\code{mean}: For each scale and each course evaluation, the mean across all participants}
#  \item{\code{sd}: For each scale and each course evaluation, the SD across all participants}
#  \item{\code{se}: For each scale and each course evaluation, the SE across all participants}
#  \item{\code{N}: For each course evaluation, the number of participants}
#  \item{\code{varnames}: For each scale, its label}
# }
#' @export
#' @examples
#' \dontrun{
#' tmp1 <- read_eval("./data/")      # read all files
#' dat1 <- join_eval(tmp1)          # combine data frames for plotting
#' }
join_eval <- function(data,
                      subset = NULL,
                      lang = c("de", "en"),
                      names = NULL,
                      wide_format = FALSE) {

    lang <- match.arg(lang)
    checkmate::assert_list(data, types = "list", any.missing = FALSE,
                           min.len = 1, names = "unique")
    checkmate::assert_integerish(subset, lower = 1, upper = length(data),
                                 any.missing = FALSE, min.len = 1,
                                 max.len = length(data), unique = TRUE,
                                 null.ok = TRUE)
    if (!is.null(subset)) {
        data <- data[subset]
    }
    checkmate::assert_character(names, min.chars = 1,
                                any.missing = FALSE,
                                len = length(data), unique = TRUE, null.ok = TRUE)
    if (!is.null(names)) {
        names(data) <- names
    }

    checkmate::qassert(wide_format, "B1")

    ### End of input wrangling ###

    dat2 <- reshape2::melt(data, measure.vars = "resp")
        # tibble::as_tibble(.)

    dat3 <- dat2 %>%
        dplyr::mutate_at(c("L1", "id", "item_no"), dplyr::funs(factor(.))) %>%
        dplyr::mutate_at("L1", dplyr::funs(factor(., levels = names(data)))) %>%
        dplyr::select(course = .data$L1, .data$id, item = .data$number, .data$value)

    dat4 <- dat3 %>%
        dplyr::group_by(.data$course, .data$id) %>%
        dplyr::filter(.data$item %in% (1:4)) %>%
        dplyr::summarise(value = mean(.data$value),
                         item = 53) %>%
        dplyr::full_join(dat3, by = c("course", "id", "value", "item"))

    topic_lb <- switch(lang,
                       "de" = "topic_de",
                       "en" = "topic_en")

    varnames_2 <- varnames[, c("number", "ordering", "domain", "topic_full", topic_lb)]


    dat5 <- dplyr::left_join(dat4, varnames_2, by = c("item" = "number")) %>%
        dplyr::select(dplyr::everything(), topic = topic_lb)

    dat6 <- dat5 %>%
        dplyr::group_by_("course", "id", "topic_full", "topic", "ordering", "domain") %>%
        dplyr::summarise(score = mean(.data$value)) %>%
        dplyr::ungroup()

    if (wide_format == FALSE) {
        attr(dat6, "lang") <- lang
        return(dat6)
    } else {
        dat7 <- reshape2::dcast(dat6, value.var = "score", course + id ~ topic)
        attr(dat7, "lang") <- lang
        attr(dat7, "long_wide") <- "wide"
        # is.null(attr(dat7, "long_wide"))
        return(dat7)
    }
}

#' Descriptives for all courses
#'
#' This function takes a data frame from \code{\link{join_eval}} containing
#' data from multiple courses and calculates descriptive statistics such as the
#' mean.
#'
#' @param data Data frame as returned from \code{\link{join_eval}}
#' @inheritParams plot_eval
#' @return A data frame (more precisely, a tibble).
# @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' tmp1 <- read_eval("./data")
#' dat1 <- join_eval(tmp1)
#' describe_eval(dat1)
#' }
describe_eval <- function(data,
                          CI = .95) {
    res <- data %>%
        dplyr::group_by(.data$course, .data$topic, .data$domain) %>%
        dplyr::summarise("N" = length(.data$score),
                         "Mean" = mean(.data$score),
                         "SD" = sd(.data$score),
                         "SE" = .data$SD/sqrt(.data$N),
                         "LL" = .data$Mean + qnorm((1 - CI)/2)*.data$SE,
                         "UL" = .data$Mean - qnorm((1 - CI)/2)*.data$SE,
                         "Q25" = quantile(.data$score, probs = .25, names = FALSE),
                         "Q50" = quantile(.data$score, probs = .50, names = FALSE),
                         "Q75" = quantile(.data$score, probs = .75, names = FALSE)) %>%
        dplyr::ungroup()
    return(res)
}

#' HTML table widget of all courses using the DataTables library
#'
#' This function takes a data frame from \code{\link{join_eval}} containing
#' data from multiple courses and displays summary statistics (e.g., mean) using
#' the DataTables library.
#'
#' @param data Data frame as returned from \code{\link{join_eval}}
#' @param digits Numeric indicating the number of decimal places to be used. Passed to \code{\link[base]{round}}.
#' @seealso \code{\link[DT]{datatable}}.
#' @export
#' @examples
#' \dontrun{
#' tmp1 <- read_eval("./data")
#' dat1 <- join_eval(tmp1)
#' datatable_eval(dat1)
#' }
#'
#' dat1 <- sim_eval()
#' datatable_eval(dat1)
datatable_eval <- function(data, digits = 2) {

    if (!requireNamespace("DT", quietly = TRUE)) {
        stop("Please run `install.packages('DT')` first.")
    }

    if (!all(c("Mean", "SE") %in% names(data))) {
        DATA <- describe_eval(data); rm(data)
    } else {
        DATA <- data; rm(data)
    }

    tmp1 <- length(levels(DATA$course))

    res <- dplyr::arrange(DATA, .data$topic, .data$course) %>%
        dplyr::mutate_at(c("Mean", "SD", "SE", "LL", "UL", "Q25", "Q50", "Q75"), round, digits = digits) %>%
        DT::datatable(options = list(lengthMenu = c(tmp1, 2*tmp1, 4*tmp1, 8*tmp1),
                                     pageLength = tmp1,
                                     autoWidth = F,
                                     dom = "Bftlpr",
                                     scrollX = T,
                                     # fixedColumns = list(leftColumns = 2),
                                     colReorder = TRUE,
                                     # scrollY = T,
                                     fixedHeader = TRUE,
                                     # fixedColumns = list(leftColumns = 3),
                                     # buttons = I("colvis"),
                                     buttons = list(list(extend = "colvis",
                                                         columns = 3:11))),
                  extensions = c("ColReorder",
                                 # "FixedColumns",
                                 "FixedHeader",
                                 "Buttons"),
                  rownames = FALSE, filter = "top") %>%
        DT::formatRound(c("Mean", "SD", "SE", "LL", "UL", "Q25", "Q50", "Q75"), digits = digits) %>%
        DT::formatStyle(c("course", "topic"), fontWeight = "bold")
    return(res)
}
