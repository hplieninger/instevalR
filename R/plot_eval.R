#' Plot multiple course evalautions
#'
#' This function takes as input a data frame from \code{\link{join_eval}} (or
#' from \code{\link{sim_eval}}) and plots the results. Choose the type of plot
#' using the argument \code{plottype}. Furhtermore,  decide how the different
#' courses and the different topics should be represented in the plot using
#' \code{course_as} and \code{topic_as}, namely, either on the x-axis, in
#' different facets/panels, or using different colors/groups.
#'
#' @param data Data frame as returned from \code{\link{join_eval}} (or from
#'   \code{\link{sim_eval}}).
#' @param plottype Character, either \code{"barplot"}, \code{"points"}, \code{"boxplot"}, or \code{"violin"}.
#' @param courses Optional character vector of length >= 1. Used to select a
#'   subset of courses present in \code{data$course}.
#' @param domains Numeric of length >= 1, specifying the domains to plot.
#' \itemize{
#'  \item{1: Overall (one scale, namely, average of \code{domains = 2})}
#'  \item{2: Overall, (i.e., didactic skills, grade lecturer, grade course, and comparison with other courses)}
#'  \item{3: Different scales (e.g., structure, sympathy, motivation)}
#'  \item{4: Settings (e.g., temperature, acoustics, media use)}
#'  \item{5: Students' presentations}
#'  \item{6: Misc}
#' }
#' @param topics Optional character vector of length >= 1. Used to select a
#'   subset of topics present in \code{data$topic}, e.g., \code{topics =
#'   c("Overall", "Motivation")}. Overrides argument \code{domains}.
#' @param course_as Character. Should each course/file be plotted using a
#'   different value on the \code{"x-axis"}, a different \code{"facet"} (panel),
#'   or a different \code{"group"}?
#' @param topic_as Character. Should each topic be plotted using a different
#'   \code{"facet"} (panel), different \code{"group"}, or a different value on
#'   the \code{"x-axis"}?
#' @param add_line Logical. Should a connecting line be added to the plot?
#' @param errorbar Logical indicating whether error bars representing a
#'   confidence interval should be plotted or not.
#' @param CI Numeric. Confidence level of error bars, often \code{.95} for a 95\% CI.
#' @param grey Logical indicating whether colors should be replaces with shades
#'   of grey.
#' @param xvar_as_legend Logical indicating whether a legend should be plotted
#'   for the variable on the x-axis.
#' @param control Optional list to fine-tune the plot.
#' \describe{
#'  \item{nrow}{Numeric. Number of rows of a facetted plot. Passed to \code{\link[ggplot2]{facet_wrap}}.}
#'  \item{ncol}{Numeric. Number of columns of a facetted plot. Passed to \code{\link[ggplot2]{facet_wrap}}.}
#'  \item{title}{Character. Passed to \code{\link[ggplot2]{labs}}.}
#'  \item{legend_title}{Character. Passed to \code{\link[ggplot2]{labs}}.}
#'  \item{x_lab}{Character. Passed to \code{\link[ggplot2]{labs}}.}
#'  \item{y_lab}{Character. Passed to \code{\link[ggplot2]{labs}}.}
#'  \item{angle, hjust}{Numeric. Adjustment of the labels of the ticks on the x-axis. Passed to \code{\link[ggplot2]{element_text}}.}
#  \item{hjust}{}
#'  \item{errorbar_width}{Numeric. Width of the whiskers. Passed to \code{\link[ggplot2]{geom_errorbar}}.}
#'  \item{ylim}{Numeric of length <= 2. Limits of the y-axis. Passed to \code{\link[ggplot2]{coord_cartesian}}.}
#'  \item{alpha}{Numeric. Alpha transparency.}
#'  \item{size}{Numeric. Factor modifying the size of points and errorbars, defaulting to 1.}
#'  \item{shape}{Numeric/Character. Shape of points. Passed to \code{\link[ggplot2]{geom_point}}.}
#'  \item{dodge_width}{Numeric. How far away from each other should different groups be plotted? Passed to \code{\link[ggplot2]{position_dodge}}.}
#' }
#' @inheritParams join_eval
# @inheritParams grDevices::pdf
# @inheritParams graphics::plot.default
#' @param ... Other parameters passed to---depending on \code{plottype}---\code{\link[ggplot2]{geom_col}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{geom_boxplot}}, or \code{\link[ggplot2]{geom_violin}}.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' tmp1 <- read_eval("./data")
#' dat1 <- join_eval(tmp1)
#' }
#'
#' # Simulate data
#' dat1 <- sim_eval()
#'
#' plot_eval(dat1)
#'
#' plot_eval(dat1, plottype = "points", domains = 1)
#'
#' plot_eval(dat1, courses = c("Course1", "Course3"), errorbar = FALSE)
#'
#' plot_eval(dat1, topics = c("Motivierung", "Note Dozent"), plottype = "boxplot")
#'
#' plot_eval(dat1, plottype = "points", topic_as = "x-axis", course_as = "group", add_line = TRUE)
#'
#' plot_eval(dat1, domains = 1:2, plottype = "violin", xvar_as_legend = TRUE,
#'           control = list(title = "My Title")) +
#'     ggplot2::theme_dark()
plot_eval <- function(data = NULL,
                      plottype = c("barplot", "points", "boxplot", "violin"),
                      courses = NULL,
                      domains = 2,
                      topics = NULL,
                      topic_as = c("facet", "group", "x-axis"),
                      course_as = c("x-axis", "facet", "group"),
                      add_line = FALSE,
                      errorbar = TRUE,
                      CI = .95,
                      grey = FALSE,
                      lang = NULL,
                      xvar_as_legend = FALSE,
                      control = list(),
                      ...
                      ) {
    DATA <- data; rm(data)

    if (!is.null(lang)) {
        lang <- match.arg(lang, choices = c("de", "en"))
    } else {
        lang <- attr(DATA, "lang")
    }

    plottype  <- match.arg(plottype)
    topic_as  <- match.arg(topic_as)
    course_as <- match.arg(course_as)

    # if (topic_as == course_as)
    if (sum(c(topic_as, course_as) %in% "x-axis") != 1)
        stop("Either 'topic_as' or 'course_as' must equal 'x-axis'.")

    ##### setup: xvar #####
    if (course_as == "x-axis") {
        xvar <- "course"
        names(xvar) <- ifelse(lang == "de", "Veranstaltung", "Course")
    } else if (topic_as == "x-axis") {
        xvar <- "topic"
        # cave: match.arg(lang) must be run before
        names(xvar) <- ifelse(lang == "de", "Bereich", "Domain")
    } else {
        stop("Either 'topic_as' or 'course_as' must equal 'x-axis'.")
    }

    ##### setup: facet #####
    # cave: match.arg(lang) must be run before
    if (topic_as == "facet") {
        fvar <- "topic"
        # names(xvar) <- ifelse(lang == "de", "Veranstaltung", "Course")
    } else if (course_as == "facet") {
        fvar <- "course"
        # names(xvar) <- ifelse(lang == "de", "Bereich", "Domain")
    } else {
        fvar <- NA
        # message("No facets.")
    }

    ##### setup: group #####
    # cave: match.arg(lang) must be run before
    if (topic_as == "group") {
        gvar <- "topic"
        names(gvar) <- ifelse(lang == "de", "Bereich", "Domain")
    } else if (course_as == "group") {
        gvar <- "course"
        names(gvar) <- ifelse(lang == "de", "Veranstaltung", "Course")
    } else {
        gvar <- NA
        # message("No groups.")
    }
    if (!is.na(gvar) & xvar_as_legend == TRUE) {
        xvar_as_legend <- FALSE
        message("'xvar_as_legend' is ignored if grouping variable is present.")
    }

    ##### checkmate #####
    checkmate::assert_data_frame(DATA, all.missing = FALSE, min.rows = 1, min.cols = 4)
    if (!all(c("course", "topic", "domain", "score") %in% names(DATA))) {
        stop("'data' must contain the columns 'course', 'topic', 'domain', and 'score'.")
    }
    if (!is.null(courses)) {
        courses <- match.arg(courses, several.ok = TRUE, choices = levels(DATA$course))
    }
    checkmate::assert_integerish(domains, lower = 1, upper = 6, max.len = 6,
                                 null.ok = TRUE)
    if (!checkmate::test_subset(as.integer(domains), unique(DATA$domain))) {
        stop("Selected domain is not present in data.")
    }
    checkmate::qassert(add_line, "B1")
    checkmate::qassert(errorbar, "B1")
    checkmate::qassert(grey, "B1")
    checkmate::qassert(CI, "R1(0,1)")
    checkmate::qassert(xvar_as_legend, "B1")
    checkmate::assert_list(control, names = "unique")
    if (!is.null(topics)) {
        topics <- match.arg(topics, several.ok = TRUE, choices = levels(DATA$topic))
    }

    ##### labels #####
    xnames <- c("domains", "topic", "mean", "rm", "xvar", "gvar") %>% {
        foo1 <- vector("list", length(.))
        setNames(foo1, .)}

    xnames[] <- list(c("Gesamt", "Gesamtbewertung", "Skalen", "Rahmenbedingungen",
                       "Referate", "Sonstiges"), "Bereich", "Mittelwert", "Bewertung",
                       names(xvar), names(gvar))
    if (lang == "en") {
        xnames[] <- list(c("Overall", "Overall", "Scales", "Setting",
                           "Presentations", "Misc"), "Domain", "Mean", "Score",
                           names(xvar), names(gvar))
    }

    ##### data #####

    if (!is.null(courses)) {
        DATA <- dplyr::filter(DATA, .data$course %in% rlang::UQ(courses))
        DATA <- droplevels(DATA)
    }

    if (is.null(topics)) {
        DATA <- dplyr::filter(DATA, .data$domain %in% rlang::UQ(domains))
        DATA <- droplevels(DATA)
    } else {
        DATA <- dplyr::filter(DATA, .data$topic %in% rlang::UQ(topics))
        DATA <- droplevels(DATA)
    }

    if (plottype %in% c("barplot", "points")) {
        DATA <- describe_eval(DATA, CI = CI)
        # if (any(DATA$N == 1) & errorbar == TRUE) {
        #     message("Errorbars are not available for courses with n=1.")
        #     # errorbar <- FALSE
        # }
    }


    ##### control #####
    con <- list(# start = 0.2, end = .6,
                nrow = NULL, ncol = NULL,
                title = ifelse(is.null(topics) & length(domains) == 1, xnames$domains[domains], ""),
                legend_title = xnames$gvar,
                x_lab = xnames$xvar,
                # y_lab = xnames$mean,
                angle = 30, hjust = 1,
                errorbar_width = 0.5,
                ylim = NULL,
                alpha = 1,
                size = 1,
                shape = 19)
    con$dodge_width <- switch(plottype,
                        "barplot" = .9,
                        "points"  = .6,
                        # "boxplot" = NULL,
                        "boxplot" = .75,
                        "violin"  = .75)
    con$y_lab <- switch(plottype,
                        "barplot" = xnames$mean,
                        "points"  = xnames$mean,
                        "boxplot" = "",
                        "violin"  = "")
    checkmate::assert_list(control, any.missing = FALSE,
                           names = "unique",
                           null.ok = TRUE)
    checkmate::assert_subset(names(control), choices = names(con))
    con[names(control)] <- control

    ##### aes #####
    xplot <- ggplot(data = DATA, aes_string(x = xvar))

    if (plottype %in% c("barplot", "points")) {
        xplot <- xplot + aes_string(y = "Mean", ymin = "LL", ymax = "UL")
    } else {
        xplot <- xplot + aes_string(y = "score")
    }

    if (!is.na(gvar))
        xplot <- xplot + aes_string(fill = gvar, color = gvar)

    ##### facet_wrap #####
    if (!is.na(fvar))
        xplot <- xplot + facet_wrap( ~ get(fvar), nrow = con$nrow, ncol = con$ncol)

    ##### geom_col #####
    if (plottype == "barplot") {
        if (xvar_as_legend == TRUE) {
            xplot <- xplot + geom_col(aes_string(fill = xvar),
                                      position = position_dodge(width = con$dodge_width),
                                      alpha = con$alpha)
            # if (grey == TRUE)
            #     xplot <- xplot + scale_fill_grey(start = con$start, end = con$end)
        } else {
            xplot <- xplot + geom_col(position = position_dodge(width = con$dodge_width),
                                      alpha = con$alpha)
        }
    }

    ##### geom_point #####
    if (plottype == "points") {
        if (xvar_as_legend == TRUE) {
            xplot <- xplot + geom_point(aes_string(color = xvar),
                                        position = position_dodge(width = con$dodge_width),
                                        alpha = con$alpha,
                                        size = 1.5*con$size,
                                        shape = con$shape, ...)
        } else {
            xplot <- xplot + geom_point(position = position_dodge(width = con$dodge_width),
                                        alpha = con$alpha, size = 1.5*con$size,
                                        shape = con$shape, ...)
        }
    }

    ##### geom_boxplot #####
        if (plottype == "boxplot") {
            if (xvar_as_legend == TRUE) {
                xplot <- xplot + geom_boxplot(aes_string(fill = xvar),
                                              position = position_dodge(width = con$dodge_width),
                                              alpha = con$alpha, ...)
            } else {
                xplot <- xplot + geom_boxplot(position = position_dodge(width = con$dodge_width),
                                              alpha = con$alpha, ...)
            }
        }

    ##### geom_violin #####
        if (plottype == "violin") {
            if (xvar_as_legend == TRUE) {
                xplot <- xplot + geom_violin(aes_string(fill = xvar),
                                             position = position_dodge(width = con$dodge_width),
                                             trim = TRUE, alpha = con$alpha, ...)
            } else {
                xplot <- xplot + geom_violin(position = position_dodge(width = con$dodge_width),
                                             trim = TRUE, alpha = con$alpha, ...)
            }
        }


    ##### geom_errorbar #####
    if (errorbar == TRUE & plottype == "barplot") {
        xplot <- xplot + geom_errorbar(aes(color = NULL),
                                       position = position_dodge(width = con$dodge_width),
                                       width = con$errorbar_width, size = 0.5*con$size)
    } else if (errorbar == TRUE & plottype == "points") {
        # xplot <- xplot + geom_errorbar(aes_string(color = xvar), position = position_dodge(width = con$dodge_width),
        #                                width = con$errorbar_width, size = 0.5*con$size)
        if (xvar_as_legend == TRUE) {
            xplot <- xplot + geom_errorbar(aes_string(color = xvar), position = position_dodge(width = con$dodge_width),
                                           width = con$errorbar_width, size = 0.5*con$size)
        } else {
            xplot <- xplot + geom_errorbar(position = position_dodge(width = con$dodge_width),
                                           width = con$errorbar_width, size = 0.5*con$size)
        }
    }

    ##### xvar_as_legend #####
    if (xvar_as_legend == TRUE) {
        # if (!is.na(gvar)) {
        #     message("'xvar_as_legend' is ignored if grouping variable is present.")
        # } else {
            xplot <- xplot +
                # aes_string(fill = xvar, color = xvar) +
                scale_x_discrete(labels = NULL) +
                theme(axis.ticks = element_blank())
            if (is.null(control$legend_title)) {
                con$legend_title <- xnames$xvar
            }
        # }
    }

    ##### geom_line #####
    if (add_line == TRUE) {
        if (plottype %in% c("barplot", "points")) {
            if (is.na(gvar)) {
                xplot <- xplot + geom_line(aes(group = 1),
                                           position = position_dodge(width = con$dodge_width))
            } else {
                xplot <- xplot + geom_line(aes(x = unclass(get(xvar)),
                                               color = get(gvar)),
                                           position = position_dodge(width = con$dodge_width))
            }

        } else {
            if (is.na(gvar)) {
                xplot <- xplot + stat_summary(fun.y = median, geom = "line",
                                              aes(group = 1),
                                              position = position_dodge(width = con$dodge_width))
            } else {
                xplot <- xplot + stat_summary(fun.y = median, geom = "line",
                                              aes(group = get(gvar)),
                                              position = position_dodge(width = con$dodge_width)
                )
            }
            # message("Argument 'add_line' has no effect for boxplots and violin plots.")
        }
    }

    ##### grey #####
    if (grey == TRUE) {
        if (plottype == "points") {
            xplot <- xplot + scale_color_grey()
        } else {
            xplot <- xplot + scale_fill_grey()
        }
    }

    ##### ylim #####
    if (plottype == "barplot") {
        ymin <- ifelse(errorbar == TRUE,
                       min(c(1, DATA$LL), na.rm = TRUE),
                       min(c(1, DATA$Mean)))
        ymax <- ifelse(errorbar == TRUE,
                       max(c(DATA$UL, DATA$Mean), na.rm = TRUE),
                       max(DATA$Mean))
        if (is.null(con$ylim)) {
            con$ylim <- c(ymin, ymax)
        } else {
            if (is.na(con$ylim[1])) con$ylim[1] <- ymin
            if (is.na(con$ylim[2])) con$ylim[2] <- ymax
        }
        xplot <- xplot + coord_cartesian(ylim = con$ylim, expand = TRUE)
    } else if (!is.null(con$ylim)) {
        if (checkmate::test_number(diff(con$ylim), null.ok = TRUE)) {
            xplot <- xplot + coord_cartesian(ylim = con$ylim, expand = TRUE)
        } else {
            warning("'control$ylim' must be a numeric vector of length 2.")
        }
    }

    ##### labs, theme #####
    xplot <- xplot + labs(title = con$title,
        color = con$legend_title, y = con$y_lab,
        x = con$x_lab, fill = con$legend_title)

    if (requireNamespace("ggthemes", quietly = TRUE)) {
        xplot <- xplot +
            ggthemes::theme_igray() +
            theme(axis.text.x = element_text(angle = con$angle, hjust = con$hjust))
    } else {
        xplot <- xplot +
            theme(axis.text.x = element_text(angle = con$angle, hjust = con$hjust))
    }

    return(xplot)
}
