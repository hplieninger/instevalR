library("magrittr")

context("Test various plotting parameters")

X <- sample(6, 1)
N <- sample(20, 1)

dat1 <- sim_eval(N_files = X, N_id = N, lang = sample(c("de", "en"), 1), missing = 0)

test_that("'sim_eval' works correct", {
    expect_true(is.data.frame(dat1))
    expect_equal(nrow(dat1), X*N*33)
})

args <- expand.grid(plottype = c("points", "boxplot", "violin", "barplot"),
                    topic_as = c("facet", "group"),
                    course_as = "x-axis",
                    add_line = c(TRUE, FALSE),
                    errorbar = c(TRUE, FALSE),
                    grey = c(TRUE, FALSE),
                    KEEP.OUT.ATTRS = F,
                    stringsAsFactors = F) %>%
    rbind(expand.grid(plottype = c("points", "boxplot", "violin", "barplot"),
                      topic_as = "x-axis",
                      course_as = c("facet", "group"),
                      add_line = c(TRUE, FALSE),
                      errorbar = c(TRUE, FALSE),
                      grey = c(TRUE, FALSE),
                      KEEP.OUT.ATTRS = F,
                      stringsAsFactors = F))

args[args$plottype == "violin", "errorbar"]  <- FALSE
args[args$plottype == "boxplot", "errorbar"] <- FALSE

args <- args[!duplicated(args), ]

test_that("'plot_eval' returns a ggplot object", {
    for (ii in seq_len(nrow(args))) {
        p1 <- plot_eval(dat1,
                        plottype       = args$plottype[[ii]],
                        domains        = sample(c(1:4, 6), 1),
                        topic_as       = args$topic_as[[ii]],
                        course_as      = args$course_as[[ii]],
                        errorbar       = args$errorbar[[ii]],
                        grey           = args$grey[[ii]],
                        add_line       = args$add_line[[ii]],
                        xvar_as_legend = sample(c(TRUE, FALSE), 1),
                        lang           = sample(c("de", "en"), 1)
                        # , control = list(ylim = NULL,
                        #                  alpha = .9, size = 2)
        )
        expect_is(p1, "ggplot", label =
                      paste0("class of plot_eval() with arguments ", paste(names(args), args[ii, ], sep = " = ", collapse = ", "), "   ")
        )
        expect_error(suppressMessages(suppressWarnings(print(p1))), NA, label =
                         paste0("print(plot_eval()) with arguments ", paste(names(args), args[ii, ], sep = " = ", collapse = ", "), "   ")
        )
        # suppressMessages(suppressWarnings(print(p1)))
    }
})
