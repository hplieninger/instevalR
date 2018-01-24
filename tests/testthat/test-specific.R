context("Test specific arguments")

tmp1 <- system.file("extdata", "InstEvaL-Rohdaten-vlg_12222-evaluationen.csv", package = "instevalR")

dat0 <- read_eval(path = tmp1, names = "foo", shiny = TRUE)
dat1 <- join_eval(dat0, subset = 1, names = "bar")

test_that("read and join works with specific arguments", {
    expect_type(dat0, "list")
    expect_is(dat1, "tbl_df")
})

p1 <- plot_eval(dat1,
                courses = "bar",
                topics = "Gesamt",
                control = list(ylim = c(NA, 7),
                               alpha = .9, size = 2)
                )

test_that("plot_elva works with specific arguments", {
    expect_is(p1, "ggplot")
    expect_error(suppressMessages(suppressWarnings(print(p1))), NA)
})
