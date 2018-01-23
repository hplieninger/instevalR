context("Test reading data")

tmp1 <- system.file("extdata", "InstEvaL-Rohdaten-vlg_12222-evaluationen.csv", package = "instevalR")

dat0 <- read_eval(path = dirname(tmp1))
dat1 <- join_eval(dat0)
dat2 <- describe_eval(dat1)
dat3 <- datatable_eval(dat2)

test_that("read_eval() works", {
  expect_type(dat0, "list")
})

test_that("join_eval() works", {
    expect_true(is.data.frame(dat1))
})

test_that("describe_eval() works", {
    expect_true(is.data.frame(dat2))
})

test_that("datatable_eval() works", {
    expect_is(dat3, "datatables")
    expect_error(print(dat3), NA)
})
