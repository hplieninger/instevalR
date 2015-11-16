#' Aggregate multiple course evalautions
#'
#' This function takes as input the output from \code{\link{read_eval}} and returns a list of aggregated data (e.g., means, standard errors).
#'
#' @param dat Data frame as returned from \code{\link{read_eval}}
#' @param id Optional numeric vector identifying a subset of data sets, which should be used.
#' @inheritParams read_eval
#' @param lang Character string specifying the language of the variable names. Currently implemented only \code{"de"} for German.
#' @return Returns a list of five elements:
#' \itemize{
#'  \item{\code{mean}: For each scale and each course evaluation, the mean across all participants}
#'  \item{\code{sd}: For each scale and each course evaluation, the SD across all participants}
#'  \item{\code{se}: For each scale and each course evaluation, the SE across all participants}
#'  \item{\code{N}: For each course evaluation, the number of participants}
#'  \item{\code{varnames}: For each scale, its label}
#' }
#' @export
#' @examples
#' \dontrun{
#' dat_1 <- read_eval("./data/")      # read all files
#' res.1 <- aggregate_eval(dat_1)     # aggregate results for plotting
#' }
aggregate_eval <- function(dat, id, lang = "de", x_labels = NULL) {
    if (is.data.frame(dat) != TRUE) stop("Object 'dat' must be a data frame")
    # library("plyr")
    if (missing(id)) id <- 1:length(dat)
    dat <- dat[id]

    # varnames <- read.csv("~/Teaching/Evaluation/instevalR/data-raw/labels.csv", sep = ";", header = T)
    scale <- as.character(varnames$thema)
    varnames_2 <- varnames[, c("number", "scale")]
    # file.names <- names(dat)

    dat <- merge(dat, varnames_2, by.x = "number", by.y = "number")[, c(2, 3, 4, 5)]

    #####################

    dat_m <- plyr::ddply(dat, .variables = c("file", "scale"), .fun = plyr::colwise(mean, na.rm = T))
    dat_m <- reshape2::dcast(dat_m, formula = file ~ scale, value.var = "resp")[, -1]
    dat_sd <- plyr::ddply(dat, .variables = c("file", "scale"), .fun = plyr::colwise(sd, na.rm = T))
    dat_sd <- reshape2::dcast(dat_sd, formula = file ~ scale, value.var = "resp")[, -1]
    dat_n <- plyr::ddply(dat, .variables = c("file", "scale"), .fun = nrow)
    dat_n <- reshape2::dcast(dat_n, formula = file ~ scale, value.var = "V1")[, -1]
    dat_se <- dat_sd / sqrt(dat_n)

    if (is.null(x_labels)) {
        rownames(dat_m) <- rownames(dat_sd) <- rownames(dat_n) <- rownames(dat_se) <- levels(dat$file)
    } else {
        rownames(dat_m) <- rownames(dat_sd) <- rownames(dat_n) <- rownames(dat_se) <- x_labels
    }

    # colnames(dat_m) <- colnames(dat_sd) <- colnames(dat_n) <- colnames(dat_se) <- varnames_2$scale[order(varnames_2$number)]

    varnames_2$scale[order(varnames_2$number)]

    #####################

#     dat_2 <- lapply(dat_1, function(x) data.frame(number = colnames(x), t(x)))
#     dat_3 <- lapply(dat_2, function(x) merge(x = x, y = varnames_2, by = "number"))
#     dat_4 <- lapply(dat_3, function(x) aggregate(x[, -c(1, ncol(x))], list(x[, "scale"]), mean, na.rm = T))
#     dat_5 <- lapply(dat_4, function(x) {
#         y <- data.frame(t(x[, -1]))
#         colnames(y) <- x[, 1]
#         return (y)
#     })
#
#     dat_5 <- lapply(dat_5, function(x) {
#         cbind(x, Gesamt = rowMeans(x[, grep("Ges_", colnames(dat_5[[1]]))], na.rm = T))
#     })
#
#     dat_m <- rbind.fill(lapply(dat_5, function(x) data.frame(t(colMeans(x, na.rm = T)))))
#     dat_sd <- rbind.fill(lapply(dat_5, function(x) {
#         data.frame(t(apply(x, 2, sd, na.rm = T)))
#     }))
#     dat_se <- rbind.fill(lapply(dat_5, function(x) {
#         data.frame(t(apply(x, 2, sd, na.rm = T)))/sqrt(nrow(x))
#     }))
#     row.names(dat_m) <- row.names(dat_sd) <- row.names(dat_se) <- file.names

    x1 <- c(grep("Ges", colnames(dat_m)),
            grep("Rahm", colnames(dat_m)),
            grep("Ref", colnames(dat_m)),
            grep("Sonst_", colnames(dat_m)))
    x2 <- (1:ncol(dat_m))[!(1:ncol(dat_m) %in% x1)]
    x3 <- c(grep("Ges", colnames(dat_m)),
            x2,
            grep("Rahm", colnames(dat_m)),
            grep("Ref", colnames(dat_m)),
            grep("Sonst_", colnames(dat_m)))
    dat_m <- dat_m[, x3]
    dat_sd <- dat_sd[, x3]
    dat_n <- dat_n[, x3]
    dat_se <- dat_se[, x3]

    # N <- unlist(lapply(dat_4, ncol)) - 1

    return(list(mean = dat_m, sd = dat_sd, se = dat_se, N = dat_n,
                varnames = varnames$labels[match(colnames(dat_m), varnames_2$scale)]))
}
