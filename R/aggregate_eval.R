#' Aggregate multiple course evalautions
#'
#' This function takes as input the output from \code{read_eval} and returns a list of aggregated data (e.g., means, standard errors).
#'
#' @param dat Data frame as returned from \code{read_eval}
#' @inheritParams read_eval
#' @return Returns a list of five elements:
#' \describe{
#'  \item{mean}{For each scale and each course evaluation, the mean across all participants}
#'  \item{sd}{For each scale and each course evaluation, the SD across all participants}
#'  \item{se}{For each scale and each course evaluation, the SE across all participants}
#'  \item{N}{For each course evaluation, the number of participants}
#'  \item{varnames}{For each scale, its label}
#' }
#' @export
#' @importFrom plyr ddply
#' @importFrom reshape2 dcast
#' @examples
#' \dontrun{
#' dat.1 <- read_eval("./data/")      # read all files
#' res.1 <- aggregate_eval(dat.1)     # aggregate results for plotting
#' }
aggregate_eval <- function(dat, id) {
    if (is.data.frame(dat) != TRUE) stop("Object 'dat' must be a data frame")
    # library("plyr")
    if (missing(id)) id <- 1:length(dat)
    dat <- dat[id]

    # varnames <- read.csv("~/Teaching/Evaluation/instevalR/data-raw/labels.csv", sep = ";", header = T)
    scale <- as.character(varnames$thema)
    varnames.2 <- varnames[, c("number", "scale")]
    # file.names <- names(dat)

    dat <- merge(dat, varnames.2, by.x = "number", by.y = "number")[, c(2, 3, 4, 5)]

    #####################

    dat.m <- ddply(dat, .variables = c("file", "scale"), .fun = colwise(mean, na.rm = T))
    dat.m <- dcast(dat.m, formula = file ~ scale, value.var = "resp")[, -1]
    dat.sd <- ddply(dat, .variables = c("file", "scale"), .fun = colwise(sd, na.rm = T))
    dat.sd <- dcast(dat.sd, formula = file ~ scale, value.var = "resp")[, -1]
    dat.n <- ddply(dat, .variables = c("file", "scale"), .fun = nrow)
    dat.n <- dcast(dat.n, formula = file ~ scale, value.var = "V1")[, -1]
    dat.se <- dat.sd / sqrt(dat.n)

    rownames(dat.m) <- rownames(dat.sd) <- rownames(dat.n) <- rownames(dat.se) <- levels(dat.1$file)
    # colnames(dat.m) <- colnames(dat.sd) <- colnames(dat.n) <- colnames(dat.se) <- varnames.2$scale[order(varnames.2$number)]

    varnames.2$scale[order(varnames.2$number)]

    #####################

#     dat.2 <- lapply(dat.1, function(x) data.frame(number = colnames(x), t(x)))
#     dat.3 <- lapply(dat.2, function(x) merge(x = x, y = varnames.2, by = "number"))
#     dat.4 <- lapply(dat.3, function(x) aggregate(x[, -c(1, ncol(x))], list(x[, "scale"]), mean, na.rm = T))
#     dat.5 <- lapply(dat.4, function(x) {
#         y <- data.frame(t(x[, -1]))
#         colnames(y) <- x[, 1]
#         return (y)
#     })
#
#     dat.5 <- lapply(dat.5, function(x) {
#         cbind(x, Gesamt = rowMeans(x[, grep("Ges_", colnames(dat.5[[1]]))], na.rm = T))
#     })
#
#     dat.m <- rbind.fill(lapply(dat.5, function(x) data.frame(t(colMeans(x, na.rm = T)))))
#     dat.sd <- rbind.fill(lapply(dat.5, function(x) {
#         data.frame(t(apply(x, 2, sd, na.rm = T)))
#     }))
#     dat.se <- rbind.fill(lapply(dat.5, function(x) {
#         data.frame(t(apply(x, 2, sd, na.rm = T)))/sqrt(nrow(x))
#     }))
#     row.names(dat.m) <- row.names(dat.sd) <- row.names(dat.se) <- file.names

    x1 <- c(grep("Ges", colnames(dat.m)),
            grep("Rahm", colnames(dat.m)),
            grep("Ref", colnames(dat.m)),
            grep("Sonst_", colnames(dat.m)))
    x2 <- (1:ncol(dat.m))[!(1:ncol(dat.m) %in% x1)]
    x3 <- c(grep("Ges", colnames(dat.m)),
            x2,
            grep("Rahm", colnames(dat.m)),
            grep("Ref", colnames(dat.m)),
            grep("Sonst_", colnames(dat.m)))
    dat.m <- dat.m[, x3]
    dat.sd <- dat.sd[, x3]
    dat.n <- dat.n[, x3]
    dat.se <- dat.se[, x3]

    # N <- unlist(lapply(dat.4, ncol)) - 1

    return(list(mean = dat.m, sd = dat.sd, se = dat.se, N = dat.n,
                varnames = varnames$labels[match(colnames(dat.m), varnames.2$scale)]))
}
