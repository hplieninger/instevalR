#' Combine and aggregate multiple course evalautions
#'
#' This function takes as input the output from \code{read_eval} and returns a list of aggregated data (e.g., means, standard errors).
#'
#' @param dat List of data frames as returned from \code{read_eval}
#' @inheritParams read_eval
#' @return Returns a list of five elements:
#' \describe{
#'  \item{mean}{For each scale and each course evaluation, the mean across all participants}
#'  \item{sd}{For each scale and each course evaluation, the SD across all participants}
#'  \item{se}{For each scale and each course evaluation, the SE across all participants}
#'  \item{N}{For each course evaluation, the number of participants}
#'  \item{labels}{For each scale, its label}
#' }
#' @export
#' @importFrom plyr rbind.fill
#' @examples
#' \dontrun{
#' dat.1 <- read_eval("./data/")            # read all files
#' res.1 <- comb_eval(dat.1)                # combine results for plotting
#' }
comb_eval <- function(dat, id) {
    # library("plyr")
    if (missing(id)) id <- 1:length(dat)
    dat <- dat[id]

    # labels <- read.csv("./../06_FSS-2015/Testtheorie/Evaluation/InstEvaL-vlg_11935/InstEvaL-Rohdaten-vlg_11935-fragen_liste.csv", sep = ";", header = T)
    # labels <- read.csv("./../06_FSS-2015/Testtheorie/Evaluation/InstEvaL-vlg_11935/labels.csv", sep = ";", header = T)
    labels.2 <- labels[, c("nummer", "scale")]
    scale <- as.character(labels$thema)
    scale[scale == "Gesamtbewertung"] <- c("Ges_Didaktische Fähigkeiten",
                                           "Ges_Note Dozent",
                                           "Ges_Note LV",
                                           "Ges_Vergleich mit anderen LVn")
    scale[scale == "Rahmenbedingungen"] <- paste0("Rahmenbedingungen", 101:111)
    scale[scale == "Referate"] <- paste0("Referate", 1:6)
    scale <- gsub(" ", ".", scale)

    labels.2 <- data.frame(nummer = labels$nummer
                           , scale = factor(scale)
                           , thema = labels$thema
                           , text = labels$text
    )
    file.names <- names(dat)

    #####################

    dat.2 <- lapply(dat.1, function(x) data.frame(nummer = colnames(x), t(x)))
    dat.3 <- lapply(dat.2, function(x) merge(x = x, y = labels.2, by = "nummer"))
    dat.4 <- lapply(dat.3, function(x) aggregate(x[, -c(1, ncol(x))], list(x[, "scale"]), mean, na.rm = T))
    dat.5 <- lapply(dat.4, function(x) {
        y <- data.frame(t(x[, -1]))
        colnames(y) <- x[, 1]
        return (y)
    })

    dat.5 <- lapply(dat.5, function(x) {
        cbind(x, Gesamt = rowMeans(x[, grep("Ges_", colnames(dat.5[[1]]))], na.rm = T))
    })

    dat.m <- rbind.fill(lapply(dat.5, function(x) data.frame(t(colMeans(x, na.rm = T)))))
    dat.sd <- rbind.fill(lapply(dat.5, function(x) {
        data.frame(t(apply(x, 2, sd, na.rm = T)))
    }))
    dat.se <- rbind.fill(lapply(dat.5, function(x) {
        data.frame(t(apply(x, 2, sd, na.rm = T)))/nrow(x)
    }))
    row.names(dat.m) <- row.names(dat.sd) <- row.names(dat.se) <- file.names

    x1 <- c(grep("Ges", colnames(dat.m)),
            grep("Ref", colnames(dat.m)),
            grep("Rahm", colnames(dat.m)),
            grep("Sonst_", colnames(dat.m)))
    x2 <- (1:ncol(dat.m))[!(1:ncol(dat.m) %in% x1)]
    x3 <- c(grep("Ges", colnames(dat.m)),
            x2,
            grep("Ref", colnames(dat.m)),
            grep("Rahm", colnames(dat.m)),
            grep("Sonst_", colnames(dat.m)))
    dat.m <- dat.m[, x3]
    dat.sd <- dat.sd[, x3]
    dat.se <- dat.se[, x3]

    N <- unlist(lapply(dat.4, ncol)) - 1

    return(list(mean = dat.m, sd = dat.sd, se = dat.se, N = N,
                labels = labels$labels[match(colnames(dat.m), labels.2$scale)]))
}
