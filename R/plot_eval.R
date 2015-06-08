#' Plot multiple course evalautions
#'
#' This function takes as input the output from \link{\code{aggregate_eval}} and plots the results.
#'
#' @param x List of data frames as returned from \link{\code{aggregate_eval}}.
#' @param plottype Integer between \code{1} and \code{4}, selects the type of plot(s):
#' \describe{
#'  \item{\code{plottype = 1}}{1 plot of 4 variables, i.e., the first four scales of "Gesamtbewertung"}
#'  \item{\code{plottype = 2}}{1 plot of 1 variable, i.e., the scale "5: Gesamt"}
#'  \item{\code{plottype = 3}}{33 plots of all 33 variables arranged in 7 plot windows}
#'  \item{\code{plottype = 4}}{1 plot of a single, user-seleted variable, see \code{subscale}}
#' }
#' @param subscale Integer indicating the number of the scale to be plotted (if \code{plottype = 4}).
#' @param error_bars Logical indicating whether error bars representing a confidence interval should be plotted or not.
#' @param ci Numeric. Confidence level, usually .95 (or .90) for a 95\% CI.
#' @param pdf Logical. If \code{TRUE}, the plots are written to a pdf-file.
#' @param xlabels Optional character vector with the labels of the tick marks of the x-axis, typically names of courses/semester. If \code{NULL}, this is borrowed from the names of the *.csv-files.
#' @param alpha
#' @param col.axis
#' @inheritParams plot.default
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
#' dat.1 <- read_eval("./data/")            # read all files
#' res.1 <- comb_eval(dat.1)                # combine results for plotting
#' }
plot_eval <- function(x, plottype = 1, subscale = NULL, error_bars = TRUE, ci = .95,
                      pdf = FALSE, type = "b", lwd = 3, ylim = NULL,
                      xlabels = NULL, alpha = .25, col.axis = "salmon") {
    opar <- par(no.readonly = TRUE)
    if (missing(xlabels)) xlabels = rownames(x$mean)
    # cols <- c("#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA")
    cols <- function(col, alpha = 1) {
        r <- c(215, 253, 171, 43, 0)
        g <- c(25, 174, 221, 131, 0)
        b <- c(28, 97, 164, 186, 0)
        return(rgb(r = r[col], g = g[col], b = b[col], a = alpha*255, maxColorValue = 255))
    }
    if (plottype == 1) {
        if (missing(ylim)) ylim <- c(1, ceiling(max(x$mean[, 1:4])))
        ytext <- 1 - ylim[2]*12^-1
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:6, y = ytext, labels = xlabels, xpd=T, srt=45, adj = 1)
        for (ii in 1:4) {
            lines(1:nrow(x$mean), x$mean[, grep("Ges_", colnames(x$mean))[ii]],
                  col = cols(ii), lwd = lwd, type = "b", pch = 20)
            if (error_bars == TRUE) {
                errbar(x = 1:nrow(x$mean), y = x$mean[, grep("Ges_", colnames(x$mean))[ii]],
                       yplus  = x$mean[, grep("Ges_", colnames(x$mean))[ii]] + qnorm((1-ci)/2 + ci)*x$se[, grep("Ges_", colnames(x$mean))[ii]],
                       yminus = x$mean[, grep("Ges_", colnames(x$mean))[ii]] - qnorm((1-ci)/2 + ci)*x$se[, grep("Ges_", colnames(x$mean))[ii]],
                       errbar.col = cols(ii, alpha = alpha), type = "n", add = T, lwd = lwd)
            }
        }
        legend("topleft", legend = x$varnames[grep("Ges_", colnames(x$mean))],
               col = cols(1:4), lwd = lwd, bty = "n")
    }
    if (plottype == 2) {
        if (missing(ylim)) ylim <- c(1, ceiling(max(x$mean[, 1:4])))
        ytext <- 1 - ylim[2]*12^-1
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:nrow(x$mean), y = ytext, labels = xlabels, xpd=T, srt=45, adj = 1)

        lines(1:nrow(x$mean), x$mean[, grep("Gesamt", colnames(x$mean))],
              lwd = lwd, type = type, pch = 20)
        title(main = x$varnames[grep("Gesamt", colnames(x$mean))])
        if (error_bars == TRUE) {
            errbar(x = 1:nrow(x$mean), y = x$mean[, grep("Gesamt", colnames(x$mean))],
                   yplus  = x$mean[, grep("Gesamt", colnames(x$mean))] + qnorm((1-ci)/2 + ci)*x$se[, grep("Gesamt", colnames(x$mean))],
                   yminus = x$mean[, grep("Gesamt", colnames(x$mean))] - qnorm((1-ci)/2 + ci)*x$se[, grep("Gesamt", colnames(x$mean))],
                   errbar.col = cols(5, alpha = alpha), type = "n", add = T, lwd = lwd)
        }
    }
    if (plottype == 3) {
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"),
                width = 7.27, height = 10.7, paper = "a4")
        }
        apply(x$mean, 2, function(x) ceiling(max(x)))
        x1 <- as.numeric(apply(x$mean, 2, function(x) ceiling(max(x, na.rm = T))))
        ymax <- quantile(apply(x$mean, 2, function(x) ceiling(max(x, na.rm = T))), probs = c(2/3, 1), names = F)
        x1[x1 <= ymax[1]] <- ymax[1]
        x1[x1 > ymax[1]] <- ymax[2]
        if (missing(ylim)) {
            ymax <- as.numeric(apply(x$mean, 2, function(x) ceiling(max(x, na.rm = T))))
            ymax2 <- quantile(ymax, probs = c(2/3, 1), names = F)
            ymax[ymax <= ymax2[1]] <- ymax2[1]
            ymax[ymax > ymax2[1]] <- ymax2[2]
        }
        par(mfrow = c(3, 2))
        for (ii in 1:ncol(x$mean)) {
            if (ii %in% c(6, 16, 27)) {
                plot.new()
                if (ii %in% c(16)) {
                    plot.new()
                }
            }
            plot.new()
            plot.window(xlim = c(1, nrow(x$mean)),
                        ylim = ifelse(rep(missing(ylim), 2), c(1, ymax[ii]), ylim))
            axis(side = 2,
                 col.axis = ifelse(missing(ylim) & ymax[ii] == max(ymax), col.axis, "black"))
            # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
            axis(side = 1, at = 1:nrow(x$mean), labels = F)
            # text(cex=1, x = 1:6, y = ytext, labels = xlabels, xpd=T, srt=45, adj = 1)
            text(cex=1, x = 1:6, y =  1 - ymax[ii]*12^-1, labels = xlabels, xpd=T, srt=45, adj = 1)
            lines(1:nrow(x$mean), x$mean[, ii],
                  col = cols(5), lwd = lwd, type = "b", pch = 20)
            title(main = x$varnames[ii])
            if (error_bars == TRUE) {
                errbar(x = 1:nrow(x$mean), y = x$mean[, ii],
                       yplus  = x$mean[, ii] + qnorm((1-ci)/2 + ci)*x$se[, ii],
                       yminus = x$mean[, ii] - qnorm((1-ci)/2 + ci)*x$se[, ii],
                       errbar.col = cols(5, alpha = alpha), type = "n", add = T, lwd = lwd)
            }
        }
    }
    if (plottype == 4) {
        if (missing(ylim)) ylim <- c(1, ceiling(max(x$mean[, subscale])))
        ytext <- 1 - ylim[2]*12^-1
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:nrow(x$mean), y = ytext, labels = xlabels, xpd=T, srt=45, adj = 1)

        lines(1:nrow(x$mean), x$mean[, subscale],
              lwd = lwd, type = type, pch = 20)
        title(main = x$varnames[subscale])
        if (error_bars == TRUE) {
            errbar(x = 1:nrow(x$mean), y = x$mean[, subscale],
                   yplus  = x$mean[, subscale] + qnorm((1-ci)/2 + ci)*x$se[, subscale],
                   yminus = x$mean[, subscale] - qnorm((1-ci)/2 + ci)*x$se[, subscale],
                   errbar.col = cols(5, alpha = alpha), type = "n", add = T, lwd = lwd)
        }
    }
    if (pdf == TRUE) {
        dev.off()
    } else par(opar)
}
