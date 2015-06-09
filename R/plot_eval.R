#' Plot multiple course evalautions
#'
#' This function takes as input the output from \code{\link{aggregate_eval}} and plots the results.
#'
#' @param x List of data frames as returned from \code{\link{aggregate_eval}}.
#' @param plottype Integer between \code{1} and \code{4}, selects the type of plot(s):
#' \describe{
#'  \item{\code{plottype = 1}}{1 plot of 4 variables, i.e., the first four scales of "Gesamtbewertung"}
#'  \item{\code{plottype = 2}}{1 plot of 1 variable, i.e., the scale "5: Gesamt"}
#'  \item{\code{plottype = 3}}{33 plots of all 33 variables arranged in 7 plot windows}
#'  \item{\code{plottype = 4}}{1 plot of a single, user-seleted variable (see \code{subscale})}
#' }
#' @param subscale Integer indicating the number of the scale to be plotted (if
#'   \code{plottype = 4}).
#' @param error_bars Logical indicating whether error bars representing a
#'   confidence interval should be plotted or not.
#' @param ci Numeric. Confidence level, often .95 for a 95\% CI.
#' @param x.labels Optional character vector with the labels of the tick marks
#'   of the x-axis, typically names of courses/semester. If \code{NULL}, this is
#'   borrowed from the names of the *.csv-files.
#' @param pdf Logical. If \code{TRUE}, the plots are written to a pdf-file.
#' @param alpha Numeric. Transparency value for the error bars (\code{0} means
#'   fully transparent and \code{1} means opaque).
#' @param col.axis Character. If \code{plottype = 3} and \code{ylim = NULL} (its
#'   default), most of the plots have black y-axis annotations. But some of the
#'   plots differ in their \code{ylim}-values and their y-axis annotations are
#'   printed in a different color. Can be changed to \code{"black"} to override
#'   this behavior.
#' @param lwd The line width, a positive number.
#' @param col Character string. The color to be used for the lines and error bars, may be a vector of length 4 if \code{plottype = 1}.
#' @param pch Either an integer specifying a symbol or a single character to be
#'   used as the default in plotting points. See \code{\link{points}} for possible values and
#'   their interpretation.
#' @inheritParams graphics::plot.default
#' @param ... Other \link{graphical parameters} passed to \code{\link[graphics]{lines}} or \code{\link[graphics]{title}}.
#' @export
#' @examples
#' \dontrun{
#' dat.1 <- read_eval("./data/")      # read all files
#' res.1 <- aggregate_eval(dat.1)     # aggregate results for plotting
#'
#' # Default: Plot of four main scales with 95% CI
#' plot_eval(res.1)
#'
#' # Plot only the scale "Gesamt", with 90% CI
#' plot_eval(res.1, plottype = 2, ci = .9)
#'
#' # Plot all scales and save the plots in a pdf file
#' plot_eval(res.1, plottype = 3, pdf = TRUE)
#'
#' # Plot a specific scale (#2) with user-defined parameters
#' plot_eval(res.1, plottype = 4, subscale = 2, lwd = 2, lty = 2, col = "blue",
#'     ylim = c(1, 6), ylab = "Scale", x.labels =)
#' }
plot_eval <- function(x, plottype = 1, subscale = NULL, error_bars = TRUE, ci = .95,
                      x.labels = NULL, pdf = FALSE,
                      col = NULL, col.axis = "salmon", alpha = .25,
                      lwd = 3,  main = NULL, pch = 20, type = "b", ylim = NULL,
                      ...) {
    opar <- par(no.readonly = TRUE)
    if (missing(x.labels)) x.labels = rownames(x$mean)
    # cols <- c("#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA")
    cols <- function(col, alpha = 1) {
        r <- c(215, 253, 171, 43, 0)
        g <- c(25, 174, 221, 131, 0)
        b <- c(28, 97, 164, 186, 0)
        return(rgb(red = r[col], green = g[col], blue = b[col],
                   alpha = alpha*255, maxColorValue = 255))
    }
    if (plottype == 1) {
        if (missing(ylim)) ylim <- c(1, ceiling(max(x$mean[, 1:4])))
        if (missing(col)) {
            col <- cols(1:4)
        } else {
            if (length(col) == 1) col <- rep(col, 4)
            }
        ytext <- 1 - ylim[2]*12^-1
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:6, y = ytext, labels = x.labels, xpd=T, srt=45, adj = 1)
        for (ii in 1:4) {
            lines(1:nrow(x$mean), x$mean[, grep("Ges_", colnames(x$mean))[ii]],
                  col = col[ii], lwd = lwd, type = type, pch = pch, ...)
            if (error_bars == TRUE) {
                Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, grep("Ges_", colnames(x$mean))[ii]],
                       yplus  = x$mean[, grep("Ges_", colnames(x$mean))[ii]] + qnorm((1-ci)/2 + ci)*x$se[, grep("Ges_", colnames(x$mean))[ii]],
                       yminus = x$mean[, grep("Ges_", colnames(x$mean))[ii]] - qnorm((1-ci)/2 + ci)*x$se[, grep("Ges_", colnames(x$mean))[ii]],
                       errbar.col = scales::alpha(col[ii], alpha = alpha), type = "n", add = T, lwd = lwd)
            }
        }
        title(main = main, ...)
        legend("topleft", legend = x$varnames[grep("Ges_", colnames(x$mean))],
               col = col, lwd = lwd, bty = "n")
    }
    if (plottype == 2) {
        if (missing(ylim)) ylim <- c(1, ceiling(max(x$mean[, 1:4])))
        if (missing(col)) col <- "black"
        if (missing(main)) main <- x$varnames[grep("Gesamt", colnames(x$mean))]
        ytext <- 1 - ylim[2]*12^-1
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = x.labels)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:nrow(x$mean), y = ytext, labels = x.labels, xpd=T, srt=45, adj = 1)

        lines(1:nrow(x$mean), x$mean[, grep("Gesamt", colnames(x$mean))],
              lwd = lwd, type = type, pch = pch, col = col, ...)
        title(main = main, ...)
        if (error_bars == TRUE) {
            Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, grep("Gesamt", colnames(x$mean))],
                   yplus  = x$mean[, grep("Gesamt", colnames(x$mean))] + qnorm((1-ci)/2 + ci)*x$se[, grep("Gesamt", colnames(x$mean))],
                   yminus = x$mean[, grep("Gesamt", colnames(x$mean))] - qnorm((1-ci)/2 + ci)*x$se[, grep("Gesamt", colnames(x$mean))],
                   errbar.col = scales::alpha(col, alpha = alpha), type = "n", add = T, lwd = lwd)
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
        if (missing(col)) col <- "black"
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
            # text(cex=1, x = 1:6, y = ytext, labels = x.labels, xpd=T, srt=45, adj = 1)
            text(cex=1, x = 1:6, y =  1 - ymax[ii]*12^-1, labels = x.labels, xpd=T, srt=45, adj = 1)
            lines(1:nrow(x$mean), x$mean[, ii],
                  col = col, lwd = lwd, type = type, pch = pch, ...)
            title(main = x$varnames[ii], ...)
            if (error_bars == TRUE) {
                Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, ii],
                       yplus  = x$mean[, ii] + qnorm((1-ci)/2 + ci)*x$se[, ii],
                       yminus = x$mean[, ii] - qnorm((1-ci)/2 + ci)*x$se[, ii],
                       errbar.col = scales::alpha(col, alpha = alpha), type = "n", add = T, lwd = lwd)
            }
        }
    }
    if (plottype == 4) {
        if (missing(subscale)) stop("Please specify argument 'subscale'!")
        if (missing(ylim)) ylim <- c(1, ceiling(max(x$mean[, subscale])))
        if (missing(col)) col <- "black"
        if (missing(main)) main <- x$varnames[subscale]
        ytext <- 1 - ylim[2]*12^-1
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:nrow(x$mean), y = ytext, labels = x.labels, xpd=T, srt=45, adj = 1)

        lines(1:nrow(x$mean), x$mean[, subscale],
              col = col, lwd = lwd, type = type, pch = pch, ...)
        title(main = main, ...)
        if (error_bars == TRUE) {
            Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, subscale],
                   yplus  = x$mean[, subscale] + qnorm((1-ci)/2 + ci)*x$se[, subscale],
                   yminus = x$mean[, subscale] - qnorm((1-ci)/2 + ci)*x$se[, subscale],
                   errbar.col = scales::alpha(col, alpha = alpha), type = "n", add = T, lwd = lwd)
        }
    }
    if (pdf == TRUE) {
        dev.off()
        cat(paste0("\nI wrote a pdf to ", getwd(), "\n\n"))
    } else par(opar)
}
