#' Plot multiple course evalautions
#'
#' This function takes as input the output from \code{\link{aggregate_eval}} and plots the results.
#'
#' @param x List of data frames as returned from \code{\link{aggregate_eval}}.
#' @param plottype Integer between \code{1} and \code{4}, selects the type of plot(s):
#' \describe{
#'  \item{\code{plottype = 1}}{1 plot of 4 variables, namely, the first four scales of "Gesamtbewertung"}
#'  \item{\code{plottype = 2}}{1 plot of 1 variable, namely, the scale "5: Gesamt"}
#'  \item{\code{plottype = 3}}{33 plots of all 33 variables arranged in 7 plot windows}
#'  \item{\code{plottype = 4}}{1 plot of a single, user-seleted variable (see \code{subscale})}
#' }
#' @param subscale Integer indicating the number of the scale to be plotted (if
#'   \code{plottype = 4}).
#' @param error_bars Logical indicating whether error bars representing a
#'   confidence interval should be plotted or not.
#' @param CI Numeric. Confidence level, often .95 for a 95\% CI.
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
#' @param plot.legend Logical. May be set to \code{FALSE} in order to specify a user-defined legend afterwards.
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
#' plot_eval(res.1, plottype = 2, CI = .9)
#'
#' # Plot all scales and save the plots in a pdf file
#' plot_eval(res.1, plottype = 3, pdf = TRUE)
#'
#' # Plot a specific scale (#2) with user-defined parameters
#' plot_eval(res.1, plottype = 4, subscale = 2, lwd = 2, lty = 2, col = "blue",
#'     ylim = c(1, 6), ylab = "Scale", x.labels = paste("Course", 1:5))
#' }
plot_eval <- function(x, plottype = 1, subscale = NULL, error_bars = TRUE, CI = .95,
                      x.labels = NULL, pdf = FALSE,
                      col = NULL, col.axis = "salmon", alpha = .25,
                      lwd = 3,  main = NULL, pch = 19, type = "b", ylim = NULL,
                      plot.legend = TRUE, ...) {
    opar <- par(no.readonly = TRUE)
    if (missing(x.labels)) x.labels = rownames(x$mean)
    # cols <- c("#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA")
    cols <- function(col, alpha = 1) {
#         r <- c(228,  55,  77, 152, 0)
#         g <- c(26,  126, 175,  78, 0)
#         b <- c(28,  184,  74, 163, 0)
        r <- c(215, 253, 171, 43, 0)
        g <- c(25, 174, 221, 131, 0)
        b <- c(28, 97, 164, 186, 0)
        return(rgb(red = r[col], green = g[col], blue = b[col],
                   alpha = alpha*255, maxColorValue = 255))
    }
    ymax <- as.numeric(apply(x$mean + qnorm((1-CI)/2 + CI)*x$se,
                             2, function(x) ceiling(max(x, na.rm = T))))
    ymax[ymax < 2] <- 2
    ymax[ymax > 6] <- 6
    if (plottype == 1) {
        if (missing(ylim)) ylim <- c(1, max(ymax[1:4]))
        if (missing(col)) {
            col <- cols(1:4)
        } else {
            if (length(col) == 1) col <- rep(col, 4)
        }
        ytext <- ylim[1] - diff(range(ylim))/8
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:6, y = ytext, labels = x.labels, xpd=T, srt=45, adj = .75)
        ordx <- order(colMeans(x$mean[, 1:4]))
        if (error_bars == TRUE) {
            for (jj in 1:4) {
                ii <- ordx[jj]
                Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, grep("Ges_", colnames(x$mean))[ii]],
                              yplus  = x$mean[, grep("Ges_", colnames(x$mean))[ii]] + qnorm((1-CI)/2 + CI)*x$se[, grep("Ges_", colnames(x$mean))[ii]],
                              yminus = x$mean[, grep("Ges_", colnames(x$mean))[ii]] - qnorm((1-CI)/2 + CI)*x$se[, grep("Ges_", colnames(x$mean))[ii]],
                              errbar.col = scales::alpha(col[ii], alpha = alpha), type = "n", add = T, lwd = lwd)
            }
        }
        for (jj in 1:4) {
            ii <- ordx[jj]
            lines(1:nrow(x$mean), x$mean[, grep("Ges_", colnames(x$mean))[ii]],
                  col = col[ii], lwd = lwd, type = type, pch = pch, ...)
        }
        title(main = main, ...)
        legend("topleft", legend = x$varnames[grep("Ges_", colnames(x$mean))][rev(ordx)],
               col = col[rev(ordx)], lwd = lwd, bty = "n", plot = plot.legend)
    }
    if (plottype == 2) {
        if (missing(ylim)) ylim <- c(1, max(ymax[grep("Gesamt", colnames(x$mean))]))
        if (missing(col)) col <- "black"
        if (missing(main)) main <- x$varnames[grep("Gesamt", colnames(x$mean))]
        ytext <- ylim[1] - diff(range(ylim))/8
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = x.labels)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(cex=1, x = 1:nrow(x$mean), y = ytext, labels = x.labels, xpd=T, srt=45, adj = .75)

        lines(1:nrow(x$mean), x$mean[, grep("Gesamt", colnames(x$mean))],
              lwd = lwd, type = type, pch = pch, col = col, ...)
        title(main = main, ...)
        if (error_bars == TRUE) {
            Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, grep("Gesamt", colnames(x$mean))],
                   yplus  = x$mean[, grep("Gesamt", colnames(x$mean))] + qnorm((1-CI)/2 + CI)*x$se[, grep("Gesamt", colnames(x$mean))],
                   yminus = x$mean[, grep("Gesamt", colnames(x$mean))] - qnorm((1-CI)/2 + CI)*x$se[, grep("Gesamt", colnames(x$mean))],
                   errbar.col = scales::alpha(col, alpha = alpha), type = "n", add = T, lwd = lwd)
        }
    }
    if (plottype == 3) {
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"),
                width = 7.27, height = 10.7, paper = "a4")
        }
        #         ymax <- as.numeric(apply(x$mean + qnorm((1-CI)/2 + CI)*x$se,
        #                                  2, function(x) ceiling(max(x, na.rm = T))))
        #         ymax[ymax < 2] <- 2
        #         ymax[ymax > 6] <- 6
        tab.max <- tabulate(ymax)
        ymax2 <- which(cumsum(prop.table(tab.max)) >= 2/3)[1]
        if (length(unique(ymax)) == 1) {
            ymax2 <- rep(ymax2, 2)
        } else if (ymax2 == max(ymax)) {
            qq1 <- which.max(prop.table(tab.max)[-ymax2])
            if (cumsum((prop.table(tab.max)[-ymax2]))[qq1] <= 1/3) {
                ymax2[2] <- which.max(tab.max[-ymax2])
            } else {
                ymax2 <- rep(ymax2, 2)
            }
        } else {
            ymax2[2] <- max(ymax)
        }
        while (any(!ymax %in% ymax2)) {
            ymax[!ymax %in% ymax2] <- ymax[!ymax %in% ymax2] + 1
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
            ylimx <- ifelse(rep(missing(ylim), 2), c(1, ymax[ii]), ylim)
            plot.window(xlim = c(1, nrow(x$mean)), ylim = ylimx)
            if (all.equal(ymax2[1], ymax2[2]) == TRUE) col.axis <- "black"
            axis(side = 2,
                 col.axis = ifelse(missing(ylim) & ymax[ii] == ymax2[2], col.axis, "black"))
            # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
            axis(side = 1, at = 1:nrow(x$mean), labels = F)
            ytext <- ylimx[1] - diff(range(ylimx))/8
            text(x = 1:6, y =  ytext, labels = x.labels, xpd = TRUE, srt = 45, adj = .75)
            lines(1:nrow(x$mean), x$mean[, ii],
                  col = col, lwd = lwd, type = type, pch = pch, ...)
            title(main = x$varnames[ii], ...)
            if (error_bars == TRUE) {
                Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, ii],
                       yplus  = x$mean[, ii] + qnorm((1-CI)/2 + CI)*x$se[, ii],
                       yminus = x$mean[, ii] - qnorm((1-CI)/2 + CI)*x$se[, ii],
                       errbar.col = scales::alpha(col, alpha = alpha), type = "n", add = T, lwd = lwd)
            }
        }
    }
    if (plottype == 4) {
        if (missing(subscale)) stop("Please specify argument 'subscale'!")
        if (missing(ylim)) ylim <- c(1, max(ymax[subscale]))
        if (missing(col)) col <- "black"
        if (missing(main)) main <- x$varnames[subscale]
        ytext <- ylim[1] - diff(range(ylim))/8
        if (pdf == TRUE) {
            pdf(paste0("insteval_", gsub(":", "-", substr(Sys.time(), 1, 19)), ".pdf"))
        }
        plot.new()
        plot.window(xlim = c(1, nrow(x$mean)), ylim = ylim)
        axis(side = 2)
        # axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
        axis(side = 1, at = 1:nrow(x$mean), labels = F)
        text(x = 1:nrow(x$mean), y = ytext,
             labels = x.labels,
             xpd = TRUE, srt = 45, adj = .75)

        lines(1:nrow(x$mean), x$mean[, subscale],
              col = col, lwd = lwd, type = type, pch = pch, ...)
        title(main = main, ...)
        if (error_bars == TRUE) {
            Hmisc::errbar(x = 1:nrow(x$mean), y = x$mean[, subscale],
                   yplus  = x$mean[, subscale] + qnorm((1-CI)/2 + CI)*x$se[, subscale],
                   yminus = x$mean[, subscale] - qnorm((1-CI)/2 + CI)*x$se[, subscale],
                   errbar.col = scales::alpha(col, alpha = alpha), type = "n", add = T, lwd = lwd)
        }
    }
    if (pdf == TRUE) {
        dev.off()
        cat(paste0("I wrote a pdf to ", getwd(), "\n"))
    } else par(opar)
}
