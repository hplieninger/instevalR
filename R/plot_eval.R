setwd("~/Teaching/Evaluation/")

# dat.1 <- read_eval("C:/Users/plieninger/Documents/Teaching/Evaluation/Daten/")
dat.1 <- read_eval("~/Teaching/Evaluation/Daten/")
res.1 <- comb_eval(dat.1)
res.1$mean


library("Hmisc")
library("RColorBrewer")
# display.brewer.all(n = 4, type = "qual")
# display.brewer.all(n = 4, type = "div")
# display.brewer.pal(n = 4, "Set1")
# cols <- brewer.pal(4, "Set1")
cols <- brewer.pal(4, "Spectral")


plot_eval <- function(x, error_bars = TRUE, ci = .95, plottype = 1, pdf = FALSE,
                      xlabels = NULL, type = "b", lwd = 3, alpha = .25,
                      ylim = NULL, col.axis = "salmon", subscale = NULL) {
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
        legend("topleft", legend = x$labels[grep("Ges_", colnames(x$mean))],
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
        title(main = x$labels[grep("Gesamt", colnames(x$mean))])
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
            title(main = x$labels[ii])
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
        title(main = x$labels[subscale])
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




plot_eval(res.1, plottype = 3, error_bars = T, pdf = F)

plot_eval(res.1, plottype = 2, pdf = T)
plot_eval(res.1, plottype = 4, pdf = F, subscale = 1)

plot_eval(res.1, plottype = 1, error_bars = T, pdf = T)
plot_eval(res.1, plottype = 2, type = "p", error_bars = F)



plot.new()
plot.window(xlim = c(1, nrow(x$mean)), ylim = c(1, 3))
axis(side = 2)
# axis(side = 1, at = 1:nrow(x$mean), labels = xlables)
axis(side = 1, at = 1:nrow(x$mean), labels = F)
text(cex=1, x = 1:6, y = .8, labels = xlabels, xpd=T, srt=45, adj = 1)
errbar(x = 1:6, y = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"],
       yplus = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"] + 1.96*res.1[[3]][, "Ges_Didaktische.Fähigkeiten"],
       yminus = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"] - 1.96*res.1[[3]][, "Ges_Didaktische.Fähigkeiten"],
       add = T, col = cols[1], errbar.col = cols[1], type = "b")











plot.new()
plot.window(xlim = c(1, nrow(res.1$mean)), ylim = c(1, 3))
axis(side = 2)
axis(side = 1, at = 1:nrow(res.1$mean))

for (ii in 1:5) {
    lines(1:6, res.1[, ii + 4], col = cols[ii], lwd = 3, type = "b", pch = 20)
}
# legend("topleft", bty = "n", legend = paste0("a = 2\nb = -3, -1, 2"))

errbar(x = 1:6, y = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"],
       yplus = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"] + res.1[[2]][, "Ges_Didaktische.Fähigkeiten"],
       yminus = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"] - res.1[[2]][, "Ges_Didaktische.Fähigkeiten"],
       add = T, col = cols[1], errbar.col = cols[1])

plot.new()
plot.window(xlim = c(1, nrow(res.1[[1]])), ylim = c(1, 4))
axis(side = 2)
axis(side = 1, at = 1:nrow(res.1[[1]]))
lines(1:6, res.1[[1]][, 5], col = cols[1], lwd = 3, type = "b", pch = 20)
errbar(x = 1:6, y = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"],
       yplus = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"] + 1.96*res.1[[2]][, "Ges_Didaktische.Fähigkeiten"],
       yminus = res.1[[1]][, "Ges_Didaktische.Fähigkeiten"] - 1.96*res.1[[2]][, "Ges_Didaktische.Fähigkeiten"],
       add = T, col = cols[1], errbar.col = cols[1])



plot(res.1$Ges_Didaktische.Fähigkeiten)

plot(res.1$Ges_Didaktische.Fähigkeiten, type = "b", pch = 20, ylim = c(1, 3))
plot(res.1$Ges_Note.Dozent, type = "b", pch = 20, ylim = c(1, 3))
plot(res.1$Ges_Note.LV, type = "b", pch = 20, ylim = c(1, 3))
plot(res.1$Ges_Vergleich.mit.anderen.LVn, type = "b", pch = 20, ylim = c(1, 3))


plot(res.1$`Angemessene Schwierigkeit`, type = "b", pch = 20)
plot(res.1$Motivierung, type = "b", pch = 20)
plot(res.1$Sympathie, type = "b", pch = 20, ylim = c(1, 3))
plot(res.1$Sympathie, type = "b", pch = 20, ylim = c(1, 3))

pdf("res.pdf", width = 8.27, height = 11.7, paper = "a4", onefile = T)
par(mfrow = c(3, 2))
for (ii in 1: ncol(res.1[[1]])) {
    plot(res.1[[1]][, ii], type = "b", pch = 20, ylim = c(1, 3),
         main = res.1$labels[ii])
}
dev.off()




library(plyr)
rbind.fill.matrix(t(x1), t(x2))
rbind.fill(t(x1), t(x2))






# dat.1 <- read.csv("./../02_FSS 2013/E2_Diagnostisches Praktikum II/Evaluation/InstEvaL-vlg_9677/InstEvaL-Rohdaten-vlg_9677-evaluationen.csv", sep = ";", header = F)[-1, ]
# dat.1 <- read.csv("./../06_FSS-2015/Testtheorie/Evaluation/InstEvaL-vlg_11935/InstEvaL-Rohdaten-vlg_11935-evaluationen.csv", sep = ";", header = T,
#                   colClasses = "integer")
# names(dat.1) <- c("item_nr", "item_id", "id", "resp")
# dat.2 <- reshape2::dcast(dat.1, id ~ item_id, value.var = "resp")
# dat.3 <- dat.2[, -1]

labels <- read.csv("./../06_FSS-2015/Testtheorie/Evaluation/InstEvaL-vlg_11935/InstEvaL-Rohdaten-vlg_11935-fragen_liste.csv", sep = ";", header = T)
scale <- as.character(labels$thema)
scale[scale == "Gesamtbewertung"] <- paste0("Gesamtbewertung", 1:4)
scale[scale == "Rahmenbedingungen"] <- paste0("Rahmenbedingungen", 101:111)
scale[scale == "Referate"] <- paste0("Referate", 1:6)

labels.2 <- data.frame(nummer = labels$nummer,
                       scale = factor(scale),
                       thema = labels$thema,
                       text = labels$text)




dat.2 <- data.frame(nummer = colnames(dat.1), t(dat.1))

dat.3 <- merge(x = dat.2, y = labels.2, by = "nummer")

aggregate(dat.3[, 2:9], by = list(dat.3$scale), mean, na.rm = T)









x2 <- data.frame(nummer = names(dat.3), val = colMeans(dat.3, na.rm = T))
names(dat.3)
x1 <- labels.2[labels.2$nummer %in% names(dat.3), ]
x3 <- merge(x = x1, y = x2, by.x = "nummer", by.y = "nummer")

aggregate(x2$val, by = list(x1$scale), mean)

aggregate(t(dat.1), by = list(factor(scale)), FUN = function(x) mean(x, na.rm = T))
aggregate(t(dat.1), by = list(factor(scale)), mean, na.rm = T)
