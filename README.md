<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/hplieninger/instevalR.svg?branch=master)](https://travis-ci.org/hplieninger/instevalR)

[![Coverage status](https://codecov.io/gh/hplieninger/instevalR/branch/master/graph/badge.svg)](https://codecov.io/github/hplieninger/instevalR?branch=master)

`instevalR` is an R package under construction that is useful for people doing course evaluations (at universities, e.g., U Mannheim) with the software [InstEvaL](https://insteval.uni-mannheim.de). The package has functions to combine the results of several course evaluations (e.g., different courses/cohorts of a single instructor) and to plot those results:

![](README-example-plot-1.png)

In order to use this package, you need three things:

1.  The CSV-files with the raw data of your course evaluations, which you can download from InstEvaL (log in to [InstEvaL](https://insteval.uni-mannheim.de) -&gt; Results -&gt; Raw data). Please save all your CSV-files in a single directory (they should have names ending with 'evaluationen.csv').
2.  R (which you can download from <https://www.r-project.org>)
3.  The package `instevalR`

The easiest way to install the package `instevalR` is to install it directly from GitHub:

``` r
install.packages("devtools")
devtools::install_github("hplieninger/instevalR")
```

After you have your CSV-files, R, and the package, you can get started:

``` r
library("instevalR")                             # load in every new R session
tmp1 <- read_eval("path/to/directory")           # read-in raw data
dat1 <- join_eval(tmp1)                          # combine into single data frame
plot_eval(dat1)                                  # plot
?plot_eval                                       # help; check out Examples
```

If you're unfamiliar with R, you may find the shiny app useful which you can run (after having R and instevalR installed) using `shiny_eval()` or simply go to . Furthermore, you can simulate data using `sim_eval()`, and you can create a nice summary table using `datatable_eval()`.

``` r
library("instevalR")                             # load in every new R session
shiny_eval()                                     # interactive shiny app
dat1 <- sim_eval()                               # simulate data   
plot_eval(dat1)                                  # plot
datatable_eval(dat1)
```

Happy teaching. Please send any bug reports, feature requests, or comments to `plieninger@uni-mannheim.de` (in English or German).

------------------------------------------------------------------------

This R package was developed independently from the official InstEvaL project. The author of this R package is in no way affiliated with InstEvaL, there is no conflict of interests, no money, no beer, nothing. Just for fun.
