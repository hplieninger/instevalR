<!-- README.md is generated from README.Rmd. Please edit that file -->
`instevalR` is an R package under construction that is useful for people doing course evaluations (at universities, e.g., U Mannheim) with the software InstEvaL (<https://insteval.uni-mannheim.de>). The package has functions to combine the results of multiple course evaluations (e.g., different courses/cohorts of a single instructor) and to plot those results:

![](README_files/figure-markdown_github/example-plot-1.png)

In order to use this package, you need three things:

1.  The *.csv-files of your course evaluations, which you can downlaod from insteval (insteval -\> Results -\> Raw data). Please save all your *.csv-files in a single directory.
2.  R (which you can download from <http://www.r-project.org>)
3.  The package `instevalR`

The easiest way to install the package `instevalR` is to install it directly from GitHub:

``` r
install.packages("devtools")                      # if it's not already installed
devtools::install_github("hplieninger/instevalR")
```

After you have your \*.csv-files, R, and the package, you can get started:

``` r
library("instevalR")                               # load in every new R session
dat.1 <- read_eval("path/to/directory")            # read-in raw data
res.1 <- aggregate_eval(dat.1)                     # combine into single object
plot_eval(res.1)                                   # do a plot
```

If you don't have \*.csv-files (yet) or just want to explore the package, you may do this using simulated data:

``` r
library("instevalR")                               # load in every new R session
dat.1 <- sim_eval()                                # simulate raw data
res.1 <- aggregate_eval(dat.1)                     # combine into single object
plot_eval(res.1)                                   # do a plot
```

Happy teaching. Please send any bug reports, feature requests, or comments to `plieninger@uni-mannheim.de` (in English or German).

------------------------------------------------------------------------

This R package was developed independently from the official InstEvaL project. The author of this R package is in no way affiliated with InstEvaL, there is no conflict of interests, no money, no beer, nothing. Just for fun.
