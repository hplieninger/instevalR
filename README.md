<!-- README.md is generated from README.Rmd. Please edit that file -->
`instevalR` is an R package under construction that is useful for people doing course evaluations (at universities, e.g., U Mannheim) with the software InstEvaL (<https://insteval.uni-mannheim.de>). The package has functions to combine the results of multiple course evaluations and to plot those results.

In order to use this package, you need three things. \* The *.csv-files of your course evaluations, which you can downlaod from insteval (-\> Results -\> Raw data). Please save all your *.csv-files in a single directory. \* R (which you can download from <http://www.r-project.org>) \* The package `instevalR`

The easiest way to install the package `instevalR` is the following:

After you have your files, R, and the package, you can get started:

``` r
library("instevalR")                               # load in every new R session
dat.1 <- read_eval("path/to/directory")            # read-in raw data
res.1 <- comb_eval(dat.1)                          # combine into single object
plot_eval(res.1)
```

Please send any bug reports, feature requests, or comments to `plieninger@uni-mannheim.de` (in English or German).

This R package was developed independently from the official InstEvaL project. The author of this R package is in no way affiliated with InstEvaL, there is no conflict of interests, no money, no beer, nothing. Just for fun.
