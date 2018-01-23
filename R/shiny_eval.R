#' Interactive shiny app to plot several course evaluations
#'
#' Launches an interactive shiny app to upload the CSV-files of several course
#' evaluations and plot them. This can be used as an alternative to
#' \code{\link{plot_eval}}.
#'
#' @param ... Parameters passed to \code{\link[shiny]{runApp}}
#' @inheritParams shiny::runApp
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(instevalR)
#' shiny_eval()
#' }
shiny_eval <- function(display.mode = "normal", ...) {
    if (!requireNamespace("shiny", quietly = TRUE))
        stop("Please run `install.packages(c('shiny', 'shinyAce', 'shinythemes', 'shinyjs', 'shinyBS'))` first.")
    if (!requireNamespace("shinyAce", quietly = TRUE))
        stop("Please run `install.packages('shinyAce')` first.")
    if (!requireNamespace("shinythemes", quietly = TRUE))
        stop("Please run `install.packages('shinythemes')` first.")
    if (!requireNamespace("shinyjs", quietly = TRUE))
        stop("Please run `install.packages('shinyjs')` first.")
    if (!requireNamespace("shinyBS", quietly = TRUE))
        stop("Please run `install.packages('shinyBS')` first.")
    if (!requireNamespace("ggthemes", quietly = TRUE))
        stop("Please run `install.packages('ggthemes')` first.")
    if (!requireNamespace("DT", quietly = TRUE))
        stop("Please run `install.packages('DT')` first.")

    appDir <- system.file("shinyPlot", "app1", package = "instevalR")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `instevalR`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = display.mode, ...)
}
