#' instevalR: An R Package For Summarizing And Plotting Several InstEvaL Course Evaluations
#'
#' InstEvaL is an online system for course evaluations, which is used mainly at
#' German universities (e.g., U Mannheim). The system
#' (\url{http://insteval.uni-mannheim.de/}) provides instructors with feedback for
#' every course that is evaluated and with the accompanying raw data. The
#' package instevalR uses the raw data of several course evaluations, combines
#' them, and provides a handful of interesting plots.
#'
#' @section Workflow:
#' \enumerate{
#'  \item{You have \code{R} and \code{instevalR} installed. Furthermore, please download the files of your course evaluations (log in to InstEvaL -> Results -> Raw data) and put them into a single directory.}
#'  \item{Then you need to import all your data into R using the function \code{\link{read_eval}}.}
#'  \item{Afterwards, you need to aggregate those data (e.g., calculate means) using \code{\link{aggregate_eval}}.}
#'  \item{Finally, you can plot your data using \code{\link{plot_eval}}. Play around with the argument \code{plottype} and generate different plots.}
#' }
#'
#' @docType package
#' @name instevalR
#' @examples
#' \dontrun{
#' # You may also simulate mock data:
#' # dat.1 <- sim_eval()
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
NULL
#> NULL
