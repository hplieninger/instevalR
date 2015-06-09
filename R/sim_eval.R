#' Simulate data for mock course evaluations
#'
#' This function may be used to simulate data in order to use \code{\link{aggregate_eval}} and \code{\link{plot_eval}} without real data.
#'
#' @param N.files Positive integer. Number of files/courses/waves.
#' @param N.id Positive integer. Number of participants in each file/course/wave.
#' @param missing Numberic between \code{0} and \code{1} indicating the amount of missing data.
#' @export
#' @examples
#' \dontrun{
#' # Simulate data for 250 participants, 50 in each of 5 courses
#' dat.x <- sim_eval(N.files = 5, N.id = 50, missing = 0)
#' }
sim_eval <- function(N.files = 5, N.id = 50, missing = .05) {
    N.number <- 53
    file <- factor(rep(rep(paste0("Course", 1:N.files), each = N.id), N.number))
    id <- rep(seq(1, N.id*N.files), N.number)
    number <- factor(rep(1:N.number, each = N.id*N.files))

    lambda <- rep(runif(N.number, min = .25, max = 1.25), each = N.files*N.id)
    resp <- 1 + rpois(N.files*N.id*N.number, lambda = lambda)
    resp[resp > 6] <- 6

#     all(length(file) == length(id),
#         length(id) == length(number),
#         length(number) == length(resp))

    dat <- data.frame(file, id, number, resp)

    dat <- subset(dat, subset = c(1:nrow(dat)) %in% sample(nrow(dat), (1-missing)*nrow(dat)))

    return(dat)
}
