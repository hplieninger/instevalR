#' Simulate data for mock course evaluations
#'
#' This function may be used to simulate data in order to use
#' \code{\link{join_eval}} and \code{\link{plot_eval}} without real data.
#'
#' @param N_files Positive integer. Number of files/courses/waves.
#' @param N_id Positive integer. Number of participants in each file/course/wave.
#' @param missing Numberic between \code{0} and \code{1} indicating the amount of missing data.
#' @inheritParams join_eval
#' @export
#' @examples
#' # Simulate data for 60 participants, 20 in each of 3 courses
#' dat1 <- sim_eval(N_files = 3, N_id = 20)
sim_eval <- function(N_files = 5,
                     N_id = 50,
                     missing = .05,
                     wide_format = FALSE,
                     lang = c("de", "en")) {
    lang <- match.arg(lang)
    N_number <- 53
    file <- factor(rep(rep(paste0("Course", 1:N_files), each = N_id), N_number),
                   levels = paste0("Course", 1:N_files))
    id <- rep(seq(1, N_id*N_files), N_number)
    number <- rep(1:N_number, each = N_id*N_files)



    file <- factor(rep(rep(paste0("Course", 1:N_files), each = N_id), N_number),
                   levels = paste0("Course", 1:N_files))
    id <- rep(seq(1, N_id*N_files), N_number)
    number <- rep(1:N_number, each = N_id*N_files)

    # lambda <- rep(runif(N_number, min = .25, max = 1.25), each = N_files*N_id)
    # lambda <- rep(runif(N_files*N_number, min = .25, max = 1.25), each = N_id)

    lambda <- rep(runif(N_files*N_number,
                        min = rep(seq(0.75, 0, length = N_files), N_number),
                        max = rep(seq(2, 1.25, length = N_files), N_number)), each = N_id)

    resp <- 1 + rpois(N_files*N_id*N_number, lambda = lambda)
    resp[resp > 6] <- 6

    dat <- data.frame(file, id, number, resp)

    dat2 <- dat[c(1:nrow(dat)) %in% sample(nrow(dat), (1 - missing)*nrow(dat)), ]

    dat3 <- dat2 %>%
        dplyr::mutate_at("id", dplyr::funs(factor(.))) %>%
        dplyr::select(course = file, id, item = number, value = resp)

    dat4 <- dat3 %>%
        dplyr::group_by(.data$course, id) %>%
        dplyr::filter(.data$item %in% (1:4)) %>%
        dplyr::summarise(value = mean(.data$value),
                         item = 53) %>%
        dplyr::full_join(dat3, by = c("course", "id", "item", "value"))

    topic_lb <- switch(lang,
                       "de" = "topic_de",
                       "en" = "topic_en")

    varnames_2 <- varnames[, c("number", "ordering", "domain", "topic_full", topic_lb)]


    dat5 <- dplyr::left_join(dat4, varnames_2, by = c("item" = "number")) %>%
        dplyr::select(dplyr::everything(), topic = topic_lb)

    dat6 <- dat5 %>%
        dplyr::group_by_("course", "id", "topic_full", "topic", "ordering", "domain") %>%
        dplyr::summarise(score = mean(.data$value)) %>%
        dplyr::ungroup()

    if (wide_format == FALSE) {
        attr(dat6, "lang") <- lang
        return(dat6)
    } else {
        dat7 <- reshape2::dcast(dat6, value.var = "score", course + id ~ topic)
        attr(dat7, "lang") <- lang
        attr(dat7, "long_wide") <- "wide"
        return(dat7)
    }
}
