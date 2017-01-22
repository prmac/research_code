#' Plot SM output
#'
#' @param x Predicted transitions under null model
#' @param obs Observed transitions
#' plot_sm()

plot_sm <- function(x, obs) {
  p <- ggplot(data.frame(x), aes(x)) +
          geom_histogram(binwidth=2) +
          geom_vline(xintercept=obs, lty=2, col="red") +
          xlab("Transitions") +
          ylab("Count") +
          theme_bw()

  p
}
