#' Run SM test
#'
#' @param trait trait data to test
#' @param tree phylogeny to test
#' @param rep number of randomisations to perform
#' @export
#' @examples
#' sm_test()

sm_test <- function(trait, tree, rep = 999) {
  lev <- attributes(factor(trait))$levels
  cost <- 1 - diag(length(lev))
  dimnames(cost) <- list(lev,lev)
  trait <- as.numeric(trait)
  attributes(trait)$names <- tree$tip
  null_model <- matrix(NA, rep, 1)
  obs <- t(data.frame(trait))
  obs <- phyDat(t(obs), type="USER", levels=attributes(factor(obs))$levels)
  obs <- parsimony(tree, obs, method="sankoff", cost=cost)
  for (i in 1:rep){
    null <- sample(as.numeric(trait))
    attributes(null)$names <- attributes(trait)$names
    null <- t(data.frame(null))
    null <- phyDat(t(null), type="USER", levels=attributes(factor(null))$levels)
    null_model[i,] <- parsimony(tree, null,method="sankoff", cost=cost)
    p.value <- sum(obs >= null_model)/(rep + 1)
  }

  p <- plot_sm(null_model, obs)

  ggsave(plot=p,
         filename=paste0(trait, "_sm_plot.pdf"),
         device="pdf", width=10, height=10)

  res <-
    t(data.frame(Levels = length(attributes(factor(trait))$levels),
                 Observed_transitions=obs,
                 Null_model_median=median(null_model),
                 Null_model_min=min(null_model),
                 Null_model_max=max(null_model),
                 p.value))

  res
}
