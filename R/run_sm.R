#' Run SM test
#'
#' @param tree phylogeny to test
#' @param data dataframe of trait data, first column must contain tip labels of phylogeny
#' @param trait column name of trait of interest
#' @param rep number of randomisations to perform
#' @export
#' @examples
#' run_sm(example_tree, example_trait_data, "Location", rep=999)
#' run_sm(example_tree, example_trait_data, "Animal_human", rep=999)

run_sm <- function(tree, data, trait, rep=999) {
  tree <- drop.tip(tree, tree[["tip.label"]][! tree[["tip.label"]] %in% data[, 1]])
  data <- data[data[,1] %in% tree[["tip.label"]], ]
  tr4 <- phylo4d(tree, data, label.type="column")
  pruned <- as(extractTree(tr4), "phylo")
  ldata <- tdata(tr4, "tip")
  ldata <- ldata[pruned[["tip.label"]], trait]
  result <- sm_test(ldata, pruned, rep=rep)

  result
}

