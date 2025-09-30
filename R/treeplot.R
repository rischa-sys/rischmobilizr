#' Tree Plot for Tree-based Models
#'
#' Creates a visualization of the tree created when using a tree-based model.
#'
#' @param model The tree-based model to plot.
#' @param ... Additional plotting parameters that can be passed. See help
#'   documentation for rpart.plot for additional information
#'
#'
#' @examples
#' m <- tree(survived ~ age + gender + class, data = titanic)
#' treeplot(m)
#'
#' @importFrom rpart.plot prp
#' @export

treeplot <- function(model, ...) {

  # Check the number of splits to ensure the plot won't be overly complex.
  if (class(model) == "rpart") {
    n_splits <- .tree_splits(x = model)
    max_splits <- base::max(n_splits)
  }

  # If there's more than 20 splits, stop and don't plot
  if (max_splits > 20) stop("Treeplot is too complex to plot")

  # If the model is a regression tree, change some of the plotting parameters
  if (model$method == "anova") {
    rpart.plot::prp(x = model, type=3, under=TRUE, clip.right.labs=FALSE,varlen=0, faclen=0, ...)
  } else {

    # For classification trees, use the following plotting parameters.
    rpart.plot::prp(x=model, type=3, extra=3, under=TRUE, clip.right.labs=FALSE,varlen=0, faclen=0, roundint = FALSE, ...)
  }
}

# A helper function to determine whether the model has too many branches to plot.
.tree_splits <- function(x) {
  cp_table <- x["cptable"]
  cp_df <- as.data.frame(cp_table)
  splits <- cp_df$cptable.nsplit
  return(splits)
}
