library(nimble)

edges <- phy$edge
n.tip <- length(phy$tip.label)

children <- diversitree:::get.children(edges, n.tip)

children_nimble <- nimbleFunction(
  run = function(edges = integer(2), n.tip = integer(0)) {
    num_edges <- dim(edges)[1]
    max_node <- max(edges)
    children <- matrix(type = "integer", init = TRUE, value = 0, nrow = max_node, ncol = 2)
    child_count <- integer(length = max_node, value = 0)
    for(i in 1:num_edges) {
      child_count[edges[i, 1]] <- child_count[edges[i, 1]] + 1L
      if(child_count[edges[i, 1]] > 2L) {
        stop("Tree appears to be non-binary. Only binary trees are supported.")
      }
      children[edges[i, 1], child_count[edges[i, 1]]] <- edges[i, 2]
    }
    returnType(integer(2))
    return(children)
  }
)

test <- children_nimble(edges, n.tip)
test_comp <- compileNimble(children_nimble)
test_comp(edges, n.tip)

idx <- seq_len(max(edges))
tips <- seq_len(n.tip)
root <- n.tip + 1
is.tip <- idx <= n.tip

diversitree:::get.ordering(test, is.tip, root)
