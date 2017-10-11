library(nimble)
library(diversitree)

set.seed(9102017)

pars <- c(.1,  .15,  .2,  # lambda 1, 2, 3
          .03, .045, .06, # mu 1, 2, 3
          .05, 0,         # q12, q13
          .05, .05,       # q21, q23
          0,   .05)       # q31, q32

phy <- tree.musse(pars, max.taxa = 30, x0=1)
plot(phy)
phy$tip.state

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

children <- children_nimble(edges, n.tip)
test_comp <- compileNimble(children_nimble)
test_comp(edges, n.tip)

idx <- seq_len(max(edges))
tips <- seq_len(n.tip)
root <- n.tip + 1
is.tip <- idx <= n.tip

diversitree:::get.ordering(test, is.tip, root)

get_ordering_nimble <- nimbleFunction(
  run = function(children = integer(2), n.tip = integer(0), root = integer(0)) {
    n_nodes <- dim(children)[1] - n.tip
    node_order <- integer(length = n_nodes, value = 0)
    node_order[1] <- root
    iter <- 2L
    
    i <- root
    while(length(i) > 0) {
      kids <- children[i, ]
      i <- kids[kids > n.tip]
      if(length(i) > 0) {
        i_2 <- i
        for(j in 1:length(i)) {
          i_2[j] <- i[length(i) - j + 1]
        }
        node_order[iter:(iter + length(i) - 1)] <- i_2
        iter <- iter + length(i)
      }
    }
    node_order_rev <- node_order
    for(j in 1:length(node_order)) {
      node_order_rev[j] <- node_order[length(node_order) - j + 1]
    }
    returnType(integer(1))
    return(node_order_rev)
  }
)

test2 <- get_ordering_nimble(children, n.tip, root)
test2

library(phangorn)


