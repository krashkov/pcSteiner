generate_report <- function (graph, treeEdges, tol, iter, cost) {
        print(paste("Number of edges in the tree: ", sum(treeEdges)))
        print(paste("Tolerance:                   ", tol))
        print(paste("Iterations:                  ", iter))

        if (is.connected(subgraph.edges(graph, as.numeric(E(graph))[treeEdges]))) {
                print(paste("Is connected:                ", TRUE))
        } else {
                print(paste("Is connected:                ", FALSE))
        }

        print(paste("Cost of the tree:            ", cost))
}
