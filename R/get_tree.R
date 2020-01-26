get_tree <- function (graph, lambda, depth, msg_old, iter) {

        edges <- NULL
        cost  <- 0

        for (vert in as.integer(V(graph))) {

                if (V(graph)$root[vert] == TRUE) {
                        #
                } else {
                        maxF_value  <- -Inf
                        maxF_vertex <- as.integer(neighbors(graph, vert)[1])

                        for (neighbor in as.integer(neighbors(graph, vert))) {
                                key_out <- paste(c(vert, neighbor), collapse="")

                                for (d in 1:depth) {
                                        if (msg_old$F[[key_out]][d] >= maxF_value) {
                                                maxF_value  <- msg_old$F[[key_out]][d]
                                                maxF_vertex <- neighbor
                                        }
                                }
                        }

                        if (maxF_value > msg_old$G[[as.character(vert)]]) {
                                edges <- c(edges, get.edge.ids(graph, c(vert, maxF_vertex)))
                                cost <- cost + E(graph)$costs[get.edge.ids(graph, c(vert, maxF_vertex))]
                        } else {
                                cost <- cost + lambda * V(graph)$prizes[vert]
                        }
                }

        }

        treeData       <- list()
        treeData$edges <- edges
        treeData$cost  <- cost

        return(treeData)
}
