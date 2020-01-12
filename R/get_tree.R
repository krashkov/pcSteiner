get_tree <- function (graph, depth, A_old, E_old, F_old, B_old, D_old, G) {

        E(graph)$PCST <- FALSE

        for (vert in as.integer(V(graph))) {

                if (V(graph)$root[vert] == TRUE) {
                        #
                } else {
                        maxF_value  <- -Inf
                        maxF_vertex <- as.integer(neighbors(graph, vert)[1])

                        for (neighbor in as.integer(neighbors(graph, vert))) {
                                key_out <- paste(c(vert, neighbor), collapse="")
                                for (d in 1:depth) {
                                        if (F_old[[key_out]][d] >= maxF_value) {
                                                maxF_value  <- F_old[[key_out]][d]
                                                maxF_vertex <- neighbor
                                        }
                                }
                        }

                        if (maxF_value > G[[as.character(vert)]]) {
                                E(graph)$PCST[get.edge.ids(graph, c(vert, maxF_vertex))] <- TRUE
                        }
                }

        }

        return(graph)
}
