loop <- function (graph, depth, permutation) {

        for (vert in permutation) {
                if (V(graph)$root[vert] == TRUE) {
                        # todo
                }

                sumE <- rep(0, depth)
                sumD <- 0

                findA <- new.env()
                initialize_findA(depth = depth, size = length(neighbors(graph, vert)))

                # Compute sum of E's and D's
                for (neighbor in as.integer(neighbors(graph, vert))) {
                        key_in  <- paste(neighbor, vert, collapse="")
                        key_out <- paste(vert, neighbor, collapse="")

                        for (d in 2:depth) {
                                sumE[d] <- sumE[d] + E_old[[key_in]][d]
                                assign(
                                        values[[d]],
                                            - E(graph)$costs[get.edge.ids(graph, c(vert, neighbor))]
                                            - E_old[[key_in]][d]
                                            + A_old[[key_in]][d-1],
                                        envir = findA
                                )
                        }
                        sumD <- sumD + D_old[[key_in]]
                }

                # Compute new messages
                for (neighbor in as.integer(neighbors(g, vert))) {
                        key_in  <- paste(neighbor, vert, collapse="")
                        key_out <- paste(vert, neighbor, collapse="")
                }

        }

        return (TRUE)
}
