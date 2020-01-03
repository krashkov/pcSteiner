loop <- function (graph, lambda, depth, permutation) {

        for (vert in permutation) {
                if (V(graph)$root[vert] == TRUE) {
                        # todo
                }

                sumE <- rep(0, depth)
                sumD <- 0

                findA <- new.env()
                initialize_findA(depth = depth, size = length(neighbors(graph, vert)))

                # Compute sum of E's and D's
                neighbor_idx <- 1
                for (neighbor in as.integer(neighbors(graph, vert))) {
                        key_in  <- paste(neighbor, vert, collapse="")
                        key_out <- paste(vert, neighbor, collapse="")

                        # Compute sum of E's
                        for (d in 2:depth) {
                                sumE[d] <- sumE[d] + E_old[[key_in]][d]

                                findA$values[[d]][neighbor_idx] <- - E(graph)$costs[get.edge.ids(graph, c(vert, neighbor))]
                                                                   - E_old[[key_in]][d]
                                                                   + A_old[[key_in]][d-1]
                        }

                        # Compute sum of D's
                        sumD <- sumD + D_old[[key_in]]

                        neighbor_idx <- neighbor_idx + 1
                }

                # Compute new messages
                neighbor_idx <- 1
                for (neighbor in as.integer(neighbors(g, vert))) {
                        key_in  <- paste(neighbor, vert, collapse="")
                        key_out <- paste(vert, neighbor, collapse="")

                        # Compute B
                        B_new[[key_out]] <- - lambda*V(graph)$prizes[vert]
                                            + sumD
                                            - D_old[[key_in]][d]

                        # Compute maxA
                        maxA <- -Inf
                        for (d in 1:depth) {
                                A_new[[key_out]][d] <-   sumE[d]
                                                       - E_old[[key_in]][d]
                                                       + max(findA$values[[d]][-neighbor_idx])
                                maxA <- max(maxA, A_new[[key_out]][d])
                        }

                        # Compute D
                        D_new[[key_out]] <- max(B_new[[key_out]], maxA)

                        # Compute E
                        C <- -Inf
                        for (d in depth:2) {
                                E_new[[key_out]][d] <- max(D_new[[key_out]], C)
                                C <- - lambda*V(graph)$prizes[vert]
                                     + sumE[d]
                                     - E_old[[key_in]][d]
                        }
                        E_new[[key_out]][1] <- D_new[[key_out]]

                        neighbor_idx <- neighbor_idx + 1

                }

        }

        return (TRUE)
}
