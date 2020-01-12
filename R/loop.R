loop <- function (graph, lambda, depth, permutation,
                  A_new, E_new, F_new, B_new, D_new,
                  A_old, E_old, F_old, B_old, D_old,
                  G) {

        for (vert in permutation) {

                if (V(graph)$root[vert] == TRUE) {
                        # Update root
                        for (neighbor in as.integer(neighbors(graph, vert))) {
                                key_out <- paste(c(vert, neighbor), collapse="")

                                for (d in 1:depth) {
                                        E_new[[key_out]][d] <- 0
                                        A_new[[key_out]][d] <- -Inf
                                }
                                A_new[[key_out]][1] <- 0

                                B_new[[key_out]] <- -Inf
                                D_new[[key_out]] <- 0
                        }

                } else {

                        sumE <- rep(0, depth)
                        sumD <- 0

                        # Init new enviroment
                        findA <- initialize_findA(depth = depth, size = length(neighbors(graph, vert)))

                        # Compute sum of E's and D's
                        neighbor_idx <- 1
                        for (neighbor in as.integer(neighbors(graph, vert))) {
                                key_in  <- paste(c(neighbor, vert), collapse="")
                                key_out <- paste(c(vert, neighbor), collapse="")

                                # Compute sum of E's
                                for (d in 2:depth) {
                                        sumE[d] <- sumE[d] + E_old[[key_in]][d]

                                        findA$values[[d]][neighbor_idx] <- (- E(graph)$costs[get.edge.ids(graph, c(vert, neighbor))]
                                                                            - E_old[[key_in]][d]
                                                                            + A_old[[key_in]][d-1])
                                }
                                sumE[1] <- sumE[1] + E_old[[key_in]][1]

                                # Compute sum of D's
                                sumD <- sumD + D_old[[key_in]]

                                neighbor_idx <- neighbor_idx + 1
                        }

                        # Compute new messages
                        neighbor_idx <- 1
                        for (neighbor in as.integer(neighbors(g, vert))) {
                                key_in  <- paste(c(neighbor, vert), collapse="")
                                key_out <- paste(c(vert, neighbor), collapse="")

                                # Compute B
                                B_new[[key_out]] <- (- lambda * V(graph)$prizes[vert]
                                                     + sumD
                                                     - D_old[[key_in]])

                                # Compute maxA
                                maxA <- -Inf
                                for (d in 1:depth) {
                                        if (length(findA$values[[d]]) == 1) {
                                                A_new[[key_out]][d] <- -Inf
                                        } else {
                                                A_new[[key_out]][d] <- (    sumE[d]
                                                                            - E_old[[key_in]][d]
                                                                            + max(findA$values[[d]][-neighbor_idx]))
                                        }
                                        maxA <- max(maxA, A_new[[key_out]][d])
                                }

                                # Compute D
                                D_new[[key_out]] <- max(B_new[[key_out]], maxA)

                                # Compute E
                                C <- -Inf
                                for (d in depth:2) {
                                        E_new[[key_out]][d] <- max(D_new[[key_out]], C)
                                        C <- (- E(graph)$costs[get.edge.ids(graph, c(vert, neighbor))]
                                              + sumE[d]
                                              - E_old[[key_in]][d])
                                        F_new[[key_out]][d] <- C + A_old[[key_in]][d - 1]
                                }
                                E_new[[key_out]][1] <- D_new[[key_out]]
                                F_new[[key_out]][1] <- -Inf

                                neighbor_idx <- neighbor_idx + 1

                        }

                        G[[as.character(vert)]] <- ( - lambda * V(graph)$prizes[vert]
                                                     + sumD)

                }

        }

        messages_new <- list()

        messages_new[[1]] <- A_new
        messages_new[[2]] <- E_new
        messages_new[[3]] <- F_new
        messages_new[[4]] <- B_new
        messages_new[[5]] <- D_new
        messages_new[[6]] <- G

        return (messages_new)
}
