loop <- function (graph, lambda, depth, msg_old, msg_new, permutation) {

        for (vert in permutation) {

                if (V(graph)$root[vert] == TRUE) {
                        # Update root
                        for (neighbor in as.integer(neighbors(graph, vert))) {
                                key_out <- paste(c(vert, neighbor), collapse="")

                                for (d in 1:depth) {
                                        msg_new$E[[key_out]][d] <- 0
                                        msg_new$A[[key_out]][d] <- -Inf
                                }
                                msg_new$A[[key_out]][1] <- 0

                                msg_new$B[[key_out]] <- -Inf
                                msg_new$D[[key_out]] <- 0
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
                                        sumE[d] <- sumE[d] + msg_old$E[[key_in]][d]

                                        findA$values[[d]][neighbor_idx] <- (- E(graph)$costs[get.edge.ids(graph, c(vert, neighbor))]
                                                                            - msg_old$E[[key_in]][d]
                                                                            + msg_old$A[[key_in]][d-1])
                                }
                                sumE[1] <- sumE[1] + msg_old$E[[key_in]][1]

                                # Compute sum of D's
                                sumD <- sumD + msg_old$D[[key_in]]

                                neighbor_idx <- neighbor_idx + 1
                        }

                        # Compute new messages
                        neighbor_idx <- 1
                        # for (neighbor in as.integer(neighbors(g, vert))) {
                        for (neighbor in as.integer(neighbors(graph, vert))) {
                                key_in  <- paste(c(neighbor, vert), collapse="")
                                key_out <- paste(c(vert, neighbor), collapse="")

                                # Compute B
                                msg_new$B[[key_out]] <- (- lambda * V(graph)$prizes[vert]
                                                         + sumD
                                                         - msg_old$D[[key_in]])

                                # Compute maxA
                                maxA <- -Inf
                                for (d in 1:depth) {
                                        if (length(findA$values[[d]]) == 1) {
                                                msg_new$A[[key_out]][d] <- -Inf
                                        } else {
                                                msg_new$A[[key_out]][d] <- (  sumE[d]
                                                                            - msg_old$E[[key_in]][d]
                                                                            + max(findA$values[[d]][-neighbor_idx]))
                                        }
                                        maxA <- max(maxA, msg_new$A[[key_out]][d])
                                }

                                # Compute D
                                msg_new$D[[key_out]] <- max(msg_new$B[[key_out]], maxA)

                                # Compute E
                                C <- -Inf
                                for (d in depth:2) {
                                        msg_new$E[[key_out]][d] <- max(msg_new$D[[key_out]], C)
                                        C <- (- E(graph)$costs[get.edge.ids(graph, c(vert, neighbor))]
                                              + sumE[d]
                                              - msg_old$E[[key_in]][d])
                                        msg_new$F[[key_out]][d] <- C + msg_old$A[[key_in]][d - 1]
                                }
                                msg_new$E[[key_out]][1] <- msg_new$D[[key_out]]
                                msg_new$F[[key_out]][1] <- -Inf

                                neighbor_idx <- neighbor_idx + 1

                        }

                        msg_new$G[[as.character(vert)]] <- ( - lambda * V(graph)$prizes[vert]
                                                             + sumD)

                }

        }

        return (msg_new)
}
