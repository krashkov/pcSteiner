initialize_messages <- function (graph, depth, rand) {

        msg   <- list()
        msg$A <- new.env()
        msg$E <- new.env()
        msg$F <- new.env()
        msg$B <- new.env()
        msg$D <- new.env()
        msg$G <- new.env()

        heads <- as.integer(head_of(graph, E(graph)))
        tails <- as.integer(tail_of(graph, E(graph)))

        for (i in 1:length(heads)) {
                key_dir <- paste(c(heads[i], tails[i]), collapse="")
                key_rev <- paste(c(tails[i], heads[i]), collapse="")

                msg$A[[key_dir]] <- rep(0, depth)
                msg$A[[key_rev]] <- rep(0, depth)

                msg$E[[key_dir]] <- rep(0, depth)
                msg$E[[key_rev]] <- rep(0, depth)

                msg$F[[key_dir]] <- rep(0, depth)
                msg$F[[key_rev]] <- rep(0, depth)

                msg$B[[key_dir]] <- 0
                msg$B[[key_rev]] <- 0

                msg$D[[key_dir]] <- 0
                msg$D[[key_rev]] <- 0

                # Adding random noise

                if (rand) {
                        msg$B[[key_dir]] <- -stats::runif(1)
                        msg$B[[key_rev]] <- -stats::runif(1)

                        for (d in 1:depth) {
                                msg$A[[key_dir]][d] <- -stats::runif(1)
                                msg$A[[key_rev]][d] <- -stats::runif(1)
                        }

                        msg$D[[key_dir]] <- max(msg$B[[key_dir]], max(msg$A[[key_dir]]))
                        msg$D[[key_rev]] <- max(msg$B[[key_rev]], max(msg$A[[key_rev]]))

                        for (d in 1:depth) {
                                C_key_dir <- -stats::runif(1)
                                C_key_rev <- -stats::runif(1)

                                msg$E[[key_dir]][d] <- max(C_key_dir, msg$D[[key_dir]])
                                msg$E[[key_rev]][d] <- max(C_key_rev, msg$D[[key_rev]])
                        }
                        msg$E[[key_dir]][depth] <- msg$D[[key_dir]]
                        msg$E[[key_rev]][depth] <- msg$D[[key_rev]]
                }

        }

        return (msg)
}
