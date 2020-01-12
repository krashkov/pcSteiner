initialize_messages <- function (graph, depth) {
        A_new <- new.env()
        E_new <- new.env()
        F_new <- new.env()
        B_new <- new.env()
        D_new <- new.env()

        heads <- as.integer(head_of(graph, E(graph)))
        tails <- as.integer(tail_of(graph, E(graph)))

        for (i in 1:length(heads)) {
                key_dir <- paste(c(heads[i], tails[i]), collapse="")
                key_rev <- paste(c(tails[i], heads[i]), collapse="")

                A_new[[key_dir]] <- rep(0, depth)
                A_new[[key_rev]] <- rep(0, depth)

                E_new[[key_dir]] <- rep(0, depth)
                E_new[[key_rev]] <- rep(0, depth)

                F_new[[key_dir]] <- rep(0, depth)
                F_new[[key_rev]] <- rep(0, depth)

                B_new[[key_dir]] <- 0
                B_new[[key_rev]] <- 0

                D_new[[key_dir]] <- 0
                D_new[[key_rev]] <- 0
        }

        messages_new <- list()

        messages_new[[1]] <- A_new
        messages_new[[2]] <- E_new
        messages_new[[3]] <- F_new
        messages_new[[4]] <- B_new
        messages_new[[5]] <- D_new

        return (messages_new)
}
