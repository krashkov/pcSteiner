initialize_messages <- function (graph, depth) {

        heads <- as.numeric(head_of(graph, E(graph)))
        tails <- as.numeric(tail_of(graph, E(graph)))

        for (i in 1:length(heads)) {
                key_dir <- paste(c(heads[i], tails[i]), collapse="")
                key_rev <- paste(c(heads[i], tails[i]), collapse="")

                assign(key_dir, rep(0, depth), envir = A_new)
                assign(key_rev, rep(0, depth), envir = A_new)

                assign(key_dir, rep(0, depth), envir = E_new)
                assign(key_rev, rep(0, depth), envir = E_new)

                assign(key_dir, rep(0, depth), envir = F_new)
                assign(key_rev, rep(0, depth), envir = F_new)

                assign(key_dir, 0, envir = B_new)
                assign(key_rev, 0, envir = B_new)

                assign(key_dir, 0, envir = D_new)
                assign(key_rev, 0, envir = D_new)
        }

}
