compute_tol <- function (graph, depth, msg_new, msg_old) {

        heads <- as.integer(head_of(graph, E(graph)))
        tails <- as.integer(tail_of(graph, E(graph)))

        tol <- 0

        for (i in 1:length(heads)) {
                key_dir <- paste(c(heads[i], tails[i]), collapse="")
                key_rev <- paste(c(tails[i], heads[i]), collapse="")

                tol <- max(
                        tol,
                        max(0, abs(stats::na.omit(msg_new$A[[key_dir]] - msg_old$A[[key_dir]]))),
                        max(0, abs(stats::na.omit(msg_new$A[[key_rev]] - msg_old$A[[key_rev]]))),
                        max(0, abs(stats::na.omit(msg_new$E[[key_dir]] - msg_old$E[[key_dir]]))),
                        max(0, abs(stats::na.omit(msg_new$E[[key_rev]] - msg_old$E[[key_rev]]))),
                        stats::na.omit(abs(msg_new$B[[key_dir]] - msg_old$B[[key_dir]])),
                        stats::na.omit(abs(msg_new$B[[key_rev]] - msg_old$B[[key_rev]])),
                        stats::na.omit(abs(msg_new$D[[key_dir]] - msg_old$D[[key_dir]])),
                        stats::na.omit(abs(msg_new$D[[key_rev]] - msg_old$D[[key_rev]]))
                )

        }

        return (tol)
}
