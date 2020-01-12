pcs.tree <- function(graph, terminals, lambda, root, depth, max_iter, terminal_infty = 10000) {

        # Check graph
        if (is.null(graph))
                stop("Error: Graph is Null")

        if (length(V(graph)) == 0 )
                stop("Error: Graph does not contain vertices")

        if (! is.connected(graph))
                stop("Error: The graph is not connected. Run pcs.forest instead")

        if (is.null(V(graph)$prizes))
                stop("Error: Vertices do not have prizes")

        if (is.null(E(graph)$costs))
                stop("Error: Edges do not have costs")


        # Check terminals
        if (is.null(terminals) || is.na(terminals) || length(terminals) == 0)
                stop("Error: Terminals not found")


        # Set root
        V(graph)$root       <- FALSE
        V(graph)$root[root] <- TRUE


        # Set terminal prizes
        V(graph)$prizes[terminals] <- terminal_infty


        # Initialize messages
        messages_new <- initialize_messages(graph, depth)

        A_new <- messages_new[[1]]
        E_new <- messages_new[[2]]
        F_new <- messages_new[[3]]
        B_new <- messages_new[[4]]
        D_new <- messages_new[[5]]
        G     <- new.env()

        A_old <- as.environment(as.list(A_new, all.names=TRUE))
        E_old <- as.environment(as.list(E_new, all.names=TRUE))
        F_old <- as.environment(as.list(F_new, all.names=TRUE))
        B_old <- as.environment(as.list(B_new, all.names=TRUE))
        D_old <- as.environment(as.list(D_new, all.names=TRUE))

        # Run the algorithm
        iter <- 0
        while (TRUE) {
                permutation <- sample(c(1:length(V(graph))))

                messages_new <- loop(graph, lambda, depth, permutation,
                                     A_new, E_new, F_new, B_new, D_new,
                                     A_old, E_old, F_old, B_old, D_old,
                                     G)

                A_new <- messages_new[[1]]
                E_new <- messages_new[[2]]
                F_new <- messages_new[[3]]
                B_new <- messages_new[[4]]
                D_new <- messages_new[[5]]
                G     <- messages_new[[6]]

                A_old <- as.environment(as.list(A_new, all.names=TRUE))
                E_old <- as.environment(as.list(E_new, all.names=TRUE))
                F_old <- as.environment(as.list(F_new, all.names=TRUE))
                B_old <- as.environment(as.list(B_new, all.names=TRUE))
                D_old <- as.environment(as.list(D_new, all.names=TRUE))

                iter <- iter + 1

                if (iter >= max_iter)
                        break
        }

        graph <- get_tree(graph, depth, A_old, E_old, F_old, B_old, D_old, G)

        return(graph)
}
