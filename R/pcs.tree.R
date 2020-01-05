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
        A_new <<- new.env()
        E_new <<- new.env()
        F_new <<- new.env()
        B_new <<- new.env()
        D_new <<- new.env()
        G     <<- new.env()

        initialize_messages(graph, depth)

        A_old <<- as.environment(as.list(A_new, all.names=TRUE))
        E_old <<- as.environment(as.list(E_new, all.names=TRUE))
        F_old <<- as.environment(as.list(F_new, all.names=TRUE))
        B_old <<- as.environment(as.list(B_new, all.names=TRUE))
        D_old <<- as.environment(as.list(D_new, all.names=TRUE))


        # Run the algorithm
        iter <- 0
        while(TRUE) {
                permutation <- sample(c(1:length(V(graph))))

                loop(graph, lambda, depth, permutation)

                iter <- iter + 1

                if (iter >= max_iter)
                        break
        }

        graph <- get_tree(graph, depth)

        return(graph)
}
