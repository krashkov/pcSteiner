####------------------------------------- Documentation -------------------------------------####
#'
#'
#' @export
####------------------------------------- End Documentation -------------------------------------####
pcs.tree <- function(graph, terminals, lambda, root, depth, eps, max_iter, terminal_infty = 10000) {

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
        msg_old <- initialize_messages(graph, depth, TRUE)
        msg_new <- initialize_messages(graph, depth, FALSE)


        # Run the algorithm
        iter <- 0
        while (TRUE) {
                permutation <- sample(c(1:length(V(graph))))

                msg_new <- loop(graph, lambda, depth, msg_old, msg_new, permutation)
                tol     <- compute_tol(graph, depth, msg_new, msg_old)
                msg_old <- msg_new

                iter    <- iter + 1

                treeEdges_and_cost <- get_tree(graph, lambda, depth, msg_old, iter)
                treeEdges <- treeEdges_and_cost[[1]]
                cost      <- treeEdges_and_cost[[2]]

                if (iter >= max_iter) {
                        generate_report(graph, treeEdges, tol, iter, cost)
                        break
                }

                if (tol <= eps) {
                        generate_report(graph, treeEdges, tol, iter, cost)
                        break
                }
        }

        return(treeEdges)
}
