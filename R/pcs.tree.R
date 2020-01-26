####------------------------------------- Documentation -------------------------------------####
#'
#'
#' @export
####------------------------------------- End Documentation -------------------------------------####
pcs.tree <- function(graph, terminals, lambda, root, depth, eps, max_iter, terminal_infty = 10000) {

        # Check input
        checkInput_res <- check_input(graph, terminals, lambda, root, depth, eps, max_iter)

        # Temporary solution
        g <- graph
        V(g)$name  <- checkInput_res$vert_names

        # Set root
        V(graph)$root <- FALSE
        V(graph)$root[checkInput_res$root_id] <- TRUE


        # Set terminal prizes
        V(graph)$prizes[checkInput_res$terminal_id] <- terminal_infty


        # Initialize messages
        msg_old <- initialize_messages(graph, depth, TRUE)
        msg_new <- initialize_messages(graph, depth, FALSE)


        # Run the algorithm
        iter     <- 0

        treeData_min       <- list()
        treeData_min$cost  <- +Inf
        treeData_min$edges <- rep(F, length(E(graph)))

        while (TRUE) {
                permutation <- sample(c(1:length(V(graph))))

                msg_new  <- loop(graph, lambda, depth, msg_old, msg_new, permutation)
                tol      <- compute_tol(graph, depth, msg_new, msg_old)
                msg_old  <- msg_new

                treeData <- get_tree(graph, lambda, depth, msg_old, iter)

                tree <- subgraph.edges(g, treeData$edges)
                if (  treeData$cost <= treeData_min$cost
                    & is.connected(tree)
                    & all(c(checkInput_res$root_name, checkInput_res$terminal_name) %in% V(tree)$name)) {
                        treeData_min <- treeData
                }

                iter <- iter + 1

                if (iter >= max_iter || tol <= eps) {
                        treeData_min$iter <- iter
                        break
                }
        }

        return(treeData_min)
}
