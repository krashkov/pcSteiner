####--------------------------------------- Documentation ---------------------------------------####
#' Solve the Prize-Collecting Steiner Tree problem
#'
#' @description Solve the Prize-Collecting Steiner Tree problem.
#'
#' @usage pcs.tree(graph, terminals, lambda, root, depth, eps, max_iter, terminal_infty=10000)
#'
#' @param graph an igraph graph.
#' @param terminals a numeric or character vector which contains either ids or names of terminal nodes.
#' @param lambda a numeric parameter which establishes a ratio between edge costs and node prizes
#'               (see Sec.1 or Sec.3 in the vignette).
#' @param root a numeric or character scalar which corresponds to either id or name of a root (see
#'             Sec.3 in the vignette).
#' @param depth a numeric scalar which sets depth of the resultant tree (see Sec.3 in the vignette).
#' @param eps a numeric scalar which specifies tolerance for termination. 
#' @param max_iter a numeric scalar which specifies maximum number of iterations.
#' @param terminal_infty a numeric scalar which corresponds to a prize for each terminal node. This
#'                       value should be large enough to ensure that all terminals will be presented
#'                       in a solution.
#'
#' @return Returns a list with cost and edges of the final tree.
#'
#' @examples
#' g <- graph('Bull')
#' E(g)$costs  <- c(3, 3, 3, 3, 3)
#' V(g)$prizes <- c(10, 2, 2, 2, 2)
#' treeData <- pcs.tree(graph=g, terminals=c(4,5), lambda=1, root=3, depth=5, eps=1e-3, max_iter=10)
#'
#' @references 1. M. Bayati, C. Borgs, A. Braunstein, J. Chayes, A. Ramezanpour, and R. Zecchina,
#'                "Statistical Mechanics of Steiner Trees". PRL, 2008.
#'
#'             2. M. Bayati, A. Braunstein, and R. Zecchina, "A rigorous analysis of the cavity
#'                equations for the minimum spanning tree". Journal of Mathematical Physics, 2008.
#'
#'             3. I. Biazzo, A. Braunstein and R. Zecchina, "Performance of a cavity-method-based
#'                algorithm for the prize-collecting Steiner tree problem on graphs". PRL, 2012.
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
