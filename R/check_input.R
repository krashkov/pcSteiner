check_input <- function (graph, terminals, lambda, root, depth, eps, max_iter) {

        # Check graph
        if (! is.igraph(graph))
                stop("Error: Graph is not an igraph graph")

        if (length(V(graph)) == 0 )
                stop("Error: Graph does not contain vertices")

        if (! is.connected(graph))
                stop("Error: The graph is not connected. Run pcs.forest instead")

        if (is.null(V(graph)$prizes))
                stop("Error: Vertices do not have prizes")

        if (is.null(E(graph)$costs))
                stop("Error: Edges do not have costs")


        # Check lambda
        if (class(lambda) != "numeric")
                stop("Error: lambda is not a number")


        # Check names of vertices
        if (is.null(V(graph)$name)) {
                # creating name attribute
                V(graph)$name <- as.character(1:length(V(graph)))
        } else {
                # creating new name and realname attributes
                V(graph)$realname <- V(graph)$name
                V(graph)$name     <- as.character(1:length(V(graph)))
        }


        # Mathcing names of vertices and terminals if possible
        if (class(terminals) == "character") {
                # terminals contain realname of vertices
                if (sum(terminals %in% V(graph)$realname) != length(terminals)) {
                        stop("Error: Vertices names do not contain terminal names")
                } else {
                        # Convert realnames of terminals to integer id's (character id's)
                        terminal_id   <- match(terminals, V(graph)$realname)
                        terminal_name <- V(graph)$name[terminal_id]
                }
        } else if (class(terminals) == "numeric") {
                # terminals contains id's of vertices
                terminal_id   <- terminals
                terminal_name <- V(graph)$name[terminal_id]
        } else
                stop("Error: Invalid type of terminals")


        # Mathcing names of vertices and root if possible
        if (class(root) == "character") {
                if (sum(root %in% V(graph)$realname) != 1) {
                        stop("Error: Vertices names do not contain root name")
                } else {
                        root_id   <- match(root, V(graph)$realname)
                        root_name <- V(graph)$name[root_id]
                }
        } else if (class(terminals) == "numeric") {
                root_id   <- root
                root_name <- V(graph)$name[root_id]
        } else
                stop("Error: Invalid type of root")


        # Check depth
        if (class(depth) != "numeric") {
                stop("Error: depth is not a number")
        }


        # Check eps
        if (class(eps) != "numeric")
                stop("Error: eps is not a number")


        # Check max_iter
        if (class(max_iter) != "numeric")
                stop("Error: max_iter is not a number")


        checkInput_res <- list()
        checkInput_res$vert_names <- V(graph)$name

        checkInput_res$terminal_id   <- terminal_id
        checkInput_res$terminal_name <- terminal_name

        checkInput_res$root_id   <- root_id
        checkInput_res$root_name <- root_name

        return(checkInput_res)
}
